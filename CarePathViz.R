library(dplyr)
library("devtools")
install_github("looker/lookr")
library(LookR)
library(ggplot2)
library(ggthemes)
library("plotly")
# devtools::install_github("ropensci/plotly")

Sys.setenv("plotly_username"="kidman007")
Sys.setenv("plotly_api_key"="f6nspht8zn")

setwd('/Users/andrewkraemer/Google\ Drive/WB\ -\ Analytics/Projects/R/CarePathViz')
source("LookRApiCredentials.R")

looker_setup(   id = IdLookR,
                secret = secretLookR,
                api_path = api_pathLookR
)

# Starting Look || pulls from "CareCardSummaryTidy" Look
TCP <- run_look(602)

TopCarePaths <- tbl_df(TCP) # so I don't have to rerun the look everytime
TopCarePaths <- as.data.frame(as.matrix(TopCarePaths),stringsAsFactors = F)

# Looker name and data type clean up --------------------------------------------------------
# Renaming CarePaths Sanely
# how do i do this as a loop?
names(TopCarePaths) <- gsub("care_cards.", "", names(TopCarePaths), fixed = TRUE)
names(TopCarePaths) <- gsub("patient_information.", "", names(TopCarePaths), fixed = TRUE)
names(TopCarePaths) <- gsub("organization_tree.", "", names(TopCarePaths), fixed = TRUE)
# Changing Factors to Numerics (Do this for all measures or the calculations won't work)
# how do i do this as a loop?
TopCarePaths$completed_sum <- as.numeric(as.character(TopCarePaths$completed_sum))
TopCarePaths$calc_appears_offset <- as.numeric(as.character(TopCarePaths$calc_appears_offset))
TopCarePaths$completed_and_expired_sum <- as.numeric(as.character(TopCarePaths$completed_and_expired_sum))
TopCarePaths$view_count_sum <- as.numeric(as.character(TopCarePaths$view_count_sum))
TopCarePaths$unique_view_count <- as.numeric(as.character(TopCarePaths$unique_view_count))
TopCarePaths$patient_care_path_count <- as.numeric(as.character(TopCarePaths$patient_care_path_count))
TopCarePaths$guide_item_id <- as.numeric(as.character(TopCarePaths$guide_item_id))
TopCarePaths$guide_item_rank_num <- as.numeric(as.character(TopCarePaths$guide_item_rank))
# Setting levels with variables
TopCarePaths$PrePostFlag <- factor(TopCarePaths$PrePostFlag, levels = c('Pre', 'Post'))
TopCarePaths$is_form <- factor(TopCarePaths$is_form, levels = c('No','Yes'))

# Original Joining & cleaning --------------------------------------------------------
TopCarePaths <- TopCarePaths %>%
  left_join(ManualTagRef) %>%
  left_join(FunctionalComponentTbl) %>%
  # , by = c("title_full","CareCardName")
  mutate(CardForm = ifelse(is_form=='Yes','Form','CareCard'),
         organization_parent_name = substr(organization_parent_name,0,20),
         organization_parent_name = ifelse( organization_parent_name == 'Avera McKennan Hospi', 'Avera',
                                            ifelse( organization_parent_name == 'Meriter Health Servi', 'Meriter',
                                                    ifelse( organization_parent_name == 'UW Health (PARENT OR', 'UW Orthopedics',
                                                            ifelse( organization_parent_name == 'Virtua Health System', 'Virtua',
                                                                    organization_parent_name )))),
         calc_appears_offset = ifelse(calc_appears_offset < -50 , -50, 
                                      ifelse( calc_appears_offset > 90, 90, calc_appears_offset) 
         ),
         guide_item_type = ifelse( is_form == 'Yes', 'Form',
                                   ifelse( required == 'Yes', 'CareCard - Required',
                                           'CareCard - Recommended')),
         title_full = ifelse(
           substr(title_full,nchar(title_full)-1,nchar(title_full)) == ', ', title,
           title_full) #gets rid of title's with ", " at end
  )



# View( filter( TopCarePaths, organization_parent_name == "Virtua Health System" ))

# Required Card Simplification --------------------------------------------------------
TopCarePaths.Req <- TopCarePaths %>%  
  filter(required == "Yes" ,
         conditional_tag == "No"
  ) %>%
  # select(-is_form,-due_offset,-appears_offset) %>%
  group_by(guide_item_id,
           organization_parent_name,
           guide_item_rank,
           guide_item_rank_num,
           title_full,
           CardForm,
           PrePostFlag,
           FunctionalComponent,
           is_form,
           Logic,
           calc_appears_offset
  )%>%
  summarise(total.completed_sum = sum(completed_sum),
            total.completed_and_expired_sum = sum(completed_and_expired_sum),
            total.view_count_sum = sum(view_count_sum),
            total.pcp = sum(patient_care_path_count)
            # , count.pcp = n() figure out how to get a functional better count of pcp Looker says total PCP is 1855 for these clients
  ) %>%
  arrange(guide_item_rank)

table(TopCarePaths.Req$organization_parent_name)

ggplot( TopCarePaths.Req, aes( x = PrePostFlag, fill = CardForm ) )+
  geom_bar() +
  facet_wrap( ~organization_parent_name ) +
  ggtitle( 'Faceted, Cards vs Forms by Pre / Post' ) +
  theme_bw()

ggplot( TopCarePaths.Req, aes( x = FunctionalComponent ) ) +
  geom_bar() +
  facet_wrap( ~organization_parent_name ) +
  ggtitle( 'Count Cards per Functional Component' ) +
  theme_bw() +
  scale_y_continuous( breaks = 1:100 ) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(TopCarePaths.Req,aes(x = guide_item_rank_num, y = total.completed_sum/total.completed_and_expired_sum))+
  geom_point( aes( shape = PrePostFlag, color = is_form, size = 3 ) ) +
  # geom_text( aes( label = guide_item_rank_num ), hjust = 0, vjust = 0 ) +
  facet_grid( organization_parent_name~. ) +
  ggtitle('Faceted, Pre post Guide Item by Rank') +
  ylab('Completion %') +
  # scale_x_continuous( limits = c( 1, 100 ), breaks = seq( 1, 100, 10)) +
  guides( size = FALSE ) +
  theme_bw() +
  xlab( 'CareCard Rank' )

ggplot(TopCarePaths.Req,aes(x = guide_item_rank_num, y = total.completed_sum/total.completed_and_expired_sum))+
  geom_point( aes( shape = PrePostFlag, color = FunctionalComponent , size = 3 ) ) +
  # geom_text( aes( label = guide_item_rank_num ), hjust = 0, vjust = 0 ) +
  facet_grid( organization_parent_name~. ) +
  ggtitle('Faceted, Pre post Guide Item by Rank -- Functional Components') +
  ylab('Completion %') +
  # scale_x_continuous( limits = c( 1, 100 ), breaks = seq( 1, 100, 10)) +
  guides( size = FALSE ) +
  theme_bw() +
  xlab( 'CareCard Rank' )

# Req Card Offset view --------------------------------------------------------

ggplot(TopCarePaths.Req,aes(x = calc_appears_offset, y = total.completed_sum/total.completed_and_expired_sum, text = paste("Title:", title_full) ) )+
  geom_point( aes( shape = is_form,  color = PrePostFlag ) , alpha = 2/5, size = 5) + #dimensions of CarePath
  # geom_text( aes( label = paste("Title:", title_full) ) ) + # changes the tooltip
  geom_rug(sides="b", position='jitter') + #adds rug, which counts number of points. Added so only applied to X-axis & spread out with jitter
  guides( alpha = FALSE ) + #hides unneccssary legend for hard coded transparancy
  facet_grid( organization_parent_name~. ) + #Facets graph horizontally
  scale_x_continuous( limits = c( -50, 90 ), breaks = seq( -50, 90, 5 ) ) + #controls X axis tick marks
  scale_y_continuous(labels = scales::percent, limits = c( 0, 1 ), breaks = seq( 0, 1, .2 )) + #quickly changes y axis to %
  geom_vline( xintercept = 0 ) + #adds solid line to chart
  geom_text( x = 0, label = "Surgery\n", y = .1, family = 'sans', angle=0,  color = 'gray34', size = 3, alpha = 1/5 ) + #puts text next to line for surg date
  theme_bw() + #changes to a simpler, easy to read theme
  theme( strip.background = element_blank() ) + #removes ugly gray from name area of facet
  ggtitle( 'Content Completion %') +
  ylab('Completion %') +
  xlab( 'Days Appears Offset from Surgery' )
ggplotly()

####colored by functional outcome
ggplot(TopCarePaths.Req,aes(x = calc_appears_offset, y = total.completed_sum/total.completed_and_expired_sum, text = paste("Title:", title_full) ) )+
  geom_point( aes( color = FunctionalComponent ) , alpha = 2/5, size = 5) + #dimensions of CarePath
  # geom_text( aes( label = paste("Title:", title_full) ) ) + # changes the tooltip
  geom_rug(sides="b", position='jitter') + #adds rug, which counts number of points. Added so only applied to X-axis & spread out with jitter
  guides( alpha = FALSE ) + #hides unneccssary legend for hard coded transparancy
  facet_grid( organization_parent_name~. ) + #Facets graph horizontally
  scale_x_continuous( limits = c( -50, 90 ), breaks = seq( -50, 90, 5 ) ) + #controls X axis tick marks
  scale_y_continuous(labels = scales::percent, limits = c( 0, 1 ), breaks = seq( 0, 1, .2 )) + #quickly changes y axis to %
  geom_vline( xintercept = 0 ) + #adds solid line to chart
  geom_text( x = 0, label = "Surgery\n", y = .1, family = 'sans', angle=0,  color = 'gray34', size = 3, alpha = 1/5 ) + #puts text next to line for surg date
  theme_bw() + #changes to a simpler, easy to read theme
  theme( strip.background = element_blank() ) + #removes ugly gray from name area of facet
  ggtitle( 'Content Completion % by Functional Component' ) +
  ylab('Completion %') +
  xlab( 'Days Appears Offset from Surgery' )
ggplotly()

####shown by views
ggplot(TopCarePaths.Req,aes(x = calc_appears_offset, y = total.view_count_sum, text = paste("Title:", title_full) ) )+
  geom_point( aes( shape = is_form,  color = PrePostFlag ) , alpha = 2/5, size = 5) + #dimensions of CarePath
  # geom_text( aes( label = paste("Title:", title_full) ) ) + # changes the tooltip
  # geom_rug(sides="b", position='jitter') + #adds rug, which counts number of points. Added so only applied to X-axis & spread out with jitter
  guides( alpha = FALSE ) + #hides unneccssary legend for hard coded transparancy
  facet_grid( organization_parent_name~. ) + #Facets graph horizontally
  scale_x_continuous( limits = c( -50, 90 ), breaks = seq( -50, 90, 5 ) ) + #controls X axis tick marks
  geom_vline( xintercept = 0 ) + #adds solid line to chart
  geom_text( x = 0, label = "Surgery\n", y = .1, family = 'sans', angle=0,  color = 'gray34', size = 3, alpha = 1/5 ) + #puts text next to line for surg date
  theme_bw() + #changes to a simpler, easy to read theme
  theme( strip.background = element_blank() ) + #removes ugly gray from name area of facet
  ggtitle( 'Sum of Completed Cards' ) +
  ylab('Completed Sum') +
  xlab( 'Days Appears Offset from Surgery' )
ggplotly()  
# What is the most popular non-required card ----------------------------------------

TopCarePaths.NotReq <- TopCarePaths %>%  
  filter( required == "No", conditional_tag == "No" ) %>%
  group_by(guide_item_id,
           organization_parent_name,
           title_full)%>%
  summarise(total.completed_sum = sum(completed_sum),
            total.completed_and_expired_sum = sum(completed_and_expired_sum),
            total.view_count_sum = sum(view_count_sum),
            total.unique_view_count_sum = sum(unique_view_count),
            total.pcp = sum(patient_care_path_count)
            # , count.pcp = n() figure out how to get a functional better count of pcp Looker says total PCP is 1855 for these clients
  )

TopCarePaths.NotReq %>%
  filter( organization_parent_name == "Avera McKennan Hospi" ) %>%
  group_by( title_full ) %>%
  summarise( total.unique_view_count_sum ) %>%
  arrange( desc( total.unique_view_count_sum ) )


ggplot( filter( TopCarePaths.NotReq, organization_parent_name == "Avera McKennan Hospi" ) ,
        aes(x = guide_item_id, y = total.view_count_sum ) ) +
  geom_point() +
  # facet_wrap(~organization_parent_name) +
  ggtitle( 'Non-Required Card Popularity' ) +
  theme_bw()

