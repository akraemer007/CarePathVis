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
# how to find table names rather than naming them explicitly

names(TopCarePaths) <- gsub("care_cards\\.|patient_information\\.|organization_tree\\.", "",names(TopCarePaths))

# Changing Factors to Numerics (Do this for all measures or the calculations won't work)

unfactorize<-c("completed_sum",
               "calc_appears_offset",
               "completed_and_expired_sum",
               "view_count_sum",
               "unique_view_count",
               "patient_care_path_count",
               "guide_item_id",
               "guide_item_rank")
TopCarePaths[,unfactorize]<-lapply(unfactorize, function(x) as.numeric(as.character(TopCarePaths)[,x]))

# Setting levels with variables
TopCarePaths$PrePostFlag <- factor(TopCarePaths$PrePostFlag, levels = c('Pre', 'Post'))
TopCarePaths$is_form <- factor(TopCarePaths$is_form, levels = c('No','Yes'))

# Original Joining & cleaning --------------------------------------------------------
TopCarePaths <- TopCarePaths %>%
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
  ) %>%
  group_by(guide_item_id,
           organization_parent_name,
           guide_item_rank,
           title_full,
           CardForm,
           PrePostFlag,
           FunctionalComponent,
           is_form,
           Logic,
           calc_appears_offset,
           required,
           conditional_tag
  )%>%
  summarise(total.completed_sum = sum(completed_sum),
            total.completed_and_expired_sum = sum(completed_and_expired_sum),
            total.view_count_sum = sum(view_count_sum),
            total.pcp = sum(patient_care_path_count)
            # , count.pcp = n() figure out how to get a functional better count of pcp Looker says total PCP is 1855 for these clients
  ) %>%
  arrange(guide_item_rank)

# Visualization --------------------------------------------------------

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


