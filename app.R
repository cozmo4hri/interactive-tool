#
# The Cozmo Emotion Validation Shiny App (version: 0.0.5)
# by Ruud Hortensius (University of Glasgow), part of the Cozmo4HRI project (github.com/comzmo4hri)
#
# This Shiny app displays the data from a emotion validation / classification study 
# The user will be able to select the valence, arousal, and confidence of the ratings, the previous engagement of the raters with robots, and
# the animation category of the Cozmo robot   
# 
# Send me a message (ruud.hortensius@glasgow.ac.uk), find me on twitter (@ruudhortensius) or github (github.com/comzmo4hri).
#    
# This is one of my first attempts to build a Shiny app: http://shiny.rstudio.com/. Just hit the 'Run App' button above.
# 
#  
# The code is based and inspired by Lisa DeBruine's code for this shiny app: http://shiny.psy.gla.ac.uk/lmem_sim/
#
#
# Ruud's wishlist:
# - Information displayed in the app
# - Density plots normal (lines are weird)
# - Reduce loading time
# - Make it interactive: a click on a datapoint results in the video loaded
# - Display the animation categories in two columns

# load the dependencies
library(shiny)
library(shinydashboard) 
library(tidyverse)
library(DT)

source("tab_intro.R")
source("tab_scatter.R")
source("tab_density.R")
source("tab_table.R")
DF.main <- read_csv("fullset_shiny.csv") 

## UI ----
ui <- dashboardPage(
  dashboardHeader(title = "cozmo4hri: interactive tool for emotion classification"),
  dashboardSidebar(
    width = 350,
    sidebarMenu(
      menuItem("Introduction", tabName = "tab_intro"),
      menuItem("Scatter plot", tabName = "tab_scatter"),
      menuItem("Density plot", tabName = "tab_density"),
      menuItem("Table", tabName = "tab_table"),
      
      # main input ----
      box(
        title = "Valence and arousal ratings",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        background = "black",
        sliderInput("filter_valence",
                    "Valence of the animation:",
                    min = -1,
                    max = 1,
                    value = c(-1,1),
                    step = 0.1),
        sliderInput("filter_arousal",
                    "Arousal of the animation:",
                    min = -1,
                    max = 1,
                    value = c(-1,1),
                    step = 0.1)
      ),
      # exploratory input ----
      box(
        title = "Exploratory factors",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        background = "black",
        sliderInput("filter_confidence",
                    "Confidence of the rating:",
                    min = 0,
                    max = 1,
                    value = c(0,1),
                    step = 0.1),
        sliderInput("filter_interaction",
                    "Engagement of the raters with robots:",
                    min = 1,
                    max = 7,
                    value = c(1,7),
                    step = 1)
      ),
      # group input - animation ----
      box(
        title = "Animation group",
        solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
        width = NULL,
        background = "black",
        checkboxGroupInput("behaviour",
                           "Animation category of the robot (select one or more):",
                           b <- unique(DF.main$group) 
        )
      )#,
      # group input - participant/video ----
      # box(
      #   title = "Data",
      #   solidHeader = TRUE, collapsible = TRUE, collapsed = FALSE,
      #   width = NULL,
      #   background = "black",
      #   radioButtons("collapse", "Show data at video or individual level:",
      #                list("video_id", "ppn"))
      # )
    )
  ),
  
  ###---- dashboardBody
  dashboardBody(  
    tabItems(
      tab_density,
      tab_scatter,
      tab_table,
      tab_intro
    )
  ),
  skin = "black"
)

## server ----
server <- function(input, output) {
  
  activeVideo <- reactiveVal()

  # # filter and group - density ----
  DF.de <- reactive({
    DF.tmp <- DF.main %>% filter(between(valence, input$filter_valence[1], input$filter_valence[2])) 
    DF.tmp <- DF.tmp %>% filter(between(arousal, input$filter_arousal[1], input$filter_arousal[2])) 
    DF.tmp <- DF.tmp %>% filter(between(confidence, input$filter_confidence[1], input$filter_confidence[2])) 
    DF.tmp <- DF.tmp %>% filter(between(interaction_score, input$filter_interaction[1], input$filter_interaction[2])) 
    DF.tmp <- DF.tmp %>% filter(if(length(input$behaviour>0)) group %in% input$behaviour else TRUE)
  })
  
  # # filter and group - scatter/table ----
  
   DF.sc <- reactive({
     DF.tmp2 <- DF.main %>% 
       dplyr::group_by(video_id, group, animation_name) %>% #input$collapse (to have a reactive grouping)
       summarise(valence = mean(valence),
                 arousal = mean(arousal),
                 confidence = mean(confidence),
                 interaction_score = mean(interaction_score)) 
     DF.tmp2 <- DF.tmp2 %>% filter(between(valence, input$filter_valence[1], input$filter_valence[2])) 
     DF.tmp2 <- DF.tmp2 %>% filter(between(arousal, input$filter_arousal[1], input$filter_arousal[2])) 
     DF.tmp2 <- DF.tmp2 %>% filter(between(confidence, input$filter_confidence[1], input$filter_confidence[2])) 
     DF.tmp2 <- DF.tmp2 %>% filter(between(interaction_score, input$filter_interaction[1], input$filter_interaction[2])) 
     DF.tmp2 <- DF.tmp2 %>% filter(if(length(input$behaviour>0)) group %in% input$behaviour else TRUE)
     DF.tmp2 <- DF.tmp2 %>% select(-interaction_score)
   })
  # 
  # #plot for density ----
  output$densPlot <- renderPlot({
      ggplot(DF.de(), aes(x=valence,
                 y=arousal,
                 #by=animation_name,
                 label = animation_name))+
      geom_point() +
      geom_density2d(color="darkblue") +
      stat_density_2d(aes(fill = stat(nlevel), alpha =0.9), geom = "polygon") +
      theme_linedraw() +
      # coord_cartesian(xlim=c(-1,1), ylim=c(-1,1)) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      scale_fill_viridis_c() +
      coord_fixed(1/1) +
      xlim(-1,1) +
      ylim(-1,1) +
      geom_hline(yintercept=0,color = "grey", size=1, alpha = 0.5) +
      geom_vline(xintercept = 0,color = "grey", size=1, alpha = 0.5) +
      theme(strip.text.x = element_text(size=10, colour="black",margin = margin(0.1,0,0.1,0, "mm")),
            strip.background = element_rect(fill="white")) +
      theme(legend.position="none")
  })
  # 
  # #plot for scatter ----
   output$scatterPlot <- renderPlot({
     DF.sc() %>% 
       ggplot(aes(x=valence,
                 y=arousal,
                 by = group,
                 colour=group,
                 label = animation_name))+
      geom_point()+
      xlim(-1,1) +
      ylim(-1,1) +
      theme_linedraw() +
      coord_fixed(1/1) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      theme(legend.position="none") +
      geom_hline(yintercept=0,color = "grey", size=1, alpha = 0.5) +
      geom_vline(xintercept = 0,color = "grey", size=1, alpha = 0.5)
  })
  # 
  # #get video_id ----
   #observeEvent(input$plot_click,
  #              {
   #               activeVideo <- DF.sc() %>% nearPoints(input$plot_click, maxpoints = 1) 
    #              activeVideo <- activeVideo %>% mutate(video_id = as.character(video_id)) 
     #             activeVideo <- activeVideo %>% pull(video_id)
                  
  #                write.csv(activeVideo, "active.csv")
  #                myfile <- fs::file_temp(activeVideo, ext = ".mp4")
                  
      #          })
  # 
  # 
  # #DF.de <- reactive({
  # # activeVideo <- DF.sc %>%
  # #    filter(video_id == "42") %>% 
  # #    mutate(video_id = as.character(video_id)) %>% 
  # #    pull(video_id)
  # # })
  # 
  # #get info for video ----
   output$info <- renderPrint({
     DF.sc() %>% 
       nearPoints(input$plot_click, maxpoints = 1)
   }) 
   
  # output$info2 <- renderPrint({
  #   DF.sc() %>% 
  #     nearPoints(input$plot_click, maxpoints = 1) %>% 
  #     pull(video_id)
  # }) 
  # output$clickVideo <- renderPrint(({
  #   activeVideo()
  # }))
   
  # #table ----
   output$tableDF <- renderDataTable(datatable(
     DF.sc(), rownames= FALSE
   )) 
}

# Run the application 
shinyApp(ui, server)
