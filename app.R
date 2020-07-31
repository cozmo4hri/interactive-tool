#
# The Interactive Cozmo Classification / Emotion Validation Shiny App
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
# The code is based and inspired by Lisa DeBruine's code for this shiny app: http://shiny.psy.gla.ac.uk/lmem_sim/
#
# Ruud's wishlist:
# - Display the animation categories in two columns

# load the dependencies
library(shiny)
library(shinydashboard) 
library(tidyverse)
library(DT)
library(gridExtra)
library(grid)
library(gtable)

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
      # group input - participant/video ---- #split for scatter/density so not needed anymore
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
   # #plot for density ----
   output$densPlot <- renderPlot({
     
     densityPlot <- ggplot(DF.de(), aes(x=valence,
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
     
     confidencePlot <- ggplot(DF.de(),aes(x=confidence)) + 
       geom_density() + 
       theme_linedraw() + 
       theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       theme(axis.line.x = element_line(color="black", size = 0.5), axis.line.y = element_line(color="black", size = 0.5))
       geom_vline(aes(xintercept=mean(confidence)),
                  color="grey", linetype="dashed", size=1)
     
     xPlot <- ggplot(DF.de(), aes(x=valence)) + 
       geom_density() + 
       theme_linedraw() + 
       xlim(-1,1)+
       theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       geom_vline(aes(xintercept=mean(valence)),
                  color="grey", linetype="dashed", size=1) +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank()) +
       theme(axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank())
     
     yPlot <- ggplot(DF.de(), aes(x=arousal)) + 
       geom_density() + 
       theme_linedraw() + 
       xlim(-1,1)+
       theme(panel.border = element_blank(), panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
       geom_vline(aes(xintercept=mean(arousal)),
                  color="grey", linetype="dashed", size=1) + 
       coord_flip() +
       theme(axis.title.x=element_blank(),
             axis.text.x=element_blank(),
             axis.ticks.x=element_blank()) +
       theme(axis.title.y=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank())
     
     grid.arrange(xPlot, confidencePlot, densityPlot, yPlot, ncol=2, nrow = 2, widths = c(2, 1), heights = c(1, 2)) 
     
     
   })
   # 
   
  # #get info for video ----
   output$info <- renderPrint({
     DF.sc() %>% 
       nearPoints(input$plot_click, maxpoints = 1)
   }) 
   
   output$info2 <- renderPrint({
     DF.sc() %>% 
       nearPoints(input$plot_click, maxpoints = 1) %>% pull(video_id)
   }) 
   
  # #get video_id and renderUI ----
  output$avideo <- renderUI({
    videotemp <- DF.sc() %>% nearPoints(input$plot_click, maxpoints = 1) %>% pull(video_id)
    
    tags$video(id="video2", type = "video/mp4",src = paste0(videotemp, ".mp4", sep = ""), width = "320px", height = "180px", controls = "controls")
    
  }) 
   
  # I think reactively updating the whole html segment could be the trick to it though? 
  # The tags$video() function could be within a renderUI({}) in the server, returning the html values that tag$video() produces. 
  # You can then just say inside of that renderUI where to get the video’s path from. 
  # You could then just feed that to an htmlOutput() in the UI?
     
  
  
  #filename <- normalizePath(file.path('./images',
  #                                    paste('image', input$n, '.jpeg', sep='')))
    #tags$video(id="video2", type = "video/mp4",src = paste0("42.mp4"), width = "320px", height = "180px", controls = "controls")

  # #table ----
   output$tableDF <- renderDataTable(datatable(
     DF.sc(), rownames= FALSE
   )) 
}

# Run the application 
shinyApp(ui, server)
