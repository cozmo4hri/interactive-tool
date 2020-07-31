### tab_scatter ----
tab_scatter <- tabItem(
  tabName = "tab_scatter",
  p("Click on a point in the graph to select a video and get the validation data and play the animation"),
  column(
    width = 12,
    box(
      title = "Animation group",
      #width = NULL,
      plotOutput(outputId = "scatterPlot", click = "plot_click")
    ),
    box(
      title = "Animation video",
      htmlOutput(outputId = "avideo"),
      verbatimTextOutput("info2")
    ),
    column(width = 12,
           box(
             title = "Animation details",
             verbatimTextOutput("info"),
             width = 12
           )
    )
  )
)
