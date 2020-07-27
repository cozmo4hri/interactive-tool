### tab_scatter ----
tab_scatter <- tabItem(
  tabName = "tab_scatter",
  column(
    width = 12,
    box(
      title = "Animation group",
      #width = NULL,
      plotOutput(outputId = "scatterPlot", click = "plot_click")
    ),
    box(
      title = "Animation video",
      tags$video(id="video2", type = "video/mp4",src = paste0(video_id, ".mp4"), width = "320px", height = "180px", controls = "controls")
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
