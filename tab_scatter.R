### tab_scatter ----
tab_scatter <- tabItem(
  tabName = "tab_scatter",
  column(
    width = 12,
    box(
      title = "Animation group",
      #width = NULL,
      plotlyOutput(outputId = "scatterPlot", height = "auto")
    ),
    box(
      title = "Animation group",
      tags$video(id="video2", type = "video/mp4",src = "1.mp4", width = "320px", height = "180px", controls = "controls")
    )
  )
)

#1280 × 720