### tab_scatter ----
tab_scatter <- tabItem(
  tabName = "tab_scatter",
  column(
    width = 12,
    box(
      title = "Animation group",
      #width = NULL,
      plotlyOutput(outputId = "scatterPlot", height = "auto")
    )
  )
)

