### tab_density ----
tab_density <- tabItem(
  tabName = "tab_density",
  column(
    width = 12,
    length = 2000,
    box(
      title = "Density",
      #width = NULL,
      plotOutput(outputId = "densPlot")
    )
  )
)
