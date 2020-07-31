### tab_density ----
tab_density <- tabItem(
  tabName = "tab_density",
  p("Explore the data across animations and participants"),
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
