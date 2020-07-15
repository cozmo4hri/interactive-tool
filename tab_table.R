### tab_table ----
tab_table <- tabItem(
  tabName = "tab_table",
  fluidRow(
    box(
      title = "Table",
      width = 12,
      DT::dataTableOutput("tableDF")
    )
  )
)
#tabPanel("Table", tableOutput("table"))