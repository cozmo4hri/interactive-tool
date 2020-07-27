tab_intro <- tabItem(
  tabName = "tab_intro",
  h3("cozmo4hri: interactive tool for emotion classification"),
  p("This app is part of 'Tracking Human Interactions with a Commercially-available Robot over Multiple Days: A Tutorial' by Bishakha Chaudhury, Ruud Hortensius, Martin Hoffmann, Emily Cross"),
  tags$a(href="https://psyarxiv.com/...", "Preprint on PsyArXiv"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/", "Github repository"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/interactive-tool", "Code for this app"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/interactive-tool/blob/master/fullset_shiny.csv", "Download data"),
  tags$br(),
  tags$br(),
  img(src='cozmo.png'),
  p("To be added: explanation of the three taps. More on shiny")
)

