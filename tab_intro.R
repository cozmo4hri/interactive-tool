tab_intro <- tabItem(
  tabName = "tab_intro",
  h3("cozmo4hri: interactive tool for emotion classification"),
  p("This app is part of 'Tracking Human Interactions with a Commercially-available Robot over Multiple Days: A Tutorial' by Bishakha Chaudhury, Ruud Hortensius, Martin Hoffmann & Emily Cross"),
  tags$a(href="https://psyarxiv.com/...", "Preprint on PsyArXiv"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/", "Github repository"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/interactive-tool", "Code for this app"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/interactive-tool/blob/master/fullset_shiny.csv", "Download the data"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/animations", "Download the animations"),
  tags$br(),
  tags$br(),
  p("Use this tool to select animations of the Cozmo robot based on valence and/or arousal or explore the impact of confidence and exposure to robots on these ratings"),
  p("Participant (n = 264) rated 10 videos randomly choses from a subset of 348 animations out of the 790 animations"),
  img(src='cozmo.png'),
  p("Version: 0.5, last update 29 July 2020, Ruud Hortensius"),
  tags$a(href="http://shiny.psy.gla.ac.uk/lmem_sim/", "First part of the code is based on this code by Lisa DeBruine")
)

