tab_intro <- tabItem(
  tabName = "tab_intro",
  h3("cozmo4hri: interactive tool for emotion classification"),
  p("This app is part of 'Tracking Human Interactions with a Commercially-available Robot over Multiple Days: A Tutorial' by Bishakha Chaudhury, Ruud Hortensius, Martin Hoffmann & Emily Cross"),
  tags$a(href="https://psyarxiv.com/fd3h2/", "Preprint on PsyArXiv", target="_blank"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/", "Github organisation", target="_blank"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/interactive-tool", "Code for this app", target="_blank"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/interactive-tool/blob/master/fullset_shiny.csv", "Download the data", target="_blank"),
  span(" | "),
  tags$a(href="https://github.com/cozmo4hri/animations", "Download the animations", target="_blank"),
  tags$br(),
  tags$br(),
  p("Use this tool to get the validation data for an animation of the Cozmo robot, select and play animations based on valence and/or arousal or explore the impact of confidence and exposure to robots on these ratings"),
  p("Participant (n = 264) rated 10 videos randomly choses from a subset of 348 animations out of the 790 animations"),
  img(src='cozmo.png'),
  tags$br(),
  tags$br(),
  p("Version: 0.5, last update 31 July 2020 by Ruud Hortensius",
    p("Thanks to Jack Taylor for help during the built and the first part of the code is based on", tags$a(href="http://shiny.psy.gla.ac.uk/lmem_sim/", "this code by Lisa DeBruine", target="_blank")))
)

