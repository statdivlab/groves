ui <- fluidPage(
  waiter::use_waiter(),
  # App title ----
  titlePanel("groves"),
  
  tabsetPanel(
    tabPanel("Import data"),
    tabPanel("Visualize tree set",
      fluidRow(
        column(6, plotly::plotlyOutput("logmap_plot", height = "600px")),
        #column(6, plotOutput("tree_plot", height = "600px"))
        column(6, verbatimTextOutput("click_name"))
      )
    ),
    tabPanel("Visualize individual trees",
      fluidRow(
        column(2, textOutput("base_name")),
        column(2, offset = 4,
               selectInput("tree1_choice", 
                           "Choose a tree to plot",
                           c("", tree_names)))
      ),
      fluidRow(
        column(6, plotOutput("base_tree_plot", height = "600px")),
        column(6, plotOutput("chosen_tree1", height = "600px"))
      ),
      fluidRow(
        column(2, offset = 2,
               selectInput("tree2_choice",
                           "Choose a tree to plot",
                           c("", tree_names))),
        column(2, offset = 4,
               selectInput("tree3_choice",
                           "Choose a tree to plot",
                           c("", tree_names)))
      ),
      fluidRow(
        column(6, plotOutput("chosen_tree2", height = "600px")),
        column(6, plotOutput("chosen_tree3", height = "600px"))
      )
    )
  )
)
