ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  titlePanel("groves"),
  
  tabsetPanel(
    tabPanel("Import data"),
    tabPanel("Visualize tree set",
             fluidRow(
               column(2, numericInput("pc_x",
                                      "Coordinate to plot on x-axis",
                                      value = 1, min = 1, max = length(tree_names), step = 1)),
               column(2, numericInput("pc_y",
                                      "Coordinate to plot on y-axis",
                                      value = 2, min = 1, max = length(tree_names), step = 1)),
               column(2, offset = 2,
                      selectInput("tree0_choice",
                                  "Choose a tree to plot",
                                  c("", tree_names)))
             ),
             fluidRow(
               column(6, plotly::plotlyOutput("logmap_plot", height = "600px")),
               column(6, plotOutput("tree_plot", height = "600px"))
             ),
             fluidRow(
               column(6, downloadButton("download_lm")),
               column(6, downloadButton("download_tree0"))
             ),
             fluidRow(
               column(3, radioButtons("base_tree",
                                      "Select a base tree (this may take a minute).",
                                      choices = list("minimizer of squared BHV distance to all trees" = "minimizer",
                                                  "consensus tree" = tree_names[phylogenomic],
                                                  "other" = "other"),
                                      select = "minimizer", inline = TRUE)),
               column(6, selectInput("other_base_tree",
                                     "base tree",
                                     choices = c("")))
             )
    ),
    tabPanel("Visualize individual trees",
             fluidRow(
               column(2, textOutput("base_name")),
               column(2, offset = 2, downloadButton("download_base_tree")),
               column(2,
                      selectInput("tree1_choice", 
                                  "Choose a tree to plot",
                                  c("", tree_names))),
               column(2, offset = 2, downloadButton("download_tree1"))
             ),
             fluidRow(
               column(6, plotOutput("base_tree_plot", height = "600px")),
               column(6, plotOutput("chosen_tree1", height = "600px"))
             ),
             fluidRow(
               column(2, selectInput("tree2_choice",
                                  "Choose a tree to plot",
                                  c("", tree_names))),
               column(2, offset = 2, downloadButton("download_tree2")),
               column(2, selectInput("tree3_choice",
                                  "Choose a tree to plot",
                                  c("", tree_names))),
               column(2, offset = 2, downloadButton("download_tree3"))
             ),
             fluidRow(
               column(6, plotOutput("chosen_tree2", height = "600px")),
               column(6, plotOutput("chosen_tree3", height = "600px"))
             )
    )
  )
)
