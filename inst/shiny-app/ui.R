ui <- fluidPage(
  shinyFeedback::useShinyFeedback(),
  titlePanel("groves"),
  
  tabsetPanel(
    tabPanel("Import data",
             fluidRow(column(12, tags$h4("Start by choosing your data."))),
             fluidRow(column(12, radioButtons("data_type", 
                                              "If you would like to work with your own data, choose 'upload my own data'. If you'd like to try out this tool with one of our datasets (made up of Prevotella genomes), choose 'use Prevotella data'.",
                                              choices = c("upload my own data", "use Prevotella data"), 
                                              width = "100%",
             ))),
             fluidRow(column(12, tags$h4("If you choose the Prevotella data, go to the 'Visualize tree set' tab. Otherwise, follow the steps below to upload data."))),
             fluidRow(column(12, tags$h5("1. Upload trees."))),
             fluidRow(
               column(12, fileInput("trees_upload", "Upload a .txt file containing a multiPhylo object including all of the trees you'd like to plot.",
                                    accept = ".txt", width = '50%'))
             ),
             fluidRow(
               column(12, tags$h5("If you would like to rescale the branches of each tree by dividing by the sum of its branch lengths, indicate below. This is only recommended if you don't want your visualization to consider differences in branch lengths between trees.")),
               column(12, radioButtons("rescale_yn", "Rescale branches?", choices = c("yes", "no"), selected = "no", width = "100%"))
             ),
             fluidRow(column(12, tags$h5("2. Give consensus tree information."))),
             fluidRow(
               column(4, radioButtons("consensus_yn", "Does your multiPhylo object include a consensus tree?", choices = c("yes", "no"), selected = "no", width = '100%')),
               column(4, numericInput("consensus_num", "If yes, what is the index of the consensus tree?", value = 1, min = 1, max = 1, width = '100%'))
             ),
             fluidRow(column(12, tags$h5("3. Provide tree names."))),
             fluidRow(
               column(3, fileInput("tree_names_upload", "Upload a .csv file containing the names of your trees, in the same order as the multiPhylo object.",
                                   accept = ".csv", width = '100%')),
               column(1, tags$h5("or")),
               column(3, textAreaInput("tree_names_text", "List tree names with a comma between each in the same order as the multiPhylo object.", 
                                       value = "", width = '100%')),
               column(1, tags$h5("or")),
               column(3, checkboxInput("tree_nums", "Number trees instead of naming them.", width = '100%'))
             ),
             fluidRow(column(12, tags$h5("4. Provide additional information about the trees to add to plot (optional)."))),
             fluidRow(
               column(5, fileInput("tree_char_upload", "Upload a .csv file containing a data frame, where each row represents a tree and trees are in the same order as the multiPhylo object.",
                                   accept = ".csv", width = '90%'))
             )
    ),
    tabPanel("Visualize tree set",
             fluidRow(
               column(1, actionButton("reset", "Reset plot", height = "100%")),
               column(1, numericInput("pc_x",
                                      "X coordinate",
                                      value = 1, min = 1, max = 1, step = 1)),
               column(1, numericInput("pc_y",
                                      "Y coordinate",
                                      value = 2, min = 1, max = 1, step = 1)),
               column(2, selectInput("var_color", 
                                     "Variable to add to plot",
                                     c("none"))),
               column(2, offset = 2, selectInput("tree0_choice",
                                                 "Choose a tree to plot",
                                                 c("")))
             ),
             fluidRow(
               column(1, tags$h5("Proportion variance explained")),
               column(1, textOutput("prop_var_x")),
               column(1, textOutput("prop_var_y"))
             ),
             fluidRow(
               column(6, plotly::plotlyOutput("trees_plot", height = "600px")),
               column(6, plotly::plotlyOutput("tree_plot", height = "600px"))
             ),
             fluidRow(
               column(6, downloadButton("download_trees")),
               column(6, downloadButton("download_tree0"))
             ),
             fluidRow(
               column(3, radioButtons("base_tree",
                                      "Select a base tree (this may take a minute).",
                                      choices = list("minimizer of squared BHV distance to all trees" = "minimizer",
                                                     "other" = "other"),
                                      select = "minimizer", inline = TRUE)),
               column(6, selectInput("other_base_tree",
                                     "base tree",
                                     choices = c("")))
             ),
             fluidRow(
               column(3, radioButtons("red_type",
                                      "Type of dimension reduction. Defaults to PCA of log map transformed trees.",
                                      choices = c("PCA", "MDS"),
                                      selected = "PCA", 
                                      width = '100%')),
               column(3, selectInput("mds_dist",
                                     "If MDS, which distance?",
                                     choices = c("", "BHV", "RF")))
             )
    ),
    tabPanel("Visualize individual trees",
             fluidRow(
               column(3, checkboxInput("midpoint",
                                       "Midpoint root trees?",
                                       value = TRUE)),
               column(3, checkboxInput("scale",
                                       "Show x-axis scale?",
                                       value = TRUE))
             ),
             fluidRow(
               column(2, textOutput("base_name")),
               column(2, checkboxInput("base_support", "Plot gene tree support?")),
               column(2, downloadButton("download_base_tree")),
               column(2,
                      selectInput("tree1_choice",
                                  "Choose a tree to plot",
                                  c(""))),
               column(2, checkboxInput("tree1_support", "Plot gene tree support?")),
               column(2, downloadButton("download_tree1"))
             ),
             fluidRow(
               column(6, plotly::plotlyOutput("base_tree_plot", height = "600px")),
               column(6, plotly::plotlyOutput("chosen_tree1", height = "600px"))
             ),
             fluidRow(
               column(2, selectInput("tree2_choice",
                                     "Choose a tree to plot",
                                     c(""))),
               column(2, checkboxInput("tree2_support", "Plot gene tree support?")),
               column(2, downloadButton("download_tree2")),
               column(2, selectInput("tree3_choice",
                                     "Choose a tree to plot",
                                     c(""))),
               column(2, checkboxInput("tree3_support", "Plot gene tree support?")),
               column(2, downloadButton("download_tree3"))
             ),
             fluidRow(
               column(6, plotly::plotlyOutput("chosen_tree2", height = "600px")),
               column(6, plotly::plotlyOutput("chosen_tree3", height = "600px"))
             )
    )
  )
)
