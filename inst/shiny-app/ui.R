ui <- fluidPage(
  
  # App title ----
  titlePanel("Gene Trees Visualization"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    sidebarPanel(
      
      # Input: Check boxes for the consensus tree ----
      # checkboxGroupInput("cons_tree", "Choose one consensus tree.",
      #                    choices = c("Concatenated Tree","Frechet Mean Tree"),
      #                    selected = "Concatenated Tree",
      #                    inline = FALSE, width = NULL, choiceNames = NULL,
      #                    choiceValues = NULL)
      
      # Input: select which tree is consensus tree
      # selectInput("cons_tree", "Choose the number of the consensus tree",
      #             c(NA, 1:length(tree_paths)))
      
    ),
    
    # Main panel for displaying outputs ----
    
    mainPanel(
      plotly::plotlyOutput("logmap_plot"), 
      plotOutput("base_plot"),
      plotOutput("tree_plot")
    )
  )
)
