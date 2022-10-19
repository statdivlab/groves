server <- function(input, output, session) {
  
  # get number of trees in paths 
  #output$tree_num <- renderText({as.character(length(tree_paths))})
  
  # calculate log map vectors
  lm_res <- compute_logmap(tree_paths = tree_paths,
                           tree_names = c(gene_names, "phylogenomic"))
  # make plot
  base_index <- 64
  plot_res <- plot_logmap(vectors = lm_res$vectors, phylogenomic = base_index,
                          title = "Prevotella Trees",
                          tree_names = c(gene_names, "phylogenomic"),
                          phylogenomic_name = "phylogenomic",
                          use_plotly = TRUE)
  
  # generate plot in app
  output$logmap_plot <- plotly::renderPlotly({
    plot_res$plot
  })
  
  # plot base tree 
  base_tree <- ape::read.tree(tree_paths[base_index])
  base_plot <- ggtree(base_tree) + geom_tiplab(size = 2)
  output$base_plot <- renderPlot({base_plot})
  
  # plot other trees
  tree_index <- reactiveVal(value = 1, label = "tree_index")
  #observeEvent(input$)
  tree_index <- which(gene_names == tree_to_plot)
  tree_plot <- ggtree(ape::read.tree(tree_paths[tree_index])) + 
    geom_tiplab(size = 2)
  output$tree_plot <- renderPlot({tree_plot})
  
  # observe the tree clicked on 
  observeEvent(
    eventExpr = input$logmap_click,
    handlerExpr = {
      tree_to_plot <- nearPoints(plot_res$pc1_df, input$logmap_click, 
                                 xvar = pc1, yvar = pc2, maxpoints = 1) %>%
        pull(name)
    }
  )
  
}
