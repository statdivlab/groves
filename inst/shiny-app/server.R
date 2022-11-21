server <- function(input, output, session) {
  
  # calculate log map vectors
  lm_res <- compute_logmap(tree_paths = tree_paths,
                           tree_names = tree_names)
  # make plot
  plot_res <- plot_logmap(vectors = lm_res$vectors, phylogenomic = phylogenomic,
                          title = "Prevotella Trees",
                          tree_names = tree_names,
                          phylogenomic_name = "phylogenomic")
  
  
  # generate plot in app
  output$logmap_plot <- plotly::renderPlotly({
    ggplotly(plot_res$plot, tooltip = "Name", source = "logmap_plot")
  })
  
  # plot base tree 
  base_index <- phylogenomic
  output$base_name <- renderText(tree_names[base_index])
  base_tree <- ape::read.tree(tree_paths[base_index])
  base_plot <- ggtree(base_tree) + geom_tiplab(size = 2.5) 
  output$base_tree_plot <- renderPlot({base_plot})
  output$chosen_tree1 <- renderPlot({
    req(input$tree1_choice)
    tree1_name <- input$tree1_choice
    tree1_ind <- which(tree_names == tree1_name)
    ggtree(ape::read.tree(tree_paths[tree1_ind])) + 
      geom_tiplab(size = 2.5)
  })
  output$chosen_tree2 <- renderPlot({
    req(input$tree2_choice)
    tree2_name <- input$tree2_choice
    tree2_ind <- which(tree_names == tree2_name)
    ggtree(ape::read.tree(tree_paths[tree2_ind])) + 
      geom_tiplab(size = 2.5)
  })
  output$chosen_tree3 <- renderPlot({
    req(input$tree3_choice)
    tree3_name <- input$tree3_choice
    tree3_ind <- which(tree_names == tree3_name)
    ggtree(ape::read.tree(tree_paths[tree3_ind])) + 
      geom_tiplab(size = 2.5)
  })
  
  observeEvent(event_data("plotly_click", source = "logmap_plot"), {
    req(event_data("plotly_click", source = "logmap_plot"))
    ed <- event_data("plotly_click", source = "logmap_plot")
    x <- ed$x
    y <- ed$y
    ind <- which.min(rowSums(plot_res$pc1_info[, 1:2] - c(x, y))^2)
    tree_name <- plot_res$pc1_info$Name[ind]
    output$click_name <- renderPrint(tree_name)
    #tree_plot <- ggtree(ape::read.tree(tree_paths[which(tree_names == tree_name)])) + 
    #  geom_tiplab(size = 2.5) + 
    #  ggtitle(paste0("Estimated ", tree_name, " tree")) + 
    #  theme(plot.title = element_text(hjust = 0.5))
    #output$tree_plot <- renderPlot({tree_plot})
  })
  
}
