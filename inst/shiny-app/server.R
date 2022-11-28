server <- function(input, output, session) {
  
  tree_data <- reactive({
    req(input$trees_upload)
    ape::read.tree(input$trees_upload$datapath)
  })
  
  n_tree <- reactive({
    req(tree_data())
    length(tree_data())
  })
  
  n <- length(tree_paths)
  
  observeEvent(input$consensus_yn, {
    req(n_tree())
    updateNumericInput(session, "consensus_num",
                       "If yes, what is the index of the consensus tree?",
                       value = 1, min = 1, max = n_tree())
  })
  
  output$num_trees <- renderPrint(length(tree_data()))  
  #output$num_trees <- renderPrint(input$trees_upload)  
  
  # get tree with minimum BHV distance to other trees
  txt_path <- "full_trees_file.txt"
  merge_txt(tree_paths, txt_path)
  dists <- compute_geodesic(txt_path)
  unlink(txt_path)
  sq_dists <- dists^2
  diag(sq_dists) <- NA
  mean_dists <- rowMeans(sq_dists, na.rm = TRUE)
  min_tree_number <- which.min(mean_dists)
  binary <- check_binary(tree_path = tree_paths[min_tree_number])
  if (!binary) {
    which_binary <- check_binary(tree_path = txt_path)
    if (sum(which_binary) == 0) {
      stop("No trees in the tree set are binary (they are all unresolved). Because of this, this visualization tool cannot be run on this tree set.")
    }
    mean_dists_binary <- mean_dists
    mean_dists_binary[!which_binary] <- Inf
    min_tree_number <- which.min(mean_dists_binary)
  }
  min_lab <- tree_names[min_tree_number]
  
  # observe base tree for log map plot
  observeEvent((input$base_tree), {
    if (input$base_tree == "other") {
      updateSelectInput(session, 
                        "other_base_tree",
                        "base tree",
                        choices = c("", tree_names))
    } else if (input$base_tree == "minimizer") {
      updateSelectInput(session, 
                        "other_base_tree",
                        "base tree",
                        choices = c(min_lab))
    } else {
      updateSelectInput(session, 
                        "other_base_tree",
                        "base tree",
                        choices = c(input$base_tree))
    }
  })
  
  logmap_res <- eventReactive((input$other_base_tree), {
    computing <- showNotification("Computing log map...", duration = NULL,
                                  closeButton = FALSE, type = "message")
    on.exit(removeNotification(computing), add = TRUE)
    # get base tree from input
    req(input$base_tree)
    if (!(input$base_tree %in% c("minimizer", "other"))) {
      base <- input$base_tree
    } else if (input$base_tree == "minimizer") {
      base <- min_lab
    } else {
      if (!isTruthy(input$other_base_tree)) {
        shinyFeedback::feedbackWarning("other_base_tree",
                                       !isTruthy(input$other_base_tree),
                                       "Please select other tree.")
      }
      req(input$other_base_tree)
      base <- input$other_base_tree
    }
    compute_logmap(tree_paths = tree_paths,
                   tree_names = tree_names,
                   base_lab = base)
  })
  
  # make sure that coordinate for x-axis is valid, if not revert to coord 1
  pc_x <- reactive({
    out_range <- input$pc_x > n
    shinyFeedback::feedbackWarning("pc_x", out_range, 
                                   paste0("Please select a number less than ", n, "."))
    req(input$pc_x)
    if (out_range) {1}
    else {input$pc_x}
  })
  
  # make sure that coordinate for y-axis is valid, if not revert to coord 2
  pc_y <- reactive({
    out_range <- input$pc_y > n
    shinyFeedback::feedbackWarning("pc_y", out_range, 
                                   paste0("Please select a number less than ", n, "."))
    req(input$pc_y)
    if (out_range) {2}
    else {input$pc_y}
  })
  
  # run plot_logmap
  plot_res <- reactive({
    lm_res <- logmap_res()
    plot_logmap(vectors = lm_res$vectors, phylogenomic = phylogenomic,
                          title = "Prevotella Trees",
                          tree_names = tree_names,
                          phylogenomic_name = "phylogenomic",
                          x_axis = pc_x(),
                          y_axis = pc_y())
    })
  
  # make reactive for plot 
  logmap_plot <- reactive({
    req(plot_res())
    plot_result <- plot_res()
    # if tree 0 has been chosen, color it blue
    if (isTruthy(input$tree0_choice)) {
      tree0_name <- input$tree0_choice
      tree0_ind <- which(plot_result$pc_x_info$Name == tree0_name)
      plot_with_point <- plot_result$plot + 
        geom_point(data = plot_result$pc_x_info[tree0_ind, ],
                   aes(x = pc_x, y = pc_y), 
                   color = "blue")
      plot_with_point
    } else {
      plot_result$plot
    }
  })
  
  logmap_plotly <- eventReactive(logmap_plot(), {
    ggplotly(logmap_plot(), tooltip = "Name", source = "logmap_plot")
  })
  
  # render log map plot 
  observeEvent(logmap_plotly(), {
    output$logmap_plot <- plotly::renderPlotly(logmap_plotly())
  })
  
  output$download_lm <- downloadHandler(
    filename = function() {
      paste('tree_set_plot', '.png', sep = '')
    },
    content = function(file) {
      req(logmap_plot())
      ggsave(file, plot = logmap_plot(), device = 'png', height = 8.5, width = 8.5)
    }
  )
  
  # function to plot single tree
  plot_tree <- function(tree_name = NULL, tree_ind = NULL) {
    if (is.null(tree_ind)) {
      tree_ind <- which(tree_names == tree_name)
    }
    tree <- phytools::midpoint.root(ape::read.tree(tree_paths[tree_ind]))
    tree_plot <- ggtree(tree) + geom_tiplab(size = 2.5)
    return(tree_plot)
  }
  
  # when user clicks on point in log map plot, update input `tree0_choice`
  observeEvent(event_data("plotly_click", source = "logmap_plot"), {
    req(event_data("plotly_click", source = "logmap_plot"))
    plot_result <- plot_res()
    ed <- event_data("plotly_click", source = "logmap_plot")
    x <- ed$x
    y <- ed$y
    ind <- which.min(rowSums(plot_result$pc_x_info[, 1:2] - c(x, y))^2)
    tree_name <- plot_result$pc_x_info$Name[ind]
    updateSelectInput(inputId = "tree0_choice",
                      label = "Choose a tree to plot",
                      choices = c("", tree_names),
                      selected = tree_name)
  })
  
  # when user inputs tree to plot, plot that tree on right of log map tab 
  tree0_plot <- eventReactive(input$tree0_choice, {
    req(input$tree0_choice)
    plot_tree(tree_name = input$tree0_choice)
  })
  
  observeEvent(tree0_plot(), {
    output$tree_plot <- renderPlot(tree0_plot())
  })
  
  # download tree 0 
  output$download_tree0 <- downloadHandler(
    filename = function() {
      req(input$tree0_choice)
      paste(input$tree0_choice, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(tree0_plot())
      ggsave(file, plot = tree0_plot(), device = 'png', height = 8, width = 8)
    }
  )
  
  # plot individual trees in 3rd tab
  # plot base tree 
  output$base_name <- renderText({
    req(input$other_base_tree)
    paste0("Base tree: ", input$other_base_tree)
  })
  base_tree_plot <- eventReactive(input$other_base_tree, {
    req(input$other_base_tree)
    plot_tree(tree_name = input$other_base_tree)
  })
  observeEvent(base_tree_plot(), {
    output$base_tree_plot <- renderPlot(base_tree_plot())
  })
  output$download_base_tree <- downloadHandler(
    filename = function() {
      req(input$other_base_tree)
      paste(input$other_base_tree, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(base_tree_plot())
      ggsave(file, plot = base_tree_plot(), device = 'png', height = 8, width = 8)
    }
  )
  # plot tree in upper right of third tab 
  tree1_plot <- eventReactive(input$tree1_choice, {
    req(input$tree1_choice)
    plot_tree(tree_name = input$tree1_choice)
  })
  observeEvent(tree1_plot(), {
    output$chosen_tree1 <- renderPlot(tree1_plot())
  })
  output$download_tree1 <- downloadHandler(
    filename = function() {
      req(input$tree1_choice)
      paste(input$tree1_choice, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(tree1_plot())
      ggsave(file, plot = tree1_plot(), device = 'png', height = 8, width = 8)
    }
  )
  # plot tree in lower left of third tab
  tree2_plot <- eventReactive(input$tree2_choice, {
    req(input$tree2_choice)
    plot_tree(tree_name = input$tree2_choice)
  })
  observeEvent(tree2_plot(), {
    output$chosen_tree2 <- renderPlot(tree2_plot())
  })
  output$download_tree2 <- downloadHandler(
    filename = function() {
      req(input$tree2_choice)
      paste(input$tree2_choice, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(tree2_plot())
      ggsave(file, plot = tree2_plot(), device = 'png', height = 8, width = 8)
    }
  )
  # plot tree in lower right of third tab
  tree3_plot <- eventReactive(input$tree3_choice, {
    req(input$tree3_choice)
    plot_tree(tree_name = input$tree3_choice)
  })
  observeEvent(tree3_plot(), {
    output$chosen_tree3 <- renderPlot(tree3_plot())
  })
  output$download_tree3 <- downloadHandler(
    filename = function() {
      req(input$tree3_choice)
      paste(input$tree3_choice, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(tree3_plot())
      ggsave(file, plot = tree3_plot(), device = 'png', height = 8, width = 8)
    }
  )
}
