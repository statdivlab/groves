server <- function(input, output, session) {
  
  # if user wants to use Prevotella data, update all required fields 
  observe({
    if (input$data_type == "use Prevotella data") {
      updateRadioButtons(session, "consensus_yn", 
                         "Does your multiPhylo object include a consensus tree?", 
                         choices = c("yes", "no"), selected = "yes")
    } else {
      updateSelectInput(session, "var_color", 
                        "Variable to add to plot",
                        c("none"))
    }
  })
  
  # get tree multiPhylo object
  raw_tree_data <- reactive({
    if (input$data_type == "use Prevotella data") {
      ape::read.tree("../prevotella/all_trees.txt")
    } else {
      req(input$trees_upload)
      ape::read.tree(input$trees_upload$datapath)
    }
  })
  
  # rescale if needed 
  tree_data <- reactive({
    req(raw_tree_data)
    if (input$rescale_yn == "yes") {
      groves::standardize_branches(raw_tree_data(), denom = "branch_sum")
    } else {
      raw_tree_data()
    }
  })
  
  # get number of trees in tree data
  n_tree <- reactive({
    req(tree_data())
    length(tree_data())
  })
  
  gene_trees <- reactive({
    req(tree_data())
    req(n_tree())
    req(input$consensus_num)
    req(input$consensus_yn)
    if (input$consensus_yn == "no") {
      tree_data()
    } else {
      tree_data()[-input$consensus_num]
    }
  })
  
  # when trees are uploaded, update consensus number input 
  observeEvent(n_tree(), {
    req(n_tree())
    if (input$data_type == "use Prevotella data") {
      updateNumericInput(session, "consensus_num", "
                         If yes, what is the index of the consensus tree?", 
                         value = 64, min = 1, max = 64)
    } else {
      updateNumericInput(session, "consensus_num",
                         "If yes, what is the index of the consensus tree?",
                         value = 1, min = 1, max = n_tree())
    }
  })
  
  # update x and y coordinate options based on number of trees
  observeEvent(n_tree(), {
    updateNumericInput(session, "pc_x",
                       "X coordinate",
                       value = 1, min = 1, max = n_tree(), step = 1)
    updateNumericInput(session, "pc_y",
                       "Y coordinate",
                       value = 2, min = 1, max = n_tree(), step = 1)
  })
  
  # reset log map plot and tree0 
  observeEvent(input$reset, {
    if (input$red_type == "PCA") {
      max_val <- n_tree()
    } else if (input$red_type == "MDS") {
      max_val <- n_tree() -  1
    }
    updateNumericInput(session, "pc_x",
                       "X coordinate",
                       value = 1, min = 1, max = max_val, step = 1)
    updateNumericInput(session, "pc_y",
                       "Y coordinate",
                       value = 2, min = 1, max = max_val, step = 1)
    updateSelectInput(session, "var_color", 
                      "Variable to add to plot", selected = "none",
                      choices = c("none", names(extra_tree_data())))
    updateSelectInput(session, "tree0_choice",
                      "Choose a tree to plot",
                      c("", tree_names()))
  })
  
  # if there is a consensus tree, upload base tree selection input 
  observeEvent(input$consensus_yn, {
    if (input$consensus_yn == "yes") {
      updateRadioButtons(session, "base_tree",
                         "Select a base tree (this may take a minute).",
                         choices = list("minimizer of squared BHV distance to all trees" = "minimizer",
                                        "consensus tree" = "consensus",
                                        "other" = "other"))
    } else {
      updateRadioButtons(session, "base_tree",
                         "Select a base tree (this may take a minute).",
                         choices = list("minimizer of squared BHV distance to all trees" = "minimizer",
                                        "other" = "other"))
    }
  })
  
  # save tree name info 
  tree_names <- reactive({
    if (input$data_type == "use Prevotella data") {
      read.csv("../prevotella/tree_names.csv")$x
    } else {
      if (isTruthy(input$tree_names_upload)) {
        req(input$tree_names_upload)
        shinyFeedback::hideFeedback("tree_names_upload")
        names_tmp <- read.csv(input$tree_names_upload$datapath)$x
        shinyFeedback::feedbackWarning("tree_names_upload",
                                       length(names_tmp) != n_tree(),
                                       paste0("The length of your tree names is ", length(names_tmp)," and the number of trees in your multiPhylo file is ", n_tree(), "."))
        names_tmp
      } else if (isTruthy(input$tree_names_text)) {
        shinyFeedback::hideFeedback("tree_names_text")
        req(input$tree_names_text)
        names_tmp <- trimws(unlist(strsplit(input$tree_names_text, ",")))
        shinyFeedback::feedbackWarning("tree_names_text",
                                       length(names_tmp) != n_tree(),
                                       paste0("The length of your tree names is ", length(names_tmp)," and the number of trees in your multiPhylo file is ", n_tree(), "."))
        if (length(names_tmp) == n_tree()) {
          names_tmp
        } 
      } else if (isTruthy(input$tree_nums)) {
        req(n_tree())
        as.character(1:n_tree())
      }
    }
  })
  
  # update trees to plot based on tree_names 
  observeEvent(tree_names(), {
    updateSelectInput(session, "tree0_choice",
                      "Choose a tree to plot",
                      c("", tree_names()))
    updateSelectInput(session, "tree1_choice",
                      "Choose a tree to plot",
                      c("", tree_names()))
    updateSelectInput(session, "tree2_choice",
                      "Choose a tree to plot",
                      c("", tree_names()))
    updateSelectInput(session, "tree3_choice",
                      "Choose a tree to plot",
                      c("", tree_names()))
  })
  
  # save tree additional data 
  extra_tree_data <- eventReactive(c(input$data_type, input$tree_char_upload), {
    if (input$data_type == "use Prevotella data") {
      read.csv("../prevotella/extra_tree_data.csv")
    } else {
      if (isTruthy(input$tree_char_upload)) {
        shinyFeedback::hideFeedback("tree_char_upload")
        df <- read.csv(input$tree_char_upload$datapath)
        shinyFeedback::feedbackWarning("tree_char_upload",
                                       nrow(df) != n_tree(),
                                       paste0("The length of your tree characteristics dataset is ", nrow(df)," and the number of trees in your multiPhylo file is ", n_tree(), "."))
        df
      } else {
        NULL
      }
    }
  })
  
  # update var color input 
  observeEvent(extra_tree_data(), {
    req(extra_tree_data())
    updateSelectInput(session, "var_color", 
                      "Variable to add to plot", selected = "none",
                      choices = c("none", names(extra_tree_data())))
  })
  
  # write each tree in the multiPhylo file to its own txt file and save paths 
  tree_paths <- reactive({
    if (!file.exists("groves_data")) {
      dir.create("groves_data")
    }
    req(tree_data()) 
    trees <- tree_data()
    req(n_tree())
    path <- paste0("groves_data/tree_", 1, ".txt")
    ape::write.tree(trees[[1]], path)
    paths <- path
    for (i in 2:n_tree()) {
      path <- paste0("groves_data/tree_", i, ".txt")
      ape::write.tree(trees[[i]], path)
      paths <- c(paths, path)
    }
    paths
  })
  
  # get distances between trees 
  tree_dists <- reactive({
    req(tree_data()) 
    req(tree_names())
    tree_data_tmp <- tree_data()
    computing <- showNotification("Computing distances between trees...", duration = NULL,
                                  closeButton = FALSE, type = "message")
    on.exit(removeNotification(computing), add = TRUE)
    ape::write.tree(tree_data_tmp, "groves_data/all_trees.txt")
    dists <- groves::compute_geodesic("groves_data/all_trees.txt")
  })
  
  min_ind <- reactive({
    req(tree_paths())
    tree_paths_tmp <- tree_paths()
    req(tree_dists())
    dists <- tree_dists()
    sq_dists <- dists^2
    diag(sq_dists) <- NA
    mean_dists <- rowMeans(sq_dists, na.rm = TRUE)
    min_tree_number <- which.min(mean_dists)
    binary <- groves::check_binary(tree_path = tree_paths_tmp[min_tree_number])
    if (!binary) {
      which_binary <- groves::check_binary(tree_path = "groves_data/all_trees.txt")
      if (sum(which_binary) == 0) {
        stop("No trees in the tree set are binary (they are all unresolved). Because of this, this visualization tool cannot be run on this tree set.")
      }
      mean_dists_binary <- mean_dists
      mean_dists_binary[!which_binary] <- Inf
      min_tree_number <- which.min(mean_dists_binary)
    }
    min_tree_number
  })
  
  min_lab <- reactive({
    req(min_ind())
    req(tree_names())
    tree_names_tmp <- tree_names()
    tree_names_tmp[min_ind()]
  })
  
  # observe base tree for log map plot
  observeEvent(c(input$base_tree, min_lab(), input$data_type), {
    req(input$red_type == "PCA")
    req(min_lab())
    req(tree_names())
    req(input$consensus_num)
    if (input$base_tree == "other") {
      updateSelectInput(session,
                        "other_base_tree",
                        "base tree",
                        choices = c("", tree_names()))
    } else if (input$base_tree == "minimizer") {
      updateSelectInput(session,
                        "other_base_tree",
                        "base tree",
                        choices = c(min_lab()))
    } else {
      updateSelectInput(session,
                        "other_base_tree",
                        "base tree",
                        choices = c(tree_names()[input$consensus_num]))
    }
  })
  
  observeEvent(input$other_base_tree, {
    req(input$red_type == "PCA")
    shinyFeedback::feedbackWarning("other_base_tree",
                                   !isTruthy(input$other_base_tree),
                                   "Please select other tree.")
  })
  
  #compute logmap
  logmap_res <- eventReactive(c(input$other_base_tree, tree_names()), {
    req(input$red_type == "PCA")
    req(tree_paths())
    req(tree_names())
    req(isTruthy(input$other_base_tree))
    req(input$other_base_tree %in% tree_names())
    computing <- showNotification("Computing log map...", duration = NULL,
                                  closeButton = FALSE, type = "message")
    on.exit(removeNotification(computing), add = TRUE)
    # get base tree from input
    req(input$base_tree)
    req(input$consensus_num)
    if (input$base_tree == "consensus") {
      base <- tree_names()[input$consensus_num]
    } else if (input$base_tree == "minimizer") {
      base <- min_lab()
    } else {
      req(input$other_base_tree)
      base <- input$other_base_tree
    }
    tree_paths_temp <- tree_paths()
    tree_names_temp <- tree_names()
    groves::compute_logmap(tree_paths = tree_paths_temp,
                           tree_names = tree_names_temp,
                           base_lab = base)
  })
  
  # make sure that coordinate for x-axis is valid, if not revert to coord 1
  pc_x <- reactive({
    req(n_tree())
    if (input$red_type == "PCA") {
      max_val = n_tree()
    } else if (input$red_type == "MDS") {
      max_val = n_tree() - 1
    }
    out_range <- input$pc_x > max_val
    shinyFeedback::feedbackWarning("pc_x", out_range,
                                   paste0("Please select a number less than ", max_val, "."))
    req(input$pc_x)
    if (out_range) {1}
    else {input$pc_x}
  })
  
  # make sure that coordinate for y-axis is valid, if not revert to coord 2
  pc_y <- reactive({
    req(n_tree())
    req(n_tree())
    if (input$red_type == "PCA") {
      max_val = n_tree()
    } else if (input$red_type == "MDS") {
      max_val = n_tree() - 1
    }
    out_range <- input$pc_y > max_val
    shinyFeedback::feedbackWarning("pc_y", out_range,
                                   paste0("Please select a number less than ", max_val, "."))
    req(input$pc_y)
    if (out_range) {2}
    else {input$pc_y}
  })
  
  # run plot_logmap
  plot_res <- reactive({
    req(logmap_res())
    req(tree_names())
    lm_res <- logmap_res()
    if (input$consensus_yn == "no") {
      if (input$var_color == "none") {
        groves::plot_logmap(vectors = lm_res$vectors, 
                            title = "Log map representation",
                            tree_names = tree_names(),
                            x_axis = pc_x(),
                            y_axis = pc_y())
      } else {
        groves::plot_logmap(vectors = lm_res$vectors, 
                            title = "Log map representation",
                            tree_names = tree_names(),
                            x_axis = pc_x(),
                            y_axis = pc_y(),
                            group = extra_tree_data()[, input$var_color])
      }
    } else {
      req(input$consensus_num)
      if (input$var_color == "none") {
        groves::plot_logmap(vectors = lm_res$vectors, phylogenomic = input$consensus_num,
                            title = "Log map representation",
                            tree_names = tree_names(),
                            phylogenomic_name = tree_names()[input$consensus_num],
                            x_axis = pc_x(),
                            y_axis = pc_y())
      } else {
        groves::plot_logmap(vectors = lm_res$vectors, phylogenomic = input$consensus_num,
                            title = "Log map representation",
                            tree_names = tree_names(),
                            phylogenomic_name = tree_names()[input$consensus_num],
                            x_axis = pc_x(),
                            y_axis = pc_y(),
                            group = extra_tree_data()[, input$var_color])
      }
    }
  })
  
  # ouput proportion of variance corresponding with x and y pc 
  output$prop_var_x <- renderText({
    if (input$red_type == "PCA") {
      paste0(round(plot_res()$prop_var[1]*100, 0), "%")
    } else {
      "NA"
    }
  })
  output$prop_var_y <- renderText({
    if (input$red_type == "PCA") {
      paste0(round(plot_res()$prop_var[2]*100, 0), "%")
    } else {
      "NA"
    }
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
        if (input$var_color == "none") {
          ggplot2::geom_point(data = plot_result$pc_x_info[tree0_ind, ],
                              ggplot2::aes(x = pc_x, y = pc_y),
                              color = "blue")
        }
      plot_with_point
    } else {
      plot_result$plot
    }
  })
  
  trees_plotly <- reactive({
    req(isTruthy(logmap_plot()) || isTruthy(MDS_plot()))
    if (input$red_type == "PCA") {
      plotly::ggplotly(logmap_plot(), tooltip = "Name", source = "trees_plot")
    } else {
      plotly::ggplotly(MDS_plot(), tooltip = "Name", source = "trees_plot")
    }
  })
  
  # render log map plot
  observeEvent(trees_plotly(), {
    output$trees_plot <- plotly::renderPlotly(trees_plotly())
  })
  
  output$download_trees <- downloadHandler(
    filename = function() {
      paste('tree_set_plot', '.png', sep = '')
    },
    content = function(file) {
      if (input$red_type == "PCA") {
        req(logmap_plot())
        ggplot2::ggsave(file, plot = logmap_plot(), device = 'png', height = 8.5, width = 8.5)
      } else {
        req(MDS_plot())
        ggplot2::ggsave(file, plot = MDS_plot(), device = 'png', height = 8.5, width = 8.5)
      }
    }
  )
  
  # function to plot single tree
  plot_tree <- function(tree_name = NULL, tree_ind = NULL, add_support = FALSE, gene_trees = NULL) {
    if (is.null(tree_ind)) {
      tree_ind <- which(tree_names() == tree_name)
    }
    if (input$midpoint) {
      tree <- phangorn::midpoint(ape::read.tree(tree_paths()[tree_ind]))
    } else {
      tree <- ape::read.tree(tree_paths()[tree_ind])
    }
    if (!add_support) {
      tree_plot <- ggtree::ggtree(tree) + ggtree::geom_tiplab(size = 2.5) 
    } else {
      if (input$midpoint == TRUE) {
        gene_trees <- phangorn::midpoint(gene_trees)
      } 
      rooted <- ape::is.rooted(tree)
      support <- groves::check_gene_support(tree, gene_trees, rooted)
      tree_plot <- groves::plot_support(tree, support, lab_size = 2.5, supp_size = 2.5, color_branch = TRUE,                          
                                        title ="")
    }
    if (input$scale) {
      tree_plot <- tree_plot + ggtree::theme_tree2()
    }
    return(tree_plot)
  }
  
  plotly_tree <- function(tree_plot) {
    tree_plotly <- tree_plot + 
      ggplot2::geom_text(ggplot2::aes(label = label), 
                         color = "black", 
                         nudge_x = max(tree_plot$data$x)*0.08,
                         hjust = 0,
                         size = 3)
  }
  
  # when user clicks on point in low dimension plot, update input `tree0_choice`
  observeEvent(plotly::event_data("plotly_click", source = "trees_plot"), {
    req(plotly::event_data("plotly_click", source = "trees_plot"))
    ed <- plotly::event_data("plotly_click", source = "trees_plot")
    x <- ed$x
    y <- ed$y
    if (input$red_type == "PCA") {
      plot_result <- plot_res()
      ind <- which.min((plot_result$pc_x_info$pc_x - x)^2 + 
                         (plot_result$pc_x_info$pc_y - y)^2)
      tree_name <- plot_result$pc_x_info$Name[ind]
    } else {
      df <- mds_res()$df
      ind <- which.min((df[, 1] - x)^2 + 
                         (df[, 2] - y)^2)
      tree_name <- tree_names()[ind]
    }
    updateSelectInput(inputId = "tree0_choice",
                      label = "Choose a tree to plot",
                      choices = c("", tree_names()),
                      selected = tree_name)
  })
  
  # when user inputs tree to plot, plot that tree on right of log map tab
  tree0_plot <- eventReactive(input$tree0_choice, {
    req(input$tree0_choice)
    plot_tree(tree_name = input$tree0_choice)
  })
  
  tree0_plotly <- reactive({
    req(tree0_plot)
    plotly_tree(tree0_plot())
  })
  
  observeEvent(tree0_plotly(), {
    output$tree_plot <- plotly::renderPlotly(plotly::ggplotly(tree0_plotly(), tooltip = ""))
  })
  
  # download tree 0
  output$download_tree0 <- downloadHandler(
    filename = function() {
      req(input$tree0_choice)
      paste(input$tree0_choice, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(tree0_plot())
      ggplot2::ggsave(file, plot = tree0_plot(), device = 'png', height = 8, width = 8)
    }
  )
  
  # switch over to MDS 
  observeEvent(input$red_type, {
    req(n_tree())
    if (input$red_type == "MDS") {
      updateNumericInput(session, "pc_x",
                         "X coordinate",
                         value = 1, min = 1, max = n_tree() - 1, step = 1)
      updateNumericInput(session, "pc_y",
                         "Y coordinate",
                         value = 2, min = 1, max = n_tree() - 1, step = 1)
    }
  })
  
  mds_dists <- eventReactive(input$mds_dist, {
    req(isTruthy(input$mds_dist))
    if (input$red_type == "MDS") {
      if (input$mds_dist == "RF") {
        as.matrix(phangorn::RF.dist(tree_data()))
      } else {
        req(tree_dists())
        tree_dists()
      }
    }
  }, ignoreInit = TRUE)
  
  mds_res <- reactive({
    req(mds_dists())
    #groves::compute_MDS(dist_metric = input$mds_dist,
    compute_MDS(dist_metric = input$mds_dist,
                        dist_matrix = mds_dists(),
                        tree_names = tree_names(),
                        x_axis = pc_x(), 
                        y_axis = pc_y())
  })
  
  # run plot_MDS
  plot_MDS_res <- reactive({
    req(mds_res())
    df <- mds_res()$df
    req(tree_names())
    req(input$consensus_yn)
    if (input$consensus_yn == "no") {
      if (input$var_color == "none") {
        #groves::plot_MDS(df = df, x_axis = pc_x(), y_axis = pc_y(), 
        plot_MDS(df = df, x_axis = pc_x(), y_axis = pc_y(), 
                         title = paste0("MDS of ", input$mds_dist, " distances"),
                         tree_names = tree_names())
      } else {
        #groves::plot_MDS(df = df, x_axis = pc_x(), y_axis = pc_y(), 
        plot_MDS(df = df, x_axis = pc_x(), y_axis = pc_y(), 
                         title = paste0("MDS of ", input$mds_dist, " distances"),
                         tree_names = tree_names(), 
                         group = extra_tree_data()[, input$var_color])
      }
    } else {
      if (input$var_color == "none") {
        req(input$consensus_num)
        #groves::plot_MDS(df = df, x_axis = pc_x(), y_axis = pc_y(), 
        plot_MDS(df = df, x_axis = pc_x(), y_axis = pc_y(), 
                 title = paste0("MDS of ", input$mds_dist, " distances"),
                 tree_names = tree_names(),
                 phylogenomic = input$consensus_num,
                 phylogenomic_name = tree_names()[input$consensus_num])
      } else {
        #groves::plot_MDS(df = df, x_axis = pc_x(), y_axis = pc_y(), 
        plot_MDS(df = df, x_axis = pc_x(), y_axis = pc_y(), 
                 title = paste0("MDS of ", input$mds_dist, " distances"),
                 tree_names = tree_names(), 
                 group = extra_tree_data()[, input$var_color],
                 phylogenomic = input$consensus_num,
                 phylogenomic_name = tree_names()[input$consensus_num])
      }
    } 
  })
  
  # make reactive for plot
  MDS_plot <- reactive({
    req(plot_MDS_res())
    plot_result <- plot_MDS_res()
    # if tree 0 has been chosen, color it blue
    if (isTruthy(input$tree0_choice)) {
      tree0_name <- input$tree0_choice
      tree0_ind <- which(tree_names() == tree0_name)
      plot_with_point <- plot_result +
        if (input$var_color == "none") {
          ggplot2::geom_point(data = mds_res()$df[tree0_ind, ],
                              ggplot2::aes(x = MDS_x, y = MDS_y),
                              color = "blue")
        }
      plot_with_point
    } else {
      plot_result
    }
  })
  
  # plot individual trees in 3rd tab
  # plot base tree
  output$base_name <- renderText({
    req(input$other_base_tree)
    paste0("Base tree: ", input$other_base_tree)
  })
  base_tree_plot <- eventReactive(c(input$other_base_tree, input$midpoint, input$scale, 
                                    input$base_support), {
                                      req(input$other_base_tree)
                                      if (!isTruthy(input$base_support)) {
                                        plot_tree(tree_name = input$other_base_tree)
                                      } else {
                                        plot_tree(tree_name = input$other_base_tree,
                                                  add_support = TRUE, gene_trees = gene_trees())
                                      }
                                    })
  base_tree_plotly <- reactive({
    req(base_tree_plot())
    plotly_tree(base_tree_plot())
  })
  observeEvent(base_tree_plot(), {
    output$base_tree_plot <- plotly::renderPlotly(plotly::ggplotly(base_tree_plotly(), tooltip = ""))
  })
  output$download_base_tree <- downloadHandler(
    filename = function() {
      req(input$other_base_tree)
      paste(input$other_base_tree, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(base_tree_plot())
      ggplot2::ggsave(file, plot = base_tree_plot(), device = 'png', height = 8, width = 8)
    }
  )
  # plot tree in upper right of third tab
  tree1_plot <- eventReactive(c(input$tree1_choice, input$midpoint, input$scale,
                                input$tree1_support), {
                                  req(input$tree1_choice)
                                  if (!isTruthy(input$tree1_support)) {
                                    plot_tree(tree_name = input$tree1_choice)
                                  } else {
                                    plot_tree(tree_name = input$tree1_choice,
                                              add_support = TRUE, gene_trees = gene_trees())
                                  }
                                })
  tree1_plotly <- reactive({
    req(tree1_plot())
    plotly_tree(tree1_plot())
  })
  observeEvent(tree1_plotly(), {
    output$chosen_tree1 <- plotly::renderPlotly(plotly::ggplotly(tree1_plotly(), tooltip = ""))
  })
  output$download_tree1 <- downloadHandler(
    filename = function() {
      req(input$tree1_choice)
      paste(input$tree1_choice, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(tree1_plot())
      ggplot2::ggsave(file, plot = tree1_plot(), device = 'png', height = 8, width = 8)
    }
  )
  # plot tree in lower left of third tab
  tree2_plot <- eventReactive(c(input$tree2_choice, input$midpoint, input$scale,
                                input$tree2_support), {
                                  req(input$tree2_choice)
                                  if (!isTruthy(input$tree2_support)) {
                                    plot_tree(tree_name = input$tree2_choice)
                                  } else {
                                    plot_tree(tree_name = input$tree2_choice,
                                              add_support = TRUE, gene_trees = gene_trees())
                                  }
                                })
  tree2_plotly <- reactive({
    req(tree2_plot())
    plotly_tree(tree2_plot())
  })
  observeEvent(tree2_plot(), {
    output$chosen_tree2 <- plotly::renderPlotly(plotly::ggplotly(tree2_plotly(), tooltip = ""))
  })
  output$download_tree2 <- downloadHandler(
    filename = function() {
      req(input$tree2_choice)
      paste(input$tree2_choice, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(tree2_plot())
      ggplot2::ggsave(file, plot = tree2_plot(), device = 'png', height = 8, width = 8)
    }
  )
  # plot tree in lower right of third tab
  tree3_plot <- eventReactive(c(input$tree3_choice, input$midpoint, input$scale,
                                input$tree3_support), {
                                  req(input$tree3_choice)
                                  if (!isTruthy(input$tree3_support)) {
                                    plot_tree(tree_name = input$tree3_choice)
                                  } else {
                                    plot_tree(tree_name = input$tree3_choice,
                                              add_support = TRUE, gene_trees = gene_trees())
                                  }
                                })
  tree3_plotly <- reactive({
    req(tree3_plot())
    plotly_tree(tree3_plot())
  })
  observeEvent(tree3_plot(), {
    output$chosen_tree3 <- plotly::renderPlotly(plotly::ggplotly(tree3_plotly(), tooltip = ""))
  })
  output$download_tree3 <- downloadHandler(
    filename = function() {
      req(input$tree3_choice)
      paste(input$tree3_choice, '_plot', '.png', sep = '')
    },
    content = function(file) {
      req(tree3_plot())
      ggplot2::ggsave(file, plot = tree3_plot(), device = 'png', height = 8, width = 8)
    }
  )
}
