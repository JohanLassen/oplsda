

feature_selection <- 
  function(selected_model, fits){
    
    best_model <- fits[[selected_model]]
    
    feature_importance <- 
      caret::varImp(best_model)
    
    important_features <- 
      as_tibble(feature_importance$importance, rownames = "feature") %>% 
      slice_max(Overall, n=50) %>% 
      filter(Overall>0)
    
    return(important_features)
  }



feature_boxplot <-
  function(x, y, features){
    x %>% 
      as_tibble() %>% 
      mutate(y = y) %>% 
      select(y, all_of(features$feature)) %>% 
      tidyr::pivot_longer(cols = -y) %>%
      mutate(name = factor(name, levels = features$feature)) %>% 
      ggplot(aes(y=value, x=name, fill=y))+
      geom_boxplot(position="dodge")+
      theme_minimal()
      
  }

get_feature_correlations <- 
  function(x, features){
    feature_feature_correlations <- 
      x %>% 
      cor() %>% 
      as_tibble() %>% 
      mutate(feature = colnames(.)) %>% 
      select(feature, everything()) %>% 
      filter(feature %in% features$feature) %>% 
      tidyr::pivot_longer(cols = -feature) %>% 
      group_by(name) %>% 
      mutate(value = ifelse(value==1, 0, value)) %>% 
      mutate(best_cor = max(abs(min(value)), max(value))) %>% 
      ungroup()
    return(feature_feature_correlations)
  }


heatmap_pathway <- 
  function(x,features){
    feature_feature_correlations <-
      get_feature_correlations(x, features) %>% 
      filter(best_cor > 0.75) %>% 
      select(-best_cor) %>% 
      tidyr::pivot_wider(names_from = name, values_from = value)
    
      values <- feature_feature_correlations %>% select(-feature) %>% as.matrix()
      values[values==0] <- 1 # important to make clustering good
      feats  <- feature_feature_correlations %>% pull(feature)
      
      return(pheatmap::pheatmap(values))
  }


correlation_map <- 
  function(x, features){
    ffcor_long <- 
      get_feature_correlations(x, features) %>% 
      filter(best_cor > 0.8) %>% 
      mutate(value = ifelse(value == 0, 1, value))
    ffcor_wide <- ffcor_long %>% select(-best_cor) %>% tidyr::pivot_wider(names_from="feature", values_from="value") %>% select(-name) # The ml selected features
    clusters <- factoextra::fviz_nbclust(t(ffcor_wide), FUNcluster = kmeans)
    clusters <- which.max(clusters$data$y)
    km       <- kmeans(t(as.matrix(ffcor_wide)), centers = clusters) # could say "clusters"
    order    <- km$cluster
    
    d1 <- data.frame(
      from = "origin", 
      to=paste("group", c(1:clusters), sep="_")
    )
    d2 <- 
      ffcor_long %>% 
      select(-best_cor, -value, to = feature, from = name) %>% 
      mutate(from = paste("group", order[match(to, names(order))], sep="_")) %>% 
      mutate(id = as.numeric(gsub(".*_", "", from))) %>% 
      arrange(id) %>% 
      select(-id)
    edges <- rbind(d1, d2)
    
    # create a dataframe with connection between leaves (individuals)
    connect <- 
      ffcor_long %>% 
      select(-best_cor, -value, to = feature, from = name)
    connect$value <- ffcor_long$value
    
    # create a vertices data.frame. One line per object of our hierarchy
    vertices  <-  data.frame(
      name = unique(c(as.character(edges$from), as.character(edges$to)))
    )
    
    # Calculating fold change
    fc <- 
      x[, vertices$name[grepl("M", vertices$name)]] %>% 
      as_tibble() %>% 
      split(y) %>% 
      purrr::map(~.x %>% purrr::map_dfc(~mean(.x))) %>% 
      bind_rows(.id="group") %>% 
      tidyr::pivot_longer(cols=-group) %>% 
      tidyr::pivot_wider(values_from=value, names_from = group) %>% 
      mutate(fold_change = -log2(control/pneumoni)) %>% 
      pull(fold_change)
    
    vertices$value <- c(rep(NA, (nrow(vertices)-length(fc))), fc)
    
    # Let's add a column with the group of each name. It will be useful later to color points
    vertices$group  <-  edges$from[ match( vertices$name, edges$to ) ]
    
    vertices$id <- NA
    myleaves <- which(is.na( match(vertices$name, edges$from) ))
    nleaves <- length(myleaves)
    vertices$id[ myleaves ] <- seq(1:nleaves)
    vertices$angle <- - 360 * vertices$id / (nleaves)
    
    # calculate the alignment of labels: right or left
    # If I am on the left part of the plot, my labels have currently an angle < -90
    vertices$hjust <- rep(0, length(vertices$angle))
    #vertices$hjust <- ifelse( vertices$angle < 90, 1, 0)
    
    # flip angle BY to make them readable
    # Correct vector
    v <- vertices$angle < -80 & vertices$angle > -270
    vertices$angle <- ifelse(v, vertices$angle+180, vertices$angle)
    vertices$hjust <- ifelse(v, 1, 0)
    
    # Create a graph object
    mg <- 
      igraph::graph_from_data_frame( edges, vertices=vertices ) %>% 
      tidygraph::as_tbl_graph(mygraph) %>% 
      tidygraph::activate(nodes) %>%
      mutate(
        `feature-outcome fold change` = c(rep(NA, (nrow(vertices)-length(fc))), fc),
      )
    
    # The connection object must refer to the ids of the leaves:
    # from  <-  match( connect$from, vertices$name)
    # to    <-  match( connect$to, vertices$name)
    # value1 <-  ffcor_long$value
    # length(from)
    # length(value)
    
    graph_info <- mg %>% tidygraph::activate(edges) %>% data.frame()
    graph_info <- graph_info %>% mutate("feature-feature correlations" = c(rep(NA, (nrow(graph_info)-length(ffcor_long$value))), ffcor_long$value))
    library(ggraph)
    graph_plot <- 
      ggraph::ggraph(mg, layout = 'dendrogram', circular = TRUE) + 
      ggraph::geom_conn_bundle(
        data = do.call(ggraph::get_con,graph_info),
        alpha=0.2, 
        width=0.9, 
        aes(colour=`feature-feature correlations`)) +
      ggraph::scale_edge_colour_distiller(palette = "YlGnBu", direction = 1) +
      
      ggraph::geom_node_text(aes(x = x*1.15, y=y*1.15, filter = leaf, label=name, hjust=hjust, angle=angle), size=2, alpha=1) +
      
      ggraph::geom_node_point(aes(filter = leaf, x = x*1.05, y=y*1.05, colour=`feature-outcome fold change`), size=6, show.legend = T) +
      scale_color_distiller(palette="Spectral")+
      scale_size_continuous( range = c(0.1,10) ) +
      theme_void() +
      theme(
        plot.margin=unit(c(1,1,1,1),"cm"),
      ) +
      expand_limits(x = c(-1.3, 1.3), y = c(-1.3, 1.3))
    
    return(graph_plot)
  }


# 
# features <- feature_selection("ranger", fits)
# 
# heatmap_pathway(x, features)
# 
# correlation_map(x, features)
  
