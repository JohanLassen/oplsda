

pqn <- function(X, n = "median", QC = NULL) {
  X.norm <- matrix(nrow = nrow(X), ncol = ncol(X))
  colnames(X.norm) <- colnames(X)
  rownames(X.norm) <- rownames(X)
  
  if (!is.null(QC)) {
    # if QC vector exists, use this as reference spectrum
    if (length(QC) == 1) {
      # only 1 reference sample given
      mX <- as.numeric(X[QC, ])
    } else {
      if (n == "mean") {
        mX <- as.numeric(colMeans(X[QC, ]))
      }
      if (n == "median") {
        mX <- as.numeric(apply(X[QC, ], 2, median))
      }
    }
  } else {
    # otherwise use the mean or median of all samples as reference sample
    if (n == "mean") {
      mX <- as.numeric(colMeans(X))
    }
    if (n == "median") {
      mX <- as.numeric(apply(X, 2, median))
    }
  }
  
  # do the actual normalisation
  for (a in 1:nrow(X)) {
    X.norm[a, ] <- as.numeric(X[a, ] / median(as.numeric(X[a, ] / mX)))
  }
  
  return(X.norm)
}

mcc <- 
  function(pred, obs){
    cm  <- table(pred, obs) / 100 #confusion matrix
    TP  <- cm[1,1]
    FN  <- cm[1,2]
    FP  <- cm[2,1]
    TN  <- cm[2,2]
    mcc <- (TP*TN-FP*FN)/sqrt((TP+FP)*(TP+FN)*(TN+FP)*(TN+FN))
    return(mcc)
  }

fit_models <- function(x, y, methods = c("glmnet", "pls")){
  cat("\nNotice:\nDepending on the data size this process may take 1-60 minutes\n\n")
  trctrl <- caret::trainControl(method = "repeatedcv", number = 3, repeats = 2, verboseIter = T, savePredictions = T, classProbs = T)
  fits   <- 
    as.list(methods) %>% 
    purrr::map(~do.call(
      caret::train,
      c(
        list(x = x, y=y, method = .x, tuneLength = 3, trControl = trctrl),
        list(importance = 'impurity')[.x=="ranger"]
        ))
      )
  names(fits) <- methods
  return(fits)
}

get_performance <- 
  function(fits){
    predictions <- 
      fits %>% 
      purrr::map(5) %>%
      bind_rows(.id = "method") %>% 
      mutate(
        rep = gsub(".*[.]", "", Resample),
        fold = gsub("[.].*", "", Resample)
      )
    
    performance <- predictions %>% 
      group_by(method, rep) %>% 
      summarise(accuracy = mean(pred == obs), mcc = mcc(pred, obs))
    
    return(performance)
  }


plot_performance <- 
  function(fits){
    
    performance <-
      get_performance(fits)
    
    metrics_plot <- 
      performance %>% 
      ggplot(aes(x=accuracy, y=mcc, fill = method))+
      geom_point(shape=21, color = "white", size = 2)+
      scale_colour_hue(l = 45)+
      theme_minimal()
    
    rocdata <- 
      MLeval::evalm(fits, silent = T, showplots = F, gnames = names(fits))$roc$data
    
    roc_curve <- 
      rocdata %>% 
      ggplot(aes(x=FPR, y=SENS, color = Group))+
      geom_abline(color="gray")+
      geom_line(show.legend = F)+
      scale_colour_hue(l = 45) +
      theme_minimal()+
      labs(x="false postive rate", y="true positive rate")
    
    combined_diagnostics <- 
      cowplot::plot_grid(roc_curve, metrics_plot)
    
    return(combined_diagnostics)
  }

# df <- 
#   readr::read_tsv("data/tx0c00448_si_002.edited.tsv")
# 
# library(dplyr)
# x <- df %>% dplyr::select(dplyr::starts_with("M")) %>% as.matrix()
# x <- x^(1/4)
# x <- pqn(x)
# y <- df %>% dplyr::pull(group)
# 
# 
# fits <- 
#   fit_models(x, y)
# 
# performance_plot <- 
#   plot_performance(fits)
