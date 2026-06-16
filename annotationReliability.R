
irrTable <- function(annotations, 
                     caption = "Inter-annotator reliability based on Cohen's pairwise $\\kappa$ and Fleiss's n-ary $\\kappa$", 
                     label = 'irr'
                     ) {
  ## Inter-annotator reliability function
  ## output pairwise k, ombnibus k, and table  
  
  # --- load required packages ---
  if (!require(irr, quietly = TRUE)) {
    stop("Package 'irr' is required but not installed.")
  }
  if (!require(xtable, quietly = TRUE)) {
    stop("Package 'xtable' is required but not installed.")
  }
  
  # ensure it's a data frame
  ann <- as.data.frame(annotations)
  
  # ensure all entries are factors
  ann[] <- lapply(ann, factor)
  
  # number and names of annotations
  annotation_names <- colnames(ann)
  n_annotations <- length(annotation_names)
  
  ## --- Pairwise Cohen's kappa ---
  combs <- combn(n_annotations, 2)
  
  pair_list <- apply(combs, 2, function(idx) {
    k <- kappa2(ann[, idx], weight = "unweighted")
    
    data.frame(
      Annotation1  = annotation_names[idx[1]],
      Annotation2  = annotation_names[idx[2]],
      kappa   = k$value,
      z       = k$statistic,
      p.value = k$p.value
    )
  })
  
  pairwise_results <- do.call(rbind, pair_list)
  
  ## --- Fleiss’ Kappa ---
  fleiss_result <- kappam.fleiss(ann)
  
  fleiss_row <- data.frame(
    Annotation1  = sprintf("All %d", n_annotations),
    Annotation2  = "",
    kappa   = fleiss_result$value,
    z       = fleiss_result$statistic,
    p.value = fleiss_result$p.value
  )
  
  combined <- rbind(pairwise_results, fleiss_row)
  
  ## --- xtable output ---
  tab <- xtable(
    combined,
    caption = caption,
    label   = label
  )
  
  ## return structured result
  list(
    pairwise = pairwise_results,
    fleiss   = fleiss_result,
    xtable   = tab
  )
}


consensusVote <- function(raters, tie.method = c("NA", "random", "first")) {
  ## consensus annotation if there are n annotations
  ## return NA if all are NA
  ## different tie-resolutions possible
  ## note that 'random' implies that analysis will not be deterministic
  
  tie.method <- match.arg(tie.method)
  
  # apply row-wise
  apply(raters, 1, function(x) {
    
    # drop missing
    x <- x[!is.na(x)]
    if (length(x) == 0) return(NA)
    
    tab <- table(x)
    maxCount <- max(tab)
    winners <- names(tab)[tab == maxCount]
    
    # no tie
    if (length(winners) == 1) return(winners)
    
    # handle ties
    if (tie.method == "NA") {
      return(NA)
    } else if (tie.method == "random") {
      return(sample(winners, 1))
    } else if (tie.method == "first") {
      return(winners[1])   # deterministic
    }
  })
}