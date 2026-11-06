
#OPTIMAL PC SELECTION ALGO
quick.elbow <- function(varpc,low=.08,max.pc=.9) {
  ee <- varpc/sum(varpc) #proportinal eigenvalues
  while(low>=max(ee)) { low <- low/2 } #when no principal component eigenvalues are large enough, cut low in half
  lowie <- (ee<low) ; highie <- ee>low/8
  low.ones <- which(lowie & highie)
  others <- length(which(!lowie))
  if(length(low.ones)>0) {
    if(length(low.ones)==1) {
      elbow <- low.ones
    } else {
      set <- ee[low.ones]
      pc.drops <- abs(diff(set))/(set[1:(length(set)-1)])
      infz <- is.infinite(pc.drops)
      #print(pc.drops)
      elbow <- which(pc.drops==max(pc.drops[!infz],na.rm=T))[1]+others
    }
  } else {
    # if somehow there are no small eigenvalues, just choose the elbow as the second last
    cat("no eigenvalues were significantly smaller than the previous\n")
    elbow <- length(ee)
  }
  if(tail(cumsum(ee[1:elbow]),1)>max.pc) {
    elbow <- which(cumsum(ee)>max.pc)[1]-1
  }
  if(elbow<1) {
    warning("elbow calculation failed, return zero")
    return(0)
  }
  elbow<-elbow + 1 # include last to be sure ..
  names(elbow) <- NULL
  return(elbow)
}

#GENES FILTERING
genes_filter <- function(df, filters = c("IgFilter", "TrFilter", "riboFilter", "mtFilter", "gmFilter", "rikFilter", "dotFilter")) {
  #ensure gene names are uppercase for consistent matching
  geneNames <- toupper(rownames(df))
  GenesToFilterOut <- integer(0)
  if(is.null(filters)){
    return(df)
  }
  
  #immunoglobulin genes (IGHV, IGKV)
  if ("IgFilter" %in% filters) {
    GenesToFilterOut <- c(
      GenesToFilterOut,
      which(stringr::str_starts(geneNames, "IGHV") | stringr::str_starts(geneNames, "IGKV"))
    )
  }
  
  #t-cell receptor genes (TRAV, TRBV)
  if ("TrFilter" %in% filters) {
    GenesToFilterOut <- c(
      GenesToFilterOut,
      which(stringr::str_starts(geneNames, "TRAV") | stringr::str_starts(geneNames, "TRBV"))
    )
  }
  
  #ribosomal genes (RPL, RPS)
  if ("riboFilter" %in% filters) {
    GenesToFilterOut <- c(
      GenesToFilterOut,
      which(stringr::str_starts(geneNames, "RPL") | stringr::str_starts(geneNames, "RPS"))
    )
  }
  
  #mitochondrial genes (MT-)
  if ("mtFilter" %in% filters) {
    GenesToFilterOut <- c(
      GenesToFilterOut,
      which(stringr::str_starts(geneNames, "MT-"))
    )
  }
  
  #GM-prefixed genes
  if ("gmFilter" %in% filters) {
    GenesToFilterOut <- c(
      GenesToFilterOut,
      which(stringr::str_starts(geneNames, "GM"))
    )
  }
  
  #genes ending with RIK
  if ("rikFilter" %in% filters) {
    GenesToFilterOut <- c(
      GenesToFilterOut,
      which(stringr::str_ends(geneNames, "RIK"))
    )
  }
  
  #genes starting with "\\.", ENSG, or a regex pattern like XX000000
  if ("dotFilter" %in% filters) {
    GenesToFilterOut <- c(
      GenesToFilterOut,
      which(
        stringr::str_starts(geneNames, "\\.") |
          stringr::str_starts(geneNames, "ENSG") |
          stringr::str_detect(geneNames, "^[A-Z]{2}\\d{6}")
      )
    )
  }
  
  #remove duplicate indices
  GenesToFilterOut <- unique(GenesToFilterOut)
  
  #return filtered dataframe
  if (length(GenesToFilterOut) > 0) {
    filtered_df <- df[-GenesToFilterOut, , drop = FALSE]
  } else {
    filtered_df <- df  # No genes to filter, return original
  }
  
  list(
    counts=filtered_df,
    filtered=setdiff(rownames(df), rownames(filtered_df))
  )
}

#PRINCIPAL GENES ALGO
principalgenes<- function(data = NULL, pcs = -1, cut = -1, filters=c("IgFilter", "TrFilter", "riboFilter", "mtFilter", "gmFilter", "rikFilter", "dotFilter")) {
  
  message(length(rownames(data)), ", ", length(colnames(data)))
  message(paste0(filters))
  if (is.null(data)) {
    stop("Error: 'data' must be provided.")
  }
  
  if(cut == -1){
    cut = 0.25
  }
  
  message("filtering genes based on specifications")
  data<-genes_filter(data, filters)
  filtered_genes<-data$filtered
  data<-data$counts
  message("Post Filtering Dataset Dimensions: Rows: ", length(rownames(data)), ", Columns: ", length(colnames(data)))
  
  zero_cols<-which(colSums(data) == 0)
  zero_var_rows <- apply(data, 1, var, na.rm = TRUE) == 0
  zero_var_rows<-which(zero_var_rows)
  
  message("Remove zero rows and remove zero columns until none are left")
  while(length(zero_cols) != 0 || length(zero_var_rows)!=0){
    zero_cols<-which(colSums(data) == 0)
    if(length(zero_cols) > 0) {
      data<-data[, -zero_cols]
    }
    zero_var_rows <- apply(data, 1, var, na.rm = TRUE) == 0
    zero_var_rows<-which(zero_var_rows)
    if(length(zero_var_rows) > 0) {
      data<-data[-zero_var_rows, ]
    }
    message("Dataset Dimensions after removing zero rows and columns: Rows: ", length(rownames(data)), ", Columns: ", length(colnames(data)))
  }
  
  message("Calculating optimal number of PCs via elbow method:")
  if(pcs == -1 || pcs >= 20 || pcs >= 0.05*ncol(data)){
    temp<- irlba::prcomp_irlba(t(data), n = min(20, 0.05*ncol(data)), retx = TRUE, center = TRUE, scale. = TRUE)
    eig = temp$sdev^2
    pcs = quick.elbow(eig)
    rm(eig, temp)
  }
  
  message("Values set: pcs = ", pcs, ", chosen cutoff = ", cut)
  
  #transpose the data to have genes as columns and cells as rows
  rotated_output <- irlba::prcomp_irlba(t(data), n = pcs, retx = TRUE, center = TRUE, scale. = TRUE)
  
  #prepare to represent results
  summary<- summary(rotated_output)
  loadings <- rotated_output$rotation
  rownames(loadings) <- rownames(data) # Associate gene names with loadings
  
  #compute variance explained by each PC
  eigenvalues <- (rotated_output$sdev)^2
  prop_var <- eigenvalues / sum(eigenvalues)
  temp_obj <- data.frame(x=colnames(loadings), y = prop_var)
  
  #plot PCA results
  graph1<- ggplot2::ggplot(temp_obj, ggplot2::aes(x = x, y = y * 100)) +
    ggplot2::geom_bar(stat = "identity", fill = "skyblue") +
    ggplot2::geom_text(ggplot2::aes(label= round(y*100, 2)), vjust = +1.5)+
    ggplot2::labs(x = NULL, y = NULL, title = NULL) +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none")
  plot(graph1)
  
  #calculate absolute values of loadings and contribution percentages
  loadings <- abs(loadings)
  sumload <- colSums(loadings)
  percent_cont <- loadings / sumload
  
  #calculate total contribution to variance for each gene
  scaled_contr<-sweep(percent_cont, 2, prop_var, FUN = "*")
  tot_contr <- rowSums(scaled_contr)
  if (any(!is.finite(tot_contr))) {
    stop("Error: contribution calculation produced NA or infinite values. Please check the input data.")
  }
  tot_contr <- sort(tot_contr, decreasing = TRUE)
  
  #select genes with total contribution in cut percentile
  message("starting the cutoff process")
  cumulative_gps<- 0
  if (cut == 1){
    result<- sort(decreasing = TRUE, tot_contr)
  } else {
    
    result<-list()
    for (i in 1:length(tot_contr)){
      result[[i]] <- tot_contr[i]
      cumulative_gps <- cumulative_gps + as.numeric(tot_contr[i])
      if (cumulative_gps >= cut){
        break
      }
    }
    result<-unlist(result)
  }
  
  # Log transformation for top genes' contributions
  logcont <- -10 * log10(result)
  
  # Return results
  list(
    summary,
    log_contributions = logcont, #-10*log10(gps), larger values imply less contribution
    top_genes = result, #ordered top selected genes
    rotated_output = rotated_output, #pca output
    filtered = filtered_genes, #list of genes filtered out
    count_matrix = data #filtered count matrix for use elsewhere
  )
}