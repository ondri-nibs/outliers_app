#' Functions from OuRS and other helpers
#' 
#' @description
#' Contains the calls to the OuRS functions, the plotting functions, and various helpers.
#' CorrMax analysis is done within primary.outlier.analyses() so when the button to run CorrMax
#' is pressed, it is already done (only needs to render the plot). This could be separated into
#' a different function so that the CorrMax process is done at the button press instead of during
#' the MCD analyses.
#' @noRd
#' 

## DB (big) Note: 
##  Maybe we can use helpers as a place for highly re-usable functions, e.g., ordination(), ordination.plot(), mcd(), mcd.plot(), etc...
##  These can work through the two primary flags: continuous.path? and mcd.path
##  We can take a closer look and divide and conquer some of this.


## we currently only allow for the mcd.path so this is now removed
# ordination <- function(continuous, mcd.path, data.to.analyze, center.columns = T, scale.columns =T, bootstrap.iterations, bootstrap.cutoff.standard) {
    ##DB: damnit.
ordination <- function(continuous, data.to.analyze, center.columns = T, scale.columns =T) {
  
  if(continuous) {
    
    ordination.res <- pca(data.to.analyze, center = center.columns, scale = scale.columns)
    
  } else {
    
    ordination.res <- ca(data.to.analyze)
    
  }

  return(list(ordination.res = ordination.res))
}

# Interactive component plot via plotly
component_plot <- function(scores, axes=c(1,2), pch=20, col="mediumorchid4",
                           main="Component scores",
                           xlab=paste0("Component ",axes[1]),
                           ylab=paste0("Component ",axes[2]),
                           xlim=c(-max(abs(scores[,axes])),max(abs(scores[,axes])))*1.3,
                           ylim=c(-max(abs(scores[,axes])),max(abs(scores[,axes])))*1.3,
                           asp=1, pos=3, display_names=T,cex=1,text.cex=1,
                           ...){
  # ax <- list(zeroline = TRUE, zerolinewidth = 3, zerolinecolor = toRGB("grey60"),
  #            spikedash = "dash", dash = "dash")
  # plot_ly(as.data.frame(scores[,axes]), type = "scatter") %>%
  #   layout(xaxis = ax, yaxis = ax)
  ax_line <- list(dash = "dash", color = toRGB("grey60"), hoverinfo = "none")
  plot_ly() %>%
    add_segments(x = min(xlim), y = 0, xend = max(xlim), yend = 0,
                 line = ax_line) %>%
    add_segments(x = 0, y = min(ylim), xend = 0, yend = max(ylim),
                 line = ax_line) %>%
    add_markers(data = as.data.frame(scores[,axes]), 
                x = scores[,axes][,1],
                y = scores[,axes][,2],
                type = 'scatter', mode = 'markers',
                text = rownames(scores[,axes]),
                hovertemplate = paste('%{text}<extra></extra>'),
                marker = list(color = toRGB(col))) %>%
    layout(xaxis = list(zeroline = F, showticklabels = F, title = xlab), 
           yaxis = list(zeroline = F, showticklabels = F, title = ylab),
           showlegend = F, title = main)
  
  # if (display_names) {
  #   text(scores[, axes], labels = rownames(scores),
  #        pos = pos, col = col, cex = text.cex)
  # }
  
}

# Component plot function taken from OuRS
component_plot_static <- function(scores, axes=c(1,2), pch=20, col="mediumorchid4",
                           main="Component scores",
                           xlab=paste0("Component ",axes[1]),
                           ylab=paste0("Component ",axes[2]),
                           xlim=c(-max(abs(scores[,axes])),max(abs(scores[,axes])))*1.3,
                           ylim=c(-max(abs(scores[,axes])),max(abs(scores[,axes])))*1.3,
                           asp=1, pos=3, display_names=T,cex=1,text.cex=1,
                           ...){
  
  plot(0, type="n", xlim=xlim, ylim=ylim, main=main, xlab=xlab, ylab=ylab, axes=F, asp=asp)
  abline(h=0,lty=2,lwd=2, col="grey60")
  abline(v=0,lty=2,lwd=2, col="grey60")
  points(scores[,axes], col=col, pch=pch, cex=cex, ...)
  
  if (display_names) {
    text(scores[, axes], labels = rownames(scores),
         pos = pos, col = col, cex = text.cex)
  }
  
}

# Note: Should only try to add named elements from first list to the second
merge.list <- function(lst, ...){
  
  if(!inherits(lst, "list")){
    # If first item is not a list, convert to one 
    return(list(lst))
  } 
  # If ... is not a list, convert it to one
  if(!inherits(..., "list")){
    lst2 <- list(...)
  } else {
    lst2 <- as.list(...)
  }
  new_lst <- lst
  # Get all of the names that are not already in list 1
  for(name in setdiff(names(lst2), names(lst))){
    new_lst[name] <- lst2[name]
  }
  return(new_lst)
}

ordination.plot <- function(ordination.res, mcd.results, has.mcd.run, interactive = T) {
  
  #DB: This can probably be removed soon and we just check for mcd.results
  if(!has.mcd.run){
    md.cex <- 1
    md.colors <- "mediumorchid4"
  }else{
    md.cex <- mcd.results$md.cex
    md.colors <- mcd.results$md.colors  
  }
  
  par.opts <- par(mfrow=c(2,2))
  num.ord.comps <- length(ordination.res$d)

  # ##DB: I do not understand this conditional statement.
  # if (has.mcd.run) { # this crashes if the data has no rownames (pretty sure the data must have rownames though)
  #   rownames(disp.u) <- ifelse(md.colors=="grey80" | md.colors=="mediumorchid4","",rownames(disp.u))
  # } else {
  #   md.colors <- "mediumorchid4"
  # } 
 
  # cur_dev <- dev.cur()
  if(!interactive){ 
    component_plot_static(ordination.res$v, xlab="Component 1", ylab="Component 2", text.cex = .65)
    component_plot_static(ordination.res$u, col = md.colors, cex = md.cex, xlab="Component 1", ylab="Component 2", text.cex = .65)
    
    component_plot_static(ordination.res$v, axes = c((num.ord.comps-1),num.ord.comps), text.cex = .65)
    component_plot_static(ordination.res$u, axes = c((num.ord.comps-1),num.ord.comps), col = md.colors, cex = md.cex, text.cex = .65)
    
    # par(par.opts)
    fig <- recordPlot()
  } else {
    p1 <- component_plot(ordination.res$v, xlab="Component 1", ylab="Component 2", text.cex = .65)
    p2 <- component_plot(ordination.res$u, col = md.colors, cex = md.cex, xlab="Component 1", ylab="Component 2", text.cex = .65)
    
    p3 <- component_plot(ordination.res$v, axes = c((num.ord.comps-1),num.ord.comps), text.cex = .65)
    p4 <- component_plot(ordination.res$u, axes = c((num.ord.comps-1),num.ord.comps), col = md.colors, cex = md.cex, text.cex = .65)
    fig <- subplot(p1, p2, p3, p4, nrows = 2, titleX = T, titleY = T)
  }
  # dev.set(cur_dev)
  fig
}

## DB: center and scale options are required.
## DB: for the MCD functions we will also allow the user to pick outlierness color palettes (but much later)

## we currently only allow for the mcd.path so this is now removed
  ### boot.standard.md_cutoff was removed too
    ### however, I may regret that...
# primary.outlier.analyses <- function(continuous, mcd.path, data.to.analyze, center.columns = T, scale.columns = T, outlier.alpha, outlier.iterations, bootstrap.iterations, bootstrap.cutoff.robust, bootstrap.cutoff.standard, boot.standard.md_cutoff) {
primary.outlier.analyses <- function(continuous, data.to.analyze, center.columns = T, scale.columns = T, outlier.alpha, outlier.iterations, bootstrap.iterations, bootstrap.cutoff.robust, bootstrap.cutoff.standard) {
  
  
    if(continuous) {
      mcd.results <- continuous_mcd(data.to.analyze, 
                              center = center.columns,
                              scale = scale.columns,
                              alpha = outlier.alpha, 
                              num.subsets = outlier.iterations)
      
      
      # boot.robust.md <- cont.boot.sup.u(data.to.analyze,
      #                                   center = mcd.results$cov$center,
      #                                   scale = mcd.results$cov$scale,
      #                                   loadings = mcd.results$cov$loadings,
      #                                   singular.values = mcd.results$cov$singular.values,
      #                                   iters = bootstrap.iterations)
      
      corrmax.res <- continuous_corrmax(data.to.analyze,
                                        center = mcd.results$cov$center,
                                        scale = mcd.results$cov$scale,
                                        loadings = mcd.results$cov$loadings,
                                        singular.values = mcd.results$cov$singular.values)
    } else {
      
      mcd.results <- generalized_mcd(data.to.analyze, 
                             alpha = outlier.alpha, 
                             num.subsets = outlier.iterations)
      
      # boot.robust.md <- cat.boot.sup.u(data.to.analyze,
      #                                  loadings = mcd.results$cov$loadings, 
      #                                  singular.values = mcd.results$cov$singular.values, 
      #                                  iters = bootstrap.iterations)

      corrmax.res <- generalized_corrmax(data.to.analyze,
                                         loadings = mcd.results$cov$loadings, 
                                         singular.values = mcd.results$cov$singular.values)
      
    }
    
    ## bootstrapped (lazily upstrapped) distribution of distances moved to outside the if/else
    boot.robust.md <- sample(mcd.results$dists$robust_mahal_dists, size = bootstrap.iterations * length(mcd.results$dists$robust_mahal_dists),  replace = T)
    boot.standard.md <- sample(mcd.results$dists$mahal_dists, size = bootstrap.iterations * length(mcd.results$dists$mahal_dists),  replace = T)
  
    ## call this function for the standard and robust  
    boot.robust.md_cutoff <- boot.md_cutoff(boot.robust.md, bootstrap.cutoff.robust)
    boot.standard.md_cutoff <- boot.md_cutoff(boot.standard.md, bootstrap.cutoff.standard)
    
    standard.md.outliers <- mcd.results$dists$mahal_dists >= boot.standard.md_cutoff
    robust.md.outliers <- mcd.results$dists$robust_mahal_dists >= boot.robust.md_cutoff
    
    ## need to reduce some of the redundant outputs from get.cex...
    cex.colors.outliers.design <- get.cex.colors.outliers.design(mcd.results, standard.md.outliers, robust.md.outliers)
    mcd.results$md.cex <- cex.colors.outliers.design$md.cex
    mcd.results$md.colors <- cex.colors.outliers.design$md.colors
    mcd.results$outliers.design <- cex.colors.outliers.design$outliers.design
    # mcd.results$md.scores <- cex.colors.outliers.design$md.scores
    
    result <- list(mcd.results = mcd.results,
                   boot.standard.md = boot.standard.md,
                   boot.robust.md = boot.robust.md,
                   boot.standard.md_cutoff = boot.standard.md_cutoff,
                   boot.robust.md_cutoff = boot.robust.md_cutoff,
                   standard.md.outliers = standard.md.outliers,
                   robust.md.outliers = robust.md.outliers,
                   corrmax.res = corrmax.res)
    
 
  return(result)
}

# render for mcd plot
  ### should eventually be replaced by an interactive version of the dd-plot from OuRS
primary.outlier.analyses.plot <- function(mcd.results, standard.md.outliers, robust.md.outliers, boot.standard.md_cutoff, boot.robust.md_cutoff) {
  
  # standard.md.outliers <- mcd.results$dists$mahal_dists >= boot.standard.md_cutoff
  # robustmd.outliers <- mcd.results$dists$robust_mahal_dists >= boot.robust.md_cutoff
  
  outliers.design <- mcd.results$outliers.design
  # md.scores <- mcd.results$md.scores
    ## the sqrt part needs to be generalized and we need to adapt this to the dd-plot from OuRS
  # md.scores <- sqrt(cbind(mcd.results$dists$mahal_dists,mcd.results$dists$robust_mahal_dists))
  md.scores <- log(cbind(mcd.results$dists$mahal_dists,mcd.results$dists$robust_mahal_dists))
  rownames(md.scores) <- names(mcd.results$dists$mahal_dists)
  
  plot(md.scores,
       xlab="Log Standard Mahalanobis Distances",
       ylab="Log Robust Mahalanobis Distances",
       main="Mahalanobis Distances",
       pch=20,
       col=mcd.results$md.colors,
       cex=mcd.results$md.cex)
  abline(v=log(boot.standard.md_cutoff), col = "olivedrab3")
  abline(h=log(boot.robust.md_cutoff), col = "mediumorchid4")
  
  if(any(outliers.design[,"Robust"]==1 & outliers.design[, "Both"]==0)){
    text(md.scores[which(outliers.design[,"Robust"]==1 & outliers.design[,"Both"]==0),,drop=F],labels = rownames(md.scores)[which(outliers.design[,"Robust"]==1)],pos=2,col="olivedrab3",cex=0.8)
  }
  
  if(any(outliers.design[,"Both"]==1)){
    text(md.scores[which(outliers.design[,"Both"]==1),,drop=F],labels = rownames(md.scores)[which(outliers.design[,"Both"]==1)],pos=2,col="firebrick3",cex=1.1)
  }

}


# render for corrmax.plot
##DB: to be updated soon, but fine for now.
corrmax.plot <- function(corrmax.res, corrmax.threshold, robust.md.outliers, mcd.results, interactive = T) {
  breaksList <- seq(0, 100, by = 1)
  # Just visualizing percent contributions
  # per_cont <- round(as.data.frame(corrmax.res["percentage_contributions"]), 2)
  per_cont <- round(corrmax.res$percentage_contributions, 2)
  per_cont[per_cont < corrmax.threshold] <- NA
  rob.md <- mcd.results$dists$robust_mahal_dists
  md.scores <- cbind(rob.md)
  
  rownames(md.scores) <- rownames(per_cont, do.NULL = F, prefix = "SUBJECT_")
  rownames(per_cont) <- rownames(md.scores)
  
  
  corrmax.rob_reordered <- per_cont[order(md.scores[, "rob.md"],decreasing = T),]
  corrmax.rob_reordered <- corrmax.rob_reordered[1:length(robust.md.outliers[robust.md.outliers == TRUE]),] # TEST kinda messy. need to remove entries that are not robust md outliers without indexing...   corrmax.rob_reordered <- corrmax.rob_reordered[which(robust.md.outliers),] seems to mess up the ordering
  # xcols <- str_replace(colnames(corrmax.rob_reordered), "percentage_contributions.", "")
  cur_dev <- dev.cur()
  
  if(interactive){
    cp <- plot_ly(z = as.matrix(corrmax.rob_reordered),
                  x = colnames(corrmax.rob_reordered), y = rownames(corrmax.rob_reordered),
                  hovertemplate = paste("<b>Variable:</b> %{x}<br><b>Subject:</b> %{y}<br><b>Contribution:</b> %{z}%<extra></extra>"),
                  colors = heatmap_colors,
                  type = "heatmap")
  }else{
    cp <- pheatmap(corrmax.rob_reordered,
                   labels_col = colnames(corrmax.rob_reordered), labels_row = rownames(corrmax.rob_reordered), 
                   color = colorRampPalette(heatmap_colors)(100),
                    cluster_rows = F,
                    cluster_cols = F,
                    breaks = breaksList,
                    fontsize = 10) # need to update this to a variable that resizes depending on how many variables in data
  }
  
  dev.set(cur_dev)
  cp
}

# helper function for boot.md_cutoff
  boot.md_cutoff <- function(boot.md, bootstrap.cutoff) {
  result <- sort(c(boot.md))[ceiling(length(c(boot.md))*bootstrap.cutoff)]
}


cat.boot.sup.u <- function(target.data,loadings,singular.values,iters=100){
  
  num_rows <- nrow(target.data)
  data_samp <- target.data[sample(num_rows, num_rows * iters, replace=T),]
  ca.preproc.data <- tapply(
    data_samp,
    factor(rep(1:iters, ncol(target.data) * num_rows)),
    function(x) ca_preproc(matrix(x, nrow=num_rows, byrow = F), compact = F))
  
  these.vecs <- lapply(ca.preproc.data,
                  function(x) sweep(loadings,1,sqrt(x$w)/x$w,"*"))
  lapply(these.vecs, function(x) {x[is.nan(x)] <- 0})
  
  boot.distrs <- mapply(function(x, y) {rowSums(
    (sweep(
      sweep(
        sweep(x$Ox ,1, x$m,"/") %*% y,
        2,singular.values,"/"),
      1,sqrt(x$m),"*"))^2)},
    ca.preproc.data, these.vecs)
  # 
  # boot.distrs <- matrix(NA,nrow(target.data),iters)
  # for(i in 1:iters){
  #   ca.preproc.data <- ca_preproc(target.data[sample(nrow(target.data),replace=T),], compact=F)
  #   ## I'm sure this could be much more efficient in some way.
  #   these.vecs <- sweep(loadings,1,sqrt(ca.preproc.data$w)/ca.preproc.data$w,"*")
  #   these.vecs[is.nan(these.vecs)] <- 0 # this happens when rare categories drop from resampling
  #   
  #   boot.distrs[,i] <- rowSums(
  #     (sweep(
  #       sweep(
  #         sweep(ca.preproc.data$Ox,1,ca.preproc.data$m,"/") %*% these.vecs,
  #         2,singular.values,"/"),
  #       1,sqrt(ca.preproc.data$m),"*"))^2)
  # }
  return(boot.distrs)
}

# # Copied from deletion on Mar.25/20 from the OuRS repo (Commit: b78bd84e373a5737c2a02cbd50c440d4461121a6)
# cat.boot.sup.u <- function(target.data,loadings,singular.values,iters=100){
#   boot.distrs <- matrix(NA,nrow(target.data),iters)
#   for(i in 1:iters){
#     ca.preproc.data <- ca_preproc(target.data[sample(nrow(target.data),replace=T),], compact=F)
#     ## I'm sure this could be much more efficient in some way.
#     these.vecs <- sweep(loadings,1,sqrt(ca.preproc.data$w)/ca.preproc.data$w,"*")
#     these.vecs[is.nan(these.vecs)] <- 0 # this happens when rare categories drop from resampling
#     
#     boot.distrs[,i] <- rowSums(
#       (sweep(
#        sweep(
#        sweep(ca.preproc.data$Ox,1,ca.preproc.data$m,"/") %*% these.vecs,
#         2,singular.values,"/"),
#       1,sqrt(ca.preproc.data$m),"*"))^2)
#   }
#   return(boot.distrs)
# }

cont.boot.sup.u <- function(target.data,center=T,scale=F,loadings,singular.values,iters=100){
  
  num_rows <- nrow(target.data)
  data_samp <- target.data[sample(num_rows, num_rows * iters, replace=T),]
  scaled_samp <- tapply(
    data_samp,
    factor(rep(1:iters, ncol(target.data) * num_rows)),
    function(x) ours_scale(matrix(x, nrow=num_rows, byrow = F), center=center, scale=scale))
  
  boot.distrs <- lapply(scaled_samp, 
      function(x) rowSums(sweep((x %*% loadings), 2, singular.values, "/")^2))

  return(simplify2array(boot.distrs))
}

# Copied from deletion on Mar.25/20 from the OuRS repo (Commit: b78bd84e373a5737c2a02cbd50c440d4461121a6)
cont.boot.sup.u2 <- function(target.data,center=T,scale=F,loadings,singular.values,iters=100){
# size after sample: 1323 nrow(target.data) of target.data (same as original sample size)
# size after ours_scale: 1323 (same)
# size after %*% loadings: 1323 (same)
# size after sweep: 1323 (same)
# size after rowSums: 147 = 1323 / 9
# size of matrix: (nrow(target.data))x(iters)

  boot.distrs <- matrix(NA,nrow(target.data),iters)
  for(i in 1:iters){

    boot.distrs[,i] <- rowSums(

      sweep( (ours_scale(target.data[sample(nrow(target.data),replace=T),],center=center,scale=scale) %*% loadings) , 2, singular.values, "/")^2

    )

  }
  return(boot.distrs)
}


### this sends back some redundant info with where its called from
get.cex.colors.outliers.design <- function(mcd.results, standard.md.outliers, robust.md.outliers) {
  standard.outliers <- standard.md.outliers
  robust.outliers <- robust.md.outliers
  both.outliers <- standard.outliers & robust.outliers
  inliers <- !(standard.outliers | robust.outliers)
  
  ## ok so here, md.scores is actually the sqrt of the standard & robust
  # md.scores <- sqrt(cbind(mcd.results$dists$mahal_dists, mcd.results$dists$robust_mahal_dists))
  # rownames(md.scores) <- names(mcd.results$dists$mahal_dists)
  
  outliers.design <- cbind(both.outliers, robust.outliers, standard.outliers, inliers)
  colnames(outliers.design) <- c("Both", "Robust", "Standard", "Neither")
  
  
  md.colors <- rep("grey80",length(mcd.results$dists$mahal_dists))
  md.colors[which(outliers.design[,"Standard"]==1)] <- "mediumorchid4"
  md.colors[which(outliers.design[,"Robust"]==1)] <- "olivedrab3"
  md.colors[which(outliers.design[,"Both"]==1)] <- "firebrick3"
  # md.colors <- ifelse(standard.outliers,"mediumorchid4",md.colors)
  # md.colors <- ifelse(robust.outliers,"olivedrab3",md.colors)
  # md.colors <- ifelse(both.outliers,"firebrick3",md.colors)
  
  md.cex <- rep(.5,length(mcd.results$dists$mahal_dists))
  md.cex[which(outliers.design[,"Standard"]==1)] <- 1.5
  md.cex[which(outliers.design[,"Robust"]==1)] <- 2.5
  md.cex[which(outliers.design[,"Both"]==1)] <- 3
  
  result <- list(md.colors = md.colors,
                 md.cex = md.cex,
                 outliers.design = outliers.design)
  
  return(result) 
}
