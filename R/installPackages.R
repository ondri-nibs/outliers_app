#' @title installPackages
#'
#' @description A function for installing all required packages and dependencies.
#' @author Jedid Ahn
#' 
#' @export
#' 
installPackages <- function(){
  # CRAN packages.
  cranPackages <- c("devtools", "shiny", "shinyFiles", "shinyjs", "plotly", 
                    "rmarkdown", "markdown", "knitr", "corrplot", "pander", 
                    "abind", "stringr", "viridis", "pheatmap", "tinytex")
  
  
  
  invisible(sapply(cranPackages, function(p){
    if (p %in% rownames(utils::installed.packages()) == FALSE) {
      utils::install.packages(p)
    }
  }))
  
  
  # GitHub packages.
  if ("GSVD" %in% rownames(utils::installed.packages()) == FALSE) {
    Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = TRUE)
    remotes::install_github("derekbeaton/GSVD")
  }
  if ("ours" %in% rownames(utils::installed.packages()) == FALSE) {
    Sys.setenv(R_REMOTES_NO_ERRORS_FROM_WARNINGS = TRUE)
    remotes::install_github("derekbeaton/OuRS", subdir = "/OuRS")
  }
  
  
  # tinytex installation for RMarkdown report generation.
  if (tinytex::is_tinytex() == FALSE){
    tinytex::install_tinytex()
  }
}

# [END]