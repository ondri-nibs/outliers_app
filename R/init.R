initializeVariables <- function(){
  assign("default.title", "", envir = globalenv())
  assign("default.name", "", envir = globalenv())
  assign("default.seed", 42, envir = globalenv())
  
  assign("this.date", toupper(gsub("[[:space:]]","",format(Sys.time(), " %Y %b %d"))), envir = globalenv())
  
  # this is the data frames and matrices from the global environment
  valid_data <- {sapply(sapply(ls(envir = globalenv()), 
                               function(x) (class(get(x)) == "data.frame") | (class(get(x)) == "matrix")),
                        any)}
  assign("dataEnv", ls(envir = globalenv())[valid_data], envir = globalenv())
  
  assign("vn_num", "0.10.2.9001", envir = globalenv()) # Version number for outliers app
  
  # names of each step
  assign("stepsTab", c("Data", "Ordination", "MCD", "CorrMax"), envir = globalenv())
  
  assign("wideDataMessage", "Skipped because of constraints (data is too wide)", envir = globalenv())
  assign("heatmap_colors", viridis(4), envir = globalenv())
  assign("heatmap_height", 550, envir = globalenv())
  assign("mcd_plot_height", 400, envir = globalenv())
  assign("ordination_plot_height", 600, envir = globalenv())
  
  # not sure if these will be available as inputs or not
  assign("center.columns", T, envir = globalenv())
  assign("scale.columns", T, envir = globalenv())
}