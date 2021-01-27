#' @title runApp
#' 
#' @description
#' Walks user through outliers processing on a given, preprocessed data package and generates report.
#' 
#' This is a Shiny app that takes input data from the global environment (either matrix or dataframe),
#' and gives a step by step approach to outlier processing specifically:
#' 1. Ordination Analysis
#' 2. (Generalized) Minimum Covariance Determinant (G)MCD Analysis
#' 3. CorrMax
#' This app is able to generate a report with the information up until the current step for each step
#' 
#' The server logic is organized as follows:
#' - Reactive Vals and Reactives: the step#done() reactiveVals are necessary for bookmark restoration
#' - Observers
#' - Render Outputs
#' - App State Trigger Funcs
#' @author Logan Lim Jeremy Tanuan
#' 
#' @import shiny shinyFiles shinyjs plotly rmarkdown markdown knitr corrplot
#' @import pander abind stringr viridis pheatmap
#' @import GSVD ours fs
#' @importFrom grDevices colorRampPalette dev.cur dev.set recordPlot
#' @importFrom graphics abline par plot points text
#' @export
#' 
runApp <- function(){
  shiny::addResourcePath("www", system.file("www", package = "ONDRIOutliersApp"))
  initializeVariables()
  set.seed(default.seed)
  
  # Define UI for app
  ui <- fluidPage(theme = "www/style.css",
                  
                  # Create title with logo.
                  titlePanel(title = tags$div(
                    img(class = "ondri-logo",
                        src = "www/ONDRI_full-logo_web.png"), 
                    paste("ONDRI Outliers Application", vn_num)), 
                    windowTitle = "ONDRI Outliers Application"),
                  
                  sidebarLayout(
                    sidebarInputsUI(stepsTab, dataEnv),
                    
                    mainPanel(
                      # this shows the input parameters
                      # verbatimTextOutput("inputVals"), # Grey info box
                      mainPanelUI(stepsTab)
                    )
                  )
  )
  
  # Define server logic required for semi-reactive outliers processing 
  server <- function(input, output, session) {
    
    # Initialization ===========================================================================
    
    # Computer volumes depend on OS: Windows, Mac, or Linux.
    computerVolumes <- c(Home = fs::path_home(), "R Installation" = R.home(), getVolumes()())
    
    shinyDirChoose(input, id = "downloadFolder", roots = computerVolumes,
                   restrictions = system.file(package = "base"))
    
    output$downloadFolderPath <- renderText("No folder selected.")
    
    # Reactive Vals and Reactives ===========================================================================
    
    # this is the file name for the outliers report
    fn <- reactiveValues(filepath = NULL)
    shinyjs::hide("downloadReport")
    
    # this is empty string when not generalized and "G" when generalized
    generalized <- reactiveVal("")
    
    # this is TRUE when the session has just been restored; turns to FALSE once necessary updates have been skipped before overriding (seed and corrmax threshold skipped)
    restored <- reactiveVal(F)
    
    # this is TRUE when the user clicks the bookmark parameters button; o/w FALSE
    bookmark.params.only <- reactiveVal(F)
    
    # this holds various other variables in a list
    meta.data <- reactiveVal()
    
    data.name <- reactiveVal()
    seed <- reactiveVal()
    iterations.val <- reactiveVal()
    bootstrap.iterations.val <- reactiveVal()
    mcd.path <- reactiveVal(T)
    output$mcd_path <- reactive(mcd.path())
    
    # this hold the actual data
    data <- reactiveVal()
    
    # these hold the records that allows plot reconstruction
    ordination.plot.val <- reactiveVal()
    ordination.plot.non_interactive.val <- reactiveVal()
    primary.plot.val <- reactiveVal()
    corrmax.plot.val <- reactiveVal()
    corrmax.plot.non_interactive.val <- reactiveVal()
    
    
    # this holds all the results from the functions from helpers.R
    all.res <- reactiveVal()
    
    # these are TRUE only when their respective step has been completed; o/w FALSE
    step1done <- reactiveVal(F)
    step3done <- reactiveVal(F)
    step4done <- reactiveVal(F)
    reportbuilt <- reactiveVal(F)
    
    # Stores the numeric column names for univariate tab
    # numer_cols <- reactiveVal()
    
    # Stores selected columns to plot for univariate tab
    # cols_to_render <- reactiveVal()
    
    # These save the steps into reactiveVals
    output$step1done <- reactive(
      step1done()
    )
    
    output$data_selected <- reactive(
      !is.null(data())
    ) # Exists so we can display heatmap before pressing the step 1 button
    outputOptions(output, "data_selected", suspendWhenHidden = FALSE)
    
    outputOptions(output, "step1done", suspendWhenHidden = FALSE)
    output$step3done <- reactive(
      step3done()
    )
    outputOptions(output, "step3done", suspendWhenHidden = FALSE)
    output$step4done <- reactive(
      step4done()
    )
    outputOptions(output, "step4done", suspendWhenHidden = FALSE)
    output$reportbuilt <- reactive(
      return (!is.null(fn$filepath))
    )
    outputOptions(output, "reportbuilt", suspendWhenHidden = FALSE)
    
    # Observers ===========================================================================================
    
    observeEvent(
      input$downloadFolder, {
        output.dir <- parseDirPath(computerVolumes, input$downloadFolder)
        
        # Reset output when selection of folder is cancelled.
        output$downloadFolderPath <- renderText("No folder selected.")
        
        # Otherwise, if folder has been chosen, print path of selected folder to
        # screen, output contents of folder, and update dropdowns in all tabs.
        if (length(output.dir) > 0){
          # Update output box by printing new folder path.
          output$downloadFolderPath <- renderPrint(output.dir)
        }
      }
    )
    
    # this changes generalized variable from empty string to "G" if continuous, o/w becomes empty string
    observeEvent(meta.data()$continuous, {
      generalized("")
      if(!meta.data()$continuous) {
        generalized("G")
      }
    })
    
    # this stores various values into meta.data()
    observe({
      allCont <- F
      if(as.logical(input$allCont)) {
        allCont <- T
      }
      meta.data.list <- list(data.name = data.name(),
                             continuous = allCont,
                             title = input$title,
                             subtitle = input$subtitle,
                             name = input$name,
                             seed = seed(),
                             #custom.descriptor = input$customDescriptor,
                             bootstrap.threshold.standard = input$bootstrapCutoffStandard,
                             bootstrap.threshold.robust = input$bootstrapCutoffRobust,
                             corrmax.threshold = input$corrmaxThreshold,
                             mcd.threshold = input$alpha,
                             mcd.iterations = iterations.val(),
                             bootstrap.iterations = bootstrap.iterations.val(),
                             mcd.path = mcd.path())
      meta.data(meta.data.list)
    })
    
    
    # updates seed to user desired seed
    observeEvent(input$seed, {
      if(is.numeric(input$seed)){
        seed(input$seed)
        set.seed(seed())
      } else if (!restored()) {
        updateNumericInput(session = session, inputId = "seed", value = default.seed)
      } else {
        restored(F)
      }
    })
    
    # formula that decides if data is mcd.path or not mcd.path
    observeEvent(data(), {
      mcd.path(T)
      if (nrow(data()) < ceiling(ncol(data())*.9)) {
        mcd.path(F)
      }
    })
    
    # this rounds then stores the respective iterations values
    observeEvent(input$iterations, {
      iterations.val(round(as.numeric(input$iterations)))
    })
    observeEvent(input$bootstrapIterations, {
      bootstrap.iterations.val(round(as.numeric(input$bootstrapIterations)))
    })
    
    # Ordination process
    run_ordination <- function(){
      withProgress(message = "Running Ordination Analyses", value = 0.5, {
        tryCatch({
          # all.res(merge.list(ordination(meta.data()$continuous, mcd.path(), data.to.analyze = data(), center.columns, scale.columns, bootstrap.iterations = bootstrap.iterations.val(), bootstrap.cutoff.standard = meta.data()$bootstrap.threshold.standard), all.res()))
          all.res(merge.list(ordination(meta.data()$continuous, data.to.analyze = data(), center.columns, scale.columns), all.res()))
        }, warning = function(w) {
          showModal(modalDialog(w$message, title = "Warning"))
        }, error = function(e) {
          showModal(modalDialog(e$message, title = "Error"))
        })
        incProgress(amount = 0.5, message = "Finished Ordination Analyses")
        updateTabsetPanel(session, "mainTab", selected = "Ordination") # Switch to Ordination Tab
      })
    }
    
    step3.in.progress <- reactiveVal(F) # Keeps track if MCD is already running so spam clicking button doesn't make it run multiple times
    # MCD process
    observeEvent(input$step3button, priority = 1, {
      if(!step3.in.progress()){  
        run_ordination()
        if(mcd.path()) {
          step3.in.progress(T)
          withProgress(message = paste0("Running ", generalized(), "MCD Analyses"), value = 0.5, {
            tryCatch({
              #all.res(merge.list(primary.outlier.analyses(meta.data()$continuous, mcd.path(), data.to.analyze = data(), center.columns, scale.columns, outlier.alpha = meta.data()$mcd.threshold, outlier.iterations = iterations.val(), bootstrap.iterations = bootstrap.iterations.val(), bootstrap.cutoff.robust = meta.data()$bootstrap.threshold.robust, bootstrap.cutoff.standard = meta.data()$bootstrap.threshold.standard, boot.standard.md_cutoff = all.res()$boot.standard.md_cutoff), all.res()))
              all.res(merge.list(
                primary.outlier.analyses(
                  meta.data()$continuous,
                  data.to.analyze = data(),
                  center.columns, scale.columns,
                  outlier.alpha = meta.data()$mcd.threshold,
                  outlier.iterations = iterations.val(),
                  bootstrap.iterations = bootstrap.iterations.val(),
                  bootstrap.cutoff.robust = meta.data()$bootstrap.threshold.robust,
                  bootstrap.cutoff.standard = meta.data()$bootstrap.threshold.standard),
                all.res()))
            }, warning = function(w) {
              showModal(modalDialog(w$message, title = "Warning"))
            }, error = function(e) {
              showModal(modalDialog(e$message, title = "Error"))
            })
            incProgress(amount = 0.5, message = paste0("Finished ", generalized(), "MCD Analyses"))
          })
        } else {
          # this puts the wide data error message into the place where the plots are supposed to be in the report.
          meta.data(merge.list(list(primary.plot.skip = wideDataMessage, corrmax.plot.skip = wideDataMessage), meta.data()))
        }
      }
      updateTabsetPanel(session, "mainTab", selected = "Ordination") # Switch to Ordination Tab
    })
    
    step4.in.progress <- reactiveVal(F) # Keeps track if corrmax is already running so spam clicking button doesn't make it run multiple times
    # CorrMax process (this does nothing at the moment)
    observeEvent(input$step4button, priority = 1, {
      if(!step4.in.progress()){
        step4.in.progress(T)
        withProgress(message = paste0("Running ", generalized(), "CorrMax Analyses"), value = 0.5, {
          if(!mcd.path()){
            # all.res(merge.list(corrmax.shr())) # TEST finish when pca shr corrmax results have been decided to be most appropriate
          }
          incProgress(0.5, message = paste0("Finished ", generalized(), "CorrMax Analyses"))
        })
      }
      updateTabsetPanel(session, "mainTab", selected = "CorrMax") # Switch to CorrMax Tab
    })
    
    observeEvent(input$step3button, {
      step3done(T)
    })
    observeEvent(input$step4button, {
      step4done(T)
    })
    
    
    
    # These change the affirmation that a step has been completed to TRUE
    observeEvent(input$step1button, priority = 1, {
      if(!is.null(data())) {
        step1done(T)
        updateTabsetPanel(session, "mainTab", selected = "Heatmap") # Switch to Heatmap Tab
      } else {
        showModal(modalDialog(
          title = "Error",
          "Must select data before continuing.",
          easyClose = T
        ))
      }
    })
    
    # This stores the data selected from data.name() selectInput into data() 
    observeEvent(input$data, priority = 1, {
      data.name(input$data)
      if (!(data.name() == "")) {
        data(as.matrix(get0(data.name(), envir = globalenv())))
        
        # Warns user if there are non-numeric columns
        if(any(sapply(data(), is.numeric) == F)){
          showModal(modalDialog("There are some non-numeric columns, there may be an error if you are running the continuous case.",
                                title = "Warning")) 
        }
        
        ## DB: we may want to remove "SUBJECT" as the preprend
        if (is.null(rownames(data()))) {
          this.data <- data()
          rownames(this.data) <- paste0("SUBJECT_", c(1:nrow(this.data))) # applies generic rownames to data if data does not have rownames
          data(this.data)
        }
        all.res(merge.list(list(data.ncol = ncol(data()), data.nrow = nrow(data())), all.res()))
      }
    })
    
    # This sets the default CorrMax Threshold based on 'the formula' applied to the selected data 
    observeEvent(data(), {
      if (!is.null(data()) && !restored()) {
        updateSliderInput(session = session, "corrmaxThreshold", value = ceiling((2/ncol(data()))*100))
      } else {
        restored(F)
      }
    })
    
    # Render Outputs ============================================================================================
    
    # renders and displays the temp_preview_report.html 
    output$previewReport <- renderUI({
      req(data())
      doc.params <- list(meta.data = meta.data(),
                         date = this.date,
                         data = data(),
                         all.res = all.res(),
                         ordination.plot = ordination.plot.non_interactive.val(),
                         primary.plot = primary.plot.val(),
                         # Need to make non-interactive version of the corrmax plot for the pdf, at least this is the easiest way I could find to do it.
                         corrmax.plot = corrmax.plot.non_interactive.val(),
                         is.preview = T,
                         output.dir = parseDirPath(computerVolumes, input$downloadFolder))
      includeHTML(render(system.file("rmd", "outliersReport.Rmd", package = "ONDRIOutliersApp"), 
                         output_format = "html_document", output_file = "temp_preview_report.html",
                         params = doc.params))
    })
    
    output$heatmap_plot <- renderPlotly({
      # Require that data be input
      req(data())
      if(meta.data()$continuous){
        hm_data <- ours_scale(data(), center = center.columns, scale = scale.columns, impute_NA_to_mean = F)
      } else {
        hm_data <- ca_preproc(data(), compact = F)$weightedZx
      }
      cur_dev <- dev.cur()
      hm <- plot_ly(z = hm_data,
                    y=rownames(hm_data), x=colnames(hm_data),
                    hovertemplate = paste("<b>Variable:</b> %{x}<br><b>Subject:</b> %{y}<br><b>Value:</b> %{z}<extra></extra>"),
                    colors = heatmap_colors,
                    type = "heatmap"
      )
      dev.set(cur_dev)
      hm
    }) 
    
    
    report.gen.in.progress <- reactiveVal(F)
    observeEvent(input$genReport, {
      tmp_file <- paste0(tempfile(), ".pdf")
      if(is.null(primary.plot.val())){
        # Below is kind of a workaround for the fact that we need the MCD plot to gen the csv files for the report
        # This is a bigger problem with the lazy evaluation that if we don't switch to a tab, the plots associated with it doesn't render
        showModal(modalDialog(
          # We need the render the MCD plot before we can generate the plot and strangely observers do not ensure the plot is generated for some reason
          "You must view the MCD tab before generating report to render the plot. Switching to MCD tab now.", title = "Warning"))
        updateTabsetPanel(session, "mainTab", selected = "MCD") # Switch to MCD Tab
      } else {
        if(class(input$downloadFolder) != "list"){
          # Should be true if no download folder specifed
          showModal(modalDialog("Download folder not specified.", title = "Error"))
        } else {
          if(!report.gen.in.progress()){
            out_dir <- parseDirPath(computerVolumes, input$downloadFolder)
            report.gen.in.progress(T)
            doc.params <- list(meta.data = meta.data(),
                               date = this.date,
                               data = data(),
                               all.res = all.res(),
                               ordination.plot = ordination.plot.non_interactive.val(),
                               primary.plot = primary.plot.val(),
                               corrmax.plot = corrmax.plot.non_interactive.val(),
                               is.preview = F,
                               output.dir = out_dir)
            report_name <- paste0(meta.data()$data.name, "_NIBS_OUTL_OutliersReport", this.date, ".pdf")
            rmarkdown::render(system.file("rmd", "outliersReport.Rmd", package = "ONDRIOutliersApp"),
                              output_format = "all",
                              intermediates_dir = doc.params$output.dir,
                              output_dir = doc.params$output.dir,
                              output_file = report_name,
                              params = doc.params,
                              envir = new.env())
            # rmarkdown::render("outliersReport.Rmd", output_format = "all", output_file = tmp_file, params = doc.params, envir = new.env())
            
            fn$filepath <- tmp_file
            
            showModal(modalDialog(paste0("Generated report files: ",
                                         meta.data()$data.name, "_NIBS_OUTL_OutlierRanking_", this.date, ".csv",
                                         ", ", meta.data()$data.name, "_NIBS_OUTL_VariableRanking_", this.date, ".csv"),
                                  report_name,
                                  easyClose = T))
            
            # shinyjs::runjs("document.getElementById('downloadReport').click();")
          }
        }
      }
      # Need to change this back just in case they want to gen the report twice
      # The same is not true for the steps tabs... at least for now. But after restructuring the tabs to be static it shouldn't matter.
      report.gen.in.progress(F)
    })
    
    output$downloadReport <- downloadHandler(
      filename = paste0(data.name(), "_", this.date, ".pdf"),
      content = file.copy(fn$filepath, file)
    )
    
    
    # these shows explanation text for wide data
    output$MCDVisNotMCD <- renderUI({
      if(!mcd.path()) {
        (wideDataMessage)
      }
    })
    output$CorrMaxVisNotMCD <- renderUI({
      if(!mcd.path()) {
        (wideDataMessage)
      }
    })
    
    # this doesn't do anything at the moment # COMPONENTS
    # output$corrmaxComponentsUI <- renderUI({
    #   if(!mcd.path()) {
    #     sliderInput("corrmaxComponentsNum",
    #                 "Components to Keep",
    #                 min = 2,
    #                 max = 20,
    #                 value = 2,
    #                 step = 1)
    #   }
    # })
    
    # this shows the parameter values that have been input by the user (or if they have yet to input, will show the default values)
    output$inputVals <- renderText({
      paste(
        "Data: ", data.name(), "\n",
        "All Continuous: ", meta.data()$continuous, "\n",
        "Title: ", meta.data()$title, "\n",
        "Subtitle: ", meta.data()$subtitle, "\n",
        "Name: ", meta.data()$name, "\n",
        "Alpha: ", meta.data()$mcd.threshold, "\n",
        "Outlier Iterations: ", meta.data()$mcd.iterations, "\n",
        "Robust Bootstrap Cutoff: ", meta.data()$bootstrap.threshold.robust, "\n",
        "Standard Bootstrap Cutoff: ", meta.data()$bootstrap.threshold.standard, "\n",
        "Bootstrap Iterations: ", meta.data()$bootstrap.iterations, "\n",
        "CorrMax Threshold: ", meta.data()$corrmax.threshold, "\n",
        sep = "")
    })
    
    # These show the renderings of the plots for each process
    output$OrdinationVis <- renderPlotly({
      req(all.res()$ordination.res)
      withProgress(message = "Rendering Ordination", value = 0.5, {
        # Interactive ordination plot
        op <- ordination.plot(all.res()$ordination.res, all.res()$mcd.results, ifelse(is.null(all.res()$mcd.results), F, T), interactive = T)
        ordination.plot.val(op) 
        # Non-interactive ordination plot
        op_ni <- ordination.plot(all.res()$ordination.res, all.res()$mcd.results, ifelse(is.null(all.res()$mcd.results), F, T), interactive = F)
        ordination.plot.non_interactive.val(op_ni)
        incProgress(0.5, message = "Finished Ordination")
      })
      if(!is.null(ordination.plot.val())) {
        outputOptions(output, "OrdinationVis", suspendWhenHidden = FALSE)
        ordination.plot.val()
      }
    })
    output$MCDVis <- renderPlot({
      req(all.res()$boot.standard.md)
      withProgress(message = paste0("Rendering ", generalized(), "MCD Outliers"), value = 0.5, {
        all.res.cur <- all.res()
        if(mcd.path()){
          
          # vertical abline
          all.res.cur$boot.standard.md_cutoff <- boot.md_cutoff(all.res.cur$boot.standard.md, meta.data()$bootstrap.threshold.standard)
          all.res.cur$standard.md.outliers <- all.res.cur$mcd.results$dists$mahal_dists >= all.res.cur$boot.standard.md_cutoff
          
          # horizontal abline
          all.res.cur$boot.robust.md_cutoff <- boot.md_cutoff(all.res.cur$boot.robust.md, meta.data()$bootstrap.threshold.robust)
          all.res.cur$robust.md.outliers <- all.res.cur$mcd.results$dists$robust_mahal_dists >= all.res.cur$boot.robust.md_cutoff
          
          
          ### THIS IS WHERE THE PROBLEM IS WITH STANDARD MDS & CUTOFFS.
          
          cex.colors.outliers.design <- get.cex.colors.outliers.design(all.res.cur$mcd.results, all.res.cur$standard.md.outliers, all.res.cur$robust.md.outliers)
          
          
          all.res.cur$mcd.results$md.cex <- cex.colors.outliers.design$md.cex
          all.res.cur$mcd.results$md.colors <- cex.colors.outliers.design$md.colors
          all.res.cur$mcd.results$outliers.design <- cex.colors.outliers.design$outliers.design
          
          #all.res.cur$mcd.results$md.scores <- cex.colors.outliers.design$md.scores
          all.res(all.res.cur)
          primary.outlier.analyses.plot(all.res()$mcd.results, all.res()$standard.md.outliers, all.res()$robust.md.outliers, all.res()$boot.standard.md_cutoff, all.res()$boot.robust.md_cutoff)
          primary.plot.val(recordPlot())
        }
        else {
          primary.outlier.analyses.sh.plot(all.res$pca.shr)
        }
        incProgress(0.5, message = paste0("Finished ", generalized(), "MCD Outliers"))
        if(!is.null(primary.plot.val())) {
          primary.plot.val()
          outputOptions(output, "MCDVis", suspendWhenHidden = FALSE)
        }
      })
    })
    # These are not currently working or needed at the moment. # COMPONENTS
    # output$loadingsCorrVis <- renderPlot({
    #   if(!mcd.path()) {
    #     withProgress(message = "Rendering Loadings Correlation", value = 0.5, {
    #       loadings.corr.plot(all.res()$pca.shr)
    #       incProgress(0.5, message = "Finished Loadings Correlation")
    #     })
    #   }
    # })
    # output$scoresCorrVis <- renderPlot({
    #   if(!mcd.path()) {
    #     withProgress(message = "Rendering Scores Correlation", value = 0.5, {
    #       scores.corr.plot(all.res()$pca.shr)
    #       incProgress(0.5, message = "Finished Scores Correlation")
    #     })
    #   }
    # })
    output$CorrMaxVis <- renderPlotly({
      req(all.res()$corrmax.res)
      withProgress(message = paste0("Rendering ", generalized(), "CorrMax"), value = 0.5, {
        cp <- corrmax.plot(all.res()$corrmax.res, meta.data()$corrmax.threshold, all.res()$robust.md.outliers, all.res()$mcd.results, interactive = T)
        corrmax.plot.val(cp)
        cp_ni <- corrmax.plot(all.res()$corrmax.res, meta.data()$corrmax.threshold, all.res()$robust.md.outliers, all.res()$mcd.results, interactive = F)
        corrmax.plot.non_interactive.val(cp_ni)
        incProgress(0.5, message = paste0("Finished ", generalized(), "CorrMax"))
      })
      if(!is.null(corrmax.plot.val())) {
        outputOptions(output, "CorrMaxVis", suspendWhenHidden = FALSE)
        corrmax.plot.val()
      }
    })
    
    
    # App State Trigger Funcs ===========================================================================================
    
    # this removes the preview_report html file when the app is stopped
    onStop(function() {suppressWarnings(file.remove("temp_preview_report.html"))})
    
    # this saves the values, plots, results, and data when bookmarked
    onBookmark(function(state) {
      if(!bookmark.params.only()) {
        state$values$data <- data()
        state$values$data.name <- data.name()
        state$values$ordination.plot.val <- ordination.plot.val()
        state$values$primary.plot.val <- primary.plot.val()
        state$values$corrmax.plot.val <- corrmax.plot.val()
        state$values$all.res <- all.res()
        state$values$mcd.path <- mcd.path() 
        
        state$values$step1done <- step1done()
        state$values$step3done <- step3done()
        state$values$step4done <- step4done()
      }
      state$values$iterations.val <- iterations.val()
      state$values$bootstrap.iterations.val <- bootstrap.iterations.val()
      
      state$values$meta.data <- meta.data()
      state$values$generalized <- generalized()
      state$values$bookmark.params.only <- bookmark.params.only()
    })
    
    # this restores the parameters and, depending on if the session being restored also included data, plots and results, those as well
    onRestored(function(state) {
      Sys.sleep(.5)
      generalized(state$values$generalized)
      if(!state$values$bookmark.params.only) {
        data(state$values$data)
        data.name(state$values$data.name)
        ordination.plot.val(state$values$ordination.plot.val)
        primary.plot.val(state$values$primary.plot.val)
        corrmax.plot.val(state$values$corrmax.plot.val)
        all.res(state$values$all.res)
        mcd.path(state$values$mcd.path)
        
        step1done(state$values$step1done)
        step3done(state$values$step3done)
        step4done(state$values$step4done)
        
        
      }
      iterations.val(state$values$iterations.val)
      bootstrap.iterations.val(state$values$bootstrap.iterations.val)
      
      meta.data(state$values$meta.data)
      
      restored(T)
      
      updateRadioButtons(session = session, "allCont", selected = meta.data()$continuous)
      updateTextInput(session = session, "title", value = meta.data()$title, placeholder = "ex.Outliers Summary Report")
      updateTextInput(session = session, "subtitle", value = meta.data()$subtitle)
      updateTextInput(session = session, "name", value = meta.data()$name)
      updateNumericInput(session, "seed", value = meta.data()$seed)
      updateSliderInput(session = session, "bootstrapCutoffStandard", value = meta.data()$bootstrap.threshold.standard)
      updateSliderInput(session = session, "bootstrapCutoffRobust", value = meta.data()$bootstrap.threshold.robust)
      updateSliderInput(session = session, "corrmaxThreshold", value = meta.data()$corrmax.threshold)    
      updateSliderInput(session = session, "alpha", value = meta.data()$mcd.threshold)    
      #updateTextAreaInput(session = session, "customDescriptor", value = meta.data()$custom.descriptor)
      
    }) 
    
  }
  
  # Run the application 
  shinyApp(ui, server, options = list(display.mode = "normal", launch.browser = TRUE))
}

# [END]