#' UI layout for main panel
#' 
#' @description
#' Contains Help tab and Data tab
#' @noRd
#' 

mainPanelUI <- function(stepsTab) {
  tabsetPanel(id = "mainTab",
              tabPanel("Help",
                       h1("Instructions and Help"),
                       hr(),
                       shiny::p("The purpose of this app is to perform outliers on data in a step-by-step process. This app is able to generate a report with the information up until the current step, for each step."),
                       shiny::p("All the parameters have default values; make changes to the parameters as needed for the analyses. After each step, the parameters of the previous steps cannot be altered; finalize adjustments to reactive parameters before continuing."),
                       hr(),
                       h2("OuRS"),
                       shiny::p("This app is reliant on the ", shiny::a("OuRS package ", href = "https://github.com/derekbeaton/ours", target = "_blank"), "for the outlier detection techniques; any necessary components for the OuRS package are also necessary for this app."),
                       hr(),
                       h2("Steps"),
                       h4("Step 1) Data:"),
                       shiny::p("1. Select data from either a matrix or data.frame from the global environment. This data must be preprocessed and already loaded into RStudio before running this app."),
                       shiny::p("2. Specify if the data is all continuous or not (refer to ONDRI Data Conventions Compendium for more info on Data Types)"),
                       shiny::p("3. Enter title and subtitle of report to be generated"),
                       shiny::p("4. Enter name of author"),
                       shiny::p("5. Set the seed of random number generator (allows for reproduceable simulation)"),
                       br(),
                       h4("Step 2) Ordination:"),
                       shiny::p("1. Select Standard Bootstrap Cutoff"),
                       shiny::p("2. Select number of iterations"),
                       br(),
                       h4("Step 3) (G)MCD:"),
                       shiny::p("1. Select Standard and Robust Bootstrap Cutoffs"),
                       shiny::p("2. Select Alpha Level and number of iterations"),
                       shiny::p("3. Select CorrMax threshold"),
                       br(),
                       h4("Step 4) (G)CorrMax:"),
                       shiny::p("1. Adjust Standard and Robust Bootstrap Cutoffs (these reactively update the (G)MCD plot's cutoff lines)"),
                       shiny::p("2. Adjust CorrMax threshold (this reactively updates the (G)CorrMax's heatmap threshold visibility"),
                       hr(),
                       h2("Plots"),
                       shiny::p("This app produces plots for the Ordination, (G)MCD, and (G)CorrMax analyses."),
                       shiny::p("Make sure to clear all plots before using this app. The Plots panel can be found in the bottom right pane in RStudio.", style = "color:red"),
                       tags$div(img(src = "www/Clear_Plots.png")),
                       hr(),
                       h2("Reactivity"),
                       shiny::p("This app contains reactive elements as follows:"),
                       shiny::p("- Standard Bootstrap Cutoff changes the threshold on the (G)MCD plot vertical axes"),
                       shiny::p("- Robust Bootstrap Cutoff changes the threshold on the (G)MCD plot horizontal axes"),
                       shiny::p("- CorrMax Threshold changes the threshold on the (G)CorrMax plot to only show outliers above the given %"),
                       hr(),
                       h2("Generate Report"),
                       shiny::p(icon("download"), "Generate Report requires ", shiny::a("LaTeX ", href = "https://www.latex-project.org/get/", terget = "_blank"), "to render pdf files."),
                       shiny::p("This generates a summary report (pdf/html) as seen in the Preview tab as well as OutlierRanking and VariableRanking .csv files")
              ),
              tabPanel("Heatmap", 
                conditionalPanel(condition = "!output.data_selected", 
                                 tags$p("Select data to display heatmap.")),
                plotlyOutput("heatmap_plot", height = heatmap_height, width = "100%")
                # plotlyOutput("OrdinationVis", height = ordination_plot_height)
                ),
              tabPanel("Ordination", 
                conditionalPanel(condition = "!output.step3done", 
                                 tags$p("Finish Step #2 to display ordination results.")),
                plotlyOutput("OrdinationVis", height = ordination_plot_height, width = "100%")
              ),
              tabPanel("MCD", 
                       conditionalPanel(condition = "!output.step3done", 
                                        tags$p("Finish Step #3 to display MCD plot.")),
                       plotOutput("MCDVis", height = mcd_plot_height, width = "100%")
                       # conditionalPanel(condition = "output.mcd_path",
                       #                  plotOutput("MCDVis"), height = mcd_plot_height),
                       # conditionalPanel(condition = "!output.mcd_path",
                       #                  uiOutput("MCDVisNotMCD"))
              ),
              tabPanel("CorrMax", 
                       conditionalPanel(condition = "!output.step3done", 
                                        tags$p("Finish Step #3 to display CorrMax plot.")),
                                        plotlyOutput("CorrMaxVis", height = heatmap_height, width = "100%")
                       # conditionalPanel(condition = "output.mcd_path",
                       #                  plotlyOutput("CorrMaxVis", height = heatmap_height)),
                       # conditionalPanel(condition = "!output.mcd_path",
                       #                  uiOutput("CorrMaxVisNotMCD"))
              ),
              tabPanel("Preview", 
                       conditionalPanel(condition = "!output.data_selected", 
                                        tags$p("Select data to preview report.")),
                       uiOutput("previewReport"))
                         
  )
}