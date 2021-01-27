#' UI layout for sidebar panel
#' 
#' @description
#' Contains every sidebar input and the logic that determines when certain inputs are seen at which step
#' @noRd
#'

# modular name for the 'next' button for each step
submit <- "Next"

sidebarInputsUI <- function(stepsTab, dataEnv){
  sidebarPanel(
    useShinyjs(),
    shinyDirButton(id = "downloadFolder",
                   label = "Select a download folder.",
                   title = "Please select a folder to download report files to.",
                   icon = icon("folder")),
    
    verbatimTextOutput("downloadFolderPath", placeholder = TRUE),
    
    br(),
    
    conditionalPanel(
      condition = "!output.step1done",
      h4(paste0("Step 1: ",  stepsTab[1]),
         style = "color:steelblue"),
      selectizeInput(
        inputId = "data",
        label = "Choose data from global environment",
        choices = dataEnv,
        options = list(
          placeholder = "Choose data...",
          onInitialize = I('function() { this.setValue(""); }')
        )),
      tags$div(title = "Strictly continuous data means that *all* columns are numeric and can be z-scored. Non-continuous data means that variables are (transformed versions of) ordinal, categorical, or mixtures of data (which could also include continuous data).",
               radioButtons("allCont", shiny::p("Select the type of data", icon("question-circle")),
                            choices = c(
                              "Strictly continuous" = T,
                              "Non-continuous" = F
                            ))),
      textInput("title",
                label = "Title",
                value = default.title),
      textInput("subtitle",
                label = "Subtitle"),
      textInput("name",
                value = default.name,
                label = "Author"),
      numericInput("seed",
                   "Set Seed",
                   value = default.seed),
      # textAreaInput("customDescriptor",
      #               "Additional Information",
      #               resize = "vertical"),
      actionButton("step1button",
                   submit)),
    
    conditionalPanel(
      condition = "!output.step3done && output.step1done",
      h4(paste0("Steps 2 & 3: ", stepsTab[2], " & ", stepsTab[3]),
         style = "color:steelblue")),
    
    conditionalPanel(
      condition = "!output.step4done && output.step3done",
      h4(paste0("Step 4: ", stepsTab[4]),
         style = "color:steelblue")),
    
    conditionalPanel(
      condition = "!output.step3done && output.step1done",
      sliderInput("alpha",
                  "Alpha Level",
                  min = 0.5,
                  max = 0.9,
                  value = 0.75,
                  step = 0.05)),
    
    conditionalPanel(
      condition = "!output.step3done && output.step1done",
      textInput("iterations",
                "Outlier Iterations",
                value = 500),
      textInput("bootstrapIterations",
                "Bootstrap Iterations",
                value = 500)),
    
    conditionalPanel(
      condition = "!output.step3done && output.step1done",
      sliderInput("bootstrapCutoffStandard",
                  "Standard Bootstrap Cutoff",
                  min = 0.5,
                  max = 0.99,
                  value = 0.75,
                  step = 0.005)),
    
    conditionalPanel(
      condition = "!output.step4done && output.step1done",
      sliderInput("bootstrapCutoffRobust",
                  "Robust Bootstrap Cutoff",
                  min = 0.5,
                  max = 0.99,
                  value = 0.75,
                  step = 0.005)),

    conditionalPanel(
      condition = "!output.step3done && output.step1done",
      actionButton("step3button",
                   # paste0("Run ", stepsTab[3])
                   submit)),
    
    conditionalPanel(
      condition = "output.step3done && !output.step4done",
      sliderInput("corrmaxThreshold",
                  "CorrMax Threshold",
                  min = 0,
                  max = 100,
                  value = 0,
                  step = 1)),
    
    conditionalPanel(
      condition = "!output.step4done && output.step3done",
      # uiOutput("corrmaxComponentsUI"), # this does nothing at the moment (for non-mcd.path though) # COMPONENTS
      actionButton("step4button",
                   # paste0("Run ", stepsTab[4])
                   submit)),
    
    conditionalPanel(
      condition = "output.step4done",
      actionButton("genReport", "Generate Report", icon = icon("file")),
      downloadButton("downloadReport", "Download Report")
      )
  )
}