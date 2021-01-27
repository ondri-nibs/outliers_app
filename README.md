README
================

Jedid Ahn, Logan Lim, Jeremy Tanuan, & Derek Beaton 2021JAN25

# Outliers (Shiny) App <img src='etc/logo.png' align="right" height="139"/>

A Shiny app for the ONDRI outlier detection pipeline which includes key
visualization, interactivity, and report generation. For more details,
see the [OuRS package](https://github.com/derekbeaton/OuRS).

<br>

-   Install [R](https://cran.r-project.org/) first and then
    [RStudio](https://rstudio.com/products/rstudio/download/). Please
    choose the correct installer carefully as it will depend on your
    computer’s operating system.

<br>

-   Download and install the shiny app directly with the following lines
    of code:

<!-- -->

      if (!require("devtools")){
        install.packages("devtools")
      }
      devtools::install_github(repo = "ondri-nibs/outliers_app")

<br>

-   Type `ONDRIOutliersApp::installPackages()` to install any missing
    packages and/or dependencies. If you get the following message in
    your RStudio console, please type 3. <br><br>
    <img src='etc/package-update.png'>

<br>

-   When installation is complete, type `ONDRIOutliersApp::runApp()` to
    open the app.
