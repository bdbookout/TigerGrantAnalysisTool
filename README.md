# TigerGrantAnalysisTool
ShinyApp and associated data files 

The Tiger Grant Interactive analysis app is hosted here:

https://bbookout.shinyapps.io/TigerGrant/

A Shiny dashboard analysis of trends in how TIGER infrastructure grants have been awarded, based on political color of the state.   Data sources include the Department of Transportation, Senate.gov, House.gov and the US Census bureau.

DEPENDENCIES:

* Base R: https://www.r-project.org
* R-Studio:   https://www.rstudio.com/products/RStudio/
* Shiny: https://www.rstudio.com/products/shiny/
* R packages:  https://www.rstudio.com/products/rpackages/.  Run the install.packages() function directly in R-Studio console to load the packages. The code then calls the needed package with library().  library(shiny)

install.packages("shinydashboard", "tidyverse", "readr", "tibble", "purrr", "lubridate", "DBI", "scales", "forcats", "DT", "plotly", "leaflet")

CODE FILES:

app.R:   the Shiny app.  Documentation is available on the Shiny apps website to show how an ap directory should be structured.   http://shiny.rstudio.com

NewStartupScript.R: This script reads data from .csv files, cleans it, and outputs .RData files for the app.

.csv files:  raw data files and interim output of the NewStartupScript that cleans and shapes the data for analysis.

RData files:   Output from the startup script.  These are the datatables that are read by app.r.  
