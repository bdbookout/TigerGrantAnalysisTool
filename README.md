# TigerGrantAnalysisTool
ShinyApp and associated files 
A Shiny dashboard that analysis trends in how TIGER infrastructure grants have been awarded, based on political color of the state.   Data sources include the Department of Transportation, Senate.gov, House.gov and the US Census bureau.

DEPENDENCIES:

* Base R: https://www.r-project.org)
* R-Studio:   https://www.rstudio.com/products/RStudio/
* Shiny: https://www.rstudio.com/products/shiny/
* R packages:  https://www.rstudio.com/products/rpackages/.  Run the install.packages() function directly in R-Studio console to load the packages. The code then calls the needed package with library().  library(shiny)

install.packages("shinydashboard", "tidyverse", "readr", "tibble", "purrr", "lubridate", "DBI", "scales", "forcats", "DT", "plotly", "leaflet")

CODE FILES:

app.R:   the Shiny app.  Documentation is available on the Shiny apps website to show how an ap directory should be structured.   http://shiny.rstudio.com

NewStartupScript.R: 

.csv files:  supplemental data files used in the analysis

RData files:   Output from the startup script.  These are datatables that are read by app.r.  
