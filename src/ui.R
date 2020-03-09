###########################/ui.R/##################################
source("package_handler.r")
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"
# library(rsconnect)
library(tm)
library(pdftools)
library(stringr)
library(stringi)
library(ggplot2)
library(dplyr)
library(wordcloud)
library(plotly)
library(DT)
library(shiny)
library(purrr)
library(plotly)
library(shinythemes)
library(shinyBS)
library(shinydashboard)
library(dashboardthemes)
library(readtext)
library(stringdist)
library(tidytext)
library(shinythemes)
library(gginnards)
library(reticulate)
library(shinyjs)
library(shinyWidgets)
library(textTinyR)
library(officer)
library(magrittr)
library(flextable)
library(quanteda)
library(cluster)
library(vegan)
library(factoextra)
library(devtools)
library(V8)
library(Rtsne)

header <- dashboardHeader(title = "plagR",titleWidth = 160)
sidebar <- dashboardSidebar(width = 0, collapsed = F, uiOutput('sidebar'))
body <- dashboardBody(
  shinyDashboardThemes(
    theme = "grey_dark"
  ),
  useShinyjs(),
  extendShinyjs(text = jscode),
  # tags$style(".fa-chart-pie {color: white}; .fa-upload {color: white}; .fa-eye {color: white}; .fa-bar-chart {color: white};"),
  tags$head(tags$style('.fa {color: white}
                       .progress-bar {background-color: #46505a;}')),
  # tags$head(tags$style(HTML('.progress-bar {background-color: red;}')))
  htmlOutput("page")
  )

ui <- dashboardPage(header, sidebar, body)