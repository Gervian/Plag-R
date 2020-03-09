# handle.packages <- function(pkg){
#   new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
#   if (length(new.pkg)) 
#     install.packages(new.pkg, dependencies = TRUE)
#   sapply(pkg, require, character.only = TRUE)
# }
# 
# all.packages <- c("tm", "pdftools", "stringr", "stringi", "ggplot2", 
#                   "dplyr", "wordcloud", "plotly", "DT", "shiny", "purrr",
#                   "plotly", "shinythemes", "shinyBS", "shinydashboard", 
#                   "readtext", "stringdist", "tidytext", "shinythemes", "gginnards",
#                   "shinyjs", "shinyWidgets", "textTinyR","officer", "magrittr","flextable", "quanteda", "cluster", "vegan",
#                   "factoextra", 
#                   "devtools"
#                   )
# handle.packages(all.packages)
# 
# install_github("nik01010/dashboardthemes")
# sapply("dashboardthemes", require, character.only = TRUE)