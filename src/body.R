body <- function(){tagList(
  tabItems(
    # First tab content
    tabItem(HTML('<meta name="viewport" content="width=1024">'),
            tabName = "sbm_PTC",
            tabsetPanel(id = "Main_Tab",
                        tabPanel(title = tags$em("Submit", style = "font-size:150%; color: lightgrey"), icon=icon("upload"), value = "ptc_stab",
                                 fluidRow(
                                   
                                 ),
                                 fluidRow(
                                   column(6, h2("Suspected Text")),
                                   column(6, h2("Evidence Text"))
                                 ),
                                 fluidRow(
                                   column(6, selectInput("susp_input_type", NA,
                                                         choices=list("Type/Paste suspected text" = "clip",
                                                                      "Upload Suspected file" = "sus_file_dd"),
                                                         selected="sus_file_dd")
                                   ),
                                   column(6, selectInput("evid_input_type", NA,
                                                         choices=list("Type/Paste Evidence text" = "clip",
                                                                      "Upload Evidence file" = "ev_file_dd"),
                                                         selected="ev_file_dd")
                                   )
                                 ),
                                 column(12,
                                        fluidRow(
                                          column(6, 
                                                 fluidRow(uiOutput("susp_method"))
                                          ),
                                          column(6, 
                                                 ###################
                                                 # Input evidence text
                                                 uiOutput("evid_method")
                                                 ###################
                                          )
                                        ),
                                        fluidRow(actionButton("go_view","Calculate"),actionButton("TIN","Turnitin"))),
                                 tags$head(tags$style(HTML('
                                                           .modal-lg {
                                                           width:  100%;hight: 150%;}
                                                           '))),
                                 bsModal("Turnitin", "Turnitin", "TIN", size = "large",tabPanel(title = "Turnitin", 
                                                                                                tags$iframe(style = "height:650px;width:100%;scrolling = yes",
                                                                                                            src= "https://www.turnitin.com/login_page.asp?lang=en_us"))),
                                 tags$head(tags$style("#Turnitin .modal-footer{ display:none}"))
                                 ),
                        tabPanel(title = tags$em("View", style="font-size:150%; color: lightgrey"), icon=icon("eye"),value = "tab_view", 
                                 fluidPage(column(12, uiOutput("probeer")))),
                        tabPanel(tags$em("Visualise", style="font-size:150%; color: lightgrey"), 
                                 icon=icon("bar-chart-o"), plotOutput("woord_wolk_ptc"))
                        )
    ),
    tabItem(tabName = "sbm_SM", 
            tabsetPanel(id = "Main_Tab_sm",
                        tabPanel(tags$em("Submit", style = "font-size:150%; overflow-y: scroll; color: lightgrey"), icon=icon("upload"),
                                 fluidPage(column(12, fluidRow(),
                                                  fluidRow(h2("Upload Texts")),
                                                  fluidRow(fileInput("sm_files", NULL, multiple = TRUE,
                                                                     accept = c(".pdf", ".txt", ".docx", ".doc")))))
                        ),
                        tabPanel(tags$em("Features", style = "font-size:150%; overflow-y: scroll; color: lightgrey"), icon=icon("chart-pie"),
                                 fluidPage(column(12,
                                                  fluidRow(h2("Features to extract")),
                                                  fluidRow(h4("Number of words in texts")),
                                                  fluidRow(uiOutput("sm_files_info")),
                                                  fluidRow(numericInput("mfw_win", "Size of word chunks",value = 1000, min = 10, max=10000, step=10)),
                                                  fluidRow(selectInput("stylo_feat_choice", "Stylometric Features", 
                                                                       choices=c("Most-Frequent-Word Chunks Model"="mfw", 
                                                                                 "Lexical Features (slow)"="lex"),
                                                                       selected = "mfw")),
                                                  fluidRow(uiOutput("sm_feat_choose"))
                                 ))
                        ),
                        tabPanel(tags$em("Model", style = "font-size:150%; overflow-y: scroll; color: lightgrey"), icon=icon("chart-pie"),
                                 fluidPage(column(12,
                                                  column(3,
                                                         fluidRow(h2("Clustering Model")),
                                                         fluidRow(uiOutput("clust_params")),
                                                         hr(),
                                                         fluidRow(uiOutput("sm_chunks_mems"))
                                                  ),
                                                  column(9,
                                                         fluidRow(uiOutput("stylo_clust_output"))
                                                  )) )
                        )
            )
    ),
    tabItem(tabName = "sbm_PD",
            tabsetPanel(id = "Main_Tab_pd",
                        tabPanel(
                          tags$em("Submit", style = "font-size:150%; color: lightgrey"), icon=icon("upload"),
                          column(12, fluidRow(),
                                 fluidRow(h2("Upload Texts")),
                                 fluidRow(fileInput("pd_files", NULL, multiple = TRUE,
                                                    accept = c(".pdf", ".txt", ".docx", ".doc")))
                          )),
                        tabPanel(tags$em("Model", style = "font-size:150%; color: lightgrey"), icon=icon("chart-pie"),
                                 column(6,
                                        fluidRow(h2("Features to extract")),
                                        fluidRow(checkboxGroupInput("pd_feat_choice", "Features to extract", 
                                                                    choices=c("Basic NLP Features"="fs1", 
                                                                              "Fuzzy Matching Features"="fs2",
                                                                              "Word Embedding Features"="fs3",
                                                                              "TF-IDF Features 1"="fs4",
                                                                              "TF-IDF Features 2"="fs5",
                                                                              "TF-IDF Features 3"="fs6",
                                                                              "TF-IDF Features 4"="fs7",
                                                                              "TF-IDF Features 5"="fs8",
                                                                              "Sentence Embedding Features"="fs9"),
                                                                    selected = "fs1")),
                                        fluidRow(actionButton("calc_pd_mod", "Fit model"))
                                 ),
                                 column(6,
                                        uiOutput("pd_mod_res")
                                 )
                        )
            ),
            # Second tab content
            tabItem(tabName = "sbm_SR",
                    tabsetPanel(id = "Main_Tab_sr",
                                tabPanel(tags$em("Submit",style = "font-size:150%; color: lightgrey"), icon=icon("upload"),
                                         fluidRow(
                                           
                                         ),
                                         fluidRow(
                                           column(6, h2("Suspected Text"),offset = 3)
                                         ),
                                         fluidRow(
                                           column(6, selectInput("susp_input_type2", NA,
                                                                 choices=list("Type/Paste suspected text" = "clip",
                                                                              "Upload Suspected file" = "sus_file_dd2"),
                                                                 selected="clip"),
                                                  offset = 3
                                           )
                                         ),
                                         fluidRow(
                                           column(6, 
                                                  ###################
                                                  # Input suspected text
                                                  uiOutput("susp_method2")
                                                  ,offset = 3),
                                           column(6,
                                                  fluidRow(actionButton("go2", "Go"),actionButton("view2","View")),
                                                  fluidRow(uiOutput("viewerbs2")))
                                         )
                                ),
                                tabPanel(tags$em("View", style="font-size:150%; color: lightgrey"),
                                         icon=icon("eye"),uiOutput("viewer_SR"),value = "tab_view2"),
                                tabPanel(tags$em("Compare", style="font-size:150%; color: lightgrey"), column(8, uiOutput("compare2")),
                                         column(4,
                                                fluidRow(
                                                  uiOutput("qgram2")
                                                ),
                                                fluidRow(
                                                  uiOutput("metric2")
                                                ))
                                ),
                                tabPanel(tags$em("Visualise", style="font-size:150%; color: lightgrey"), 
                                         icon=icon("bar-chart-o"))
                    )
            )
    )
  ))
}