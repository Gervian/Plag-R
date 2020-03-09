###########################/server.R/##################################

my_username <- ""
my_password <- ""
directory <- "C:/Users/FG/Downloads/text.matcher-PP_present/text.matcher-PP_present/src/www/"
e <- "temp.vosloo.temp@gmail.com"
key <- '272B741F-5161-4BD2-9507-D7E5CBD68723'
source("login.r")
source("body.r")
source("sidebar.r")
source("helpers.r")
bin <- F

server <- (function(input, output, session) {
  options(shiny.maxRequestSize=800*1024^2) 
  options(shiny.trace = F)
  
  Logged <- F
  Security <- T
  
  USER <- reactiveValues(Logged = Logged)
  SEC <- reactiveValues(Security = Security)
  
  susp_input_typeInput <- reactive({input$susp_input_type})
  evid_input_typeInput <- reactive({input$evid_input_type})
  
  sm_input_typeInput <- reactive({input$sm_input_type})
  
  susp_input_typeInput2 <- reactive({input$susp_input_type2})
  
  susp_txtInput <- reactive({input$susp_raw})
  evid_txtInput <- reactive({input$evid_raw})
  
  text_list <- reactive({
    si <- susp_input_typeInput()
    ei <- evid_input_typeInput()
    withProgress({
      setProgress(message = "Extracting Text...")
      tt <- list()
      if (si != "clip") {
        susp.lst=list()
        susp.inFile <- input$susp_files
        for(i in 1:length(susp.inFile[,"name"])){
          susp.lst[[i]] <- readtext(susp.inFile[[i, "datapath"]])$text
        }
      } else {
        susp.lst <- NULL
      }
      if (ei != "clip") {
        evid.lst=list()
        evid.inFile <- input$evid_files
        for(i in 1:length(evid.inFile[,"name"])){
          evid.lst[[i]] <- readtext(evid.inFile[[i, "datapath"]])$text
        }
      } else {
        evid.lst <- NULL
      }
      return(list(susp=susp.lst, evid=evid.lst))
    })
  })
  
  raw_text_list <- reactive({
    si <- susp_input_typeInput()
    ei <- evid_input_typeInput()
    txt_lst <- list()
    susp.inTxt <- input$susp_raw
    evid.inTxt <- input$evid_raw
    if (si == "clip") {
      txt_lst$susp <- input$susp_raw
    } else {
      txt_lst$susp <- NULL
    }
    if (ei == "clip") {
      txt_lst$evid <- input$evid_raw
    } else {
      txt_lst$evid <- NULL
    }
    return(txt_lst)
  })
  
  get_sr_text <- reactive({
    si <- susp_input_typeInput2()
    if (si == "clip") {
      res <- list("Suspected Text"=input$susp_raw2)
    }
    else {
      txt.lst=list()
      susp.inFile <- input$susp_files2
      for(i in 1:length(susp.inFile[,"name"])){
        txt.lst[[i]] <- readtext(susp.inFile[[i, "datapath"]])$text
      }
      res <- list("Suspected Text"=txt.lst)
    }
    return(res)
  })
  
  get_sm_texts <- reactive({
    withProgress({
      setProgress(message = "Extracting Text...")
      tt <- list()
      lst=list()
      inFile <- input$sm_files
      ll <- length(inFile[,"name"])
      for(i in 1:ll){
        lst[[i]] <- readtext(inFile[[i, "datapath"]])$text
        incProgress(1/ll)
      }
      return(list("Texts"=lst))
    })
  })
  
  get_toks <- reactive({
    a <- get_texts()
    dc_names <- doc_names()[[2]]
    b <- unlist(a$susp)
    c <- unlist(a$evid)
    withProgress({ setProgress(message = "Tokenizing...")
      susp_sent = (b %>% tibble::enframe(name = NULL) %>% unnest_tokens(sentence, value, token = "sentences",to_lower = F))$sentence
      names(susp_sent) <- paste0("susp_sent_", 1:length(susp_sent))
      
      susp_nword <- unlist(lapply(susp_sent, ntoken))
      susp_nsent <- susp_sent[susp_nword >= 3] 
      
      evid_sent <- (c %>% tibble::enframe(name = NULL) %>% unnest_tokens(sentence, value, token = "sentences", to_lower = F))$sentence
      names(evid_sent) <- paste0("evid_sent_", 1:length(evid_sent))
      
      evid_nword <- unlist(lapply(evid_sent, ntoken))
      evid_nsent <- evid_sent[evid_nword >= 3]
      
      corp <- corpus(c(susp_nsent,evid_nsent))
      
      dfm_corp <- dfm(corp, stem = T, remove_punct = T, remove_number = T, remove_symbols = T,tolower = T)
      
      sim <- textstat_simil(dfm_corp, margin = "documents", selection = names(susp_nsent), method = "cosine")
      sim <- sim[-(1:length(susp_nsent)),]
    })
    return(list(similarities = sim,suspect_sentence = susp_sent, evidence_sentence = evid_sent))
  })
  
  get_toks2 <- reactive({
    dc_names <- doc_names()[[2]]
    b <- unlist(input$susp_raw_part)
    c <- unlist(input$evid_raw_part)
    withProgress({ setProgress(message = "Tokenizing...")
      susp_word = (b %>% tibble::enframe(name = NULL) %>% unnest_tokens(sentence,value,token = "words"))$sentence
      names(susp_word) <- paste0("susp_word_", 1:length(susp_word))
      
      evid_word <- (c %>% tibble::enframe(name = NULL) %>% unnest_tokens(sentence,value,token = "words"))$sentence
      names(evid_word) <- paste0("evid_word_", 1:length(evid_word))
      
      corp <- corpus(c(susp_word,evid_word))
      dfm_corp <- dfm(corp, stem = T,remove_punct = T, remove_number = F, remove_symbols = F)
      sim <- textstat_simil(dfm_corp, margin = "documents", selection = names(susp_word), method = "cosine")
      sim <- sim[-(1:length(susp_word)),]
    })
    return(list(similarities = sim,suspect_word = susp_word, evidence_word = evid_word))
  })
  
  highlight_t2 <- reactive({
    tks <- get_toks2()
    pwc <- tks$similarities
    withProgress({ setProgress(message = "Highlighting...")
      num_evid <- as.numeric(unlist(lapply(1:nrow(pwc), function(.){
        tail(unlist(strsplit(rownames(pwc)[.],"_")),1)
      })))
      num_susp <-as.numeric(unlist(lapply(1:ncol(pwc), function(.){
        tail(unlist(strsplit(colnames(pwc)[.],"_")),1)
      })))
      
      sent1 <- unlist(tks$suspect_word)
      sent2 <- unlist(tks$evidence_word)
      
      for (i in 1:length(num_susp)) {
        sumans = length(which(pwc[,i] > 0.7))
        if (input$sim_mes_susp == "Similiar") {
          if (sumans > 0 ) {
            ssim <- c()
            ssim <- which.max(pwc[,i])
            sent1[num_susp[i]] = paste0('<mark> ',sent1[num_susp[i]],'</mark>')
          }
        }
        else {
          if (sumans <= 0 ) {
            ssim <- c()
            ssim <- which.max(pwc[,i])
            sent1[num_susp[i]] = paste0('<mark> ',sent1[num_susp[i]],'</mark>')
          }
        }
      }
      
      for (j in 1:length(num_evid)) {
        sumans = length(which(pwc[j,] > 0.7))
        if (input$sim_mes_evid == "Similiar") {
          if (sumans > 0 ) {
            esim <- c()
            esim <- which.max(pwc[j,])
            sent2[num_evid[j]] = paste0('<mark> ',sent2[num_evid[j]],'</mark>')
          }
        }
        else {
          if (sumans <= 0 ) {
            esim <- c()
            esim <- which.max(pwc[j,])
            sent2[num_evid[j]] = paste0('<mark> ',sent2[num_evid[j]],'</mark>')
          }
        }
      }
      
      stor1 <- paste0(sent1, collapse = " ")
      
      stor2 <- paste0(sent2, collapse = " ")
      
      output$hp_susp <- renderUI({
        HTML(stor1)
      })
      
      output$hp_evid <- renderUI({
        HTML(stor2)
      })
    })
    return(list("susp_high" = stor1, "evid_high" = stor2))
  })
  
  highlight_t <- reactive({
    gt <- get_texts()
    tks <- get_toks()
    pwc <- tks$similarities
    withProgress({ setProgress(message = "Highlighting...")
      num_evid <- as.numeric(unlist(lapply(1:nrow(pwc), function(.){
        tail(unlist(strsplit(rownames(pwc)[.],"_")),1)
      })))
      num_susp <-as.numeric(unlist(lapply(1:ncol(pwc), function(.){
        tail(unlist(strsplit(colnames(pwc)[.],"_")),1)
      })))
      
      b <- gt$susp
      c <- gt$evid
      d <- unlist(gt$evid)
      evid_leng <- c(0)
      susp_leng <- c(0)
      
      for (i in 1:length(c)) {
        evid_leng[i+1] <- nsentence(c[[i]]) + evid_leng[i]
      }
      for (i in 1:length(b)) {
        susp_leng[i+1] <- nsentence(b[[i]]) + susp_leng[i]
      }
      
      sent1 <- unlist(tks$suspect_sentence)
      sent2 <- unlist(tks$evidence_sentence)
      
      m = 0
      for (i in 1:length(num_susp)) {
        sumans = length(which(pwc[,i] > 0.7))
        if (sumans > 0 ) {
          ssim <- c()
          ssim <- which.max(pwc[,i])
          m = m + 1
          sent1[num_susp[i]] = paste0('<style >A:focus,a:checked { COLOR: white;background-color: blue}; ::selection {color: blue;} </style><a onclick=$(".box-body").css("display","Block");$("#B1")[0].style.display="block" ','id=',paste0('\"susplink_',num_susp[i],'\" '), 'href=',paste0('\"#evidlink_',num_evid[ssim],'\" '),'> ',sent1[num_susp[i]],'</a>')
        }
      }
      
      sent1a = list()
      sent2a = list()
      k = 0
      n = 0
      for (j in 1:length(num_evid)) {
        sumans = length(which(pwc[j,] > 0.7))
        if (sumans > 0 ) {
          esim <- c()
          esim <- which.max(pwc[j,])
          n = n + 1
          sent2[num_evid[j]] = paste0('<style >A:focus,a:checked { COLOR: white;background-color: blue};A{background-color: green} </style><a ','id=',paste0('\"evidlink_',num_evid[j],'\" '), 'href=',paste0('\"#susplink_',num_susp[esim],'\" '),'> ',sent2[num_evid[j]],'</a>')
        }
      }
      
      for (i in 1:(length(susp_leng)-1)) {
        sent1a[[i]] = sent1[(susp_leng[i]+1):susp_leng[i+1]]
      }
      
      for (i in 1:(length(evid_leng)-1)) {
        sent2a[[i]] = sent2[(evid_leng[i]+1):evid_leng[i+1]]
      }
      
      stor1 <- lapply(1:length(sent1a), function(.){
        return(paste0(sent1a[[.]], collapse = " "))
      })
      
      stor2 <- lapply(1:length(sent2a), function(.){
        return(paste0(sent2a[[.]], collapse = " "))
      })
      
      ans1 <- lapply(1:length(stor1), function(.){
        output[[paste0("hightext_susp", .)]] <- renderUI({
          HTML(stor1[[.]])
        })
      })
      do.call(column,c(ans1,width = 12))
      
      ans2 <- lapply(1:length(stor2), function(.){
        output[[paste0("hightext_evid", .)]] <- renderUI({
          HTML(stor2[[.]])
        })
      })
    })
    do.call(column,c(ans2,width = 12))
  })
  
  shinyjs::onclick("susplink_1", js$collapse(id = "B1"))
  
  observeEvent(input$load_file, {
    updateTabItems(session, "tabs", "subMenuViewData")
  }
  )
  
  observeEvent(input$go_view, {
    updateTabsetPanel(session, "Main_Tab",
                      selected = "tab_view")
  })
  
  get_texts <- reactive({
    f <- text_list()
    r <- raw_text_list()
    texts <- list()
    if (is.null(f$susp) & !is.null(r$susp)) {texts$susp <- r$susp}
    else if (!is.null(f$susp) & is.null(r$susp)) {texts$susp <- f$susp}
    if (is.null(f$evid) & !is.null(r$evid)) {texts$evid <- r$evid}
    else if (!is.null(f$evid) & is.null(r$evid)) {texts$evid <- f$evid}
    return(texts)
  })
  
  get_texts2 <- reactive({
    sus_raw <- input$susp_raw_part
    ev_raw <- input$evid_raw_part
    texts <- list()
    if (sus_raw == "") {
      texts$susp = "No text found in suspected text-box"
    }
    else {
      texts$susp <- sus_raw
    }
    if (ev_raw == "") {
      texts$evid = "No text found in evidence text-box"
    }
    else {
      texts$evid <- ev_raw
    }
    return(texts)
  })
  
  doc_names <- reactive({
    susp.inFile <- input$susp_files
    evid.inFile <- input$evid_files
    if (is.null(susp.inFile) || is.null(evid.inFile)) {return(NULL)}
    else {return(list(susp.inFile$name, evid.inFile$name))}
  })
  
  observe({ 
    if (USER$Logged == FALSE) {
      if (!is.null(input$Login)) {
        if (input$Login > 0) {
          Username <- isolate(input$userName)
          Password <- isolate(input$passwd)
          if(my_username == Username & my_password == Password) {
            USER$Logged <- TRUE
          } else {SEC$Security <- FALSE}
        } 
      }
    }    
  })
  
  observe({
    if (USER$Logged == FALSE) {output$page <- renderUI({login()})}
    if (USER$Logged == TRUE) {
      output$page <- renderUI({body()})
      output$sidebar = renderUI({sidebar()})
    }
  })
  
  observe({
    output$dataInfo <- renderText({
      if (SEC$Security) {""}
      else {"Your username or password is not correct"}
    })
  })
  
  output$susp_method <- renderUI({
    si <- susp_input_typeInput()
    if (si == "clip") {
      textAreaInput("susp_raw", NULL, 
                    placeholder = "Suspected text...", width="100%")
    }
    else {
      tagList(
        fileInput("susp_files", NULL, multiple = TRUE,
                  accept = c(".pdf", ".txt", ".docx", ".doc"))
      )
    }
  })
  
  output$evid_method <- renderUI({
    ei <- evid_input_typeInput()
    if (ei == "clip") {
      textAreaInput("evid_raw", NULL, 
                    placeholder = "Evidence text...", width="100%")
    }
    else {
      fileInput("evid_files", NULL, multiple = TRUE,
                accept = c(".pdf", ".txt", ".docx", ".doc"))
    }
  })
  observeEvent(input$susp_files,{
    req(input$go)
    test_file <- readBin(input$susp_files$datapath, what="raw",n=input$susp_files$size)
    writeBin(test_file, paste0(directory,"myreport.pdf"))
  })
  observeEvent(input$file1,{
    tks <- get_toks2()
    pwc <- tks$similarities
    num_evid <- as.numeric(unlist(lapply(1:nrow(pwc), function(.){
      tail(unlist(strsplit(rownames(pwc)[.],"_")),1)
    })))
    num_susp <-as.numeric(unlist(lapply(1:ncol(pwc), function(.){
      tail(unlist(strsplit(colnames(pwc)[.],"_")),1)
    })))
    sent1 <- unlist(tks$suspect_word)
    sent1a <- list()
    sent2 <- unlist(tks$evidence_word)
    sent2a <- list()
    
    sims <- sim()
    
    for (i in 1:length(num_susp)) {
      sumans = length(which(pwc[,i] > 0.7))
      if (input$sim_mes_susp == "Similiar") {
        if (sumans > 0 ) {
          sent1a[[i]] = hyperlink_text(paste0(sent1[num_susp[i]]," ") ,url = "#", props = fp_text(shading.color = "red"))
        }
        else{
          sent1a[[i]] = hyperlink_text(paste0(sent1[num_susp[i]]," "),url = "#")
        }
      }
      else {
        if (sumans <= 0 ) {
          sent1a[[i]] = hyperlink_text(paste0(sent1[num_susp[i]]," "),url = "#", props = fp_text(shading.color = "red"))
        }
        else{
          sent1a[[i]] = hyperlink_text(paste0(sent1[num_susp[i]]," "),url = "#")
        }
      }
    }
    for (j in 1:length(num_evid)) {
      sumans = length(which(pwc[j,] > 0.7))
      if (input$sim_mes_evid == "Similiar") {
        if (sumans > 0 ) {
          sent2a[[j]] = hyperlink_text(paste0( sent2[num_evid[j]]," "),url = "#", props = fp_text(shading.color = "red"))
        }
        else {
          sent2a[[j]] = hyperlink_text(paste0( sent2[num_evid[j]]," "),url = "#")
        }
      }
      else {
        if (sumans <= 0 ) {
          sent2a[[j]] = hyperlink_text(paste0( sent2[num_evid[j]]," "),url = "#", props = fp_text(shading.color = "red"))
        }
        else {
          sent2a[[j]] = hyperlink_text(paste0( sent2[num_evid[j]]," "),url = "#")
        }
      }
    }
    
    bold_face <- shortcuts$fp_bold(font.size = 13)
    bold_redface <- update(bold_face, color = "red")
    fpar_ <- fpar(ftext("Results", prop = bold_face))
    myft <- flextable(data.frame("Suspected Text"=c("","","","",""),"Evidence Text"=c("","","","","")),cwidth = 3.4,theme_fun = theme_zebra)
    myft <- compose(myft, i = 3, j = 1,
                    value = as_paragraph(
                      as_chunk(paste0(sims$Metric, collapse = "\n"))
                    ),
                    part = "body")
    myft <- compose(myft, i = 3, j = 2,
                    value = as_paragraph(
                      as_chunk(paste0(paste0(round(sims$Similarity, 2), "%"), collapse = "\n"))
                    ),
                    part = "body")
    myft <- compose(myft, i = 4, j = 1,
                    value = as_paragraph(
                      as_chunk(input$sim_mes_susp)
                    ),
                    part = "body")
    myft <- compose(myft, i = 4, j = 2,
                    value = as_paragraph(
                      as_chunk(input$sim_mes_evid)
                    ),
                    part = "body")
    myft <- compose(myft, i = 5, j = 1,
                    value = as_paragraph(
                      list_values = sent1a
                    ),
                    part = "body")
    myft <- compose(myft, i = 5, j = 2,
                    value = as_paragraph(
                      list_values = sent2a
                    ),
                    part = "body")
    myft <- align(myft, align = "left", part = "body")
    myft <- align(myft, align = "left", part = "header")
    
    doc <- read_docx(input$file1$datapath) %>% body_add_fpar(fpar_) %>% body_add_flextable(myft)
    
    print(doc, target = input$file1$datapath)
  })
  output$myFile <- downloadHandler(    
    # generate file name
    filename = function() {     
      input$file1$name
    },
    # set file content
    content  = function(file) {  
      file.copy(input$file1$datapath , file)
    }
  )
  observeEvent(input$evid_files,{
    req(input$go)
    req(input$evid_files)
    test2_file <- readBin(input$evid_files$datapath, what="raw",n=input$evid_files$size)
    writeBin(test2_file, paste0(directory,"output.pdf"))
  })
  
  observeEvent(input$susp_files2,{
    test_file <- readBin(input$susp_files2$datapath, what="raw",n=input$susp_files2$size)
    writeBin(test_file, paste0(directory,"myreport3.pdf"))
  })
  
  output$viewerbs2 <-  renderUI({
    req(input$susp_files2)
    column(12,
           tags$head(tags$style(HTML('
                                     .modal-lg {
                                     width:  50%;
                                     }
                                     '))),
           bsModal("modalExample", "PDF VIEW", "view2", size = "large",tabPanel(title = "PDF view", tags$iframe(style = "height:650px;width:100%;scrolling = yes",
                                                                                                                src=paste0(directory,"myreport3.pdf")))),
           tags$head(tags$style("#modalExample .modal-footer{ display:none}")))
  })
  
  observeEvent(input$go2,{
    updateTabsetPanel(session,"Main_Tab_sr",selected = "tab_view2")
  })
  
  sm_texts_corpus_obj <- reactive({
    txx <- get_sm_texts()$Texts
    nms <- sm_doc_names()
    tx_corp <- lapply(txx, function(.) {
      crp <- quanteda::corpus(.)
    })
    return(tx_corp)
  })
  
  sm_texts_word_tokens_stemmed <- reactive({
    corpus <- sm_texts_corpus_obj()
    word_tokens <- lapply(corpus, function(.) {
      toks <- tokens(., what="word", remove_numbers=T, remove_punct = T)
      return(tokens_wordstem(toks))
    })
    return(word_tokens)
  })
  
  sm_texts_word_tokens_stemmed_global <- reactive({
    txx <- unlist(get_sm_texts()$Texts, use.names = T)
    nms <- sm_doc_names()
    tx_corp <- quanteda::corpus(txx)
    toks <- tokens(tx_corp, what="word", remove_numbers=T, remove_punct = T)
    stemmed <- tokens_wordstem(toks)
    return(stemmed)
  })
  
  sm_clust_members <- reactiveVal()
  sm_source_docs_colors <- reactiveVal()
  
  observeEvent(input$show_clusters, {
    pp <- sm_clus_plot()
    km <- sm_clus_res()
    nms <- sm_doc_names()
    names_vec <- names(km$cluster)
    output$stylo_clust_obj <- renderPlotly({
      gg <- ggplotly(pp, tooltip = "cluster")
      mytext=paste("Document: ", names_vec, "\n" , "Cluster: ", km$cluster, "\n",sep="")
      plotly::style(gg, text=mytext, hoverinfo = "text", traces = 1:length(nms))
    })
  })
  
  observeEvent(input$show_documents, {
    nms <- sm_doc_names()
    num_docs <- length(nms)
    doc_colors <- sm_source_docs_colors()
    mytext <- sm_source_docs()
    dfff <- match(nms, mytext)
    dg <- mytext[dfff]
    nm <- reactiveValuesToList(num_chunks_in_docs)
    nm_m <- nm %>% map_depth(1, 2) %>% flatten()
    n_ <- names(nm$n)
    n__ <- nm$c
    nm_cols <- factor(mytext)
    nm_cols2 <- as.character(nm_cols)
    
    summ <- summary(nm_cols)
    nmm <- sapply(1:length(dfff), function(.) {
      rep(doc_colors[.], dfff[.])
    })
    colorrr = rep(NA, length=length(mytext))
    for (idx in seq_along(mytext)) {
      for (d_idx in seq_along(doc_colors)) {
        colorrr[which(mytext==nms[d_idx])] = doc_colors[d_idx]
      }
    }
    pp <- sm_clus_plot()
    km <-  sm_clus_res()
    km$clust_plot$data$cluster <- nm_cols
    km_d <- as.data.frame(km$clust_plot$data)
    nmc <- factor(names(km$cluster))
    pl <- pp + scale_colour_manual(values = nm_cols, labels = nms) + scale_fill_manual(name = '', values =  nm_cols) #+ scale_shape_manual(values = nm_cols)
    
    output$stylo_clust_obj <- renderPlotly({
      ggl <- ggplotly(pl, tooltip = "cluster")
      plotly::style(ggl, text=mytext, hoverinfo = "text", traces = 1:length(unique(mytext)))
      
    })
  })
  
  output$stylo_clust_output <- renderUI({
    cll <- sm_clust_members()
    ccc <- sm_marked_stuff()
    
    chunker <- ccc$marked
    ii <- ccc$ids
    nnms <- names(chunker)
    
    chunky <- lapply(1:length(chunker), function(.) {
      tt <- paste0(chunker[[.]], sep="", collapse = " ")
      return(tt)
    })
    
    if (!is.null(cll)) {
      column(12,
             fluidRow(plotlyOutput("stylo_clust_obj")),
             # fluidRow(
             #          column(width = 6, style = 'padding-left: 0px; padding-right: 0px;', 
             #                 actionButton(style = ' background: #343e48; color: white', 
             #                              inputId = "show_clusters", label = "Colour by cluster", width = "100%")), 
             #          column(width = 6, style = 'padding-left: 0px; padding-right: 0px;', 
             #                 actionButton(style = ' background: #343e48; color: white', 
             #                              inputId = "show_documents", label = "Colour by document", width = "100%"))),
             fluidRow(box(collapsible = T, collapsed = T, style = 'overflow-y: scroll',
                          title = paste0("Writing Styles - ", length(unique(cll$cl)), " clusters"), 
                          width = NULL, status = "primary",
                          div(style = 'overflow-y: scroll', 
                              uiOutput("sm_doclist")
                          )
             ))
      )
    }
  })
  
  num_chunks_in_docs <- reactiveValues()
  
  output$sm_doclist <- renderUI({
    clusts <- sm_clust_members()
    if (!is.null(clusts)) {
      nms <- names(clusts$cl)
      num_clusts <- length(unique(clusts$cl))
      colorss <- clusts$cols_levs
      
      cluster_display_labels <- paste0("Cluster ", 1:num_clusts)
      boxes <- lapply(1:num_clusts, function(num){
        clust_idx <- (clusts$cl == num)
        num_in_clust <- sum(clust_idx)
        mems <- ifelse(num_in_clust==1, " member", " members")
        nn <- nms[clust_idx]
        return(column(12, box(id = paste0("doc_box_", num), width = 300, collapsible = TRUE, collapsed = TRUE,
                              style = 'overflow-y: scroll',
                              title = tags$p(style = paste0('color: ', colorss[num]), 
                                             paste0(cluster_display_labels[num], " - ", num_in_clust, mems)), 
                              status = "info", 
                              disp_obj <- lapply(1:length(nn), function(.) {
                                linker <- paste0("clust_", num, "_member_", .)
                                fluidRow(column(12,
                                                actionLink(style = paste0('color: ', colorss[num]), 
                                                           inputId=linker, label=nn[.]), offset = 1))
                              }))
        ))
      })
      do.call(column,c(boxes, width = 12))
    }
    else return(NULL)
  })
  
  output$sm_files_info <- renderUI({
    txx <- get_sm_texts()$Texts
    nms <- sm_doc_names()
    num_words <- lapply(1:length(txx), function(.) {
      return(ntoken(txx[[.]]))
    })
    disp_obj <- lapply(1:length(txx), function(.) {
      dd <- paste0(num_words[[.]], " words")
      tags$li(HTML(paste0(nms[.], " :  ", tags$b(dd, style = 'text-decoration: underline; font-size: 18px;'))))
    })
    do.call(tags$ul, c(disp_obj))
  })
  
  output$sm_feat_choose <- renderUI({
    sel <- input$stylo_feat_choice
    if (sel == "mfw") {
      column(12,
             fluidRow(numericInput("mfw_num", "Number of function words",value = 20, min = 10, max=200, step=1))
      )
    } 
    else if (sel=="lex") {
      column(12,
             column(6, checkboxGroupInput("char_feat_choice", h3("Character features"), 
                                          choices =  c("Number of characters"="num.all_characters", "Proportion alphabetical"="prop.alpha", 
                                                       "Proportion upper case characters"="prop.upcase", "Proportion numerical digits"="prop.digits", 
                                                       "Proportion white spaces"="prop.white_space", "Proportion space delimiters"="prop.space", 
                                                       "Frequencies of letters (26)"="freq.alpha", "Frequencies of special characters (21)"="freq.spec_chars"),
                                          selected = c("num.all_characters", "prop.alpha", "prop.upcase", 
                                                       "prop.digits", "prop.white_space", "prop.space",
                                                       "freq.alpha", "freq.spec_chars"))
             ),
             column(6, checkboxGroupInput("word_feat_choice", h3("Word features"), 
                                          choices = c("Number of words"="num.words", "Proportion short words"="prop.short_words", 
                                                      "Proportion characters in words"="prop.charsWords", 
                                                      "Mean word length"="avg.word_length", 
                                                      "Mean number of characters per sentence"="avg.num_chars_sentence", 
                                                      "Mean number of words per sentence"="avg.num_words_sentence", 
                                                      "Proportion of words unique"="prop.unique_words", 
                                                      "Mean number of syllables per word"="mean_num_syllables_per_word", 
                                                      "Number of punctuation characters"="num_punctuation",
                                                      "Hapax Legomena - number of once-occurring words"="hapax_legomena", 
                                                      "Hapax Dislegomena - number of twice-occurring words"="hapax_dislegomena",
                                                      "Yule's K - Readability measure"="yules.k",
                                                      "Simpson's D - readability measure"="simpsons.d"),
                                          selected = c("num.words", "prop.short_words", "prop.charsWords", "avg.word_length",
                                                       "avg.num_chars_sentence", "avg.num_words_sentence", "prop.unique_words",
                                                       "mean_num_syllables_per_word", "num_punctuation", "hapax_legomena", "hapax_dislegomena", 
                                                       "yules.k","simpsons.d"))
             ))
    }
  })
  
  sm_marked_stuff <- reactiveVal()
  
  sm_texts_corpus_obj <- reactive({
    txx <- get_sm_texts()$Texts
    xx <- unlist(txx)
    nms <- sm_doc_names()
    names(xx) <- nms
    num_words_chunks <- input$mfw_win
    tx_corp <- lapply(txx, function(.) {
      crp <- quanteda::corpus(.)
    })
    marked_texts <- place_chunk_markers(xx, chunk_length = num_words_chunks)
    sm_marked_stuff(marked_texts)
    return(tx_corp)
  })
  
  sm_alltexts_corpus_obj <- reactive({
    txx <- unlist(get_sm_texts()$Texts)
    nms <- sm_doc_names()
    names(txx) <- nms
    tx_corp <- corpus(txx)
    docnames(tx_corp) <- nms
    return(tx_corp)
  })
  
  
  sm_texts_word_tokens_stemmed <- reactive({
    corpus <- sm_texts_corpus_obj()
    word_tokens <- lapply(corpus, function(.) {
      toks <- tokens(., what="word", remove_numbers=T, remove_punct = T)
      return(tokens_wordstem(toks))
    })
    return(word_tokens)
  })
  
  sm_texts_word_tokens_stemmed_global <- reactive({
    txx <- unlist(get_sm_texts()$Texts, use.names = T)
    nms <- sm_doc_names()
    tx_corp <- quanteda::corpus(txx)
    toks <- tokens(tx_corp, what="word", remove_numbers=T, remove_punct = T)
    stemmed <- tokens_wordstem(toks)
    return(stemmed)
  })
  
  chunks_style_invariant <- reactive({
    win.size <- isolate({input$mfw_win})
    num.func_words <- isolate({input$mfw_num})
    txx <- sm_texts_word_tokens_stemmed()
    nms <- sm_doc_names()
    
    withProgress({
      setProgress(message = "Processing chunks...")
      global_dfm <- dfm(sm_texts_word_tokens_stemmed_global())
      mfws <- topfeatures(global_dfm, num.func_words)
      dict <- dictionary(list(mfw=names(mfws)))
      mfw_labs <- paste0("mfw_", 1:num.func_words)
      di <- names(mfws)
      names(di) <- mfw_labs
      di <- as.list(di)
      dict <- dictionary(di)
      mats <- list()
      res <- list()
      for (tx in seq_along(txx)) {
        tks <- tokens(txx[[tx]])
        chunks <- tokens_chunk(tks, size=win.size)
        ds <- dfm(chunks, dictionary = dict)
        n.c <- length(chunks)
        wins <- names(chunks)
        chunk_id <- paste0(nms[tx], "_", 1:n.c)
        res[[tx]] <- list(chunk=wins, id=nms[tx], chunk_id=paste0(nms[tx], "_", 1:n.c), xtra=chunks)
        mats[[tx]] <- as.matrix(ds)
        incProgress(1/length(txx))
      }
    })
    rr <- list(res, mats)
    return(rr)
  })
  
  output$clust_params <- renderUI({
    sel <- input$stylo_feat_choice
    if (sel == "mfw") {
      column(12,
             fluidRow(selectInput("clustering_method_choice", "",
                                  choices =c("K-Means"="kmeans","Partitioning Around Medoids"="pam","Clustering Large Applications"="clara", 
                                             "Fuzzy Analysis Clustering"="fanny", "Hierarchical Clustering"="hclust"),
                                  selected = "kmeans")),
             fluidRow(checkboxInput("tsne_bool", "t-SNE preprocessing", value = T)),
             fluidRow(bsButton("calc_stylo_clusts", "Calculate"))
      )
    }
    else {
      column(12,
             # fluidRow(selectInput("distance_measure_choice", "Distance Measure",
             #                      choices = c("Cosine"="cosine", "Correlation"="correlation",
             #                                  "Euclidean"="euclidean"),
             #                      selected = "euclidean")),
             fluidRow(selectInput("clustering_method_choice", "",
                                  choices =c("K-Means"="kmeans","Partitioning Around Medoids"="pam","Clustering Large Applications"="clara", 
                                             "Fuzzy Analysis Clustering"="fanny", "Hierarchical"="hclust"),
                                  selected = "kmeans")),
             fluidRow(checkboxInput("tsne_bool", "t-SNE preprocessing", value = T)),
             fluidRow(bsButton("calc_stylo_clusts", "Calculate"))
      )
    }
  })
  cluster_links <- reactiveValues()
  
  sm_all_marked <- reactiveVal()
  
  sm_source_docs <- reactiveVal()
  
  sm_clus_plot <- reactiveVal()
  
  sm_clus_res <- reactiveVal()
  
  observeEvent(input$calc_stylo_clusts, {
    sel <- isolate({input$stylo_feat_choice})
    num.func_words <- isolate({input$mfw_num})
    num_words_chunks <- isolate({input$mfw_win})
    clus_m <- input$clustering_method_choice
    # dist_m <- input$distance_measure_choice
    tsne_ <- input$tsne_bool
    
    if (sel == "mfw") {
      c <- chunks_style_invariant()
      x <- c[[2]]
      x_m <- c[[1]]
      labs <- c()
      x_mm <- unlist(lapply(1:length(x_m), function(.) {
        return(x_m[[.]]$chunk_id)
      }))
      x_df <- rep(NA, num.func_words)
      for (ii in 1: length(x)) {
        x_df <- rbind(x_df, x[[ii]])
      }
      x <- as.data.frame(x_df)[-1,]
      
      
      nms <- sm_doc_names()
      
      withProgress({ 
        if (tsne_) {
          setProgress(message = "Calculating t-SNE projections...")
          perp <- ((nrow(x) - 1)/3) - 1
          x_tsne <- Rtsne(x, perplexity = perp)
          x <- as.data.frame(x_tsne$Y)
        }
        rownames(x) <- x_mm
        setProgress(message = "Clustering...")
        res.km <- eclust(x, FUNcluster = clus_m)
        sm_clus_res(res.km)
        
        colsss <- rainbow(length(unique(res.km$cluster)))#brewer.pal(length(unique(res.km$cluster)), "Paired")
        color_vec <- rainbow(length(nms))#brewer.pal(length(nms), "Paired")
        sm_source_docs_colors(color_vec)
        colsss_ <- colsss[res.km$cluster]
        names_vec <- names(res.km$cluster)
        names_vec_split <- unlist(lapply(1:length(names_vec), function(.) {
          sp <- unlist(strsplit(names_vec[.], "_", fixed = T))
          return(paste0(head(sp, -1), collapse = "_"))
        }))
        nm_cols <- factor(names_vec_split)
        sm_source_docs(names_vec_split)
        sm_clust_members(list(cl=res.km$cluster, colss = colsss_, cols_levs = colsss))
        
        c_ <- sm_marked_stuff()
        ids_ <- c_$ids
        segids_ <- c_$sid
        res.km$clust_plot$data$docs <- nm_cols
        nm_nm <- unique(as.numeric(nm_cols))
        pp <- fviz_cluster(object = res.km, repel = T, show.clust.cent = T, geom = "point") +
          # geom_point(aes(shape = docs)) +
          labs(title = "Writing Style Clusters") +
          xlab("Writing Signal 1") + ylab("Writing Signal 2") +
          scale_colour_manual(values = colsss, labels = paste0("Cluster ", 1:max(res.km$cluster))) +
          scale_fill_manual(values = colsss) +
          theme(plot.title = element_text(color = "white"),
                axis.text = element_text(color = "white"),
                axis.title.x = element_text(color = "white"),
                axis.title.y = element_text(color = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.background = element_rect(fill = "#2d3741"),
                legend.text = element_text(color = "white"),
                legend.title = element_text(color = "white", hjust = 0),
                panel.background = element_rect(fill = "#46515a", color = "#2d3741",
                                                size = 2, linetype = "solid"),
                plot.background = element_rect(fill = "#46515a")) #+ geom_point(aes(shape = nm_cols, col = colsss_))
        sm_clus_plot(pp)
        output$stylo_clust_obj <- renderPlotly({
          gg <- ggplotly(pp, tooltip = "cluster")
          mytext=paste("Document: ", names_vec, "\n" , "Cluster: ", res.km$cluster,  "\n",sep="")    
          # pp=plotly_build(gg)   
          plotly::style(gg, text=mytext, hoverinfo = "text", traces = 1:length(nms))
        })
        
        output$sm_chunks_mems <- renderUI({
          c <- sm_marked_stuff()
          clus <- sm_clust_members()
          colors_vec <- clus$colss
          chunks <- c$marked
          ids <- c$ids
          segids <- c$sid
          nms <- names(chunks)
          og_names <- sm_doc_names()
          chunks_names <- nms
          num_docs <- length(og_names)
          linky <- paste0("doc_", ids, "_chunk_", segids)
          link_docs <- unlist(lapply(1:length(linky), function(.) {
            nr <- unlist(strsplit(linky[.], "_"))
            return(nr[2])
          }))
          real_marked <- lapply(1:length(chunks), function(lii) {
            tr_ <- chunks[[lii]]
            tr_[1] <- paste0('<span style=\"background-color: ', colors_vec[lii], '; color: black; text-decoration: none\">')
            tr_ <- paste0(paste0(tr_, collapse = " ", sep = ""), "\n")
            return(tr_)
          })
          
          sm_all_marked(real_marked)
          boxes <- lapply(1:num_docs, function(num){
            chunks_idx <- (ids == num)
            colorss <- colors_vec[chunks_idx]
            num_in_clust <- sum(chunks_idx)
            num_chunks_in_docs[[og_names[num]]] <- list(n=num_in_clust, c=rep(color_vec[num], num_in_clust))
            mems <- ifelse(num_in_clust==1, " chunk", " chunks")
            nn <- og_names[chunks_idx]
            nn_ <- chunks_names[chunks_idx]
            pop_cont_idx <- paste0(unlist(real_marked[chunks_idx]), collapse = "\n")
            
            return(tags$ul(column(12, box(id = paste0("docs_box_", num), width = "100%", collapsible = TRUE, collapsed = TRUE,
                                          style = 'overflow-y: scroll',
                                          title = tags$p(style = 'font-size: 11px',
                                                         paste0(og_names[num], " - ", num_in_clust, mems)), 
                                          status = "info", 
                                          disp_obj <- lapply(1:length(nn_), function(.) {
                                            linker <- paste0("doc_", num, "_chunk_", .)
                                            fluidRow(column(12,
                                                            tags$li(style = paste0('color: ', colorss[.]), actionLink(style = paste0('color: ', colorss[.], '; font-size: 9px; 
                                                                                                                                     a:visited {background-color: transparent;}'), 
                                                                                                                      inputId=linker, label=nn_[.])),
                                                            bsModal(id = paste0("modal_doc_", num), title = og_names[num], trigger = linker,
                                                                    HTML(pop_cont_idx)), offset = 1))
                                            
                                          })
                                            ))))
          })
          column(12,
                 fluidRow(box(title = "Annotated document chunks", collapsible = T, collapsed = T, style = 'overflow-y: scroll', width = "100%",
                              do.call(column, c(boxes, width = 12))
                 )))
        })
      })
      }
    else if (sel == "lex") {
      c <- stylo_features()
      x <- c
      rws <- rownames(x)
      nms <- sm_doc_names()
      withProgress({
        
        if (tsne_) {
          setProgress(message = "Calculating t-SNE projections...")
          perp <- ((nrow(x) - 1)/3) - 1
          x_tsne <- Rtsne(x, perplexity = perp)
          x <- as.data.frame(x_tsne$Y)
        }
        rownames(x) <- rws
        setProgress(message = "Clustering...")
        res.km <- eclust(x, FUNcluster = clus_m)
        sm_clus_res(res.km)
        
        colsss <- rainbow(length(unique(res.km$cluster)))#brewer.pal(length(unique(res.km$cluster)), "Paired")
        color_vec <- rainbow(length(nms))#brewer.pal(length(nms), "Paired")
        
        # colsss <- brewer.pal(length(unique(res.km$cluster)), "Paired")
        colsss_ <- colsss[res.km$cluster]
        # color_vec <- brewer.pal(length(nms), "Paired")
        sm_source_docs_colors(color_vec)
        names_vec <- names(res.km$cluster)
        sm_clust_members(list(cl=res.km$cluster, colss = colsss_, cols_levs = colsss))
        
        c_ <- sm_marked_stuff()
        ids_ <- c_$ids
        segids_ <- c_$sid
        names_vec <- names(res.km$cluster)
        names_vec_split <- unlist(lapply(1:length(names_vec), function(.) {
          sp <- unlist(strsplit(names_vec[.], "_", fixed = T))
          return(paste0(head(sp, -1), collapse = "_"))
        }))
        nm_cols <- factor(names_vec_split)
        res.km$clust_plot$data$docs <- nm_cols
        pp <- fviz_cluster(res.km, repel = T, show.clust.cent = T, geom = "point") + 
          labs(title = "Writing Style Clusters", fill = "Cluster", colour = "Cluster") +
          guides(fill=guide_legend(title="Clusters", 
                                   title.theme = element_text(color = "white"))) + 
          xlab("Writing Signal 1") + ylab("Writing Signal 2") +
          scale_colour_manual(values = colsss, labels = paste0("Cluster ", 1:max(res.km$cluster))) +
          scale_fill_manual(values = colsss) +  #scale_shape_manual(values = as.numeric(nm_cols)) +
          theme(plot.title = element_text(color = "white"),
                axis.text = element_text(color = "white"),
                axis.title.x = element_text(color = "white"),
                axis.title.y = element_text(color = "white"),
                panel.grid.major = element_blank(),
                panel.grid.minor = element_blank(),
                legend.background = element_rect(fill = "#2d3741"),
                legend.text = element_text(color = "white"),
                panel.background = element_rect(fill = "#46515a", color = "#2d3741",
                                                size = 2, linetype = "solid"),
                plot.background = element_rect(fill = "#46515a"))
        sm_clus_plot(pp)
        output$stylo_clust_obj <- renderPlotly({
          gg <- ggplotly(pp, tooltip = "cluster")
          mytext=paste("Document: ", names_vec, "\n" , "Cluster: ", res.km$cluster,  "\n",sep="")    
          # pp=plotly_build(gg)   
          plotly::style(gg, text=mytext, hoverinfo = "text", traces = 1:length(nms))
        })
        
        output$sm_chunks_mems <- renderUI({
          c <- sm_marked_stuff()
          clus <- sm_clust_members()
          colors_vec <- clus$colss
          chunks <- c$marked
          ids <- c$ids
          segids <- c$sid
          nms <- names(chunks)
          og_names <- sm_doc_names()
          chunks_names <- nms
          num_docs <- length(og_names)
          linky <- paste0("doc_", ids, "_chunk_", segids)
          link_docs <- unlist(lapply(1:length(linky), function(.) {
            nr <- unlist(strsplit(linky[.], "_"))
            return(nr[2])
          }))
          real_marked <- lapply(1:length(chunks), function(lii) {
            tr_ <- chunks[[lii]]
            tr_[1] <- paste0('<span style=\"background-color: ', colors_vec[lii], '; color: black; text-decoration: none\">')
            tr_ <- paste0(tr_, collapse = " ", sep = "")
            return(tr_)
          })
          
          sm_all_marked(real_marked)
          boxes <- lapply(1:num_docs, function(num){
            chunks_idx <- (ids == num)
            colorss <- colors_vec[chunks_idx]
            num_in_clust <- sum(chunks_idx)
            mems <- ifelse(num_in_clust==1, " chunk", " chunks")
            nn <- og_names[chunks_idx]
            nn_ <- chunks_names[chunks_idx]
            pop_cont_idx <- paste0(paste0(unlist(real_marked[chunks_idx]), collapse = "\n"), "\n\n")
            return(tags$ul(column(12, box(id = paste0("docs_box_", num), width = "100%", collapsible = TRUE, collapsed = TRUE,
                                          style = 'overflow-y: scroll',
                                          title = tags$p(style = 'font-size: 11px',
                                                         paste0(og_names[num], " - ", num_in_clust, mems)), 
                                          status = "info", 
                                          disp_obj <- lapply(1:length(nn_), function(.) {
                                            linker <- paste0("doc_", num, "_chunk_", .)
                                            fluidRow(column(12,
                                                            tags$li(style = paste0('color: ', colorss[.]), actionLink(style = paste0('color: ', colorss[.], '; font-size: 9px; 
                                                                                                                                     a:visited {background-color: transparent;}'), 
                                                                                                                      inputId=linker, label=nn_[.])),
                                                            bsModal(id = paste0("modal_doc_", num), title = og_names[num],
                                                                    trigger = linker,
                                                                    HTML(pop_cont_idx)), offset = 1))
                                            
                                          })
                                            ))))
          })
          column(12,
                 fluidRow(box(title = "Document chunks", collapsible = T, collapsed = T, style = 'overflow-y: scroll', width = "100%",
                              do.call(column, c(boxes, width = 12))
                 )))
        })
      })
    }
    })
  
  doc_chunks <- reactive({
    win.size <- isolate({input$mfw_win})
    txx <- get_sm_texts()$Texts
    nms <- sm_doc_names()
    xx <- unlist(txx)
    names(xx) <- nms
    marked_texts <- place_chunk_markers(xx, chunk_length = win.size)
    sm_marked_stuff(marked_texts)
    res <- list()
    withProgress({
      setProgress(message = "Processing chunks...")
      for (tx in seq_along(txx)) {
        tks <- tokens(txx[[tx]])
        chunks <- tokens_chunk(tks, size=win.size)
        n.c <- length(chunks)
        chunk_id <- paste0(nms[tx], "_", 1:n.c)
        res[[tx]] <- list(seg_id=chunk_id, chunker=chunks)
        incProgress(1/length(txx))
      }
    })
    return(res)
  })
  
  sm_doc_names <- reactive({
    inFile <- input$sm_files
    if (is.null(inFile)) {return(NULL)}
    else {return(inFile$name)}
  })
  
  stylo_features <- reactive({
    chrf <- isolate({input$char_feat_choice})
    wrdf <- isolate({input$word_feat_choice})
    titles <- sm_doc_names()
    
    chnks <- doc_chunks()
    total_chunks <- sum(unlist(lapply(1:length(chnks), function(.){
      return(length(chnks[[.]]$chunker))
    })))
    txx <- lapply(1:length(chnks), function(.){
      t <- chnks[[.]]$chunker
      t_in <- t
      names(t_in) <- chnks[[.]]$seg_id
      return(t_in)
    })
    total_feats <- (length(chrf) + length(wrdf))*total_chunks
    df <- rep(NA, total_feats)
    row_labels <- "dummy_label_yo"
    withProgress({
      for (doc_idx in 1:length(txx)) {
        row_labels <- c(row_labels, names(txx[[doc_idx]]))
        for (tx_idx in 1:length(txx[[doc_idx]])) {
          tx <- txx[[doc_idx]][[tx_idx]]
          pt <- parse.text(tx)
          pt.text <- pt$text
          # Character features
          setProgress(message = paste0("Extracting character features of chunk ", tx_idx, " in text ", doc_idx, "..."))
          chs <- unlist(strsplit(tx, ""))
          char_feats <- list(num.all_characters = length(chs))
          if ("prop.alpha" %in% chrf) {char_feats$prop.alpha <- length(grep("[[:alpha:]]", chs))/char_feats$num.all_characters;incProgress(1/total_feats)}
          if ("prop.upcase" %in% chrf) {char_feats$prop.upcase <- length(grep("[[:upper:]]", chs))/char_feats$num.all_characters;incProgress(1/total_feats)}
          if ("prop.digits" %in% chrf) {char_feats$prop.digits <- length(grep("[[:digit:]]", chs))/char_feats$num.all_characters;incProgress(1/total_feats)}
          if ("prop.white_space" %in% chrf) {char_feats$prop.white_space <- length(grep("\\s", chs))/char_feats$num.all_characters;incProgress(1/total_feats)}
          if ("prop.space" %in% chrf) {char_feats$prop.space <- length(grep("[[:space:]]", chs))/char_feats$num.all_characters;incProgress(1/total_feats)}
          if ("freq.alpha" %in% chrf) {
            char_feats$freq.alpha <- length(grep("[[:alpha:]]", chs))
            nms <- paste0(LETTERS, "_freq")
            res <- rep(0, 26)
            res <- matrix(nrow=length(res[1]), ncol=length(nms), data = res, dimnames = list(rep("", length(res[1])), nms))
            for (l in 1:26) {
              char_feats[[nms[l]]]<- length(grep(letters[l], tolower(chs)))
            }
            incProgress(1/total_feats)
          }
          if ("freq.spec_chars" %in% chrf) {
            schs <- c("~", "@", "#", "$", "%", "^", "&", "*", "-", "_",
                      "=", "+", ">", "<", "[", "]", "{", "}", "/", "\\", "|")
            res <- rep(0, length(schs))
            for (a in 1:length(schs)) {
              for (c in 1:length(chs)) {
                if (chs[c] == schs[a]) {
                  res[a] <- res[a] + 1
                }
              }
            }
            nms <- paste0("spec_char_", 1:length(res))
            res <- matrix(nrow=length(res[1]), ncol=length(nms), data = res, dimnames = list(rep("", length(res[1])), nms))
            char_feats <- data.frame(char_feats, res)
            incProgress(1/total_feats)
          }
          # Word features
          setProgress(message = paste0("Extracting word features of chunk ", tx_idx, " in text ", doc_idx, "..."))
          word_feats <- list()
          words <- parse.text(tx, tok="words")$element
          word_feats <- list(num.words=length(words))
          if ("prop.short_words" %in% wrdf) {
            count <- 0
            for (i in 1:length(words)) {
              if (length(unlist(strsplit(words[i], ""))) < 4) {count <- count + 1}
            }
            word_feats$prop.short_words <- count/word_feats$num.words
            incProgress(1/total_feats)
          }
          if ("prop.charsWords" %in% wrdf) {word_feats$prop.charsWords <- sum(sapply(words, function(c) {cs <- unlist(strsplit(c, ""));return(length(cs))}))/char_feats$num.all_characters;incProgress(1/total_feats)}
          if ("avg.word_length" %in% wrdf) {word_feats$avg.word_length <- mean(sapply(tx, function(c) {w <- unlist(strsplit(c, ""));return(length(w))}));incProgress(1/total_feats)}
          sents <- parse.text(tx, tok="sentences")$element
          if ("avg.num_chars_sentence" %in% wrdf) {word_feats$avg.num_chars_sentence <- mean(sapply(sents, function(s) {return(length(unlist(strsplit(gsub(" ", "", s), ""))))}));incProgress(1/total_feats)}
          if ("avg.num_words_sentence" %in% wrdf) {word_feats$avg.num_words_sentence <- mean(sapply(sents, function(s) {wrds <- parse.text(s, tok="words")$element;return(length(wrds))}));incProgress(1/total_feats)}
          unique_words <- unique(words)
          if ("prop.unique_words" %in% wrdf) {word_feats$prop.unique_words <- length(unique_words)/word_feats$num.words;incProgress(1/total_feats)}
          freqs <- rep(0, length(unique_words))
          for (uw in 1:length(unique_words)) {
            for (w in 1:length(words)) {
              if (words[w] == unique_words[uw]) {
                freqs[uw] <- freqs[uw] + 1
              }
            }
          }
          freq.words <- data.frame(word=unique_words, freq = freqs)
          if ("hapax_legomena" %in% wrdf) {word_feats$hapax_legomena <- sum(freq.words$freq==1);incProgress(1/total_feats)}
          if ("hapax_dislegomena" %in% wrdf) {word_feats$hapax_dislegomena <- sum(freq.words$freq==2);incProgress(1/total_feats)}
          if ("yules.k" %in% wrdf) {
            summation <- 0
            for (i in 1:word_feats$num.words) {
              summation <- summation + sum(freq.words$freq==i)*(i/word_feats$num.words)^2
            }
            word_feats$yules.k <- (10^4)*((summation - word_feats$num.words)/word_feats$num.words^2)
            incProgress(1/total_feats)
          }
          if ("simpsons.d" %in% wrdf) {
            m <- 0
            for (k in 1:length(freq.words$freq)) {
              m <- m +  freq.words$freq[k] * (freq.words$freq[k] - 1)
            }
            word_feats$simpsons.d <- 1 - (m / (word_feats$num.words * (word_feats$num.words - 1)))
            incProgress(1/total_feats)
          }
          saam <- cbind(char_feats, word=word_feats)
          df <- rbind(df, saam)
        }
      }
    })
    df <- df[-1,]
    df <- df[,(colSums(df) !=0)]
    row_labels <- row_labels[-1]
    rownames(df) <- row_labels
    return(df)
  })
  
  output$evid_method <- renderUI({
    ei <- evid_input_typeInput()
    if (ei == "clip") {
      textAreaInput("evid_raw", NULL, 
                    placeholder = "Evidence text...", width="100%")
    }
    else {
      fileInput("evid_files", NULL, multiple = TRUE,
                accept = c(".pdf", ".txt", ".docx", ".doc"))
    }
  })
  
  output$downloadPlot <- downloadHandler(
    filename = "FG.PDF",
    content = function(file) {
      file.copy("www/0.pdf", file)
    })
  
  output$probeer <- renderUI({
    req(input$go_view)
    t <- highlight_t()
    ts <- unlist(t$susp)
    te <- unlist(t$evid)
    rs <- doc_names()[[1]]
    re <- doc_names()[[2]]
    susplink = paste0("susp_link_", 1:length(ts))
    suspnum <- lapply(1:length(rs), function(num){
      return(column(12, box(id = paste0("D",num),width = 300,collapsible = TRUE, collapsed = F,
                            title = paste0(num, ". ", rs[num]), status = "info", 
                            div(style = 'height: 550; width: 100%;', 
                                htmlOutput(paste0("hightext_susp",num))
                            ))))
    })
    evidlink = paste0("evid_link_", 1:length(te))
    evidnum <- lapply(1:length(re), function(num){
      return(column(12, box(id = paste0("B",num),width = 300,collapsible = TRUE, collapsed = F,
                            title = paste0(num, ". ", re[num]), status = "info", 
                            div(style = 'height: 550; width: 100%;', 
                                htmlOutput(paste0("hightext_evid",num))
                            ))))
    })
    
    column(12,
           column(6,
                  fluidRow(box(
                    title = "Suspected Text", width = NULL, height = 500,status = "primary",
                    div(style = 'height: 450px; overflow-y: scroll', do.call(column,c(suspnum,width = 12)))
                  )),
                  fluidRow(column(10, textAreaInput("susp_raw_part", NULL, 
                                                    placeholder = "Suspected text...", width="150%", height = "100px")))),
           column(6,
                  fluidRow(box(
                    title = "Evidence Text", width = NULL, height = 500,status = "primary",
                    div(style = 'height: 450px; overflow-y: scroll', do.call(column,c(evidnum,width = 12)))
                  )),
                  fluidRow(column(10,textAreaInput("evid_raw_part", NULL, 
                                                   placeholder = "Evidance text...", width="150%", height = "100px")), actionButton("go3", "Compare"))),
           tags$head(tags$style(HTML('
                                     .modal-lg {
                                     width:  100%;
                                     }
                                     '))),
           bsModal(size = "large", id = "SOEK", title = "Similarity Results", "go3", 
                   fluidPage(
                     column(8, uiOutput("compare")),
                     column(4, wellPanel(style = 'background-color: #46515a',
                                         fluidRow(radioButtons("sim_mes_susp", "Similiarity measure suspected text", 
                                                               choices = c("Similiar","Unique"), 
                                                               selected = "Similiar", inline = T),
                                                  radioButtons("sim_mes_evid", "Similiarity measure evidence text", 
                                                               choices = c("Similiar","Unique"), 
                                                               selected = "Similiar", inline = T)
                                         ),
                                         fluidRow(
                                           uiOutput("qgram")
                                         ),
                                         fluidRow(
                                           uiOutput("metric")
                                         ),
                                         fluidRow(
                                           actionButton("calc_sim", "Calculate", width = "100%")
                                         ),
                                         fluidRow(
                                           
                                         ),
                                         fluidRow(
                                           fileInput("file1", "Extract info to",
                                                     multiple = FALSE,
                                                     accept = c(".docx")),
                                           downloadButton(outputId = 'myFile', label = 'Export information') 
                                         ))
                     )
                   ))
           )
    
    
    
    })
  
  max_q <- reactive({
    t <- get_texts2()
    max(length(unlist(strsplit(unlist(t$susp), ""))), length(unlist(strsplit(unlist(t$evid), ""))))/4
  })
  
  output$compare <- renderUI({
    req(input$metric_choice)
    high_part<- highlight_t2()
    mc <- input$metric_choice
    mc.d <- get.metricDescription(mc)
    mc <- unlist(lapply(1:length(mc), function(.) {
      ss <- unlist(strsplit(mc[.], " "))
      st <- paste0(ss, collapse  = "", sep = "")
      return(st)
    }))
    selected <- paste0(mc, "_disp")
    
    results <- lapply(1:length(selected), function(number){
      fluidRow(progressBar(id = selected[number], value = 0, 
                           title = mc.d[number], 
                           display_pct = T, 
                           status = "danger"))
    })
    column(12, 
           fluidRow(column(6,
                           box(title = "Suspected Text",div(style = 'height:240px; overflow-y: scroll',uiOutput("hp_susp")),collapsible = T,width = "100%")),
                    column(6,
                           box(title = "Evidance Text",div(style = 'height: 240px; overflow-y: scroll',uiOutput("hp_evid")),collapsible = T,width = "100%"))),
           fluidRow(hr()),
           fluidRow(do.call(column, c(results, width=12)))
    )
  })
  
  observeEvent(input$calc_sim, {
    req(input$metric_choice)
    mat <- sim()
    updateProgressBar(session = session, id = "cosine_disp", value = mat$Similarity[which(mat$Metric=="Cosine between Q-Gram profiles")])
    updateProgressBar(session = session, id = "ejaccard_disp", value = mat$Similarity[which(mat$Metric=="Extended Jaccard between Q-Gram profiles")])
    updateProgressBar(session = session, id = "edice_disp", value = mat$Similarity[which(mat$Metric=="Extended Dice")])
    updateProgressBar(session = session, id = "simplematching_disp", value = mat$Similarity[which(mat$Metric=="Simple Matching")])
    updateProgressBar(session = session, id = "faith_disp", value = mat$Similarity[which(mat$Metric=="Faith")])
  })
  
  output$simp_pass_btn <- renderUI({
    column(12,
           fluidRow(actionButton(inputId = "sim_pass", label = "Similar words")),
           fluidRow(uiOutput("sim_passages"))
    )
  })
  
  
  observeEvent(input$sim_pass, {
    txt.s <- data.frame("text"=unlist(get_texts2()$susp), stringsAsFactors = F)
    txt.e <- data.frame("text"=unlist(get_texts2()$evid), stringsAsFactors = F)
    
    if (is.null(txt.s) || is.null(txt.e)) {return(NULL)}
    else {
      nnn <- 2
      withProgress(message = "Getting similar passages..", style = "notification", value = 0, {
        res <- list()
        firstvector <- unnest_tokens(tbl = txt.s, output = word, input = text, token = "ngrams", n=nnn)
        secondvector <- unnest_tokens(tbl = txt.e, output = word, input = text, token = "ngrams", n=nnn)
        
        res$match <- character(length(firstvector))
        res$threshold <- 0.2
        res$maxsim <- integer(length(firstvector))
        res$sortedmatches <- character(length(firstvector))
        
        res$suspect <- firstvector
        for (i in 1:length(firstvector) ) {
          matchdist <- stringsim(unlist(firstvector[i]), unlist(secondvector)) # several methods available
          
          matchdist<-ifelse(matchdist < res$threshold, NA, matchdist)
          
          res$sortedmatches[i] <- paste(secondvector$word[order(matchdist, na.last=NA, decreasing = T)], collapse = ", ")
          
          res$maxsim[i]<- tryCatch(ifelse(is.integer(which.max(matchdist)), matchdist[which.max(matchdist)], NA), 
                                   error = function(e){NA})
          res$match[i]<-ifelse(length(secondvector$word[which.max(matchdist)])==0,NA,
                               secondvector$word[which.max(matchdist)])
          
          incProgress(amount = 1/length(firstvector))
        }
        res <- data.frame(res, stringsAsFactors = F)
        output$sim_passages <- renderTable({cbind(res)})
      })
    }})
  
  observeEvent(input$sim_pass1, {
    txt.s <- data.frame("text"=unlist(get_texts2()$susp), stringsAsFactors = F)
    txt.e <- data.frame("text"=unlist(get_texts2()$evid), stringsAsFactors = F)
    if (is.null(txt.s) || is.null(txt.e)) {return(NULL)}
    else {
      withProgress(message = "Getting similar passages..", style = "notification", value = 0, {
        ts <- data.frame(text=txt.s, doc="Suspect", stringsAsFactors = F)
        te <- data.frame(text=txt.e, doc="Evidence", stringsAsFactors = F)
        texts <- data.frame(rbind(ts, te), stringsAsFactors = F)
        
        words <- texts %>%
          unnest_tokens(word, text) %>%
          anti_join(stop_words, by = "word") %>%
          count(doc, word) %>%
          ungroup()
        
        word_tf_idf <- words %>%
          bind_tf_idf(word, doc, n) %>%
          arrange(desc(tf_idf))
        
        sims1 <- word_tf_idf %>% pairwise_similarity(doc, word, tf_idf, upper = FALSE, sort = TRUE)
        sims <- words %>% pairwise_similarity(doc, word, n, upper = FALSE, sort = TRUE)
      })
    }})
  
  progress_links <- reactiveVal()
  
  sim <- reactive({
    req(input$metric_choice, input$q)
    txt.s <- unlist(get_texts2()$susp)
    txt.e <- unlist(get_texts2()$evid)
    if (is.null(txt.s) || is.null(txt.e)) {return(NULL)}
    else {
      txt.sim <- input$metric_choice
      mets <- unlist(lapply(1:length(txt.sim), function(.) {
        ss <- unlist(strsplit(txt.sim[.], " "))
        st <- paste0(ss, collapse  = "", sep = "")
        return(st)
      }))
      selected <- paste0(mets, "_disp")
      progress_links(selected)
      qq <- input$q
      corptxt <- corpus(c(txt.s,txt.e))
      toks <- tokens(corptxt, remove_numbers = T, remove_punct = T, ngrams = qq)
      dfmtxt <- dfm(toks)
      D <- cbind(Metric=txt.sim, Similarity=rep(0.00001, length(txt.sim)))
      if (dim(D)[1] != 0) {
        withProgress(message = "Calculating similaritites...", style = "notification", value = 0, {
          for (metric_it in 1:length(txt.sim)) {
            s <- textstat_simil(dfmtxt, margin = "documents",
                                method = D[metric_it,1])
            D[metric_it,2] <- s
            D[metric_it,1] <- get.metricDescription(D[metric_it,1])
            incProgress(1/length(txt.sim))
          }
        })
        D <- D[order(D[,2], decreasing = T),]
        D <- list(Metric=D[,1], Similarity=as.numeric(D[,2])*100)
      } 
      else {
        D <- NULL  
      }
      return(D)
    }
  })
  
  output$qgram <-  renderUI({
    txt.s <- unlist(get_texts2()$susp)
    txt.e <- unlist(get_texts2()$evid)
    if (is.null(txt.s) || is.null(txt.e))
      return(NULL)
    column(12,
           fluidRow(sliderInput("q", em("Q-gram profiles:",style="color: #cdcdcd; font-size: 100%"),
                                min = 1,
                                max = 25,
                                value = 2, 
                                step = 1))
    )
  })
  
  output$woord_wolk_ptc <- renderPlot({
    s <- unlist(get_texts()$susp)
    e <- unlist(get_texts()$evid)
    s_corp <- corpus(s)
    e_corp <- corpus(e)
    
    d <- doc_names()
    d_s <- d[[1]]
    d_e <- d[[2]]
    doc_ids <- c(d_s, d_e)
    
    docnames(s_corp) <- d_s
    docnames(e_corp) <- d_e
    
    corp <- corpus(c(s_corp, e_corp))
    
    toks <- corp %>% tokens(remove_punct = TRUE, remove_numbers = T, remove_symbols = T) %>% tokens_tolower() %>% tokens_chunk(size = 800)
    
    di <- docvars(toks, "_docid")
    si <- docvars(toks, "_segid")
    ii <- paste0("doc_", di, "_seg_", si)
    docvars(toks, "identifier") <- ii
    docvars(toks, "doc_identifier") <- doc_ids[di]
    
    dfmat2 <- dfm(toks, remove_punct = TRUE, groups = "doc_identifier") %>% dfm_trim(min_termfreq = 10)
    
    return(textplot_wordcloud(dfmat2, comparison = TRUE, max_words = 200, labelsize = 1))
  })
  
  output$metric <- renderUI({
    txt.s <- unlist(get_texts2()$susp)
    txt.e <- unlist(get_texts2()$evid)
    if (is.null(txt.s) || is.null(txt.e))
      return(NULL)
    fluidRow(
      column(10,
             checkboxGroupInput("metric_choice", "Similarity Measure",
                                choices = c("Cosine"="cosine",
                                            "Extended Jaccard"="ejaccard", 
                                            "Extended Dice"="edice",
                                            "Simple Matching"="simple matching", 
                                            "Faith"="faith"),
                                selected = c("cosine", "ejaccard", "edice", "simple matching", "faith"))
      ),
      column(2,
             actionLink("metric_help", "?"),
             bsModal(id = "metric_help_modal", title = "Similarity Measures", trigger = "metric_help",
                     h6("Similarity measures are based on approximating the 'distance' between data representations of character objects and 
                        subtracting that distance from the maximum possible distance. The resulting value is then expressed as a percentage of the maximum possible distance"),
                     h6("The main difference between similarity metrics is the way in which the 'distance' between texts is conceptualised"),
                     h6(tags$em("- \"In mathematics and computer science, a string metric (also known as a string similarity metric or string distance function) 
                                is a metric that measures distance ('inverse similarity') between two text strings for approximate string matching or 
                                comparison and in fuzzy string searching. A necessary requirement for a string metric (e.g. in contrast to string matching) 
                                is fulfillment of the triangle inequality. For example, the strings 'Sam' and 'Samuel' can be considered to be close. 
                                A string metric provides a number indicating an algorithm-specific indication of distance.\""),
                        br(), br(),
                        tags$a("Wikipedia: String metric", href="https://en.wikipedia.org/wiki/String_metric")),
                     hr(),
                     h5("Levenshtein"),
                     tags$ul(
                       tags$li("The Levenshtein measure counts the number of deletions, insertions and substitutions necessary to turn one text into another.")
                     ), 
                     h5("Optimal String Alignment"),
                     tags$ul(
                       tags$li("The Optimal String Alignment measure is like the Levenshtein measure but also allows transposition of 
                               adjacent characters. Here, each substring may be edited only once. (For example, a character cannot be transposed 
                               twice to move it forward in the string).")
                       ), 
                     h5("Full Damerau-Levenshtein"),
                     tags$ul(
                       tags$li("The full Damerau-Levenshtein distance is like the optimal string alignment distance except that it allows for multiple edits on substrings.")
                     ), 
                     h5("Longest Common Substring"),
                     tags$ul(
                       tags$li("The longest common substring is defined as the longest string that can be obtained by pairing characters from a and b 
                               while keeping the order of characters intact. The lcs-distance is defined as the number of unpaired characters. 
                               The distance is equivalent to the edit distance allowing only deletions and insertions, each with weight one.")
                       ), 
                     h5("Q-gram"),
                     tags$ul(
                       tags$li("A q-gram is a subsequence of q consecutive characters of a string. If x (y) is the vector of counts of q-gram occurrences in a (b), 
                               the q-gram distance is given by the sum over the absolute differences of the counts.")
                       ), 
                     h5("Cosine Measure"),
                     tags$ul(
                       tags$li("The cosine distance is computed as the correlation between x and y, where x and y are as defined above.")
                     ), 
                     h5("Jaccard"),
                     tags$ul(
                       tags$li("Let X be the set of unique q-grams in a and Y the set of unique q-grams in b. The Jaccard measure is given by 1-|X AND Y|/|X OR Y|.")
                     ), 
                     h5("Jaro"),
                     tags$ul(
                       tags$li("The Jaro distance, is a number between 0 (exact match) and 1 (completely dissimilar) measuring dissimilarity between 
                               strings. It is defined to be 0 when both strings have length 0, and 1 when there are no character matches between a and b. 
                               Otherwise, the Jaro distance is defined as 1-(1/3)(w_1m/|a| + w_2m/|b| + w_3(m-t)/m). Here,|a| indicates the number of characters 
                               in a, m is the number of character matches and t the number of transpositions of matching characters. 
                               The w_i are weights associated with the characters in a, characters in b and with transpositions. 
                               A character c of a matches a character from b when c occurs in b, and the index of c in a differs less than max(|a|,|b|)/2 -1 
                               (where we use integer division) from the index of c in b. Two matching characters are transposed when they are matched but they 
                               occur in different order in string a and b.")
                       ), 
                     h5("Jaro-Winkler"),
                     tags$ul(
                       tags$li("The Jaro-Winkler distance adds a correction term to the Jaro-distance. 
                               It is defined as d - l\\cdot p\\cdot d, where d is the Jaro-distance. Here, l is obtained by counting, 
                               from the start of the input strings, after how many characters the first character mismatch between 
                               the two strings occurs, with a maximum of four. The factor p is a penalty factor, which in the work of 
                               Winkler is often chosen 0.1.")
                       ), 
                     h5("Soundex"),
                     tags$ul(
                       tags$li("For the soundex distance, strings are translated to a soundex code (see phonetic for a specification). 
                               The distance between strings is 0 when they have the same soundex code, otherwise 1. Note that soundex recoding is only 
                               meaningful for characters in the ranges a-z and A-Z. A warning is emitted when non-printable or non-ascii characters are encountered.")
                       ), footer = NULL)
                       ))
  })
  
  
  get.metricDescription <- function(key) {
    mm <- c("cosine" = "Cosine between Q-Gram profiles",
            "ejaccard" = "Extended Jaccard between Q-Gram profiles",
            "edice" = "Extended Dice",
            "simple matching" = "Simple Matching",
            "faith" = "Faith")
    return(mm[key])
  }
  })