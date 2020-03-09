slider <- function(t, size, step=1, tk="words") {
  t <- unlist(parse.text(t, tok=tk)[,-1])
  n <- length(t)
  if (step > size) {return("Step size larger than window size")}
  else {
    if (size > n) {
      return("Window size larger than text size")
    }
    else {
      n <- length(t)
      numOfChunks <- as.integer(((n - size) / step) + 1)
      chunks <- c()
      ss <- seq(1, numOfChunks*step, by = size)
      for (j in 1:(numOfChunks)) {
        token <- paste(t[j:(j + size - 1)], collapse=" ")
        chunks <- cbind(chunks, token)
      }
    }
    return(list(chunk=as.character(chunks)))
  }
}
place_chunk_markers <- function(t, already_toks=F, chunk_length=1000, markers=c('<span style="background-color: ', 
                                                                '</span>'), marker_col = '#FFFF00') {
  if(is.null(t)) return(NULL)
  else {
    if (already_toks == F) {
      if (length(marker_col) == 1) {
        markers[1] <- paste0(markers[1], marker_col, '>')
        toks__ <- tokens(t,remove_numbers=T, 
                         remove_punct = T)
        ress <- tokens_chunk(toks__, size=chunk_length)
        dd <- docvars(ress, "_docid")
        ss <- docvars(ress, "_segid")
        # cat("\n die og chunks \n", str(ress), "\n")
        # cat("\n die ids van die og chunks \n", str(ss), "\n")
        res_conc <- lapply(1:length(ress), function(.) {
          op <- markers[1]#paste0(markers[1], ss[.], ">")
          clsd <- markers[2]#paste0(markers[2], ss[.], ">")
          return(c(op, ress[[.]], clsd))
        })
        names(res_conc) <- names(ress)
        # cat("\n fok \n", str(res_conc), "\n")
        return(list(marked=res_conc, ids=dd, sid = ss))
      }
      else if (length(marker_col) > 1) {
        toks__ <- tokens(t
                         , remove_numbers=T, 
                         remove_punct = T
                         )
        ress <- tokens_chunk(toks__, size=chunk_length)
        dd <- docvars(ress, "_docid")
        ss <- docvars(ress, "_segid")
        # cat("\n die og chunks \n", str(ress), "\n")
        # cat("\n die ids van die og chunks \n", str(ss), "\n")
        res_conc <- lapply(1:length(ress), function(.) {
          op <- paste0(markers[1], marker_col[.], '>')#paste0(markers[1], ss[.], ">")
          clsd <- markers[2]#paste0(markers[2], ss[.], ">")
          return(c(op, ress[[.]], clsd))
        })
        names(res_conc) <- names(ress)
        # cat("\n fok \n", str(res_conc), "\n")
        return(list(marked=res_conc, ids=dd, sid=ss))
      }
    }
    else {
      if (length(marker_col) == 1) {
        markers[1] <- paste0(markers[1], marker_col, '>')
        toks__ <- t
        # if (crp) ress <- t
        ress <- toks__#tokens_chunk(toks__, size=chunk_length)
        cat("\n in helpers \n", str(ress), "\n")
        dd <- docvars(ress, "_docid")
        ss <- docvars(ress, "_segid")
        # cat("\n die og chunks \n", str(ress), "\n")
        # cat("\n die ids van die og chunks \n", str(ss), "\n")
        res_conc <- lapply(1:length(ress), function(.) {
          op <- markers[1]#paste0(markers[1], ss[.], ">")
          clsd <- markers[2]#paste0(markers[2], ss[.], ">")
          return(c(op, ress[[.]], clsd))
        })
        names(res_conc) <- names(ress)
        # cat("\n fok \n", str(res_conc), "\n")
        return(list(marked=res_conc
                    , ids=dd,
                    sid=ss
                    ))
      }
    }
    
  }
}

close_ngrams <- function(t, thresh=0.6, gram_size=2, max_candidates=3L) {
  process <- function(s) {
    ngramss <- lapply(1:length(s), function(.) {
      return(s[.] %>% as_tibble() %>% unnest_tokens(ngram, value, token="ngrams", n=gram_size))
    })
    ngrams_annos <- rep(1:length(s), lapply(ngramss, function(.) {
      return(length(.$ngram))
    }))
    ngrams_df <- data.frame(doc_id=ngrams_annos, ngrams=unlist(ngramss), stringsAsFactors = F)
    return(ngrams_df)
  }
  ngrams <- lapply(1:length(t), function(.) {
    return(t[.] %>% as_tibble() %>% unnest_tokens(ngram, value, token="ngrams", n=gram_size))
  })
  sents <- unlist(lapply(1:length(t), function(.) {
    return(t[.] %>% as_tibble() %>% unnest_tokens(sentence, value, token="sentences"))
  }))
  ngrams_anno <- rep(1:length(t), lapply(ngrams, function(.) {
    return(length(.$ngram))
  }))
  ngram_df <- data.frame(doc_id=ngrams_anno, ngrams=unlist(ngrams), stringsAsFactors = F)
  res <- list()
  for (idx in seq_along(ngram_df$ngrams)) {
    di <- ngram_df$doc_id[idx]
    oss <- sents[-di]
    ref_corp <- process(oss)
    res[[idx]] <- GetCloseMatches(string = ngram_df$ngrams[idx], sequence_strings = ref_corp$ngrams, 
                                  n = max_candidates, cutoff = thresh)
  }
  cat("\n hier1\n", length(res), "\n hier1\n")
  r <- NA
  s <- NA
  for (ii in seq_along(res)) {
    cat("\n hier2\n",ii, "\n hier2\n")
    cat("\nwaka \n", length(res[[ii]]), "\n")
    if (length(res[[ii]]) > 0) {
      r <- rbind(r, res[[ii]])
      s <- rbind(s, T)
    }
    else {
      s <- rbind(s, F)
    }
  }
  r <- r[-1,]
  s <- s[-1,]
  cat("\n ss\n", str(r), "\n")
  return(list(query=ngram_df[s,], candidates=r))
}

FS_1 <- function(sents) {
  feats <- lapply(1:nrow(sents), function(.) {
    s1_all_chars <- unlist(strsplit(sents$sent1[.], ""))
    s2_all_chars <- unlist(strsplit(sents$sent2[.], ""))
    s1_words <- sents$sent1[.] %>% as_tibble() %>% unnest_tokens(word, value)
    s2_words <- sents$sent2[.] %>% as_tibble() %>% unnest_tokens(word, value)
    
    s1_length <- length(s1_all_chars)
    s2_length <- length(s2_all_chars)
    diff_lengths <- s1_length - s2_length
    s1_char_length <- length(s1_all_chars[s1_all_chars!=" "])
    s2_char_length <- length(s2_all_chars[s2_all_chars!=" "])
    s1_num_words <- nrow(s1_words)
    s2_num_words <- nrow(s2_words)
    num_shared_words <- sum(!is.na(match(s1_words$word, s2_words$word)))
    return(c(s1_length, s2_length, diff_lengths, s1_char_length, s2_char_length, 
             s1_num_words, s2_num_words, num_shared_words))
  })
  FS_1_df <- as.data.frame(do.call(rbind, feats))
  names(FS_1_df) <- c("s1_length", "s2_length", "diff_lengths", "s1_char_length", "s2_char_length", 
                      "s1_num_words", "s2_num_words", "num_shared_words")
  return(FS_1_df)
}

FS_2 <- function(sents) {
  feats <- lapply(1:nrow(sents), function(.) {
    init <- FuzzMatcher$new()
    qratio <- init$QRATIO(string1 = sents$sent1[.], 
                          string2 = sents$sent2[.], 
                          force_ascii = T)
    wratio <- init$WRATIO(string1 = sents$sent1[.], 
                          string2 = sents$sent2[.], 
                          force_ascii = T)
    partial_ratio <- init$Partial_ratio(string1 = sents$sent1[.], 
                                        string2 = sents$sent2[.])
    partial_token_set_ratio <- init$Partial_token_set_ratio(string1 = sents$sent1[.], 
                                                            string2 = sents$sent2[.],
                                                            force_ascii = T,
                                                            full_process = T)
    partial_token_sort_ratio <- init$Partial_token_sort_ratio(string1 = sents$sent1[.], 
                                                              string2 = sents$sent2[.],
                                                              force_ascii = T,
                                                              full_process = T)
    token_set_ratio <- init$Token_set_ratio(string1 = sents$sent1[.], 
                                            string2 = sents$sent2[.],
                                            force_ascii = T,
                                            full_process = T)
    token_sort_ratio <- init$Token_sort_ratio(string1 = sents$sent1[.], 
                                              string2 = sents$sent2[.],
                                              force_ascii = T,
                                              full_process = T)
    return(c(qratio, wratio, partial_ratio, partial_token_set_ratio, partial_token_sort_ratio,
             token_set_ratio, token_sort_ratio))
  })
  FS_2_df <- as.data.frame(do.call(rbind, feats))
  names(FS_2_df) <- c("qratio", "wratio", "partial_ratio", "partial_token_set_ratio", "partial_token_sort_ratio", 
                      "token_set_ratio", "token_sort_ratio")
  return(FS_2_df)
}

FS_3 <- function(sents) {
  feats <- lapply(1:nrow(sents), function(.) {
    context_size <- 8
    corp <- corpus(c(sents$sent1[.], sents$sent2[.]))
    toks_ <- tokens(corp)
    cooc <- fcm(toks_, context = "window", count = "weighted", weights = 1 / (1:context_size), tri = TRUE, window=context_size)
    
    glove <- GlobalVectors$new(word_vectors_size = 50, vocabulary = featnames(cooc), x_max = 10)
    main <- fit_transform(cooc, glove, n_iter = 20)
    
    context <- glove$components
    vectors <- main + t(context)
    vector_dfm <- as.dfm(vectors)
    rwmd_model = RWMD$new(vectors)
    rwmd_dist = dist2(dtm[1:100, ], dtm[1:10, ], method = rwmd_model, norm = 'none')
    head(rwmd_dist)
    
    cos_sim <-  textstat_simil(vector_dfm, margin = "documents", method = "cosine")
  })
}

parse.text <- function(txt, tok=NULL, ...) {
  res <- tibble(line = seq(1, length(txt), by=1), text = txt)
  if (is.null(tok)) {
    return(res)
  }
  else {
    if (tok %in% c("ngrams", "character_shingles")) {
      res <- res %>% unnest_tokens(element, text, n=q, token=tok)
    }
    else {
      res <- res %>% unnest_tokens(element, text, token=tok)
    }
    return(res)
  }
}

most.freq_words <- function(t, num=100, inc.freq=F) {
  if (is.null(t)) {return(NULL)}
  else {
    te <- tibble(line=1:length(t), text =t)
    txt <- unnest_tokens(tbl = te, output = word, input = text, token = "words")
    fr <- freq.words(txt$word)
    f <- fr$freq
    w <- fr$word
    idx <- order(f, decreasing = T)
    if (inc.freq) {res <- list(freq=(f[idx])[1:num], word=(w[idx])[1:num])}
    else {res <- list(word=(w[idx])[1:num])}
    return(res)
  }
}

freq.words <- function(t) {
  if (is.null(t)) {return(NULL)}
  else {
    words <- t
    unique_words <- unique(words)
    freqs <- rep(0, length(unique_words))
    for (uw in 1:length(unique_words)) {
      for (w in 1:length(words)) {
        if (words[w] == unique_words[uw]) {
          freqs[uw] <- freqs[uw] + 1
        }
      }
    }
    res <- list()
    res$word <- unique_words
    res$freq <- freqs
    return(res)
  }
}