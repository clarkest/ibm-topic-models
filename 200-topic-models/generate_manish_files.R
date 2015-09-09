
wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")

AddMultiLevelFactor <- function(docs, factor.list) {
  if (length(factor.list) > 1) {
    multi.label <- paste(factor.list, collapse="--")
    docs[multi.label] <- do.call(paste, c(docs[factor.list], sep = "-"))
  } else {
    multi.label <- factor.list[1]
  }
  return(list(label=multi.label, docs=docs))
}

GetDocsWithDocLen <- function(model.object) {
  unnormal.doc.topics <- mallet.doc.topics(model.object$topic.model, smoothed=F, normalized=F)
  ret.val <- model.object$documents
  ret.val$doc.length <- rowSums(unnormal.doc.topics)
  return (ret.val)
}

PrepareBrowserDocs <- function(model.num, factor.list=c("jam", "forum"), 
                               other.data=c("manager", "gender", "doc.length", "title")) {
  model.object <- load.model.for.analysis(n.topics, model.name, model.num) 
  docs <- GetDocsWithDocLen(model.object)
  labeled.docs <- AddMultiLevelFactor(docs, factor.list)
  documents <- labeled.docs$docs
  factor <- labeled.docs$label
  tm <- model.object$topic.model
  out.dir <- paste0("/Users/clarkbernier/sandbox/ibm_docs/ibm_", model.num, "/")
  dir.create(out.dir, showWarnings=FALSE)
  in.state <- paste0("/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/models_dir/",
                     model.object$model.label, ".gz") 
  words.per.topic <- 100
  vocab <- mallet.word.freqs(model.object$topic.model)
  
  # topic, word, score
  # 0,arts,0.132423295699499
  # order by topic, score
  topic.words <- mallet.topic.words(model.object$topic.model, smoothed=F, normalized=T)
  a <- data.frame(t(topic.words))
  a <- cbind(vocab$words, a)
  names(a) <- c("word",1:n.topics)
  b <- melt(a)
  b <- b[b$value>0,]
  names(b) <- c("word", "topic", "score")
  b <- b[c("topic", "word", "score")]
  out.set <- NULL
  for (i in 1:n.topics) {
    topic.set <- b[b$topic==i,]
    topic.set <- topic.set[order(-topic.set$score),]
    out.set <- rbind(out.set, topic.set[1:words.per.topic,])  
  }
  out.set$topic <- as.numeric(out.set$topic) - 1
  write.table(out.set, paste0(out.dir,"topic_word_score.csv"), sep=",", quote=FALSE, row.names=FALSE, fileEncoding="UTF-8")
  

  # "doc","docword","topic"
  # 1,"briefs",10
  # order by doc, token order within doc  ## VERY IMPORTANT ##
 # source appears to be entirely unused in this file
  
  # we need the gibbs state from this model
  state <- read.table(gzfile(in.state), 
                      header=F, 
                      skip=3, 
                      sep=" ", 
                      stringsAsFactors=FALSE, 
                      comment.char="",
                      quote="")
  names(state) <- c("doc","undefined","pos","typeindex","type","topic")
  
  state.2 <- state[,c("doc","type","topic")]
  names(state.2) <- c("doc","docword","topic")
  state.2$doc <- as.numeric(state.2$doc) + 1
  write.table(state.2, paste0(out.dir,"doc_token_topic.csv"), sep=",", quote=FALSE, row.names=FALSE, fileEncoding="UTF-8")


  # sub-corpus vocabularies
  # Topic: 0
  # 1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,
  # word[1][year1], word[1][year2], ...
  # word[2][year1], word[2][year2], ...
  # "arts [0.18578508431641]", "arts [0.18578508431641]", etc.
  
  template <- "\nTopic: %d\n%s\n%s"
  factor.lvls <- levels(factor(documents[, factor]))
  words.by.factor <- NULL
  for (lvl in factor.lvls) { 
     topic.words <- mallet.subset.topic.words(model.object$topic.model, documents[,factor]==lvl, 
                                    normalized=T, smoothed=F)
     a <- data.frame(t(topic.words))
     a <- cbind(vocab$words, a)
     names(a) <- c("word",1:n.topics)
     b <- melt(a)
     names(b) <- c("word", "topic", "score")
     b <- b[c("topic", "word", "score")]
     this.factor <- NULL
     for (i in 1:n.topics) {
        topic.set <- b[b$topic==i,]
        topic.set <- topic.set[order(-topic.set$score),]
        topic.set$string <- sprintf('"%s [%f]"', topic.set$word, topic.set$score)
        this.factor <- rbind(this.factor, topic.set[1:words.per.topic, c("topic","string")])  
     }
     words.by.factor <- cbind(words.by.factor, this.factor$string)
  }
  
  rows.for.out <- apply(words.by.factor, 1, paste, collapse=", ")
  out.strs <- c()
  for (i in 1:n.topics) {
    range <- ((i-1)*words.per.topic + 1):(i*words.per.topic)
    topic.out.str <- sprintf(template, i-1, paste(factor.lvls, collapse=","), paste(rows.for.out[range], collapse=",\n"))
    out.strs <- c(out.strs, topic.out.str)
  }
  full.text <- paste(out.strs, collapse=",\n")
  header <- paste(factor.lvls, collapse=",")
  write(paste(header, full.text, sep=",\n"), paste0(out.dir,"sub_corpus_vocab.txt"))
  
  
  # "doc","topic","docprop","source","year"
  # 1,6,0.025,"HOU",1986
  
  doc.topics <- mallet.doc.topics(model.object$topic.model, smoothed=F, normalized=T)
  a <- data.frame(doc.topics)
  a <- cbind(1:nrow(a), a, documents[,other.data], documents[,factor])
  names(a) <- c("doc", 1:n.topics, other.data, factor)
  b <- melt(a, id.vars=c("doc", other.data, factor))
  b <- b[b$value>0,]
  names(b) <- c("doc", other.data, "factor", "topic", "docprop")
  b <- b[c("doc", "topic", "docprop", "factor", other.data)]
  b <- b[order(b$doc, b$topic),]
  b$topic <- as.numeric(b$topic) - 1
  
  write.table(b, paste0(out.dir,"doc_topic_props.csv"), sep=",",  quote=FALSE,row.names=FALSE, fileEncoding="UTF-8")
  
  
  # rawtext
  template <- "=== RECORD %d === \n  
  Jam: %s
  %s 
  Forum: %s
  Corpus Text: \n%s
  === END RECORD ==="
  other.data.template <- paste0(paste0(other.data, collapse=": %s\n  "), ": %s")
  other.data.str <- do.call("sprintf", c(other.data.template, documents[,other.data]))
  
  a <- sprintf(template,
               1:nrow(documents),
               documents$jam, 
               other.data.str, 
               documents$forum,  
               documents$text
  )
  write(a, paste0(out.dir,"text.txt"))
}


model.name <- "anchor_ngram"
n.topics <- 30
model.num <- 9
PrepareBrowserDocs(model.num,
                   factor.list=c("jam", "forum"), 
                   other.data=c("manager", "gender", "doc.length", "title"))

PrepareBrowserDocs(model.num,
                   factor.list=c("jam"), 
                   other.data=c("manager", "forum", "gender", "doc.length", "title"))
