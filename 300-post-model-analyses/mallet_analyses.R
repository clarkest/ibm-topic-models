library(ggplot2)
library(countrycode)
library(Hmisc) ## this has to go first because it contains conflicts with "summarize" from dplyr
library(dplyr) ## overwrite "summarize" from Hmisc
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(mallet)
library(reshape2)

get.regex <- function(regex.file, regex.name) {
  df <- read.table(regex.file, stringsAsFactors=F, sep=",", header=T, allowEscapes=T, strip.white=T)
  if (sum(df$name==regex.name) > 0) {
      return(head(df[df$name==regex.name,"regex"],1))
  } else {
    # return the first regex
    return(df[1,"regex"])
  }
}

load.from.saved.state <- function(model.name, model.num, n.topics, regex.name="curated_punctuation") {
  stop.word.file <- "200-topic-models/en.txt"
  regex.file <- "200-topic-models/regex.txt"
  
  regex <- get.regex(regex.file, regex.name)
  
  # load the persisted documents -- these are needed before we can load a model from state
  file.name <- paste0(paste("models_dir", model.name, sep="/"), "-docs.Rdata")
  # this shoudl create an object called "documents"
  load(file.name)
  mallet.instances <- mallet.import(documents$id, 
                                    documents$text, 
                                    stop.word.file, 
                                    token.regexp=regex
                                    #token.regexp = "\\p{L}[\\p{L}\\_\\-&@'`\\p{N}]+[\\p{N}\\p{L}]"
  )
  
  ## Initialize from a previously trained state
  model.label <- paste(model.name, n.topics, formatC(model.num, width=2, flag="0"), sep="-")
  file.name <- paste("models_dir", paste0(model.label, ".gz"), sep="/")
  topic.model <- MalletLDA(num.topics=n.topics)
  topic.model$loadDocuments(mallet.instances)
  topic.model$initializeFromState(.jnew("java.io.File", file.name))
  return(topic.model)
}

load.model.for.analysis <- function(n.topics, model.name, model.num, ...) {
  
  topic.model <- load.from.saved.state(model.name, model.num, n.topics, ...)
  model.label <- paste(model.name, n.topics, formatC(model.num, width=2, flag="0"), sep="-")
  file.name <- paste0(paste("models_dir", model.name, sep="/"), "-docs.Rdata")
  
  # this should create an object called "documents"
  load(file.name)
  
  ## Get the probability of topics in documents and the probability of words in topics.
  ## By default, these functions return raw word counts. Here we want probabilities, 
  ##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
  doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
  topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
  
  ## Get short labels that can be used as part of filename
  topics.labels <- gsub("\\W", "_", mallet.topic.labels(topic.model, topic.words, 3))
  #topics.long.labels <- mallet.topic.labels(topic.model, topic.words, num.top.words=50)
  
  doc.topics.frame <- data.frame(doc.topics)
  #names(doc.topics.frame) <- paste("Topic", 1:n.topics, sep="")
  names(doc.topics.frame) <- topics.labels
  
  return (list(topic.model=topic.model, documents=documents, doc.topics=doc.topics, doc.topics.frame=doc.topics.frame, model.label=model.label))
}


get.posts.by.window <- function(documents, by.vars) { 
  aggregate.set = c("DateWindow", by.vars)
  
  posts.by.window <- aggregate(documents$id, 
                               by=documents[, aggregate.set], 
                               FUN=length)
  
  if (length(by.vars) > 1) {
    posts.by.window$by.var <- do.call(paste, posts.by.window[,by.vars])
  } else {
    posts.by.window$by.var <- posts.by.window[,by.vars]
  }
  
  #drop any window with fewer than 10 posts
  posts.by.window <- posts.by.window[(posts.by.window$x>10), ]
  
  #normalize by the average by by.vars
  avg.posts = aggregate(posts.by.window$x, by=posts.by.window[,by.vars], mean)
  posts.by.window <- merge(posts.by.window, avg.posts, by=by.vars)
  posts.by.window$post.rate <- posts.by.window$x.x / posts.by.window$x.y
  return(posts.by.window)
}

##################################
# Topic Share Plotting Function  #
##################################
plot_topic_shares <- function(df, 
                              by.vars, 
                              output.dir="output/default/", 
                              topic.num,
                              topic.num.label=NULL,
                              ylim=c(0,0.2),
                              threshold.prev=NULL,
                              threshold.words=40*threshold.prev,
                              doc.len=NULL) {
  
  dir.create(output.dir, showWarnings = FALSE)
  aggregate.set <- c("DateWindow", "jam", by.vars)
  topic.name <-  colnames(df)[topic.num]
  boot.hi <- function(x) { smean.cl.boot(x)["Upper"] }
  boot.low <- function(x) { smean.cl.boot(x)["Lower"] }
  boots <- function(x) { smean.cl.boot(x) }
  if (is.null(threshold.prev)) {
    #topic.rate <- summarize(group_by_(cbind(df[topic.num], df[aggregate.set]), .dots=aggregate.set), mean)
    topic.rate <- aggregate(df[topic.num], 
                            by=df[,aggregate.set], 
                            function(x) boots(x)
    )
  } else {
    # if we have document lengths, then for smaller documents we want the threshold prev to be higher 
    # to make sure we're above the threshold.words for that document
    if (!is.null(doc.len)) {
      threshold.prev <- ifelse(threshold.prev * doc.len < threshold.words, 
                               threshold.words/doc.len, 
                               threshold.prev
                              )
    }
    topic.rate <- aggregate(df[topic.num], 
                            by=df[,aggregate.set], 
                            function(x) boots(ifelse(x>threshold.prev,1,0))
    )
  }
  if (is.null(topic.num.label)) topic.num.label <- topic.num
  
  if (length(by.vars) > 1) {
    topic.rate$by.var <- do.call(paste, topic.rate[,by.vars])
  } else {
    topic.rate$by.var <- topic.rate[,by.vars]
  }
  #colnames(avg.topic.rate)[colnames(avg.topic.rate) == topic.name] <- 'topic.data'
  plt <- qplot(as.integer(DateWindow), get(topic.name)[,"Mean"], 
               data = topic.rate, 
               geom = "line", 
               color = by.var, 
               ylab = topic.name) +
            geom_errorbar(aes(ymin=get(topic.name)[,"Lower"], 
                      ymax=get(topic.name)[,"Upper"]), 
                      width=.3) + 
            geom_point() + 
            coord_cartesian(ylim=ylim) 
  plt.title <- paste(sprintf("%02d",topic.num.label),"-",topic.name)
  ggsave(file.path(output.dir, paste(plt.title, ".png", sep="")),
         plt+thm+ggtitle(plt.title)
  )
}

plot.all.topic.shares <- function(model.object,
                                  col.keeps, 
                                  by.vars, 
                                  output.dir="outputs/default/",
                                  topic.num.labels=NULL,
                                  ...) {
  
  doc.topics.data <- cbind(model.object$doc.topics.frame, 
                           model.object$documents[col.keeps])
  # we want the DateWindows with enough data for these by variables
  temp.posts.by.window <- get.posts.by.window(model.object$documents, by.vars)
  doc.topics.data <- doc.topics.data[doc.topics.data$DateWindow %in% unique(temp.posts.by.window$DateWindow),]
  
  for (topic in 1:ncol(model.object$doc.topics.frame)) {
    if (is.null(topic.num.labels)) {
      plot_topic_shares(doc.topics.data, by.vars, output.dir, topic, ...)
    } else {
      plot_topic_shares(doc.topics.data, by.vars, output.dir, topic, topic.num.labels[topic], ...)
    }
  }
}

topic.occurence.counts <- function(topic.model,
                                   correlationMinTokens = 4, 
                                   correlationMinProportion = 0.1) {
  doc.topics <- mallet.doc.topics(topic.model.1, smoothed=T, normalized=T)
  num.docs = nrow(doc.topics)
  unnormal.doc.topics <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)
  doc.len <- rowSums(unnormal.doc.topics)
  num.topic.tokens <- doc.topics * doc.len
  topic.occur <- doc.topics > correlationMinProportion & num.topic.tokens >= correlationMinTokens
}

########################
# Topic Co-occurence 
#    adapted from David Mimno's jsLDA 
#    (https://github.com/mimno/jsLDA/blob/master/jslda.html)
########################
topic.co.occur <- function(topic.model.1,
                           correlationMinTokens = 4, 
                           correlationMinProportion = 0.1,
                           topic.model.2 = NULL,
                           doc.topics.1=NULL,
                           unnormal.doc.topics=NULL) {
  if((is.null(doc.topics.1) | is.null(unnormal.doc.topics))) {
    doc.topics.1 <- mallet.doc.topics(topic.model.1, smoothed=T, normalized=T)
    n.topics <- topic.model.1$getNumTopics()
    unnormal.doc.topics <- mallet.doc.topics(topic.model.1, smoothed=F, normalized=F)
  } else {
    n.topics <- ncol(doc.topics.1)
  }
  doc.topics.2 <- NULL
  num.docs = nrow(doc.topics.1)
  
  doc.len.1 <- rowSums(unnormal.doc.topics)
  
  if (is.null(topic.model.2)) {
    print("initializing comparison model to first model")
    doc.topics.2 <- doc.topics.1
    doc.len.2 <- doc.len.1
  } else {
    # make sure the number of docs of the two are the same. 
    # trusting the caller than same length means they are the same documents
    doc.topics.2 <- mallet.doc.topics(topic.model.2, smoothed=T, normalized=T) 
    if (num.docs != nrow(doc.topics.2) | n.topics != topic.model.2$getNumTopics()) {
      stop(sprintf("Model 1 has %d docs and % topics. Model 2 has %d and %d. Not sure how I can compare these.",
                   num.docs, n.topics,
                   nrow(doc.topics.2), topic.model.2$getNumTopics()
        )
      )
    }
    unnormal.doc.topics <- mallet.doc.topics(topic.model.2, smoothed=F, normalized=F)
    doc.len.2 <- rowSums(unnormal.doc.topics)
  }

  
  # grab the number of tokens per document
  #Count the number of docs with this topic
  num.topic.tokens.1 <- doc.topics.1 * doc.len.1
  topic.occur.1 <- doc.topics.1 > correlationMinProportion &
                    num.topic.tokens.1 >= correlationMinTokens
  topic.counts.1 <- colSums(topic.occur.1)
  num.topic.tokens.2 <- doc.topics.2 * doc.len.2
  topic.occur.2 <- doc.topics.2 > correlationMinProportion & 
                  num.topic.tokens.2 >= correlationMinTokens
  topic.counts.2 <- colSums(topic.occur.2)
  
  # iterate through each pair of topics and add to the cooccurence count
  co.occur.count <- matrix(0, n.topics, n.topics)
  corr.matrix <- matrix(0, n.topics, n.topics)
  for (topic.i in 1 : (n.topics)) {
    for (topic.j in  1 : (n.topics)) {
      # if one of hte topics is missing, we'd prefer to keep the "zero" correlation default
      if (topic.counts.1[topic.i] >0 & topic.counts.2[topic.j] > 0) {
        co.occurs <- sum(topic.occur.1[,topic.i] & topic.occur.2[,topic.j])
        co.occur.count[topic.i, topic.j] <- co.occurs
        corr.matrix[topic.i, topic.j] <- log(num.docs * (co.occurs) / (topic.counts.1[topic.i] * topic.counts.2[topic.j]))
      }
    }
  }
  dist.matrix <- proxy::dist(t(topic.occur.1), t(topic.occur.2), method="binary", upper=TRUE)[1:n.topics, 1:n.topics]
  simil.matrix <- proxy::simil(t(topic.occur.1), t(topic.occur.2), method="binary", upper=TRUE)[1:n.topics, 1:n.topics]
  return(list(co.occur.count=co.occur.count, corr.matrix=corr.matrix, dist=dist.matrix, simil=simil.matrix))
}


corr.heatmap <- function(corr.matrix, min.v=-1, max.v=1) {
  y <- melt(corr.matrix)
  # replace values outside of [-1,1] with extremes
  y$new.val <- y$value
  if (!is.null(min.v)) y[y$new.val < min.v, "new.val"] <- min.v
  if (!is.null(max.v))  y[y$new.val > max.v, "new.val"] <- max.v 
  
  p <- ggplot(y, aes(y=Var1, x=Var2)) + 
    geom_tile(aes(fill=new.val)) + 
    scale_fill_gradient2(low = "red", mid = "white",
                         high = "blue", midpoint = 0, limits=c(min.v, max.v)) + 
    xlab("") + ylab("")
  
  return(p)
}

#########################
# Stability of topic language
#########################

# Given:
#   factors: a list of factor variables in
#   docs: the documents data.frame
#   topic.model: the topic model to use
#   (optional) doc.subset: if we want just a subset of the documents, this is the vector of 
#                         TRUE/FALSE that mark which documents to include
#   (optional) lambda: determines the relevance score used to calculate distance.  
#                       =1 uses just the word prevalence
#                       relevance(term w | topic t) = λ * log(p(w | t)) + (1 - λ) * log(p(w | t)/p(w)) 
#                       see Sievert & Shirley (2014)
# Return a list, by topic, of the l1 differences (absolute value) between each pair of factor levels
vocab.diff.by.factor <- function(factor, docs, topic.model, doc.subset=NULL, lambda=1.0, use.relevance=FALSE) {
  floor.percent.docs <- 0.05
  floor.percent.words <- 0.05
  
  # TODO -- would be great if this could handle multiple factors
  words <- mallet.word.freqs(topic.model)
  # turns out the counts in this function are buggy, they include zeros -- sum directly from the topics counts
  words$term.freq <- colSums(mallet.topic.words(topic.model, normalized=F), na.rm=TRUE)
  word.freq <- words$term.freq / sum(words$term.freq)
  if (is.null(doc.subset)) {
    doc.subset <- !is.na(docs$text)
  }
  num.docs <- sum(doc.subset, na.rm=TRUE)
  factor.lvls <- levels(factor(docs[doc.subset, factor]))
  
  words.by.factor <- list()
  word.relevence.by.factor <- list()
  # get the word propensity list for each factor
  
  warnings <- list()
  for (lvl in factor.lvls) { 
    factor.lvl.ndocs <- sum(documents[,factor]==lvl & doc.subset, na.rm=TRUE)
    if (factor.lvl.ndocs < floor.percent.docs * num.docs) {
      warnings <- c(warnings, (sprintf("Warning: %s/%s for this subset has only %d documents.", factor, lvl, factor.lvl.ndocs)))
    }
    # we use normalizations so the rows sum to 1, and smoothing so nothing is exactly zero
    words.by.factor[[lvl]] <- mallet.subset.topic.words(topic.model, 
                                                        documents[,factor]==lvl & doc.subset, 
                                                        normalized=T, smoothed=T)
    if(use.relevance) {
      word.relevence.by.factor[[lvl]] <- 
        lambda * log(words.by.factor[[lvl]]) + 
        (1-lambda) * log(words.by.factor[[lvl]] / ifelse(word.freq==0, 1, word.freq)
        )
    }
  }
  
  #factor.pairs.l1 <- as.data.frame(matrix(0, ncol = 0, nrow = 30))
  factor.pairs.differences <- list()
  factor.pairs <- combn(factor.lvls, 2)
  for (i in 1:ncol(factor.pairs)) {
    lvl.1 <- factor.pairs[1,i]
    lvl.2 <- factor.pairs[2,i]
    comparison.name = paste0(lvl.1,":",lvl.2)
    if(use.relevance) {
      factor.pairs.differences[[comparison.name]] <- 
        (word.relevence.by.factor[[lvl.1]] - word.relevence.by.factor[[lvl.2]])
      #factor.pairs.l1[,comparison.name] <- 
      #0.5 * rowSums(abs(word.relevence.by.factor[[lvl.1]] - word.relevence.by.factor[[lvl.2]]))  
    } else {
      factor.pairs.differences[[comparison.name]] <- 
        (words.by.factor[[lvl.1]] - words.by.factor[[lvl.2]])
      #factor.pairs.l1[,comparison.name] <- 
      #0.5 * rowSums(abs(words.by.factor[[lvl.1]] - words.by.factor[[lvl.2]]))  
    }
  }
  for (warn in warnings) {print(warn)}
  return(factor.pairs.differences)
}


l1.distances <- function(factor, docs, topic.model, doc.subset=NULL, lambda=1.0, use.relevance=FALSE) {
  factor.pairs.differences <- vocab.diff.by.factor(factor, docs, topic.model, doc.subset, lambda, use.relevance)
  factor.pairs.l1 <- as.data.frame(matrix(0, ncol = 0, nrow = topic.model$numTopics))
  for (comp.name in names(factor.pairs.differences)) {
    factor.pairs.l1[,comp.name] <- 0.5 * rowSums(abs(factor.pairs.differences[[comp.name]]))
  }
  return(factor.pairs.l1)
}


topic.vocab.diff <- 
  function(factor, docs, topic.model, topic.id, doc.subset=NULL, lambda=1.0, use.relevance=FALSE, number.words=10) {
    factor.pairs.differences <- vocab.diff.by.factor(factor, docs, topic.model, doc.subset, lambda, use.relevance)
    words <- mallet.word.freqs(topic.model)
    words$term.freq <- colSums(mallet.topic.words(topic.model, normalized=F), na.rm=TRUE)
    word.lists <- list()
    
    for (comp.name in names(factor.pairs.differences)) {
      factors <- strsplit(comp.name, split=":")
      this.topic.words <- data.frame(words = words$words, 
                                     diff = factor.pairs.differences[[comp.name]][topic.id,],
                                     overall.freq = words$term.freq
      )
      
      factor.1.list <- head(this.topic.words[order(this.topic.words$diff, decreasing=TRUE),], number.words)
      factor.2.list <- head(this.topic.words[order(this.topic.words$diff, decreasing=FALSE),], number.words)
      word.lists[[comp.name]] <- rbind(cbind(factor=factors[[1]][1], factor.1.list), 
                                       cbind(factor=factors[[1]][2], factor.2.list)
      ) 
    }
    return(word.lists)
  }



newDateWindows <- function(documents, hours.per.window) {
  DateWindow <- 
    paste(strftime(documents$Timestamp,"%Y-%m-%d"),
          trunc(as.numeric(strftime(documents$Timestamp,"%H"))/hours.per.window)
    )
  return(factor(DateWindow))
}