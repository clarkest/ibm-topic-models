
wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")

model.name <- "anchor_ngram_model"
n.topics <- 30
iters <- 800
maxims <- 100
model.num <- 1

model.object <- load.model.for.analysis(n.topics, model.name, iters, maxims, model.num) 
tm <- model.object$topic.model
out.dir <- "/Users/clarkbernier/Dropbox/IBM Local/topic_browser/ibm/"
in.state <- paste0("/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/models_dir/",model.object$model.label, ".gz") 
words.per.topic <- 100

# topic, word, score
# 0,arts,0.132423295699499
# order by topic, score
vocab <- mallet.word.freqs(model.object$topic.model)
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


# "doc","year","source","docword","topic"
# 1,1986,"HOU","briefs",10
# order by doc, token order within doc  ## VERY IMPORTANT ##

# we need the gibbs state from this model
state <- read.table(gzfile(in.state), 
                    header=F, 
                    skip=3, 
                    sep=" ", 
                    stringsAsFactors=FALSE, 
                    comment.char="",
                    quote="")
names(state) <- c("doc","source","pos","typeindex","type","topic")
state.2 <- cbind(state[,c("doc")], documents[state$doc+1, c("jam", "manager")], state[,c("type","topic")])
names(state.2) <- c("doc","year","source","docword","topic")
state.2$doc <- as.numeric(state.2$doc) + 1
write.table(state.2, paste0(out.dir,"doc_token_topic.csv"), sep=",", quote=FALSE, row.names=FALSE, fileEncoding="UTF-8")

# sub-corpus vocabularies
# Topic: 0
# 1986,1987,1988,1989,1990,1991,1992,1993,1994,1995,1996,1997,
# word[1][year1], word[1][year2], ...
# word[2][year1], word[2][year2], ...
# "arts [0.18578508431641]", "arts [0.18578508431641]", etc.

factor <- "jam"
template <- "\nTopic: %d
%s 
%s"
factor.lvls <- levels(factor(documents[, factor]))
words.by.factor <- NULL
for (lvl in factor.lvls) { 
  # we use normalizations so the rows sum to 1, and smoothing so nothing is exactly zero
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
a <- cbind(1:nrow(a), a, documents$manager, documents$jam)
names(a) <- c("doc", 1:n.topics, "manager", "jam")
b <- melt(a, id.vars=c("doc", "manager", "jam"))
b <- b[b$value>0,]
names(b) <- c("doc", "source", "year", "topic", "docprop")
b <- b[c("doc","topic","docprop","source","year")]
b <- b[order(b$doc, b$topic),]
b$topic <- as.numeric(b$topic) - 1

write.table(b, paste0(out.dir,"doc_topic_props.csv"), sep=",",  quote=FALSE,row.names=FALSE, fileEncoding="UTF-8")


# rawtext
template <- "=== RECORD %d === \n  
Jam: %s
Manager: %s 
Forum: %s
Thread: %s \n
Corpus Text: \n%s
=== END RECORD ==="
a <- sprintf(template,
             1:nrow(documents),
             documents$jam, 
             documents$manager, 
             documents$forum, 
             documents$DateWindow, 
             documents$text
)
write(a, paste0(out.dir,"text.txt"))
