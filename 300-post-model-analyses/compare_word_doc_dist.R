doc.topics.unnormal <- data.frame(mallet.doc.topics(topic.model, smoothed=T, normalized=F))
doc.topics.unnormal.unsmooth <- data.frame(mallet.doc.topics(topic.model, smoothed=F, normalized=F))

unnormal.df <- cbind(new.docs[,c("id","title","text","manager","jam","forum")], doc.topics.unnormal)
unnormal.unsmooth.df <- cbind(new.docs[,c("id","title","text","manager","jam","forum")], doc.topics.unnormal.unsmooth)

write.table(unnormal.df, file="/Users/clarkbernier/Downloads/anchor_8_doc_topics_unnormal.tsv", sep="\t", row.names=FALSE)
write.table(unnormal.unsmooth.df, file="/Users/clarkbernier/Downloads/anchor_8_doc_topics_unnormal_unsmooth.tsv", sep="\t", row.names=FALSE)



wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")

model.name <- "anchor_ngram_model"
n.topics <- 30
iters <- 800
maxims <- 100
model.num <- 8

model.object <- load.model.for.analysis(n.topics, model.name, iters, maxims, model.num) 
corr.obj <- topic.co.occur(model.object$topic.model, cooccurenceMinTokens, cooccurenceThreshold)
topic.words <- mallet.topic.words(model.object$topic.model, smoothed=T, normalized=T)

jensenShannon <- function(x, y) {
  m <- 0.5*(x + y)
  0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
}
melt.dist <- function(dist) {
  dist <- melt(dist)
  dist <- dist[dist$X1 < dist$X2,]
  return (dist) 
}
get.dist.ranks <- function(dist) {
  dist <- melt.dist(dist)
  o.dist <- (dist[with(dist, order(value)),])
  ranks <- list()
  for(i in 1:nrow(o.dist)) {
    row <- o.dist[i,]
    ranks[[paste(row$X1, row$X2, sep="-")]] <- i
  }
  return(ranks)
}

word.dist.mat <- as.matrix(proxy::dist(x = topic.words, method = jensenShannon))

word.dist.ranks <- get.dist.ranks(word.dist.mat)
doc.dist.ranks <- get.dist.ranks(corr.obj$dist)

results <- data.frame(pair=character(), 
                      word.dist.rank=numeric(), 
                      doc.dist.rank=numeric(), 
                      diff=numeric(),
                      stringsAsFactors=FALSE
            )
for (pair in names(word.dist.ranks)) {
  diff <- word.dist.ranks[[pair]] - doc.dist.ranks[[pair]]
  res <- c(pair, word.dist.ranks[[pair]], doc.dist.ranks[[pair]], diff)
  results[nrow(results)+1, ] <- res
}
results$diff <- as.numeric(results$diff)
results[with(results, order(diff)),]
hist(results$diff )

# how about if we center the distances from both and subtract them?  this gives 
word.dist <- melt.dist(word.dist.mat)
doc.dist <- melt.dist(corr.obj$dist)
x <- word.dist$value
word.dist$norm.dist <- (x-min(x))/(max(x)-min(x))
word.dist$sd.dist <- (x-mean(x))/sd(x)
x <- doc.dist$value
doc.dist$norm.dist <- (x-min(x))/(max(x)-min(x))
doc.dist$sd.dist <- (x-mean(x))/sd(x)

combined <- merge(word.dist, doc.dist, by=c("X1","X2"), suffixes=c(".word",".doc"))
combined$diff <- combined$norm.dist.word - combined$norm.dist.doc
combined$sd.diff <- combined$sd.dist.word - combined$sd.dist.doc
hist(combined$diff)
hist(combined$sd.diff)
combined[with(combined, order(sd.diff)),]

results[results$pair=="4-15",]




# Hierarchical clustering

plot(mallet.topic.hclust(model.object$doc.topics, topic.words, 0.3), labels=1:30)
plot(mallet.topic.hclust(model.object$doc.topics, topic.words, 0), labels=1:30)
plot(mallet.topic.hclust(model.object$doc.topics, topic.words, 1.0), labels=1:30)
