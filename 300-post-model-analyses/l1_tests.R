source("300-post-model-analyses/mallet_analyses.R")
# load the persisted documents -- these are needed before we can load a model from state
file.name <- "/users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/models_dir/windows-docs.Rdata"
# this shoudl create an object called "documents"
load(file.name)
stop.word.file <- "200-topic-models/en.txt"
wd <-  "/users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
mallet.instances <- mallet.import(documents$id, 
                                  documents$text, 
                                  stop.word.file, 
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]"
)

## Initialize from a previously trained state
topic.model <- MalletLDA(num.topics=1)
topic.model$loadDocuments(mallet.instances)
topic.model$setNumThreads(5L)
topic.model$setAlphaOptimization(20, 50)
topic.model$train(20)
topic.model$maximize(10)

jensenShannon <- function(x, y) {
  m <- 0.5*(x + y)
  0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
}
dist.mat <- proxy::dist(x = phi, method = jensenShannon)

percents <- seq(0.05,0.5, by=0.1)
l1dists <- c()
jsdists <- c()
for (perc.small in percents) {
  n <- 10
  l1.tot <- 0
  js.tot <- 0
  print (perc.small)
  for (i in 1:n) { 
    documents$rand <- "large"
    documents[sample(1:nrow(documents), nrow(documents)*perc.small, replace=F), "rand"] = "small"
    l1.tot <- l1.tot + l1.distances("rand", documents, topic.model)[[1]]
    
    words.by.factor <- list()
    words.by.factor[["large"]] <- mallet.subset.topic.words(topic.model, 
                                                        documents[,"rand"]=="large", 
                                                        normalized=T, smoothed=T)
    words.by.factor[["small"]] <- mallet.subset.topic.words(topic.model, 
                                                            documents[,"rand"]=="small", 
                                                            normalized=T, smoothed=T)
    
    js.tot <- js.tot + jensenShannon(words.by.factor[["large"]], words.by.factor[["small"]])
  }
  print(l1.tot/n)
  l1dists <- c(l1dists, l1.tot / n)
  print(js.tot/n)
  jsdists <- c(jsdists, js.tot / n)
}
plot(percents, l1dists)

plot(percents, jsdists)


sample.frac <- seq(0.1,1, by=0.1)
l1.eq.size <- c()
for (perc.small in sample.frac) {
  n <- 20
  tot <- 0
  print (perc.small)
  for (i in 1:n) { 
    documents$rand <- "first"
    documents[sample(1:nrow(documents), nrow(documents)*0.5, replace=F), "rand"] = "second"
    documents$sample <- "no.use"
    documents[sample(1:nrow(documents), nrow(documents)*perc.small, replace=F), "sample"] = "use"
    tot <- tot + l1.distances("rand", documents, topic.model, documents$sample=="use")[[1]]
  }
  print(tot/n)
  l1.eq.size <- c(l1.eq.size, tot / n)
}
plot(sample.frac, l1.eq.size)





# What if we try Mahalanobis distance?


percents <- seq(0.05,0.5, by=0.1)
mdists <- c()
for (perc.small in percents) {

  # we want to normalize by the stdev of each word under this sampling, 
  # so first we need to bootstrap token-level means and sd:
  n <- 30
  boots <- NULL
  lrg.boots <- NULL
  for (i in 1:n) {
    documents$rand <- "large"
    documents[sample(1:nrow(documents), nrow(documents)*perc.small, replace=F), "rand"] = "small"
    boot.set <- mallet.subset.topic.words(topic.model, 
                                          documents[,"rand"]=="small", 
                                          normalized=T, smoothed=T)
    lrg.boot.set <- mallet.subset.topic.words(topic.model, 
                                          documents[,"rand"]=="large", 
                                          normalized=T, smoothed=T)
    boots <- rbind(boots, boot.set)
    lrg.boots <- rbind(lrg.boots, lrg.boot.set)
  }
  small.word.means <- apply(boots, 2, mean)
  small.word.sd <- apply(boots, 2, sd) 
  lrg.word.means <- apply(lrg.boots, 2, mean)
  lrg.word.sd <- apply(lrg.boots, 2, sd)
  
  iter <- 10
  m.tot <- 0
  print (perc.small)
  for (i in 1:iter) { 
    documents$rand <- "large"
    documents[sample(1:nrow(documents), nrow(documents)*perc.small, replace=F), "rand"] = "small"
    
    words.by.factor <- list()
    words.by.factor[["large"]] <- mallet.subset.topic.words(topic.model, 
                                                            documents[,"rand"]=="large", 
                                                            normalized=T, smoothed=T)
    words.by.factor[["small"]] <- mallet.subset.topic.words(topic.model, 
                                                            documents[,"rand"]=="small", 
                                                            normalized=T, smoothed=T)
    
    m.tot <- m.tot + 
      sqrt(sum(
            ((words.by.factor[["large"]] - lrg.word.means) / lrg.word.sd - 
              (words.by.factor[["small"]] - small.word.means) / small.word.sd   
            )^2
          ))
  }
  print(m.tot / iter)
  mdists <- c(mdists, m.tot / iter)
  
}
plot(percents, mdists)
