source("300-post-model-analyses/mallet_analyses.R")
# load the persisted documents -- these are needed before we can load a model from state
file.name <- "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model/models_dir/windows-docs.Rdata"
# this shoudl create an object called "documents"
load(file.name)
stop.word.file <- "200-topic-models/en.txt"
wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model"
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
topic.model$train(80)
topic.model$maximize(10)


percents <- seq(0.05,0.5, by=0.05)
l1dists <- c()
for (perc.small in percents) {
  n <- 20
  tot <- 0
  print (perc.small)
  for (i in 1:n) { 
    documents$rand <- "large"
    documents[sample(1:nrow(documents), nrow(documents)*perc.small, replace=F), "rand"] = "small"
    tot <- tot + l1.distances("rand", documents, topic.model)[[1]]
  }
  print(tot/n)
  l1dists <- c(l1dists, tot / n)
}
plot(percents, l1dists)


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
