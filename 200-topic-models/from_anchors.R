if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(mallet)
library(countrycode)
library(dplyr)
library("tsne")
source('200-topic-models/lda_visualize.R')

this.dir = "." #"/Users/mimno/Documents/github/ibm-topic-models"
setwd(this.dir)

run.id <- as.numeric(Sys.getenv("ARRAYID"))
initial.type <- Sys.getenv("INITIAL")
n.topics <- as.numeric(Sys.getenv("TOPICS"))
mod.label <- as.numeric(Sys.getenv("OUTLABEL"))


anchor.run <- function(model.ids, initial.type, n.topics, mod.label="") {
  if (is.na(n.topics)) {
    n.topics <- 30
  }
  iters <- 800
  maxims <- 100
  initial.state <- sprintf("anchor/docs/%s_state%s-%d.gz", initial.type, mod.label, n.topics)
  
  model.dir <- "models_dir"
  model.name <- sprintf("%s%s_ngram", initial.type, mod.label)
  file.name <- paste0(paste(model.dir, model.name, sep="/"), "-docs.Rdata")
  
  dir.create("models_dir/LDAvis")
  # fetch the persisted documents from the initialization run
  load(file.name)
  
  ##################################
  #    Mallet Topic Model Loading  #
  ##################################
  ## Create a mallet instance list object. Right now I have to specify the stoplist
  ##  as a file, I can't pass in a list from R.
  ## This function has a few hidden options (whether to lowercase, how we 
  ##   define a token). See ?mallet.import for details.
  mallet.instances <- mallet.import(documents$id, 
                                    documents$text, 
                                    "200-topic-models/en.txt", 
                                    token.regexp = "\\p{L}[\\p{L}\\_\\-&@'`\\p{N}]+[\\p{N}\\p{L}]"
  )
  #token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")
  for (i in model.ids) {
    # create and train a topic model from the mallet.instances
    new.topic.model <- MalletLDA(num.topics=n.topics)
    new.topic.model$loadDocuments(mallet.instances)
    new.topic.model$setNumThreads(8L)
    new.topic.model$initializeFromState(.jnew("java/io/File", initial.state))
    new.topic.model$setAlphaOptimization(20, 50)
    new.topic.model$train(iters)
    new.topic.model$maximize(maxims)
    model.label = paste(model.name, n.topics,formatC(i, width=2, flag="0"), sep="-")
    create.ldavis(new.topic.model, model.dir, model.label, 
                  cooccurenceThreshold=0.1, cooccurenceMinTokens=4, 
                  mds.method=function(x, k) tsne(x, k=k, perplexity=6, max_iter=2500))
    new.topic.model$printState(.jnew("java.io.File", paste(model.dir, paste0(model.label, ".gz"), sep="/")))
  }
}

anchor.run(c(run.id), "anchor", 30)
#anchor.run(c(1:10), "anchor", 30)

# perplexity sensitivity analysis

#perps <- c(5:15)
#for (p in perps) {
#  model.label = paste(model.name, n.topics, iters, maxims, formatC(p, width=2, flag="0"), sep="-")
#create.ldavis(new.topic.model, model.dir, model.label, 
#              cooccurenceThreshold=0.1, cooccurenceMinTokens=4, 
#              mds.method=function(x, k) tsne(x, k=k, perplexity=p, max_iter=2500))
#}
