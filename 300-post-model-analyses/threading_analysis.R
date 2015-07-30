source("300-post-model-analyses/mallet_analyses.R")

n.topics <- 30
# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
model.name <- "windows"
iters <- 800
maxims <- 50
model.num <- 1

list <- load.model.for.analysis(n.topics, model.name, iters, maxims, model.num) 
topic.model <- list$topic.model
documents <- list$documents
doc.topics <- list$doc.topics
doc.topics.frame <- list$doc.topics.frame
model.label <- list$model.label

#################
#   THREADING   #
#################

# the ancestry was calculated in SQL, using the non-processed data for cases when culled docs
# were "missing links"

# load the ancestry information from the sql dump
ancestry <- read.delim("place_docs_here/thread_ancestry.csv", encoding="UTF-8", sep="\t", quote='', stringsAsFactors=FALSE)

# map to documents by id 
##  -- note that the document ids have been altered because of the duplicate ids
new.documents <- merge(documents, ancestry, by.x="id", by.y="id")


