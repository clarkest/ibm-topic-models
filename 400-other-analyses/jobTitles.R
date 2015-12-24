
# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
# wd <-  "/media/sf_ibm-topic-model"
wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")
model.name <- "anchor_ngram"
n.topics <- 30
model.num <- 9

# load the persisted documents -- these are needed before we can load a model from state
file.name <- paste0(paste("models_dir", model.name, sep="/"), "-docs.Rdata")
# this shoudl create an object called "documents"
load(file.name)

jobs <- summarize(group_by(documents, user), job.title=max(job))[,"job.title"]
job.words <- data.frame(words=unlist(strsplit(jobs$job.title," ")))
job.words$words <- gsub("[[:punct:]]", "", job.words$words)
job.words <- filter(job.words, !(words %in% c("")))
job.words$words <- factor(tolower(job.words$words))
job.word.counts <- data.frame(table(job.words))
job.word.counts[order(job.word.counts$Freq, decreasing=T),][1:500,]

# make sure to hold on to the user name -- it's the only way back
jobs <- summarize(group_by(documents, user), job.title=max(job))
# clean up the titles
job.titles <- jobs$job.title
CleanTitles <- function(job.titles) {
  job.titles <- tolower(job.titles)
  job.titles <- gsub("h\\.r\\.", "hr", job.titles)
  job.titles <- gsub("a\\/r", "ar", job.titles)
  job.titles <- gsub("i\\.t\\.", "it", job.titles)
  job.titles <- gsub("i\\/t", "it", job.titles)
  job.titles <- gsub("\\be-", "e", job.titles)
  job.titles <- gsub("\\bz\\/", "z", job.titles)
  job.titles <- gsub("m\\/a", "mergers and acquisitions", job.titles)
  job.titles <- gsub("m\\&a", "mergers and acquisitions", job.titles)
  job.titles <- gsub("\\bmgr\\b", "manager", job.titles)
  job.titles <- gsub("[,\\.\\/]", " ", job.titles)
  job.titles <- gsub("\\bsr\\b", "senior", job.titles)
  
  job.titles <- gsub("[[:punct:]]", "", job.titles)
  job.titles <- gsub("\\s\\s+", " ", job.titles)
}  
#job.titles <- job.titles[!(job.titles %in% c(""))]
#View(job.titles)
jobs$job.title <- CleanTitles(job.titles)


#####################
#  co-occurence     #
#####################

GetCoTokens <- function(doc) {
  words <- unique(strsplit(doc, split=" ")[[1]])
  pairs <- expand.grid(words, words, stringsAsFactors=F)
  pairs <- pairs[pairs$Var1 < pairs$Var2,]
  return (paste(pairs$Var1, pairs$Var2, sep=":"))
}

job.word.pairs <- unlist(sapply(jobs$job.title, GetCoTokens))

# we need the counts for pairs
word.pair.counts <- data.frame(table(job.word.pairs), stringsAsFactors=F)

# we also need word counts, weighed by the number of unique words in the title
# but, it'd be easier to just count the number of times the word is in a pair
f <- head(word.pair.counts)  
a <- strsplit(sapply(word.pair.counts$job.word.pairs, toString), split=":")
word.freq <- data.frame(table(unlist(a)))
word.freq.lookup <- split(word.freq$Freq, word.freq$Var1)

# raw counts of words
job.words <- data.frame(words=unlist(strsplit(jobs$job.title," ")))
job.word.counts <- data.frame(table(job.words))
raw.freq.lookup <- split(job.word.counts$Freq, job.word.counts$job.words)
lookup.freq <- function(word) { 
  return(as.numeric(raw.freq.lookup[toString(word)]))
}

# function to calc the mutual information of a given word pair
# I(w1, w2) = log(p(w1,w2) / p(w1)p(w2))
n.titles <- nrow(jobs)
library(stringr)
word.pair.counts <- cbind(word.pair.counts, 
                          str_split_fixed(word.pair.counts$job.word.pairs, ":", 2))
word.pair.counts <- word.pair.counts[word.pair.counts$Freq>=10,]
word.pair.counts$freq.1 <- sapply(word.pair.counts[,"1"], lookup.freq)
word.pair.counts$freq.2 <- sapply(word.pair.counts[,"2"], lookup.freq) 

p.w1.w2 <- word.pair.counts$Freq / n.titles
p.w1 <- word.pair.counts$freq.1 / n.titles
p.w2 <- word.pair.counts$freq.2 / n.titles
word.pair.counts$mut.info <- log(p.w1.w2 / (p.w1 * p.w2))

word.pair.counts[order(word.pair.counts$mut.info, decreasing=T),][1:500,c(1,2,5,6,7)]

##############
# bi grams   #
##############

GetBiGrams <- function(doc) {
  words <- strsplit(doc, split=" ")[[1]]
  pairs <- paste(head(words, -1), words[-1], sep=":")
  return (pairs)
}
bigrams <- unlist(sapply(jobs$job.title, GetBiGrams))
bigram.counts <- data.frame(table(bigrams), stringsAsFactors=F)
n.titles <- nrow(jobs)
bigram.counts <- cbind(bigram.counts, 
                          str_split_fixed(bigram.counts$bigrams, ":", 2))
bigram.counts <- bigram.counts[bigram.counts$Freq>=10,]
bigram.counts$freq.1 <- sapply(bigram.counts[,"1"], lookup.freq)
bigram.counts$freq.2 <- sapply(bigram.counts[,"2"], lookup.freq) 
p.w1 <- bigram.counts$freq.1 / n.titles

p.w1.w2 <- bigram.counts$Freq / n.titles
p.w2 <- bigram.counts$freq.2 / n.titles
bigram.counts$mut.info <- log(p.w1.w2 / (p.w1 * p.w2))

bigram.counts[order(bigram.counts$mut.info, decreasing=T),][1:500,c(1,2,5,6,7)]

View(bigram.counts)


###########################
# mapping titles to roles #
###########################

# grab the spreadsheet 
title.maps <- read.csv("place_docs_here/Words in Job Titles - words.csv", header=T)
# fix a typo in the original
title.maps[title.maps$occupation=="other?","occupation"] <- ""
title.maps$occupation <- factor(title.maps$occupation)

manager <- list()
manager[[nrow(jobs) + 1]] <- ""
internal <- list()
internal[[nrow(jobs) + 1]] <- ""
func <- list()
func[[nrow(jobs) + 1]] <- ""
# iterate over job.titles
for (i in 1:nrow(jobs)) {
  for (word in strsplit(as.character(jobs[i,"job.title"]), " ")[[1]]) {
    lookup.idx <- match(word, title.maps$word)
    if (!is.na(lookup.idx)) {
      if (title.maps[lookup.idx, "manager_level"] != "") {
        manager[[i]] <- c(manager[[i]], 
                          as.character(title.maps[lookup.idx, "manager_level"]))
      }
      if (title.maps[lookup.idx, "client_facing"] != "") {
        internal[[i]] <- c(internal[[i]], 
                           as.character(title.maps[lookup.idx, "client_facing"]))
      }
      if (title.maps[lookup.idx, "occupation"] != "") {
        func[[i]] <- c(func[[i]], as.character(title.maps[lookup.idx, "occupation"]))
      }
    }
  }
  # special rules / overrides
  # iterate through the words again
  word.list <- strsplit(as.character(jobs[i,"job.title"]), " ")[[1]]
  if (length(word.list) > 1) {
    for (w in (length(word.list)-1)) {
      # "general manger" -- exec
      if (word.list[w]=="general" & word.list[w+1]=="manager") {
        manager[[i]] <- c("exec") 
      }
      # "process director" -- exec
      # "process manager" -- non
      if (word.list[w]=="process"){
        if (word.list[w+1]=="director") {
          manager[[i]] <- c("exec") 
        }
        if (word.list[w+1]=="manager") {
          manager[[i]] <- c("non") 
        }
      }
      # "program manager" -- non
      # "program executive" -- non
      if (word.list[w]=="program" & 
        (word.list[w+1]=="manager" | word.list[w+1]=="executive")) {
          manager[[i]] <- c("non") 
      }
      # "distinguished engineer" -- manager
      if (word.list[w]=="distinguished" & word.list[w+1]=="engineer") {
        manager[[i]] <- c("manager") 
      }
      # "project manager" -- non
      # "project executive" -- non
      if (word.list[w]=="project" & 
          (word.list[w+1]=="manager" | word.list[w+1]=="executive")) {
        manager[[i]] <- c("non") 
      }
    }
  }
}






mng.tries <- 0
mng.conflict <- 0
mng.votes <- 0
mng.max <- 0
int.tries <- 0
int.conflict <- 0
inter.votes <- 0
int.max <- 0
func.tries <- 0
func.conflict <- 0
func.votes <- 0
func.max <- 0
for (i in 1:nrow(jobs)) {
  if (!is.null(manager[[i]])) {
    mng.tries <- mng.tries + 1
    #mgr <- distinct(data.frame(manager[[i]]))
    mgr <- summarize(group_by(data.frame(a=manager[[i]]), a), n=n())
    if (nrow(mgr) > 1){
      mng.conflict <- mng.conflict + 1
      if (max(mgr$n) > 1) { mng.votes <- mng.votes + 1}
      if (nrow(mgr) > mng.max) {mng.max <- nrow(mgr)}
    }
  }
  if (!is.null(internal[[i]])) {
    int.tries <- int.tries + 1
    #inter <- distinct(data.frame(internal[[i]]))
    inter <- summarize(group_by(data.frame(a=internal[[i]]), a), n=n())
    if (nrow(inter) > 1){
      int.conflict <- int.conflict + 1
      if (max(inter$n) > 1) { inter.votes <- inter.votes + 1}
      if (nrow(inter) > int.max) {int.max <- nrow(inter)}
    }
  }
  if (!is.null(func[[i]])) {
    func.tries <- func.tries + 1
    #funcer <- distinct(data.frame(func[[i]]))
    funcer <- summarize(group_by(data.frame(a=func[[i]]), a), n=n())
    if (nrow(funcer) > 1){
      func.conflict <- func.conflict + 1
      if (max(funcer$n) > 1) { func.votes <- func.votes + 1}
      if (nrow(funcer) > func.max) {func.max <- nrow(funcer)}
    }
  }
}
  
# out of 15,485 titles, we could identify for:
# managers: 7766, 498 of which we have more than one different type identified
# interna/external: 5686, 412 of which we have more than one different type identified
# functional role: 10990, 2691 of which we have more than one different type identified


# we need to output these so that Charles & co. can scan the 
# conflicts and non-identified by hand
n <- nrow(jobs)
empt.num <- rep(0, n)
empt <- rep("", n)
out.frame <- data.frame(user=jobs$user, title=jobs$job.title, 
                        manager.flags=empt.num, manager=empt,
                        int.ext.flags=empt.num, int.ext=empt,
                        functions.flags=empt.num, functions=empt,
                        stringsAsFactors = F
                        )
for (i in 1:n) {
  this.row <- c()
  for (set in list(manager, internal, func)) {
    if (is.null(set[[i]])){
      this.row <- c(this.row, 0, "")
    } else {
      labels <- summarize(group_by(data.frame(a=set[[i]]), a), n=n())
      this.row <- c(this.row, nrow(labels))
      labels.str <- transmute(labels, this=paste0(a,":",n))  
      out.str <- paste(labels.str$this, collapse=",")
      this.row <- c(this.row, out.str)
    }
  }
  out.frame[i,3:8] <- this.row
}    

write.csv(out.frame, file="place_docs_here/flagged title categories.csv")
