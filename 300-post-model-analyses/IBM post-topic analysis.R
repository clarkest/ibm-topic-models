##install.packages("countrycode")

library(ggplot2)
library(countrycode)
library(Hmisc) ## this has to go first because it contains conflicts with "summarize" from dplyr
library(dplyr) ## overwrite "summarize" from Hmisc
if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(mallet)
library(reshape2)

n.topics <- 30
# assumes a working directory with 
#   a folder "model_states" containing .gz of topic models and .Rdata documents object
#   a folder "outputs" where this code sends the graph images

# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model/"
setwd(wd)

stop.word.file <- "200-topic-models/en.txt"

model.name <- "ngram_model"


# load the persisted documents -- these are needed before we can load a model from state
file.name <- paste0(paste("models_dir", model.name, sep="/"), "-docs.Rdata")
# this shoudl create an object called "documents"
load(file.name)
mallet.instances <- mallet.import(documents$id, 
                                  documents$text, 
                                  stop.word.file, 
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]")

## Initialize from a previously trained state
iters <- 800
maxims <- 25
model_num <- 2
model.label = paste(model.name, iters, maxims, formatC(model_num, width=2, flag="0"), sep="-")
file.name <- paste(model.dir, paste0(model.label, ".gz"), sep="/")
topic.model <- MalletLDA(num.topics=n.topics)
topic.model$loadDocuments(mallet.instances)
topic.model$initializeFromState(.jnew("java.io.File", file.name))


## Get the probability of topics in documents and the probability of words in topics.
## By default, these functions return raw word counts. Here we want probabilities, 
##  so we normalize, and add "smoothing" so that nothing has exactly 0 probability.
doc.topics <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
topic.words <- mallet.topic.words(topic.model, smoothed=T, normalized=T)
topic.words.0 <- mallet.topic.words(topic.model, smoothed=F, normalized=T)

## What are the top words in topic 7?
##  Notice that R indexes from 1, so this will be the topic that mallet called topic 6.
#mallet.top.words(topic.model, topic.words[7,])

## Get short labels that can be used as part of filename
topics.labels <- gsub("\\W", "_", mallet.topic.labels(topic.model, topic.words, 3))
topics.long.labels <- mallet.topic.labels(topic.model, topic.words, num.top.words=50)

doc.topics.frame <- data.frame(doc.topics)
#names(doc.topics.frame) <- paste("Topic", 1:n.topics, sep="")
names(doc.topics.frame) <- topics.labels

##################
# Other Metadata #
##################

# works remotely


##################################
#   Graph Formatting             #
##################################
text.1.5 <- element_text(size=rel(1.5))
text.1.0 <- element_text(size=rel(1.0))
text.2.0 <- element_text(size=rel(2.0))
thm <- theme(legend.text=text.1.5, 
             axis.title=text.1.5,
             legend.title=text.1.5,
             axis.text=text.1.5,
             plot.title=text.2.0           
)

##################################
#  1. Total Posts Over Time      #
##################################
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

#by.vars = c("manager", "continent", "jam")
by.vars = c("manager", "jam")
posts.by.window <- get.posts.by.window(documents, by.vars)

#output both the raw and the normalized plots
plt=qplot(as.integer(DateWindow), x.x, data = posts.by.window, 
          geom = "line", color=by.var) + geom_point() 
ggsave("outputs/raw_posts_by_time.png", plt + thm)

plt=qplot(as.integer(DateWindow), post.rate, data = posts.by.window, 
          geom = "line", color=by.var) + geom_point()
ggsave("outputs/indexed_posts_by_time.png", plt+thm)

##################################
# Topic Share Plotting Function  #
##################################
plot_topic_shares <- function(df, 
                              by.vars, 
                              output.dir="output/default/", 
                              topic.num,
                              topic.num.label=NULL,
                              ylim=NULL) {
  aggregate.set <- c("DateWindow", "jam", by.vars)
  topic.name <-  colnames(df)[topic.num]
  avg.topic.rate <- aggregate(df[topic.num], by=df[,aggregate.set], mean) 
  if (is.null(topic.num.label)) topic.num.label <- topic.num
  
  if (length(by.vars) > 1) {
    avg.topic.rate$by.var <- do.call(paste, avg.topic.rate[,by.vars])
  } else {
    avg.topic.rate$by.var <- avg.topic.rate[,by.vars]
  }
  #colnames(avg.topic.rate)[colnames(avg.topic.rate) == topic.name] <- 'topic.data'
  
  plt <- qplot(as.integer(DateWindow), get(topic.name), 
               data = avg.topic.rate, geom = "line", color=by.var, 
               ylab = topic.name) + geom_point() + coord_cartesian(ylim=ylim)
  plt.title <- paste(sprintf("%02s",topic.num.label),"-",topic.name)
  ggsave(paste(output.dir, plt.title, ".png", sep=""),
         plt+thm+ggtitle(plt.title)
  )
}
##################################
#  2. Topics Shares Over Time    #
##################################
# for a given document-topic prevalence data frame, generate graphs of prevalence 
# by topic over time
plot.all.topic.shares <- function(df, docs, col.keeps, 
                                  by.vars, 
                                  output.dir="outputs/default/",
                                  topic.num.labels=NULL,
                                  ylim=c(0,0.2)) {
  doc.topics.data <- cbind(df, docs[col.keeps])
  # we want the DateWindows with enough data for these by variables
  temp.posts.by.window <- get.posts.by.window(docs, by.vars)
  doc.topics.data <- doc.topics.data[doc.topics.data$DateWindow %in% unique(temp.posts.by.window$DateWindow),]
  
  for (topic in 1:ncol(df)) {
    if (is.null(topic.num.labels)) {
      plot_topic_shares(doc.topics.data, by.vars, output.dir, topic, ylim=ylim)
    } else {
      plot_topic_shares(doc.topics.data, by.vars, output.dir, topic, topic.num.labels[topic], ylim=ylim)
    }
  }
}

# run for all of the topics
# topic / forum
col.keeps <- c("forum", "continent", "jam", "manager", "DateWindow")
by.vars <- c("jam", "forum")
plot.all.topic.shares(doc.topics.frame, documents, col.keeps, by.vars, 
                      "outputs/forum_prev/", ylim=c(0,0.35))

# managers and jam
col.keeps <- c("manager", "continent", "jam", "DateWindow")
by.vars <- c("manager", "jam")
plot.all.topic.shares(doc.topics.frame, documents, col.keeps, by.vars, "outputs/prev/")


#diagnostic tool for seeing a particular topic's raw numbers
#View(avg.topic.rate <- aggregate(doc.topics.data[2], by=doc.topics.data[,aggregate.set], mean))


###############################################################
#  3. Topics Shares Over Time, Docs over a Topic threshold    #
###############################################################

######
# 3.a. histograms of topic prevalance over documents, with a threshold prevalence
######

# TODO: changed doc.topics.data to doc.topics.frame -- confirm that this is okay
threshold <- 0.03
for (topic.num in 1:1) {
  topic.name <-  colnames(doc.topics.frame)[topic.num]
  #png(paste("outputs/prev_histos/",topic.num,"-",topic.name,".png", sep=""))
  #histogram(doc.topics.data[,topic.num])
  thresh.doc.by.topic <- doc.topics.frame[doc.topics.frame[topic.num] > threshold,]
  plt <- qplot(thresh.doc.by.topic[,topic.num], binwidth=0.01, xlab = topic.name)
  plt <- ggplot(data.frame(a=thresh.doc.by.topic[,topic.num]), aes(x=a)) + xlab(topic.name) + geom_density()
  ggsave(paste("outputs/threshold_prev_histos/",topic.num,"-",topic.name,".png", sep=""), plt+thm)
}

############
#  3.b. plots limited to threshold
############

threshold <- 0.05

#col.keeps <- c("manager", "continent", "jam", "DateWindow")
by.vars <- c("manager", "jam")
#doc.topics.data <- cbind(doc.topics.frame, documents[col.keeps])
#doc.topics.data <- doc.topics.data[doc.topics.data$DateWindow %in% unique(posts.by.window$DateWindow),]

output.dir <- "outputs/topic_prev_threshold/"
for (topic in 1:n.topics) {
  thresh.doc.by.topic <- doc.topics.data[doc.topics.data[topic] > threshold,]
  plot_topic_shares(thresh.doc.by.topic, by.vars, output.dir, topic, ylim=c(0,0.3))
}



#################
# working with just the topics of focus
#################

focal.topics <-  c(6, 10, 19, 23, 27, 28, 22)
##  Notice that R indexes from 1, so these indices need incremented by 1
focal.topics.index <- focal.topics + 1

# create a data frame with just the focal topics, with prevalence rebalanced to be proportion of focal topics
focal.doc.topics <- doc.topics.frame[,focal.topics.index] / rowSums(doc.topics.frame[,focal.topics.index])
plot.all.topic.shares(focal.doc.topics, documents, col.keeps, by.vars, 
                      posts.by.window, "outputs/focal_prev/", focal.topics, ylim=c(0,0.3))


mallet.top.words(topic.model, topic.words[20,], 20)


#############
# histo of document lengths
#############

words.per.doc <- sapply(gregexpr("[A-z]\\W+", documents$text), length) + 1L
documents$words <- words.per.doc

plt <- qplot(words.per.doc, binwidth=5, xlab = "number of words")
ggsave("outputs/words_per_doc.png", plt+thm)

# by jam
ggplot(documents, aes(x=words, color=jam)) + geom_density()
# --and logged to show that it's approx log-normal
qplot(log(words.per.doc), binwidth=0.1, xlab = "log(number of words)")

# poking around some of the shorter docs
documents[words.per.doc < 10,'text']
documents[documents$text==" demonstrate personal responsibility Exactly ...thank you ", ]
  # see if its parent offers any clues
documents[documents$id=="<ffdc6a81b3.f2081933.WORLDJAM@d25was503.mkm.can.ibm.com>", ]
  # or its siblings
documents[documents$parent=="<ffdc6a81b3.f2081933.WORLDJAM@d25was503.mkm.can.ibm.com>", ]
# it still looks weird....

################################
# histo of docs per user
################################

plt <- qplot(words.per.doc, binwidth=5, xlab = "number of words")
ggsave("outputs/words_per_doc.png", plt+thm)

#################
#   THREADING   #
#################

# first check out some docs with no responses
head(documents[documents$parent=='null',])

# are there repeat ids?!?!?
repeat.ids <- documents %>% group_by(id) %>% summarise(count=n())  
repeat.ids <- repeat.ids[repeat.ids$count>1,]
documents[documents$id %in% repeat.ids$id,]
# they all have different text, so are actually distinct.  The 1x2 has the same parent, 
#   and there are two 1x4 within the 1x8 that have the same parents as one another
documents[documents$parent %in% repeat.ids$id,]
# there are only three children of these 10 repeats.  One of them is in the wrong forum?  Will probably need to 
#    delete that one.  Figuring out which of the parents to tie the others to will require looking at the text?

# text of their parents
documents[documents$id =="<ffd664eb8f.2d00a6be.WORLDJAM@d25was503.mkm.can.ibm.com>",]
documents[documents$id =="<ffe03a1a56.8914988a.WORLDJAM@d25was504.mkm.can.ibm.com>",]
  #and it's parent, too
  documents[documents$id =="<ffd7497c22.9326a6f9.WORLDJAM@d25was503.mkm.can.ibm.com>",]
documents[documents$id =="<ffe038da61.62f53160.WORLDJAM@d25was504.mkm.can.ibm.com>",]


# weird -- some of the repeats have children that are in DIFFERENT fora. how ofter is this the case?
forum.matching <- inner_join(documents[,c("forum","id")], documents[,c("forum","parent")], by=c("id"="parent"))
forum.matching[forum.matching$forum.x != forum.matching$forum.y,]
#   it looks like it is just the two identified above

# we have multi-level nesting, but would like to group all docs that appeared in the same thread together
# I'm going to call the orginating doc of such a thread the "common.ancestor." the originating doc will 
# have a common.ancestor equal to its own id

# step 1: all parent='null' set to the comment id
documents$common.ancestor <- ifelse(documents$parent=='null', documents$id, 'null')
View(documents[,c("id","parent","common.ancestor")])

# step 2: iterate through each nested layer: set the common.ansestor equal to the common ancestor of the parent 
#         of each comment.  this will populate the next level down in the trees during each pass
sum(documents$common.ancestor=='null')
while (sum(documents$common.ancestor=='null') > 0) {
  documents$parent.ancestor <- left_join(documents[,c("id","parent")], documents[,c("id","common.ancestor")], by=c("parent"="id"))
  documents$common.ancestor <- ifelse(documents$common.ancestor=='null', )
}



########################
# BETTER Topic Co-occurence 
#    adapted from David Mimno's jsLDA 
#    (https://github.com/mimno/jsLDA/blob/master/jslda.html)
########################
correlationMinTokens = 4
correlationMinProportion = 0.1

num.docs = nrow(doc.topics)
unnormal.doc.topics <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)
doc.len <- rowSums(unnormal.doc.topics)

# topic occurence binary matrix
topic.occur <- doc.topics > correlationMinProportion

# grab the number of tokens per document
#Count the number of docs with this topic
num.topic.tokens <- doc.topics * doc.len
topic.counts <- colSums(doc.topics > correlationMinProportion & num.topic.tokens >= correlationMinTokens)

# iterate through each pair of topics and add to the cooccurence count
co.occur.count <- matrix(0, n.topics, n.topics)
corr.matrix <- matrix(0, n.topics, n.topics)
for (topic.i in 1 : (n.topics-1)) {
  for (topic.j in (topic.i+1) : n.topics) {
    co.occurs <- sum(topic.occur[,topic.i] & topic.occur[,topic.j])
    co.occur.count[topic.i, topic.j] <- co.occurs
    corr.matrix[topic.i, topic.j] <- log(num.docs * co.occurs / (topic.counts[topic.i] * topic.counts[topic.j]))
  }
}
topic.counts
co.occur.count
corr.matrix
max(corr.matrix)

cm <- data.frame(corr.matrix)
heatmap(corr.matrix, Rowv=NA, Colv="Rowv", symm=TRUE)



#############
# can we use topic co-occurence as a way of seeing how different two models are 
# in terms of classifying docs?
#############

# Create a second, no-ngram topic model

ngram.documents <- documents
nongram.model.name <- "nongram_model"

# load the persisted documents -- these are needed before we can load a model from state
nongram.file.name <- paste0(paste("models_dir", nongram.model.name, sep="/"), "-docs.Rdata")
# this shoudl create an object called "documents"
load(nongram.file.name)
nongram.documents <- documents
nongram.mallet.instances <- mallet.import(nongrams.documents$id, 
                                  nongrams.documents$text, 
                                  stop.word.file, 
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]")

## Initialize from a previously trained state
iters <- 800
maxims <- 20
model_num <- 6
model.label = paste(nongram.model.name, iters, maxims, formatC(model_num, width=2, flag="0"), sep="-")
file.name <- paste(model.dir, paste0(model.label, ".gz"), sep="/")
nongram.topic.model <- MalletLDA(num.topics=n.topics)
nongram.topic.model$loadDocuments(nongram.mallet.instances)
nongram.topic.model$initializeFromState(.jnew("java.io.File", file.name))

# the documents don't line up exactly, so we need to map the two sets together
non.to.n.overlaps <- sapply(nongram.documents$id, function(x) is.element(x, ngram.documents$id))
sum(non.to.n.overlaps)
nongram.documents[!non.to.n.overlaps, "text"]
n.to.non.overlaps <- sapply(ngram.documents$id, function(x) is.element(x, nongram.documents$id))
sum(n.to.non.overlaps)
ngram.documents[!n.to.non.overlaps, "text"]

########################
# pull out interesting topics by forum/topic ?
########################




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
#                       relevance(term w | topic t) = λ * p(w | t) + (1 - λ) * p(w | t)/p(w) 
#                       see Sievert & Shirley (2014)
# Return a list, by topic, of the l1 differences (absolute value) between each pair of factor levels
vocab.diff.by.factor <- function(factor, docs, topic.model, doc.subset=NULL, lambda=1.0, use.relevance=FALSE) {
  # TODO -- would be great if this could handle multiple factors
  words <- mallet.word.freqs(topic.model)
  word.freq <- words$term.freq / sum(words$term.freq)
  if (is.null(doc.subset)) {
    doc.subset <- !is.na(docs$text)
  }
  factor.lvls <- levels(factor(docs[doc.subset, factor]))
  
  words.by.factor <- list()
  word.relevence.by.factor <- list()
  # get the word propensity list for each factor

  for (lvl in factor.lvls) { 
    words.by.factor[[lvl]] <- mallet.subset.topic.words(topic.model, 
                                                        documents[,factor]==lvl & doc.subset, 
                                                        normalized=T)
    if(use.relevance) {
      word.relevence.by.factor[[lvl]] <- 
        lambda * log(1+words.by.factor[[lvl]]) + 
        (1-lambda) * log(1+words.by.factor[[lvl]] / ifelse(word.freq==0, 1, word.freq))
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

# Compare World and Value Topics 
l1.distances("jam", documents, topic.model)

# people_make_time is the most similar, interesting because it is the one that seems to be the most prevalent
#    across all documents

# Compare across the fora, since we saw that prevalences can be very different
l1.distances("forum", documents, topic.model)

# -- by world jam
## this DOESN'T work yet -- need to pass the full logic vector to the mallet function...
l1.distances("forum", documents, topic.model, documents$jam=="world")
# -- by values jam
l1.distances("forum", documents, topic.model, documents$jam=="values")

# Compare across managers/non-managers
l1.distances("manager", documents, topic.model)
l1.distances("manager", documents, topic.model, lambda=0.5, use.relevance=TRUE)

# Compare across continents
l1.distances("continent", documents, topic.model)


# Compare across time windows
l1.distances("CreationDate", documents, topic.model, documents$jam=="world")
l1.distances("CreationDate", documents, topic.model, documents$jam=="values")

#######################################
# compare vocabs of a topic by factor #
#######################################

topic.vocab.diff <- 
  function(factor, docs, topic.model, topic.id, doc.subset=NULL, lambda=1.0, use.relevance=FALSE) {
    number.words <- 20
    factor.pairs.differences <- vocab.diff.by.factor(factor, docs, topic.model, doc.subset, lambda, use.relevance)
    words <- mallet.word.freqs(topic.model)
    word.lists <- list()
    for (comp.name in names(factor.pairs.differences)) {
      word.lists[,comp.name] <- 0.5 * rowSums(abs(factor.pairs.differences[[comp.name]]))
    }
    comp.name = "Manager:Other"
    this.topic.words <- data.frame(words = words$words, 
                                   diff = factor.pairs.differences[[comp.name]][topic.id,],
                                   overall.freq = words$term.freq
                        )
    head(this.topic.words[order(this.topic.words$diff, decreasing=TRUE),], number.words)
    head(this.topic.words[order(this.topic.words$diff, decreasing=FALSE),], number.words)
  }
  




