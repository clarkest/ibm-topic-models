

# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
wd <-  "/users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")
library(tidyr)

model.name <- "anchor_ngram"
n.topics <- 30
model.num <- 9

model.name <- "anchor_world_ngram"
model.name <- "anchor_values_ngram"
n.topics <- 30
model.num <- 1


#model.object <- load.model.for.analysis(n.topics, model.name, model.num, regex.name="old_punctuation") 
model.object <- load.model.for.analysis(n.topics, model.name, model.num) 
topic.model <- model.object$topic.model
documents <- model.object$documents
doc.topics <- model.object$doc.topics
doc.topics.frame <- model.object$doc.topics.frame
model.label <- model.object$model.label

# new manager labels
mng.words <- c("mgr","manager","manages","mngr")
exec.words <- c("president","ceo","cfo","chief","exec", "executive","vp","vice president","dir","director","treasurer")
non.mgr.words <- c("program", "project")
clean.titles <- CleanTitles(documents$job)

# the replacing function
LabelTitles <- function(titles, word.list, new.label, old.labels) {
  for (word in word.list) {
    regex <- paste0("\\b",word,"\\b")
    old.labels[grepl(regex, titles)] <- new.label
  } 
  return(old.labels)
}

documents$new.mgr <- "other"
new.mgr <- documents$new.mgr
# manager words first
new.mgr <- LabelTitles(clean.titles, mng.words, "manager", new.mgr)
# exec words bump those up
new.mgr <- LabelTitles(clean.titles, exec.words, "executive", new.mgr)
# non manager words trump both though
new.mgr <- LabelTitles(clean.titles, non.mgr.words, "other", new.mgr)
#slap them back into the data frame and the model.object
documents$new.mgr <- new.mgr
model.object$documents <- documents
table(new.mgr)

# make sure the outpus directory for this model exists
output.dir <- file.path("outputs", model.label) 
dir.create(output.dir, showWarnings = FALSE)

##################################
#   Graph Formatting             #
##################################
text.1.5 <- element_text(size=rel(1.5))
text.1.0 <- element_text(size=rel(1.0))
text.2.0 <- element_text(size=rel(2.0))
thm <- theme(legend.text=text.1.0, 
             axis.title=text.1.0,
             legend.title=text.1.0,
             axis.text=text.1.0,
             plot.title=text.1.0           
)

##################################
#  1. Total Posts Over Time      #
##################################


#by.vars = c("manager", "continent", "jam")
by.vars = c("manager", "jam")
by.vars = c("new.mgr", "jam")
posts.by.window <- get.posts.by.window(documents, by.vars)

#output both the raw and the normalized plots
plt=qplot(as.integer(DateWindow), x.x, data = posts.by.window, 
          geom = "line", color=by.var) + geom_point() 
ggsave(file.path(output.dir, "raw_posts_by_time.png"), plt + thm)

plt=qplot(as.integer(DateWindow), post.rate, data = posts.by.window, 
          geom = "line", color=by.var) + geom_point()
ggsave(file.path(output.dir, "indexed_posts_by_time.png"), plt+thm)

##################################
#  2. Topics Shares Over Time    #
##################################
# for a given document-topic prevalence data frame, generate graphs of prevalence 
# by topic over time

# run for all of the topics
# topic / forum
col.keeps <- c("forum", "continent", "jam", "manager", "DateWindow")
by.vars <- c("jam", "forum")
plot.all.topic.shares(model.object, col.keeps, by.vars, 
                      file.path(output.dir, "forum_prev/"), ylim=c(0,0.35))

# managers and jam
col.keeps <- c("manager", "continent", "jam", "DateWindow")
by.vars <- c("manager", "jam")
plot.all.topic.shares(model.object, col.keeps, by.vars, file.path(output.dir, "manager_prev/"))

# gender and jam
col.keeps <- c("gender", "continent", "jam", "DateWindow", "manager")
by.vars <- c("gender", "jam")
plot.all.topic.shares(model.object, col.keeps, by.vars, file.path(output.dir, "gender_prev/"), ylim=c(0,0.15))

# new manager labels!
# managers and jam
col.keeps <- c("new.mgr", "continent", "jam", "DateWindow")
by.vars <- c("new.mgr", "jam")
plot.all.topic.shares(model.object, col.keeps, by.vars, 
                      file.path(output.dir, "new_manager_prev/"), 
                      ylim=c(0,0.13))

#diagnostic tool for seeing a particular topic's raw numbers
#View(avg.topic.rate <- aggregate(doc.topics.data[2], by=doc.topics.data[,aggregate.set], mean))


###############################################################
#  3. Topics Doc Counts Over Time, by threshold    #
###############################################################

######
# 3.a. histograms of topic prevalance over documents, with a threshold prevalence
######
# add the document lengths to the model.object
unnormal.doc.topics <- mallet.doc.topics(model.object$topic.model, smoothed=F, normalized=F)
model.object$doc.len <- rowSums(unnormal.doc.topics)

threshold <- 0.1
# minimum number of words
col.keeps <- c("manager", "continent", "jam", "DateWindow")
by.vars <- c("manager", "jam")
plot.all.topic.shares(model.object,
                      col.keeps, 
                      by.vars, 
                      sprintf(file.path(output.dir, "threshold_%1.0f/"), 100*threshold),
                      ylim=NULL, 
                      threshold.prev=threshold
                      )
threshold<-0.2
plot.all.topic.shares(model.object,
                      col.keeps, 
                      by.vars, 
                      sprintf(file.path(output.dir, "threshold_%1.0f/"), 100*threshold),
                      ylim=NULL, 
                      threshold.prev=threshold
)
threshold<-0.05
plot.all.topic.shares(model.object,
                      col.keeps, 
                      by.vars, 
                      sprintf(file.path(output.dir, "threshold_%1.0f/"), 100*threshold),
                      ylim=NULL, 
                      threshold.prev=threshold
)

#################
# working with just the topics of focus
#################

focal.topics <-  c(6, 10, 19, 23, 27, 28, 22)
##  Notice that R indexes from 1, so these indices need incremented by 1
focal.topics.index <- focal.topics + 1

# create a data frame with just the focal topics, with prevalence rebalanced to be proportion of focal topics
focal.doc.topics <- doc.topics.frame[,focal.topics.index] / rowSums(doc.topics.frame[,focal.topics.index])
plot.all.topic.shares(focal.doc.topics, documents, col.keeps, by.vars, 
                      posts.by.window, file.path(output.dir, "focal_prev/"), focal.topics, ylim=c(0,0.3))


mallet.top.words(topic.model, topic.words[20,], 20)


#############
# histo of document lengths
#############

unnormal.doc.topics <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)
doc.len <- rowSums(unnormal.doc.topics)
documents$words <- doc.len

plt <- qplot(documents$words, binwidth=5, xlab = "number of words")
ggsave(file.path(output.dir, "words_per_doc.png"), plt+thm)
# --and logged to show that it's approx log-normal
qplot(log(documents$words), binwidth=0.1, xlab = "log(number of words)")


# by jam
ggplot(documents, aes(x=words, color=jam)) + geom_density()
ggplot(documents, aes(x=log(words), color=jam)) + geom_density()

# by manager
ggplot(documents, aes(x=words, color=manager)) + geom_density()
ggplot(documents, aes(x=log(words), color=manager)) + geom_density()

# by manager-jam
ggplot(documents, aes(x=words, color=paste(jam,manager,sep="-"))) + geom_density()
plt <- ggplot(documents, aes(x=log(words), color=paste(jam,manager,sep="-"))) + 
          geom_density()
ggsave(file.path(output.dir, "relative_doc_lengths.png"), plt)

################################
# histo of docs per user
################################

docs.per.user <- summarize(group_by(documents, jam, manager, user), docs=n(), words=sum(words))
ggplot(docs.per.user, aes(x=ifelse(docs>20,20,docs), color=paste(jam,manager,sep="-"))) + geom_histogram(postion="dodge", binwidth=1)
ggplot(docs.per.user, aes(x=log(words), color=paste(jam,manager,sep="-"))) + geom_density()

summarize(group_by(docs.per.user, jam, manager), mean(words), sd(words), mean(docs), sd(docs))




########################
# distances between subgroups within each topic
########################

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
l1.distances("manager", documents, topic.model, lambda=0.7, use.relevance=TRUE)

# Compare across continents
l1.distances("continent", documents, topic.model)


# Compare across time windows
l1.distances("CreationDate", documents, topic.model, documents$jam=="world")
l1.distances("CreationDate", documents, topic.model, documents$jam=="values")

#######################################
# compare vocabs of a topic by factor #
#######################################

topic.vocab.diff("manager", documents, topic.model, 19, number.words=10)
topic.vocab.diff("continent", documents, topic.model, 19, number.words=10)

topic.vocab.diff("manager", documents, topic.model, 19, number.words=10, lambda=0.8, use.relevance=TRUE)
topic.vocab.diff("continent", documents, topic.model, 2, number.words=10)



##############################
# topic co-occurence
##############################

ret.1 <- topic.co.occur(topic.model)
#topic.counts
ret.1$co.occur.count
ret.1$corr.matrix

corr.heatmap(ret.1$corr.matrix, min=-2, max=2)



########################
# Individual Word Freq
########################


graphTopicWords <- function(word, by.var = "jam", filter.func=F, prev.thresh = 0.2, title.postpend) {
  # which docs have the word?
  documents[,"docs.have.word"] <- grepl(word, documents$text, ignore.case=T)
  
  #documents %>% group_by(jam) %>% 
  #  dplyr::summarise(word = mean(docs.have.word))
  
  # quick view at topics
  d.tps <- ifelse(doc.topics >= prev.thresh, 1, 0)
  colnames(d.tps) <- paste0("topic_", 1:30)
  
  #topic.word.rates <- colMeans(d.tps * documents$docs.have.word) / colMeans(d.tps) / mean(documents$docs.have.word)
  # round(topic.word.rates-1,2)
  
  # topic.word.freq <- colMeans(d.tps * documents$docs.have.word) / colMeans(d.tps)
  
  d.tps.d <- cbind(documents, d.tps) 
  if (is.function(filter.func)) {
    d.tps.d <- filter.func(d.tps.d) 
  }  
  d.tps.d <- d.tps.d %>%
    select_(.dots=c(by.var, "docs.have.word", colnames(d.tps))) %>%
    #gather(topic, appears, -docs.have.word, -jam)
    gather_("topic", "has.topic", paste0("topic_", 1:30)) %>%
    rename_(.dots=c("by.var" = by.var)) %>%
    group_by(by.var, topic) %>%
    summarise(doc.share=mean(has.topic * docs.have.word) / mean(has.topic))
  
  overall <- documents %>%
    group_by_(by.var) %>%
    summarise(doc.share = mean(docs.have.word)) %>%
    rename_(.dots=c("by.var" = by.var)) %>%
    cbind(data.frame(topic="OVERALL"))
  
  d.tps.d <- rbind(d.tps.d, overall)
  
  ggplot(data=d.tps.d, aes(y=topic)) +
    geom_point(aes(x=doc.share, color=by.var), size=2) +
    xlim(0,1) + xlab(sprintf("Share of Documents Containing Word '%s'", word)) +
    ggtitle(sprintf("Share of Documents Containing '%s'%s", word, title.postpend)) +
    thm
}

getFilePath <- function(file.name) {
  sprintf("%s/word_compare/%s.png", output.dir, file.name)
}
library(grid)

jam.filter = function(jam.value) {function(data){filter(data, jam==jam.value)}}

graphTopicWords(" I ", "new.mgr", jam.filter("values"), title.postpend=" - Values Jam")
graphTopicWords(" I ", "new.mgr", jam.filter("world"), title.postpend=" - World Jam")
graphTopicWords("ibm", "new.mgr", jam.filter("values"))
graphTopicWords("ibm", "new.mgr", jam.filter("world"))
graphTopicWords(" we ", "new.mgr", jam.filter("values"))
graphTopicWords(" we ", "new.mgr", jam.filter("world"))

gridWordGraph <- function(word.list, by.var, out.file.name) {
  gset <- list()
  for (word in word.list) {
    gset[[length(gset) + 1]] <- cbind(
      ggplotGrob(graphTopicWords(word, by.var, jam.filter("values"), title.postpend=" - Values Jam")),
      ggplotGrob(graphTopicWords(word, by.var, jam.filter("world"), title.postpend=" - World Jam")),
      size="last"
    )
  }
  png(filename=getFilePath(out.file.name), width=1200, height=600 * length(word.list))
    grid.newpage()
    grid.draw(do.call("rbind", c(gset, size="last")))
  dev.off()
}

gridWordGraph(c(" I ", " we ", "ibm"), "new.mgr", "we_i_ibm")
gridWordGraph(c("customer", "client", "solution"), "new.mgr", "customer_client_solution")

gridWordGraph(c("manager", "decision", "empowerment", "authority", "mentor"), 
              "new.mgr", "manager_empowerment")




