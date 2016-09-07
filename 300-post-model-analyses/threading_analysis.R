library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(stargazer)
library(xtable)
library(digest)
library(stringr)
library(pscl)
library(MASS)
detach("package:dplyr", unload=TRUE)
library(dplyr)


# wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
# wd <-  "/media/sf_ibm-topic-model"
wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")
source("300-post-model-analyses/thread_functions.R")
model.name <- "anchor_ngram"
n.topics <- 30
model.num <- 9

# now load the unix model data and documents
list <- load.model.for.analysis(n.topics, model.name, model.num) 
topic.model <- list$topic.model
documents <- list$documents
doc.topics <- list$doc.topics
doc.topics.frame <- list$doc.topics.frame
model.label <- list$model.label
doc.topics.unsmooth <- mallet.doc.topics(topic.model, smoothed=F, normalized=T)
doc.topics.unnormal <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)
unnormal.doc.topics <- doc.topics.unnormal

diags <- rJava::.jnew("cc/mallet/topics/TopicModelDiagnostics", 
                      rJava::.jcast(topic.model, "cc/mallet/topics/ParallelTopicModel"), 20L)
diags$getCoherence()$scores
diags$getEffectiveNumberOfWords()$scores
diags$toXML()


MyParentsParents <- function(id, parents, ancestor.list=c()) {
  parent.id <- toString(parents[id])
  if (parent.id=="NA") {
    return (c("missing", ancestor.list))
  } else if (parent.id == "null") {
    return (ancestor.list)
  } else {
    return (MyParentsParents(parent.id, parents, c(parent.id, ancestor.list)))
  }
}

MyParentsParentsStr <- function(id, parents, ancestor.list="") {
  parent.id <- toString(parents[id])
  if (parent.id=="NA") {
    return (paste("missing", ancestor.list, sep=","))
  } else if (parent.id == "null") {
    return (ancestor.list)
  } else {
    return (MyParentsParentsStr(parent.id, parents, paste(parent.id, ancestor.list, sep=",")))
  }
}


# weak version of memoization, where anything already run from the leaf is stored
# and the roots are stored up front
# but not the nodes along the way to the root.
MyParentsParentsStrM <- function(id, parents, memo) {
  if(!(id %in% names(memo))) {
    stop("Why have you given me a missing id?")
  }
  if(!is.na(memo[id])) {return(memo[id])}
  parent.id <- toString(parents[id])
  if(!(parent.id %in% names(memo))) {
    terminal.id <- paste0(substr(parent.id, 2,20), "-missing")
    return(c(terminal.id))
  }
  a <- c(parent.id, MyParentsParentsStrM(parent.id, parents, memo))
  # a <- paste(parent.id, MyParentsParentsStrM(parent.id, parents, memo), sep=",")
  return(a)
}

paste(MyParentsParentsStrM(documents[7,"id"], parents, memo), collapse=",")

parents <- documents$parent
names(parents) <- documents$id

memo <- rep(NA, nrow(documents))
# prefill the terminal ids
memo <- ifelse(documents$parent=="NA", "missing_parent", memo)
memo <- ifelse(documents$parent=="null", "root", memo)
names(memo) <- documents$id


id.list <- documents[documents$parent != "null" & documents$parent != "NA", "id"]
for(i in 1:length(id.list)) { 
  id <- id.list[i]
  if (i%%1000 == 0) print(i)
  b <- MyParentsParentsStrM(id, parents, memo)
  memo[id] <- paste(b, collapse=",")
}
m.ancestors <- memo
# to be compatible with old code
ancestors <- m.ancestors

threaded.docs <- documents


# transform the ancestry text into a list of ancestors
threaded.docs$ancestors <- lapply(ancestors, function(x) unlist(strsplit(x, split=",")))
threaded.docs$ancestor.str <- ancestors
threaded.docs$generation <- sapply(threaded.docs$ancestors, length)
# note this is still not quite right because we are counting those with missing parents as one generation older than they are
ggplot() + geom_histogram(aes(x=threaded.docs$generation), binwidth = 1)

# threaded.docs$ancestors[threaded.docs$ancestors=="root"] <- NA 
melted.ancestors <- melt(threaded.docs$ancestors)
melted.ancestors <- melted.ancestors[melted.ancestors$value!="root",]

num.children <- summarize(group_by(melted.ancestors, value), n.children=n())
# drop the count on "missing"
num.children <- num.children[num.children$value!="missing",]
# threaded.docs <- merge(threaded.docs, num.children, by.x="id", by.y="value", all.x=TRUE, suffixes=c("x",""))

threaded.docs <- 
  left_join(threaded.docs, num.children, by=c("id"="value")) %>%
  mutate(n.children = ifelse(is.na(n.children), 0, n.children))


###################
# MISSING THREADS #
###################

ans.list <- strsplit(m.ancestors, split=",")
threaded.docs$missing.ancestor <- F
threaded.docs$root.id <- threaded.docs$id
for (i in 1:length(ans.list)) {
  al <- ans.list[[i]]
  if (length(al)>0) {
    last.id <- al[length(al)]
    if(substr(last.id, (nchar(last.id)-7),  nchar(last.id)) == "-missing") {
      threaded.docs[i, "missing.ancestor"] <- T
      threaded.docs[i,"root.id"] <- last.id
      # AND the generation needs to have 1 added to it 
      threaded.docs[i,"generation"] <- threaded.docs[i,"generation"] + 1
    }
    # if this doesn't have a missing root, and is its own root, mark it's root id as itself
    else if(length(al)==1) {
      threaded.docs[i,"root.id"] <- threaded.docs[i,"id"]
    } else {
      # if it has another root, make sure to grab the id of that root
      threaded.docs[i,"root.id"] <- al[length(al)-1]
    }
  }
}
ggplot() + geom_histogram(aes(x=threaded.docs$generation), binwidth = 1)
save(threaded.docs, file="place_docs_here/threaded_docs.Rdata")
# load("place_docs_here/threaded_docs.Rdata")


# missing ancestors?
sum(threaded.docs$missing.ancestor)
# unique root ids?
length(unique(threaded.docs$root.id))

getMode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
  
# what do groups by root.id look like?
by.root <- threaded.docs %>% group_by(root.id, jam) %>% 
  summarise(comments=n(), title.example=getMode(title),
            first.post=min(Timestamp), last.post=max(Timestamp), 
            duration=difftime(max(Timestamp), min(Timestamp), units="hours"),
            forum=first(forum)) %>%
  arrange(desc(comments))


by.root.window <- threaded.docs %>% group_by(root.id, DateWindow) %>% 
  summarise(n=n()) %>% spread(DateWindow, n, fill="") 
names(by.root.window) = c("root.id", sprintf("v%02d",1:11), sprintf("w%02d",1:10))
by.root <- left_join(by.root, by.root.window, by="root.id")

save(by.root, file="place_docs_here/by_root.Rdata")

ggplot(by.root, aes(x=comments, color=jam)) + geom_histogram(breaks=1:25) + facet_wrap(~ jam)
summary(by.root[,"comments"])
summary(by.root[by.root$jam=="world", "comments"])
summary(by.root[by.root$jam=="values", "comments"])

sum(by.root$comments>25 & by.root$jam=="world")
sum(by.root$comments>25 & by.root$jam=="values")
by.root %>% group_by(jam) %>% summarise(mx=max(comments))





####################################################
##################### ANALYSES #####################
####################################################

################
# RESPONSE TIMING 
################

parent.timing <- left_join(threaded.docs[,c("id", "parent", "Timestamp", "new.mgr")], 
      threaded.docs[,c("id", "Timestamp", "new.mgr")], 
      by=c("parent"="id"),
      suffixes=c("child","parent")
      )

parent.timing$response.time <- 
  difftime(parent.timing$Timestamp.x, parent.timing$Timestamp.y, units="hours")

ggplot(parent.timing, aes(x=as.numeric(response.time))) + geom_density() +
  xlab("Hours since prior post") +
  ggtitle("Response time of responding posts")
ggplot(parent.timing, aes(x=as.numeric(response.time), color=new.mgr.y)) + 
  geom_density() +
  xlab("Hours since prior post") +
  ggtitle("Response time of responding to posts from Manager Class")
ggplot(parent.timing, aes(x=as.numeric(response.time), color=new.mgr.x)) + 
  geom_density() +
  xlab("Hours since prior post") +
  ggtitle("Response time of responding to posts by Manager Class")


# 1 obs with 0 response time, 28 with less than 0.  weird
sum(parent.timing$response.time==0, na.rm=T)
sum(parent.timing$response.time<0, na.rm=T)
# in minutes -- only 3, so not terrible
60*as.numeric(min(parent.timing$response.time, na.rm=T))

# distribution within first hour?
ggplot(parent.timing, aes(x=as.numeric(response.time), color=new.mgr.y)) + geom_density() +
  xlab("Hours since prior post") +
  ggtitle("Response time of responding posts") +
  xlim(0,1)



###################
# Thread topic rates
###################

# threshold at which to count a topic in a doc
prev.thresh <- 0.15
topic.list <- topicLists(doc.topics.unsmooth, prev.thresh)
threaded.docs$topic.list <- topic.list

# overall topic prev of each thread
by.root.topics <- threaded.docs %>% select(root.id) %>%
  cbind(doc.topics.unsmooth) %>%
  group_by(root.id) %>%
  summarise_each(funs(prev=mean, count=sum(.>prev.thresh)))
thread.topic.over.thresh <- as.matrix(by.root.topics[,32:61])
top.topic.list <- topicLists(thread.topic.over.thresh, 5, " %d")
by.root$top.topic.list <- top.topic.list

thread.prevs <- as.matrix(by.root.topics[,2:31])
thread.topic.list <- topicLists(thread.prevs, prev.thresh)
by.root$thread.topic.prev <- thread.topic.list


###################
# thread browser
###################

cut.off <- 1
root.ids <- by.root$root.id[by.root$comments >= cut.off]
out.frame <- by.root %>%
  filter(comments >= cut.off) %>%
  mutate(thread = paste0('<a href="', digest(root.id, algo="md5", serialize=F), '.html">', title.example,'</a>')) %>%
  ungroup() %>%
  select_(.dots=c("thread", "forum", "thread.topic.prev", "top.topic.list", "comments", 
                  sprintf("v%02d",1:11), sprintf("w%02d",1:10))) %>%
  arrange(desc(comments))
xt <- xtable(out.frame)
print(xt, type="html", file="~/sandbox/thread_test/thread_browse/index.html", sanitize.text.function = force)

# make sure threaded docs know their doc #
threaded.docs$doc.id <- 1:nrow(threaded.docs)

# allow us to look up the id of the parent doc too
parent.id.lookup <- threaded.docs %>% transmute(id=id, parent.doc.id=doc.id) 
t.docs <- left_join(threaded.docs, parent.id.lookup, by=c("parent"="id"))
t.docs$parent.doc.id <- ifelse(is.na(t.docs$parent.doc.id), 
                              ifelse(t.docs$missing.ancestor, "missing", "root"),
                              t.docs$parent.doc.id)

for (rid in root.ids) {
  thread.frame <- t.docs %>%
    filter(root.id == rid) %>%
    mutate(document = paste0('<a href="../by_jam/article_', doc.id, '.html">', doc.id,'</a>'),
           dateTime = paste(CreationDate, str_pad(CreationTime, 11, pad="0"), sep=" "),
           direct.parent = paste0('<a href="../by_jam/article_', parent.doc.id, '.html">', parent.doc.id,'</a>'),
           gen=sprintf("%d", generation)) %>%
    select(document, dateTime, new.mgr, job, gender, gen, direct.parent, topic.list) %>%
    arrange(dateTime)
  xt <- xtable(thread.frame)
  fil.nam <- paste0("~/sandbox/thread_test/thread_browse/", digest(rid, algo="md5", serialize = F), ".html")
  print(xt, type="html", file=fil.nam, sanitize.text.function = force)
}













##########
# titles #
##########

# how do threads comapre to titles?
length(unique(threaded.docs$title))
# root threads
sum(threaded.docs$generation==1)
# and threads one step below a missing root
sum(ancestors=="missing_parent")
# together, how many threads have repeat titles and report a missing parent?
sum(threaded.docs$generation==1) + sum(ancestors=="missing_parent") - 
  length(unique(threaded.docs$title))

# by title, how often are the parents mapping to missing threads
a <- threaded.docs %>% group_by(title) %>% 
  summarise(n=n(), first.gen=sum(generation==1), 
            missing.parent=sum(missing.parent),
            jam=max(as.character(jam)))
View(a)

threaded.docs %>% group_by(jam) %>% 
  summarise(n=n(),missing.parent=sum(missing.parent), 
            perc = sum(missing.parent)/n())




###############
# response rates
###############

# how many managers v. others get a response when they post?
ResponseRates <- function(class) {
  summarize(group_by_(threaded.docs, .dots=c("jam",class)),
          total.posts=n(), 
          unique.uesrs=n_distinct(user),  
          with.children=sum(n.children>0),
          with.5=sum(n.children>=5),
          with.10=sum(n.children>=10),
          perc.response=with.children/total.posts,
          perc.5=with.5/total.posts,
          perc.10=with.10/total.posts
  )
}

ResponseRates("manager")
# threaded.docs$new.mgr <- new.mgr
ResponseRates("new.mgr")
ResponseRates("gender")
# do post shares change between jams though??
table(threaded.docs$jam, threaded.docs$new.mgr) / as.vector(table(threaded.docs$jam))
table(threaded.docs$jam, threaded.docs$gender) / as.vector(table(threaded.docs$jam))

# how about for top-line comments?
topLineComments <- function(class) {
  summarize(group_by_(threaded.docs[threaded.docs$generation==1,], .dots=c("jam", class)), 
            total.posts=n(), 
            unique.uesrs=n_distinct(user),  
            with.children=sum(n.children>0),
            with.5=sum(n.children>=5),
            with.10=sum(n.children>=10),
            perc.response=with.children/total.posts,
            perc.5=with.5/total.posts,
            perc.10=with.10/total.posts
  )
}
topLineComments("new.mgr")
topLineComments("gender")

# are people more likely to respond to people in the same class as them?
class <- "new.mgr"
class <- "gender"

class.parent <- paste0(class,".parent")
# same as parent?
match.to.parents <- merge(threaded.docs[,c("id", "parent", class)], 
      threaded.docs[,c("id", class)], 
      by.x="parent",
      by.y="id",
      suffixes=c(".child",".parent"),
      all.x=TRUE)

match.to.parents$match.parent <- 
  match.to.parents[paste0(class,".child")] == match.to.parents[class.parent]
match.to.parents <- match.to.parents[c("id", "match.parent", class.parent)]
#attr(match.to.parents$match.parent, "dimanmes") <- NULL

# CAREFUL WITH MERGES
threaded.docs <- left_join(threaded.docs, match.to.parents)
# oddly, the match.parent column is picking up attributes that kill summarize.  so, remove them
attr(threaded.docs$match.parent, "dimnames") <- NULL
attr(threaded.docs$match.parent, "dimnames") <- NULL




# look at whether comments share the same CLASS as their parent
summarize(group_by_(threaded.docs, .dots=c("jam", class)), 
         match=sum(match.parent, na.rm=TRUE), 
         different=sum(!match.parent, na.rm=TRUE),
         match_perc = match / (match + different)
         )

subset <- threaded.docs$jam == "values"
val.class.chi <- chisq.test(table(threaded.docs[subset, class], threaded.docs[subset, "match.parent"]))
val.class.chi$residuals



# BETTER: chi-square standard residuals of child class and parent class
subset <- threaded.docs$jam == "values"
val.class.chi <- chisq.test(table(threaded.docs[subset, class], threaded.docs[subset, class.parent]))
val.class.chi$residuals

subset <- threaded.docs$jam == "world"
world.class.chi <- chisq.test(table(threaded.docs[subset, class], threaded.docs[subset, class.parent]))
world.class.chi$residuals


# what we really want is expected values that are based on the total population proportions of comments
# not "the number of postings by managers that received responses,"
# but, "the number of posts by managers"

t <- table(threaded.docs[subset, class], threaded.docs[subset, class.parent])
a <- table(threaded.docs[subset, class]) / sum(subset)
colSums(t)


# let's focus on just threads above X in length
min.thread <- 5

# get the original comment of those threads
long.threads <- threaded.docs[threaded.docs$n.children >= 5 & threaded.docs$parent=="null", c("id","n.children")]
long.threads$original <- long.threads$id
children.of.long <- melted.ancestors[melted.ancestors$value %in% long.threads$id,]
names(children.of.long) <- c("original", "id")
children.of.long <- rbind(long.threads[c("original","id")], children.of.long)
children.of.long <- merge(children.of.long, threaded.docs[c("id", class)])

class.counts.by.thread <- summarize(group_by_(children.of.long, .dots=c("original")), 
          count=n(),
          num_man = sum(manager=="Manager", na.rm=TRUE),
          num_oth = sum(manager=="Other", na.rm=TRUE),
          perc_oth = num_oth/count,
          perc_man = num_man/count
          )

hist(class.counts.by.thread$perc_man, breaks=10)
hist(class.counts.by.thread$perc_oth, breaks=10)
hist(class.counts.by.thread$count)
summarize(group_by_(class.counts.by.thread, .dots=c("original")),
          tot = sum(count, na.rm=TRUE),
          tot = sum(count, na.rm=TRUE)
)

class.counts.by.thread <- merge(class.counts.by.thread, 
                                threaded.docs[c("id", class)],
                                by.x="original",
                                by.y="id"
                          )


# get the list of children for each of these



# how many of its ancestors is it like?
class <- "manager"
# same as parent?
match.to.ancestors <- merge(threaded.docs[,c("id", "parent", class)], 
                          threaded.docs[,c("id", class)], 
                          by.x="parent",
                          by.y="id",
                          suffixes=c("child","parent"),
                          all.x=TRUE)



##############################
# Provking Response Analysis #
##############################

load("place_docs_here/threaded_docs.Rdata")
load("place_docs_here/by_root.Rdata")
th.doc.topics <- threaded.docs
th.doc.topics$responded <- ifelse(th.doc.topics$n.children==0, 0, 1)
th.doc.topics$is.exec <- ifelse(th.doc.topics$new.mgr=="executive", 1, 0)
th.doc.topics$is.manager <- ifelse(th.doc.topics$new.mgr=="manager", 1, 0)
th.doc.topics$ancestors <- th.doc.topics$generation - 1
th.doc.topics$length <- rowSums(doc.topics.unnormal)
th.doc.topics$length.sq <- rowSums(doc.topics.unnormal)^2
th.doc.topics$log.length <- log(rowSums(doc.topics.unnormal))

d.tps <- doc.topics.unsmooth
# calculate the topic focus for each document
th.doc.topics$focus <- rowSums(d.tps^2)
th.doc.topics$solo.topic <- th.doc.topics$focus == 1
th.doc.topics$adj.focus <- rowSums(d.tps^2) - th.doc.topics$solo.topic
th.doc.topics$adj.conc.sq <- th.doc.topics$adj.focus ^ 2
ggplot(data=th.doc.topics, aes(x=focus, color=jam)) + geom_density()

ggplot(data=th.doc.topics, aes(y=focus, x=log.length)) + 
  geom_point() +
  geom_density_2d()

# we'd also like to know how responses are related to the current temporal intensity of the conversations
#  (1) is this the first comment in a thread?
#  (2) if not, how long after it's parent is it being posted?
#    (2a) need to include the dummy for missing parents
th.doc.topics$is.first.comment <- th.doc.topics$generation == 1
th.doc.topics$missing.parent <- th.doc.topics$missing.ancestor & th.doc.topics$generation == 2
a <- select(threaded.docs, id, Timestamp) %>% 
  right_join(select(threaded.docs, parent, Timestamp), 
             by=c("id"="parent") )
time.since.parent <- as.numeric(a$Timestamp.y- a$Timestamp.x, units="secs")
th.doc.topics$sec.since.parent <- time.since.parent
th.doc.topics$log.sec.since.parent <- ifelse(th.doc.topics$generation > 1 & !th.doc.topics$missing.parent,
                                          log(time.since.parent),
                                          0)
th.doc.topics$log.sec.since.parent <- ifelse(is.na(th.doc.topics$log.sec.since.parent) | th.doc.topics$log.sec.since.parent < 0,
                                             0,
                                             th.doc.topics$log.sec.since.parent)
th.doc.topics$log.sec.since.parent.sq <- th.doc.topics$log.sec.since.parent ^ 2
th.doc.topics$log.sec.since.parent.cube <- th.doc.topics$log.sec.since.parent ^ 3

# jaccard similarity from way way below

th.doc.topics <- left_join(select(th.doc.topics, -jaccard.thresh, -focus.js, -focus.cos, -focus.kld), 
                           select(child.tps, id, jaccard.thresh, focus.js, focus.cos, focus.kld), by ="id") %>%
  mutate(simil.to.parent = ifelse(is.na(jaccard.thresh), 0, jaccard.thresh),
         simil.to.parent.cent = ifelse(is.na(jaccard.thresh) | generation==1, 0,
                                       simil.to.parent - mean(jaccard.thresh[generation>1], na.rm=T)),
         focus.js=ifelse(is.na(focus.js), 0, focus.js),
         focus.js.cent=ifelse(focus.js==0, 0, focus.js - mean(focus.js[focus.js>0])),
         focus.cos.cent=ifelse(focus.cos==0, 0, focus.cos - mean(focus.cos[focus.cos>0], na.rm=T)),
         focus.cos.cent=ifelse(is.na(focus.cos), 0, focus.cos.cent),
         focus.cos=ifelse(is.na(focus.cos), 0, focus.cos),
         focus.kld=ifelse(is.na(focus.kld), 0, focus.kld),
         focus.kld.cent=ifelse(focus.kld==0, 0, focus.kld - mean(focus.kld[focus.kld>0]))
         )
sd.js <- sd(child.tps$focus.js)
sd.kld <- sd(child.tps$focus.kld)
sd.cos <- sd(child.tps$focus.cos)

th.doc.topics$focus.js.std <- th.doc.topics$focus.js.cent / sd.js
th.doc.topics$focus.kld.std <- th.doc.topics$focus.kld.cent / sd.kld
th.doc.topics$focus.cos.std <- th.doc.topics$focus.cos.cent / sd.cos

# Track how long into a Conversation a comment was made

thread.family.timing <- 
  select(th.doc.topics, id, root.id, Timestamp) %>%
  left_join(select(by.root, root.id, first.post)) %>%
  mutate(secs.since.thread = as.numeric(Timestamp - first.post, units="secs")) 


aa <- th.doc.topics %>% filter(!missing.parent & !is.first.comment & log.sec.since.parent<4.1 & responded==0)
ans <- unlist(strsplit(aa[3, "ancestor.str"], split=","))
this.time <- aa[3, "Timestamp"]
ancestors.times <- filter(th.doc.topics, id %in% ans)$Timestamp
secs.ago <- as.numeric(this.time - ancestors.times, units="secs")
secs.ago <- ifelse(secs.ago<0, 0, secs.ago)
alpha <- exp(log(0.5) / 60)
sum(alpha ^ secs.ago)
this.id<-aa[3,"id"]
filter(th.doc.topics, id==this.id)


addExpExcite <- function(th.doc.topics, half.life.sec) {
  which.excite <- sprintf("exp.excite.%d", round(half.life.sec/60,1))
  which.excite.first <- sprintf("exp.excite.%d.1st", round(half.life.sec/60,1))
  which.excite.2nd <- sprintf("exp.excite.%d.2nd", round(half.life.sec/60,1))
  which.excite.2nd.2 <- sprintf("exp.excite.%d.2nd.2", round(half.life.sec/60,1))
  print(which.excite)
  excite <- rep(0, nrow(th.doc.topics))
  excite.1st <- rep(0, nrow(th.doc.topics))
  excite.2nd <- rep(NA, nrow(th.doc.topics))
  excite.2nd.2 <- rep(NA, nrow(th.doc.topics))
  alpha <- exp(log(0.5) / half.life.sec)
  
  for (i in 1:nrow(th.doc.topics)) {
    if (th.doc.topics[i, "generation"] > 1 & !th.doc.topics[i, "missing.parent"]) {
      this.time <- th.doc.topics[i, "Timestamp"]
      if(th.doc.topics[i, "generation"] == 2) {
        ans <- th.doc.topics[i, "parent"] 
      } else {
        ans <-  unlist(strsplit(th.doc.topics[i, "ancestor.str"], split=","))
      }
      ancestors.times <- th.doc.topics[th.doc.topics$id %in% ans, "Timestamp"]
      secs.ago <- as.numeric(this.time - ancestors.times, units="secs")
      secs.ago <- ifelse(secs.ago<0, 0, secs.ago)
      excite[i] <- sum(alpha ^ secs.ago)
      # just the first parent portion
      excite.1st[i] <- alpha ^ min(secs.ago)
      if (length(secs.ago) > 1) {
        excite.2nd[i] <- alpha ^ secs.ago[order(secs.ago)[2]]
        excite.2nd.2[i] <- alpha ^ (secs.ago[order(secs.ago)[2]] - secs.ago[order(secs.ago)[1]])
      } else {
        excite.2nd[i] <- NA
        excite.2nd.2[i] <- NA
      }
    } 
    # else keep the defaul zeros
  }
  th.doc.topics[,which.excite] <- excite
  th.doc.topics[, which.excite.first] <- excite.1st
  th.doc.topics[, which.excite.2nd] <- excite.2nd
  th.doc.topics[, which.excite.2nd.2] <- excite.2nd.2
  return(th.doc.topics)
}



th.doc.topics <- addExpExcite(th.doc.topics, 60)
th.doc.topics <- addExpExcite(th.doc.topics, 120)
th.doc.topics <- addExpExcite(th.doc.topics, 180)
th.doc.topics <- addExpExcite(th.doc.topics, 300)
th.doc.topics <- addExpExcite(th.doc.topics, 600)
th.doc.topics <- addExpExcite(th.doc.topics, 1200)
th.doc.topics <- addExpExcite(th.doc.topics, 1800)
th.doc.topics <- addExpExcite(th.doc.topics, 3600)
th.doc.topics <- addExpExcite(th.doc.topics, 5400)
th.doc.topics <- addExpExcite(th.doc.topics, 7200)
th.doc.topics <- addExpExcite(th.doc.topics, 10800)

gg <- 
  th.doc.topics %>%
  filter(!missing.parent & !is.first.comment) %>%
  select(exp.excite.1, exp.excite.2, exp.excite.5, exp.excite.10, exp.excite.20, exp.excite.30) %>%
  gather(window.minutes, excitement) %>%
  ggplot(aes(excitement, colour=window.minutes)) +
  geom_density(size=1) +
  coord_cartesian(ylim=c(0,100)) +
  theme(text=element_text(size=16))
ggsave(gg, file="outputs/excitement_exp_decay_densities.png")

excite.zero.order <- th.doc.topics %>%
  filter(generation>1 & !missing.parent) %>%
  group_by(jam, responded) %>% 
  summarise(mean(exp.excite.1), mean(exp.excite.2), mean(exp.excite.3),
            mean(exp.excite.5), mean(exp.excite.10), mean(exp.excite.20), mean(exp.excite.30), mean(exp.excite.60))
(excite.zero.order[2, -1] - excite.zero.order[1, -1]) / excite.zero.order[1, -1]
(excite.zero.order[4, -1] - excite.zero.order[3, -1]) / excite.zero.order[3, -1]

# Final Eight Hours
#    let's add in a flag to control for the last two periods of each jam
th.doc.topics$last.period <- 
  ifelse((th.doc.topics$Timestamp >  "2003-08-01 04:00:00 PDT" & th.doc.topics$jam=="values") | # 64 hours into values
           (th.doc.topics$Timestamp >  "2004-10-28 15:00:00 PDT" & th.doc.topics$jam=="world"), # 46 hours into world
         1, 0)
  
# and u.s. nighttime
th.doc.topics$u.s.time.window <- substr(th.doc.topics$DateWindow,12,13)
# 4-hour windows?
th.doc.topics$date.window.4 <- newDateWindows(th.doc.topics, 4)
th.doc.topics$u.s.time.window.4 <- substr(th.doc.topics$date.window.4,12,13)
time.window.names <- data.frame(u.s.time.window.4=as.character(0:5), 
                                u.s.time.=c("00-04 GMT", "04-08 GMT", "08-12 GMT", "12-16 GMT", "16-20 GMT", "20-24 GMT"))
th.doc.topics <- left_join(th.doc.topics, time.window.names, by="u.s.time.window.4")
# allow Americas to be omitted continent
# get the proper continent
th.doc.topics <- left_join(select(th.doc.topics, -continent), select(documents, id, continent))
th.doc.topics$continent2 <- ifelse(th.doc.topics$continent=="Americas", "AAmericas", th.doc.topics$continent)
#th.doc.topics$continent2 <- ifelse(is.na(th.doc.topics$continent2), "unknown", th.doc.topics$continent2)

##################
# I, we, and ibm #
##################

th.doc.topics$docs.have.we <- grepl(" we ", documents$text, ignore.case=T)
th.doc.topics$docs.have.i <- grepl(" I ", documents$text, ignore.case=T)
th.doc.topics$docs.have.ibm <- grepl("ibm[ .,!-_']", documents$text, ignore.case=T)
th.doc.topics$identity.small_ <- ifelse(th.doc.topics$docs.have.we, "we.", "")
th.doc.topics$identity.small_ <- ifelse(th.doc.topics$docs.have.i, 
                                  paste0(th.doc.topics$identity.small_,"i."), 
                                  th.doc.topics$identity.small_)
th.doc.topics$identity.small_ <- ifelse(th.doc.topics$docs.have.ibm, 
                                  paste0(th.doc.topics$identity.small_,"ibm"), 
                                  th.doc.topics$identity.small_)
table(th.doc.topics$identity.small_)
th.doc.topics$identity.small_ <- factor(th.doc.topics$identity.small_)
# mid set
th.doc.topics$docs.have.we <- grepl(" we | our[ .,!-_]", documents$text, ignore.case=T)
th.doc.topics$docs.have.i <- grepl(" I | my ", documents$text, ignore.case=T)
th.doc.topics$docs.have.ibm <- grepl("ibm[ .,!-_']", documents$text, ignore.case=T)
th.doc.topics$identity.mid_ <- ifelse(th.doc.topics$docs.have.we, "we.", "")
th.doc.topics$identity.mid_ <- ifelse(th.doc.topics$docs.have.i, 
                                  paste0(th.doc.topics$identity.mid_,"i."), 
                                  th.doc.topics$identity.mid_)
th.doc.topics$identity.mid_ <- ifelse(th.doc.topics$docs.have.ibm, 
                                  paste0(th.doc.topics$identity.mid_,"ibm"), 
                                  th.doc.topics$identity.mid_)
table(th.doc.topics$identity.mid_)
th.doc.topics$identity.mid_ <- factor(th.doc.topics$identity.mid_)

# full set
th.doc.topics$docs.have.we <- grepl(" we | our[ .,!-_]| ours[ .,!-_]| us[ .,!-_]", documents$text, ignore.case=T)
th.doc.topics$docs.have.i <- grepl(" I | me[ .,!-_]| my | mine[ .,!-_]", documents$text, ignore.case=T)
th.doc.topics$docs.have.ibm <- grepl("ibm[ .,!-_']", documents$text, ignore.case=T)
th.doc.topics$identity.full_ <- ifelse(th.doc.topics$docs.have.we, "we.", "")
th.doc.topics$identity.full_ <- ifelse(th.doc.topics$docs.have.i, 
                                 paste0(th.doc.topics$identity.full_,"i."), 
                                 th.doc.topics$identity.full_)
th.doc.topics$identity.full_ <- ifelse(th.doc.topics$docs.have.ibm, 
                                 paste0(th.doc.topics$identity.full_,"ibm"), 
                                 th.doc.topics$identity.full_)
table(th.doc.topics$identity.full_)
th.doc.topics$identity.full_ <- factor(th.doc.topics$identity.full_)

# We'll use the MID set
th.doc.topics$identity_  <- th.doc.topics$identity.mid_ 

save(th.doc.topics, file="th_doc_topics.Rdata")
# load("th_doc_topics.Rdata")

# they're all related to each other
chisq.test(th.doc.topics$docs.have.ibm, th.doc.topics$docs.have.we)
chisq.test(th.doc.topics$docs.have.ibm, th.doc.topics$docs.have.i)
chisq.test(th.doc.topics$docs.have.we, th.doc.topics$docs.have.i)

# in a positive way
cor(cbind(th.doc.topics$docs.have.ibm, th.doc.topics$docs.have.we, th.doc.topics$docs.have.i))

# quick view at topics
prev.thresh <- 0.2
d.tps <- ifelse(doc.topics >= prev.thresh, 1, 0)
colnames(d.tps) <- paste0("topic_", 1:30)

ibm.we.i.rates <- data.frame(
  rate.ibm = colMeans(d.tps * th.doc.topics$docs.have.ibm) / colMeans(d.tps) / mean(th.doc.topics$docs.have.ibm),
  rate.we = colMeans(d.tps * th.doc.topics$docs.have.we) / colMeans(d.tps) / mean(th.doc.topics$docs.have.we),
  rate.i = colMeans(d.tps * th.doc.topics$docs.have.i) / colMeans(d.tps )/ mean(th.doc.topics$docs.have.ibm)
)
round(ibm.we.i.rates-1,2)
        
ibm.we.i.rates.values <- data.frame(
  rate.ibm = colMeans(d.tps * th.doc.topics$docs.have.ibm * (th.doc.topics$jam=="values")) / colMeans(d.tps * (th.doc.topics$jam=="values")) / mean(th.doc.topics$docs.have.ibm),
  rate.we = colMeans(d.tps * th.doc.topics$docs.have.we * (th.doc.topics$jam=="values")) / colMeans(d.tps * (th.doc.topics$jam=="values")) / mean(th.doc.topics$docs.have.we),
  rate.i = colMeans(d.tps * th.doc.topics$docs.have.i * (th.doc.topics$jam=="values")) / colMeans(d.tps * (th.doc.topics$jam=="values"))/ mean(th.doc.topics$docs.have.ibm)
)
round(ibm.we.i.rates.values-1,2)

ibm.we.i.rates.world <- data.frame(
  rate.ibm = colMeans(d.tps * th.doc.topics$docs.have.ibm * (th.doc.topics$jam=="world")) / colMeans(d.tps * (th.doc.topics$jam=="world")) / mean(th.doc.topics$docs.have.ibm),
  rate.we = colMeans(d.tps * th.doc.topics$docs.have.we * (th.doc.topics$jam=="world")) / colMeans(d.tps * (th.doc.topics$jam=="world")) / mean(th.doc.topics$docs.have.we),
  rate.i = colMeans(d.tps * th.doc.topics$docs.have.i * (th.doc.topics$jam=="world")) / colMeans(d.tps * (th.doc.topics$jam=="world"))/ mean(th.doc.topics$docs.have.ibm)
)
round(ibm.we.i.rates.world-1,2)

####
# center the focus and the log.length for the interaction
th.doc.topics$focus.cent <- th.doc.topics$focus - mean(th.doc.topics$focus)
th.doc.topics$log.length.cent <- th.doc.topics$log.length - mean(th.doc.topics$log.length)
th.doc.topics$focus.log.length.cent <- th.doc.topics$focus.cent * th.doc.topics$log.length.cent

th.doc.topics$focus.std <- th.doc.topics$focus.cent / sd(th.doc.topics$focus)
th.doc.topics$log.length.std <-th.doc.topics$log.length.cent/ sd(th.doc.topics$log.length)
th.doc.topics$focus.log.length.std <- th.doc.topics$focus.std * th.doc.topics$log.length.std

# center and standardized excitations
excites <- c("1", "2", "3", "5", "10", "20", "30", "60", "90", "120", "180")
gen.set <- th.doc.topics %>% filter(!missing.parent & !is.first.comment) 

for (excite in excites) {
  ex.name <- sprintf("exp.excite.%s", excite)
  new.name <- sprintf("exp.excite.%s.std", excite)
  q.name <- sprintf("exp.excite.%s.quan", excite)
  name.2nd <- sprintf("exc.%s.after.1st", excite)
  name.1st <- sprintf("exp.excite.%s.1st", excite)
  non.first.comments <- filter(th.doc.topics, generation>1 & !missing.parent) %>% select_(.dots=ex.name)
  non.missing.comments <- filter(th.doc.topics, generation>1 & !missing.parent) %>% select_(.dots=ex.name)
  th.doc.topics[, new.name] <- ifelse(th.doc.topics$generation>1 & !th.doc.topics$missing.parent,
  #th.doc.topics[, new.name] <- ifelse(!th.doc.topics$missing.parent,
    (th.doc.topics[, ex.name] - mean(non.first.comments[,1])) / sd(non.first.comments[,1]),
    0
  )
  exc.dist <- ecdf(gen.set[, ex.name])
  th.doc.topics[, q.name] <- exc.dist(th.doc.topics[, ex.name])
  th.doc.topics[, name.2nd] <- th.doc.topics[, ex.name] - th.doc.topics[, name.1st] 
  #old <- th.doc.topics[, ex.name]
  #th.doc.topics[, new.name] <- (old - mean(old)) / sd(old)
}

#####################
# MODEL FUNCTION    #
#####################

## cut points for topics
cutModels <- function(prev.thresh=NULL, controls, topic.interaction="", interaction.terms = c(), filter.set=NULL, do.hurdles=T) {
  if (is.null(prev.thresh)) {
    thread.dt.cuts <- select_(th.doc.topics, .dots=c("n.children", "responded", "jam", controls))
  } else {
    d.tps <- ifelse(doc.topics.unsmooth >= prev.thresh, 1, 0)
    colnames(d.tps) <- paste0("t", 1:30)
    thread.dt.cuts <- 
      cbind(select_(th.doc.topics, .dots=c("n.children", "responded", "jam", controls)),
            d.tps)
  }
  if (topic.interaction != "") {
    d.tps.inter <- th.doc.topics[,topic.interaction] * d.tps
    colnames(d.tps.inter) <- paste0("t-",topic.interaction,"-", 1:30)
    thread.dt.cuts <- cbind(thread.dt.cuts, d.tps.inter)
  }
  
  if (!is.null(filter.set)) {
    thread.dt.cuts <- thread.dt.cuts[filter.set,]
  }
  formula.text <- paste(c("responded ~ . ", interaction.terms), collapse = " + ")
  formula <- as.formula(formula.text)
  p.formula.text <- paste(c("n.children ~ . ", interaction.terms), collapse = " + ")
  p.formula <- as.formula(p.formula.text)
  # no more Overall
  # fit.cuts <- glm(formula,
  #             data=thread.dt.cuts,
  #             family="binomial")
  
  fit.world.cut <- glm(formula,
                   data=thread.dt.cuts %>% filter(jam=="world") %>% select(-jam,-n.children),
                   family="binomial")
  
  fit.value.cut <- glm(formula,
                   data=thread.dt.cuts %>% filter(jam=="values") %>% select(-jam, -n.children),
                   family="binomial")
  
  if (do.hurdles) {
    hurdle.world <- hurdle(p.formula, 
                        data=thread.dt.cuts %>% filter(jam=="world") %>% select(-jam, -responded),
                        dist = "negbin")
    hurdle.value <- hurdle(p.formula, 
                         data=thread.dt.cuts %>% filter(jam=="values") %>% select(-jam, -responded),
                         dist = "negbin")
  } else {
    hurdle.world <- NULL
    hurdle.value <- NULL
  }
  # the poisson versions of the same model have a ~30% lower log-likelihood 
  #hpos.world <- hurdle(p.formula, 
  #                       data=thread.dt.cuts %>% filter(jam=="world") %>% select(-jam, -responded),
  #                       dist = "poisson")
  #hpos.value <- hurdle(p.formula, 
  #                       data=thread.dt.cuts %>% filter(jam=="values") %>% select(-jam, -responded),
  #                       dist = "poisson")
  
  ret.val <- list(values=fit.value.cut, world=fit.world.cut, 
                  hurdle.world=hurdle.world, hurdle.val = hurdle.value)
  return(ret.val)
}




########
# Main Paper Set
########
focal.vars.nofoc <-  c("log.length.cent",
                       "identity.i_",
                       "is.first.comment", "missing.parent") 
focal.vars.cent <- c("focus.js.cent", "focus.cent", "log.length.cent", "focus.log.length.cent", 
                "identity.i_",
                "is.first.comment", "missing.parent") 
focal.vars.noid <- c("focus.js.cent", "focus.cent", "log.length.cent", "focus.log.length.cent",
                     "is.first.comment", "missing.parent") 
focal.vars.noexp <- c("focus.js.std", "focus.std", "log.length.std", "focus.log.length.std", 
                      "identity.i_",
                      "is.first.comment", "missing.parent")
focal.vars.nofoc <- c("is.first.comment", "missing.parent", 
                      "identity.i_")
focal.vars.notime <- c("focus.cent", "log.length.cent", "focus.log.length.cent",
                      "identity.i_")
basic.controls <- c("is.manager", "is.exec", "gender", 
           "u.s.time.", "continent2", "last.period", "forum")

th.doc.topics$inv.js.dist <- -th.doc.topics$focus.js.cent

# Nested models for World and Values
# 1	focal vars
values.1 <- cutModels(NULL, controls=c(focal.vars.noexp, "exp.excite.30.std"), do.hurdles=F)$values
# 2	focal and control
values.2 <- cutModels(NULL, controls=c(focal.vars.noexp, "exp.excite.30.std", basic.controls), do.hurdles=F)$values
# 3 focal and topics
values.3 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.30.std"), do.hurdles=F)$values
# 4 focal and controls and topics
values.std <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.30.std", basic.controls), do.hurdles=F)$values
values.non.stand <- cutModels(0.2, controls=c(focal.vars.cent, "exp.excite.30", basic.controls), do.hurdles=F)$values
values.non.stand.1st <- cutModels(0.2, controls=c(focal.vars.cent, "exp.excite.30.1st", basic.controls), do.hurdles=F)$values
values.non.stand.2 <- cutModels(0.2, controls=c(focal.vars.cent, "exp.excite.30", "simil.to.parent.cent", 
                                              basic.controls), do.hurdles=F)$values
values.mid <- cutModels(0.2, controls=c(focal.vars.noid, "identity.mid_", "exp.excite.30", basic.controls), do.hurdles=F)$values
values.full <- cutModels(0.2, controls=c(focal.vars.noid, "identity.full_", "exp.excite.30", basic.controls), do.hurdles=F)$values

# 1	focal vars
world.1 <- cutModels(NULL, controls=c(focal.vars.noexp, "exp.excite.20.std"), do.hurdles=F)$world
# 2	focal and control
world.2 <- cutModels(NULL, controls=c(focal.vars.noexp, "exp.excite.20.std", basic.controls), do.hurdles=F)$world
# 3 focal and topics
world.3 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.20.std"), do.hurdles=F)$world
# 4 focal and controls and topics
world.std <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.20.std", basic.controls), do.hurdles=F)$world

world.non.stand <- cutModels(0.2, controls=c(focal.vars.cent, "exp.excite.20", basic.controls), do.hurdles=F)$world
world.non.stand.1st <- cutModels(0.2, controls=c(focal.vars.cent, "exp.excite.20.1st", basic.controls), do.hurdles=F)$world
world.non.stand.2 <- cutModels(0.2, controls=c(focal.vars.cent, "exp.excite.20", "simil.to.parent.cent", 
                                             basic.controls), do.hurdles=F)$world

world.mid <- cutModels(0.2, controls=c(focal.vars.noid, "identity.mid_", "exp.excite.20", basic.controls), do.hurdles=F)$world
world.full <- cutModels(0.2, controls=c(focal.vars.noid, "identity.full_", "exp.excite.20", basic.controls), do.hurdles=F)$world
                       
library(car)
vif(world.std)
vif(values.std)

all.models <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.20.std", basic.controls))


# output the tables
  # a single table with the full, non standardized models

stargazer(values.non.stand, world.non.stand, column.labels=c("Values","World"),
          type='text', out=paste0(output.dir,"/overall_prob_response_odds_ratios.txt"),
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))

stargazer(values.non.stand, values.non.stand.1st, 
          world.non.stand,  world.non.stand.1st,
          column.labels=c("Values","World"),
          type='text', out=paste0(output.dir,"/overall_prob_response_odds_ratios.txt"),
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(values.1, values.2, values.3, values.4, 
          type='text', out="outputs/values_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(world.1, world.2, world.3, world.4,
          type='text', out="outputs/world_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)

stargazer(values.non.stand, values.mid, values.full,
          world.non.stand, world.mid, world.full,
          column.labels=c("Values","World"),
          type='text', out=paste0(output.dir,"/prob_response_identity_types.txt"),
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)


# Exploring different interpost focuses
cut.focus.kld <- cutModels(0.2, controls=c("focus.kld.cent", focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F)
cut.focus.cos <- cutModels(0.2, controls=c("focus.cos.cent", focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F)
cut.focus.js <- cutModels(0.2, controls=c("focus.js.cent", focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F)

cut.focus.kld.f <- cutModels(0.2, controls=c("focus.kld.cent", "focus.cent",
                                           focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F)
cut.focus.cos.f <- cutModels(0.2, controls=c("focus.cos.cent", "focus.cent", 
                                           focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F)
cut.focus.js.f <- cutModels(0.2, controls=c("focus.js.cent", "focus.cent",
                                          focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F)

cut.focus.kld.fi <- cutModels(0.2, controls=c("focus.kld.cent", "focus.cent",
                                             focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F,
                             interaction.terms="focus.kld.cent:focus.cent")
cut.focus.cos.fi <- cutModels(0.2, controls=c("focus.cos.cent", "focus.cent", 
                                             focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F,
                             interaction.terms="focus.cos.cent:focus.cent")
cut.focus.js.fi <- cutModels(0.2, controls=c("focus.js.cent", "focus.cent",
                                            focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F,
                            interaction.terms="focus.js.cent:focus.cent")

cut.just.focus <- cutModels(0.2, controls=c("focus.cent", focal.vars.nofoc, "exp.excite.20.std", basic.controls), do.hurdles=F)

cor(th.doc.topics$focus.cent, th.doc.topics$focus.cos.cent)

stargazer(cut.just.focus$values, 
          cut.focus.kld$values, cut.focus.kld.f$values, cut.focus.kld.fi$values,   
          cut.focus.js$values, cut.focus.js.f$values, cut.focus.js.fi$values,
          cut.focus.cos$values, cut.focus.cos.f$values, cut.focus.cos.fi$values,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)

stargazer(cut.just.focus$world, 
          cut.focus.kld$world, cut.focus.kld.f$world, cut.focus.kld.fi$world,  
          cut.focus.js$world, cut.focus.js.f$world, cut.focus.js.fi$world,
          cut.focus.cos$world, cut.focus.cos.f$world, cut.focus.cos.fi$world,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)



###
# ZERO order models
###
mod.focus.0 <- cutModels(NULL, controls=c("focus.cent", "log.length.cent", "focus.log.length.cent"), do.hurdles=F)
mod.inter.focus.0 <- cutModels(NULL, controls=c("focus.js.cent", "is.first.comment", "missing.parent"), do.hurdles=F)
mod.ident.0 <- cutModels(NULL, controls=c("identity_"), do.hurdles=F)
th.doc.topics$identity.i_ <- ifelse(th.doc.topics$identity_=="i.", " ",
                                    ifelse(th.doc.topics$identity_=="", "none", as.character(th.doc.topics$identity_)))
mod.ident.i.0 <- cutModels(NULL, controls=c("identity.i_"), do.hurdles=F)
mod.excite.0.val <- cutModels(NULL, controls=c("is.first.comment", "missing.parent",  "exp.excite.30.std"), do.hurdles=F)$values
mod.excite.0.wor <- cutModels(NULL, controls=c("is.first.comment", "missing.parent",  "exp.excite.20.std"), do.hurdles=F)$world
stargazer(mod.inter.focus.0$values, mod.focus.0$values, mod.ident.i.0$values, mod.excite.0.val, 
          mod.inter.focus.0$world, mod.focus.0$world,  mod.ident.i.0$world, mod.excite.0.wor,
          column.labels=c(rep("Values",4),rep("World",4)),
          type='text', out=paste0(output.dir,"/zero_order_models.txt"),
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))

# pairwise significance of identity vars

library(multcomp)
summary(glht(world.non.stand, mcp(identity_="Tukey")))
summary(glht(values.non.stand, mcp(identity_="Tukey")))

summary(glht(world.mid, mcp(identity.mid_="Tukey")))
summary(glht(values.mid, mcp(identity.mid_="Tukey")))
summary(glht(world.full, mcp(identity.full_="Tukey")))
summary(glht(values.full, mcp(identity.full_="Tukey")))

# comparing excitation decays
cuts.20.4.1 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.1.std", basic.controls), 
                         do.hurdles=F)
cuts.20.4.2 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.2.std", basic.controls),
                          do.hurdles=F)
cuts.20.4.3 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.3.std", basic.controls),
                          do.hurdles=F)
cuts.20.4.5 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.5.std", basic.controls),
                          do.hurdles=F)
cuts.20.4.10 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.10.std", basic.controls),
                           do.hurdles=F)
cuts.20.4.20 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.20.std", basic.controls),
                           do.hurdles=F)
cuts.20.4.30 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.30.std", basic.controls),
                           do.hurdles=F)
cuts.20.4.60 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.60.std", basic.controls),
                           do.hurdles=F)
cuts.20.4.90 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.90.std", basic.controls),
                          do.hurdles=F)
cuts.20.4.120 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.120.std", basic.controls),
                          do.hurdles=F)
cuts.20.4.180 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.180.std", basic.controls),
                          do.hurdles=F)
stargazer(cuts.20.4.1$values, cuts.20.4.2$values, cuts.20.4.3$values, 
          cuts.20.4.5$values, cuts.20.4.10$values, cuts.20.4.20$values, cuts.20.4.30$values, cuts.20.4.60$values,
          cuts.20.4.90$values, cuts.20.4.120$values, cuts.20.4.180$values,
          type='text', 
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(cuts.20.4.1$world, cuts.20.4.2$world, cuts.20.4.3$world, 
          cuts.20.4.5$world, cuts.20.4.10$world, cuts.20.4.20$world, cuts.20.4.30$world, cuts.20.4.60$world,
          cuts.20.4.90$world, cuts.20.4.120$world, cuts.20.4.180$world,
          type='text', 
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)

######
# output graph of coefficients
######
excites <- c("1", "2", "3", "5", "10", "20", "30", "60", "90", "120", "180")
jams <- c("values", "world")
emp.vec <- rep(NA, length(excites) * length(jams))
excite.df <- data.frame(
  jam = emp.vec,
  half.life = emp.vec,
  coef = emp.vec,
  se.vec = emp.vec,
  stringsAsFactors = F
)
i <- 0
for (excite in excites) {
  these.models <- get(sprintf("cuts.20.4.%s", excite)) 
  exp.name <- sprintf("exp.excite.%s.std", excite)
  for (jam in jams) {
    i <- i + 1
    this.model <- these.models[[jam]]
    coef.set <- summary(this.model)$coef
    excite.df[i,] <- c(jam, excite, coef.set[exp.name, 1:2])
  }
}  
excite.df$half.life <- as.numeric(excite.df$half.life)
excite.df$coef <- as.numeric(excite.df$coef)
excite.df$se.vec <- as.numeric(excite.df$se.vec)
gg <- ggplot(excite.df, aes(x=half.life, y=coef, color=jam)) +
  geom_errorbar(aes(ymin=coef - 1.96 * se.vec, ymax=coef + 1.96 * se.vec), 
                width=.3, position=pd, size=1) +
  geom_line(position=pd, size=1) +
  geom_point(position=pd, size=5) + 
  geom_hline(aes(yintercept=0), size=1.25, linetype="dashed") +
  theme(text=element_text(size=16)) +
  ylab("Standardized Coefficient Estimate") +
  xlab("Half Life of Excitation Variable")
ggsave(gg, file=paste0(output.dir, "/excitation_coef_by_halflife.png"))


# compare excite quans
# comparing excitation decays
cuts.20.4.1 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.1.quan", basic.controls), 
                         do.hurdles=F)
cuts.20.4.2 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.2.quan", basic.controls),
                         do.hurdles=F)
cuts.20.4.3 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.3.quan", basic.controls),
                         do.hurdles=F)
cuts.20.4.5 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.5.quan", basic.controls),
                         do.hurdles=F)
cuts.20.4.10 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.10.quan", basic.controls),
                          do.hurdles=F)
cuts.20.4.20 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.20.quan", basic.controls),
                          do.hurdles=F)
cuts.20.4.30 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.30.quan", basic.controls),
                          do.hurdles=F)
cuts.20.4.60 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.60.quan", basic.controls),
                          do.hurdles=F)
cuts.20.4.90 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.90.quan", basic.controls),
                          do.hurdles=F)
cuts.20.4.120 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.120.quan", basic.controls),
                           do.hurdles=F)
cuts.20.4.180 <- cutModels(0.2, controls=c(focal.vars.noexp, "exp.excite.180.quan", basic.controls),
                           do.hurdles=F)

# compare excitement half lifes between the first parent and earlier parents
gen.filter <- th.doc.topics$generation > 2 & ( th.doc.topics$generation > 3 | th.doc.topics$missing.ancestor==F)
cuts.20.4.1.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.1.1st", "exp.excite.1.2nd", basic.controls),
                         filter.set=gen.filter, do.hurdles=F)
cuts.20.4.2.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.2.1st", "exp.excite.2.2nd", basic.controls),
                         filter.set=gen.filter, do.hurdles=F)
cuts.20.4.3.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.3.1st", "exp.excite.3.2nd", basic.controls),
                          filter.set=gen.filter, do.hurdles=F)
cuts.20.4.5.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.5.1st", "exp.excite.5.2nd", basic.controls),
                          filter.set=gen.filter, do.hurdles=F)
cuts.20.4.10.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.10.1st", "exp.excite.10.2nd", basic.controls),
                          filter.set=gen.filter, do.hurdles=F)
cuts.20.4.20.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.20.1st", "exp.excite.20.2nd", basic.controls),
                          filter.set=gen.filter, do.hurdles=F)
cuts.20.4.30.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.30.1st", "exp.excite.30.2nd", basic.controls),
                          filter.set=gen.filter, do.hurdles=F)
cuts.20.4.60.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.60.1st", "exp.excite.60.2nd", basic.controls),
                          filter.set=gen.filter, do.hurdles=F)
cuts.20.4.90.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.90.1st", "exp.excite.90.2nd", basic.controls),
                              filter.set=gen.filter, do.hurdles=F)
cuts.20.4.120.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.120.1st", "exp.excite.120.2nd", basic.controls),
                              filter.set=gen.filter, do.hurdles=F)
cuts.20.4.180.1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.180.1st", "exp.excite.180.2nd", basic.controls),
                              filter.set=gen.filter, do.hurdles=F)



stargazer(cuts.20.4.1.1st$values, cuts.20.4.2.1st$values, cuts.20.4.3.1st$values, 
          cuts.20.4.5.1st$values, cuts.20.4.10.1st$values, cuts.20.4.20.1st$values, cuts.20.4.30.1st$values, cuts.20.4.60.1st$values,
          cuts.20.4.90.1st$values, cuts.20.4.120.1st$values, cuts.20.4.180.1st$values, 
          type='text', 
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(cuts.20.4.1.1st$world, cuts.20.4.2.1st$world, cuts.20.4.3.1st$world, 
          cuts.20.4.5.1st$world, cuts.20.4.10.1st$world, cuts.20.4.20.1st$world, cuts.20.4.30.1st$world, cuts.20.4.60.1st$world,
          cuts.20.4.90.1st$world, cuts.20.4.120.1st$world, cuts.20.4.180.1st$world, 
          type='text', 
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)


th.doc.topics$exc.30.1.2 <- th.doc.topics$exp.excite.30.1st + th.doc.topics$exp.excite.30.2nd
cut.30.just1st <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.30.1st",  basic.controls),
                            filter.set=gen.filter, do.hurdles=F)
cut.30.1.2 <- cutModels(0.2, controls=c(focal.vars.notime, "exc.30.1.2",  basic.controls),
                        filter.set=gen.filter, do.hurdles=F)
cut.30 <- cutModels(0.2, controls=c(focal.vars.notime, "exp.excite.30",  basic.controls),
                    filter.set=gen.filter, do.hurdles=F)
stargazer(cuts.20.4.30.1st$values, cut.30.just1st$values,
          cut.30.1.2$values, cut.30$values, 
          type='text', 
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(cuts.20.4.30.1st$world, cut.30.just1st$world,
          cut.30.1.2$world, cut.30$world, 
          type='text', 
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)


th.doc.topics %>% 
  filter(generation>1) %>%
  select(log.sec.since.parent, exp.excite.5, exp.excite.10, exp.excite.20) %>% cor()

# and the hurdle models
stargazer(cuts.20.1$values, cuts.20.2.int$values, 
          cuts.20.4$values, cuts.20.3.exp10$values, 
          type='text', # out="outputs/values_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(cuts.20.1$hurdle.world, cuts.20.2.int$hurdle.world, 
          cuts.20.4$hurdle.world, cuts.20.3.exp10$hurdle.world, 
          type='text', out="outputs/world_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)


stargazer(cuts.20.3$values, cuts.20.3.we$values, cuts.20.3$world, cuts.20.3.we$world,
          type='text', out="outputs/we_i_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)





#  INTERACTION BETWEEN FOCUS AND LENGTH   #
jam <- "values"
interact.term <- "log.length:focus"
var.int <- c("focus","log.length","log.length:focus")
foc.int.vcov <- 
  vcov(cuts.20.3.int[[jam]])[var.int, var.int]
foc.int <- summary(cuts.20.3.int[[jam]])$coef[var.int,1]

# at focus = 0.25, 0.5, 0.75, 1   effect of length
focus.list <- c(0.25, 0.5, 0.75, 1)
focus.val <- 0.5
this.leng.coef <- foc.int[interact.term] * focus.val + foc.int["log.length"]
this.leng.sd <- sqrt(
  foc.int.vcov[interact.term, interact.term] + 
    foc.int.vcov["log.length", "log.length"] + 
    2 * foc.int.vcov[interact.term, "log.length"])
this.leng.coef
this.leng.coef / this.leng.sd



# at length = 2, 4, 6, effect of focus
reg.set <- cuts.20.3.int
#reg.set <- cuts.20.2.int
title <- "Estimated Reponse Rate Odds Ratio of Focus 
at Different Comment Lengths
Controlling for Topic"

words <- c(10,20,40,80,160,320)
jams <- c("values","world")
emp.vec <- rep(NA, length(words) * length(jams))
focus.coefs <- data.frame(words = emp.vec,
                          log.leng = emp.vec,
                          coef = emp.vec,
                          sd = emp.vec,
                          jam = emp.vec)
i <- 0
for (word in words) {
  log.leng.val = log(word)
  for (jam in jams) {
    foc.int.vcov <- 
      vcov(reg.set[[jam]])[var.int, var.int]
    foc.int <- summary(reg.set[[jam]])$coef[var.int,1]
    this.focus.coef <- foc.int[interact.term] * log.leng.val + foc.int["focus"]
    this.focus.sd <- sqrt(
      foc.int.vcov[interact.term, interact.term] + 
        foc.int.vcov["focus", "focus"] + 
        2 * foc.int.vcov[interact.term, "focus"])
    i <- i + 1
    focus.coefs[i,1:4] <- c(word, log.leng.val, this.focus.coef, this.focus.sd)
    focus.coefs[i,5] <- jam
  }
  # this.focus.coef
  # this.focus.coef / this.focus.sd
}
pd <- position_dodge(width=0.8)
gg <- ggplot(data=focus.coefs, aes(x=words, color=jam)) +
  geom_line(aes(y=exp(coef)), size=1.5) + 
  geom_ribbon(aes(ymin = exp(coef - 1.96 * sd), ymax = exp(coef + 1.96 * sd), fill=jam), alpha=0.2) +
  geom_hline(aes(yintercept = 1)) +
  ylab("Estimated Response Rate Odds Ratio of Focus") + 
  xlab("Length of Post") +
  theme(text=element_text(size=16)) +
  ggtitle(title)
ggsave(gg, file = "focus odds ratio by comment length.png")

# how many docs is that?
th.doc.topics %>% filter(log.length>=4.4) %>% group_by(jam) %>% summarise(n=n())

summary(lm(adj.focus ~ log.length, data=th.doc.topics))
summary(lm(log.length ~ adj.focus + solo.topic, data=th.doc.topics))


#############################
# coefficients output
#############################
  

focalCoefPlot <- function(this.model, excite.half.life, title.lab) {
  coef <- summary(this.model)$coef
  inter.foc.var <- "focus.js.std"
  inter.foc.label <- "Interpost Topic Focus\n(JS Similarity)"
  inter.foc.effect <- coef[inter.foc.var, "Estimate"]
  inter.foc.se <- coef[inter.foc.var, "Std. Error"]
  
  foc.vars <- c("focus.std", "focus.log.length.std")
  foc.int.vcov <- vcov(this.model)[foc.vars,foc.vars]
  
  this.focus.se <- sqrt(
    foc.int.vcov[foc.vars[2], foc.vars[2]] + 
      foc.int.vcov[foc.vars[1], foc.vars[1]] + 
      2 * foc.int.vcov[foc.vars[1], foc.vars[2]])
  
  sd.log.len <- sd(th.doc.topics$log.length)
  mn.log.len <- mean(th.doc.topics$log.length)
  
  lengths <- c(10, 20, 50, 100, 200)
  log.lens.std <- (log(lengths) - mn.log.len) / sd.log.len
  
  
  focus.effect <- coef[foc.vars[1],"Estimate"] + coef[foc.vars[2],"Estimate"] * log.lens.std
  focus.se <- rep(this.focus.se, length(log.lens.std))
  focus.labels <- sprintf("Intrapost Focus %d Words", lengths)
  # order (with "I" excluded) will be:  
  #  no identity term; we/our only; IBM only; We/our and I/my; 
  #  I/my and IBM; We/our and IBM; We/our, I/my and IBM
  ident.vars <- c("identity.i_none", "identity.i_we.", "identity.i_ibm",
                  "identity.i_we.i.", "identity.i_i.ibm", "identity.i_we.ibm", 
                  "identity.i_we.i.ibm"   
                  )
  ident.labels <- c("Identity: None", "Identity: We/Our", "Identity: IBM",
                    "Identity: We/Our & I/My", "Identity: I/My & IBM", "Identity: We/Our & IBM", 
                    "Identity: We/Our & I/My & IBM"
                    )
  ident.effect <- coef[ident.vars, "Estimate"]
  ident.se <- coef[ident.vars, "Std. Error"]
  
  excite.var <- c(sprintf("exp.excite.%d.std", excite.half.life))
  excite.label <- c(sprintf("Excitation %d Min HL", excite.half.life))
  excite.effect <- coef[excite.var, "Estimate"]
  excite.se <- coef[excite.var, "Std. Error"]
  
  outputs <- data.frame(labels = c(inter.foc.label, focus.labels, ident.labels, excite.label),
              vars = c(inter.foc.effect, focus.effect, ident.effect, excite.effect),
              se = c(inter.foc.se, focus.se, ident.se, excite.se))
  # reverse the order
  outputs <- outputs[nrow(outputs):1,]
  outputs$factor.label <- factor(outputs$labels, levels = outputs$labels)
  pd <- position_dodge(0.2) 
  ggplot(outputs, aes(x=factor.label, y=vars)) +
    geom_errorbar(aes(ymin=vars - 1.96 * se, ymax=vars + 1.96 * se), 
                  width=.3, position=pd, size=1) +
    #geom_line(position=pd, size=1) +
    geom_point(position=pd, size=5) + 
    geom_hline(aes(yintercept=0), size=1.25, linetype="dashed") +
    ylim(-0.4, 0.42) +
    coord_flip() +
    theme(text=element_text(size=16)) +
    ylab("Standardized Coefficient Estimate") +
    ggtitle(title.lab)
}

ggsave(focalCoefPlot(values.std, 30, "Values Jam Coefficient Estimates\nwith 95% Confidence Intervals"), 
       file=paste0(output.dir,"/values_coefs.png"), width=7, height=7)
ggsave(focalCoefPlot(world.std, 20, "World Jam Coefficient Estimates\nwith 95% Confidence Intervals"), 
       file=paste0(output.dir,"/world_coefs.png"), width=7, height = 7)





##################
# Descriptive Statistics
##################
getStatsDf <- function(this.model, excite.half.life, this.jam, num.dig=3, exclude.excite.0s=F) {
  
  ident.vars <- c("identity.i_none", "identity.i_we.", "identity.i_ibm",
                  "identity.i_we.i.", "identity.i_i.ibm", "identity.i_we.ibm", 
                  "identity.i_we.i.ibm"   
  )
  ident.labels <- c("Identity: None", "Identity: We/Our", "Identity: IBM",
                    "Identity: We/Our & I/My", "Identity: I/My & IBM", "Identity: We/Our & IBM", 
                    "Identity: We/Our & I/My & IBM"
  )
  
  excite.var <- sprintf("exp.excite.%d", excite.half.life)
  excite.label <- sprintf("Excitation %d Min HL", excite.half.life)
  focus.var <- c("focus", "focus.js")
  focus.label <- c("Intrapost Topic Focus", "Interpost Topic Focus (JS Similarity)")
  
  mm <- data.frame(model.matrix(this.model))
  # add a depedendent variable
  mm$dep.var <- this.model$y
  dep.label <- "Share of Posts Which Elicit a Response"  
  
  # replace known zeros in excitation with NA
  if (exclude.excite.0s) mm[mm$is.first.commentTRUE==1 | mm$missing.parentTRUE==1, excite.var] <- NA
  mm$doc.length <- exp(mm$log.length.cent + mean(th.doc.topics$log.length))
  mm$is.non.manager <- 1 - rowSums(mm[,c("is.manager","is.exec")])
  mm$gender.female <- 1 - rowSums(mm[,c("gendermale","genderunknown")])
  mm$time.00.04 <- 1 - rowSums(mm[,c("u.s.time.04.08.GMT","u.s.time.08.12.GMT","u.s.time.12.16.GMT",
                                     "u.s.time.16.20.GMT","u.s.time.20.24.GMT")])
  mm$continent2Americas <- 1 - rowSums(mm[,c("continent2Africa","continent2Asia","continent2Europe","continent2Oceania","continent2unknown")])
  if ("forumforum.5" %in% colnames(mm)) {
    forum.var <- c("forumforum.2","forumforum.3","forumforum.4","forumforum.5","forumforum.6")
    forum.labels <- c("Forum 1", "Forum 2", "Forum 3", "Forum 4", "Forum 5", "Forum 6")
  } else {
    forum.var <- c("forumforum.2","forumforum.3","forumforum.4")
    forum.labels <- c("Forum 1", "Forum 2", "Forum 3", "Forum 4")
  }
  mm$forum1 <- 1 - rowSums(mm[,forum.var])
  mm$identity.i__ <- 1 - rowSums(mm[,ident.vars])
  mm$focus <- mm$focus.cent + mean(th.doc.topics$focus)
  mm$focus.js <- mm$focus.js.cent + mean(child.tps$focus.js)
  no.parent <- mm$missing.parentTRUE | mm$is.first.commentTRUE
  mm[no.parent, excite.var] <- NA
  mm[no.parent, "focus.js"] <- NA
  
  
  quantiles <- t(apply(mm, 2, function(x) {round(quantile(x, na.rm=T), num.dig)}))
  colnames(quantiles) <- c("Min", "Quant.25", "Median", "Quant.75", "Max")
  sum.stats <- data.frame(
    Mean = round(colMeans(mm, na.rm=T),num.dig),
    StDev = round(apply(mm, 2, sd, na.rm=T),num.dig),
    stringsAsFactors = F
  )
  sum.stats <- cbind(sum.stats, data.frame(quantiles, stringsAsFactors = F))
  rownames(sum.stats) <- colnames(mm)
  
  sum.stats$StDev <- ifelse(apply(mm, 2, function(x){sum(x==1, na.rm=T) == sum(x, na.rm=T)}), NA, sum.stats$StDev)
  
  
  control.vars <-
    c("is.non.manager","is.manager","is.exec",
      "gender.female","gendermale","genderunknown",
      "time.00.04","u.s.time.04.08.GMT","u.s.time.08.12.GMT","u.s.time.12.16.GMT","u.s.time.16.20.GMT","u.s.time.20.24.GMT",
      "continent2Americas", "continent2Africa","continent2Asia","continent2Europe","continent2Oceania","continent2unknown",
      "is.first.comment", "missing.parent",
      "last.period",
      "forum1", forum.var)
  
  control.labels <-
    c("Non Manager", "Manager", "Executive",
      "Female", "Male", "Unknown Gender",
      "00:00-04:00 GMT", "04:00-08:00 GMT", "08:00-12:00 GMT", "12:00-16:00 GMT", "16:00-20:00 GMT", "20:00-24:00 GMT",
      "Americas", "Africa", "Asia", "Europe", "Oceania", "Unknown Continent",
      "Top-level Post", "Share Posts Missing Parent", 
      "Last 8 Hours of Jam",
      forum.labels)
  

  
  
  var.set <- c("doc.length", "dep.var", focus.var, "identity.i__", 
               ident.vars, excite.var, control.vars)
  stats.df <- data.frame(
    Variable.Name = c("Post Length (words)", dep.label, focus.label, "Identity: I/My", ident.labels, excite.label, control.labels),
    sum.stats[var.set,],
    stringsAsFactors = F
  )
  ###
  # other variables
  ###
  doctopics.data <- filter(th.doc.topics, jam==this.jam)
    
    stats.prepend <- data.frame()
    # number of posters
    # posts per poster
    # % who post more than once
    posts.by.user <- doctopics.data %>% group_by(user) %>%
      summarise(posts=n()) 
    stats.df["num.posts",] <- c("Number of Posts", nrow(doctopics.data), rep(NA, 6))            
    stats.df["posters",] <- c("Number of Posters", nrow(posts.by.user), rep(NA, 6))            
    stats.df["posts.per.poster",] <- c("Posts per Poster", 
                                       round(mean(posts.by.user$posts), num.dig),
                                       round(sd(posts.by.user$posts), num.dig),  
                                       round(quantile(posts.by.user$posts), num.dig))
    stats.df["multi.posters",] <- c("Precent Posters Posting More Than Once", 
                                    round(sum(posts.by.user$posts > 1) / nrow(posts.by.user), num.dig), 
                                    rep(NA,6))
  
    # thread length
    #   count threads from their terminal leaves 
    aa <- doctopics.data %>% 
      # get all terminal posts
      filter((!id %in% unique(th.doc.topics$parent))) 
    # and generation becomes our guide  
    stats.df["thread.length",] <- c("Length of Thread", 
                                    round(mean(aa$generation),num.dig), 
                                    round(sd(aa$generation), num.dig), 
                                    round(quantile(aa$generation)),num.dig)
    
    stats.df["num.threads",] <- c("Number of Threads Length>1", sum(aa$generation>1), rep(NA,6))
    
    # missing parents
    #stats.df["missing.parent",] <- c("Share Posts Missing Parent", 
    #                                round(sum(doctopics.data$missing.parent)/nrow(doctopics.data), num.dig), 
    #                                rep(NA,6))
    
    # duration between posts
    sec.since.par <- 
      doctopics.data %>% filter(!is.na(sec.since.parent)) %>%
      mutate(rev.min = ifelse(sec.since.parent<0, 0, sec.since.parent) / 60) %>%
      select(rev.min)
    stats.df["duration",] <- c("Duration Between Posts (minutes)", 
                               round(mean(sec.since.par$rev.min),num.dig), 
                               round(sd(sec.since.par$rev.min),num.dig), 
                               round(quantile(sec.since.par$rev.min),num.dig))
  
  
  df.len <- nrow(stats.df)    
  return(stats.df[c((df.len - 6):df.len, 1:(df.len - 7)),])
}


library(ReporteRs)
library(magrittr)

val.stats <- getStatsDf(values.non.stand, 30, "values", 2)
# val.stats <- getStatsDf(values.non.stand, 30, "values", 2, T)
world.stats <- getStatsDf(world.non.stand, 20, "world", 2)
# world.stats <- getStatsDf(world.non.stand, 20, "world", 2, T)
all.thread.stats <- rbind(
  c("VALUES JAM", rep(NA, 7)),
  val.stats[1:8,],
  c("WORLD JAM", rep(NA, 7)),
  world.stats[1:8,] 
)

val.focal <- val.stats[9:20, ] 
val.focal[c(1,4:11), 3:8] <- NA
world.focal <- world.stats[9:20, ] 
world.focal[c(1,4:11), 3:8] <- NA
all.focal <- rbind(
  c("VALUES JAM", rep(NA, 7)),
  val.focal,
  c("WORLD JAM", rep(NA, 7)),
  world.focal
)

all.controls <- right_join(val.stats[21:nrow(val.stats), 1:2], 
                           world.stats[21:nrow(world.stats), 1:2], 
                           by="Variable.Name")
names(all.controls) <- c("Variable", "Share Values Jam", "Share World Jam")

# topics table 
jams <- c("values", "world")
num.dig <- 3
topic.stats <- NULL
for (this.jam in jams) {
  filter.set <- documents$jam==this.jam
  t.prev <- round(colSums(doc.topics.unnormal[filter.set,]) / sum(doc.topics.unnormal[filter.set,]), num.dig)  
  if (is.null(topic.stats)) {topic.stats <- data.frame(a=t.prev)} else {topic.stats <- cbind(topic.stats, t.prev)}
  t.thresh <- round(colSums(doc.topics.unsmooth[filter.set,] >= prev.thresh) / nrow(doc.topics.unsmooth[filter.set,]), num.dig)
  topic.stats <- cbind(topic.stats, t.thresh)
}
names(topic.stats) <- c("Values\nPrevalence", 
                        "Values Share\nOver 0.2",
                        "World\nPrevalence", 
                        "World Share\nOver 0.2")
row.names(topic.stats) <- sprintf("Topic %s", 1:30)

library(Hmisc)


corWithStars <- function(mm, num.dig=3, col.names=NULL) {
  correls <- round(rcorr(mm)$r, num.dig)
  correls.p <- rcorr(mm)$P 
  
  ## define notions for significance levels; spacing is important.
  mystars <- ifelse(correls.p < .001, "***", ifelse(correls.p < .01, "** ", ifelse(correls.p < .05, "* ", " ")))
  
  ## build a new matrix that includes the correlations with their apropriate stars 
  Rnew <- matrix(paste0(correls, mystars), ncol=ncol(correls)) 
  diag(Rnew) <- paste(diag(correls), " ", sep="")
  Rnew[upper.tri(Rnew, diag = F)] <- ""
  if (is.null(col.names)) col.names <- colnames(mm)
  colnames(Rnew) <- col.names
  rownames(Rnew) <- col.names
  Rnew <- as.data.frame(Rnew[,-ncol(Rnew)]) 
  return(Rnew)
}
# focal correlations
getVarList <- function(excite.half.life) {
  return(list(
    vars = c("focus", "focus.js",  
                  sprintf("exp.excite.%d", excite.half.life),
                  "identity.i_none", "identity.i_ ", "identity.i_we.", "identity.i_ibm",
                  "identity.i_we.i.", "identity.i_i.ibm", "identity.i_we.ibm", 
                  "identity.i_we.i.ibm"   
    ),
    labels = c("Intrapost Topic Focus", "Interpost Topic Focus (JS Similarity)",
                    sprintf("Excitation %d Min HL", excite.half.life),
                    "Identity: None", "Identity: I/My", "Identity: We/Our", "Identity: IBM",
                    "Identity: We/Our & I/My", "Identity: I/My & IBM", "Identity: We/Our & IBM", 
                    "Identity: We/Our & I/My & IBM"
    )))
}  
val.vars <- getVarList(30)
val.parents <- 
  filter(th.doc.topics, generation>1 & jam=="values" & missing.parent!=1) %>% 
  select(focus, focus.js, exp.excite.30, identity.i_) 
val.mm <- model.matrix(~0 + ., val.parents)[, val.vars$vars]
val.cor <- corWithStars(val.mm, num.dig, val.vars$labels)

wol.vars <- getVarList(20)
world.parents <- 
  filter(th.doc.topics, generation>1 & jam=="world"& missing.parent!=1) %>% 
  select(focus, focus.js, exp.excite.20, identity.i_)
world.mm <- model.matrix(~0 + ., world.parents)[, wol.vars$vars]
world.cor <- corWithStars(world.mm, num.dig, wol.vars$labels)

focal.corr <- rbind(
  list("VALUES JAM", NA, NA, NA, NA),
  list("Intrapost Focus-Excitation Correlation", val.foc.cor, NA, NA, NA),
  list("Intrapost-Interpost Focus Correlation", val.foc.foc, NA, NA, NA),
  th.doc.topics %>% filter(jam=="values") %>%
    group_by(identity.i_) %>%
    summarise(mean.excite = mean(exp.excite.30),
              sd.excite = sd(exp.excite.30),
              mean.focus=mean(focus),
              sd.focus=sd(focus)) %>%
    mutate(identity.i_ = paste0("identity.i_", identity.i_)),
  list("WORLD JAM", NA, NA, NA, NA),
  list("Focus-Excitation Correlation", world.foc.cor, NA, NA, NA),
  list("Intrapost-Interpost Focus Correlation", world.foc.foc, NA, NA, NA),
  th.doc.topics %>% filter(jam=="world") %>%
    group_by(identity.i_) %>%
    summarise(mean.excite = mean(exp.excite.20),
              sd.excite = sd(exp.excite.20),
              mean.focus=mean(focus),
              sd.focus=sd(focus)) %>%
    mutate(identity.i_ = paste0("identity.i_", identity.i_))
)
focal.corr[,2:5] <- round(focal.corr[,2:5], num.dig)

focal.corr.df <- data.frame(focal.corr, stringsAsFactors = F)
focal.corr.df[is.na(focal.corr.df)] <- ""
names(focal.corr.df) <- c("", "Mean Excitation", "SD Excitation", "Mean Focus", "SD Focus")


docx() %>% 
  addTitle("Thread Characteristics by Jam", level=2) %>%
  addFlexTable(all.thread.stats %>%
                  FlexTable( header.text.props = textProperties(font.weight="bold", font.size = 8 ),
                             body.text.props = textProperties( font.size = 8 ),
                             add.rownames = FALSE ) %>%
                  setFlexTableWidths(widths=c(1.5,rep(0.6,7)))
                )  %>%
  addPageBreak() %>%
  addTitle("Focal Variables Summary Statistics, by Jam", level=2) %>%
  addFlexTable( all.focal %>%
                  FlexTable( header.text.props = textProperties(font.weight="bold", font.size = 8 ),
                             body.text.props = textProperties( font.size = 8 ),
                             add.rownames = FALSE ) %>%
                  setFlexTableWidths(widths=c(1.5,rep(0.6,7)))
                )  %>%
  addPageBreak() %>%
  addTitle("Control Variables Summary Statistics, by Jam", level=2) %>%
  addFlexTable( all.controls %>%
                  FlexTable( header.text.props = textProperties(font.weight="bold", font.size = 8 ),
                             body.text.props = textProperties( font.size = 8 ),
                             add.rownames = FALSE ) %>%
                  setFlexTableWidths(widths=c(1.2, 0.8, 0.8))
              ) %>%
  addTitle("Topic Model Summary Statistics, by Jam", level=2) %>%
  addFlexTable( topic.stats %>%
                  FlexTable( header.text.props = textProperties(font.weight="bold", font.size = 8 ),
                             body.text.props = textProperties( font.size = 8 ),
                             add.rownames = T ) %>%
                  setFlexTableWidths(widths=c(0.8, 0.8, 0.8, 0.8, 0.8))
  ) %>%
#  addTitle("Focal Variable Relationships by Jam", level=2) %>%
#  addFlexTable( focal.corr.df %>%
#                  FlexTable( header.text.props = textProperties(font.weight="bold", font.size = 8 ),
#                             body.text.props = textProperties( font.size = 8 ),
#                             add.rownames = F ) %>%
#                  setFlexTableWidths(widths=c(1, 0.8, 0.8, 0.8, 0.8))
#  ) %>%
  addTitle("Focal Variable Correlation Values Jam", level=2) %>%
  addFlexTable( val.cor %>%
                  FlexTable( header.text.props = textProperties(font.weight="bold", font.size = 6 ),
                             body.text.props = textProperties( font.size = 6 ),
                             add.rownames = T ) %>%
                  setFlexTableWidths(widths=c(1, rep(0.6, 10)))
  ) %>%
  addTitle("Focal Variable Correlation World Jam", level=2) %>%
  addFlexTable( world.cor %>%
                  FlexTable( header.text.props = textProperties(font.weight="bold", font.size = 6 ),
                             body.text.props = textProperties( font.size = 6 ),
                             add.rownames = T ) %>%
                  setFlexTableWidths(widths=c(1, rep(0.6, 10)))
  ) %>%
  writeDoc( file = paste0(output.dir, "/summary stats.docx" ))
  






  








# values: c("is.first.comment", "log.sec.since.parent")
  # length and length2 as well, but we expect that
cor(model.matrix(cuts.20.3$values))[, c("is.first.commentTRUE", "log.sec.since.parent")]
  # they correlate with each other.  
  # so, this is just the fact that they zero-inflation control for each other.

# world: c("is.first.comment, missing.parent, log.sec.since.parent, forum
  # length and length2 as well, but we expect that
abs(cor(model.matrix(cuts.20.3$world))[, c("is.first.commentTRUE", "log.sec.since.parent",
                                        "missing.parentTRUE", 
                                        "forumforum 2", "forumforum 3", "forumforum 4", 
                                        "forumforum 5", "forumforum 6")]) > 0.3
  # same as values, and the fact that some topics in world are forum-specific  



# understanding the timing square function results
val.int <- function(x) {-0.02*x^2 + 0.155 * x}
exp(0.155/.04) # 48 seconds, maximum

world.int <- function(x){0.011 * x^2 - 0.294 * x}
exp(.294 / 0.022) # 176 hours, minimum

# combined.forums <- c(cuts.15, cuts.20, cuts.25) 

stargazer(cuts.15, cuts.20, cuts.25, 
          type="text", 
          column.labels=names(c(cuts.15, cuts.20, cuts.25)))

names <- paste0(sapply(c("overall-", "value-", "world-"), FUN=rep, times=3), c(0.15, 0.20, 0.25))
combined <- c(cuts.15, cuts.20, cuts.25) 
stargazer(combined[c(2,5,8,11,14,17,3,6,9,12,15,18)], 
          type="text", 
          column.labels=names)

combined <- c(cuts.20, cuts.20.forum)
stargazer(combined[c(2,5,3,6)], 
          type="text", 
          column.labels=names(combined[c(2,5,3,6)]))

combined <- c(cuts.20, cuts.20.forum, cuts.20.times)
stargazer(combined[c(2,5,8,3,6,9)], 
          type="text", 
          column.labels=names(combined[c(2,5,8,3,6,9)]))

combined <- c(cuts.20, cuts.20.forum, cuts.20.times.4)
stargazer(combined[c(2,5,8,3,6,9)], 
          type="text", 
          column.labels=names(combined[c(2,5,8,3,6,9)]))

#######################################
# WHICH TOPIC RESPONDS ANALYSIS #
#######################################



transitionMatrixPlot <- function(th.doc.topics, doc.topics.unsmooth, 
                                 topic.prev, jam.list=c("values","world"), 
                                 color, title,
                                 controls=c(), interaction.terms = c()) {  
  parent.dtp <- data.frame(doc.topics.unsmooth > topic.prev)
  names(parent.dtp) <- paste0("parent_topic_", 1:30)
  coef.names <- paste0("parent_topic_", 1:30, "TRUE")
  
  parent.dtp$id <- th.doc.topics$id
  
  parent.child.topic.coef <- data.frame(matrix(NA,30,30))
  parent.child.topic.sig <- data.frame(matrix(NA,30,30))
  for (focal.topic in 1:30) {
    names(parent.child.topic.coef)[focal.topic] <- paste0("topic_", focal.topic)
    names(parent.child.topic.sig)[focal.topic] <- paste0("topic_", focal.topic)
    top.name <- paste0("parent_topic_", focal.topic)
    th.doc.topics$focal.topic <- parent.dtp[,top.name]
  
    glm.df <- th.doc.topics %>%  
      filter(jam %in% jam.list) %>%
      select_(.dots=c("parent", "focal.topic", controls)) %>%
      #select(parent, focal.topic) %>%
      inner_join(parent.dtp, by=c("parent"="id")) %>%
      select(-parent)

    formula.text <- paste(c("focal.topic ~ . ", interaction.terms), collapse = " + ")
    formula <- as.formula(formula.text)
    fit <- glm(formula, data=glm.df)
    parent.child.topic.sig[, focal.topic] <- summary(fit)$coef[coef.names, 3]
    parent.child.topic.coef[, focal.topic] <- summary(fit)$coef[coef.names, 1]
  }
  row.names(parent.child.topic.sig) <- paste0("topic_", 1:30)
  row.names(parent.child.topic.coef) <- paste0("topic_", 1:30)
  dat <- melt(as.matrix(parent.child.topic.sig))
  coefs <- melt(as.matrix(parent.child.topic.coef))
  dat$odds.ratio <- exp(coefs$value)
  gg <- ggplot(dat, aes(as.factor(Var2), Var1, group=Var1)) +
    geom_tile(aes(fill = value)) + 
    geom_text(aes(fill = dat$odds.ratio, 
                  label = round(dat$odds.ratio, 2)),
                  size=3) +
    scale_fill_gradientn(colors=c("white","white", color, color),
                         values=rescale(c(min(dat$value), 1, 4, max(dat$value))),
                         guide = FALSE) +
    xlab("Child Topic") +
    ylab("Parent Topic") +
    ggtitle(title) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(list(chart=gg, matrix=as.matrix(parent.child.topic.coef)))
}
val.trans.mat <- transitionMatrixPlot(th.doc.topics, doc.topics.unsmooth, 
                     topic.prev=0.2, jam="values", color="red",
                     title="Values Jam Estimated Topic Transition Matrix: 
                     Odds Ratio of Child Topic in Response Given Parent Topic in Posting")
world.trans.mat <- transitionMatrixPlot(th.doc.topics, doc.topics.unsmooth, 
                     topic.prev=0.2, jam="world", color="green",
                     title="World Jam Estimated Topic Transition Matrix: 
                     Odds Ratio of Child Topic in Response Given Parent Topic in Posting")
ggsave("outputs/estimated topic transitions - values.png", 
       plot=val.trans.mat$chart, width=10, height=8)
ggsave("outputs/estimated topic transitions - world.png", 
       world.trans.mat$chart, width=10, height=8)

# With Controls
controls <- c("is.manager", "is.exec", "gender", 
           "u.s.time.", "continent2", "last.period",
           "log.length", "focus", #"solo.topic",
           "is.first.comment","missing.parent","exp.excite.10","log.sec.since.parent", 
           "forum")
interaction.terms <-  c("log.length : focus")

val.trans.mat.c <- transitionMatrixPlot(th.doc.topics, doc.topics.unsmooth, 
                                      topic.prev=0.2, jam="values", color="red",
                                      title="Values Jam Estimated Topic Transition Matrix: 
                                      Odds Ratio of Child Topic in Response Given Parent Topic in Posting
                                      with Controls",
                                      controls=controls, interaction.terms = interaction.terms)
world.trans.mat.c <- transitionMatrixPlot(th.doc.topics, doc.topics.unsmooth, 
                                        topic.prev=0.2, jam="world", color="green",
                                        title="World Jam Estimated Topic Transition Matrix: 
                                        Odds Ratio of Child Topic in Response Given Parent Topic in Posting
                                        with Controls",
                                        controls=controls, interaction.terms = interaction.terms)
ggsave("outputs/estimated topic transitions - values controls.png", 
       plot=val.trans.mat.c$chart, width=10, height=8)
ggsave("outputs/estimated topic transitions - world controls.png", 
       world.trans.mat.c$chart, width=10, height=8)

# Transition maps? 
topic.prve <- 0.2
topic.base.prop <- colSums(doc.topics.unsmooth>=topic.prev) / nrow(doc.topics.unnormal)
mm <- exp(val.trans.mat.c$matrix) 
a <- t(t(mm) * topic.base.prop)
a %*% a

ggsave("outputs/test-1.png", 
       plot=val.trans.mat, width=10, height=8)
#######################################
# CONVERSATION Temporal focus #
#######################################


regressOnTopics <- function(window.len, min.conv.size, 
                            jams=c("values", "world"),
                            prev.thresh=0) {
  start.end.times <- threaded.docs %>% 
    group_by(jam) %>% 
    summarise(start=min(Timestamp), end=max(Timestamp))
  by.str <- paste0(window.len, " hour")
  val.times <- seq(trunc(start.end.times$start[1], "hour"), 
                   trunc(start.end.times$end[1], "hour"), 
                   by=by.str)
  world.times <- seq(trunc(start.end.times$start[2], "hour"), 
                   trunc(start.end.times$end[2], "hour"), 
                   by=by.str)
  time.list <- c(val.times, world.times)
  time.list <- data.frame(window.id=1:length(time.list), 
                          new.window=paste(strftime(time.list,"%Y-%m-%d"),
                                           trunc(as.numeric(strftime(time.list,"%H"))/window.len)))
  
  threaded.docs$new.window <- newDateWindows(threaded.docs, window.len)
  threaded.docs$words <- rowSums(unnormal.doc.topics)
  # create a data set with each row a time window for a particular root (conversation)
  by.root.window <- threaded.docs %>%
    filter(!root.id %in% "-missing", jam %in% jams) %>%
    group_by(root.id, new.window) %>% 
    # count comments, total words, number of users in the window
    summarise(comments=n(), words = sum(words), users=n_distinct(user)) %>%
    # and include the overall totals for the conversation
    inner_join(group_by(threaded.docs, root.id) %>% 
                 summarize(total.comments=n(), total.words=sum(words)) %>% 
                 filter(total.comments >= min.conv.size))
  by.root.window <- left_join(by.root.window,
                              by.root.window %>%
                                group_by(root.id) %>%
                                summarize(conv.time.focus=sum(comments^2)/(sum(comments)^2))
  )
  by.root.window <- by.root.window %>%
    left_join(time.list)
  # max(by.root.window$comments)  
  # qplot(by.root.window$comments, binwidth=1)
  
  root.window.topics <- 
    combineConvDT(threaded.docs, doc.topics.unnormal, 
                  grouping=c("jam", "root.id", "new.window")) %>%
    right_join(by.root.window)
  
  root.window.topics[,paste0("t", 1:30,".prev")] <- 
    root.window.topics[,paste0("X", 1:30)] / root.window.topics$words

  # TODO-- this should be an indicator of whether comments mention the topic
   # right now it is the window's overall prevalence
  if (prev.thresh>0) {
    root.window.topics[,paste0("t", 1:30,".prev")] <-
      root.window.topics[,paste0("t", 1:30,".prev")] >= prev.thresh
  }
  
  # regress the number of comments in any given window on the prior comments and topic distribution
  lm.df <- root.window.topics %>% ungroup() %>%
    select(comments, matches(".prev"))
  fit.1 <- lm(comments ~ ., data=lm.df)
  # summary(fit.1)
  
  lm.df <- root.window.topics %>% ungroup() %>%
    mutate(time.focus.w.comments = conv.time.focus * comments) %>%
    select(time.focus.w.comments, matches(".prev"))
  fit.2 <- lm(time.focus.w.comments ~ ., data=lm.df)
  # summary(fit.2)
  
  # how about what was going on in the prior period?
  root.window.topics$prior.window.id <- root.window.topics$window.id - 1
  
  merged.windows <- 
    root.window.topics %>% ungroup() %>%
    select_(.dots=c("root.id", "comments", "conv.time.focus", "window.id",
                       paste0("t", 1:30,".prev")))
  names(merged.windows) <- paste0("prior.", names(merged.windows))
  merged.windows <- right_join(merged.windows, root.window.topics, 
                               by=c("prior.window.id", "prior.root.id"="root.id"))
  merged.windows[is.na(merged.windows)] <- 0 
  
  
  lm.df <- merged.windows %>% ungroup() %>%
    select(comments, prior.comments, matches("t[0-9]."), -matches("prior.t"))
  fit.3 <- lm(comments ~ ., data=lm.df)
  # summary(fit.3)
  
  lm.df <- merged.windows %>% ungroup() %>%
    mutate(time.focus.w.comments = conv.time.focus * comments,
           prior.fw.comments = conv.time.focus * prior.comments) %>%
    select(time.focus.w.comments, prior.fw.comments, matches("t[0-9]."), -matches("prior.t"))
  fit.4 <- lm(time.focus.w.comments ~ ., data=lm.df)
  # summary(fit.4)
  
  stargazer(fit.1, fit.3, fit.2, fit.4,
            type="text", 
            omit.stat=c("F","ser"))
}

# let's look at armonkization
by.root.window %>% filter(root.id == threaded.docs[29, "id"])

jams <- c("values", "world")
# jams <- "values"
window.len <- 1
min.conv.size <- 3

regressOnTopics(1, 5, jams)
regressOnTopics(1, 5, jams="world")
regressOnTopics(1, 5, jams="values")

regressOnTopics(1, 10, jams)
regressOnTopics(1, 10, jams="world")
regressOnTopics(1, 10, jams="values")

regressOnTopics(4, 5, jams)
regressOnTopics(4, 5, jams="world")
regressOnTopics(4, 5, jams="values")
regressOnTopics(2, 5, jams="values")

regressOnTopics(1, 5, jams, prev.thresh=0.15)
regressOnTopics(1, 5, jams="world", prev.thresh=0.15)
regressOnTopics(1, 5, jams="values", prev.thresh=0.15)

####################################
# response to current thread state #
####################################
# we'll need a lookup table for mapping ids to indexes
id.lookup <- 1:nrow(threaded.docs)
names(id.lookup) <- threaded.docs$id

# for each comment which has a parent, we want the state of its thread at the time of posting
current.thread.topics <- matrix(0, ncol=ncol(unnormal.doc.topics), nrow=nrow(threaded.docs))
parent.topics <- matrix(0, ncol=ncol(unnormal.doc.topics), nrow=nrow(threaded.docs))
for (i in 1:nrow(threaded.docs)) {
  current.thread <- rep(0, ncol(unnormal.doc.topics))
  if (!(threaded.docs[i, "generation"] == 1)) {
    # grab the ancestors list
    is.first <- TRUE
    for (id in threaded.docs[i, "ancestors"][[1]]) {
      if (id != "root" && substr(id, nchar(id)-6, nchar(id))!="missing") {
        indx <- id.lookup[id]
        current.thread <- current.thread + unnormal.doc.topics[indx,]
        # if this is the first ancestor (parent), store the topic dist
        if (is.first) {
          parent.topics[i,] <- unnormal.doc.topics[indx,]
          is.first <- FALSE
        }
      }
    }
  }
  current.thread.topics[i,] <- current.thread
}
# confirm that we've histories for every comment with a history
sum(rowSums(current.thread.topics)>0)
sum(rowSums(parent.topics)>0)
sum(threaded.docs$generation>1) - 
  sum(threaded.docs$generation==2 & threaded.docs$missing.ancestor)

# how many zero topics are there? (ignore the 18,357 that have no parents)
table(rowSums(current.thread.topics==0))

# how do comments compare to the prevelance of what comes before them?
# their parents and the thread preceding them

unnormal.doc.topics


# we'd expect the shaping impact to be greater with a longer history
#  length can be number of comments OR amount of words


# how many pairs of top two topics are there?

cut.thresh <- 0.3
min.tokens <- 5
df <- doc.topics >= cut.thresh & unnormal.doc.topics >= min.tokens 
df2 <- df * doc.topics
top.topics <- matrix(NA, ncol=2, nrow=nrow(df2))
for (i in 1:nrow(df2)) {
  a <- which(df2[i,] > 0)
  if (length(a)==1) {
    top.topics[i,1] <- a  
  } 
  if (length(a)==2) {
    top.topics[i,1] <- min(a)
    top.topics[i,2] <- max(a)
  }
}
max(rowSums(df2))
unique(top.topics)
colnames(top.topics) <- c("tp1", "tp2")
top.topic.cnts <- data.frame(top.topics) %>% group_by(tp1, tp2) %>% summarize(n=n()) %>% ungroup() %>% arrange(desc(n))







##################################
# conversation topic prevelences #
##################################

# do conversations over a certain size exhibit more of some topics than the corpus as a whole
conv.topics <- combineConvDT(threaded.docs, doc.topics.unnormal)
graphConvTop <- function(n.cut, jams = c("world","values")) {
  total.conv <- conv.topics %>% filter(n>n.cut, jam %in% jams) %>%
    ungroup %>% select(-root.id, -jam) %>%
    summarise_each(funs(sum))
  totals <- conv.topics %>% filter(n>0, jam %in% jams) %>%
    ungroup %>% select(-root.id, -jam) %>%
    summarise_each(funs(sum))
  
  df <- data.frame( 
    total.conv = t(total.conv[,1:30] / sum(total.conv[,1:30])),
    totals = t(totals[,1:30] / sum(totals[,1:30]))
  )
  ggplot(data=df, aes(x=totals, y=total.conv)) + 
    geom_text(label=1:30) +
    geom_abline(aes(intercept=0, slope=1))
}

graphConvTop(5)
graphConvTop(10)

graphConvTop(5, "values")
graphConvTop(10, "values")

graphConvTop(5, "world")
graphConvTop(10, "world")

# co-occurence within conversations, compared to documents
documentCoOccur <- function(jams, min.v=-1, max.v=1) {
  filter <- threaded.docs$jam %in% jams 
  doc.corr.mat <- topic.co.occur(doc.topics.1=doc.topics[filter,],
                                   unnormal.doc.topics = doc.topics.unnormal[filter,], 
                                   correlationMinProportion=0.2)
  cor.mat <- doc.corr.mat$corr.matrix
  rownames(cor.mat) <- paste0("topic_", 1:30)
  colnames(cor.mat) <- paste0("topic_", 1:30)
  corr.heatmap(cor.mat, min=min.v, max=max.v) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
}

documentCoOccur(c("values","world"))

documentCoOccur("world", NULL, NULL)

ggsave("outputs/topic doc co-occur - values-n-n.png", 
       plot=documentCoOccur("values", NULL, NULL), width=10, height=8)
ggsave("outputs/topic doc co-occur - values-2-2.png", 
       plot=documentCoOccur("values", -2, 2), width=10, height=8)
ggsave("outputs/topic doc co-occur - values-1-1.png", 
       plot=documentCoOccur("values", -1, 1), width=10, height=8)
ggsave("outputs/topic doc co-occur - world-n-n.png", 
       plot=documentCoOccur("world", NULL, NULL), width=10, height=8)
ggsave("outputs/topic doc co-occur - world-2-2.png", 
       plot=documentCoOccur("world", -2, 2), width=10, height=8)
ggsave("outputs/topic doc co-occur - world-1-1.png", 
       plot=documentCoOccur("world", -1, 1), width=10, height=8)

#What we'd prefer is to count is if at least one document
#    within the conversation was above the threshold
conversationCooccurHeat <- function(
                          correlationMinProportion = 0.20,
                          correlationMinTokens = 8,
                          min.convo.len = 5,
                          jams = c("values","world")) {

  doc.topics.1 <- mallet.doc.topics(topic.model, smoothed=F, normalized=T)
  n.topics <- topic.model$getNumTopics()
  unnormal.doc.topics <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)
  doc.len.1 <- rowSums(unnormal.doc.topics)
  
  num.topic.tokens.1 <- doc.topics.1 * doc.len.1
  topic.occur.1 <- doc.topics.1 > correlationMinProportion &
    num.topic.tokens.1 >= correlationMinTokens
  
  conv.dt <- combineConvDT(threaded.docs, topic.occur.1)
  
  filter <- conv.dt$jam %in% jams & conv.dt$n >= min.convo.len
  # filter <- rep(TRUE, nrow(conv.topic.occur))
  conv.topic.occur <- conv.dt[filter, 3:32]
  
  # kl covariance
  topic.counts <- colSums(conv.topic.occur>0)
  co.occur.count <- matrix(0, n.topics, n.topics)
  corr.matrix <- matrix(0, n.topics, n.topics)
  for (topic.i in 1 : (n.topics)) {
    for (topic.j in  1 : (n.topics)) {
      if (topic.counts[topic.i] >0 & topic.counts[topic.j] > 0) {
        co.occurs <- sum(conv.topic.occur[,topic.i] & conv.topic.occur[,topic.j])
        co.occur.count[topic.i, topic.j] <- co.occurs
        corr.matrix[topic.i, topic.j] <- log(nrow(conv.topic.occur) * co.occurs / (topic.counts[topic.i] * topic.counts[topic.j]))
      }
    }
  }
  
  corr.mat <- corr.matrix
  
  rownames(corr.mat) <- paste0("topic_", 1:30)
  colnames(corr.mat) <- paste0("topic_", 1:30)
  hm <- corr.heatmap(corr.mat, min=-1, max=1) +
    theme(axis.text.x=element_text(angle=45, hjust=1))
  return(list(chart = hm, corr.mat = corr.mat))
}


# correlationMinProportion <- 0.20
# correlationMinTokens <- 8
# min.convo.len <- 5

conv.hm.w <- conversationCooccurHeat(jams="world")
ggsave("outputs/topic conversation co-occur - world.png", 
       plot=conv.hm.w$chart, width=10, height=8)

conv.hm.v <- conversationCooccurHeat(jams="values")
ggsave("outputs/topic conversation co-occur - values.png", 
       plot=conv.hm.v$chart, width=10, height=8)




# Compare to Document level
correlates <- melt(corr.mat)
#drop everything on or below the diagonal
corr.triangle <- correlates[correlates$Var1 < correlates$Var2,]
# top 20 topic pairs
corr.triangle[with(corr.triangle, order(-value)),][1:20,]
# compared to top 20 pairs that document co-occur
correlates <- melt(doc.corr.mat$corr.matrix)
corr.triangle.docs <- correlates[correlates$Var1 < correlates$Var2,]
# top 20 topic pairs
corr.triangle.docs[with(corr.triangle.docs, order(-value)),][1:20,]


# which have the biggest difference between doc co-occur and conv co-occur?
# best to compare on rank, not absolute

a <- inner_join(corr.triangle, corr.triangle.docs, by=c("Var1", "Var2"))
new.a <- a %>% 
  arrange(desc(value.x)) %>% mutate(conv.rank=1:nrow(a)) %>%
  arrange(desc(value.y)) %>% mutate(doc.rank=1:nrow(a)) %>%
  mutate(rank.diff = conv.rank - doc.rank,
         abs.rank.diff = abs(conv.rank - doc.rank))
new.a %>% arrange(desc(abs.rank.diff))



####################
# forum experience #
####################

# how often do users post in more than one forum?
threaded.docs %>% 
  group_by(jam, user) %>% summarize(n=n(), forums=n_distinct(forum)) %>% 
  filter(n>2) %>% 
  group_by(jam, forums) %>% summarize(n=n())
# surpisingly, users who post more than once are MORE likely to do so in different forums

# which begs the question of how consistent an individual is in their topical focus

a <- threaded.docs %>% 
  group_by(jam, user) %>% summarize(n=n(), forums=n_distinct(forum)) %>%
  ggplot(a) %>% geom_histogram(aes(x=a$n, binwidth=1, color=jam))

######################################
# What characterizes the missing?
######################################

missing.by.gen <- threaded.docs %>% group_by(jam, generation) %>% summarize(sum(missing.ancestor))

threaded.docs$is.exec <- ifelse(threaded.docs$new.mgr=="executive", 1, 0)
threaded.docs$is.manager <- ifelse(threaded.docs$new.mgr=="manager", 1, 0)

prev.thresh <- 0.2
controls <- c("is.manager", "is.exec", "gender", "forum")

d.tps <- ifelse(doc.topics >= prev.thresh, 1, 0)
colnames(d.tps) <- paste0("t", 1:30)
thread.dt.miss <- 
  cbind(select_(threaded.docs, .dots=c("missing.ancestor", "jam", controls)),
        d.tps)
formula <- missing.ancestor ~ .

fit.miss <- glm(formula,
                data=thread.dt.miss,
                family="binomial")
summary(fit.miss)
fit.world.miss <- glm(formula,
                     data=thread.dt.miss %>% filter(jam=="world") %>% select(-jam),
                     family="binomial")
summary(fit.world.miss)

fit.value.miss <- glm(formula,
                     data=thread.dt.miss %>% filter(jam=="values") %>% select(-jam),
                     family="binomial")
summary(fit.value.miss)



#################################################################
# Which topics are associated with more conversation intensity? #
#################################################################

# [1] among comments that get a response, how long after is the response?

sum(threaded.docs$parent != "null")
a <- select(threaded.docs, id, Timestamp) %>% 
  right_join(select(threaded.docs, parent, Timestamp), 
            by=c("id"="parent") )
time.since.parent <- as.numeric(a$Timestamp.y- a$Timestamp.x, units="secs")

sum(time.since.parent<=0, na.rm=T)
# 29 where the child is recorded before the parent
# let's set them to 1 second
time.since.parent[time.since.parent<=0] <- 1

qplot(log(time.since.parent))

# for each comment with child, grab the fastest response
time.to.child <- select(threaded.docs, id, Timestamp) %>% 
  inner_join(select(threaded.docs, parent, Timestamp), 
             by=c("id"="parent")) %>%
  group_by(id) %>%
  summarize(sec.to.first.child = min(as.numeric(Timestamp.y - Timestamp.x,units="secs")))

# and merge back into the main set
child.times <- select(th.doc.topics, id, manager, forum, jam, u.s.time., is.manager, is.exec,
                      gender, new.mgr, Timestamp, continent2, log.sec.since.parent,
                      length, length.sq, is.first.comment, last.period, adj.focus, solo.topic,
                      missing.parent) %>%
  left_join(time.to.child, by=c("id"))


# response rate by time of day
ggplot(child.times, aes(x=log(sec.to.first.child), color=u.s.time.)) + geom_density()
ggplot(child.times, aes(x=log(sec.to.first.child), color=u.s.time.)) + 
  stat_ecdf(geom = "step") +
  geom_hline(aes(yintercept=0.1)) +
  geom_hline(aes(yintercept=0.25)) +
  geom_hline(aes(yintercept=0.5)) +
  geom_hline(aes(yintercept=0.75)) +
  geom_hline(aes(yintercept=0.9))

# response times by manager status
ggplot(child.times, aes(x=log(sec.to.first.child), color=new.mgr)) + geom_density()

# resposne times by gender
ggplot(child.times, aes(x=log(sec.to.first.child), color=gender)) + geom_density()

# by forum/jam
child.times$forum.jam <- paste(child.times$jam, child.times$forum, sep=":")
ggplot(child.times, aes(x=log(sec.to.first.child), color=forum.jam)) + geom_density()
ggplot(child.times, aes(x=log(sec.to.first.child), color=jam)) + geom_density()

# confirm via regression
lm.df <- child.times %>% filter(sec.to.first.child>0)
fit.response.time <- lm(log(sec.to.first.child) ~ new.mgr + gender + jam, data=lm.df)
summary(fit.response.time)



regressResponseTime <- function(topic.prev=NULL, controls=c()) {
  if (is.null(topic.prev)) {
    lm.df <- child.times %>%
      select_(.dots=c("sec.to.first.child", "jam", controls)) %>%
      filter(sec.to.first.child > 0)  
  } else {
    doc.tps <- doc.topics.unsmooth >= topic.prev
    colnames(doc.tps) <- paste0("occur-tp", 1:30)
    lm.df <- child.times %>%
      select_(.dots=c("sec.to.first.child", "jam", controls)) %>%
      cbind(doc.tps) %>%
      filter(sec.to.first.child > 0)
  }
  # no more overall models
  # fit.resp.topics <- lm(log(sec.to.first.child) ~ ., data=lm.df)
  fit.resp.topics.values <- lm(log(sec.to.first.child) ~ ., 
                               data=select(filter(lm.df, jam=="values"), -jam))
  fit.resp.topics.world <- lm(log(sec.to.first.child) ~ . , 
                              data=select(filter(lm.df, jam=="world"), -jam))
  return(list(values=fit.resp.topics.values, world=fit.resp.topics.world))
}
stargazer(fit.resp.topics.values, fit.resp.topics.world,
          type='text',
          column.labels = c("values","world"), 
          omit.stat=c("F","ser"))

# Nested models for World and Values
# 1	occupation, gender, time and region
fit.20.1 <- regressResponseTime(NULL, controls=c("is.manager", "is.exec", "gender", 
                                        "u.s.time.", "continent2", "last.period"))
# 2	above + post length, solo topic, concentration, first comment, missing parent, log seconds, and forum
fit.20.2 <- regressResponseTime(NULL, controls=c("is.manager", "is.exec", "gender", 
                                        "u.s.time.", "continent2", "last.period",
                                        "length",  "length.sq", "adj.focus", "solo.topic",
                                        "is.first.comment","missing.parent","log.sec.since.parent", 
                                         "forum"))
# 3	above + topics > .20
fit.20.3 <- regressResponseTime(0.2, controls=c("is.manager", "is.exec", "gender", 
                                       "u.s.time.", "continent2", "last.period",
                                       "length",  "length.sq", "adj.focus", "solo.topic",
                                       "is.first.comment","missing.parent","log.sec.since.parent", 
                                       "forum"))
# 4 maybe do a model with topic dummies only (to give us baseline descriptive measure)
fit.20.4 <- regressResponseTime(0.2, controls=c("last.period", "length",  "length.sq"))

stargazer(fit.20.1$values, fit.20.2$values, 
          fit.20.4$values, fit.20.3$values, 
          type='text', out="outputs/values_log_time_to_first_child.txt")
stargazer(fit.20.1$world, fit.20.2$world, 
          fit.20.4$world, fit.20.3$world, 
          type='text', out="outputs/world_log_time_to_first_child.txt")


stargazer(fit.20.1$values, fit.20.2$values, 
          fit.20.4$values, fit.20.3$values, 
          type='text', out="outputs/values_log_time_to_first_child_exp.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(fit.20.1$world, fit.20.2$world, 
          fit.20.4$world, fit.20.3$world, 
          type='text', out="outputs/world_log_time_to_first_child_exp.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)


###############################
# QUANTILE REGRESSION VERSION #
###############################

library(quantreg)

qr.resp.topics <- rq(log(sec.to.first.child) ~ ., tau=0.5, data=lm.df)
qr.resp.topics.val <- rq(log(sec.to.first.child) ~ ., tau=0.5, data=filter(lm.df, str_detect(forum.jam, "values")))
qr.resp.topics.world <- rq(log(sec.to.first.child) ~ ., tau=0.5, data=filter(lm.df, str_detect(forum.jam, "world")))

stargazer(fit.resp.topics, fit.resp.topics.values, fit.resp.topics.world,
          qr.resp.topics, qr.resp.topics.val, qr.resp.topics.world,
          type='text',
          column.labels = c("both","values","world","both","values","world"), 
          omit.stat=c("F","ser"))

qrs <- list()
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
for (tau in taus) {
  qr.val.test <- rq(log(sec.to.first.child) ~ ., tau=tau, data=filter(lm.df, str_detect(forum.jam, "values")))
  qrs[[as.character(tau)]] <- qr.val.test
}  
stargazer(qrs, type='text', column.labels = paste0("quantile:", names(qrs)))

qrs <- list()
taus <- c(0.1, 0.25, 0.5, 0.75, 0.9)
for (tau in taus) {
  qr.val.test <- rq(log(sec.to.first.child) ~ ., tau=tau, data=filter(lm.df, str_detect(forum.jam, "world")))
  qrs[[as.character(tau)]] <- qr.val.test
}  
stargazer(qrs, type='text', column.labels = paste0("quantile:", names(qrs)))



doc.tps.prev <- doc.topics.unsmooth 
colnames(doc.tps.prev) <- paste0("prev_tp",1:30)
lm.df <- cbind(child.times, doc.tps.prev, solo.topic, adj.focus) %>%  
  filter(sec.to.first.child>0) %>%
  select(-id, -manager, -forum, -jam, -prev_tp29)
#select(-manager, -forum, -jam, -mgr.gender, -forum.jam)

fit.resp.topics <- lm(log(sec.to.first.child) ~ ., data=lm.df)
summary(fit.resp.topics)




###############################
# comparing time series       #
###############################

# treat as cumulative distributions
#   % of total comments and % of total words

# time of first 




dtu <- doc.topics.unsmooth
names(dtu) <- paste0("topic_", 1:30)  
a <- cbind(select(documents,jam), dtu) %>%
  group_by(jam) %>%
  summarise_each(funs(mean)) %>%
  select(2:31) %>% t() %>% round(3) 
colnames(a) <- c("   values","   world")
a







##########################
# Who changes the topic? #
##########################

## cut points for topics
cutModels <- function(prev.thresh=NULL, 
                      controls, 
                      topic.interaction="", 
                      interaction.terms = c()) {
  if (is.null(prev.thresh)) {
    thread.dt.cuts <- select_(th.doc.topics, .dots=c("n.children", "responded", "jam", controls))
  } else {
    d.tps <- ifelse(doc.topics >= prev.thresh, 1, 0)
    colnames(d.tps) <- paste0("t", 1:30)
    thread.dt.cuts <- 
      cbind(select_(th.doc.topics, .dots=c("n.children", "responded", "jam", controls)),
            d.tps)
  }
  if (topic.interaction != "") {
    d.tps.inter <- th.doc.topics[,topic.interaction] * d.tps
    colnames(d.tps.inter) <- paste0("t-",topic.interaction,"-", 1:30)
    thread.dt.cuts <- cbind(thread.dt.cuts, d.tps.inter)
  }
  
  formula.text <- paste(c("responded ~ . ", interaction.terms), collapse = " + ")
  formula <- as.formula(formula.text)
  p.formula.text <- paste(c("n.children ~ . ", interaction.terms), collapse = " + ")
  p.formula <- as.formula(p.formula.text)
  
  fit.world.cut <- glm(formula,
                       data=thread.dt.cuts %>% filter(jam=="world") %>% select(-jam,-n.children),
                       family="binomial")
  
  fit.value.cut <- glm(formula,
                       data=thread.dt.cuts %>% filter(jam=="values") %>% select(-jam, -n.children),
                       family="binomial")

  
  poisson.world <- glm(p.formula,
                       data=thread.dt.cuts %>% filter(jam=="world") %>% select(-jam),
                       family=poisson())
  
  poisson.value <- glm(p.formula,
                       data=thread.dt.cuts %>% filter(jam=="values") %>% select(-jam),
                       family=poisson())
  ret.val <- list(values=fit.value.cut, world=fit.world.cut, p.world=poisson.world, p.val = poisson.value)
  return(ret.val)
}



######################################
# response length by parent/child title
######################################


child.len <- select(th.doc.topics, id, jam, parent, length, log.length, new.mgr) %>%
  inner_join(select(th.doc.topics, id, new.mgr), by=c("parent"="id"), suffix=c(".child", ".parent")) %>%
  mutate(parent.child.mgr = sprintf("%s-%s", new.mgr.parent, new.mgr.child))


ggplot(child.len, aes(log.length, color=new.mgr.parent)) +
  geom_density() +
  thm

ggplot(child.len, aes(log.length, color=parent.child.mgr)) +
  geom_density() +
  thm

child.len %>% group_by(new.mgr.parent, new.mgr.child) %>%
  summarise(mean_log_len_child = mean(log.length),
            se = sd(log.length)/sqrt(n()),
            geom_mean = exp(mean_log_len_child),
            arith_mean_len = mean(length))
# executives leave the shortest responses on average
child.len %>% group_by(new.mgr.child) %>%
  summarise(mean_log_len_child = mean(log.length),
            se = sd(log.length)/sqrt(n()),
            geom_mean = exp(mean_log_len_child),
            arith_mean_len = mean(length))
# people respond with longer resposnes to executive postings than to non-executives, but the difference in average length is only a few words
child.len %>% group_by(new.mgr.parent) %>%
  summarise(mean_log_len_child = mean(log.length),
            se = sd(log.length)/sqrt(n()),
            geom_mean = exp(mean_log_len_child),
            arith_mean_len = mean(length))

# compare values and world jam
child.len %>% group_by(jam, new.mgr.parent, new.mgr.child) %>%
  summarize(mean_log_len = mean(log.length),
            sd_log_len = sd(log.length),
            mean_sd = sd(log.length)/sqrt(n()))
# world jam has longer responses over all, but cross-category proportions are roughly the same

# do topics from parent appear in child by mgr-mgr type?
topic.prev <- 0.3
d.tps <- doc.topics.unsmooth >= topic.prev
colnames(d.tps) <- paste0("topic.", 1:30)
d.tp.count <- doc.topics.unnormal
colnames(d.tp.count) <- paste0("topic.", 1:30, ".words")
smooth.val <- 0.0000001
d.tps.u <- (doc.topics.unsmooth + smooth.val) / rowSums(smooth.val + doc.topics.unsmooth)
colnames(d.tps.u) <- paste0("topic.", 1:30, ".prev")

parent.child.set <- select(th.doc.topics, jam, id, parent, length, log.length, new.mgr) %>%
  cbind(d.tps) %>%
  cbind(d.tp.count) %>%
  cbind(d.tps.u) 
  
# count the number of topics in parent and the number of overlapping topics
overlap.form <- paste(sprintf("topic.%d.child * topic.%d.parent", 1:30, 1:30), collapse = "+")
#overlap.words <- paste(sprintf("topic.%d.words.child * topic.%d.words.parent", 1:30, 1:30), collapse = "+")
parent.topic.count <- paste(sprintf("topic.%d.parent", 1:30), collapse = "+")
child.topic.count <- paste(sprintf("topic.%d.child", 1:30), collapse = "+")
#parent.word.count <- paste(sprintf("topic.%d.words.parent", 1:30), collapse = "+")
#child.word.count <- paste(sprintf("topic.%d.words.child", 1:30), collapse = "+")



child.tps <-  parent.child.set %>%
  inner_join(select(parent.child.set, -parent, -jam), by=c("parent"="id"), suffix=c(".child", ".parent")) %>%
  mutate(parent.child.mgr = sprintf("%s-%s", new.mgr.parent, new.mgr.child)) %>% 
  mutate_(.dots=setNames(c(overlap.form, 
                           parent.topic.count, 
                           child.topic.count),
          c("overlap.topics", 
            "num.parent.topics", 
            "num.child.topics"))) %>%
  mutate(jaccard.thresh = ifelse(num.parent.topics + num.child.topics == 0, 
                                 0,
                                 overlap.topics / (num.parent.topics + num.child.topics - overlap.topics))
         #,jaccard.words = overlap.words / (parent.words + child.words - overlap.words)
         )

jensenShannon <- function(x, y) {
  m <- 0.5*(x + y)
  0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
}
KLD <- function(x,y) sum(x * log(x/y))
symKLD <- function(x,y) (0.5 * KLD(x, y)) + (0.5 * KLD(y, x))
cosSimil <- function(x,y) sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))

child.tps$focus.js <- NA
child.tps$focus.cos <- NA
child.tps$focus.kld <- NA
for (row in 1:nrow(child.tps)) {
  child.t <- child.tps[row, sprintf("topic.%d.prev.child", 1:30)]
  parent.t <- child.tps[row, sprintf("topic.%d.prev.parent", 1:30)]
  child.tps[row, "focus.js"] <- jensenShannon(child.t, parent.t)
  child.tps[row, "focus.cos"] <- cosSimil(child.t, parent.t)
  child.tps[row, "focus.kld"] <- symKLD(child.t, parent.t)
}
# flip JS dist to JS similarity
child.tps$focus.js <- log(2) - child.tps$focus.js


child.tps %>% group_by(jam, new.mgr.parent, new.mgr.child) %>%
  summarise(echo.share = sum(overlap.topics > 0)/n(),
            echo.share.1 = sum(overlap.topics[num.parent.topics==1] > 0)/sum(num.parent.topics==1),
            echo.share.2 = sum(overlap.topics[num.parent.topics==2] > 0)/sum(num.parent.topics==2),
            echo.share.3 = sum(overlap.topics[num.parent.topics==3] > 0)/sum(num.parent.topics==3),
            avg.echo.rate = sum(overlap.topics)/sum(num.parent.topics),
            # Jaccard Index: intersection divided by the union
              # based on thesholds
            jaccard.mean = mean(jaccard.thresh)
  )

child.tps %>% group_by(jam, new.mgr.parent) %>%
  summarise(echo.share = sum(overlap.topics > 0)/n(),
            echo.share.1 = sum(overlap.topics[num.parent.topics==1] > 0)/sum(num.parent.topics==1),
            echo.share.2 = sum(overlap.topics[num.parent.topics==2] > 0)/sum(num.parent.topics==2),
            echo.share.3 = sum(overlap.topics[num.parent.topics==3] > 0)/sum(num.parent.topics==3),
            avg.echo.rate = sum(overlap.topics)/sum(num.parent.topics),
            # Jaccard Index: intersection divided by the union
            # based on thesholds
            jaccard.mean = mean(jaccard.thresh)
  )



#####
# Response rates across classes
#####

child.map <- select(th.doc.topics, jam, parent, length, log.length, new.mgr, missing.parent) %>%
  left_join(select(th.doc.topics, id, new.mgr), by=c("parent"="id"), suffix=c(".child", ".parent")) %>%
  mutate(parent.child.mgr = sprintf("%s-%s", new.mgr.parent, new.mgr.child),
         parent.mgr = ifelse(missing.parent, "missing", 
                             ifelse(is.na(new.mgr.parent), "top-level", new.mgr.parent)))
table(child.map$parent.mgr)





responseGraph <- function(which.jam="Values", this.root.id=NULL) {
  if (!is.null(root.id)) {
    subset <- filter(th.doc.topics, root.id==this.root.id)
    this.child.map <- select(subset, jam, parent, length, log.length, new.mgr, missing.parent) %>%
      left_join(select(subset, id, new.mgr), by=c("parent"="id"), suffix=c(".child", ".parent")) %>%
      mutate(parent.child.mgr = sprintf("%s-%s", new.mgr.parent, new.mgr.child),
             parent.mgr = ifelse(missing.parent, "missing", 
                                 ifelse(is.na(new.mgr.parent), "top-level", new.mgr.parent)))
    this.df <- this.child.map %>%
      filter(parent.mgr != "missing") %>%
      group_by(new.mgr.child, parent.mgr) %>%
      summarise(n=n()) %>%
      group_by(new.mgr.child) %>%
      mutate(share = n / sum(n))
    gtitle <- sprintf("Who Responded to Whom in %s Thread", root.id)
  } else {
    source.df <- child.map %>%
      filter(parent.mgr != "missing") %>%
      group_by(jam, new.mgr.child, parent.mgr) %>%
      summarise(n=n()) %>%
      group_by(jam, new.mgr.child) %>%
      mutate(share = n / sum(n))
    this.df <- filter(source.df, jam==tolower(which.jam))
    gtitle <- sprintf("Who Responded to Whom in %s Jam", which.jam)
  }
    
  ggplot(this.df, aes(x=parent.mgr, fill=new.mgr.child, y=share)) +
    geom_bar(stat="identity", position=position_dodge()) +
    ggtitle(gtitle) +
    thm +
    xlab("Title of Parent Poster") +
    ylab("Share of Comments Made to Each Parent Type") +
    scale_fill_hue(name="Title of Poster")
}

ggsave(responseGraph(which.jam="world"), file=paste0(output.dir, "/Responding By Title/response_rates_by_title_world.png"))
ggsave(responseGraph("values"), file=paste0(output.dir, "/Responding By Title/response_rates_by_title_values.png"))

responseGraph(this.root.id="ffd5ff79c8.692b9ae5.break_down_the_silos")
responseGraph(this.root.id="<f6aed0dda9.fcabf807.VALUESJAM@w3prime1.sby.ibm.com>")

receive.df <- child.map %>%
  filter(parent.mgr != "missing") %>%
  group_by(jam, new.mgr.child, parent.mgr) %>%
  summarise(n=n()) %>%
  group_by(jam, parent.mgr) %>%
  mutate(share = n / sum(n))


responderGraph <- function(which.jam) {
  ggplot(filter(receive.df, jam==tolower(which.jam)), aes(fill=parent.mgr, x=new.mgr.child, y=share)) +
    geom_bar(stat="identity", position=position_dodge()) +
    ggtitle(sprintf("Who Participants Responded to in %s Jam", which.jam)) +
    ylim(0, 0.8) + thm +
    xlab("Title of Poster") +
    ylab("Share of Parent-Type from Each Child Type") +
    scale_fill_hue(name="Title of Parent Poster")
}

ggsave(responderGraph("world"), file=paste0(output.dir, "/Responding By Title/responder_rates_by_title_world.png"))
ggsave(responderGraph("values"), file=paste0(output.dir, "/Responding By Title/responder_rates_by_title_values.png"))




threaded.docs %>% group_by(jam) %>% summarise(sum(missing.ancestor))
th.doc.topics %>% filter(generation<=10) %>%
  group_by(generation, jam) %>% summarise(resp.rate=mean(responded), n=n()) %>%
  ggplot(aes(x=generation, y=resp.rate, color=jam)) + geom_line(size=1.5) + geom_point(size=5) + thm


th.doc.topics %>% filter(n.children==0) %>%
  group_by(generation, jam) %>% summarise(n())


th.doc.topics %>% filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=exp.excite.30, y=responded, color=jam)) + geom_smooth() + 
  geom_point(position = position_jitter(height=0.1), alpha=0.05)+
  ggtitle("Zero-order Relationship of Total Excitation to Response Rate")
th.doc.topics %>% filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=exp.excite.30.1st + exp.excite.30.2nd, y=responded, color=jam)) + geom_smooth() + 
  geom_point(position = position_jitter(height=0.1), alpha=0.05) +
  ggtitle("Zero-order Relationship of Parent and Grandparent Excitation to Response Rate")
th.doc.topics %>% filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=exp.excite.30.1st, y=responded, color=jam)) + geom_smooth() + 
  geom_point(position = position_jitter(height=0.1), alpha=0.05) +
  ggtitle("Zero-order Relationship of Parent Excitation to Response Rate")
th.doc.topics %>% filter(generation>2 & !missing.parent) %>%
  ggplot(aes(x=exp.excite.30.2nd, y=responded, color=jam)) + geom_smooth(method="loess") + 
  geom_point(position = position_jitter(height=0.1), alpha=0.05) +
  ggtitle("Zero-order Relationship of Grandpartn Excitation to Response Rate")
th.doc.topics %>% filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=log.sec.since.parent, y=responded, color=jam)) + geom_smooth()+ 
  geom_point(position = position_jitter(height=0.1), alpha=0.05) +
  ggtitle("Zero-order Relationship of Log(time since parent) to Response Rate")

th.doc.topics %>% filter(generation>2) %>%
  ggplot(aes(x=exp.excite.120.2nd, y=responded, color=jam)) + geom_smooth() + 
  geom_point(position = position_jitter(height=0.1), alpha=0.05)


th.doc.topics %>% filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=exc.30.after.1st, y=responded, color=jam)) + geom_smooth() + 
  geom_point(position = position_jitter(height=0.1), alpha=0.05)


th.doc.topics %>% filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=exp(log.sec.since.parent), y=responded, color=jam)) + geom_smooth()+ 
  geom_point(position = position_jitter(height=0.1), alpha=0.05)

gen.set <- th.doc.topics %>% filter(!missing.parent & !is.first.comment) 
exc.dist <- ecdf(gen.set$exp.excite.30)
th.doc.topics %>% filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=exc.dist(exp.excite.30), y=responded, color=jam)) + geom_smooth()
th.doc.topics %>% filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=exp.excite.30.quan, y=responded, color=jam)) + geom_smooth()

time.rr <- th.doc.topics %>% group_by(jam, u.s.time.) %>% summarise(mean.response.rate=mean(responded))
th.doc.topics %>% #filter(!missing.parent & !is.first.comment) %>%
  ggplot(aes(x=u.s.time., y=responded, color=jam)) +
  geom_point(position = position_jitter(height=0.1), alpha=0.05) +
  stat_summary(fun.y = "mean", size = 4, geom="point")
  geom_line(data=time.rr, aes(y=mean.response.rate, x=u.s.time., color=jam))
+
  ggtitle("Zero-order Relationship of Log(time since parent) to Response Rate")

  
  filter(th.doc.topics, jam=="world") %>% filter(Timestamp==min(Timestamp)) %>% select(text, Timestamp)
  filter(th.doc.topics, jam=="world") %>% filter(Timestamp  <= min(Timestamp)+10800) %>% select(text, Timestamp, office, job)

  
aa1 <- th.doc.topics %>% filter(DateWindow %in% c("2004-10-29 0","2004-10-29 1")) %>% select(parent, text, u.s.time.)
aa <- th.doc.topics %>% filter(DateWindow %in% c("2004-10-26 2","2004-10-26 1")) %>% select(parent, text, u.s.time.)
table(aa$u.s.time.)


th.doc.topics %>% 
  group_by(jam, new.mgr) %>%
  mutate(total.mgr.posts = n()) %>%
  group_by(jam, is.first.comment, new.mgr) %>% 
  summarise(mean(responded), n()/min(total.mgr.posts))
th.doc.topics %>% 
  group_by(jam, generation) %>% 
  summarise(mean(responded), n())

focal.vars.cent.1stgen <- c("focus.cent", "log.length.cent", "focus.log.length.cent", 
                     "identity.i_") 
top.filter <- th.doc.topics$is.first.comment
top.level.mods <- cutModels(0.2, controls=c(focal.vars.cent.1stgen, basic.controls),
                             filter.set=top.filter, do.hurdles=F)
summary(top.level.mods$values)
summary(top.level.mods$world)

temp.th.doc <- th.doc.topics
th.doc.topics$responded <- th.doc.topics$generation==2 & th.doc.topics$n.children==0
who.resp.model <- cutModels(0.2, controls=c(focal.vars.cent, "exp.excite.20", basic.controls),
                               do.hurdles=F)
