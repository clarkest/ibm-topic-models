library(tidyr)
library(plyr)
library(ggplot2)
library(scales)
library(stargazer)
library(xtable)
library(digest)
library(stringr)
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

diags <- rJava::.jnew("cc/mallet/topics/TopicModelDiagnostics", rJava::.jcast(topic.model, "cc/mallet/topics/ParallelTopicModel"), 20L)
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
th.doc.topics$log.sec.since.parent <- ifelse(th.doc.topics$generation > 1 & !th.doc.topics$missing.parent,
                                          log(time.since.parent),
                                          0)
th.doc.topics$log.sec.since.parent <- ifelse(is.na(th.doc.topics$log.sec.since.parent) | th.doc.topics$log.sec.since.parent < 0,
                                             0,
                                             th.doc.topics$log.sec.since.parent)
th.doc.topics$log.sec.since.parent.sq <- th.doc.topics$log.sec.since.parent ^ 2
th.doc.topics$log.sec.since.parent.cube <- th.doc.topics$log.sec.since.parent ^ 3


# Track how long into a Conversation a comment was made

thread.timing <- 
  select(th.doc.topics, id, root.id, Timestamp) %>%
  left_join(select(by.root, root.id, first.post)) %>%
  mutate(secs.since.thread = as.numeric(Timestamp - first.post, units="secs")) 
  
# linear -- find comments in the prior X seconds, count as  (X - t) / X intensity

linear.excitement <- function(X) {
  tt <- thread.timing %>% 
    left_join(select(thread.timing, id, root.id, secs.since.thread), by="root.id") %>%
    filter(secs.since.thread.x - secs.since.thread.y >= 0  & 
             secs.since.thread.x - secs.since.thread.y < X &
             id.x != id.y) %>%
    mutate(secs.ago = secs.since.thread.x - secs.since.thread.y) %>%
    group_by(id.x) %>%
    summarize(linear.X = sum((X - secs.ago) / X)) %>%
    right_join(thread.timing, by=c("id.x" = "id")) %>%
    select(id.x, linear.X) %>%
    transmute(id=id.x, 
               excitement = ifelse(is.na(linear.X), 0, linear.X))
  return(tt)
}

linear.15 <- linear.excitement(900)
linear.30 <- linear.excitement(1800)
linear.60 <- linear.excitement(3600)
linear.120 <- linear.excitement(7200)
linear.180 <- linear.excitement(180 * 60)
linear.240 <- linear.excitement(240 * 60)
linear.set <- 
  merge(linear.15, linear.30, by="id", suffixes=c("", ".30")) %>%
  merge(linear.60, by="id", suffixes=c("",".60")) %>%
  merge(linear.120, by="id", suffixes=c("",".120")) %>%
  merge(linear.180, by="id", suffixes=c("",".180")) %>%
  merge(linear.240, by="id", suffixes=c(".15",".240")) 

th.doc.topics <- left_join(th.doc.topics, linear.set, by="id")

gg <- 
  th.doc.topics %>%
    filter(!missing.parent & !is.first.comment) %>%
    select(excitement.60, excitement.120, excitement.180, excitement.240) %>%
    gather(window.minutes, excitement) %>%
    ggplot(aes(excitement, colour=window.minutes)) +
      geom_density(size=1) +
      coord_cartesian(xlim=c(0,5), ylim=c(0,1.2)) +
      theme(text=element_text(size=16))
ggsave(gg, file="outputs/excitement_linear_decay_densities_hours.png")

# exponential decay
  # keep a running total score
  # each comment, reduce running total multiplying by decay.rate ^ seconds.since.prior

exp.excitement <- function(half.life.sec) {
  alpha <- exp(log(0.5) / half.life.sec)
  tt <- thread.timing %>% 
    left_join(select(thread.timing, id, root.id, secs.since.thread), by="root.id") %>%
    filter(secs.since.thread.x >= secs.since.thread.y  &
             id.x != id.y) %>%
    mutate(secs.ago = secs.since.thread.x - secs.since.thread.y) %>%
    group_by(id.x) %>%
    summarize(exp.X = sum(alpha ^ secs.ago)) %>%
    right_join(thread.timing, by=c("id.x" = "id")) %>%
    select(id.x, exp.X) %>%
    transmute(id=id.x, 
              exp.excite = ifelse(is.na(exp.X), 0, exp.X))
  return(tt)
}

exp.5 <- exp.excitement(300)
exp.10 <- exp.excitement(600)
exp.20 <- exp.excitement(1200)
exp.30 <- exp.excitement(1800)
exp.60 <- exp.excitement(3600)
exp.set <- 
  merge(exp.5, exp.10, by="id", suffixes=c("", ".10")) %>%
  merge(exp.20, by="id", suffixes=c("",".20")) %>%
  merge(exp.30, by="id", suffixes=c("",".30")) %>%
  merge(exp.60, by="id", suffixes=c(".5",".60")) 

th.doc.topics <- left_join(th.doc.topics, exp.set, by="id")

gg <- 
  th.doc.topics %>%
  filter(!missing.parent & !is.first.comment) %>%
  select(exp.excite.5, exp.excite.10, exp.excite.20, exp.excite.30, exp.excite.60) %>%
  gather(window.minutes, excitement) %>%
  ggplot(aes(excitement, colour=window.minutes)) +
  geom_density(size=1) +
  coord_cartesian(xlim=c(0,5), ylim=c(0,3)) +
  theme(text=element_text(size=16))
ggsave(gg, file="outputs/excitement_exp_decay_densities.png")




# Final Periods
#    let's add in a flag to control for the last two periods of each jam
th.doc.topics$last.period <- 
  ifelse(th.doc.topics$DateWindow %in% c("2003-08-01 2", "2004-10-29 0", "2004-10-29 1"),
         1, 0)
# and u.s. nighttime
th.doc.topics$u.s.time.window <- substr(th.doc.topics$DateWindow,12,13)
# 4-hour windows?
th.doc.topics$date.window.4 <- newDateWindows(th.doc.topics, 4)
th.doc.topics$u.s.time.window.4 <- substr(th.doc.topics$date.window.4,12,13)
time.window.names <- data.frame(u.s.time.window.4=as.character(0:5), 
                                u.s.time.=c("00-04 PDT", "04-08 PDT", "08-12 PDT", "12-16 PDT", "16-20 PDT", "20-24 PDT"))
th.doc.topics <- left_join(th.doc.topics, time.window.names, by="u.s.time.window.4")
# allow Americas to be omitted continent
th.doc.topics$continent2 <- ifelse(th.doc.topics$continent=="Americas", "AAmericas", th.doc.topics$continent)


##################
# I, we, and ibm #
##################
th.doc.topics$docs.have.ibm <- grepl("ibm", documents$text, ignore.case=T)
th.doc.topics$docs.have.we <- grepl(" we ", documents$text, ignore.case=T)
th.doc.topics$docs.have.i <- grepl(" I ", documents$text, ignore.case=T)

th.doc.topics %>% group_by(jam) %>% 
  dplyr::summarise(ibm = mean(docs.have.ibm),
                   we = mean(docs.have.we),
                   i = mean(docs.have.i))


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



#####################
# MODEL FUNCTION    #
#####################

## cut points for topics
cutModels <- function(prev.thresh=NULL, controls, topic.interaction="", interaction.terms = c()) {
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
  
  poisson.world <- glm(p.formula,
                       data=thread.dt.cuts %>% filter(jam=="world") %>% select(-jam),
                       family=poisson())
  
  poisson.value <- glm(p.formula,
                       data=thread.dt.cuts %>% filter(jam=="values") %>% select(-jam),
                       family=poisson())
  ret.val <- list(values=fit.value.cut, world=fit.world.cut, p.world=poisson.world, p.val = poisson.value)
  return(ret.val)
}




cuts.15 <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender"))
cuts.20 <- cutModels(0.20, controls=c("is.manager", "is.exec", "gender"))
cuts.25 <- cutModels(0.25, controls=c("is.manager", "is.exec", "gender"))
cuts.20.forum <- 
  cutModels(0.20, controls=c("is.manager", "is.exec", "gender", "forum"))
stargazer(cuts.15, cuts.15.a, type="text")

cuts.20.times <- 
  cutModels(0.20, controls=c("is.manager", "is.exec", "gender", "forum", "last.period", "u.s.time.window"))


cuts.20.times.4 <- 
  cutModels(0.20, controls=c("is.manager", "is.exec", "gender", "forum", "last.period", "u.s.time."))

# with focus
#cuts.15.c <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender", "focus"))
cuts.15.noac <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender"))
cuts.15.ac <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender", 
                                         "adj.focus", "solo.topic"))
cuts.15.ac.2 <- cutModels(0.15, controls=c("is.manager", "is.exec", "gender", 
                                           "adj.focus", "adj.conc.sq", "solo.topic"))

stargazer(cuts.15.noac, cuts.15.ac, cuts.15.ac.2, type="text")

# both focus and interacting focus with topic
   # ----  though this isn't taking into account how concentrated the focal topic is 
cuts.20.ac.time <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", "adj.focus", "solo.topic",
                                         "is.first.comment","missing.parent","log.sec.since.parent", 
                                         "log.sec.since.parent.sq","u.s.time.", "continent2","last.period"))
cuts.20.ac <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", "adj.focus", 
                                        "solo.topic", "u.s.time.", "continent2","last.period"))
cuts.20.ac.int <- cutModels(0.2, 
                        controls=c("is.manager", "is.exec", "gender", "adj.focus", "solo.topic"), 
                        topic.interaction="adj.focus")
# stargazer(cuts.20.ac, cuts.20.ac.int, type="text")
stargazer(cuts.20.ac, cuts.20.ac.time, type="text")


# Nested models for World and Values
# 1	occupation, gender, time and region
cuts.20.1 <- cutModels(NULL, controls=c("is.manager", "is.exec", "gender", 
                                       "u.s.time.", "continent2", "last.period"))
# 2	above + post length, solo topic, concentration, first comment, missing parent, log seconds, and forum
cuts.20.2 <- cutModels(NULL, controls=c("is.manager", "is.exec", "gender", 
                                        "u.s.time.", "continent2", "last.period",
                                        "log.length", "focus",  #"solo.topic",
                                        "is.first.comment","missing.parent","log.sec.since.parent", 
                                        "forum"))
cuts.20.2.int <- cutModels(NULL, controls=c("is.manager", "is.exec", "gender", 
                                            "u.s.time.", "continent2", "last.period",
                                            "log.length", "focus",  #"solo.topic",
                                            "is.first.comment","missing.parent","log.sec.since.parent", 
                                            "forum"),
                           interaction.terms = c("log.length : focus"))

# 3	above + topics > .20
cuts.20.3 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                        "u.s.time.", "continent2", "last.period",
                                        "log.length", "focus", #"solo.topic",
                                        "is.first.comment","missing.parent","log.sec.since.parent", 
                                        "forum"))
cuts.20.3.int <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                       "u.s.time.", "continent2", "last.period",
                                       "log.length", "focus", #"solo.topic",
                                       "is.first.comment","missing.parent","log.sec.since.parent", 
                                       "forum"),
                       interaction.terms = c("log.length : focus"))
cuts.20.3.exc60 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                           "u.s.time.", "continent2", "last.period",
                                           "log.length", "focus", #"solo.topic",
                                           "is.first.comment","missing.parent", "excitement.60",#"log.sec.since.parent", 
                                           "forum"),
                           interaction.terms = c("log.length : focus"))
cuts.20.3.exc30 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                           "u.s.time.", "continent2", "last.period",
                                           "log.length", "focus", #"solo.topic",
                                           "is.first.comment","missing.parent", "excitement.30",#"log.sec.since.parent", 
                                           "forum"),
                           interaction.terms = c("log.length : focus"))
cuts.20.3.exc15 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "excitement.15",#"log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exc120 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "excitement.120",#"log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exc180 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "excitement.180",#"log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exc240 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                             "u.s.time.", "continent2", "last.period",
                                             "log.length", "focus", #"solo.topic",
                                             "is.first.comment","missing.parent", "excitement.240",#"log.sec.since.parent", 
                                             "forum"),
                             interaction.terms = c("log.length : focus"))
cuts.20.3.exc <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                              "u.s.time.", "continent2", "last.period",
                                              "log.length", "focus", #"solo.topic",
                                              "is.first.comment","missing.parent",#"log.sec.since.parent", 
                                              "excitement.240", "excitement.120", "excitement.60", "excitement.15",
                                              "forum"),
                              interaction.terms = c("log.length : focus"))


cuts.20.3.exp5 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                              "u.s.time.", "continent2", "last.period",
                                              "log.length", "focus", #"solo.topic",
                                              "is.first.comment","missing.parent", "exp.excite.5","log.sec.since.parent", 
                                              "forum"),
                              interaction.terms = c("log.length : focus"))
cuts.20.3.exp10 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                            "u.s.time.", "continent2", "last.period",
                                            "log.length", "focus", #"solo.topic",
                                            "is.first.comment","missing.parent","exp.excite.10","log.sec.since.parent", 
                                            "forum"),
                            interaction.terms = c("log.length : focus"))
cuts.20.3.exp20 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                            "u.s.time.", "continent2", "last.period",
                                            "log.length", "focus", #"solo.topic",
                                            "is.first.comment","missing.parent", "exp.excite.20","log.sec.since.parent", 
                                            "forum"),
                            interaction.terms = c("log.length : focus"))
cuts.20.3.exp30 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                            "u.s.time.", "continent2", "last.period",
                                            "log.length", "focus", #"solo.topic",
                                            "is.first.comment","missing.parent", "exp.excite.30","log.sec.since.parent", 
                                            "forum"),
                            interaction.terms = c("log.length : focus"))
cuts.20.3.exp60 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                            "u.s.time.", "continent2", "last.period",
                                            "log.length", "focus", #"solo.topic",
                                            "is.first.comment","missing.parent", "exp.excite.60","log.sec.since.parent", 
                                            "forum"),
                            interaction.terms = c("log.length : focus"))





cuts.20.3.we <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                       "u.s.time.", "continent2", "last.period",
                                       "log.length", "focus", #"solo.topic",
                                       "is.first.comment","missing.parent", "log.sec.since.parent", 
                                       "forum", 
                                       "docs.have.ibm", "docs.have.we", "docs.have.i"))
# 4 maybe do a model with topic dummies only (to give us baseline descriptive measure)
cuts.20.4 <- cutModels(0.2, controls=c("last.period", "log.length"))

stargazer(cuts.20.3.int, 
          type='text', out="outputs/values_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)

# output the tables
stargazer(cuts.20.1$values, cuts.20.2.int$values, 
          cuts.20.4$values, cuts.20.3.exp10$values, 
          type='text', out="outputs/values_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(cuts.20.1$world, cuts.20.2.int$world, 
          cuts.20.4$world, cuts.20.3.exp10$world, 
          type='text', out="outputs/world_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)

stargazer(cuts.20.3$values, cuts.20.3.we$values, cuts.20.3$world, cuts.20.3.we$world,
          type='text', out="outputs/we_i_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)


# Excitation Models
stargazer(cuts.20.3.int$world, cuts.20.3.exc15$world, 
          cuts.20.3.exc30$world, cuts.20.3.exc60$world,
          cuts.20.3.exc120$world, cuts.20.3.exc180$world,
          cuts.20.3.exc240$world, cuts.20.3.exc$world,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          out="outputs/excite_linear_world.txt")
stargazer(cuts.20.3.int$values, cuts.20.3.exc15$values, 
          cuts.20.3.exc30$values, cuts.20.3.exc60$values,
          cuts.20.3.exc120$values, cuts.20.3.exc180$values,
          cuts.20.3.exc240$values, cuts.20.3.exc$values,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          out="outputs/excite_linear_values.txt")

stargazer(cuts.20.3.int$world, cuts.20.3.exp5$world, 
          cuts.20.3.exp10$world, cuts.20.3.exp20$world,
          cuts.20.3.exp30$world, cuts.20.3.exp60$world,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          out="outputs/excite_exp_world.txt")
stargazer(cuts.20.3.int$values, cuts.20.3.exp5$values, 
          cuts.20.3.exp10$values, cuts.20.3.exp20$values,
          cuts.20.3.exp30$values, cuts.20.3.exp60$values,
          type='text',
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          out="outputs/excite_exp_values.txt")


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



# testing the full models for multicolinearity
library(car)
vifv <- vif(cuts.20.2.int$values)
vifv
vifw <- vif(cuts.20.2$world)
vifw

vif(cuts.20.3.int$values)
vif(cuts.20.3.int$world)

vif(cuts.20.3.exp10$values)
vif(cuts.20.3.exp10$world)



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
