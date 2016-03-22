library(tidyr)
library(dplyr)
library(ggplot2)
library(stargazer)

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
'
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
'

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

threaded.docs <- left_join(threaded.docs, num.children, by=c("id"="value"))

threaded.docs$n.children[is.na(threaded.docs$n.children)] <- 0

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
save(threaded.docs, file=sprintf("place_docs_here/threaded_docs.Rdata", model.name))
# load("place_docs_here/threaded_docs.Rdata")


# missing ancestors?
sum(threaded.docs$missing.ancestor)
# unique root ids?
length(unique(threaded.docs$root.id))

# what do groups by root.id look like?
by.root <- threaded.docs %>% group_by(root.id, jam) %>% 
  summarise(comments=n(), title.example=min(title),
            first.post=min(Timestamp), last.post=max(Timestamp), 
            duration=difftime(max(Timestamp), min(Timestamp), units="hours"),
            forum=first(forum)) %>%
  arrange(desc(comments))


by.root.window <- threaded.docs %>% group_by(root.id, DateWindow) %>% 
  summarise(n=n()) %>% spread(DateWindow, n, fill="") 
names(by.root.window) = c("root.id", sprintf("v%02d",1:11), sprintf("w%02d",1:10))
by.root <- left_join(by.root, by.root.window, by="root.id")

ggplot(by.root, aes(x=n)) + geom_density()



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
library(xtable)
library(digest)
library(stringr)
cut.off <- 5
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
class <- "manager"
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

threaded.docs <- merge(threaded.docs, match.to.parents)
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

# let's add in a flag to control for the last two periods of each jam
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

mean(th.doc.topics$docs.have.ibm)
mean(th.doc.topics$docs.have.we)
mean(th.doc.topics$docs.have.i)
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
        


# straight prevelence
colnames(d.tps) <- paste0("t", 1:30)
thread.dt.prev <- cbind(select(th.doc.topics, responded, jam, is.manager, is.exec, gender),
                        d.tps)

fit.1 <- glm(responded ~ . - t29,
             data=thread.dt.prev,
             family="binomial")
summary(fit.1)


f2  <- responded ~ . - t29

fit.world <- glm(f2,
              data=thread.dt.prev %>% filter(jam=="world") %>% select(-jam),
              family="binomial")
summary(fit.world)

fit.values <- glm(f2,
             data=thread.dt.prev %>% filter(jam=="values") %>% select(-jam),
             family="binomial")
summary(fit.values)

## cut points for topics
cutModels <- function(prev.thresh=NULL, controls, topic.interaction="") {
  if (is.null(prev.thresh)) {
    thread.dt.cuts <- select_(th.doc.topics, .dots=c("responded", "jam", controls))
  } else {
    d.tps <- ifelse(doc.topics >= prev.thresh, 1, 0)
    colnames(d.tps) <- paste0("t", 1:30)
    thread.dt.cuts <- 
      cbind(select_(th.doc.topics, .dots=c("responded", "jam", controls)),
            d.tps)
  }
  if (topic.interaction != "") {
    d.tps.inter <- th.doc.topics[,topic.interaction] * d.tps
    colnames(d.tps.inter) <- paste0("t-",topic.interaction,"-", 1:30)
    thread.dt.cuts <- cbind(thread.dt.cuts, d.tps.inter)
  }
  formula <- responded ~ .
  # no more Overall
  # fit.cuts <- glm(formula,
  #             data=thread.dt.cuts,
  #             family="binomial")
  
  fit.world.cut <- glm(formula,
                   data=thread.dt.cuts %>% filter(jam=="world") %>% select(-jam),
                   family="binomial")
  
  fit.value.cut <- glm(formula,
                   data=thread.dt.cuts %>% filter(jam=="values") %>% select(-jam),
                   family="binomial")
  
  ret.val <- list(values=fit.value.cut, world=fit.world.cut)
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
                                       "length",  "length.sq", "adj.focus", "solo.topic",
                                       "is.first.comment","missing.parent","log.sec.since.parent", 
                                       "forum"))
# 3	above + topics > .20
cuts.20.3 <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                        "u.s.time.", "continent2", "last.period",
                                        "length",  "length.sq", "adj.focus", "solo.topic",
                                        "is.first.comment","missing.parent","log.sec.since.parent", 
                                        "forum"))
cuts.20.3.we <- cutModels(0.2, controls=c("is.manager", "is.exec", "gender", 
                                       "u.s.time.", "continent2", "last.period",
                                       "length",  "length.sq", "adj.focus", "solo.topic",
                                       "is.first.comment","missing.parent","log.sec.since.parent", 
                                       "forum", 
                                       "docs.have.ibm", "docs.have.we", "docs.have.i"))
# 4 maybe do a model with topic dummies only (to give us baseline descriptive measure)
cuts.20.4 <- cutModels(0.2, controls=c("last.period", "length",  "length.sq"))

# output the tables
stargazer(cuts.20.1$values, cuts.20.2$values, 
          cuts.20.4$values, cuts.20.3$values, 
          type='text', out="outputs/values_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)
stargazer(cuts.20.1$world, cuts.20.2$world, 
          cuts.20.4$world, cuts.20.3$world, 
          type='text', out="outputs/world_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)

stargazer(cuts.20.3$values, cuts.20.3.we$values, cuts.20.3$world, cuts.20.3.we$world,
          type='text', out="outputs/values_prob_response_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp)

# testing the full models for multicolinearity
library(car)
vifv <- vif(cuts.20.3$values)
vifv
vifw <- vif(cuts.20.3$world)
vifw

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
library(stargazer)
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
                                 topic.prev, jam.list=c("values","world"), color, title) {  
  parent.dtp <- data.frame(doc.topics.unsmooth > topic.prev)
  names(parent.dtp) <- paste0("parent_topic_", 1:30)
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
      select(parent, focal.topic) %>%
      inner_join(parent.dtp, by=c("parent"="id")) %>%
      select(-parent)
    fit <- glm(focal.topic ~ . , data=glm.df)
    parent.child.topic.sig[, focal.topic] <- summary(fit)$coef[-1,3]
    parent.child.topic.coef[, focal.topic] <- summary(fit)$coef[-1,1]
  }
  row.names(parent.child.topic.sig) <- paste0("topic_", 1:30)
  row.names(parent.child.topic.coef) <- paste0("topic_", 1:30)
  dat <- melt(as.matrix(parent.child.topic.sig))
  coefs <- melt(as.matrix(parent.child.topic.coef))
  dat$odds.ratio <- exp(coefs$value)
  ggplot(dat, aes(as.factor(Var2), Var1, group=Var1)) +
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
       plot=val.trans.mat, width=10, height=8)
ggsave("outputs/estimated topic transitions - world.png", 
       world.trans.mat, width=10, height=8)
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
doc.corr.mat <- topic.co.occur(topic.model, correlationMinProportion=0.2)
corr.heatmap(doc.corr.mat$corr.matrix)

# and conversations
smooth <- 0.00000001
conv.top <- conv.topics[,3:32]
normal.conv.top <- (smooth + conv.top) / rowSums(conv.top + smooth)
conv.corr.mat <- topic.co.occur(doc.topics.1=normal.conv.top,
                                unnormal.doc.topics=conv.top, correlationMinProportion=0.2)
corr.heatmap(conv.corr.mat$corr.matrix)

# this isn't quite right though -- this has a prevelence cut-off 
#    of 20% of the CONVERSATION.  What we'd prefer is to count is if a document
#    within the conversation was above that threshold
correlationMinProportion <- 0.20
correlationMinTokens <- 8

doc.topics.1 <- mallet.doc.topics(topic.model, smoothed=T, normalized=T)
n.topics <- topic.model$getNumTopics()
unnormal.doc.topics <- mallet.doc.topics(topic.model, smoothed=F, normalized=F)
doc.len.1 <- rowSums(unnormal.doc.topics)

num.topic.tokens.1 <- doc.topics.1 * doc.len.1
topic.occur.1 <- doc.topics.1 > correlationMinProportion &
  num.topic.tokens.1 >= correlationMinTokens
conv.topic.occur <- combineConvDT(threaded.docs, topic.occur.1)
# basic correlation
corr.mat <- cor(conv.topic.occur[,3:32])
corr.heatmap(corr.mat)
# kl covariance

topic.counts <- colSums(conv.topic.occur[3:32])
co.occur.count <- matrix(0, n.topics, n.topics)
corr.matrix <- matrix(0, n.topics, n.topics)
for (topic.i in 1 : (n.topics)) {
  for (topic.j in  1 : (n.topics)) {
    co.occurs <- sum(conv.topic.occur[,topic.i+2] & conv.topic.occur[,topic.j+2])
    co.occur.count[topic.i, topic.j] <- co.occurs
    corr.matrix[topic.i, topic.j] <- log(nrow(conv.topic.occur) * co.occurs / (topic.counts[topic.i] * topic.counts[topic.j]))
  }
}

corr.mat <- corr.matrix
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







