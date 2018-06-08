library(dplyr)
library(stm)
library(ggplot2)
library(stargazer)
library(tidyr)

setwd("/Users/clarkbernier/Box Sync/IBM Local/ibm-topic-model")
source("400-other-analyses/successFunctions.R")

load("place_docs_here/quoted_comments_with_stats.Rdata")
load("place_docs_here/world_stm_30.Rdata")
model <- stm.fit.30
tName <- function(t){sprintf("t%02d", t)}
# document-topic dataframe
dtdf <- data.frame(model$theta)
names(dtdf) <- tName(1:ncol(dtdf))

# grab the country from the old data and use to populate the is.US variable
raw.df <- prepped.docs$meta
sum(raw.df$id == analysis.set$id)
analysis.set$is.US <- ifelse(raw.df$country=="USA", 1, 0)
# and make a psuedo continent with US as the reference group
analysis.set$non.us.continent <- ifelse(raw.df$country=="USA", "__USA", analysis.set$continent)

# combine parentless indicators
analysis.set$no.parent <- analysis.set$missing.parent | analysis.set$is.first.comment 
  
# get categories for time slices
analysis.set$jam.time.window <-
  ifelse(analysis.set$CreationDate=="10/26/2004", 1, 
    ifelse(analysis.set$CreationDate=="10/27/2004",
           ifelse(analysis.set$CreationTime < "12:00:00 PDT", 2, 3),
           ifelse(analysis.set$CreationTime < "12:00:00 PDT", 4, 5)
    )
  )
analysis.set$jam.time.window <- factor(analysis.set$jam.time.window)   

h1.2 <- c("is.exec", "is.manager") #H1, H2
h.3 <- c("gender") # H3
h.4 <- c("non.us.continent") # H4
h.5 <- c("log.length") #H5
h.6 <- c("log.sec.since.parent", "log.sec.since.parent.sq",  "no.parent") #H6
h.7 <- c("responded", "n.children") #H7
h.8 <- c("jam.time.window") # H8: time spline
all.focal.vars <- c(h1.2, h.3, h.4, h.5, h.6, h.7, h.8)

basic.controls <- c()

full.set <- c(all.focal.vars,  basic.controls)

# filter.set <- which(analysis.set$length>10)
filter.set <- which(analysis.set$length>10 & 
                      (analysis.set$no.parent==1 | 
                         (analysis.set$no.parent==0 & analysis.set$log.sec.since.parent >=  5.446737)))


full.fit.10 <- cutQuoteModel(prev.thresh=0.1, full.set, filter.set = filter.set, remove.topics = 30)
full.fit.15 <- cutQuoteModel(prev.thresh=0.15, full.set, filter.set = filter.set, remove.topics = 30)
full.fit.20 <- cutQuoteModel(prev.thresh=0.2, full.set, filter.set = filter.set, remove.topics = 30)
summary(full.fit.10)


starMe(full.fit.10, full.fit.15, full.fit.20, 
       column.labels=c("Threshold=0.10","Threshold=0.15","Threshold=0.20"),
       out="outputs/success_threads_2018/model_full_by_threshold.htm",
       type="html")
starMe(full.fit.10, full.fit.15, full.fit.20, 
       column.labels=c("Threshold=0.10","Threshold=0.15","Threshold=0.20"),
       out="outputs/success_threads_2018/model_full_by_threshold.txt",
       type="text")

prev.thresh <- NULL
remove.topics <- 30

#H1-4
h1.4.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                        c(h1.2, h.3, h.4,  basic.controls), 
                        filter.set = filter.set, remove.topics = remove.topics)
#H5-8
h5.8.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                        c(h.5, h.6, h.7, h.8, basic.controls), 
                        filter.set = filter.set, remove.topics = remove.topics)


h1.2.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                            c(h1.2,  basic.controls), 
                            filter.set = filter.set, remove.topics = remove.topics)
h3.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                            c(h.3,  basic.controls), 
                            filter.set = filter.set, remove.topics = remove.topics)
h4.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                           c(h.4,  basic.controls), 
                           filter.set = filter.set, remove.topics = remove.topics)
h5.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                           c(h.5,  basic.controls), 
                           filter.set = filter.set, remove.topics = remove.topics)
h6.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                           c(h.6,  basic.controls), 
                           filter.set = filter.set, remove.topics = remove.topics)
h7.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                           c(h.7,  basic.controls), 
                           filter.set = filter.set, remove.topics = remove.topics)
h8.fit <- cutQuoteModel(prev.thresh=prev.thresh, 
                           c(h.8,  basic.controls), 
                           filter.set = filter.set, remove.topics = remove.topics)
starMe(h1.2.fit, h3.fit, h4.fit, h5.fit, h6.fit, h7.fit, h8.fit, full.fit.10,
       column.labels=c("H1&2", sprintf("H%s",3:8), "H1-8"),
       type="html",
       out="outputs/success_threads_2018/model_first_8_hyp.htm")
starMe(h1.2.fit, h3.fit, h4.fit, h5.fit, h6.fit, h7.fit, h8.fit, full.fit.10,
       column.labels=c("H1&2", sprintf("H%s",3:8), "H1-8"),
       type="text",
       out="outputs/success_threads_2018/model_first_8_hyp.txt")

# overall token prevalence
token.counts <- Matrix::rowSums(convertCorpus(prepped.docs$documents, prepped.docs$vocab, "Matrix"))
token.weighted.dt <- dtdf * token.counts


#####
# H9: occupation status groups talk about the same topics
####
# topic.rm <- sprintf("t%02d", c(30))
which.dtdf <- token.weighted.dt
# which.dtdf <- dtdf
topic.rm <- c()
thresh <- 0.1
occupation.prevs <- 
  data.frame(occupation.group=analysis.set$newer.mgr, which.dtdf) %>%
  gather("topic", "prev", 2:(ncol(which.dtdf)+1)) %>%
  #mutate(binary.topic=ifelse(prev>=thresh,1,0)) %>%
  group_by(occupation.group, topic) %>%
  summarise(mn.prev=mean(prev)) %>% # ,
            #above.thresh = mean(binary.topic)) %>%
  filter(!(topic %in% topic.rm))

exec.prevs <- (filter(occupation.prevs, occupation.group=="executive"))$mn.prev
mgr.prevs <- (filter(occupation.prevs, occupation.group=="manager"))$mn.prev
other.prevs <- (filter(occupation.prevs, occupation.group=="other"))$mn.prev

# exec.prevs <- (filter(occupation.prevs, occupation.group=="executive"))$above.thresh
# mgr.prevs <- (filter(occupation.prevs, occupation.group=="manager"))$above.thresh
# other.prevs <- (filter(occupation.prevs, occupation.group=="other"))$above.thresh

cor(exec.prevs, other.prevs)
cor(exec.prevs, mgr.prevs)
cor(other.prevs, mgr.prevs)

# ggplot(occupation.prevs) +
#   geom_point(aes(y=topic, x=above.thresh, color=occupation.group))
topic.prev.by.job.plt <- ggplot(occupation.prevs) +
  geom_point(aes(y=topic, x=mn.prev, color=occupation.group)) +
  xlab("Topic Prevalence") +
  ylab("Topic") +
  theme(legend.position="bottom") +
  labs(title="Topic Prevalence by Occupation Group",
       subtitle="
       For each topic, we calculate the overall prevalence (x-axis) for each occupation group 
       for each topic (y-axis), based on the total token allocated to each topic. ") 

fil.nam <- outFile(sprintf("occupation_topics_overall"))
ggsave(filename = fil.nam,
       plot = topic.prev.by.job.plt, 
       width = 8,
       height = 6)

# H9B: and by forum
occupation.prevs.forum <- 
  data.frame(occupation.group=analysis.set$newer.mgr, 
             forum=analysis.set$forum,
             which.dtdf) %>%
  gather("topic", "prev", 3:(ncol(which.dtdf)+2)) %>%
  group_by(occupation.group, topic, forum) %>%
  filter(!(topic %in% topic.rm)) %>%
  summarise(mn.prev=mean(prev)) 


for (fm in sprintf("forum %d",1:6)) {
  plt <- filter(occupation.prevs.forum, forum==fm) %>%
    ggplot() +
    geom_point(aes(y=topic, x=mn.prev, color=occupation.group))
    coord_cartesian(xlim=c(0,0.1)) # +
    #labs(title=sprintf("LIWC Rates in %s", fm),
    #     subtitle="For each LIWC category, we calculated the percentage of words\nused in the forum that are included in the category in question.") +
    #theme(legend.position="bottom") +
    #xlab("Proportion of words in Category") +
    #ylab("LIWC Category")
  
  fil.nam <- outFile(sprintf("occupation_topics_%s", fm))
  ggsave(filename = fil.nam,
         plot = plt, 
         width = 8,
         height = 6)
}


####
# H10. posts like kickoff posts in their own forum do better
####

# selecting at least one of the topics mentioned by the kickoffs increases selection

# Pre-processing

early.posts <- analysis.set %>% arrange(Timestamp) %>% filter(parent=="null") %>% slice(1:17) %>% select(id, forum, title, text)

which.dtdf <- token.weighted.dt
# which.dtdf <- dtdf

early.dt <- data.frame(id=analysis.set$id, which.dtdf) %>%
  right_join(early.posts, by="id")

forum.kickoff.topics <- early.dt %>% 
  select(-id, -title, -text) %>%
  group_by(forum) %>%
  summarise_all(.funs=sum) %>%
  data.frame()
just.the.dt <- forum.kickoff.topics[2:(1+ncol(which.dtdf))]
just.the.dt <- just.the.dt / rowSums(just.the.dt)


# A. find the topics mentioned in each forum's kickoffs
kickoff.thresh <- 0.1
rowSums(just.the.dt / rowSums(just.the.dt) >= kickoff.thresh)
kickoff.topics <- list()
for (f in 1:nrow(forum.kickoff.topics)) {
  kickoff.topics[[f]] <- which(just.the.dt[f,] >= kickoff.thresh)
}

# B. for each document, see if its topics match the forum
topic.thresh <- 0.1
analysis.set$has.kickoff.topic <- 0
analysis.set$num.kickoff.topic <- 0
forum.list <- analysis.set$forum
for (i in 1:nrow(analysis.set)) {
  these.topics <- which(dtdf[i,] >= topic.thresh) 
  forum.lookup <- as.numeric(strsplit(as.character(forum.list[i]), " ")[[1]][2])
  forum.topics <- kickoff.topics[[forum.lookup]]
  overlap.size <- length(intersect(these.topics, forum.topics))
  if (overlap.size > 0) {
    analysis.set[i, "has.kickoff.topic"] <- 1
    analysis.set[i, "num.kickoff.topic"] <- overlap.size
  }
}
sum(analysis.set$has.kickoff.topic)
sum(analysis.set$num.kickoff.topic)

# C. Regress!
remove.topics <- c(30)
h10.fit <- cutQuoteModel(prev.thresh=NULL, 
                        c("has.kickoff.topic"), 
                        filter.set = filter.set, remove.topics = remove.topics)

h10.topics.fit <- cutQuoteModel(prev.thresh=topic.thresh, 
                    c("has.kickoff.topic"), 
                    filter.set = filter.set, remove.topics = remove.topics)

h10.full.fit <- cutQuoteModel(prev.thresh=topic.thresh, 
                             c("has.kickoff.topic", full.set),
                             filter.set = filter.set, 
                             remove.topics = remove.topics)

starMe(h10.fit, h10.topics.fit, full.fit.10, h10.full.fit,
       column.labels=c("H10", "H10 w/Topics", "H1-8", "H1-8,10"),
       out="outputs/success_threads_2018/model_hyp_10.htm",
       type="html")
starMe(h10.fit, h10.topics.fit, full.fit.10, h10.full.fit,
       column.labels=c("H10", "H10 w/Topics", "H1-8", "H1-8,10"),
       out="outputs/success_threads_2018/model_hyp_10.txt",
       type="text")




####
# H11. higher prevalence topics -> higher rate of selection	
####

topics.fit.10 <- cutQuoteModel(prev.thresh=0.1, 
                          c(), 
                          filter.set = filter.set, remove.topics = remove.topics)

coefs <- summary(topics.fit.10)$coef[-1,1]
zs <- summary(topics.fit.10)$coef[-1,3]

tot.prev <- colMeans(dtdf[,-remove.topics])
tokened.prev <- (colSums(token.weighted.dt) / sum(token.weighted.dt))[-remove.topics]


cor(coefs, tot.prev)
cor(exp(coefs), tot.prev)
cor(zs, tot.prev)

cor(coefs, tokened.prev)
cor(exp(coefs), tokened.prev)
cor(zs, tokened.prev)


ggplot() + 
  geom_point(aes(x=tot.prev, y=coefs)) +
  xlab("Overall Topic Prevalence") +
  ylab("Selection Regression Coefficient")

ggplot() + 
  geom_point(aes(x=tot.prev, y=zs)) +
  xlab("Overall Topic Prevalence") +
  ylab("Selection Regression Z Score")

# Maybe we mean raw rate of selection?
which.dtdf <- token.weighted.dt
# which.dtdf <- dtdf
overall.prev <- (colSums(which.dtdf) / sum(which.dtdf))[-remove.topics]
quote.idx <- which(analysis.set$quoted==1)
select.prev <- (colSums(which.dtdf[quote.idx,])/ sum(which.dtdf[quote.idx,]))[-remove.topics]
topic.prev.plot <- ggplot() + 
  geom_text(aes(x=overall.prev, y=select.prev, label=names(overall.prev))) +
  geom_abline(aes(intercept=0, slope=1)) +
  xlab("Overall Topic Prevalence") +
  ylab("Selection Topic Prevalence") +
  labs(title="Topic Prevalence in Selected Set and Jam Overall",
       subtitle="
       For each topic, we calculate the overall prevalence in the jam (x-axis), and
       the prevalence of the topic in the 274 selected posts.  Topics above (below) the 
       line are selected at a higher (lower) rate than their expression in the Jam.") 
ggsave(plot = topic.prev.plot,
       filename = outFile("topic_prevalence"),
       width = 8,
       height = 6)

cor(overall.prev, select.prev)







#######################
# New Recombination [H12 and H13]
#######################
topics.ignore <- c(4, 6, 20, 29, 30)

###
# Approach A: How dense is my neighborhood in Topic Space?
###

#   LATER THOUGHTS: 
# [even in A] if you're in a tight cluster that's distant from everything else
#  A is about putting together new things not normally put together
#       B is about placing yourself "in the middle" of a bunch of idiosyncracies

# how robust is the rank order of novelties as the neighborhood radius changes?
# floor to the # neighbors

# ---> distance of its neighborhood from other neighborhoods
#     operationalized by taking its mean distance to other things after removing its neighbors
#     



which.dtdf <- dtdf[,-topics.ignore]
this.dt <- which.dtdf[filter.set, ]
a.set <- analysis.set[filter.set,]

library(parallelDist)

test.mat <- as.matrix(this.dt)
norm.test.mat <- test.mat / rowSums(test.mat)
dd <- parallelDist(test.mat, threads=3)
dd.norm <- parallelDist(norm.test.mat, threads=3)
ddm <- as.matrix(dd)
ddmn <- as.matrix(dd.norm)

# check out the overall distribution of pairwise distances
ad <- sample(dd.norm, 20000)
ggplot() + geom_density(aes(x=ad)) + xlim(0,1) + ylim(0,4)

# and the distribution over hte number of neighbors that each doc has
# at a chosen distance
aa <- rowSums(ddmn < 0.12)
hist(aa, breaks=100)

quote.idx <- which(analysis.set[filter.set,]$quoted == 1)
  
plotNeigh <- function(dd.mat, cut.point) {
  aa <- rowSums(dd.mat < cut.point)
  aa2 <- rowSums(dd.mat[quote.idx,] < cut.point)
  rbind(data.frame(neighbors=aa, quoted="Not Quoted"),
        data.frame(neighbors=aa2, quoted="Quoted")) %>%
    ggplot() + geom_density((aes(x=neighbors, color=quoted))) +
    ggtitle(sprintf("Neighbors within %s",cut.point))
}

plotNeigh(ddm, 0.12)
plotNeigh(ddmn, 0.12)




fitNeighModel <- function(dd.mat, cut.point, density.quantile.limit) {
  a.set$neighbors <- rowSums(dd.mat < cut.point)
  max.neigh <- quantile(a.set$neighbors, density.quantile.limit)
  a.set$neighbors <- ifelse(a.set$neighbors > max.neigh, max.neigh, a.set$neighbors)
  a.set$neighbors.2 <- a.set$neighbors^2
  a.set$neighbors.3 <- a.set$neighbors^3
  
  neigh.mod <- glm(quoted ~ neighbors, data=a.set, family = "binomial")
  neigh.mod.2 <- glm(quoted ~ neighbors + neighbors.2, data=a.set, family = "binomial")
  neigh.mod.3 <- glm(quoted ~ neighbors + neighbors.2 + neighbors.3, data=a.set, family = "binomial")
  print(summary(neigh.mod)$coef)
  print(summary(neigh.mod.2)$coef)
  print(summary(neigh.mod.3)$coef)
  #summary(neigh.mod)
  #summary(neigh.mod.2)
  #summary(neigh.mod.3)
  
  x <- 1:max(a.set$neighbors)
  y.1 <- coef(neigh.mod)[1] + 
    coef(neigh.mod)[2]*x  
  y.1 <- 1 / (1 + exp(-y.1))
  y.2 = coef(neigh.mod.2)[1] + 
    coef(neigh.mod.2)[2]*x + 
    coef(neigh.mod.2)[3]*x*x 
  y.2 <- 1 / (1 + exp(-y.2))
  y.3 = coef(neigh.mod.3)[1] +
    coef(neigh.mod.3)[2]*x + 
    coef(neigh.mod.3)[3]*x*x + 
    coef(neigh.mod.3)[4]*x*x*x
  y.3 <- 1 / (1 + exp(-y.3))
  
  data.frame(x=c(x,x,x),
             y=c(y.2, y.1, y.3),
             label=c(rep("Square", length(x)), rep("Linear", length(x)),
                     rep("Cubic", length(x)))) %>%
    ggplot() + 
    geom_line(aes(x=x, y=y, color=label)) +
    ggtitle(sprintf("Predicted Probs by Number Neighbors within %s,\nMax Quantile: %s",cut.point,density.quantile.limit)) +
    xlab(sprintf("Number of Neighbors within %s",cut.point)) +
    ylab("Predicted Prob of Selection")
}

fitNeighModel(ddmn, 0.05, 0.99)
fitNeighModel(ddmn, 0.1, 0.99)
fitNeighModel(ddmn, 0.12, 0.99)
fitNeighModel(ddmn, 0.13, 0.99)
fitNeighModel(ddmn, 0.14, 0.99)
fitNeighModel(ddmn, 0.15, 0.99)
fitNeighModel(ddmn, 0.16, 0.99)
fitNeighModel(ddmn, 0.17, 0.99)
fitNeighModel(ddmn, 0.25, 0.99)


this.dd <- ddmn

  cut.point <- 0.15
  density.quantile.limit <- 0.99
  gam.set <- select(a.set, neighbors, quoted)
  gam.set$neighbors <- rowSums(this.dd < cut.point)
  max.neigh <- quantile(gam.set$neighbors, density.quantile.limit)
  gam.set$neighbors <- ifelse(gam.set$neighbors > max.neigh, max.neigh, gam.set$neighbors)

  gam.15 <- ggplot(gam.set) +
    geom_smooth(aes(y=quoted, x=neighbors), 
              method="gam",
              formula = y ~ s(x, bs = "cs")) +
    ggtitle("Cubic Spline of Selection Rate by Number of Neighbors within 0.15 in Topic Space") +
    xlab("Number of Neighbors within 0.15") +
    ylab("Mean Post Selection Rate")
  ggsave(plot = gam.15,
         filename = outFile("GAM_by_neighbors_15"),
         width = 8,
         height = 6)

a.set %>% filter(quoted==1) %>%
  ggplot() +
  geom_point(aes(x=neighbors, y=focus))

# Outlier selected docs
a.set %>% 
  select(quoted, neighbors, text, focus) %>%
  cbind(dtdf[filter.set, ]) %>%
  filter(quoted==1 & neighbors>100) %>%
  write.csv(file="outputs/selected_docs_with_many_neighbors.csv", row.names = F)  

knotModel <- function(a.set, k) {
  qu <- a.set$quoted
  small.neigh <- ifelse(a.set$neighbors <= k, a.set$neighbors, k)
  big.neigh <- ifelse(a.set$neighbors > k, a.set$neighbors - k, 0)
  return(glm(qu ~ small.neigh + big.neigh, family = "binomial"))
}

knotFinder <- function(a.set, cut.point, mn.k=10) {
  a.set$neighbors <- rowSums(this.dd < cut.point)
  max.k <- floor(quantile(a.set$neighbors, 0.99)/10)
  min.k <- floor(mn.k/10)
  ks <- 10*(min.k:max.k)
  aics <- rep(0, length(ks))
  for (i in 1:length(ks)) {
    aics[i] <- summary(knotModel(a.set, ks[i]))$aic
  }
  plot(ks, aics)
  # print(sprintf("Best knot: %s", ks[which(aics == min(aics))]))
}


cut.point <- 0.12
density.quantile.limit <- 0.99
gam.set <- select(a.set, neighbors, quoted)
gam.set$neighbors <- rowSums(this.dd < cut.point)
max.neigh <- quantile(gam.set$neighbors, density.quantile.limit)
gam.set$neighbors <- ifelse(gam.set$neighbors > max.neigh, max.neigh, gam.set$neighbors)
ggplot(gam.set) +
  geom_smooth(aes(y=quoted, x=neighbors), 
              method="gam",
              formula = y ~ s(x, bs = "cs")) 

 
knotFinder(a.set, 0.10) # 
knotFinder(a.set, 0.12) # 80
knotFinder(a.set, 0.13) #
knotFinder(a.set, 0.14) #
knotFinder(a.set, 0.15) # 150
knotFinder(a.set, 0.16) #
knotFinder(a.set, 0.17) # 560
knotFinder(a.set, 0.18, mn.k=20) # 160 -- may as well be linear
knotFinder(a.set, 0.20, mn.k=200) # 170(min=100), 850 (min=200) 
knotFinder(a.set, 0.25) # 3650

a.set$neighbors <- rowSums(this.dd < 0.15)
summary(knotModel(a.set, 320))
summary(glm(quoted ~ neighbors, data=a.set))

a.set$neighbors <- rowSums(this.dd < 0.12)
summary(knotModel(a.set, 80))
summary(glm(quoted ~ neighbors, data=a.set))

a.set$neighbors <- rowSums(this.dd < 0.17)
summary(knotModel(a.set, 560))

# ABOVE 0.17, it becomes strictly linear, no knots needed
a.set$neighbors <- rowSums(this.dd < 0.18)
summary(knotModel(a.set, 850))

a.set$neighbors <- rowSums(this.dd < 0.20)
summary(knotModel(a.set, 850))


# Pull the final model and merge into the main data set for the other analyses!
final.radius <- 0.15
knot <- 150

id.set <- select(analysis.set, id)
a.set$neighbors <- rowSums(this.dd < final.radius)
neigh.names <- sprintf(c("neighbors_within_%s_below_%s", "neighbors_within_%s_above_%s"), final.radius, knot)
a.set[,neigh.names[1]] <- ifelse(a.set$neighbors <= knot, a.set$neighbors, knot)
a.set[,neigh.names[2]] <- ifelse(a.set$neighbors > knot, a.set$neighbors - knot, 0)
  
neigh.set <- a.set %>%
  select_(.dots=c("id", neigh.names, "neighbors")) %>%
  right_join(id.set, by="id")
analysis.set[,neigh.names] <- neigh.set[, neigh.names]
analysis.set$neighbors <- neigh.set$neighbors
f.neigh <- as.formula(sprintf("quoted ~ %s + %s", neigh.names[1], neigh.names[2]))

summary(glm(f.neigh, data=analysis.set[filter.set,], family="binomial"))
h12a.mod <- cutQuoteModel(prev.thresh=NULL, 
              c(neigh.names),
              filter.set = filter.set, 
              remove.topics = remove.topics)
summary(h12a.mod)

###
# Approach B: How far apart are the Topics I combine?
###

which.dtdf <- dtdf[,-topics.ignore]
this.dt <- which.dtdf[filter.set, ]

phi <- exp(model$beta$logbeta[[1]])
phi <- phi[-topics.ignore,]
if(any(phi==0)){
  phi<-phi + .Machine$double.eps
  phi<-phi/rowSums(phi)
}


topic.dists <- matrix(rep(0,nrow(phi)^2), ncol=nrow(phi))
for (i in 1:nrow(phi)) {
  for (j in 1:nrow(phi)) {
    topic.dists[i, j] <- jensenShannon(phi[i,], phi[j,])
  }  
}


out.set <- analysis.set[filter.set,"id"]
out.set$topic.novelty <- 0

for (i in 1:nrow(out.set)) {
  # cross multiply the row to get the matrix of pairwise topic weights
  b <- as.numeric(this.dt[i,])
  w <- b %*% t(b)
  
  # then element-wise multiply by the pairwise topic dists, 
  # normalise by the sum of the weights, and sum the results
  out.set[i, "topic.novelty"] <- sum(w * topic.dists / sum(w))
}

out.set %>% 
  ggplot() + 
  geom_density(aes(x=topic.novelty))

id.set <- select(analysis.set, id)
ided.set <- out.set %>%
  right_join(id.set, by="id")
analysis.set$topic.novelty <- ided.set$topic.novelty
analysis.set$topic.novelty.2 <- analysis.set$topic.novelty^2


a.set[,neigh.names[2]] <- ifelse(a.set$neighbors > knot, a.set$neighbors - knot, 0)

kink <- 0.475
analysis.set$topic.novelty.below.0.475 <- ifelse(analysis.set$topic.novelty <= kink, 
                                                 analysis.set$topic.novelty, kink)
analysis.set$topic.novelty.above.0.475 <- ifelse(analysis.set$topic.novelty > kink, 
                                                 analysis.set$topic.novelty - kink, 0)
topic.novel.names <- c("topic.novelty.below.0.475", "topic.novelty.above.0.475")
h12b.mod <- cutQuoteModel(prev.thresh=NULL, 
                          topic.novel.names,
                         filter.set = filter.set, 
                         remove.topics = remove.topics)
summary(h12b.mod)

# GAMS!!!
gam.topic.novel <- analysis.set[filter.set,] %>%
  ggplot() +
  geom_smooth(aes(y=quoted, x=topic.novelty), 
              method="gam",
              formula = y ~ s(x, bs = "cs")) +
  ggtitle("Cubic Spline of Selection Rate by Topic Combination Novelty") +
  xlab("Topic Combination Novelty Score") +
  ylab("Mean Post Selection Rate")
ggsave(plot = gam.topic.novel,
       filename = outFile("GAM_by_topic_novelty"),
       width = 8,
       height = 6)

analysis.set[filter.set,] %>%
  filter(topic.novelty>0.4) %>%
  ggplot() +
    geom_smooth(aes(y=quoted, x=topic.novelty), 
              method="gam",
              formula = y ~ s(x, bs = "cs")) 

cor(analysis.set$topic.novelty.above.0.475, 
    analysis.set$neighbors_within_0.15_below_150, 
    use="complete.obs")


save(analysis.set, file="place_docs_here/analysis_set_june2018.Rdata")




###########
# MODEL TABLES
###########

# H1-8 + 10

starMe(h1.4.fit, h5.8.fit, h10.fit, h10.full.fit,
       column.labels=c("H1-4 (Status)", "H5-8 (Quality)", "H10 (Forum Framing)", "Overall"),
       type="html",
       out="outputs/success_threads_2018/model_hyp_1-10.htm")
starMe(h1.4.fit, h5.8.fit, h10.fit, h10.full.fit,
       column.labels=c("H1-4 (Status)", "H5-8 (Quality)", "H10 (Forum Framing)", "Overall"),
       type="text",
       out="outputs/success_threads_2018/model_hyp_1-10.txt")


# H1-8 + 10 + 12/13

h12a.mod <- cutQuoteModel(prev.thresh=NULL, 
                          c(neigh.names),
                          filter.set = filter.set, 
                          remove.topics = remove.topics)
summary(h12a.mod)
h12b.mod <- cutQuoteModel(prev.thresh=NULL, 
                          topic.novel.names,
                          filter.set = filter.set, 
                          remove.topics = remove.topics)
summary(h12b.mod)
h12all.mod <- cutQuoteModel(prev.thresh=NULL, 
                          c(neigh.names,topic.novel.names),
                          filter.set = filter.set, 
                          remove.topics = remove.topics)
summary(h12all.mod)
h1.12.full.fit <- cutQuoteModel(prev.thresh=NULL,
                              c(neigh.names, topic.novel.names, full.set[c(5)]),
                              filter.set = filter.set, 
                              remove.topics = remove.topics)
summary(h1.12.full.fit)

starMe(h12a.mod, h12b.mod, h12all.mod, h1.12.full.fit, 
       column.labels=c("H12/13A (Novel Position)", "H12/13 (Novel Combination)", 
                       "H12/13 A/B", "H12/13 w/Length"),
       type="html",
       out="outputs/success_threads_2018/model_hyp_12-13.htm")
starMe(h12a.mod, h12b.mod, h12all.mod, h1.12.full.fit, 
       column.labels=c("H12/13A (Novel Position)", "H12/13 (Novel Combination)", 
                       "H12/13 A/B", "H12/13 w/Length"),
       type="text",
       out="outputs/success_threads_2018/model_hyp_12-13.txt")




test.fit <- cutQuoteModel(prev.thresh=NULL,
                                c(neigh.names, topic.novel.names, full.set[c()]),
                                filter.set = which(analysis.set[filter.set,]$log.length>log(80)), 
                                remove.topics = remove.topics)
summary(test.fit)

###########
# H14: LIWC MATTERS
###########

# see success_liwc_work.R file








###
# Detritus
###

summary(cutQuoteModel(prev.thresh=NULL, 
              c("novelty.max", "no.topic", "solo.topic"), 
              filter.set = filter.set, remove.topics = remove.topics))


### If anything below this breaks, it's because a chunk of code from worldSuccess.R is missing


###
#  recombination based on how rare the combination is at 10% threshold
###
thresh <- 0.05
K <- ncol(model$theta)
# Add in the topic model stats
d.tps <- model$theta
# calculate the binary topic map above threshold for each doc
binary.theta <- ifelse(d.tps >= thresh,1,0)
logical.theta <- d.tps >= thresh
binary.docs <- apply(binary.theta,1,paste0, collapse="")
a <- table(binary.docs)
hist(a, breaks=100)
table(apply(binary.theta, 1, sum))
# with 10% thresh:
#     0     1     2     3     4     5 
#   302  7862 14687  7226  1188    64 

topic.co.occur.rates <- matrix(nrow=K, ncol=K)
topic.cos.docs <- matrix(nrow=K, ncol=K)
for (i in 1:K) {
  for (j in 1:K) {
    # intersect over union
    topic.co.occur.rates[i,j] <- sum(logical.theta[,i] & logical.theta[,j]) / sum(logical.theta[,i] | logical.theta[,j])
    topic.cos.docs[i,j] <- cosSimil(binary.theta[,i], binary.theta[,j])
    
  }
}

co.occur <- topic.co.occur.rates
# co.occur <- topic.co.occur.rates
all.pairs <- array(co.occur)[which(array(co.occur<0.999))]
mn <- mean(all.pairs)
sd <- sd(all.pairs)
co.occur.novelty <- 1 - co.occur

analysis.set$novelty.max <- rep(0,nrow(analysis.set))
analysis.set$novelty.avg <- rep(0,nrow(analysis.set))
analysis.set$solo.topic <- rep(0,nrow(analysis.set))
analysis.set$no.topic <- rep(0,nrow(analysis.set))

for (d in 1:nrow(analysis.set)) {
  topic.list <- which(logical.theta[d,])
  # leave the measure at 0 if there are 0 or 1 focal topics in the doc
  if (length(topic.list) >= 2) {
    topic.pairs <- combn(topic.list, 2)
    res.set <- rep(NA, ncol(topic.pairs))
    for (pair in 1:ncol(topic.pairs)) {
      res.set[pair] <- co.occur.novelty[topic.pairs[1,pair], topic.pairs[2,pair]]
    }
    analysis.set[d,"novelty.max"] <- max(res.set)
    analysis.set[d,"novelty.avg"] <- mean(res.set)
  } else {
    # otherwise we'll need a control for single-topics 
    if (length(topic.list)==0) {
      analysis.set[d,"no.topic"] <- 1 
    } else {
      analysis.set[d,"solo.topic"] <- 1 
    }  
  }
}

analysis.set$novelty.max.std <- (analysis.set$novelty.max - mean(analysis.set$novelty.max))/sd(analysis.set$novelty.max)
analysis.set$novelty.max.sq <- analysis.set$novelty.max ^ 2

nov.d <- ecdf(analysis.set$novelty.max)
analysis.set$novelty.quart <- as.factor(ceiling(nov.d(analysis.set$novelty.max)/.25))

hist(analysis.set$novelty.avg, breaks=100)




