library(dplyr)
library(tidyr)
library(ggplot2)
library(stargazer)



setwd("/Users/clarkbernier/Box Sync/IBM Local/ibm-topic-model")
load("place_docs_here/world_stm_30.Rdata")
load("place_docs_here/quoted_comments_with_stats.Rdata")

model <- stm.fit.30
# Add in the topic model stats
d.tps <- model$theta
weighted.tps <- d.tps * analysis.set$token.counts

dt.10 <- d.tps[analysis.set$length>25,]
as.10 <- analysis.set[analysis.set$length>25,]


## exec titles
table(analysis.set[analysis.set$is.exec==1, "job"])
exec.words <- c("president","ceo","cfo","vp","vice president","dir","director","treasurer")
# new manager labels
mng.words <- c("mgr","manager","manages","mngr")
non.mgr.words <- c("program", "project")
client.words <- c("customer", "client", "consult", "consulting", "consultant")
clean.titles <- CleanTitles(analysis.set$job)

analysis.set[grepl("executive", clean.titles) & analysis.set$quoted==1, c("job")]

percQuote <- function(word) {
  print(sum(grepl(word, clean.titles)))
  print(sum(grepl(word, clean.titles) & analysis.set$quoted==1) / sum(grepl(word, clean.titles) ))
}
percQuote("engineer")
percQuote("dir")
percQuote("vp")
percQuote("manager")
percQuote("architect")
percQuote("senior")
percQuote("sale")
percQuote("executive")
percQuote("exec")
percQuote("region")
percQuote("communication")
percQuote("client")

sum(grepl("communication", clean.titles) & analysis.set$quoted==1 & grepl("exec", clean.titles)) 
sum(grepl("communication", clean.titles) & grepl("exec", clean.titles)) 

# the replacing function
LabelTitles <- function(titles, word.list, new.label, old.labels) {
  for (word in word.list) {
    regex <- paste0("\\b",word,"\\b")
    old.labels[grepl(regex, titles)] <- new.label
  } 
  return(old.labels)
}

analysis.set$newer.mgr <- "other"
new.mgr <- analysis.set$newer.mgr
# manager words first
new.mgr <- LabelTitles(clean.titles, mng.words, "manager", new.mgr)
# exec words bump those up
new.mgr <- LabelTitles(clean.titles, exec.words, "executive", new.mgr)
# non manager words trump both though
new.mgr <- LabelTitles(clean.titles, non.mgr.words, "other", new.mgr)
#slap them back into the data frame and the model.object
analysis.set$newer.mgr <- new.mgr

analysis.set$is.exec <- ifelse(analysis.set$newer.mgr=="executive", 1, 0)
analysis.set$is.manager <- ifelse(analysis.set$newer.mgr=="manager", 1, 0)

analysis.set$out.facing <- 0
out.facing <- analysis.set$out.facing
out.facing <- LabelTitles(clean.titles, client.words, 1, out.facing)
table(out.facing)
analysis.set$out.facing <- out.facing


#####
# before doing k-means, let's take a look at box plots of execs' topic distributions
#####

######
# to what extent are Execs more like other Execs than like others?
######
thresh <- 0.10
overall.tp <- colSums(d.tps)/sum(d.tps)

# how many posts from each exec?
aa <- filter(analysis.set, is.exec==1) %>%
  group_by(user) %>%
  summarise(n=n()) %>%
  arrange(desc(n))
table(aa$n)


wtp.df <- as.data.frame(weighted.tps)
names(wtp.df) <- paste0("wt", 1:ncol(d.tps))

exec.user.dtop <- analysis.set %>%
  select(user, is.exec, token.counts) %>%
  bind_cols(as.data.frame(d.tps)) %>%
  bind_cols(wtp.df) %>%
  filter(is.exec==1) %>%
  group_by(user) %>%
  mutate(words = sum(token.counts),
            avg.post = mean(token.counts),
            n.posts=n()) %>%
  ungroup()

exec.tp <- exec.user.dtop %>%
  filter(n.posts > 2) %>%
  group_by(user) %>%
  summarise_all(sum) %>%
  select(starts_with("wt"), token.counts, user) %>%
  mutate_at(funs(. / token.counts), .vars=vars(-user))
  
 select(exec.tp, -token.counts) %>%
   ggplot() 

overall.adjuster <- t(matrix(rep(overall.tp,nrow(exec.tp)),ncol=nrow(exec.tp),nrow=ncol(weighted.tps)))   

exec.tp[,names(wtp.df)] <- exec.tp[,names(wtp.df)] - overall.adjuster

mm <- melt(select(exec.tp, - token.counts), id="user")
 
ggplot(mm)+geom_boxplot(aes(x=variable, y=value))


hist(user.words$words, breaks = 100)  
hist(user.words$avg.post, breaks=100)  
hist(user.words$n.posts, breaks = 100)  

# QUADRANTS
binary.theta <- ifelse(d.tps >= thresh,1,0)
analysis.set$binary.label <- apply(binary.theta,1,paste0, collapse="")
all.quadrants <- table(analysis.set$binary.label)
hist(all.quadrants, breaks=100)

min.quad.members <- 5

# unique quadrants overall?
length(all.quadrants) #  793
# with 3 or more members?
sum(all.quadrants>=min.quad.members)   # 412
populated.quads <- names(all.quadrants[all.quadrants>=min.quad.members])

# in how many topic quadrants do Exec posts live?
exec.quads <- table(filter(analysis.set, is.exec==1)$binary.label)
length(exec.quads) # 349
# and out of the set of those with x or more?
length(intersect(names(exec.quads), populated.quads))  # 297 / 412

# in how many topic quadrants do quoted posts live?
quote.quads <- table(filter(analysis.set, quoted==1)$binary.label)
length(quote.quads) # 87
# and out of the set of those with x or more?
length(intersect(names(quote.quads), populated.quads))  # 82 / 412
# quoted, execs, and populated?
length(intersect(intersect(names(quote.quads), names(exec.quads)), populated.quads)) # 75 out of the 78 quoted quads!
length(intersect(names(quote.quads), names(exec.quads))) # 79 out of 87 quoted quads!!!!



####
# k-means in topic space
####

set.seed(12345)    
exec.tps <- d.tps[analysis.set$is.exec==1,]
exec.tps.10 <- dt.10[as.10$is.exec==1,]

wss <- (nrow(dt.10)-1)*sum(apply(dt.10,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(dt.10, centers=i, nstart=10)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

exec.wss <- (nrow(exec.tps)-1)*sum(apply(exec.tps,2,var))
for (i in 2:20) exec.wss[i] <- sum(kmeans(exec.tps, centers=i, nstart=10)$withinss)
plot(1:20, exec.wss, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

exec.wss.10 <- (nrow(exec.tps.10)-1)*sum(apply(exec.tps.10,2,var))
for (i in 2:20) exec.wss.10[i] <- sum(kmeans(exec.tps.10, centers=i, nstart=10)$withinss)
plot(1:20, exec.wss.10, type="b", xlab="Number of Clusters",
     ylab="Within groups sum of squares")

k.exec <- kmeans(exec.tps, centers=10, nstart=10)
table(k.exec$cluster)
k.exec$withinss / k.exec$size

avgFromWtps <- function(tps) {
  colSums(tps) / sum(tps)
}
(avgFromWtps(weighted.tps[k.exec$cluster==8,]) - avgFromWtps(weighted.tps)) /  avgFromWtps(weighted.tps)

min(analysis.set[apply(d.tps, 1, function(r) any(r > 0.35)), "length"])
View(analysis.set[apply(d.tps, 1, function(r) any(r > 0.35)) & analysis.set$length < 4,])

kmeans.results.10$centers
library(mclust)
d_clust <- Mclust(exec.tps.10, G=1:15, modelNames = mclust.options("emModelNames"))
d_clust$BIC
plot(d_clust)

library(NbClust)
nb <- NbClust(d.tps, diss=NULL, distance = "euclidean", 
              min.nc=2, max.nc=15, method = "kmeans", 
              index = "all", alphaBeale = 0.1)

hist(nb$Best.nc[1,], breaks = max(na.omit(nb$Best.nc[1,])))
####
# h-clusts
####

# first, we need doc-doc distance matrices
cosSimil <- function(x,y) sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))
library(proxy)
doc.dists <- proxy::dist(d.tps, method=cosSimil)
hc <- hclust(doc.dists)

plot(hc, )



#####
# Docs for liwc comparison
#    key here is we want a block of exec-level comments and then every comment overall
#    so execs will be doubled up, with the "user" replaced with "exec_set"
#####

temp.set <- filter(analysis.set, length >= 10) %>%
  select(user, text, newer.mgr, responded, quoted)

quote.set <- temp.set %>%
  filter(quoted==1) %>%
  mutate(user="quote_set")

responded.set <- temp.set %>%
  filter(responded==1) %>%
  mutate(user="responded_set")

exec.set <- temp.set %>%
  filter(newer.mgr == "executive") %>%
  mutate(user="exec_set")

early.posts <- analysis.set %>% 
  arrange(Timestamp) %>% 
  filter(parent=="null") %>% 
  slice(1:17) %>%
  mutate(user="early_set")

out.set <- temp.set %>%
  filter(newer.mgr == "executive") %>%
  mutate(user="exec_set") %>%
  bind_rows(temp.set) %>% 
  bind_rows(quote.set) %>%
  bind_rows(responded.set) %>%
  bind_rows(early.posts) %>%
  group_by(user) %>%
  summarise(text = paste(text, collapse="\n"))

write.csv(out.set, file="outputs/exec_set_docs.csv", row.names=F)

#### run the python notebook script to find the JS LIWC distances

exec.liwc.dists <- read.csv("outputs/dists_from_exec.csv")
liwc.dist.ecdf <- ecdf(exec.liwc.dists$dist.from.exec)
liwc.dist.ecdf(0.040412790118085379)

# find which users have enough comments to encode
min.comments <- 10

user.comments <- analysis.set %>%
  group_by(user) %>%
  summarise(n=n(),
            tot.words = sum(length),
            has.quoted = max(quoted)) %>%
  filter(n >= min.comments)
sum(user.comments$has.quoted)  


# this will leave NA for docs with fewer than 10 words
analysis.set <- analysis.set %>%
  #filter(user %in% user.comments$user) %>%
  left_join(exec.liwc.dists) 

save(analysis.set, file="place_docs_here/quoted_comments_with_stats.Rdata")

# how about we look at just the prolific uesrs?
pro.controls <- c(controls, "dist.from.exec", "multi.forums")
qf.pro.users.100 <- cutQuoteModel(prev.thresh=NULL, controls, 
                                 filter.set = which(analysis.set$length>10 & 
                                                      analysis.set$tot.words>100))

qf.pro.users.250 <- cutQuoteModel(prev.thresh=NULL, controls, 
                           filter.set = which(analysis.set$length>10 & 
                                                analysis.set$tot.words>250))

qf.pro.users.500 <- cutQuoteModel(prev.thresh=NULL, controls, 
                                  filter.set = which(analysis.set$length>10 & 
                                                       analysis.set$tot.words>500))

summary(qf.pro.users.100)
summary(qf.pro.users.250)
summary(qf.pro.users.500)

qf.pro.users.100.t <- cutQuoteModel(prev.thresh=0.15, controls, 
                                  filter.set = which(analysis.set$length>10 & 
                                                       analysis.set$tot.words>100))

qf.pro.users.250.t <- cutQuoteModel(prev.thresh=0.15, controls, 
                                  filter.set = which(analysis.set$length>10 & 
                                                       analysis.set$tot.words>250))

qf.pro.users.500.t <- cutQuoteModel(prev.thresh=0.15, controls, 
                                  filter.set = which(analysis.set$length>10 & 
                                                       analysis.set$tot.words>500))

summary(qf.pro.users.100.t)
summary(qf.pro.users.250.t)
summary(qf.pro.users.500.t)
table(analysis.set$tot.words>500, analysis.set$quoted)

plot(analysis.set$length, analysis.set$dist.from.exec)

#####
# Bootstrap Approach
#####

# also, let's bootstrap a set of comment sets to get a distribution in which to place the quoted docs
n <- nrow(quote.set)
samples <- 100
comp.df <- data.frame("user"=c(paste0("comp", 1:samples, sep = ""), 
                               "quote_set", "exec_set", "responded_set", "early_set"), 
                      'text'=rep("", samples + 4), stringsAsFactors = F)
for (s in 1:samples) {
  sampset <- sample_n(temp.set, n)
  print(sum(sampset$newer.mgr=="executive"))
  a <- sampset %>% summarise(text = paste(text, collapse="\n"))
  comp.df[s,"text"] = a$text
}
comp.df[s+1, "text"] = summarise(quote.set, text = paste(text, collapse="\n"))$text  
comp.df[s+2, "text"] = summarise(exec.set, text = paste(text, collapse="\n"))$text  
comp.df[s+3, "text"] = summarise(responded.set, text = paste(text, collapse="\n"))$text  
comp.df[s+4, "text"] = summarise(early.posts, text = paste(text, collapse="\n"))$text  
gznam <- "outputs/exec_comps_set_docs.gz"
gz.out <- gzfile(gznam, 'wt')
  write.csv(comp.df, file=gz.out, row.names=F)
close(gz.out)

# RUN THE NOTEBOOK
exclud <- c("quote_set", "exec_set", "responded_set", "early_set")
comp.liwc.dists <- read.csv("outputs/dists_from_exec_comps.csv", stringsAsFactors = F)
comp.set <- filter(comp.liwc.dists, !(user %in% exclud))$dist.from.exec
bs.dist <- ecdf(comp.set)

quote.from.exec.dist <- filter(comp.liwc.dists, user=="quote_set")$dist.from.exec
respon.from.exec.dist <- filter(comp.liwc.dists, user=="responded_set")$dist.from.exec
early.from.exec.dist <- filter(comp.liwc.dists, user=="early_set")$dist.from.exec
bs.dist(quote.from.exec.dist)
bs.dist(respon.from.exec.dist)
bs.dist(early.from.exec.dist)
hist(comp.set, breaks=50)
quote.dist

quote.liwc.dists <- read.csv("outputs/dists_from_quote_comps.csv", stringsAsFactors = F)
comp.set.quote <- filter(quote.liwc.dists, !(user %in% c("quote_set")))$dist.from.quote
exec.from.quote.dist <- filter(quote.liwc.dists, user=="exec_set")$dist.from.quote
respon.from.quote.dist <- filter(quote.liwc.dists, user=="responded_set")$dist.from.quote
early.from.quote.dist <- filter(quote.liwc.dists, user=="early_set")$dist.from.quote
hist(comp.set.quote, breaks=50, 
     main="Bootstrapped Comment Set Distances from Successful Comments",
     xlab="LIWC JS Distance")
abline(v=exec.from.quote.dist, col="red")

sum(comp.set.quote < exec.from.quote.dist) / length(comp.set.quote)

# from random comp
rand.liwc.dists <- read.csv("outputs/dists_from_rand_comp.csv", stringsAsFactors = F)
comp.set.rand <- filter(rand.liwc.dists, !(user %in% c("comp876")))$dist.from.rand
exec.dist <- filter(quote.liwc.dists, user=="exec_set")$dist.from.quote
hist(comp.set.rand, breaks=80)  
abline(v=exec.dist, col="red")

filter(rand.liwc.dists, user=="exec_set")$dist.from.rand
filter(rand.liwc.dists, user=="quote_set")$dist.from.rand

# comparing the distirbutions
raw.dists <- read.csv("outputs/exec_quote_overall_dists.csv", stringsAsFactors = F)
aa <- raw.dists %>% select(liwc.cat, overall.dist, exec.dist, quoted.dist) %>%
  gather(key=liwc.cat, value=dist, overall.dist, exec.dist, quoted.dist)
names(aa) <- c("liwc.cat", "set", "dist")
ggplot(aa) + geom_col(aes(x=liwc.cat, y=dist, color=set), position="dodge")

bb <- bind_rows(data.frame(set="execs", liwc.cat=raw.dists$liwc.cat, dist=raw.dists$exec.dist - raw.dists$overall.dist, stringsAsFactors = F),
          data.frame(set="quoted", liwc.cat=raw.dists$liwc.cat, dist=raw.dists$quoted.dist - raw.dists$overall.dist, stringsAsFactors = F))
different.liwcs <- raw.dists[which(abs(raw.dists$exec.dist - raw.dists$quoted.dist) > 0.001),]$liwc.cat
bb %>% filter(liwc.cat %in% different.liwcs) %>%
  ggplot() + 
    geom_col(aes(x=liwc.cat, y=dist, color=set, fill=set), width=0.6, position="dodge") +
    coord_flip()
  
  #theme(axis.text.x = element_text(angle = 90, hjust = 1))
# some categories are either rarely used, so remove them from the graph
# Funct is used much more highly than others, so remove for clarity's sake
cat.to.remove <- c("Swear", "Sexual", "Anx", "Anger", "Assent", "Body", "Death", "Family", "Ingest", "Funct", "Home", "Nonflu")
raw.dists <- read.csv("outputs/liwc_centers.csv", stringsAsFactors = F)
gather(raw.dists, "Group", "value", 2:6) %>%
  filter(Group != "responded" & !(liwc.cat %in% cat.to.remove)) %>%
  ggplot(aes(y=as.factor(liwc.cat), x=value, color=Group)) +
  geom_point() +
  xlim(0,0.08)

winners <- filter(analysis.set, quoted==1)
table(analysis.set$responded, analysis.set$generation, analysis.set$quoted)  
