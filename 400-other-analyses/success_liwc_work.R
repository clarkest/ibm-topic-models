library(dplyr)

setwd("/Users/clarkbernier/Box Sync/IBM Local/ibm-topic-model")
# source("400-other-analyses/successFunctions.R")

load("place_docs_here/analysis_set_june2018.Rdata")
#load("place_docs_here/world_stm_30.Rdata")



#####
# Docs for liwc comparison
#    key here is we want a block of exec-level comments and then every comment overall
#    so execs will be doubled up, with the "user" replaced with "exec_set"
#####

temp.set <- filter(analysis.set, length > 10) %>%
  select(user, text, newer.mgr, responded, quoted, non.us.continent)

quote.set <- temp.set %>%
  filter(quoted==1) %>%
  mutate(user="quote_set")

responded.set <- temp.set %>%
  filter(responded==1) %>%
  mutate(user="responded_set")

exec.set <- temp.set %>%
  filter(newer.mgr == "executive") %>%
  mutate(user="exec_set")

mgr.set <- temp.set %>%
  filter(newer.mgr == "manager") %>%
  mutate(user="mgr_set")

non.mgr.set <- temp.set %>%
  filter(newer.mgr == "other") %>%
  mutate(user="non_mgr_set")

asia.set <- temp.set %>%
  filter(non.us.continent == "Asia") %>%
  mutate(user="asia_set")

euro.set <- temp.set %>%
  filter(non.us.continent == "Europe") %>%
  mutate(user="euro_set")

us.set <- temp.set %>%
  filter(non.us.continent == "__USA") %>%
  mutate(user="us_set")

early.posts <- analysis.set %>% 
  arrange(Timestamp) %>% 
  filter(parent=="null") %>% 
  slice(1:17) %>%
  mutate(user="early_set")




# also, let's bootstrap a set of comment sets to get a distribution in which to place the quoted docs
n <- nrow(quote.set)
samples <- 100
set.names <- c("quote_set", "responded_set", "early_set", 
               "exec_set", "mgr_set", "non_mgr_set",
               "asia_set", "us_set", "euro_set")
comp.df <- data.frame("user"=c(paste0("comp", 1:samples, sep = ""), 
                               set.names), 
                      'text'=rep("", samples + length(set.names)), stringsAsFactors = F)
for (s in 1:samples) {
  sampset <- sample_n(temp.set, n)
  a <- sampset %>% summarise(text = paste(text, collapse="\n"))
  comp.df[s,"text"] = a$text
}
comp.df[s+1, "text"] = summarise(quote.set, text = paste(text, collapse="\n"))$text  
comp.df[s+2, "text"] = summarise(responded.set, text = paste(text, collapse="\n"))$text  
comp.df[s+3, "text"] = summarise(early.posts, text = paste(text, collapse="\n"))$text
comp.df[s+4, "text"] = summarise(exec.set, text = paste(text, collapse="\n"))$text  
comp.df[s+5, "text"] = summarise(mgr.set, text = paste(text, collapse="\n"))$text  
comp.df[s+6, "text"] = summarise(non.mgr.set, text = paste(text, collapse="\n"))$text 
comp.df[s+7, "text"] = summarise(asia.set, text = paste(text, collapse="\n"))$text  
comp.df[s+8, "text"] = summarise(us.set, text = paste(text, collapse="\n"))$text  
comp.df[s+9, "text"] = summarise(euro.set, text = paste(text, collapse="\n"))$text  
gznam <- "outputs/exec_comps_set_docs.gz"
gz.out <- gzfile(gznam, 'wt')
write.csv(comp.df, file=gz.out, row.names=F)
close(gz.out)

# RUN THE NOTEBOOK

set.names




bootstrapQuant <- function(usr, bs.dist) {
  bs.dist(filter(comp.liwc.dists, user == usr)$dist)
}
pairwiseDist <- function(usr) {
  filter(comp.liwc.dists, user == usr)$dist
}

comp.names <- c(set.names, "overall_center", "comp87")
m <- matrix(0, ncol=length(comp.names), nrow=length(comp.names), 
            dimnames=list(comp.names, comp.names))
quantiles.liwc.dist <- data.frame(m)
pairwise.liwc.dist <- data.frame(m)

for (set.name in comp.names) {
  fil.name <- sprintf("outputs/liwc_sets/dists_from_%s.csv", set.name)
  comp.liwc.dists <- read.csv(fil.name, stringsAsFactors = F)
  comp.set <- filter(comp.liwc.dists, !(user %in% comp.names))$dist
  bs.dist <- ecdf(comp.set)
  quantiles.liwc.dist[,set.name] <- sapply(comp.names, bootstrapQuant, bs.dist)
  pairwise.liwc.dist[,set.name] <- sapply(comp.names, pairwiseDist)
}
quantiles.liwc.dist
pairwise.liwc.dist





####
# This bootstrapping approach is way too slow.  
# Let's calculate the LIWC distributions for each document once and then just use those for calculating everything else in R
out.doc.set <- select(analysis.set, text)
out.doc.set$user <- 1:nrow(out.doc.set)
gznam <- "outputs/all_docs_for_liwc.gz"
gz.out <- gzfile(gznam, 'wt')
write.csv(out.doc.set, file=gz.out, row.names=F)
close(gz.out)

# RUN NOTEBOOK

doc.liwcs <- read.csv("outputs/liwc_sets/liwc_doc_distributions.csv", stringsAsFactors = F)

getLiwcDist <- function(doc.nums, set.name=NA) {
  liwc.cats <- data.frame(liwc.cat=unique(doc.liwcs$liwc.cat), stringsAsFactors = F) %>% 
    arrange(liwc.cat)
  doc.len.lookup <- data.frame(doc.num = doc.nums,
                               len = analysis.set[doc.nums,]$length)
  
  out.df <- doc.liwcs %>%
    filter(doc.num %in% doc.nums) %>%
    inner_join(doc.len.lookup, by="doc.num") %>%
    group_by(liwc.cat) %>%
    summarise(prev = sum(len * prev) / sum(len)) %>%
    # right join with full cats list so that all categories are included and sorted the same
    right_join(liwc.cats, by="liwc.cat") %>%
    mutate(prev = ifelse(is.na(prev), 0, prev))
  
  if (!is.na(set.name)) {
    out.df$set.name <- set.name
  }
  return(out.df)
}

bootedLiwcDists <- function(ref.doc.nums, focal.doc.nums, n.boots, replace=F) {
  ref.liwc <- getLiwcDist(ref.doc.nums)
  focal.liwc <- getLiwcDist(focal.doc.nums)
  boot.dists <- rep(0, n.boots)
  for (i in 1:n.boots) {
    n <- length(focal.doc.nums)
    boot.docs <- sample(1:nrow(analysis.set), n, replace=replace)
    boot.dists[i] <- jensenShannon(ref.liwc$prev, getLiwcDist(boot.docs)$prev)
  }
  bs.dist <- ecdf(boot.dists)
  return(bs.dist(jensenShannon(ref.liwc$prev, focal.liwc$prev)))
}

eps.me <- function(dist, eps=1e-12) {
  dist <- dist + eps
  return(dist / sum(dist))
}
  

# rather than comparing one set or the other, figure out the distances between all pairs of the same sizes
# (as ref.doc.nums and focal.1.doc.nums)
# and use that to define the distributions of sizes between pairs their size.
# we ALSO want to know if a third entity [focal.2.doc.nums] is closer or further than the 
# first [focal.1.doc.nums].  Since we have focal.2.doc.nums, first repeat the same process as focal.1.doc.nums.
# then, for each boot sample, calculate the difference between each distance and persist
bootedPairwise <- function(ref.doc.nums, 
                           focal.1.doc.nums, 
                           focal.2.doc.nums, 
                           n.boots) {
  
  # bootstrapping always is sampling with replacement
  replace <- T 
  
  # for words-based sampling
  avg.words <- mean(analysis.set$length) 
  f1.words <-  sum(analysis.set[focal.1.doc.nums,]$length)
  f2.words <- sum(analysis.set[focal.2.doc.nums,]$length)
  ref.words <- sum(analysis.set[ref.doc.nums,]$length)
  
  # naive sampling (match document numbers)
  # n.focal.1 <- length(focal.1.doc.nums)
  # n.focal.2 <- length(focal.2.doc.nums)
  # n.ref <- length(ref.doc.nums)
  
  # alt sampling -- how many to get the same number of words?
  n.focal.1 <- round(f1.words / avg.words)
  n.focal.2 <- round(f2.words / avg.words)
  n.ref <- round(ref.words / avg.words)
                     
  dists.1 <- rep(0, n.boots)
  dists.2 <- rep(0, n.boots)
  diffs <- rep(0, n.boots)
  
  actual.ref.liwc <- getLiwcDist(ref.doc.nums)
  ref.dists.1 <- rep(0, n.boots)
  ref.dists.2 <- rep(0, n.boots)
  ref.diffs <- rep(0, n.boots)
  
  nwords.f1 <- rep(0, n.boots)
  nwords.f2 <- rep(0, n.boots)
  nwords.ref <- rep(0, n.boots)
  for (i in 1:n.boots) {
    doc.nums.boot.f1 <- sample(1:nrow(analysis.set), n.focal.1, replace=replace)
    doc.nums.boot.f2 <- sample(1:nrow(analysis.set), n.focal.2, replace=replace)
    doc.nums.boot.ref <- sample(1:nrow(analysis.set), n.ref, replace=replace)
    
    nwords.f1[i] <- sum(analysis.set[doc.nums.boot.f1,]$length)
    nwords.f2[i] <- sum(analysis.set[doc.nums.boot.f2,]$length)
    nwords.ref[i] <- sum(analysis.set[doc.nums.boot.ref,]$length)
    
    focal.1.liwc <- getLiwcDist(doc.nums.boot.f1)
    focal.2.liwc <- getLiwcDist(doc.nums.boot.f2)
    ref.liwc <- getLiwcDist(doc.nums.boot.ref)
    
    # two ways to measure dists and diffs
    # (1) to a random ref-sized reference group
    dists.1[i] <- jensenShannon(eps.me(ref.liwc$prev), eps.me(focal.1.liwc$prev))
    dists.2[i] <- jensenShannon(eps.me(ref.liwc$prev), eps.me(focal.2.liwc$prev))
    diffs[i] <- dists.1[i] - dists.2[i] 
    # (2) to the reference group itself
    ref.dists.1[i] <- jensenShannon(eps.me(actual.ref.liwc$prev), eps.me(focal.1.liwc$prev))
    ref.dists.2[i] <- jensenShannon(eps.me(actual.ref.liwc$prev), eps.me(focal.2.liwc$prev))
    ref.diffs[i] <- ref.dists.1[i] - ref.dists.2[i] 

  }
  # okay, now we have our two dists and diffs, we can calculate the quantile of the 
  # two observed distances and the observed diff
  ref.liwc <- getLiwcDist(ref.doc.nums)
  focal.1.liwc <- getLiwcDist(focal.1.doc.nums)
  focal.2.liwc <- getLiwcDist(focal.2.doc.nums)
  
  f1 <- jensenShannon(eps.me(ref.liwc$prev), eps.me(focal.1.liwc$prev))
  f2 <- jensenShannon(eps.me(ref.liwc$prev), eps.me(focal.2.liwc$prev))
  dif <- f1 - f2
  
  focal1.dist <- ecdf(dists.1)
  focal2.dist <- ecdf(dists.2)
  diff.dist <- ecdf(diffs)
  
  ref.focal1.dist <- ecdf(ref.dists.1)
  ref.focal2.dist <- ecdf(ref.dists.2)
  ref.diff.dist <- ecdf(ref.diffs)
  
  f1.nwords.dist <- ecdf(nwords.f1)
  f2.nwords.dist <- ecdf(nwords.f2)  
  ref.nwords.dist <- ecdf(nwords.ref)
  
  return(list(mean.diff = mean(diffs),
              focal1.dist = f1,
              focal.1.quant = focal1.dist(f1),
              focal2.dist = f2,
              focal.2.quant = focal1.dist(f2),
              diff.quant = diff.dist(dif),
              ref.f1.quant = ref.focal1.dist(f1),
              ref.f2.quant = ref.focal2.dist(f2),
              ref.diff.quant= ref.diff.dist(dif),
              f1.n = f1.nwords.dist(f1.words),
              f2.n = f2.nwords.dist(f2.words),
              ref.n = ref.nwords.dist(ref.words)
              ))
}


#booted.dist[["exec.to.quote"]] <- 
  
exec.dists <-  bootedPairwise(which(analysis.set$quoted==1),
                 which(analysis.set$is.exec==1),
                 which(analysis.set$is.manager==0 & analysis.set$is.exec==0),
                 1000)
# want roughly the same number of words on each side of the split
sum(analysis.set[analysis.set$length<=49,]$length) / sum(analysis.set$length)
length.cut <- 49
short.liwc <- getLiwcDist(which(analysis.set$length <= length.cut), "short")
long.liwc <- getLiwcDist(which(analysis.set$length > length.cut), "long")
ggplot(rbind(short.liwc, long.liwc)) +
  geom_point(aes(x=prev, y=liwc.cat, color=set.name)) 

fisher.test(rbind(short.liwc$prev*analysis.set[analysis.set$length<=49,]$length), 
            long.liwc$prev*analysis.set[analysis.set$length>49,]$length)))

mgr.dists <-  bootedPairwise(which(analysis.set$quoted==1),
                              which(analysis.set$is.manager==1),
                              which(analysis.set$is.manager==0 & analysis.set$is.exec==0),
                              1000)

asia.dists <-  bootedPairwise(which(analysis.set$quoted==1),
                              which(analysis.set$non.us.continent=="Asia"),
                              which(analysis.set$non.us.continent=="__USA"),
                             1000)

euro.dists <-  bootedPairwise(which(analysis.set$quoted==1),
                              which(analysis.set$non.us.continent=="Europe"),
                              which(analysis.set$non.us.continent=="__USA"),
                              1000)

rand.dists <-  bootedPairwise(which(analysis.set$quoted==1),
                              sample(1:nrow(analysis.set),3000),
                              sample(1:nrow(analysis.set),3000),
                             100)

booted.dist <- list("execs"=exec.dists,
                    "mgr"=mgr.dists,
                    "asia"=asia.dists,
                    "euro"=euro.dists,
                    "rand"=rand.dists)
save(booted.dist, file="place_docs_here/bootstrap_distance_objects.Rdata")



#####
# Graph the Category Centers
#####
cat.to.remove <- c("Swear", "Sexual", "Anx", "Anger", "Assent", "Body", "Death", "Family", "Ingest", "Funct", "Home", "Nonflu")


liwc.centers.occ <- rbind(getLiwcDist(which(analysis.set$quoted==1), "Selected"),
  getLiwcDist(which(analysis.set$is.exec==1), "Execs"),
  getLiwcDist(which(analysis.set$is.manager==1), "Managers"),
  getLiwcDist(which(analysis.set$newer.mgr=="other"), "Non-managers")) %>%
  filter(!(liwc.cat %in% cat.to.remove))

liwc.centers.cont <- rbind(getLiwcDist(which(analysis.set$quoted==1), "Selected"),
  getLiwcDist(which(analysis.set$non.us.continent=="Asia"), "Asia"),
  getLiwcDist(which(analysis.set$non.us.continent=="__USA"), "USA")) %>%
  filter(!(liwc.cat %in% cat.to.remove))


liwc.centers.occ %>%
  ggplot(aes(y=as.factor(liwc.cat), x=prev, color=set.name)) +
  geom_point() +
  xlim(0,0.08)  +
  xlab("Category Prevalence") +
  ylab("LIWC Category") +
  theme(legend.position="bottom") +
  labs(title="Liwc Category Prevalence by Occupation Group",
       subtitle="
       For each LIWC category (y-axis), we calculate the overall prevalence (x-axis) for each location group
        based on the total category-relevant words identified. ") 

liwc.centers.cont %>%
  ggplot(aes(y=as.factor(liwc.cat), x=prev, color=set.name)) +
  geom_point() +
  xlim(0,0.08)  +
  xlab("Category Prevalence") +
  ylab("LIWC Category") +
  theme(legend.position="bottom") +
  labs(title="Liwc Category Prevalence by Occupation Group",
       subtitle="
       For each LIWC category (y-axis), we calculate the overall prevalence (x-axis) for each occupation group 
       based on the total category-relevant words identified. ") 








####
# Detritus
####


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




