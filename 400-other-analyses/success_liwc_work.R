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
               "exec_set", "mgr_set",
               "asia_set", "us_set", "euro_set")
comp.df <- data.frame("user"=c(paste0("comp", 1:samples, sep = ""), 
                               set.names), 
                      'text'=rep("", samples + 8), stringsAsFactors = F)
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
comp.df[s+6, "text"] = summarise(asia.set, text = paste(text, collapse="\n"))$text  
comp.df[s+7, "text"] = summarise(us.set, text = paste(text, collapse="\n"))$text  
comp.df[s+8, "text"] = summarise(euro.set, text = paste(text, collapse="\n"))$text  
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


quantiles.liwc.dist
pairwise.liwc.dist

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




