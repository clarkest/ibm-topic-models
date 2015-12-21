
# load the persisted documents -- these are needed before we can load a model from state
file.name <- "/users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/models_dir/mac-docs.Rdata"
# this shoudl create an object called "documents"
load(file.name)
stop.word.file <- "200-topic-models/en.txt"
wd <-  "/users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")
mallet.instances <- mallet.import(documents$id, 
                                  documents$text, 
                                  stop.word.file, 
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]"
)

## Initialize a single topic model
topic.model <- MalletLDA(num.topics=1)
topic.model$loadDocuments(mallet.instances)
topic.model$setNumThreads(5L)
topic.model$setAlphaOptimization(20, 50)
topic.model$train(20)
topic.model$maximize(10)

jensenShannon <- function(x, y) {
  m <- 0.5*(x + y)
  0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
}
dist.mat <- proxy::dist(x = phi, method = jensenShannon)

percents <- seq(0.05, 0.5, by=0.1)
l1dists <- c()
jsdists <- c()
for (perc.small in percents) {
  n <- 10
  l1.tot <- 0
  js.tot <- 0
  print (perc.small)
  for (i in 1:n) { 
    documents$rand <- "large"
    documents[sample(1:nrow(documents), nrow(documents)*perc.small, replace=F), "rand"] = "small"
    l1.tot <- l1.tot + l1.distances("rand", documents, topic.model)[[1]]
    
    words.by.factor <- list()
    words.by.factor[["large"]] <- mallet.subset.topic.words(topic.model, 
                                                        documents[,"rand"]=="large", 
                                                        normalized=T, smoothed=T)
    words.by.factor[["small"]] <- mallet.subset.topic.words(topic.model, 
                                                            documents[,"rand"]=="small", 
                                                            normalized=T, smoothed=T)
    
    js.tot <- js.tot + jensenShannon(words.by.factor[["large"]], words.by.factor[["small"]])
  }
  print(l1.tot/n)
  l1dists <- c(l1dists, l1.tot / n)
  print(js.tot/n)
  jsdists <- c(jsdists, js.tot / n)
}
plot(percents, l1dists)

plot(percents, jsdists)


sample.frac <- seq(0.1,1, by=0.1)
l1.eq.size <- c()
for (perc.small in sample.frac) {
  n <- 20
  tot <- 0
  print (perc.small)
  for (i in 1:n) { 
    documents$rand <- "first"
    documents[sample(1:nrow(documents), nrow(documents)*0.5, replace=F), "rand"] = "second"
    documents$sample <- "no.use"
    documents[sample(1:nrow(documents), nrow(documents)*perc.small, replace=F), "sample"] = "use"
    tot <- tot + l1.distances("rand", documents, topic.model, documents$sample=="use")[[1]]
  }
  print(tot/n)
  l1.eq.size <- c(l1.eq.size, tot / n)
}
plot(sample.frac, l1.eq.size)





# What if we try Mahalanobis distance?


percents <- seq(0.05,0.45, by=0.1)
mdists <- c()
cdf.dists <- c()
l5.dists <- c()
js.dists <- c()
p <- 1
for (perc.small in percents) {
  print (perc.small)
  # we want to normalize by the stdev of each word under this sampling, 
  # so first we need to bootstrap token-level means and sd:
  boot.diffs <- boot(data=documents, statistic=diff.replicate, R=50, topic.model=topic.model, perc.small=perc.small, p=p)
  js.diffs <- boot(data=documents, statistic=diff.replicate, R=50, dist.mthd=jensenShannon, topic.model=topic.model, perc.small=perc.small)
  l5.diffs <- boot(data=documents, statistic=diff.replicate, R=50, topic.model=topic.model, perc.small=perc.small, p=0.5)
  
  dist.cdf <- ecdf(boot.diffs$t)
  js.cdf <- ecdf(js.diffs$t)
  l5.cdf <- ecdf(l5.diffs$t)
  
  iter <- 40
  m.tot <- 0
  d.tot <- 0
  js.tot <- 0
  for (i in 1:iter) { 
    documents$rand <- "large"
    documents[sample(1:nrow(documents), nrow(documents)*perc.small, replace=F), "rand"] <- "small"
    
    words.by.factor <- list()
    words.by.factor[["large"]] <- mallet.subset.topic.words(topic.model, 
                                                            documents[,"rand"]=="large", 
                                                            normalized=T, smoothed=T)
    words.by.factor[["small"]] <- mallet.subset.topic.words(topic.model, 
                                                            documents[,"rand"]=="small", 
                                                            normalized=T, smoothed=T)
    #this.val <- sum(
    #        abs(ifelse(lrg.word.sd==0, 0, (words.by.factor[["large"]] - lrg.word.means) / lrg.word.sd) - 
    #            ifelse(small.word.sd==0, 0, (words.by.factor[["small"]] - small.word.means) / small.word.sd)   
    #        )^p
    #    )
    this.val <- lp.dist(words.by.factor[["large"]], words.by.factor[["small"]], p=p)
    this.js <- jensenShannon(words.by.factor[["large"]], words.by.factor[["small"]])
    #print(sprintf("%f comes to %f", this.val, dist.cdf(this.val)))
    #m.tot <- m.tot + (this.val - min.dist) / (max.dist - min.dist)
    m.tot <- m.tot + this.val
    d.tot <- d.tot + dist.cdf(this.val) 
    js.tot <- js.tot + this.js
    
  }
  print(d.tot / iter)
  print(dist.cdf(m.tot / iter))
  print(js.cdf(js.tot / iter))
  mdists <- c(mdists, m.tot / iter)
  cdf.dists <- c(cdf.dists, dist.cdf(m.tot / iter))
  js.dists <- c(js.dists, js.cdf(js.tot / iter))
  
}

plot(percents, mdists)


library(boot)
diff.replicate <- function(data, indices, dist.mthd=lp.dist, topic.model, perc.small, perc.large=1-perc.small, ...) {
  doc.sample <- sample(1:nrow(data), nrow(documents)*(perc.small + perc.large), replace=F)
  
  small.share <- perc.small / (perc.small + perc.large)
  small.sub.sample <- sample(1:length(doc.sample), length(doc.sample)*small.share, replace=F)
  data$rand <- "no.use"
  data[doc.sample[small.sub.sample],"rand"] <- "small"
  data[doc.sample[-small.sub.sample],"rand"] <- "large"
  #print(sprintf("%f small and %f large for %f small share of %f sample", sum(data$rand=="small"), sum(data$rand=="large"), small.share, perc.small+perc.large))
  boot.set <- mallet.subset.topic.words(topic.model, 
                                        data[,"rand"]=="small", 
                                        normalized=T, smoothed=T)
  lrg.boot.set <- mallet.subset.topic.words(topic.model, 
                                            data[,"rand"]=="large", 
                                            normalized=T, smoothed=T)
  # limit to just words above a threshold
  corp.dist <- mallet.topic.words(topic.model, normalized=F, smooth=F)
  threshold <- 50
  
  return(dist.mthd(boot.set[corp.dist>=threshold], lrg.boot.set[corp.dist>=threshold], ...))
}

lp.dist <- function(set.1, set.2, p) {
  return(sum(abs(set.1 - set.2)^p)^(1/p))
}
jensenShannon <- function(x, y) {
  m <- 0.5*(x + y)
  0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
}
library(entropy)
symm.kl <- function(set.1, set.2) {
  up <- KL.plugin(set.1, set.2)
  down <- KL.plugin(set.2, set.1)
  return ((up + down)/2)
}


doc.set <- documents
percents <- seq(0.05, 1, by=0.05)
p <- 1
r <- 100
l1.boots <- list()
l5.boots <- list()
js.boots <- list()
kl.boots <- list()
for (i in 1: round(length(percents)/2)) {
  #l1.boots[[toString(i)]] <- list()
  #l5.boots[[toString(i)]] <- list()
  #js.boots[[toString(i)]] <- list()
  kl.boots[[toString(i)]] <- list()
  for (j in i : (length(percents)-i)) {
    perc.small <- percents[i]
    perc.large <- percents[j]
    print (sprintf("%f compared to %f", perc.small, perc.large))
    # we want to normalize by the stdev of each word under this sampling, 
    # so first we need to bootstrap token-level means and sd:
    #l1.diffs <- boot(data=doc.set, statistic=diff.replicate, R=r, topic.model=topic.model, perc.small=perc.small, perc.large=perc.large, p=p)
    #js.diffs <- boot(data=doc.set, statistic=diff.replicate, R=r, dist.mthd=jensenShannon, topic.model=topic.model, perc.small=perc.small, perc.large=perc.large)
    #l5.diffs <- boot(data=doc.set, statistic=diff.replicate, R=r, topic.model=topic.model, perc.small=perc.small, perc.large=perc.large, p=0.5)
    kl.diffs <- boot(data=doc.set, statistic=diff.replicate, R=r, dist.mthd=symm.kl, topic.model=topic.model, perc.small=perc.small, perc.large=perc.large)
    
    #dist.cdf <- ecdf(boot.diffs$t)
    #js.cdf <- ecdf(js.diffs$t)
    #l5.cdf <- ecdf(l5.diffs$t)
    #l1.boots[[i]][[j]] <- l1.diffs$t
    #l5.boots[[i]][[j]] <- l5.diffs$t
    #js.boots[[i]][[j]] <- js.diffs$t
    kl.boots[[i]][[j]] <- kl.diffs$t
    
  }
}

save(l1.boots, l5.boots, js.boots, kl.boots, file="/users/clarkbernier/boots.Rdata")
#save(kl.boots, file="/users/clarkbernier/kl-boots.Rdata")
#load("/users/clarkbernier/boots.Rdata")

plot.heat <- function(boots, percents, file.name, fun=mean) {
  vals <- lapply(boots, function (x) lapply(x, fun))
  plot <- ggplot(data=melt(vals), aes(x=percents[as.integer(L2)], y=percents[as.integer(L1)])) + 
    geom_tile(aes(fill=as.numeric(value)))
  ggsave(file.path(file.name), plot)
}

plot.heat(l1.boots, percents, "/users/clarkbernier/sandbox/outputs/l1means.png")
plot.heat(l5.boots, percents, "/users/clarkbernier/sandbox/outputs/l5means.png")
plot.heat(js.boots, percents, "/users/clarkbernier/sandbox/outputs/jsmeans.png")
plot.heat(kl.boots, percents, "/users/clarkbernier/sandbox/outputs/klmeans.png")

min(kl.plot.heat(l1.boots, percents, "/users/clarkbernier/sandbox/outputs/l1sd.png", sd)
plot.heat(l5.boots, percents, "/users/clarkbernier/sandbox/outputs/l5sd.png", sd)
plot.heat(js.boots, percents, "/users/clarkbernier/sandbox/outputs/jssd.png", sd)
plot.heat(kl.boots, percents, "/users/clarkbernier/sandbox/outputs/klsd.png", sd)

plot.heat(l1.boots, percents, "/users/clarkbernier/sandbox/outputs/l1median.png",  function(x) ifelse(is.null(x), NA, median(x)))

plot.heat(l1.boots, percents, "/users/clarkbernier/sandbox/outputs/l1coef.var.png", function(x) sd(x)/mean(x))
plot.heat(l5.boots, percents, "/users/clarkbernier/sandbox/outputs/l5coef.var.png", function(x) sd(x)/mean(x))
plot.heat(js.boots, percents, "/users/clarkbernier/sandbox/outputs/jscoef.var.png", function(x) sd(x)/mean(x))
plot.heat(kl.boots, percents, "/users/clarkbernier/sandbox/outputs/klcoef.var.png", function(x) sd(x)/mean(x))

for (i in 1: round(length(percents)/2)) {
  for (j in i : (length(percents)-i)) {
    perc.small <- percents[i]
    perc.large <- percents[j]
    print (sprintf("%f compared to %f", perc.small, perc.large))
  }
}










# bootstrapped distance metric
# reload model with one topic
mallet.instances <- mallet.import(documents$id, 
                                  documents$text, 
                                  stop.word.file, 
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]"
)
topic.model <- MalletLDA(num.topics=1)
topic.model$loadDocuments(mallet.instances)
topic.model$setNumThreads(5L)
topic.model$setAlphaOptimization(20, 50)
topic.model$train(20)
topic.model$maximize(10)

# bootstrap the distance between the two distributions
library(boot)
library(data.table)
library(dplyr)

KLD <- function(x,y) sum(x * log(x/y))
symKLD <- function(x,y) (0.5 * KLD(x, y)) + (0.5 * KLD(y, x))
chi.sqr <- function(x, y) sum(ifelse((x+y==0), 0, (x-y)^2 / (x+y)))
chi.sqr2 <- function(x, y, numx, numy) {
  x.weight <- (numy/numx)^0.5
  y.weight <- (numx/numy)^0.5
  sum(ifelse((x+y)==0,0,(x.weight * x - y.weight*y)^2 / (x+y)))
}                              

ResampledDist <- function(word.dist, total.words, smooth) {
  vocab.size <- length(word.dist)
  resample <- sample(1:vocab.size,
                     size=total.words,
                     replace=T,
                     prob=word.dist
  )
  # resample.table <- table(resample)
  # resample.table <- dplyr::summarise(group_by(data.table(a=resample), a), Freq=n())
  resample.table <- cbind(1:vocab.size, tabulate(resample, nbins=vocab.size))
  # map back into a probability distribution across words
  
  #freq.dist <- rep(0, vocab.size)
  #freq.dist[as.numeric(names(resample.table))] <- resample.table
  #freq.dist[resample.table$a] <- resample.table$Freq
  freq.dist <- resample.table[, 2] + smooth
  freq.dist <- freq.dist / sum(freq.dist)
  return(freq.dist)
}

boot.dist.replicant <- function(data, indices, perc.small, perc.large, smooth=0.000000001) {
  documents <- data
  documents$rand <- "no.use"
  doc.sample <- sample(1:nrow(documents), nrow(documents)*(perc.small + perc.large), replace=F)
  small.share <- perc.small / (perc.small + perc.large)
  small.sub.sample <- sample(1:length(doc.sample), length(doc.sample)*small.share, replace=F)
  documents[doc.sample[small.sub.sample],"rand"] <- "small"
  documents[doc.sample[-small.sub.sample],"rand"] <- "large"
  words.by.factor <- list()
  words.by.factor[["large"]] <- mallet.subset.topic.words(topic.model, 
                                                          documents[,"rand"]=="large", 
                                                          normalized=F, smoothed=F)
  words.by.factor[["small"]] <- mallet.subset.topic.words(topic.model, 
                                                          documents[,"rand"]=="small", 
                                                          normalized=F, smoothed=F)
  
  word.counts <- list()
  word.counts[["large"]] <- sum(words.by.factor[["large"]])
  word.counts[["small"]] <- sum(words.by.factor[["small"]])
  
  small.dist <- words.by.factor[["small"]] 
  small.dist <- small.dist / sum(small.dist) + smooth
  large.dist <- words.by.factor[["large"]] 
  large.dist <- large.dist / sum(large.dist) + smooth
  
  num.reps <- 50
  replications <- rep(NA, num.reps) 
  for (i in 1:num.reps) {
    freq_dist0 <- ResampledDist(small.dist, word.counts[["small"]], smooth)
    freq_dist1 <- ResampledDist(large.dist, word.counts[["large"]], smooth)
    replications[i] <- symKLD(freq_dist1, freq_dist0)
  }
  
  # symKLD(words.by.factor[["small"]], words.by.factor[["large"]])
  # quantile(replications, probs=c(0.025, 0.975))
  return(mean(replications))
}

chi.replicant <- function(data, indices, perc.small, perc.large) {
  documents <- data
  documents$rand <- "no.use"
  doc.sample <- sample(1:nrow(documents), nrow(documents)*(perc.small + perc.large), replace=F)
  small.share <- perc.small / (perc.small + perc.large)
  small.sub.sample <- sample(1:length(doc.sample), length(doc.sample)*small.share, replace=F)
  documents[doc.sample[small.sub.sample],"rand"] <- "small"
  documents[doc.sample[-small.sub.sample],"rand"] <- "large"
  words.by.factor <- list()
  words.by.factor[["large"]] <- mallet.subset.topic.words(topic.model, 
                                                          documents[,"rand"]=="large", 
                                                          normalized=T, smoothed=F)
  words.by.factor[["small"]] <- mallet.subset.topic.words(topic.model, 
                                                          documents[,"rand"]=="small", 
                                                          normalized=T, smoothed=F)
  
  word.counts <- list()
  word.counts[["large"]] <- sum(mallet.subset.topic.words(topic.model, 
                                                          documents[,"rand"]=="large", 
                                                          normalized=F, smoothed=F)
                            )
  word.counts[["small"]] <- sum(mallet.subset.topic.words(topic.model, 
                                                          documents[,"rand"]=="small", 
                                                          normalized=F, smoothed=F)
                            )
                                  
  small.dist <- words.by.factor[["small"]] 
  large.dist <- words.by.factor[["large"]] 
  #out.val <- chi.sqr2(small.dist, large.dist, word.counts[["small"]], word.counts[["large"]])
  out.val <- chi.sqr(small.dist, large.dist)
  return(out.val)
}

a <- boot(data=documents, 
          statistic=boot.dist.replicant, 
          R=10, 
          perc.small=0.05, perc.large=0.50, 
          parallel="snow")

Rprof("~/sandbox/dist_test.prof")
statistic <- chi.replicant
# statistic <- boot.dist.replicant
percents <- seq(0.05, 1, by=0.05)
p <- 1
r <- 50
boot.boots <- list()
#for (i in 1: round(length(percents)/2)) {
for (i in 1: round(length(percents)/2)) {
  boot.boots[[toString(i)]] <- list()
  for (j in i : (length(percents)-i)) {
    perc.small <- percents[i]
    perc.large <- percents[j]
    print (sprintf("%f compared to %f", perc.small, perc.large))
    a <- boot(data=documents, 
              statistic=statistic, 
              R=r, perc.small=perc.small, perc.large=perc.large,
              parallel="snow")$t
    boot.boots[[i]][[j]] <- a
  }
}
# Rprof(NULL)
# summaryRprof("~/sandbox/dist_test.prof")

plot.heat(boot.boots, percents, "~/sandbox/outputs/chi_1_boots.png")
save(boot.boots, file="~/sandbox/booted_boots.Rdata")


library(microbenchmark)
resample.table <- dplyr::summarise(group_by(data.table(a=resample), a), Freq=n())
compare <- microbenchmark(dplyr::summarise(group_by(data.table(a=resample), a), Freq=n()), table(resample), times=100)
autoplot(compare)

compare <- microbenchmark(boot(data=documents, 
                               statistic=boot.dist.replicant, 
                               R=10, 
                               perc.small=0.05, perc.large=0.50, 
                               parallel="multicore"), 
                          boot(data=documents, 
                               statistic=boot.dist.replicant, 
                               R=10, 
                               perc.small=0.05, perc.large=0.50, 
                               parallel="snow"),
                          times=25
                          )
autoplot(compare)
doc.bkup <- documents
documents <- documents[ ,c("text","id")]
documents<- doc.bkup
save(documents, file="~/Desktop/docs.Rdata")

library(devtools)
?Rprof


rawKLD <- function(x,y) (x * log(x/y))
rawsymKLD <- function(x,y) (0.5 * KLD(x, y)) + (0.5 * rawKLD(y, x))
freq_dist1 <- mallet.subset.topic.words(topic.model, 
                                        documents[,"rand"]=="large", 
                                        normalized=T, smoothed=T)
freq_dist0 <- mallet.subset.topic.words(topic.model, 
                                        documents[,"rand"]=="small", 
                                        normalized=T, smoothed=T)
corp.dist <- mallet.topic.words(topic.model, normalized=F, smooth=F)

freq_dist1 <- freq_dist1 / (sum(freq_dist1) + smooth * length(freq_dist1)) + smooth
freq_dist0 <- freq_dist0 / (sum(freq_dist0) + smooth * length(freq_dist0)) + smooth

kl.by.token <- rawsymKLD(freq_dist1, freq_dist0)
hist(kl.by.token, breaks=50)
sum(kl.by.token[corp.dist<2]) / sum(kl.by.token)
sum(corp.dist<2) / length(corp.dist)


kl.share <- rep(0, 200)
num.tokens <- rep(0,200)
for (i in 1:200) {
  kl.share[i] <- sum(b[corp.dist<i]) / sum(b)
  num.tokens[i] <- sum(corp.dist<i)
}
plot(share)
plot(num.tokens, kl.share)
hist(freq_dist0)
plot(freq_dist0, b)
min(freq_dist1)
min(freq_dist0)
