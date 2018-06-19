library(dplyr)
library(stm)
library(ggplot2)
library(stargazer)
library(tidyr)

setwd("/Users/clarkbernier/Box Sync/IBM Local/ibm-topic-model")
world.results <- read.delim("place_docs_here/world jam quotes raw.txt", 
                            sep="\n", quote="", header=F,
                            stringsAsFactors = F)

# find all that start with "Quote "
quote.str <- "Quote|Count"
quotes.idx <- grep(quote.str, ignore.case=T, apply(world.results, MARGIN=1, FUN=function(x) substr(x,1,5)))

quotes <- world.results[quotes.idx,]
findings <- world.results[-quotes.idx,]  

world <- read.delim("place_docs_here/hashed-reloaded_world_ided.tsv", 
                    encoding="UTF-8", 
                    colClasses=c("factor", "character", "character", "character", 
                                 "character", "factor", "factor", "factor", 
                                 "factor", "factor", "factor", "factor", 
                                 "character", "character", "factor", "character", 
                                 "factor","character","character"
                    ), 
                    sep="\t", 
                    quote=""
)

world.file <- "place_docs_here/hashed-world-docs-ngrams.tsv"
final.world <- read.delim(world.file, 
                    encoding="UTF-8", 
                    colClasses=c("factor", "character", "character", "character", 
                                 "character", "factor", "factor", "factor", 
                                 "factor", "factor", "factor", "factor", 
                                 "character", "character", "factor", "character", 
                                 "factor","character","character", "factor", "character"
                    ), 
                    sep="\t", 
                    quote=""
)

quote.locs <- rep(0, length(quotes))
not.enough.locs <- rep(NA, length(quotes))
too.many.locs <- list()

search.str.len <- 40
set.seed(1234)
for (i in 1:length(quotes)) {
  this.quote <- quotes[i]
  
  len.s <- nchar(this.quote)
  starts <- round((runif(5, 17, len.s - search.str.len)))
  matches <- list()
  for (start in starts) {
    sub.q <- substr(this.quote, start, start + search.str.len)
    quote.loc <- grep(sub.q, world$text, fixed=T)
    for (ql in quote.loc) {
      ql <- as.character(ql)
      if (length(matches)>0 && ql %in% names(matches)){
        matches[[ql]] <- matches[[ql]] + 1
      } else {
        matches[[ql]] <- 1
      }
    }
  }
  if (length(matches)==0) {
    # print(paste0("Found zero matches for ", i))
    not.enough.locs[i] <- 0
  }  
  if (length(matches)==1) {
    if (matches[1]>=3) {
      # print(sprintf("Found %s for %s", matches[1], i))
      quote.locs[i] <- names(matches)[1]
    } else {
      # print(sprintf("%s had 1 match, but only %s substrings", i, matches[1]))
      not.enough.locs[i] <- matches[1]
    }
  }
  if (length(matches)>1) {
    #print(sprintf("Found %s matches for %s", length(matches), i))
    #print(matches)
    too.many.locs[[as.character(i)]] <- matches
  }
}
sum(quote.locs != 0)
sum(!is.na(not.enough.locs))
table(unlist(not.enough.locs))
   #  0  1  2 
  #  13 13 37  
length(too.many.locs)
print(too.many.locs)

quoted.doc.idx <- quote.locs[quote.locs != 0]
# how many times to multiple quotes map to the same location?
id.table <- table(quoted.doc.idx)
id.table[id.table>1]
quotes[which(quote.locs==19234)]
world[19234, "text"]
quotes[which(quote.locs==26414)]
world[26414, "text"]
quotes[which(quote.locs==30713)]
quotes[which(quote.locs==30719)]

quote.ids <- world[quoted.doc.idx, "commentid"]

# how many of these are in the real world set?
full.world.quoted.idx <- final.world$commentid %in% quote.ids
sum(full.world.quoted.idx)
a <- !(quote.ids %in% world.2$commentid)


substr(quotes[7], 17,60)
grep("However, for multinationals", world$text, fixed=T)
quotes[177]
grep(substr(quotes[177], 17, 60), world.2$text, fixed=T)
world$text[c(17521, 17689, 17772, 18565)]



# finding the missing quotes by hand

txts <- tolower(world$text)
findMe <- function(str) {
  stl <- tolower(str)
  idx <- grep(stl, txts, fixed=T)
  print(idx)
  txts[idx]
}

new.quote.locs <- quote.locs
which(new.quote.locs=="0")

new.quote.locs[3] <- "12036"
new.quote.locs[4] <- "3015"
new.quote.locs[9] <- "30867"
new.quote.locs[10] <- "12831"
new.quote.locs[14] <- "Combines 14010 and 15425 - different authors"
  newidx = length(new.quote.locs) + 1
  new.quote.locs[14] <- 14010
  new.quote.locs[newidx] <- 15425
  quotes[newidx] <- quotes[14]
new.quote.locs[17] <- "NONE"
new.quote.locs[27] <- "20797"
new.quote.locs[28] <- "27084"
new.quote.locs[30] <- "21500" # either quoted or repeated verbatim in 21666
new.quote.locs[32] <- "22377"
new.quote.locs[34] <- "13631"
new.quote.locs[35] <- "32060" # ordering of the phrasing is changed
new.quote.locs[36] <- "MANY - same user either quoting or repeating in 10 comments"
new.quote.locs[37] <- "19210"
new.quote.locs[41] <- "22302"
new.quote.locs[45] <- "27954" # but repeated by the same author in 28341
new.quote.locs[52] <- "10000"
new.quote.locs[60] <- "16644" # repeated entirely in 16646
new.quote.locs[68] <- "18743"
new.quote.locs[69] <- "30910"
new.quote.locs[72] <- "13604"
new.quote.locs[78] <- "14544" # repeated entirely in 14547
new.quote.locs[80] <- "1058" # repeated by the same author in 1088
new.quote.locs[81] <- "12701" # quoted in 20750
new.quote.locs[84] <- "30594"
new.quote.locs[87] <- "8839"
new.quote.locs[88] <- "32121"
new.quote.locs[92] <- "28544"
new.quote.locs[93] <- "34"
new.quote.locs[94] <- "491"
new.quote.locs[108] <- "29845"
new.quote.locs[109] <- "2840"
new.quote.locs[110] <- "27921" # repeated entirely in 27924
new.quote.locs[113] <- "23667"
new.quote.locs[114] <- "3909"
new.quote.locs[115] <- "15051"
new.quote.locs[118] <- "18760" # though part of the quote is the body of 18315
new.quote.locs[120] <- "30661"
new.quote.locs[124] <- "27116"
new.quote.locs[125] <- "30100"
new.quote.locs[126] <- "30698"
new.quote.locs[138] <- "21397"
new.quote.locs[143] <- "32202"
new.quote.locs[146] <- "6179"
new.quote.locs[151] <- "20844"
new.quote.locs[154] <- "22200"
new.quote.locs[155] <- "14060"
new.quote.locs[162] <- "737" # repeated verbatim the next day in 13914
new.quote.locs[167] <- "18642"
new.quote.locs[170] <- "10341" # and repeated in 11446 11698
new.quote.locs[181] <- "17521" # and repeated in 17689, 17772, 18565"
new.quote.locs[182] <- "29020"
new.quote.locs[183] <- "193" # slight rephrase in 243
new.quote.locs[185] <- "18794"
new.quote.locs[190] <- "107"
new.quote.locs[193] <- "9076"
new.quote.locs[194] <- "3800"
new.quote.locs[198] <- "19234"
new.quote.locs[201] <- "22233"
new.quote.locs[202] <- "15722"
new.quote.locs[203] <- "18483"
new.quote.locs[207] <- "11159"
new.quote.locs[215] <- "6166"
new.quote.locs[217] <- "14158" #repeated by the same author in 14793
new.quote.locs[220] <- "4590"
new.quote.locs[221] <- "9481"
new.quote.locs[222] <- "32430" # repeated by the same author in 32480
new.quote.locs[224] <- "14799"
new.quote.locs[226] <- "20849"
new.quote.locs[227] <- "30140"
new.quote.locs[230] <- "31394"
new.quote.locs[231] <- "26538"
new.quote.locs[239] <- "13983"
new.quote.locs[243] <- "12924" # repeated in another thread in 15870
new.quote.locs[244] <- "26338"
new.quote.locs[246] <- "18888"
new.quote.locs[250] <- "18074"
new.quote.locs[253] <- "31567"
new.quote.locs[260] <- "29825"
new.quote.locs[264] <- "29359"
new.quote.locs[269] <- "12752"
new.quote.locs[271] <- "15286"
new.quote.locs[274] <- "12709"
new.quote.locs[278] <- "23897"

## Workbook for finding mathces
  quotes[115]
  findMe("most challenging problem I get in my projects ")
  View(world[c(  12924, 15870),])
  threaded.docs[substr(threaded.docs$id,1,20) %in% substr(world[c( 17521, 17689, 17772, 18565),"commentid"],1,20),]
  threaded.docs[substr(threaded.docs$id,1,20) == substr(world[c( 1211),"commentid"],1,20),]
####

save(new.quote.locs, file="place_docs_here/final_quote_locations")
# output the quotes and the quoted comments
quoted.texts = data_frame(as.quoted=quotes,
                          as.commented=world[new.quote.locs, "text"])
write.csv(quoted.texts, file="outputs/quoted_comments.csv")



# first fix the ids to the updated world id format
titles <- tolower(gsub("\\?","",world$title))
# then remove the non-ascii characters and append to the supposed-to-be-unique part 
# of the comment id
world$newid <- paste(substring(world$commentid,2,20), 
                         substring(iconv(titles, "UTF-8", "ASCII", sub=""), 1, 20), 
                         sep="."
) 
# then pull out the new ids for the identified quotes
quoted.idxs <- as.numeric(new.quote.locs[!is.na(as.numeric(new.quote.locs))])
quoted.ids <- world[quoted.idxs, "new_id"]


# we'll use final.world to map from the new_id field in world to the revised ids in the world.dt
titles.fin <- gsub("\\?","",final.world$title)
final.world$newid <- paste(substring(final.world$commentid,2,20), 
                     substring(iconv(titles.fin, "UTF-8", "ASCII", sub=""), 1, 20), 
                     sep="."
) 

sum(final.world$new_id %in% quoted.ids)
quoted.new.ids <- final.world[final.world$new_id %in% quoted.ids, "newid"]


# load the full analysis set of documents from prior work
load("place_docs_here/th_doc_topics.Rdata")

# keep just the world docs
quoted.thdt <- th.doc.topics %>%
  mutate(quoted = ifelse(id %in% quoted.new.ids, 1, 0))

sum(quoted.thdt$id %in% quoted.new.ids)
save(quoted.thdt, file="place_docs_here/quoted_th_doc_topics")
#  load("place_docs_here/quoted_th_doc_topics")

load("place_docs_here/world_stm_30.Rdata")
model <- stm.fit.30
# just the docs that are in the STM and in the same order
analysis.set <- select(prepped.docs$meta, id) %>% 
  left_join(quoted.thdt) %>%
  select(-focus, -adj.focus, -adj.conc.sq, -solo.topic)


# Add in the topic model stats
d.tps <- model$theta
# calculate the topic focus for each document
analysis.set$focus <- rowSums(d.tps^2)
analysis.set$focus.sq <- analysis.set$focus^2
analysis.set$focus.std <- (analysis.set$focus - mean(analysis.set$focus))/sd(analysis.set$focus)
analysis.set$log.length.sq <- analysis.set$log.length ^ 2
analysis.set$log.length.std <- (analysis.set$log.length - mean(analysis.set$log.length))/sd(analysis.set$log.length)
analysis.set$log.length.std.sq <- analysis.set$log.length.std ^ 2
analysis.set$focus.log.length.std <- analysis.set$focus.std * analysis.set$log.length.std
analysis.set$focus.log.length <- analysis.set$focus * analysis.set$log.length

analysis.set$length.over.25 <- ifelse(analysis.set$length>25, 1, 0)
analysis.set$length.over.50 <- ifelse(analysis.set$length>50, 1, 0)
analysis.set$length.over.100 <- ifelse(analysis.set$length>100, 1, 0)

analysis.set$excite..length.25 <- analysis.set$length.over.25 * analysis.set$exp.excite.20
analysis.set$excite..length.50 <- analysis.set$length.over.50 * analysis.set$exp.excite.20
analysis.set$excite..length.100 <- analysis.set$length.over.100 * analysis.set$exp.excite.20

analysis.set$focus.sq <- analysis.set$focus ^ 2

analysis.set$has.ibm.mgr <- analysis.set$docs.have.ibm * analysis.set$is.manager
analysis.set$has.ibm.exec <- analysis.set$docs.have.ibm * analysis.set$is.exec

ggplot(data=analysis.set , aes(x=focus)) + geom_density()
ggplot(data=analysis.set , aes(x=log.length, focus)) + geom_point() + geom_smooth()
ggplot(data=th.doc.topics , aes(x=log.length, focus)) + geom_point() + geom_smooth()

ggplot() + geom_density(aes(x=as.vector(d.tps))) 

ggplot() + geom_density(aes(x=as.vector(doc.topics.unsmooth)))

plotTopicShares <- function(which.rows, title) {
  overall.topic.share <- colSums(d.tps) / sum(d.tps)
  just.the.targets <- d.tps[which.rows,]
  print(nrow(just.the.targets))
  target.topic.shares <- colSums(just.the.targets) / sum(just.the.targets)
  #plt.1 <- qplot(x=1:length(target.topic.shares), y=target.topic.shares) + 
  #  ylim(0, 0.1) +
  #  ggtitle(title)
  
  topic.diffs <- target.topic.shares - overall.topic.share
  plt.2 <- qplot(x=1:length(topic.diffs), y=topic.diffs, geom="text", label=1:length(topic.diffs)) + 
    ylim(-0.04, 0.04) +
    ggtitle(title)
  
  return(list(target.topic.shares, topic.diffs, plt.2))
}

# compare topics of quoted comments to overall topics
plotTopicShares(which(analysis.set$quoted==1), "Quoted Comments")

# what do execs look like?
plotTopicShares(which(analysis.set$is.exec==1), "Execs")
plotTopicShares(which(analysis.set$new.mgr=="executive" & analysis.set$newer.mgr!="executive"), "Execs")
# line managers
plotTopicShares(which(analysis.set$is.manager==1), "Managers")
plotTopicShares(which(analysis.set$is.manager==0 & analysis.set$is.exec==0), "Non-Managers")

# gender
plotTopicShares(which(analysis.set$gender=="male"), "Boys")
plotTopicShares(which(analysis.set$gender=="female"), "Girls")
plotTopicShares(which(analysis.set$gender=="unknown"), "Unkown Gender")

# placement in thread
plotTopicShares(which(analysis.set$generation==1), "Top-Level")
plotTopicShares(which(analysis.set$n.children>1), "Starts Convo")


overall.topic.share <- colSums(d.tps) / sum(d.tps)
overall.over.15 <- colSums(d.tps >= 0.15)/nrow(d.tps)

just.the.targets <- d.tps[which(analysis.set$is.exec==1),]
exec.topic.shares <- colSums(just.the.targets) / sum(just.the.targets)
exec.over.15 <- colSums(just.the.targets >= 0.15)/nrow(just.the.targets)

just.the.targets <- d.tps[which(analysis.set$is.manager==1),]
manager.topic.shares <- colSums(just.the.targets) / sum(just.the.targets)
manager.over.15 <- colSums(just.the.targets >= 0.15)/nrow(just.the.targets)

just.the.targets <- d.tps[which(analysis.set$is.manager==0 & analysis.set$is.exec==0),]
nonmanager.topic.shares <- colSums(just.the.targets) / sum(just.the.targets)
nonmanager.over.15 <- colSums(just.the.targets >= 0.15)/nrow(just.the.targets)

out.df <- data.frame(
  topic = 1:30,
  overall.shares = overall.topic.share,
  overall.over.15 = overall.over.15,
  exec.shares = exec.topic.shares,
  exec.over.15 = exec.over.15,
  manager.shares = manager.topic.shares,
  manager.over.15 = manager.over.15,
  nonmanager.shares = nonmanager.topic.shares,
  nonmanager.over.15 = nonmanager.over.15
)
write.csv(out.df, file = "outputs/topic_shares_by_title_for_paul.csv", row.names = F)

######
# Average Focus By Topic
######

avg.topic.focus <- colSums(analysis.set$focus * d.tps) / colSums(d.tps) / mean(analysis.set$focus)
qplot(x=1:length(avg.topic.focus), y=avg.topic.focus)


############
# Add in intercomment focus
############

d.tps.u <- model$theta
colnames(d.tps.u) <- paste0("topic.", 1:30, ".prev")

parent.child.set <- select(analysis.set, id, parent, new.mgr) %>%
  cbind(d.tps.u) 

child.tps <-  parent.child.set %>%
  inner_join(select(parent.child.set, -parent), by=c("parent"="id"), suffix=c(".child", ".parent")) %>%
  mutate(parent.child.mgr = sprintf("%s-%s", new.mgr.parent, new.mgr.child)) 

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


sd.js <- sd(child.tps$focus.js)
sd.kld <- sd(child.tps$focus.kld)
sd.cos <- sd(child.tps$focus.cos)
analysis.set <- select(analysis.set, -focus.js, -focus.cos, -focus.kld)
analysis.set.2 <- analysis.set %>%
  left_join(select(child.tps, id, focus.js, focus.cos, focus.kld), by ="id") %>%
  mutate(focus.js=ifelse(is.na(focus.js), 0, focus.js),
         focus.js.cent=ifelse(focus.js==0, 0, focus.js - mean(focus.js[focus.js>0])),
         focus.cos.cent=ifelse(focus.cos==0, 0, focus.cos - mean(focus.cos[focus.cos>0], na.rm=T)),
         focus.cos.cent=ifelse(is.na(focus.cos), 0, focus.cos.cent),
         focus.cos=ifelse(is.na(focus.cos), 0, focus.cos),
         focus.kld=ifelse(is.na(focus.kld), 0, focus.kld),
         focus.kld.cent=ifelse(focus.kld==0, 0, focus.kld - mean(focus.kld[focus.kld>0])),
         focus.js.std = focus.js.cent / sd.js,
         focus.kld.std = focus.kld.cent / sd.kld,
         focus.cos.std = focus.cos.cent / sd.cos
  )

ggplot(filter(analysis.set, is.first.comment==F & missing.parent==F)) + geom_density(aes(x=focus.cos))
ggplot(filter(quoted.thdt, is.first.comment==F & missing.parent==F & jam =="world")) + geom_density(aes(x=focus.cos))


analysis.set <- analysis.set.2

# add the question mark measures
analysis.set$anyQ <- grepl("\\?", ignore.case=T, analysis.set$text)
# terminal ?
analysis.set$terminalQ <- "?" == substr(analysis.set$text,nchar(analysis.set$text),nchar(analysis.set$text)+1)

analysis.set$has.ibm <- grepl("ibm[ .,!-_']", analysis.set$text, ignore.case=T)

save(analysis.set, file="place_docs_here/quoted_comments_with_stats.Rdata")
# load("place_docs_here/quoted_comments_with_stats.Rdata")


# zero-order
ggplot(analysis.set, aes(x=focus, y=quoted)) + 
  geom_point(height=0.02, width=0.1) +
  geom_smooth(method="loess") 

#######################
# Recombination
#######################
jensenShannon <- function(x, y) {
  m <- 0.5*(x + y)
  0.5*sum(x*log(x/m)) + 0.5*sum(y*log(y/m))
}
KLD <- function(x,y) sum(x * log(x/y))
symKLD <- function(x,y) (0.5 * KLD(x, y)) + (0.5 * KLD(y, x))
cosSimil <- function(x,y) sum(x * y) / (sqrt(sum(x^2)) * sqrt(sum(y^2)))


# how many pairs of words would we be looking up the JS-distance of?
sum(unlist(lapply(prepped.docs$documents, ncol))^2) / 2
# 18 M
# if we pre-compute all words, it would be:
length(model$vocab)^2
# 30 M
# so, better to compute each on the fly

beta <- exp(model$beta$logbeta[[1]])


intra.comment.simil <- rep(NA,length(prepped.docs$documents))
for (idx in 1:length(prepped.docs$documents)) {
  this.doc <- prepped.docs$documents[[idx]]
  if (ncol(this.doc) == 1) {
    intra.comment.simil[idx] <- 1
  } else {
    # start with the simlarity of 1 for the diagonal in the running total
    running.tot <- sum(this.doc[2,])
    for (col.x in 1:(ncol(this.doc)-1)) {
      for (col.y in (col.x + 1):ncol(this.doc)) {
        word.x.topics <- beta[,this.doc[1, col.x]]
        word.y.topics <- beta[,this.doc[1, col.y]]
        running.tot <- running.tot + 2 * this.doc[2, col.x] * this.doc[2, col.y] * cosSimil(word.x.topics, word.y.topics)
      }
    }
    intra.comment.simil[idx] <- running.tot / (sum(this.doc[2,])^2)
  }
}
analysis.set$intra.comment.simil <- intra.comment.simil
analysis.set$recombination <- 1 - intra.comment.simil

token.counts <- rep(NA,length(prepped.docs$documents))
unique.token.counts <- rep(NA,length(prepped.docs$documents))
for (idx in 1:length(prepped.docs$documents)) {
  this.doc <- prepped.docs$documents[[idx]]
  token.counts[idx] <- sum(this.doc[2,])
  unique.token.counts[idx] <- ncol(this.doc)
}
analysis.set$token.counts <- token.counts
analysis.set$unique.token.counts <- unique.token.counts    

ggplot(analysis.set) + geom_density(aes(x=intra.comment.simil))
ggplot(analysis.set) + geom_density2d(aes(y=intra.comment.simil, x=focus))
ggplot(analysis.set) + geom_density2d(aes(y=recombination, x=unique.token.counts))
ggplot(analysis.set) + geom_density2d(aes(y=intra.comment.simil, x=unique.token.counts))

filter(analysis.set, unique.token.counts>=8 ) %>%
  ggplot() + geom_point(aes(y=intra.comment.simil, x=log.length))
filter(analysis.set, unique.token.counts>=8 ) %>%
  ggplot() + geom_point(aes(y=focus, x=unique.token.counts))

raw.length <- unlist(lapply(analysis.set$text, function (x) {length(strsplit(x, " ")[[1]])}))
analysis.set$raw.length <- raw.length

cap <- 0.35
analysis.set$capped.intra.simil <- ifelse(analysis.set$intra.comment.simil >= cap,
                                          cap,
                                          analysis.set$intra.comment.simil)
analysis.set$capped.recomb <- 1 - analysis.set$capped.intra.simil
analysis.set$recomb.x.length <- analysis.set$log.length * analysis.set$recombination
analysis.set$capped.recomb.x.length <- analysis.set$log.length * analysis.set$capped.recomb

analysis.set$capped.recomb.std <- analysis.set$capped.recomb / sd(analysis.set$capped.recomb)
analysis.set$capped.recomb.x.length.std <- analysis.set$capped.recomb.std * analysis.set$log.length.std

save(analysis.set, file="place_docs_here/quoted_comments_with_stats.Rdata")
# load("place_docs_here/quoted_comments_with_stats.Rdata")


#### 
# compare focus calculated in the two different models
####

both.focus <- select(analysis.set, id, length, focus, text, length, quoted) %>%
  left_join(select(th.doc.topics, id, focus), by="id", suffix=c(".stm",".lda"))
both.focus.large <- filter(both.focus, length>20)
both.focus.larger <- filter(both.focus, length>50)
both.focus.med <- filter(both.focus, length<=50 & length > 10)
both.focus.small <- filter(both.focus, length<=10)
cor(both.focus.large$focus.lda, both.focus.large$focus.stm)
cor(both.focus.larger$focus.lda, both.focus.larger$focus.stm)
cor(both.focus.small$focus.lda, both.focus.small$focus.stm)
cor(both.focus.med$focus.lda, both.focus.med$focus.stm)

ggplot(both.focus.large, aes(x=focus.stm, y=focus.lda)) + 
  geom_jitter(height=0.02, width=0.1) +
  geom_smooth() 




# docs with high old focus and low new focus
both.focus <- mutate(both.focus, hi.lo = focus.lda * (1-focus.stm))
hist(both.focus$hi.lo)
sum(both.focus$hi.lo > 0.9)
hist(filter(both.focus, hi.lo > 0.8)$length)
off.texts <- filter(both.focus, hi.lo > 0.8)$text
View(off.texts)
write.csv(off.texts, file="outputs/off_focus_texts.csv")



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

# which users post in more than one forum
analysis.set <- analysis.set %>%
  group_by(user) %>%
  mutate(number.of.forums = n_distinct(forum),
         multi.forums = ifelse(number.of.forums>1, 1, 0),
         tot.words = sum(length),
         log.tot.words = log(tot.words)) %>%
  ungroup()

save(analysis.set, file="place_docs_here/quoted_comments_with_stats.Rdata")
# load("place_docs_here/quoted_comments_with_stats.Rdata")


####
# Cos v euclidian distnace scratch work
####


# the biggest concern would be that close to the center of the simplex (balanced laod across every dimension),
# that small pertubations will radically change cosine distance even though we'd not think of them as "far"
euDist <- function(x1,x2) {sqrt(sum((x1-x2)^2))}
cosSimil(a,a)
a <- rep(1,30)
b <- c(30,rep(0,29))
b2 <- c(0,30,rep(0,28))
c <- c(1.1, rep((30-1.1)/29,29))
d <- c((30-1.1)/29, 1.1, rep((30-1.1)/29,28))

euDist(a,b)
cosSimil(a,b)

euDist(b,b2)
cosSimil(b,b2)

euDist(a,c)
cosSimil(a,c)


euDist(c,d)
cosSimil(c,d)

#############
# Analyses
#############

# remember, there are some comments quoted multiple times!
sum(duplicated(quoted.idxs))
sum(duplicated(quoted.ids))

do.hurdles <- F
topic.interaction <- ""



# STANDARDIZED
# "capped.recomb.x.length"
#focal.vars.noid <- c("focus.cos.std", "focus.std", "log.length.std", "log.length.std.sq", "focus.log.length.std", 
#                     "novelty.avg", "solo.topic", "no.topic", "is.first.comment", "missing.parent") 
# NOT STANDARDIZED
focal.vars.noid <- c("focus.cos", "focus",  "log.length", 
                     "novelty.max", "solo.topic", "no.topic", "is.first.comment", "missing.parent",
                     "multi.forums", "log.tot.words", "dist.from.exec") 

Q.controls <- c("is.manager", "is.exec", "gender", "has.ibm",
                "u.s.time.", "continent2", "last.period", "forum", "anyQ", "terminalQ")

controls <- c(focal.vars.noid, "exp.excite.20", Q.controls)
# controls <- c(focal.vars.noid, "exp.excite.20", "excite..length.50", "length.over.50", Q.controls)
# controls <- c(focal.vars.noid, "identity.i_", "exp.excite.20.std", Q.controls)


##########
# NOTE: These used to automatically include "n.children", "responded" but no longer
#########



summary(glm(quoted ~ solo.topic + novelty.max.std + focus.std, data = analysis.set, family="binomial"))
quote.fit.10 <- cutQuoteModel(prev.thresh=0.1, controls, filter.set = which(analysis.set$length>10))
summary(quote.fit.10)
# plot(fit.world.cut)


quote.fit.notop <- cutQuoteModel(prev.thresh=NULL, controls, filter.set = which(analysis.set$length>10))
summary(quote.fit.notop)
quote.fit.15 <- cutQuoteModel(prev.thresh=0.15, controls, filter.set = which(analysis.set$length>10))#, 
summary(quote.fit.15)

these.controls <- controls[c(-1,-2,-4:-6, -9:-11, -17,-18,-20)]
qf.10.f1 <- cutQuoteModel(prev.thresh=0.1, these.controls, 
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 1"))#, 
qf.10.f2 <- cutQuoteModel(prev.thresh=0.1, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 2"))#, 
qf.10.f3 <- cutQuoteModel(prev.thresh=0.1, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 3"))#, 
qf.10.f4 <- cutQuoteModel(prev.thresh=0.1, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 4"))#, 
qf.10.f5 <- cutQuoteModel(prev.thresh=0.1, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 5"))#, 
qf.10.f6 <- cutQuoteModel(prev.thresh=0.1, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 6"))#, 
stargazer(qf.10.f1, qf.10.f2, qf.10.f3, qf.10.f4, qf.10.f5, qf.10.f6,
          type='text', #out="outputs/overall_prob_quoted_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))

these.controls <- controls[c(-1,-2,-4:-6, -9:-11, -17,-18,-20)]
these.controls <- c()
qf.15.f1 <- cutQuoteModel(prev.thresh=0.15, these.controls, 
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 1"))#, 
qf.15.f2 <- cutQuoteModel(prev.thresh=0.15, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 2"))#, 
qf.15.f3 <- cutQuoteModel(prev.thresh=0.15, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 3"))#, 
qf.15.f4 <- cutQuoteModel(prev.thresh=0.15, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 4"))#, 
qf.15.f5 <- cutQuoteModel(prev.thresh=0.15, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 5"))#, 
qf.15.f6 <- cutQuoteModel(prev.thresh=0.15, these.controls,
                          filter.set = which(analysis.set$length>10 & analysis.set$forum=="forum 6"))#, 
stargazer(qf.15.f1, qf.15.f2, qf.15.f3, qf.15.f4, qf.15.f5, qf.15.f6,
          type='text', #out="outputs/overall_prob_quoted_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))


summary(cutQuoteModel(prev.thresh=0.15, controls[c(-3,-6,-7,-8)]))
exp(-coef(quote.fit.15)['log.length']/(2*coef(quote.fit.15)['log.length.sq']))

stargazer(quote.fit.15,
          type='text', out="outputs/overall_prob_quoted_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))

quote.fit.15.noshort <- cutQuoteModel(prev.thresh=0.15, controls, 
                              filter.set = which(analysis.set$length>=10))
summary(quote.fit.15.noshort)
# + 3, 16, 27, 28
# - 18, 

quote.fit.15.exec <- cutQuoteModel(prev.thresh=0.15, controls[c(-2, -13,-14)], 
                                      filter.set = which(analysis.set$newer.mgr=="executive" &
                                                           analysis.set$length>=10))
summary(quote.fit.15.exec)
# + 28

quote.fit.15.mgr <- cutQuoteModel(prev.thresh=0.15, controls[c(-2,-13,-14)], 
                                      filter.set = which(analysis.set$newer.mgr=="manager" &
                                                           analysis.set$length>=10))
summary(quote.fit.15.mgr)
# + 16, 3

quote.fit.15.nonmgr <- cutQuoteModel(prev.thresh=0.15, controls[c(-4,-5,-6,-13,-14)], 
                                      filter.set = which(analysis.set$newer.mgr=="other" &
                                                           analysis.set$length>=10))
summary(quote.fit.15.nonmgr)
# + 3, 27, 28
# - 18

stargazer(quote.fit.15.noshort, quote.fit.15.exec, quote.fit.15.mgr, quote.fit.15.nonmgr,
          column.labels = c("Overall","Execs","Managers","Non-Managers"),
          type='text', out="outputs/overall_prob_quoted_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))


quote.fit.12.noshort <- cutQuoteModel(prev.thresh=0.12, controls, 
                                      filter.set = which(analysis.set$length>=10))
summary(quote.fit.12.noshort)
# + 3, 16, 27, 28
# - 18, 

quote.fit.12.exec <- cutQuoteModel(prev.thresh=0.12, controls[c(-10,-11)], 
                                   filter.set = which(analysis.set$newer.mgr=="executive" &
                                                        analysis.set$length>=10))
summary(quote.fit.12.exec)
# + 28

quote.fit.12.mgr <- cutQuoteModel(prev.thresh=0.12, controls[c(-10,-11)], 
                                  filter.set = which(analysis.set$newer.mgr=="manager" &
                                                       analysis.set$length>=10))
summary(quote.fit.12.mgr)
# + 16, 3

quote.fit.12.nonmgr <- cutQuoteModel(prev.thresh=0.12, controls[c(-10,-11)], 
                                     filter.set = which(analysis.set$newer.mgr=="other" &
                                                          analysis.set$length>=10))
summary(quote.fit.12.nonmgr)
# + 3, 27, 28
# - 18

stargazer(quote.fit.12.noshort, quote.fit.12.exec, quote.fit.12.mgr, quote.fit.12.nonmgr,
          column.labels = c("Overall","Execs","Managers","Non-Managers"),
          type='text', out="outputs/overall_prob_quoted_odds_ratios.txt",
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))



# model for each of the highly quoted topics (p>0.15)
hq.thresh <- 0.15
novsq.cont <- c(controls[1:4], "novelty.max.sq", controls[c(-1:-4, -5, -6, -14, -15, -17)])
# novsq.cont <- c(controls[1:4], controls[c(-1:-4, -5, -6, -14, -15, -17)])
quote.fit.t3 <- cutQuoteModel(prev.thresh=NULL, novsq.cont, 
                              filter.set = which(model$theta[,3] >= 0.15 &
                                                   analysis.set$length>=10))
quote.fit.t16 <- cutQuoteModel(prev.thresh=NULL, novsq.cont, 
                              filter.set = which(model$theta[,16] >= 0.15 &
                                                   analysis.set$length>=10))
quote.fit.t27 <- cutQuoteModel(prev.thresh=NULL, novsq.cont, 
                              filter.set = which(model$theta[,27] >= 0.15 &
                                                   analysis.set$length>=10))
quote.fit.t28 <- cutQuoteModel(prev.thresh=NULL, novsq.cont, 
                              filter.set = which(model$theta[,28] >= 0.15 &
                                                   analysis.set$length>=10))

stargazer(quote.fit.t3, quote.fit.t16, quote.fit.t27, quote.fit.t28,
          column.labels = c("Topic 3", "Topic 16", "Topic 27", "Topic 28"),
          type='text', no.space = T,
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))



hist(filter(analysis.set, solo.topic==F & no.topic==F & is.manager==T & quoted==0)$novelty.max, breaks = 100)


min(filter(analysis.set, quoted==T)$length)

quote.fit.20 <- cutQuoteModel(prev.thresh=0.20, controls)
summary(quote.fit.20)


# what percent of quoted and non quoted docs repsond within 10 minutes to their parent doc?
nrow(filter(analysis.set, sec.since.parent<600 & quoted==F))/
    nrow(filter(analysis.set, is.first.comment==F & missing.parent==F & quoted==F))

nrow(filter(analysis.set, sec.since.parent<600 & quoted==T))/
    nrow(filter(analysis.set, is.first.comment==F & missing.parent==F & quoted==T))





  
focalCoefPlot(quote.fit.15, 20, 
              "STM Response Coefficient Estimates\nwith 95% Confidence Intervals",
              identities=F)#,
              #ylim=c(-0.3,0.25))
  
  # better might be just some summary stats
  View(analysis.set %>%
    group_by(quoted) %>%
    summarise(n(), 
              log.len = mean(log.length), 
              n.children = mean(n.children), 
              any.response = mean(responded), 
              intra.focus = mean(focus), 
              inter.focus = mean(focus.js), 
              excite = mean(exp.excite.20.std),
              manager = mean(is.manager),
              exec = mean(is.exec),
              male = mean(gender=="male")))
  

stargazer(values.non.stand, world.non.stand, column.labels=c("Values","World"),
          type='text', out=paste0(output.dir,"/overall_prob_response_odds_ratios.txt"),
          report = "vct*", t.auto=F, p.auto=F, apply.coef=exp,
          star.cutoffs=c(0.05, 0.01, 0.001))

quoted.ids[!(quoted.ids %in% world.dt$newid)]

View(world[c(18924, 30867),])



# Are top-level comments ever quoted?
table(quoted.thdt$quoted, quoted.thdt$generation)[2,]/table(quoted.thdt$quoted, quoted.thdt$generation)[1,]
table(quoted.thdt$quoted, quoted.thdt$is.first.comment)


# Are childless comments ever quoted
table(quoted.thdt$quoted, quoted.thdt$responded)

# shortest quoted comment
min(quoted.thdt$length)
min(quoted.thdt[quoted.thdt$quoted, 'length'])
  # how many comments are that long?
  sum(quoted.thdt$length >= min(quoted.thdt[quoted.thdt$quoted, 'length']))  
  
  
# Do isolates get quoted?
isolated = quoted.thdt$responded==F & quoted.thdt$generation==1
table(quoted.thdt$quoted, isolated)  


# containing question marks
qst.idx <- grep("\\?", ignore.case=T, world$text)

# terminal question marks
sum("?" == substr(world$text,nchar(world$text),nchar(world$text)+1))


# test whether there are non-zero topics in each forum

apply(d.tps[analysis.set$forum=="forum 1",],2,min)
min(apply(d.tps[analysis.set$forum=="forum 2",],2,min))
min(apply(d.tps[analysis.set$forum=="forum 3",],2,min))
min(apply(d.tps[analysis.set$forum=="forum 4",],2,min))
min(apply(d.tps[analysis.set$forum=="forum 5",],2,min))
min(apply(d.tps[analysis.set$forum=="forum 6",],2,min))



library(randomForest)
controls
these.controls <- controls[c(-9, -11, -18)]
theta.set <- model$theta
colnames(theta.set) <- sprintf("t%02s", 1:ncol(theta.set))
rf.dset <- cbind(select_(analysis.set, .dots=c("n.children", "responded", "quoted", these.controls)),
                 theta.set)
formula.text <- paste0("as.factor(quoted) ~ ", paste(paste0("t", 1:ncol(theta.set)), collapse = "+"))
formula <- as.formula(formula.text)
fit <- randomForest(formula,
                    data=rf.dset, 
                    importance=TRUE, 
                    ntree=20)

fit
varImpPlot(fit)
summary(fit)
plot(fit)

library(rpart)
fit.tree <- rpart(quoted ~ .,
                  data=rf.dset,
                  weights = ifelse(rf.dset$quoted==1, 1, 275/(31329-275)),
                  method="class")
fit.tree
summary(fit.tree)
par(mfrow = c(1,2), xpd = NA) # otherwise on some devices the text is clipped
plot(fit.tree)
formula




# So, trees ain't gonna do shit
# let's look at the distributions for each covariate
outFile <- function(name) {sprintf("outputs/dists/%s.png", name)}



sumStatMaker <- function(mm) {  
  n <- nrow(mm)
  print(sprintf("n=%d", n))
  quantiles <- t(apply(mm, 2, function(x) {round(quantile(x, na.rm=T), num.dig)}))
  colnames(quantiles) <- c("Min", "Quant.25", "Median", "Quant.75", "Max")
  sum.stats <- data.frame(
    Mean = round(colMeans(mm, na.rm=T),num.dig),
    StDev = round(apply(mm, 2, sd, na.rm=T),num.dig),
    st.err = round(apply(mm, 2, sd, na.rm=T)/sqrt(n-1),num.dig),
    stringsAsFactors = F
  )
  sum.stats <- cbind(sum.stats, data.frame(quantiles, stringsAsFactors = F))
  sum.stats <- data.frame(name=colnames(mm), sum.stats)
  return(sum.stats)
}

sumStatsCompare <- function(mm) {
  rbind(data.frame(quoted=1, sumStatMaker(filter(mm, quoted==1))),
        data.frame(quoted=0, sumStatMaker(filter(mm, quoted==0)))) %>%
    arrange(name, quoted)
}


this.model <- lm(quoted ~ ., data=rf.dset)
mm <-  data.frame(model.matrix(this.model), quoted= this.model$model$quoted)
mm$forumforum.1 <- 1 - rowSums(mm[,sprintf("forumforum.%d", 2:6)])
mm$is.nonexec <- 1 - rowSums(mm[,c("is.manager","is.exec")])
mm$genderfemale <- 1 - rowSums(mm[,c("gendermale","genderunknown")])
num.dig <- 3

overall.stats <- sumStatsCompare(mm)
f1.stats <- sumStatsCompare(filter(mm, forumforum.1==1))
f2.stats <- sumStatsCompare(filter(mm, forumforum.2==1))
f3.stats <- sumStatsCompare(filter(mm, forumforum.3==1))
f4.stats <- sumStatsCompare(filter(mm, forumforum.4==1))
f5.stats <- sumStatsCompare(filter(mm, forumforum.5==1))
f6.stats <- sumStatsCompare(filter(mm, forumforum.6==1))


sum.stats$name <- row.names(sum.stats)
arrange(sum.stats, name)

tpc.labels <- apply(labelTopics(model)$frex,1,function(x){paste(x,collapse=":")})


tpc <- 3




#####
# Thread Length
#####

# confirm that all posts' generation is one greater than their parents
a.s <- analysis.set %>% select(id, parent, generation)
a.s %>% inner_join(a.s, by=c("id"="parent")) %>%
  mutate(gen.diff = generation.y-generation.x) %>%
  summarise(min(gen.diff), max(gen.diff), sum(is.na(gen.diff)))
rm(a.s)
# solid.

# Finding thread.max.len
# (1) all posts are at least  equal to their generation
analysis.set$thread.max.len <- analysis.set$generation
# (1.a.) all posts are part of at least one thread
analysis.set$number.threads <- 1
# (2) working backwards through generations, set the parent equal to the
#     largest thread.max.len among its children
# (2.a.) AND count how many threads each post is part of
working.as <- select(analysis.set, id, parent, generation, thread.max.len, number.threads)
for (g in (max(analysis.set$generation)):2) {
  this.gens.parents <- working.as %>% filter(generation==g) %>%
    group_by(parent) %>%
    summarise(max.len = max(thread.max.len),
              number.threads = sum(number.threads))
  
  
  for (r in 1:nrow(this.gens.parents)) {
    this.idx <- which(working.as$id==this.gens.parents$parent[r])
    working.as[this.idx, "thread.max.len"] <- this.gens.parents$max.len[r]
    working.as[this.idx, "number.threads"] <- this.gens.parents$number.threads[r]
  }
}
table(working.as$thread.max.len)
analysis.set$thread.max.len <- working.as$thread.max.len
analysis.set$number.threads <- working.as$number.threads
rm(working.as) 

######
# Graph Generation
######

controls
these.controls <- c(controls[c(-9, -11)], "generation", "thread.max.len", "number.threads")
theta.set <- model$theta
colnames(theta.set) <- sprintf("t%02s", 1:ncol(theta.set))
rf.dset <- cbind(select_(analysis.set, .dots=c("n.children", "responded", "quoted", these.controls)),
                 theta.set)

# just the comments of appropriate length
this.df <- filter(rf.dset, log.length > 2.2)

outFile <- function(name) {sprintf("outputs/dists/%s.png", name)}

for (tp in 1:ncol(theta.set)) {
  fil.nam <- outFile(sprintf("forum_topic_%02s", tp))
  ggsave(filename = fil.nam,
         plot = topicPrevForum(this.df, tp), 
         width = 8,
         height = 6)
}

for (tp in 1:ncol(theta.set)) {
  fil.nam <- outFile(sprintf("forum_topic_with_early_%02s", tp))
  ggsave(filename = fil.nam,
         plot = topicPrevForum(this.df, tp, forum.set.to.add = forum.kickoff.topics), 
         width = 8,
         height = 6)
}

# we want a single graph with each of the thirty topics 
plot.df <- select(this.df, quoted, sprintf("t%02s", 1:ncol(theta.set))) %>%
  gather(key=topic, value=prev, -quoted)

thresh <- 0.1
plot.df %>% group_by(topic, quoted) %>%
  mutate(has.topic = ifelse(prev >= thresh, 1, 0)) %>%
  summarise(mn.prev = mean(has.topic),
            q25 = quantile(has.topic, probs=0.25),
            q75 = quantile(has.topic, probs=0.75)) %>%
  ggplot() +
  geom_point(aes(x=topic, y=mn.prev, color=as.factor(quoted)), position=pd) +
  geom_errorbar(aes(x=topic, ymin=q25, ymax=q75, color=as.factor(quoted)), position=pd) 
  


var.list <- c("n.children", "responded", "focus", "focus.cos",
              "novelty.max", "log.length","is.manager", "is.exec",
              "is.first.comment", "anyQ", "last.period", "has.ibm")
var.list <- c("generation", "thread.max.len", "number.threads")
title.list <- c("Number Children","Any Children", "Intracomment Focus", "Intercomment Focus",
                "Novelty (max pairwise)", "Log Comment Length", "Is Manager", "Is Exec",
                "Top-Level Comment", "Contains Question Mark", "Post in Last Period", "Mentions IBM")
title.list <- c("Thread Generation", "Longest Thread Length", "Number of Threads")
for (v in 1:length(var.list)) {
  fil.nam <- outFile(sprintf("forum_%s", title.list[v]))
  ggsave(filename = fil.nam,
         plot = topicPrevForum(this.df, var.list[v], title.list[v]), 
         width = 8,
         height = 6)
}

 factor.var.list <- c("gender", "continent2")
factor.titles <- c("Gender='%s'", "Continent='%s'")
for (v in 1:length(factor.var.list)) {
  for (f in unique(this.df[,factor.var.list[v]])) {  
    this.title <- sprintf(factor.titles[v], f)
    fil.nam <- outFile(sprintf("forum_%s_%s", factor.var.list[v], f))
    
    ggsave(filename = fil.nam,
           plot = topicPrevForum(this.df, factor.var.list[v], 
                                 this.title), 
           width = 8,
           height = 6)
  }
}

v <-1
f <- unique(this.df[,factor.var.list[v]])[1]


#######
# Model Beta For LIWC Matching
#######
beta <- data.frame(t(exp(model$beta[[1]][[1]])))
names(beta) <- sprintf("t%02s", 1:ncol(theta.set))
vocab <- model$vocab

beta.df <- data.frame(words = vocab, beta, stringsAsFactors = F)
write.csv(beta.df, file="place_docs_here/stm30_beta.csv", row.names=F)

# may also want the word marginals
word.marg <- beta / rowSums(beta)
out.df <- data.frame(words = vocab, word.marg)
write.csv(out.df, file="place_docs_here/stm30_word_marginals.csv", row.names=F)

# REALLY, we just need the vocab list to do LIWC ups and pass that back here for 
# mapping into topics
write.csv(vocab, file="place_docs_here/stm30_vocab.csv", row.names=F, col.names = F)

# and bring the compiled list back in
vocab.to.liwc <- read.csv("outputs/vocab_liwc_map.csv", stringsAsFactors = F)

# for each topic, find the weighted sum for each LIWC category
liwc.topics <- vocab.to.liwc %>%
  left_join(beta.df, by=c("word"="words")) %>%
  select(-word) %>%
  group_by(liwc.cat) %>%
  summarise_all(sum)
liwc.topics$mn <- rowMeans(liwc.topics[,-1])


for (rw in 1:nrow(liwc.topics)) {
  l.cat <- liwc.topics$liwc.cat[rw]
  plt <- gather(liwc.topics[rw,-1]) %>%
    rename(topic = key) %>%
    ggplot() +
    geom_point(aes(y=topic, 
                   x=value)) +
    labs(title=sprintf("Share of topic in LIWC Category: %s",l.cat),
        subtitle=sprintf("Sums the proportion of each topic dedicated to words included in the '%s' LIWC category",l.cat)) +
    xlim(0,0.35)
  
  fil.nam <- outFile(sprintf("topic_LIWC_rates_%s", l.cat))
  ggsave(filename = fil.nam,
         plot = plt, 
         width = 6,
         height = 6)
} 




# and, finally, the docs aggregated for each forum--quoted combination
out.df <- filter(analysis.set, length >= 10) %>%
  select(forum, quoted, text) %>%
  mutate(user=sprintf("%s--%s", forum, ifelse(quoted==0,"UnChosen","Chosen"))) %>%
  group_by(user) %>%
  summarise(text = paste(text, collapse="\n"))
write.csv(out.df, file="place_docs_here/aggregate_docs_forum.csv", row.names=F)

# now, pull the results back in and graph them
forum.liwc.df <- read.csv("outputs/forum_quoted_liwc_centers.csv", stringsAsFactors = F)
for.cho <- sapply(forum.liwc.df$user, function(x){strsplit(x, split="--")[[1]]})
forum.liwc.df$forum <- for.cho[1,]
forum.liwc.df$quoted <- for.cho[2,]

for (fm in sprintf("forum %d",1:6)) {
  plt <- filter(forum.liwc.df, forum==fm) %>%
  ggplot() +
    geom_point(aes(y=as.factor(liwc.cat), 
                   x=center, 
                   color=as.factor(quoted))) + #, position=position_dodge(0.001))
    coord_cartesian(xlim=c(0,0.1)) +
    labs(title=sprintf("LIWC Rates in %s", fm),
         subtitle="For each LIWC category, we calculated the percentage of words\nused in the forum that are included in the category in question.") +
    theme(legend.position="bottom") +
    xlab("Proportion of words in Category") +
    ylab("LIWC Category")
  
  fil.nam <- outFile(sprintf("%s_LIWC_rates", fm))
  ggsave(filename = fil.nam,
         plot = plt, 
         width = 8,
         height = 6)
}


#####
# Looking at topic prev by percentile prevalence
#####

# function to get the set of topic prevs for an arbitray percentile
topicPrevByP <- function(p) {
  k <- ncol(d.tps)
  out.vec <- rep(0, k)
  for (i in 1:k) {
    out.vec[i] <- quantile(d.tps[,i], p)
  }
  return(out.vec)
}
topicPrevByP(0.9)




library(glmnet)
NFOLDS = 5
dtm_train <- convertCorpus(prepped.docs$documents, prepped.docs$vocab, type="Matrix")
subset <- analysis.set$log.length > 2.3
glmnet_classifier = cv.glmnet(x = dtm_train[subset,], y = analysis.set[subset,]$quoted, 
                              family = 'binomial', 
                              # L1 penalty
                              alpha = 1,
                              # interested in the area under ROC curve
                              type.measure = "auc",
                              # K-fold cross-validation
                              nfolds = NFOLDS,
                              # high value is less accurate, but has faster training
                              thresh = 1e-5,
                              # again lower number of iterations for faster training
                              maxit = 1e4)
plot(glmnet_classifier)
results <- coef(glmnet_classifier, s = "lambda.min")
which(results != 0)
results@x
vocab[results@i[results@x>0]]
results@Dimnames[[1]][results@i[results@x<0]]

coefList <- coef(glmnet_classifier, s='lambda.1se')
coefList <- data.frame(coefList@Dimnames[[1]][coefList@i+1],coefList@x)
names(coefList) <- c('var','val')

coefList %>%
  arrange(-abs(val)) 
  



######
# Forum Kick-offs
######
# earliest posts

early.posts <- analysis.set %>% arrange(Timestamp) %>% filter(parent=="null") %>% slice(1:17) %>% select(id, forum, title, text)
write.csv(early.posts, file="outputs/early_posts.csv", row.names = F)


early.tps <- data.frame(id=analysis.set$id, theta.set) %>%
  right_join(early.posts, by="id")
forum.kickoff.topics <- early.tps %>% 
  select(-id, -title, -text) %>%
  group_by(forum) %>%
  summarise_all(.funs=mean) %>%
  data.frame()
  
  



isKickoff <- function(txt) {grepl("Welcome to ", txt)}
analysis.set %>% filter(isKickoff(title)) %>% select(forum, title, text, parent) %>% View()

grepl("sgr", "sgrsgsef")





####
# Are selected comments threaded?
####

analysis.set %>% 
  filter(log.length > 2.3) %>%
  group_by(quoted) %>%
  mutate(tot = n()) %>%
  group_by(quoted, generation) %>%
  summarise(n=n(),
            perc=n()/max(tot))


 








