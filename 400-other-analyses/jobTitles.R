jobs <- summarize(group_by(documents, user), job.title=max(job))[,"job.title"]
job.words <- data.frame(words=unlist(strsplit(jobs$job.title," ")))
job.words$words <- gsub("[[:punct:]]", "", job.words$words)
job.words <- filter(job.words, !(words %in% c("")))
job.words$words <- factor(tolower(job.words$words))
job.word.counts <- data.frame(table(job.words))
job.word.counts[order(job.word.counts$Freq, decreasing=T),][1:500,]
?order
?sapply

# make sure to hold on to the user name -- it's the only way back
jobs <- summarize(group_by(documents, user), job.title=max(job))
# clean up the titles
job.titles <- jobs$job.title
job.titles <- tolower(job.titles)
job.titles <- gsub("h\\.r\\.", "hr", job.titles)
job.titles <- gsub("a\\/r", "ar", job.titles)
job.titles <- gsub("i\\.t\\.", "it", job.titles)
job.titles <- gsub("i\\/t", "it", job.titles)
job.titles <- gsub("\\be-", "e", job.titles)
job.titles <- gsub("\\bz\\/", "z", job.titles)
job.titles <- gsub("m\\/a", "mergers and acquisitions", job.titles)
job.titles <- gsub("m\\&a", "mergers and acquisitions", job.titles)
job.titles <- gsub("\\bmgr\\b", "manager", job.titles)
job.titles <- gsub("[,\\.\\/]", " ", job.titles)
job.titles <- gsub("\\bsr\\b", "senior", job.titles)

job.titles <- gsub("[[:punct:]]", "", job.titles)
job.titles <- gsub("\\s\\s+", " ", job.titles)
#job.titles <- job.titles[!(job.titles %in% c(""))]
View(job.titles)
jobs$job.title <- job.titles


#####################
#  co-occurence     #
#####################

GetCoTokens <- function(doc) {
  words <- unique(strsplit(doc, split=" ")[[1]])
  pairs <- expand.grid(words, words, stringsAsFactors=F)
  pairs <- pairs[pairs$Var1 < pairs$Var2,]
  return (paste(pairs$Var1, pairs$Var2, sep=":"))
}

job.word.pairs <- unlist(sapply(jobs$job.title, GetCoTokens))

# we need the counts for pairs
word.pair.counts <- data.frame(table(job.word.pairs), stringsAsFactors=F)

# we also need word counts, weighed by the number of unique words in the title
# but, it'd be easier to just count the number of times the word is in a pair
f <- head(word.pair.counts)  
a <- strsplit(sapply(word.pair.counts$job.word.pairs, toString), split=":")
word.freq <- data.frame(table(unlist(a)))
word.freq.lookup <- split(word.freq$Freq, word.freq$Var1)

# raw counts of words
job.words <- data.frame(words=unlist(strsplit(jobs$job.title," ")))
job.word.counts <- data.frame(table(job.words))
raw.freq.lookup <- split(job.word.counts$Freq, job.word.counts$job.words)
lookup.freq <- function(word) { 
  return(as.numeric(raw.freq.lookup[toString(word)]))
}

# function to calc the mutual information of a given word pair
# I(w1, w2) = log(p(w1,w2) / p(w1)p(w2))
n.titles <- nrow(jobs)
library(stringr)
word.pair.counts <- cbind(word.pair.counts, 
                          str_split_fixed(word.pair.counts$job.word.pairs, ":", 2))
p.w1.w2 <- word.pair.counts$Freq / n.titles

word.pair.counts$freq.1 <- sapply(word.pair.counts[,"1"], lookup.freq)
word.pair.counts$freq.2 <- sapply(word.pair.counts[,"2"], lookup.freq) 
p.w1 <- word.pair.counts$freq.1 / n.titles
p.w2 <- word.pair.counts$freq.2 / n.titles
word.pair.counts$mut.info <- log(p.w1.w2 / (p.w1 * p.w2))

word.pair.counts[order(word.pair.counts$mut.info, decreasing=T),][1:500,c(1,2,5)]

##############
# bi grams   #
##############

GetBiGrams <- function(doc) {
  words <- strsplit(doc, split=" ")[[1]]
  pairs <- paste(head(words, -1), words[-1], sep=":")
  return (pairs)
}
bigrams <- unlist(sapply(jobs$job.title, GetBiGrams))
bigram.counts <- data.frame(table(bigrams), stringsAsFactors=F)
n.titles <- nrow(jobs)
bigram.counts <- cbind(bigram.counts, 
                          str_split_fixed(bigram.counts$bigrams, ":", 2))
p.w1.w2 <- bigram.counts$Freq / n.titles

bigram.counts$freq.1 <- sapply(bigram.counts[,"1"], lookup.freq)
bigram.counts$freq.2 <- sapply(bigram.counts[,"2"], lookup.freq) 
p.w1 <- bigram.counts$freq.1 / n.titles
p.w2 <- bigram.counts$freq.2 / n.titles
bigram.counts$mut.info <- log(p.w1.w2 / (p.w1 * p.w2))

bigram.counts[order(bigram.counts$mut.info, decreasing=T),][1:50,c(1,2,5,6,7)]

View(bigram.counts)



library("git2r")
hash("email@email.com")


job.counts <- data.frame(table(job.titles))
job.counts[order(job.counts$Freq, decreasing=T),][1:100,]


?hist
ggplot(documents, aes(x=doc.len)) + geom_histogram(aes(y = ..density.., fill = ..count..)) + geom_density() + scale_x_log10() 
?scale_x_log10
