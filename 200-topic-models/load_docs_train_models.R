if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(mallet)
library(countrycode)

n.topics <- 30
wd <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-topic-model"
# wd <- "C:/Users/clarkest/Dropbox/IBM Local/ibm-code"
setwd(wd)

model.dir <- "models_dir"
model.name <- "ngram_model"
iters <- 800
maxims <- 25

###################################
#   Loading and processing data   #
###################################

values <- read.delim("place_docs_here/values-docs-ngrams.tsv", encoding="UTF-8", colClasses=c("factor", "character", "character", "character", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "factor", "factor", "factor", "factor", "factor", "factor", "character"), sep="\t", quote="")
values.nongrams <- read.delim("place_docs_here/values-docs-nongrams.tsv", encoding="UTF-8", colClasses=c("factor", "character", "character", "character", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "factor", "factor", "factor", "factor", "factor", "factor", "character"), sep="\t", quote="")

values$id <- as.factor(values$id)
values.nongrams$id <- as.factor(values.nongrams$id)
non.to.n.overlaps <- sapply(values.nongrams$id, function(x) is.element(x, values$id))
sum(!non.to.n.overlaps)
values.nongrams[!non.to.n.overlaps, "text"]
n.to.non.overlaps <- sapply(values$id, function(x) is.element(x, values.nongrams$id))
sum(!n.to.non.overlaps)
values[!n.to.non.overlaps, "text"]


world.file <- "place_docs_here/world-docs-nongrams.tsv"

#values <- read.delim("place_docs_here/nongrams-values-managers-bigrams.tsv", encoding="UTF-8", colClasses=c("factor", "character", "character", "character", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "factor", "factor", "factor", "factor", "factor", "factor", "character"), sep="\t", quote="")
#world.file <- "place_docs_here/nongrams-world-manager-bigrams.tsv"

world <- read.delim(world.file, 
                    encoding="UTF-8", 
                    colClasses=c("factor", "character", "character", "character", 
                                 "character", "factor", "factor", "factor", 
                                 "factor", "factor", "factor", "factor", 
                                 "character", "factor", "factor", "character", 
                                 "factor","character","factor", "character"), 
                    sep="\t", 
                    quote=""
)

#removing these outright to not affect the 8-hr blocks
#world[world$CreationDate == "10/29/2004",]$CreationDate <- "10/28/2004"
#world = world[!world$CreationDate == "10/29/2004",]

#stack the values and world jams into a single document set
documents <- 
  rbind(data.frame(id = as.character(values$id), text = as.character(values$text), 
                   CreationDate = values$CreationDate, 
                   CreationTime = values$CreationTime, jam="values",
                   Timestamp = strptime(paste(values$CreationDate, 
                                              values$CreationTime), '%m/%d/%Y %I:%M:%S %p', tz="US/Pacific"),
                   manager = values$manager, stringsAsFactors=F,
                   continent = countrycode(values$Country, "country.name", "continent"),
                   parent = as.character(values$Parent),
                   forum = as.character(values$Forum),
                   user = as.character(values$AuthorEmail)
  ),
  data.frame(id = as.character(world$commentid), text = as.character(world$text), 
             CreationDate = world$creation_date, 
             CreationTime=world$creation_time, jam="world",
             Timestamp = strptime(paste(world$creation_date, 
                                        world$creation_time), '%m/%d/%Y %H:%M:%S', tz="US/Pacific"),
             manager = world$manager, stringsAsFactors=F,
             continent = countrycode(world$country, "country.name", "continent"), 
             parent = as.character(world$parent),
             forum = as.character(world$forum),
             user = as.character(world$author_email)
  )
  )

# divide each day into ceiling(24/hours.per.window) windows 
#   of hours.per.window hours
hours.per.window <- 8
documents$DateWindow <- 
  paste(strftime(documents$Timestamp,"%Y-%m-%d"),
        trunc(as.numeric(strftime(documents$Timestamp,"%H"))/hours.per.window)
  )
documents$DateWindow <- factor(documents$DateWindow)

documents$manager <- factor(documents$manager)
documents$jam <- factor(documents$jam)
documents$forum <- factor(documents$forum)


##################################
#    Mallet Topic Model Loading  #
##################################
## Create a mallet instance list object. Right now I have to specify the stoplist
##  as a file, I can't pass in a list from R.
## This function has a few hidden options (whether to lowercase, how we 
##   define a token). See ?mallet.import for details.
mallet.instances <- mallet.import(documents$id, 
                                  documents$text, 
                                  "en.txt", 
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]")
#token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

# persist the documents so that we don't need to go through these steps everytime.  
file.name <- paste0(paste(model.dir, model.name, sep="/"), "-docs.Rdata")
save(documents, file=file.name)

source('lda_visualize.R')
# now, run 10 of the models and see how they compare
for (i in 1:1) {
  # create and train a topic model from the mallet.instances
  new.topic.model <- MalletLDA(num.topics=n.topics)
  new.topic.model$loadDocuments(mallet.instances)
  new.topic.model$setAlphaOptimization(20, 50)
  new.topic.model$train(iters)
  new.topic.model$maximize(maxims)
  model.label = paste(model.name, iters, maxims, formatC(i, width=2, flag="0"), sep="-")
  create.ldavis(new.topic.model, model.dir, model.label)
}