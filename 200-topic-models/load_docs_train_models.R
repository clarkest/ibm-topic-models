if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(mallet)
library(countrycode)

n.topics <- 30

this.dir = "/users/clarkbernier/Dropbox/IBM Local/ibm-topic-model/"
setwd(this.dir)
model.dir <- "models_dir"
model.name <- "anchor_ngram"
value_file<-"place_docs_here/hashed-values-docs-ngrams.tsv"
world.file <- "place_docs_here/hashed-world-docs-ngrams.tsv"
iters <- 800
maxims <- 100
model_ids <- c(1)


###################################
#   Loading and processing data   #
###################################

values <- read.delim(value_file, encoding="UTF-8", colClasses=c("factor", "character", "character", "character", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "factor", "factor", "factor", "factor", "factor", "factor", "character"), sep="\t", quote="")
world <- read.delim(world.file, 
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
# update all world comment ids and parent ids to include the title so that we no longer have duplicate ids
# need to strip out the ? marks from titles to match the SQL ids
titles <- gsub("\\?","",world$title)
# then remove the non-ascii characters and append to the supposed-to-be-unique part 
# of the comment id
world$commentid <- paste(substring(world$commentid,2,20), 
                         substring(iconv(titles, "UTF-8", "ASCII", sub=""), 1, 20), 
                         sep="."
                         ) 
world$parent_comment_id <- ifelse(world$parent_comment_id=='null','null',
                                  paste(substring(world$parent_comment_id,2,20), 
                                        substring(iconv(titles, "UTF-8", "ASCII", sub=""), 1, 20), 
                                        sep=".")
                            ) 

#   write.table(world, file="place_docs_here/world_with_fixed_ids.tsv", sep="\t", row.names=F)
#removing these outright to not affect the 8-hr blocks
#world[world$CreationDate == "10/29/2004",]$CreationDate <- "10/28/2004"
#world = world[!world$CreationDate == "10/29/2004",]

#stack the values and world jams into a single document set
documents <- 
rbind(
  data.frame(id = as.character(values$id), text = as.character(values$text), 
                   CreationDate = values$CreationDate, 
                   CreationTime = values$CreationTime, jam="values",
                   Timestamp = strptime(paste(values$CreationDate, 
                                              values$CreationTime), '%m/%d/%Y %I:%M:%S %p', tz="US/Pacific"),
                   manager = values$manager, stringsAsFactors=F,
                   continent = countrycode(values$Country, "country.name", "continent"),
                   country = values$Country,
                   parent = as.character(values$Parent),
                   forum = as.character(values$Forum),
                   user = as.character(values$AuthorEmail),
                   job = as.character(values$JobResp),
                   office = as.character(values$Office),
                   title = as.character(values$Name)
  ),
  data.frame(id = as.character(world$commentid), text = as.character(world$text), 
             CreationDate = world$creation_date, 
             CreationTime=world$creation_time, jam="world",
             Timestamp = strptime(paste(world$creation_date, 
                                        world$creation_time), '%m/%d/%Y %H:%M:%S', tz="US/Pacific"),
             manager = world$manager, stringsAsFactors=F,
             continent = countrycode(world$country, "country.name", "continent"), 
             country = world$country,
             parent = as.character(world$parent),
             forum = as.character(world$forum),
             user = as.character(world$author_email),
             job = as.character(world$jobresp),
             office = as.character(world$office),
             title = as.character(world$title)
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

documents$continent <- ifelse(documents$country == "Taiwan", "Asia", documents$continent)
documents$continent <- ifelse(documents$country == "N/A", "unknown", documents$continent)

##################################
#    Mallet Topic Model Loading  #
##################################
## Create a mallet instance list object. Right now I have to specify the stoplist
##  as a file, I can't pass in a list from R.
## This function has a few hidden options (whether to lowercase, how we 
##   define a token). See ?mallet.import for details.

old.docs <- documents
mallet.instances <- mallet.import(documents$id, 
                                  documents$text, 
                                  "200-topic-models/en.txt", 
                                    token.regexp = "\\p{L}[\\p{L}\\_\\-&@'`\\p{N}]+[\\p{N}\\p{L}]"
)
# persist the documents so that we don't need to go through these steps everytime.  
file.name <- paste0(paste(model.dir, model.name, sep="/"), "-docs.Rdata")
save(documents, file=file.name)

## Save a mallet instance list for anchor calculation
mallet.instances$save(.jnew("java/io/File", "place.docs.here/saved.instances"))


## Separate Values and Jam Models
old.docs <- documents
documents <- filter(old.docs, jam=="values")
val.mallet.instances <- mallet.import(documents$id, 
                                      documents$text, 
                                  "200-topic-models/en.txt", 
                                  token.regexp = "\\p{L}[\\p{L}\\_\\-&@'`\\p{N}]+[\\p{N}\\p{L}]"
)
val.model.name <- "anchor_values_ngram"
file.name <- paste0(paste(model.dir, val.model.name, sep="/"), "-docs.Rdata")
save(documents, file=file.name)
val.mallet.instances$save(.jnew("java/io/File", "place.docs.here/values.instances"))

documents <- filter(old.docs, jam=="world")
world.mallet.instances <- mallet.import(documents$id, 
                                        documents$text, 
                                      "200-topic-models/en.txt", 
                                      token.regexp = "\\p{L}[\\p{L}\\_\\-&@'`\\p{N}]+[\\p{N}\\p{L}]"
)
world.model.name <- "anchor_world_ngram"
file.name <- paste0(paste(model.dir, world.model.name, sep="/"), "-docs.Rdata")
save(documents, file=file.name)
world.mallet.instances$save(.jnew("java/io/File", "place.docs.here/world.instances"))
#token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

documents <- old.docs

########### Continue if not using Anchors, otherwise:
   # 1. look at and follow ./anchor/README
   # 2. continue with:   R -f 200-topic-models/from_anchors.R

#load(file.name)

source('200-topic-models/lda_visualize.R')
# now, run 10 of the models and see how they compare
for (i in model_ids) {
  # create and train a topic model from the mallet.instances
  new.topic.model <- MalletLDA(num.topics=n.topics)
  new.topic.model$setNumThreads(3L)
  new.topic.model$loadDocuments(mallet.instances)
  new.topic.model$setAlphaOptimization(20, 50)
  new.topic.model$train(iters)
  new.topic.model$maximize(maxims)
  model.label = paste(model.name, n.topics, iters, maxims, formatC(i, width=2, flag="0"), sep="-")
  create.ldavis(new.topic.model, model.dir, model.label, cooccurenceThreshold=0.1, cooccurenceMinTokens=4)
  # also dump the state to disk so that we can analyze this model later
  new.topic.model$printState(.jnew("java.io.File", paste(model.dir, paste0(model.label, ".gz"), sep="/")))
}

wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model"
setwd(wd)
source("300-post-model-analyses/mallet_analyses.R")
source('200-topic-models/lda_visualize.R')
ad.hoc.ldavis <- function(n.topics, model.name, model.num, out.name) {
  model.dir <- "models_dir"
  model.object <- load.model.for.analysis(n.topics, model.name, model.num)
  model.label = paste(out.name, model.num, sep="_")
  create.ldavis(model.object$topic.model, model.dir, model.label, 
                cooccurenceThreshold=0.1, cooccurenceMinTokens=4)
}
ad.hoc.ldavis(30,"anchor_ngram",2,"anchor_PCA")
for (i in 3:10) {
  ad.hoc.ldavis(30,"anchor_ngram",i,"anchor_PCA")
}



#######################
# Gender Update       #
#######################

# add the gender information to the saved documents file
file.name <- paste0(paste(model.dir, model.name, sep="/"), "-docs.Rdata")
load(file.name)

#doc.genders <- select(threaded.docs, id, gender)
#save(doc.genders, file="place_docs_here/genders_by_id.Rdata")

# backup the old file in case we mess things up
save(documents, file=paste0(file.name , ".bkup"))

# load the gendered names
genders <- read.table("place_docs_here/gendered_names.csv", 
                      header=F, sep=",", col.names=c("email","name","gender"))
# clean up the gender labels
genders[genders$gender=="ambiguous", "gender"] <- "unknown"
genders[genders$gender=="unclear", "gender"] <- "unknown"
genders[genders$gender=="MALE", "gender"] <- "male"
#TODO move this further up stream
#genders[is.na(gender$gender), "gender"] <- "unknown"
genders$gender <- factor(genders$gender)

# remove trailing chars from emails and lowercase all emails
documents$user <- gsub(" -pfld", "", tolower(documents$user))
documents$user <- gsub(" - pfld", "", tolower(documents$user))
genders$email <- tolower(genders$email)

# merge, but preserve the original documents ordering (or the model state won't be recoverable)
documents$sort.order <- seq_len(nrow(documents))
new.docs <- merge(documents, genders, by.x="user", by.y="email", all.x=T)
# two names fell off the list -- update them here
new.docs[new.docs$user=="jonish@us.ibm.com", "gender"] <- "female"
new.docs[new.docs$user=="tim_main@uk.ibm.com", "gender"] <- "male"
# return to old sort order
new.docs <- new.docs[order(new.docs$sort.order),]
documents <- subset(new.docs, select=-c(sort.order))

save(documents, file=file.name)