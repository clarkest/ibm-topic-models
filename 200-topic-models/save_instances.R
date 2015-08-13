if (Sys.getenv("JAVA_HOME")!="")
  Sys.setenv(JAVA_HOME="")
library(rJava)
library(mallet)
library(countrycode)
library(dplyr)

this.dir = "." #"/Users/mimno/Documents/github/ibm-topic-models"
setwd(this.dir)
value_file<-"place_docs_here/values-docs-ngrams.tsv"
world.file <- "place_docs_here/world-docs-ngrams.tsv"

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
                                 "factor","character","character", "factor", "character"), 
                    sep="\t", 
                    quote=""
)


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
               parent = as.character(values$Parent),
               forum = as.character(values$Forum),
               user = as.character(values$AuthorEmail),
               job = as.character(values$JobResp),
               office = as.character(values$Office)
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
               user = as.character(world$author_email),
               job = as.character(world$jobresp),
               office = as.character(world$office)
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
                                  "200-topic-models/en.txt", 
                                  token.regexp = "\\p{L}[\\p{L}\\p{P}\\p{N}]+[\\p{N}\\p{L}]"
)
#token.regexp = "\\p{L}[\\p{L}\\p{P}]+\\p{L}")

# To save and reload through java:
mallet.instances$save(.jnew("java/io/File", "saved.instances"))
