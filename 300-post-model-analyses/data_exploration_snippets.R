#################
# does anyone change from being a non-manager to being a manager?
#################

# find the users that have changed management status
documents$manager.str <- as.character(documents$manager)
manager.status <- documents %>% group_by(user) %>% summarise(mng1=min(manager.str), mng2=max(manager.str))  
management.changers <- manager.status[manager.status$mng1 != manager.status$mng2,]

# it'd be useful to see the titles side by side as well
# the titles aren't in the tsv files, so pull them fresh from the local db

#install.packages("RMySQL")
library(RMySQL)
mydb = dbConnect(MySQL(), user='root', password='root', dbname='ibm_jam', host='127.0.0.1')
rs = dbSendQuery(mydb, "select AuthorEmail, JobResp from world_jam group by AuthorEmail;")
world_titles = fetch(rs, n=-1)
rs = dbSendQuery(mydb, "select AuthorEmail, JobResp from value_jam group by AuthorEmail;")
value_titles = fetch(rs, n=-1)
dbDisconnect(mydb)

# merge the titles into the list of manager status changers
management.changers <- left_join(management.changers, value_titles, by=c("user" = "AuthorEmail"))
names(management.changers)[names(management.changers)=="JobResp"] <- "values_title"
management.changers <- left_join(management.changers, world_titles, by=c("user" = "AuthorEmail"))
names(management.changers)[names(management.changers)=="JobResp"] <- "world_title"


# quick look if anyone has a title like Kristine Lawas, with w3Jams or something like it.
titles <- rbind(value_titles, world_titles)
titles[grepl("Jam", titles$JobResp), ]
titles[grepl("jam", titles$JobResp), ]
#       nope -- just her

#################
# are some comments cut off?
#################
library(gdata)
world.raw <- read.csv("/Users/clarkbernier/Dropbox/IBM Jams 2013 reboot/Data/world_jam.csv",
                      sep="\t", 
                      row.names = NULL, 
                      stringsAsFactors = FALSE
)
names(world.raw)[names(world.raw)=="CommentID"] <- "CommentId"
value.raw <- read.csv("/Users/clarkbernier/Dropbox/IBM Jams 2013 reboot/Data/value_jam.csv",
                      sep="\t", 
                      row.names = NULL, 
                      stringsAsFactors = FALSE
)
# how many comments were split by at least one tab?
nrow(world.raw[nchar(world.raw$X.1)>0,])
nrow(value.raw[nchar(value.raw$X.1)>0,])

# example of docs cut at different places
world.raw[world.raw$CommentId=="<ffd7497c22.9326a6f9.WORLDJAM@d25was503.mkm.can.ibm.com>",]$text
documents[documents$id=="<ffd7497c22.9326a6f9.WORLDJAM@d25was503.mkm.can.ibm.com>",]$text


# how many comments are shorter in the documents tsv than the raw data (ignoring ones where there are tab cutoffs)
#### This is totally thrown off by the difference in cleaning approaches between the two. Abort!
# combined.raw.text <- rbind(world.raw[,c("CommentId","Text")], value.raw[,c("CommentId","Text")])
# docs.to.raw.text.matching <- left_join(documents[,c("id","text")], combined.raw.text, by=c("id" = "CommentId"))
# sum(is.na(docs.to.raw.text.matching$Text))
# non.matches <- docs.to.raw.text.matching[docs.to.raw.text.matching$text != docs.to.raw.text.matching$Text, c("text","Text")]

# here are some known introduction posts from across different fora 
# -- we'll see how many of them made it into documents
intro.doc.ids <- c("<ffd241578b.dbbee7d1.WORLDJAM@d25was504.mkm.can.ibm.com>","<ffd2439f11.4e66942c.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffd243b202.74e204d6.WORLDJAM@d25was504.mkm.can.ibm.com>","<ffd2442d0c.16b5a603.WORLDJAM@d25was503.mkm.can.ibm.com>","<ffd2446a89.a7da6c88.WORLDJAM@d25was504.mkm.can.ibm.com>","<ffd244d99d.e4139e54.WORLDJAM@d25was504.mkm.can.ibm.com>","<ffd245750b.acfc14a9.WORLDJAM@d25was504.mkm.can.ibm.com>","<ffd245f3d6.cb367fe8.WORLDJAM@d25was504.mkm.can.ibm.com>")
documents[documents$id %in% intro.doc.ids,]



####################
# do continents have different business functions?
####################
library(countrycode)
library(stringr)
library(dplyr)
wd <-  "C:/Users/clarkest/Dropbox/IBM Local/ibm-topic-model/"
setwd(wd)
world.file <- "place_docs_here/world-docs-ngrams-a.tsv"

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
world$continent <- countrycode(world$country, "country.name", "continent")
world.users <- summarize(group_by(world, author_email), continent=max(continent), business_unit=max(business_unit), business_unit2=max(business_unit2))
world.users$long.bu <- ifelse(str_length(world.users$business_unit) >= str_length(world.users$business_unit2), world.users$business_unit, world.users$business_unit2)

continent.bu <- summarize(group_by(world.users, continent, long.bu), number=n())
out.file <- "outputs/continent.business.units.csv"
write.table(continent.bu, out.file, row.names=FALSE)

# what is "02"?
summarize(group_by(world.users[world.users$business_unit2=="02",], business_unit),number=n()) 
