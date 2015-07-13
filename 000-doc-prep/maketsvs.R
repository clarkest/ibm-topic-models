values <- read.delim("values-managers-bigrams.tsv", colClasses=c("factor", "character", "character", "character", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "factor", "factor", "factor", "factor", "factor", "factor", "character"), sep="\t", quote="")

world <- read.delim("world-managers-bigrams.tsv", colClasses=c("factor", "character", "character", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "factor"), sep="\t", quote="")

world[world$CreationDate == "10/29/2004",]$CreationDate <- "10/28/2004"

documents <- rbind(data.frame(id = values$id, text = values$text, CreationDate = values$CreationDate, manager = values$manager, stringsAsFactors=F),
                   data.frame(id = world$id, text = world$text, CreationDate = world$CreationDate, manager = world$manager, stringsAsFactors=F))

documents$CreationDate <- factor(documents$CreationDate, levels=c("7/29/2003", "7/30/2003", "7/31/2003", "8/1/2003", "10/26/2004", "10/27/2004", "10/28/2004"))

write.table(documents, file="both.tsv", sep="\t", row.names=F, qmethod="double")

world <- read.delim("world-managers-bigrams.tsv", colClasses=c("factor", "character", "character", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "factor"), sep="\t", quote="")

world[world$CreationDate == "10/29/2004",]$CreationDate <- "10/28/2004"

documents <- data.frame(id = world$id, text = world$text, CreationDate = world$CreationDate, manager = world$manager, stringsAsFactors=F)

documents$CreationDate <- factor(documents$CreationDate, levels=c("7/29/2003", "7/30/2003", "7/31/2003", "8/1/2003", "10/26/2004", "10/27/2004", "10/28/2004"))

write.table(documents, file="world.tsv", sep="\t", row.names=F, qmethod="double")