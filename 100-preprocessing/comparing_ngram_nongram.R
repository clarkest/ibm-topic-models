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


world.file <- "place_docs_here/world-docs-ngrams.tsv"


#values <- read.delim("place_docs_here/nongrams-values-managers-bigrams.tsv", encoding="UTF-8", colClasses=c("factor", "character", "character", "character", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "factor", "character", "factor", "factor", "factor", "factor", "factor", "factor", "character"), sep="\t", quote="")
#world.file <- "place_docs_here/nongrams-world-manager-bigrams.tsv"
world.file <- "place_docs_here/world-docs-ngrams.tsv"
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
world.file <- "place_docs_here/world-docs-nongrams.tsv"
world.nongrams <- read.delim(world.file, 
                             encoding="UTF-8", 
                             colClasses=c("factor", "character", "character", "character", 
                                          "character", "factor", "factor", "factor", 
                                          "factor", "factor", "factor", "factor", 
                                          "character", "character", "factor", "character", 
                                          "factor","character","character", "factor", "character"), 
                             sep="\t", 
                             quote=""
)
world$new_id <- as.factor(world$new_id)
world.nongrams$new_id <- as.factor(world.nongrams$new_id)
non.to.n.overlaps <- sapply(world.nongrams$new_id, function(x) is.element(x, world$new_id))
sum(!non.to.n.overlaps)
world.nongrams[!non.to.n.overlaps, "text"]
n.to.non.overlaps <- sapply(world$new_id, function(x) is.element(x, world.nongrams$new_id))
sum(!n.to.non.overlaps)
world[!n.to.non.overlaps, "text"]