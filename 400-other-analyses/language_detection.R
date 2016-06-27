#install.packages("textcat")
library(textcat)


# we want the non-ngrams texts
val <- read.delim("place_docs_here/hashed-values-docs-nongrams.tsv", sep="\t", quote="")
wol <- read.delim("place_docs_here/hashed-world-docs-nongrams.tsv", sep="\t", quote="")
wol$commentid.2 <- paste(substring(wol$commentid,2,20), 
                         substring(iconv(gsub("\\?","",wol$title), "UTF-8", "ASCII", sub=""), 1, 20), 
                         sep="."
) 
raw.text <- 
  rbind(data.frame(id=val$id, text=val$text, jam="value"),
        data.frame(id=wol$commentid, text=wol$text, jam="world"))
langs <- textcat(raw.text$text)
table(langs)
cbind(raw.text, langs) %>%
  filter(langs != "english") %>%
  write.csv(file="place_docs_here/languages.csv")

new.langs <- read.csv(file="place_docs_here/languages.csv")
table(new.langs$new.lang)

# update the ids in the new langs so that the world ids match
id.lookup <- select(wol, commentid, commentid.2)
new.langs.2 <- 
  left_join(new.langs, id.lookup, by = c("id" = "commentid")) 
new.langs.2$id <- ifelse(is.na(new.langs.2$commentid.2), new.langs.2$id, new.langs.2$commentid.2)
new.langs.2$new.lang <- as.character(new.langs.2$new.lang)

td <- select(new.langs.2, id, new.lang, is.eng) %>%
  right_join(threaded.docs)
filter(td, is.eng==0)$title
filter(td, is.eng==0)$text
filter(td, is.eng==0)$n.children
filter(td, is.eng==0)$new.lang
td$new.lang <- ifelse(is.na(td$new.lang), "english", as.character(td$new.lang))
table(td$new.lang)
td$is.non.eng <- ifelse(is.na(td$is.eng), 0, 1 - td$is.eng)

threaded.docs <- td
save(threaded.docs, file="place_docs_here/threaded_docs.Rdata")

ids <- filter(td, is.eng==0)$id
View(filter(td, parent %in% ids))

# parts of threads?

# count the translated threads as English?

