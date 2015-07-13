library("plyr")
# World Jam Remappings

# adjust the system locale to suppress all the warnings we get otherwise
Sys.setlocale('LC_ALL','C')

# grab the text 
text <- scan("/Users/clarkbernier/Dropbox/IBM Jams 2013 reboot/Data/world_jam_raw_data/text.dat", what='character', sep="\n", blank.lines.skip=FALSE)

# the rest of the data are in ".class" files -- grab each in turn
dir <- "/Users/clarkbernier/Dropbox/IBM Jams 2013 reboot/Data/world_jam_raw_data/"
class.files <- list.files(dir, pattern="*.class")
data.list <- list()
for (fil.name in class.files) {
  obj.name <- unlist(strsplit(fil.name, "[.]"))[1]
  fil.path <- paste0(dir, fil.name)
  data.list[[obj.name]] <- scan(fil.path, sep="\n", what='character', blank.lines.skip=FALSE)   
}

df <- rename(data.frame(data.list), 
             c(creation="creation_time", 
                   author="author_email", 
                   from="user_name",
                   bunit="business_unit",
                   bunit2="business_unit2",
                   parent="parent_comment_id",
                   name="title"
                 )
            )
# confirm that things loaded mapped correctly
df[grep("seretary", text),]$user_name 

# add in the text data
df <- cbind(df, text)
df[grep("seretary", df$text),]$user_name 

# it's going to break the tsv format unless we strip out tabs
for (col in 1:ncol(df)) {
  df[, col] <- gsub("\t", " ", df[,col])
  print(grep("\t", df[,col]))
}

out.name <- "/Users/clarkbernier/Dropbox/IBM Jams 2013 reboot/Data/world_jam_raw_data/reloaded_world.tsv"
write.table(df, out.name, sep="\t", row.names=FALSE)
other.out.name <- "/Users/clarkbernier/Dropbox/IBM Local/ibm-code/preprocessing/reloaded_world.tsv"
write.table(df, other.out.name, sep="\t", row.names=FALSE, quote=FALSE)
