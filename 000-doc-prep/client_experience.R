

install.packages("openxlsx")
library("openxlsx")
library("gdata")
library("plyr")
#ce.data <- openXL("/Users/clarkbernier/Dropbox/IBM Jams 2013 reboot/Data/Client Experience Jam 2013/CEJam_Comments_CH.xlsx")
setwd("/Users/clarkbernier/Dropbox/IBM Local/")
ce.data <- read.xls("CEJam_xls_pwfree.xls")
# comes out nasty

?openXL
?openxlsx
>import
# getting the names from the names excel
filnam <- "/Users/clarkbernier/Dropbox/IBM Jams 2013 reboot/Data/Client Experience Jam 2013/(see Reboot folder) Client Experience Jam Users FINAL-NDA.xlsx"
ce_jam_names <- list()
for (i in 1:20) {
  df1<- read.xlsx(filnam, sheet=i)
  ce_jam_names[[i]] <- df1
}
ce_names <- rbind.fill(ce_jam_names)


