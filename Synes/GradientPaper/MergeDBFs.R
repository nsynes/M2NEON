
library(plyr)
library(foreign)


dirDBF <- "C:/Dropbox (ASU)/M2NEON/Paper_1/DATA/DBFs"
OutCsv <- "C:/Dropbox (ASU)/M2NEON/Paper_1/DATA/MergedDBFs.csv"


setwd(dirDBF)

listDBFs <- list.files(pattern = "\\.dbf$")


# Collect all dbf files into a list of dataframes
my.datalist <- vector('list', length(listDBFs))
i <- 1
for (dbf in listDBFs) {
  cat(paste(dbf, "\n"))
  df <- read.dbf(dbf, as.is = FALSE)
  colnames(df) <- ifelse(!(colnames(df) %in% c("COUNT","AREA","TARGET_FID")), substr(dbf,1,nchar(dbf)-4), colnames(df))
  df$COUNT <- NULL
  df$AREA <- NULL
  my.datalist[[i]] <- df
  i <- i + 1
}

# Merge the list of dbf data frame together
dfAll <- Reduce(function(x, y) merge(x, y, all=TRUE, by="TARGET_FID"), my.datalist)

write.csv(dfAll, OutCsv, row.names=FALSE)

