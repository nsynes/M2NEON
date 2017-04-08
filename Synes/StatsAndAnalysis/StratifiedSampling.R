library(caTools)


ScaleForSizeWeWant <- 250
SizeWeWant <- nrow(read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/Paper_1/DATA/GBM_InputData/D17_%sm.csv", ScaleForSizeWeWant)))



scale <- 50

df <- read.csv(sprintf("C:/Dropbox (ASU)/M2NEON/Paper_1/DATA/GBM_InputData/D17_%sm.csv", scale))

dfOut <- df[sample(nrow(df), SizeWeWant), ]

write.csv(dfOut, sprintf("C:/Dropbox (ASU)/M2NEON/Paper_1/DATA/GBM_InputData/D17_%sm_SubsampledTo%smSampleSize.csv", scale, ScaleForSizeWeWant), row.names=FALSE)









###################################
# subset the data, stratified by the column "strata"
###################################
train_rows <- sample.split(df$strata, SplitRatio=0.2)
train <- df[ train_rows,]
test  <- df[train_rows == FALSE,]
####################################
###################################






