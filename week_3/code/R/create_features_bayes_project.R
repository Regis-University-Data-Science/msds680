library(tm)
library(data.table)
file_loc <- "~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week3/spam.csv"
dt <- fread(file_loc)
labels <- as.data.table(dt[, v1])
labels.filename <- "~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week3/spam.labels.csv"
fwrite(labels, labels.filename)
dt[, c('v1', 'V3', 'V4', 'V5'):=NULL]
dt[, doc_id:=seq(1:nrow(dt))]
names(dt) <- c('text', 'doc_id')
setcolorder(dt, c('doc_id', 'text'))
dt[, text:=unlist(lapply(text, function(x) {iconv(x, "latin1", "ASCII", sub="")}))]  # convert non-ascii characters which cause problems
str(dt)
corp <- VCorpus(DataframeSource(dt))
dtm <- DocumentTermMatrix(corp, control = list(weighting = weightTfIdf,
                                               stopwords = T,
                                               stemming = T,
                                               wordLengths = c(2, 13),
                                               tolower = T,
                                               removePunctuation = T,
                                               removeNumbers = T))
inspect(dtm)
inspect(dtm.dense)

# highly unbalanced dataset
barplot(table(labels))
dtm.matrix <- as.matrix(dtm)
filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week3/dtm.csv'
write.csv(dtm.matrix, filename, row.names = F, col.names = F)
filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week3/dtm.dense.csv'
write.csv(as.matrix(dtm.dense), filename, row.names = F, col.names = F)
filename <- '~/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week3/dtm.data'
save(dtm.matrix, file = filename)
