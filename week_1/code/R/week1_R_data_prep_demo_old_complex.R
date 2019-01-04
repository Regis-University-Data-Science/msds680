library(data.table)

fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/adult.data'

adult.dt <- fread(fn)#, stringsAsFactors = T)
str(adult.dt)

fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/adult.names'

adult.names <- readLines(fn)
adult.names

# https://stackoverflow.com/a/35728025/4549682
# copy-pasted dashes from here:
short.names <- unlist(lapply(adult.names, FUN = function(x) gsub('(.+):\\s+.+', '\\1', x)))
short.names <- unlist(lapply(short.names, FUN = function(x) gsub('-', '.', x)))
short.names
short.names <- c(short.names, 'under.over.50k')

names(adult.dt) <- short.names
adult.dt[adult.dt == '?'] <- NA
str(adult.dt)

class(adult.dt$occupation)
# convert character columns to factors
# https://stackoverflow.com/a/37833702/4549682
changeCols<- names(Filter(is.character, adult.dt))
adult.dt[,(changeCols):= lapply(.SD, as.factor), .SDcols = changeCols]
str(adult.dt)
# would probably want to set this to integers that correpsond to the proper education level, or leave as factor so R dummies it
levels(adult.dt$education)
