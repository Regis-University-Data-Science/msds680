# this is how the raw heart disease data was loaded and processed
# it loads the .data and .names files and outputs a .csv
# (the .names file was manually 
# cleaned up to only have lines with column labels)

library(data.table)
fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/cleveland.data'

f <- readLines(fn)
head(f, 10)
head(f, 20)
# we can see that every 10 lines is one subject
# but the end is strange...
# just looking at the file in a text editor, I was able to find the last 'good' record at line 2820
data <- c()
for (i in seq(1, 2811, 10)) {
  data <- c(data, strsplit(paste(f[i:(i + 9)], collapse = ' '), split = ' '))
}

# need to transpose it, i.e. t(), switch rows and columns
dt <- transpose(as.data.table(data))

fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/names.txt'
names <- readLines(fn)

# this took some playing around with to figure out how to grab the text right before :
gsub('\\s*\\d\\s(\\w*):.*', '\\1', "      1 id: patient identification number")

get_names <- function(x) {
  if (grepl('\\s\\s\\s\\s\\s\\d+\\s', x)) {
    return(gsub('\\s*\\d+\\s(\\w*).*', '\\1', x))
  }
}

cln_names <- unlist(lapply(names, FUN = get_names))
head(cln_names)
cln_names

length(cln_names)
# remove any nulls and empty strings
cln_names <- cln_names[!is.null(cln_names) & cln_names != '']
length(cln_names)  # 76, same number as expected

names(dt) <- cln_names

# columns to keep
keepers <- c(3, 4, 9, 10, 12, 14, 15, 16, 17, 18, 19, 32, 38, 51, 58)
dt_keep <- dt[, keepers, with = F]

fn <- '/home/nate/Dropbox/MSDS/MSDS680_ncg_S8W1_18/week1/heart.disease.data'
fwrite(dt_keep, fn)
