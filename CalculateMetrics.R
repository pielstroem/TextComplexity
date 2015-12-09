# readibiity indices:
# - Automated readability index
# - LIX
# - Coleman-Liau index

# TODO
# - CLI -> requires alternative segmenting
# - correct for text length effects


# Load packages
library(stylo) # MyGetValues() relies on Stylo's make.ngrams() 

# load functions
source("~/R/myRscripts/my.shannon.entropy.R")
source("~/R/myRscripts/my.ARI.R")
source("~/R/myRscripts/my.CLI.R")
source("~/R/myRscripts/my.LIX.R")
source("functions.R")

#########################################################################

# Test routine for single run
#values = my.get.the.values("./data/csv/Doyle_AStudyinScarlet.txt.csv")

# Extract file names
list.csv = list.files(path = "./data/csv/")

# Run loop to calculate values
results = data.frame()
for(i in 1:length(list.csv)){
  print(paste("Processing text no", i, "of", length(list.csv), ":", list.csv[i]))
  path = paste("./data/csv/", list.csv[i], sep = "")
  values = MyGetValues(path)
  results = rbind(results, values)
}
colnames = c("c.tokens",
             "w.tokens",
             "w.ttr",
             "s.length.median",
             "s.length.IQR",
             "s.length.H",
             "s.punc.median",
             "s.punc.IQR",
             "s.punc.H",
             "s.punc.length.median",
             "s.punc.length.IQR",
             "s.punc.length.H",
             "ARI",
             "LIX",
             "lex.H",
             "pos.H",
             "pos.bigrams.H",
             "pos.trigrams.H"
             )
colnames(results) = colnames

text = list.csv
results = cbind(text, results)

write.table(results, "./data/metrics/metrics.csv", row.names = F)

