# Load packages
library (lattice)
library(stylo)

# Load the data
#df = results
df = read.table("/data/metrics/metrics.csv", header = T)

# Process metadata

author = character(length(df$tex))
for(i in 1:length(author))
{
  author[i] = unlist(strsplit(as.character(df$text[i]), split = ","))[1]
}

# Find relevant authors (with 2 or more texts, not called "anonymous")
selected.authors = rownames(table(author)[table(author) > 3])

# Generate a stylo-fit data frame
stylo.df = df[4:19][author %in% selected.authors,]
rownames(stylo.df) = df$text[author %in% selected.authors]

# An additional vector of the relevant author names
stylo.author = author[author %in% selected.authors]

# Data with the ther texts for calculating regression models...



# sehr längenabhänig -> hier log-trans der X-Werte, effekt durch regression rausrechnen
w.tokens = df$w.tokens[author %in% selected.authors]
plot(w.ttr ~ w.tokens, data = stylo.df)
plot(lex.H ~ w.tokens, data = stylo.df)
plot(pos.H ~ w.tokens, data = stylo.df)
plot(pos.bigrams.H ~ w.tokens, data = stylo.df)
plot(pos.trigrams.H ~ w.tokens, data = stylo.df)

# wohl ganz gut
plot(ARI ~ w.tokens, data = stylo.df) # vielleicht eher durcheinander
plot(LIX ~ w.tokens, data = stylo.df) # ähnlich dem ARI
plot(s.length.median ~ w.tokens, data = stylo.df) # wohl ganz gut
plot(s.length.IQR ~ w.tokens, data = stylo.df)
plot(s.punc.median ~ w.tokens, data = stylo.df)
plot(s.punc.IQR ~ w.tokens, data = stylo.df)



# Getting rid of text length effects for type-token ratio
par(mar = c(5,5,2,2))
plot(stylo.df$w.ttr ~ w.tokens,
     xlab = "Tokens",
     ylab = "Type-token ratio"
     )
plot(w.ttr ~ log(w.tokens), data = stylo.df,
     xlab = "log(Tokens)",
     ylab = "Type-token ratio"
     )
lm = lm(w.ttr ~ log(w.tokens), data = stylo.df)
abline(lm)
w.ttr.rel = as.numeric(lm$res)
plot(w.ttr.rel ~ w.tokens, 
     data = stylo.df,
     log = "x",
     xlab = "Tokens",
     ylab = "Residuals of TTR"
     )
abline(h = 0)

# Getting rid of text length effects for lexical entropy
plot(lex.H ~ w.tokens, data = stylo.df)
plot(lex.H ~ log(w.tokens), data = stylo.df)
lm = lm(lex.H ~ log(w.tokens), data = stylo.df)
lex.H.rel = as.numeric(lm$res)
plot(lex.H.rel ~ w.tokens, data = stylo.df)

# Getting rid of text length effects for type-token ratio
plot(pos.trigrams.H ~ w.tokens, data = stylo.df)
plot(pos.trigrams.H ~ log(w.tokens), data = stylo.df)
lm = lm(pos.trigrams.H ~ log(w.tokens), data = stylo.df)
abline(lm)
pos.trigrams.H.rel = as.numeric(lm$res)
plot(pos.trigrams.H.rel ~ w.tokens, log = "x", data = stylo.df)

# Replace 
stylo.df$w.ttr = w.ttr.rel
colnames(stylo.df)[1] = "w.ttr.rel"
stylo.df$lex.H = lex.H.rel
colnames(stylo.df)[13] = "lex.H.rel"
stylo.df$pos.trigrams.H = pos.trigrams.H.rel
colnames(stylo.df)[16] = "pos.trigrams.H.rel"

# Verkürzte Autorennamen
strsplit(stylo.author, ",")

names(stylo.df)
par(mar = c(5, 12, 2, 2))
boxplot(stylo.df$LIX~ stylo.author, 
        horizontal = T, 
        las = 1,
        range = 0,
        xlab = "LIX readability score"
        )
boxplot(stylo.df$w.ttr.rel ~ stylo.author, 
        horizontal = T, 
        las = 1,
        range = 0,
        xlab = "TTR residuals"
        )
boxplot(stylo.df$s.length.median ~ stylo.author, 
        horizontal = T, 
        las = 1,
        range = 0,
        xlab = "Median sentence length in words"
        )



par(mar = c(5,1,2,10))
stylo(frequencies = stylo.df)
# only the re-adjusted values
stylo(frequencies = stylo.df[c(1, 13, 16)])


author = character(length(df$tex))
for(i in 1:length(author))
{
  author[i] = unlist(strsplit(as.character(df$text[i]), split = "_"))[1]
}
