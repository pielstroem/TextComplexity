
# Main function to compute values for a single text
MyGetValues = function(file)
{
  df = read.table(file, header = T, fill = T)
  
  # Overall text characteristics
  w.tokens = length(df$Token[df$CPOS != "PUNC"])
  w.types = length(unique(df$Token[df$CPOS != "PUNC"]))
  
  # Word length
  w.length = lapply(as.character(df$Token), nchar)
  w.length = unlist(w.length)
  w.length = w.length[df$CPOS != "PUNC"]
  
  # Find the long words for the LIX measure
  w.long = w.length > 6
  w.long = length(w.long[w.long == T])
  
  # Words and punctuations per sentence
  sel = df$CPOS == "PUNC"
  s.counts = tapply(df$Token, 
                    list(df$SentenceId, sel), 
                    length)
  s.length = as.integer(s.counts[,1])
  s.punc = as.integer(s.counts[,2])
  
  # Get rid of empty sentences -> still producing ERROR
  sel = s.length > 0 & s.punc > 0
  s.length = s.length[sel]
  s.punc = s.punc[sel]
  
  # Aggregated indices for overall text
  s.length.median = median(s.length, na.rm = T)
  s.length.IQR = IQR(s.length, na.rm = T)
  s.length.H = my.shannon.entropy( (s.length / sum(s.length)), normalized = T)
  s.punc.median = median(s.punc, na.rm = T)
  s.punc.IQR = IQR(s.punc, na.rm = T)
  s.punc.H = my.shannon.entropy( (s.punc / sum(s.punc)), normalized = T)
  s.punc.length.median = median((s.length / s.punc), na.rm = T)
  s.punc.length.IQR = IQR((s.length / s.punc), na.rm = T)
  s.punc.length.H = my.shannon.entropy( ((s.length / s.punc) / sum(s.length / s.punc, na.rm = T)), normalized = T)
  
  w.ttr = w.types / w.tokens
  c.tokens = sum(w.length)
  
  # Automated Readability Index
  sentences = max(df$SentenceId, na.rm = T)
  ARI = my.ARI(c.tokens, w.tokens, sentences)
  
  # LIX readability measure
  periods = length(df$CPOS[df$CPOS == "PUNC" & df$Token != "," & df$Token != ";"])
  LIX = my.LIX(w.tokens, periods, w.long)
  
  # Lexical entropy
  w.count.table = table(df$Token[df$CPOS != "PUNC"])
  w.freq.table = w.count.table / sum(w.count.table)
  lex.H = my.shannon.entropy(w.freq.table, normalized = T)
  
  # POS-tag entropy
  pos.count.table = table(df$CPOS[df$CPOS != "PUNC"])
  pos.freq.table = pos.count.table / sum(pos.count.table)
  pos.H = my.shannon.entropy(pos.freq.table, normalized = T)
  
  # POS-based ngrams (using the 'stylo' package)
  pos.bigrams = make.ngrams(df$CPOS, 2)
  pos.trigrams = make.ngrams(df$CPOS, 3)
  
  # Bigram entropy
  pos.bigrams = table(pos.bigrams)
  pos.bigrams = data.frame(pos.bigrams)
  pos.bigrams.numbers = pos.bigrams$Freq
  pos.bigrams.rel = pos.bigrams.numbers / sum(pos.bigrams.numbers)
  pos.bigrams.H = my.shannon.entropy(pos.bigrams.rel, normalized = T)
  # Trigram entropy
  pos.trigrams = table(pos.trigrams)
  pos.trigrams = data.frame(pos.trigrams)
  pos.trigrams.numbers = pos.trigrams$Freq
  pos.trigrams.rel = pos.trigrams.numbers / sum(pos.trigrams.numbers)
  pos.trigrams.H = my.shannon.entropy(pos.trigrams.rel, normalized = T)
  
  # Combine the values
  values = c(c.tokens,
             w.tokens,
             w.ttr,
             s.length.median,
             s.length.IQR,
             s.length.H,
             s.punc.median,
             s.punc.IQR,
             s.punc.H,  
             s.punc.length.median,
             s.punc.length.IQR,
             s.punc.length.H,
             ARI,
             LIX,
             lex.H,
             pos.H,
             pos.bigrams.H,
             pos.trigrams.H
  )
  return(values)
}
