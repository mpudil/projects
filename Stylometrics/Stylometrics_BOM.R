# Book of Mormon Voices: EDA
library(quanteda)
library(tidyverse)
library(spacyr)
library(cluster)
library(factoextra)
library(readtext)
library(plotly)
library(tau)
library(data.table)
library(randomForest)
setwd("C:/Users/Mitchell Pudil/Documents/textstat_tools/BookMormon/Voices/Subset/")
authors <- c("alma", "helaman", "joseph", "mormon", "moroni", "nephi")
# Question 1 -------------------------------------------------------------
pos_by_author <- data.frame(matrix(nrow=length(authors), ncol=10, data=NA))
colnames(pos_by_author) <- c("author", "total_words", "verb",
                             "adj", "adv", "noun", "sentence_length")
pos_by_author$author <- authors
# We’ll get the paths to the book of mormon subsets
bom_paths <- list.files(full.names = TRUE, pattern = "*.txt")
for(i in 1:length(authors)){
  # And we’ll subset out the author (voice)
  paths_sub <- bom_paths %>% str_detect(authors[i]) %>%
    keep(bom_paths, .)
  # Finally, we’ll create our data.frame of texts and doc_ids
  sub_df <- readtext(paths_sub)
  # We going to use regex to remove all periods including those between words
  # which will help with the accuracy of our parsing.

  sub_df$text <- as.character(sub_df$text) %>%
    gsub("\\.", " ", .) %>% gsub("\\;", " ", .) %>%
    gsub("\\,", " ", .)
  # And create a corpus object.
  sub_corpus <- corpus(sub_df)
  # Next, wee’re going to use spacy to parse the corpus.
  sub_prsd <- spacy_parse(sub_corpus, pos = T, tag = T,
                          dependency = T, entity = F)
  # Remove any spaces, and then combine our pos and tag columns.
  sub_prsd <- sub_prsd %>% filter(pos != "SPACE") %>%
    unite("pos", pos:tag)
  # Calculate percentage of each major part of speech by author
  sub_prsd_no_punct <- filter(sub_prsd, dep_rel!="punct")
  pos_by_author$total_words[i] <- nrow(sub_prsd_no_punct)
  pos_by_author$verb[i] <- nrow(filter(sub_prsd, grepl(pattern = "VERB",
                                                       x=sub_prsd$pos)))/nrow(sub_prsd_no_punct)
  pos_by_author$adj[i] <- nrow(filter(sub_prsd, grepl(pattern = "ADJ",
                                                      x=sub_prsd$pos)))/nrow(sub_prsd_no_punct)
  pos_by_author$adv[i] <- nrow(filter(sub_prsd, grepl(pattern = "ADV",
                                                      x=sub_prsd$pos)))/nrow(sub_prsd_no_punct)
  pos_by_author$noun[i] <- nrow(filter(sub_prsd, grepl(pattern = "NOUN",
                                                       x=sub_prsd$pos)))/nrow(sub_prsd_no_punct)
  pos_by_author$conj[i] <- nrow(filter(sub_prsd, grepl(pattern = "CONJ",
                                                       x=sub_prsd$pos)))/nrow(sub_prsd_no_punct)
  pos_by_author$det[i] <- nrow(filter(sub_prsd, grepl(pattern = "DET",
                                                      x=sub_prsd$pos)))/nrow(sub_prsd_no_punct)
  pos_by_author$pron[i] <- nrow(filter(sub_prsd, grepl(pattern = "PRON",
                                                       x=sub_prsd$pos)))/nrow(sub_prsd_no_punct)
  pos_by_author$sentence_length[i] <- nrow(sub_prsd) / sum(sub_prsd$token==".")
}
pos_by_author$author <- c("Alma", "Helaman", "Joseph Smith", "Mormon", "Moroni",
                          "Nephi")
pos_by_author$otherpos <- 1 - pos_by_author$verb -
  pos_by_author$adj - pos_by_author$adv - pos_by_author$noun -
  pos_by_author$conj - pos_by_author$det - pos_by_author$pron
# Plot stacked bar chart of parts of speech
plot_ly(subset(pos_by_author, author!="Joseph Smith"),
        x = ~author, y = ~adj,
        type = ’bar’, name = ’Adjectives’) %>%
  add_trace(y = ~adv, name = ’Adverbs’) %>%
  add_trace(y = ~noun, name = ’Nouns’) %>%
  add_trace(y = ~verb, name = ’Verb’) %>%
  add_trace(y = ~conj, name = ’Conjunction’) %>%
  add_trace(y = ~det, name = ’Determinant’) %>%
  add_trace(y = ~pron, name = ’Pronoun’) %>%
  add_trace(y = ~otherpos, name = ’Other Part of Speech’) %>%
  layout(yaxis = list(title = ’Proportion of Words’),
         xaxis = list(title=’Speaker’), barmode = ’stack’)
plot_ly(pos_by_author, x = ~author, y = ~sentence_length, type = ’bar’,
        name = ’Average Sentence Length’) %>%
  layout(yaxis = list(title = ’Average Sentence Length’))
# N-gram Frequencies
# given a string vector and size of ngrams this function returns word ngrams
# with corresponding frequencies
createNgram <-function(stringVector, ngramSize){
  ngram <- data.table()
  ng <- textcnt(stringVector, method = "string", n=ngramSize, tolower = FALSE)
  if(ngramSize==1){
    ngram <- data.table(w1 = names(ng), freq = unclass(ng), length=nchar(names(ng)))
  }
  else {
    ngram <- data.table(w1w2 = names(ng), freq = unclass(ng), length=nchar(names(ng)))
  }
  return(ngram)
}
# Most popular ngrams by book
for(i in c("nephi.txt", "alma.txt", "helaman.txt", "mormon.txt", "moroni.txt",
           "joseph.txt")){
  ngrams <- createNgram(readtext(i), 4)
  ordered_ngrams <- ngrams[order(ngrams$freq, decreasing = TRUE),]
  total <- sum(ordered_ngrams$freq)
  print(paste0(i, ", ", ordered_ngrams$w1w2[1], ", ",
               (ordered_ngrams$freq[1] / total)*100))
}
# Most popular POS ngrams by book
sub_df <- readtext("mormon.txt")
sub_df$text <- gsub("\\.", " ", sub_df$text)
sub_corpus <- corpus(sub_df)
sub_prsd <- spacy_parse(sub_corpus, pos = T, tag = T, dependency = T,
                        entity = F)$pos
ngrams <- createNgram(sub_prsd, 2)
ordered_ngrams <- ngrams[order(ngrams$freq, decreasing = TRUE),]
ordered_ngrams$prop <- ordered_ngrams$freq/sum(ordered_ngrams$freq)
ordered_ngrams[1:3]
# Cluster Plot 2 ------------------------------------------------------------
source("functions/helper_functions.R")
setwd("C:/Users/Mitchell Pudil/Documents/textstat_tools/BookMormon/Voices/CrossVal/")
# We’ll get the paths to the BOM corpus
bom_paths <- list.files(full.names = TRUE, pattern = "*.txt")
# Create our data.frame of texts and doc_ids
sub_df <- readtext(bom_paths)
# We going to use regex to remove all punctuation
# which will help with the accuracy of our parsing.
sub_df$text <- gsub(’[[:punct:] ]+’,’ ’,sub_df$text, perl = T)
# And create a corpus object.
sub_corpus <- corpus(sub_df)
# Next, wee’re going to use spacy to parse the corpus.
# Note that we can add a dependency column to our parsing.
sub_prsd <- spacy_parse(sub_corpus, pos = T, tag = T, dependency = T,
                        entity = F)
# We’re going to remove any spaces, and then combine our pos
#and tag columns.
sub_prsd <- sub_prsd %>% filter(pos != "SPACE") %>%
  unite("pos", pos:tag)
# Next we create a named list from the new, concatenated column.
sub_tokens <- split(sub_prsd$pos, sub_prsd$doc_id)
# See what the result looks like.
sub_tokens
# Now, we’ll use that as our tokens object.
sub_tokens <- as.tokens(sub_tokens)
# From that, we’ll generate a dfm.
sub_dfm <- dfm(sub_tokens)
# We’ll weight the raw counts.
sub_dfm <- dfm_weight(sub_dfm, scheme = "prop")
# And convert the result to a data.frame.
sub_dfm <- convert(sub_dfm, to = "data.frame") %>%
  rename(doc_id = document)
# Finally, we’re going to convert the first row (doc_id) into row names.
# And, for fun, we’ll order our columns alphabetically.
sub_dfm <- sub_dfm %>% column_to_rownames("doc_id")
df <- data.frame(scale(sub_dfm))
# Create cluster plot
m <- sapply(1:nrow(df), function(i) sapply(1:nrow(df),
                                           function(j) dist(rbind(df[i,], df[j,]))))
cmdeuc <- cmdscale(m) %>% data.frame
a <- gsub("[0-9]+.txt", "", bom_paths ) %>% basename
cmdeuc$Author <- paste0(toupper(substr(a, 1, 1)), substr(a, 2, nchar(a)),
                        sep="")
colnames(cmdeuc)[1:2] <- c("PC1", "PC2")
ggplot(cmdeuc, aes(PC1, PC2,color=Author)) + geom_point() +
  theme_bw() + ggtitle("Euclidean Distance")
# Frequencies and Distributions of All Files --------------------------------------
# Create training dataset
columns <- c("author", "total_words", "verb", "adj", "adv", "noun", "sentence_length",
             "did_verb", "save_it_be", "cause", "things_which", "by_the_power", "passed_away",
             "write", "holy_ghost", "det_noun", "adp_det", "pron_verb", "say_unto_you",
             "came_to_pass", "these_are_they")
bomtrain <- data.frame(matrix(nrow=length(bom_paths), ncol=length(columns), data=NA))
colnames(bomtrain) <- columns
bomtrain$author <- cmdeuc$Author
# We’ll get the paths to the book of mormon subsets
bom_paths <- list.files(full.names = TRUE, pattern = "*.txt")
# Our clustering will be based on pos counts, so we need to initialize our spcacy model.
for(i in 1:nrow(bomtrain)){
  # Finally, we’ll create our data.frame of texts and doc_ids
  sub_df <- readtext(bom_paths[i])
  # And create a corpus object.
  sub_corpus <- corpus(sub_df)
  # Next, we’re going to use spacy to parse the corpus.
  sub_prsd <- spacy_parse(sub_corpus, pos = T, tag = T, dependency = T, entity = F)
  for(j in 1:nrow(sub_prsd)){
    sub_prsd$nextpos[j] <- ifelse(j==nrow(sub_prsd), NA, sub_prsd$pos[j+1])
    sub_prsd$nextword[j] <- ifelse(j==nrow(sub_prsd), NA, sub_prsd$token[j+1])
    sub_prsd$nextnextword[j] <- ifelse(j >= nrow(sub_prsd)-1, NA, sub_prsd$token[j+2])
  }
  
  # Common phrases
  bomtrain$did_verb[i] <- sum(sub_prsd$token=="did" &
                                sub_prsd$nextpos == "VERB")/(nrow(sub_prsd) -1)
  bomtrain$more_part[i] <- sum(sub_prsd$token=="more" &
                                 sub_prsd$nextword == "part")/(nrow(sub_prsd) -1)
  bomtrain$save_it_be[i] <- sum(sub_prsd$token=="save" &
                                  sub_prsd$nextword == "it" & sub_prsd$nextnextword %in% c("were", "be"))/(nrow(sub_prsd) -2)
  bomtrain$cause[i] <- sum(grepl("cause", sub_prsd$token))/nrow(sub_prsd)
  bomtrain$things_which[i] <- sum(sub_prsd$token=="things" &
                                    sub_prsd$nextword == "which")/(nrow(sub_prsd) -1)
  bomtrain$by_the_power[i] <- sum(sub_prsd$token=="by" &
                                    sub_prsd$nextword == "the" & sub_prsd$nextnextword == "power")/(nrow(sub_prsd) -2)
  bomtrain$passed_away[i] <- sum(sub_prsd$token=="passed"
                                 & sub_prsd$nextword == "away")/(nrow(sub_prsd) -1)
  bomtrain$write[i] <- sum(sub_prsd$token=="write") / nrow(sub_prsd)
  bomtrain$holy_ghost[i] <- sum(sub_prsd$token %in% c("Holy", "holy")
                                & sub_prsd$nextword %in% c("Ghost", "ghost"))/(nrow(sub_prsd) -1)
  bomtrain$say_unto_you[i] <- sum(sub_prsd$token == "say" &
                                    sub_prsd$nextword == "unto" & sub_prsd$nextnextword == "you")/(nrow(sub_prsd) -2)
  bomtrain$came_to_pass[i] <- sum(sub_prsd$token == "came" &
                                    sub_prsd$nextword == "to" & sub_prsd$nextnextword == "pass")/(nrow(sub_prsd) -2)
  bomtrain$these_are_they[i] <- sum(sub_prsd$token == "these" &
                                      sub_prsd$nextword == "are" & sub_prsd$nextnextword == "they")/(nrow(sub_prsd) -2)
  # Common bigrams
  bomtrain$det_noun[i] <- sum(sub_prsd$pos=="DET" &
                                sub_prsd$nextpos == "NOUN")/nrow(sub_prsd)
  bomtrain$adp_det[i] <- sum(sub_prsd$pos=="ADP" &
                               sub_prsd$nextpos == "DET")/nrow(sub_prsd)
  bomtrain$pron_verb[i] <- sum(sub_prsd$pos=="PRON" &
                                 sub_prsd$nextpos == "VERB")/nrow(sub_prsd)
  # Calculate percentage of each major part of speech by author
  sub_prsd_no_punct <- filter(sub_prsd, dep_rel!="punct")
  bomtrain$total_words[i] <- nrow(sub_prsd_no_punct)
  bomtrain$verb[i] <- nrow(filter(sub_prsd, grepl(pattern = "VERB",
                                                  x=sub_prsd$pos)))/nrow(sub_prsd)
  bomtrain$adj[i] <- nrow(filter(sub_prsd, grepl(pattern = "ADJ",
                                                 x=sub_prsd$pos)))/nrow(sub_prsd)
  bomtrain$adv[i] <- nrow(filter(sub_prsd, grepl(pattern = "ADV",
                                                 x=sub_prsd$pos)))/nrow(sub_prsd)
  bomtrain$noun[i] <- nrow(filter(sub_prsd, grepl(pattern = "NOUN",
                                                  x=sub_prsd$pos)))/nrow(sub_prsd)
  bomtrain$conj[i] <- nrow(filter(sub_prsd, grepl(pattern = "CONJ",
                                                  x=sub_prsd$pos)))/nrow(sub_prsd)
  bomtrain$det[i] <- nrow(filter(sub_prsd, grepl(pattern = "DET",
                                                 x=sub_prsd$pos)))/nrow(sub_prsd)
  bomtrain$pron[i] <- nrow(filter(sub_prsd, grepl(pattern = "PRON",
                                                  x=sub_prsd$pos)))/nrow(sub_prsd)
  bomtrain$sentence_length[i] <- nrow(sub_prsd) / sum(sub_prsd$token==".")
}
bomtrain[is.na(bomtrain)] <- 0
#bomtrain <- bomtrain[,-which(is.na(colnames(bomtrain)))]
#bomtrain <- bomtrain[,-14]
# Plot distributions of each part of speech
par(mfrow=c(3,3))
for(i in 3:ncol(bomtrain)){
  hist(bomtrain[,i], xlab=gsub("_", "+", colnames(bomtrain)[i]), main="", col="light blue")
}
# Dispersions & Distributions ----------------------------------------------
# Look at dispersions and word frequencies of words
sub_df <- readtext(bom_paths)
bom_corpus <- corpus(sub_df)
summary(bom_corpus)
docvars(bom_corpus) <- bomtrain
bom_tokens <- tokens(bom_corpus, include_docvars=TRUE, remove_punct = TRUE,
                     remove_numbers = TRUE, remove_symbols = TRUE, what = "word")
bom_dfm <- dfm(bom_tokens)
topfeatures(bom_dfm)
bom_disp <- dispersions_all(bom_dfm) # All dispersions
word_freq <- textstat_frequency(bom_dfm)
ggplot(word_freq[1:100,], aes(x = rank, y = frequency)) +
  geom_point(shape = 1, alpha = .5) +
  theme_classic()
ggplot(bom_disp, aes(x=freq, y=DP)) + geom_point(col="dark green")
bom_d <- bom_disp %>% rownames_to_column("token") %>% arrange(DP)
bom_d$token <- factor(bom_d$token, levels=bom_d[order(bom_d$DP,decreasing=T),]$token)
every_nth = function(n) {
  return(function(x) {x[c(TRUE, rep(FALSE, n - 1))]})
}
#bom_d <- bom_d[-which(grepl("\\.", as.character(bom_d$token))),]
ggplot(bom_d, aes(x=as.factor(token))) +
  geom_point(aes(y=DP), col="dark green") +
  theme_bw() +
  scale_x_discrete(breaks=every_nth(250)) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(x="Token")
# JS and BOM word usage Plot ---------------------------------------------------
js_files <- which(grepl("joseph", bomfiles))
bom_only <- readtext(bomfiles[-js_files])
bom_only$text <- gsub(’[[:punct:] ]+’,’ ’,bom_only $text, perl = T)
js_only <- readtext(bomfiles[js_files])
js_only$text <- gsub(’[[:punct:] ]+’,’ ’,js_only $text, perl = T)
bom_only_corpus <- corpus(bom_only)
js_only_corpus <- corpus(js_only)
bom_tokens_count <- tokens(bom_only_corpus) %>% unlist %>% tolower %>%
  table %>% data.frame
colnames(bom_tokens_count) <- c("Token", "Frequency_bom")
js_tokens_count <- tokens(js_only_corpus) %>% unlist %>% tolower %>%
table %>% data.frame
colnames(js_tokens_count) <- c("Token", "Frequency_js")
all_tokens_count <- merge(bom_tokens_count, js_tokens_count,
                          by="Token", all=TRUE)
all_tokens_count[is.na(all_tokens_count)] <- 0
all_tokens_count$NFbom <- (all_tokens_count$Frequency_bom /
                             sum(all_tokens_count$Frequency_low))*100
all_tokens_count$NFjs <- (all_tokens_count$Frequency_js /
                            sum(all_tokens_count$Frequency_high))*100
all_tokens_count$NFbom <- all_tokens_count$Frequency_bom/sum(all_tokens_count$Frequency_bom)
all_tokens_count$NFjs <- all_tokens_count$Frequency_js/sum(all_tokens_count$Frequency_bom)
all_tokens_count_subset <- subset(all_tokens_count, NFbom < 0.02 &
                                    NFjs < 0.02)
ggplot(data=all_tokens_count_subset, mapping=aes(x=NFbom, y=NFjs),
       label=Token) +
  geom_point() +
  theme_bw() +
  theme(axis.title=element_text(size=15), axis.text=element_text(size=12)) +
  geom_abline(slope=0.2, color="red", linetype="dashed") +
  labs(x="Book of Mormon", y="Joseph Smith",
       caption = "Figure 1: Normalized Frequency of Tokens in Book
of Mormon vs. Joseph Smith") +
  geom_text(aes(label=ifelse(all_tokens_count_subset$NFbom > 0.013 |
                               all_tokens_count_subset$NFjs > 0.0017 &
                               !all_tokens_count_subset$Token %in% c("the", "in"),
                             as.character(Token), ’’)) , hjust=-0.2, vjust=0)
# Random Forest JS vs BOM -----------------------------------------------------------
set.seed(12)
bomtrain$js <- ifelse(bomtrain$author=="Joseph", "Joseph", "BOM")
bomall <- bomtrain
# Sample separately from Joseph Smith and other authors to make sure
# we have some
# of Smith’s work to go off of
# Regular Random Forest
js_rows <- sample(which(bomtrain$js=="Joseph"), 6, replace=FALSE)
bom_rows <- sample(which(bomtrain$js=="BOM"), 28, replace=FALSE)
train_rows <- c(js_rows, bom_rows)
bom_train <- bomall[train_rows,]
bom_test <- bomall[-train_rows,]
bom_m1 <- randomForest(formula = as.factor(js) ~ . -author -total_words,
                       data = bom_train)
pred_df <- data.frame(matrix(nrow=nrow(bomall[-train_rows,]), ncol=2, data=NA))
colnames(pred_df) <- c("actual", "pred")
pred_df$actual <- bomall[-train_rows,]$js
pred_df$pred <- predict(bom_m1, bom_test) %>% as.character
imp <- bom_m1$importance %>%
  data.frame() %>%
  rownames_to_column("feature") %>%
  dplyr::arrange(desc(MeanDecreaseGini)) %>%
  dplyr::top_n(20)
imp$feature <- gsub("_", "+", imp$feature)
imp$feature[which(imp$feature=="sentence+length")] <- "sentence length"
imp %>%
  ggplot(aes(x = reorder(feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Gini") +
  ggtitle("Top 20 important variables (JS vs. BOM)") +
  theme_classic()
# Leave one out
pred_df <- data.frame(matrix(nrow=nrow(bomall), ncol=2, data=NA))
colnames(pred_df) <- c("actual", "pred")
pred_df$actual <- bomall$js
for(i in 1:nrow(bomall)){
  bom_train <- bomall[-i,]
  bom_test <- bomall[i,]
  bom_model <- randomForest(formula = as.factor(js) ~ . -author -total_words, data = bom_train)
  pred_df$pred[i] <- predict(bom_model, bom_test) %>% as.character
}
pred_df
# RF Book of Mormon
bom_only_all <- bomall[-which(bomall$author=="Joseph"),]
bom_train_rows <- sample(1:nrow(bom_only_all), 22, replace=FALSE)
bom_train_nojs <- bom_only_all[bom_train_rows,]
bom_test_nojs <- bom_only_all[-bom_train_rows,]
bom_m2 <- randomForest(formula = as.factor(author) ~ . -total_words -js,
                       data = bom_train_nojs)
pred_bom <- predict(bom_m2, bom_test_nojs)
pred_bom %>% data.frame()
imp <- bom_m2$importance %>%
  data.frame() %>%
  rownames_to_column("feature") %>%
  dplyr::arrange(desc(MeanDecreaseGini)) %>%
  dplyr::top_n(20)
imp$feature <- gsub("_", "+", imp$feature)
imp$feature[which(imp$feature=="sentence+length")] <- "sentence length"
imp %>%
  ggplot(aes(x = reorder(feature, MeanDecreaseGini), y = MeanDecreaseGini)) +
  geom_col() +
  coord_flip() +
  labs(x = "", y = "Mean Decrease Gini") +
  ggtitle("Top 20 important variables (Within BOM)") +
  theme_classic()
# Leave One Out - BOM only
pred_df <- data.frame(matrix(nrow=nrow(bom_only_all), ncol=2, data=NA))
colnames(pred_df) <- c("actual", "pred")
pred_df$actual <- bom_only_all$author
for(i in 1:nrow(bom_only_all)){
  bom_train <- bom_only_all[-i,]
  bom_test <- bom_only_all[i,]
  bom_model <- randomForest(formula = as.factor(author) ~ . -js -total_words, data = bom_train)
  pred_df$pred[i] <- predict(bom_model, bom_test) %>% as.character
}
pred_df
