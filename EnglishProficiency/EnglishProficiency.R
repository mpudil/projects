require(xml2)
library(tidyverse)
library(quanteda)
library(readtext)
library(reshape2)
library(ggplot2)
library(ggpubr)
library(DescTools)
library(magrittr)
library(randomForest)
library(cluster)
library(corpus)
library(ggpubr)
source("C:/Users/Mitchell Pudil/Downloads/G_Test_DescTools.R") 
# Let's load our functions.
setwd("C:/Users/Mitchell Pudil/Documents/textstat_tools/")
source("functions/helper_functions.R")
source("functions/keyness_functions.R")
source("C:/Users/Mitchell Pudil/Desktop/CMU1/Linear Models/heatmapcreator.R") 

setwd("C:/Users/Mitchell Pudil/Documents/textstat_tools/data/meta_data")
files_meta <- read.csv("midterm_meta.csv", header=TRUE)
setwd("C:/Users/Mitchell Pudil/Documents/textstat_tools/")

files_list <- files_meta$file_path

# We now need to separate the files by grade
# - low
# - medium (commented out, but may be used for future work if desired)
# - high

low <- files_list[which(files_meta$test_score=="low")] %>% as.character

# And we'll do the same for the medium scores
# medium <- files_list[which(files_meta$test_score=="medium")] %>% as.character


# And high
high <- files_list[which(files_meta$test_score=="high")] %>% as.character


# Now we'll use the readtext function to extract the text.
df_low <- readtext(low)
# df_medium <-  readtext(medium)
df_high <- readtext(high)

# Convert these into three corpora...
low_corpus <- corpus(df_low)
# medium_corpus <- corpus(df_medium)
high_corpus <- corpus(df_high)

# Quickly tokenize our corpora...
low_tokens <- tokens(low_corpus, what = "word", remove_punct = T)
# medium_tokens <- tokens(medium_corpus, what = "word", remove_punct = T)
high_tokens <- tokens(high_corpus, what = "word", remove_punct = T)

# Create our dfms...
low_dfm <- dfm(low_tokens)
# medium_dfm <- dfm(medium_tokens)
high_dfm <- dfm(high_tokens)


# Check our token frequencies...
textstat_frequency(low_dfm, n = 25)


# key_shakes_all <- keyness_pairs(low_dfm, medium_dfm, high_dfm)
key_shakes_hl <- keyness_pairs(low_dfm, high_dfm) 
# Most significant words: i, you, he, eveverything, student, go, 
# understanding, number, often, being, of would




# POS Keyness -------------------------------------------------------------
sub_prsd_low <- spacy_parse(low_corpus, pos = TRUE, tag = TRUE)
# sub_prsd_medium <- spacy_parse(medium_corpus, pos = TRUE, tag = TRUE)
sub_prsd_high <- spacy_parse(high_corpus, pos = TRUE, tag = TRUE)
# sub_prsd_all <- rbind(sub_prsd_low, sub_prsd_medium, sub_prsd_high)
sub_prsd_all <- rbind(sub_prsd_low, sub_prsd_high)

sub_tokens <- as.tokens(sub_prsd_all, include_pos = "pos", concatenator = "_")


sub_tokens <- tokens_select(sub_tokens, "_[A-Z]", selection = "keep", 
                            valuetype = "regex", case_insensitive = T)
sub_tokens <- tokens_select(sub_tokens, "\\W_", selection = "remove", 
                            valuetype = "regex")
sub_tokens <- tokens_select(sub_tokens, "\\d_", selection = "remove", 
                            valuetype = "regex")

sub_tokens <- lapply(sub_tokens, function(x) gsub(pattern = ".*_", "", x)) %>% as.tokens() 
sub_dfm <- dfm(sub_tokens)


# Separate low/high

docvars(sub_dfm, "score") <- c(rep("low", 100), rep("high", 100))
low_index <- docvars(sub_dfm, "score") == "low"
# medium_index <- docvars(sub_dfm, "score") == "medium"
high_index <- docvars(sub_dfm, "score") == "high"

# High index is target
report_keywords <- textstat_keyness(sub_dfm, high_index, measure = "lr")
report_keywords

high_keywords <- textstat_keyness(sub_dfm, high_index, measure = "lr") %>% data.frame
high_keywords$feature <- toupper(high_keywords$feature)
high_keywords

# Find the 5 POS that differentiates low and high the most.
arrange(high_keywords, desc(abs(G2)))$feature[1:5] 
# "ADJ"   "PRON"  "PUNCT" "ADP"   "ADV"  


# Create columns for POS, average word length, number of words, etc -------------

# Determine top 10 words that differentiate low vs. high
top10words <- c(key_shakes_hl %>% head %>% rownames, key_shakes_hl %>% tail %>% rownames)


pos_cols <- data.frame(file_path = unique(sub_prsd_all$doc_id), words=NA, av_word_len = NA, 
                       sentences = NA, uniquewords = NA)

for(i in 1:length(top10words)){
  pos_cols <- cbind(pos_cols, NA)
  colnames(pos_cols)[ncol(pos_cols)] <- top10words[i]
}

for(j in 1:nrow(pos_cols)){
  df <- sub_prsd_all[which(sub_prsd_all$doc_id==pos_cols$file_path[j]),]
  
  # Create bigrams of parts of speech
  for(k in 1:nrow(df)){
    df$nextpos[k] <- ifelse(k==nrow(df), NA, df$pos[k+1])
  }
  df$bigram <- paste(df$pos, df$nextpos, sep = "-")
  
  # Add all parts of speech to pos_cols df
  t <- table(df$pos)
  for(i in 1:nrow(t)){
    if(names(t[i]) %in% colnames(pos_cols)){
      w <- which(names(t[i]) == colnames(pos_cols))
      pos_cols[j,w] <- t[i]
    } else {
      pos_cols <- cbind(pos_cols, 0)
      colnames(pos_cols)[ncol(pos_cols)] <- names(t[i])
      pos_cols[j,ncol(pos_cols)] <- t[i]
    }
    
  }
  
  # Add bigram pos to pos_cols df
  b <- table(df$bigram)
  for(i in 1:nrow(b)){
    if(names(b[i]) %in% colnames(pos_cols)){
      w <- which(names(b[i]) == colnames(pos_cols))
      pos_cols[j,w] <- b[i]
    } else {
      pos_cols <- cbind(pos_cols, 0)
      colnames(pos_cols)[ncol(pos_cols)] <- names(b[i])
      pos_cols[j,ncol(pos_cols)] <- b[i]
    }
  }
  
  # Top 10 numbers
  for(i in 6:17){
    pos_cols[j,i] <- sum(df$token %>% tolower == colnames(pos_cols)[i])
  }
  
  

  # Number of unique words
  pos_cols$uniquewords[j] <- filter(df, pos!="PUNCT" & pos!="SPACE")$token %>% tolower %>% unique %>% length
  
  
  # Average word length
  pos_cols$av_word_len[j] <- mean(nchar(df$lemma[df$pos!="PUNCT" & df$pos != "SPACE"]))
  
  # Number words
  pos_cols$words[j] <- nrow(subset(df, pos!="PUNCT" & pos != "SPACE"))
  
  # Number sentences
  pos_cols$sentences[j] <- nrow(subset(df, tag=="."))
  
  
  
  
}

pos_cols$file_path <- as.character(pos_cols$file_path)
files_meta$file_path <- as.character(files_meta$file_path) %>% basename
posmeta <- merge(pos_cols, files_meta, by = "file_path")



# Hedging -----------------------------------------------------------------

# High vs. hedged confidence dictionary
hb_dict <- dictionary(file = "dictionaries/hedges_boosters.yml")

# Use actual tokens instead of POS


hb_tokens <- tokens_lookup(c(low_tokens, high_tokens), dictionary = hb_dict, levels = 1)
hb_dfm <- dfm(hb_tokens)
hb_dataframe <- convert(hb_dfm, to = "data.frame")
colnames(hb_dataframe)[1] <- "file_path"

meta <- merge(posmeta, hb_dataframe, by = "file_path")



# EDA ---------------------------------------------------------------------
meta_final <- filter(meta, test_score=="low" | test_score=="high") %>%
  mutate(highscore=as.numeric(test_score=="high"), 
         hedges_norm = (confidencehedged/words)*100,
         boosters_norm = (confidencehigh/words)*100)



# Plot Confidence vs. Scores
hb_df_low <- meta_final[which(meta_final$test_score=="low"),c(248:249)] %>% 
  gather(confidence, freq_norm)
hb_df_low$score <- "low"

hb_df_high <- meta_final[which(meta_final$test_score=="high"),c(248:249)] %>% 
  gather(confidence, freq_norm)
hb_df_high$score <- "high"

hb_df_all <- rbind(hb_df_low, hb_df_high)


ggplot(hb_df_all,aes(x = confidence, y= freq_norm, color=score, fill = score)) + 
  geom_boxplot(alpha=0.5) +
  theme_minimal() +
  scale_x_discrete(labels=c("confidencehigh" = "Boosters (High Conf)", 
                            "confidencehedged" = "Hedges (Low Conf)")) +
  labs(x="Confidence", y="Normalized Frequency") +
  theme(legend.position = "none", 
        plot.background = element_rect("#e8e8e8e8"),
        panel.grid = element_line(colour = "white",size=0.75))





# Plot pronoun usages, normalized


pronplot <- ggplot(meta_final, aes(test_score, PRON, 
                                   color=test_score, fill=test_score)) +
  geom_boxplot(alpha=0.5) +
  labs(x="Test Score", y="Pronouns") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background = element_rect("#e8e8e8e8"),
        panel.grid = element_line(colour = "white",size=0.75))


# Plot interjections, normalized


intjplot <- ggplot(meta_final, aes(test_score, INTJ, 
                                   color=test_score, fill=test_score)) +
  geom_boxplot(alpha=0.5) +
  theme_classic() +
  labs(x="Test Score", y="Interjections") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background = element_rect("#e8e8e8e8"),
        panel.grid = element_line(colour = "white",size=0.75))


# Average Word length
wordlengthplot <- ggplot(meta_final, aes(test_score, av_word_len, 
                                         color=test_score, fill=test_score)) +
  geom_boxplot(alpha=0.5) +
  theme_classic() +
  labs(x="Test Score", y="Average Word Length") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background = element_rect("#e8e8e8e8"),
        panel.grid = element_line(colour = "white",size=0.75))



# Number of unique words, normalized = # (different words / total words) * 100
uniquewordsplot <- ggplot(meta_final, aes(test_score, uniquewords, 
                                          color=test_score, fill=test_score)) +
  geom_boxplot(alpha=0.5) +
  theme_classic() +
  labs(x="Test Score", y="Unique Words") +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background = element_rect("#e8e8e8e8"),
        panel.grid = element_line(colour = "white",size=0.75))

ggarrange(pronplot, intjplot, wordlengthplot, uniquewordsplot, nrow=2, ncol=2)


# Plot most frequent words 

low_tokens_count <- tokens(low_corpus) %>% unlist %>% tolower %>% table %>% data.frame
colnames(low_tokens_count) <- c("Token", "Frequency_low")
high_tokens_count <- tokens(high_corpus) %>% unlist %>% tolower %>% table %>% data.frame
colnames(high_tokens_count) <- c("Token", "Frequency_high")

all_tokens_count <- merge(low_tokens_count, high_tokens_count, 
                          by="Token", all=TRUE)
all_tokens_count[is.na(all_tokens_count)] <- 0
all_tokens_count$NFlow <- (all_tokens_count$Frequency_low / sum(all_tokens_count$Frequency_low))*100
all_tokens_count$NFhigh <- (all_tokens_count$Frequency_high / sum(all_tokens_count$Frequency_high))*100

all_tokens_count <- filter(all_tokens_count, Token!="." & Token!=",")

max_prop <- max(c(all_tokens_count$NFlow, all_tokens_count$NFhigh))

ggplot(data=all_tokens_count, mapping=aes(x=NFlow, y=NFhigh), label=Token) +
  geom_point() +
  coord_cartesian(xlim=c(0,max_prop), ylim=c(0,max_prop)) +
  geom_abline(slope=1, color="red", linetype="dashed") +
  labs(x="Failing Scores (Avg.)", y="Passing Scores (Avg.)", 
       caption = "Normalized Frequencies of Words in Passing vs. Failing TOEFL Exams") + 
  geom_text(aes(label=ifelse((all_tokens_count$NFlow > 1.15 | all_tokens_count$NFhigh > 1.15),
                             as.character(Token), ''), hjust=-0.2, vjust=0)) +
  theme_minimal() +
  theme(legend.position = "none", 
        plot.background = element_rect("#e8e8e8e8"),
        panel.grid = element_line(colour = "white",size=0.75))



# Correlation Matrix

heatmapCreator(meta_final[,-c(1,11:14,18:19)])



# Modeling ---------------------------------------------

# Full model
glm.1 <- glm(highscore ~ pron + adj + adp + intj + punct + words + av_word_len + sentences +
               confidencehedged + confidencehigh + uniquewords, data=meta_final, family="binomial")

summary(glm.1)
car::vif(glm.1) # Note the high collinearity between words and average word length
PseudoR2(glm.1, which = "Nagelkerke")
Cstat(glm.1)

# Without word length (since collinear with words)
glm.2 <- glm(highscore ~ pron + adj + adp + intj + punct + av_word_len + sentences +
               confidencehedged + confidencehigh + uniquewords, data=meta_final, family="binomial")

car::vif(glm.2)
summary(glm.2)

PseudoR2(glm.2, which = "Nagelkerke")
Cstat(glm.2)


# PCA

meta_pca <- meta[,which(sapply(meta, class)=="numeric")]
meta.pr <- prcomp(meta_pca, center = TRUE, scale = TRUE)

screeplot(meta.pr, type = "l", npcs = 15, main = "Screeplot of the first 10 PCs")
abline(h = 1, col="red", lty=5)
legend("topright", legend=c("Eigenvalue = 1"),
       col=c("red"), lty=5, cex=0.6)
cumpro <- cumsum(meta.pr$sdev^2 / sum(meta.pr$sdev^2))
plot(cumpro[0:15], xlab = "PC #", ylab = "Amount of explained variance", main = "Cumulative variance plot")
abline(v = 6, col="blue", lty=5)
abline(h = 0.88759, col="blue", lty=5)
legend("topleft", legend=c("Cut-off @ PC6"),
       col=c("blue"), lty=5, cex=0.6)



library("factoextra")

fviz_pca_ind(meta.pr, geom.ind = "point", pointshape = 21, 
             pointsize = 2, 
             fill.ind = meta$test_score, 
             col.ind = "black", 
             palette = "jco", 
             addEllipses = TRUE,
             label = "var",
             col.var = "black",
             repel = TRUE,
             legend.title = "Score") +
  ggtitle("2D PCA-plot from 200+ features ") +
  theme(plot.title = element_text(hjust = 0.5),
        plot.background = element_rect(fill="#e8e8e8"))



# Random Forest
meta$test_score <- as.numeric(meta$test_score == "high")
meta_final <- meta[,which(sapply(meta, class) %in% c("numeric", "integer"))]
write.csv(meta_final, "english_cleaned5.csv", row.names = FALSE)

set.seed(42)
english <- read.csv("english_cleaned5.csv")

train.rows <- sample(1:nrow(english), size = round(nrow(english))*0.7, replace=FALSE)
train <- english[train.rows,]
test <- english[-train.rows,]
rf <- randomForest(formula = test_score ~ ., data=train)

preds <- round(predict(rf, test)) %>% as.numeric
tp <- mean(preds==1 & test$test_score==1)
fp <- mean(preds==1 & test$test_score==0)
tn <- mean(preds==0 & test$test_score == 0)
fn <- mean(preds==0 & test$test_score == 1)
list(tp=tp, fp=fp, tn=tn, fn=fn)

# Full model
rf_full <- randomForest(formula = test_score ~ ., data=english)



# rpart tree
library(rpart)
library(rpart.plot)
tree <- rpart(formula = test_score ~ ., data=english)
rpart.plot(tree, box.palette = "RdBu", shadow.col = "gray", nn=TRUE) +
  theme(plot.background = element_rect(fill="#e8e8e8"))

english$test_result <- ifelse(english$test_score==1, "Pass", "Fail")

binary.model <- rpart(formula = test_result ~ ., data=english[,-which(colnames(english)=="test_score")], cp = .02)
rpart.plot(binary.model) 
  
  
  






# Importance 
imp <-rf_full$importance %>%
  data.frame() %>%
  rownames_to_column("feature") %>%
  dplyr::arrange(desc(IncNodePurity)) %>%
  dplyr::top_n(20)

imp2 <- imp
imp2$feature <- c("Unique Words", "Total Words", "Adj-Part", "Average Word Length",
                 "Prepositions", "Sentences", "Adp-Noun", "Conditional Phrases",
                 "Adjectives", "Pron-Verb", "Adv-Verb", "Adverb", "Particle-Verb", 
                 "Pronoun", "Adp-Verb", "Noun-Adp", "Adj-Noun", "Being", "Adp", 
                 "Noun-Punct")

imp2 %>%
  ggplot(aes(x = reorder(feature, IncNodePurity), y = IncNodePurity)) +
  geom_col(fill="cadetblue2") +
  coord_flip() +
  labs(x = "", y = "Node Purity") +
  ggtitle("Top 20 Important Variables") +
  theme_minimal() +
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(), axis.text.x = element_blank(),
        plot.title = element_text(size=16, hjust = 0.5), axis.title.x = element_text(size=14), 
        plot.background = element_rect(fill="#e8e8e8")) +
  geom_text(label=round(imp$IncNodePurity, 2), hjust = 1.5) 











# Predict -----------------------------------------------------------------


# Function for text to pred

text2pred <- function(text){
  text <- as.character(text)
  english <- read.csv("english_cleaned5.csv")
  
  df <- spacy_parse(text) %>% data.frame
  
  pos_cols <- data.frame(matrix(nrow=1, ncol=ncol(english)))
  colnames(pos_cols) <- colnames(english)
  
  
  for(j in 1:nrow(df)){
    # Create bigrams of parts of speech
    df$nextpos[j] <- ifelse(j==nrow(df), NA, df$pos[j+1])
  }
    
  df$bigram <- paste(df$pos, df$nextpos, sep = ".")
  
  # Add all parts of speech to pos_cols df
  t <- table(df$pos)
  for(i in 1:nrow(t)){
      w <- which(names(t[i]) == colnames(pos_cols))
      pos_cols[1,w] <- t[i]
    } 
  
  
  # Add bigram pos to pos_cols df
  b <- table(df$bigram)
  for(i in 1:nrow(b)){
      w <- which(names(b[i]) == colnames(pos_cols))
      pos_cols[1,w] <- b[i]
  }
  
  # Top 10 words
  for(i in 5:16){
    pos_cols[1,i] <- sum(df$token %>% tolower == colnames(pos_cols)[i])
  }
  
  
  # Number of unique words
  pos_cols$uniquewords <- filter(df, pos!="PUNCT" & pos!="SPACE")$token %>% tolower %>% unique %>% length
  
  
  # Average word length
  pos_cols$av_word_len <- mean(nchar(df$lemma[df$pos!="PUNCT" & df$pos != "SPACE"]))
  
  # Number words
  pos_cols$words <- nrow(subset(df, pos!="PUNCT" & pos != "SPACE"))
  
  # Number sentences
  pos_cols$sentences <- nrow(subset(df, token=="." | token=="!" | token=="?"))
  
  # Confidence
  
    # High vs. hedged confidence dictionary
  hb_dict <- dictionary(file = "C:/Users/Mitchell Pudil/Documents/textstat_tools/dictionaries/hedges_boosters.yml")
  
    # Use actual tokens instead of POS
  
  hb_tokens <- tokens_lookup(c(tokens(text)), dictionary = hb_dict, levels = 1)
  
  pos_cols$confidencehedged <- sum(hb_tokens=="ConfidenceHedged")
  pos_cols$confidencehigh <- sum(hb_tokens=="ConfidenceHigh")
  
  
  # Fill NA with 0
  pos_cols[is.na(pos_cols)] <- 0
  
  # Random Forest model
  rf <- randomForest(formula = test_score ~ ., data=english)
  
  # Make prediction for 
  pred <- predict(rf, pos_cols)
  if(pred < 0) {
    pred <- 0
  }
  if(pred > 1) {
    pred <- 1
  }
  
  pred <- round(pred*100, 2)
  print(paste0("Your probability of passing the English exam is ", pred, "%."))
  
  struct <- c("unique words", "total words", "average word length", "sentences")
  pass_avg <- sapply(english[english$test_score==1,], mean)[c("uniquewords", "words", "av_word_len", 
                                                              "sentences")] 
  names(pass_avg) <- NULL
  you <- pos_cols[c("uniquewords", "words", "av_word_len","sentences")] %>% as.numeric
  all_results <- data.frame(struct, pass_avg, you)
  nyxlong <- reshape2::melt(all_results, id=c("struct"))
  
  gg1 <- ggplot(nyxlong[-c(3, 4, 7, 8),]) +
    geom_bar(aes(x = struct, y = value, fill = variable), 
             stat="identity", position = "dodge", width = 0.7) +
    scale_fill_manual("", values = c("red","blue"), 
                      labels = c("Average Passing", "You")) +
    labs(x="",y="") +
    theme_bw(base_size = 14) +
    ylim(0,450) +
    geom_text(aes(x = struct, y = value, label=value), 
              hjust=ifelse(nyxlong[-c(3, 4, 7, 8),]$variable=="pass_avg", 1.5, -2), 
              vjust=-1)
  
  gg2 <- ggplot(nyxlong[c(3, 4, 7, 8),]) +
    geom_bar(aes(x = struct, y = value, fill = variable), 
             stat="identity", position = "dodge", width = 0.7) +
    scale_fill_manual("", values = c("red","blue"), 
                      labels = c("Average Passing", "You")) +
    labs(x=paste0("\nYour probability of passing the English exam is ", pred, "%."),y="") +
    theme_bw(base_size = 14) +
    ylim(0,25) +
    geom_text(aes(x = struct, y = value, label=round(value, 2)), 
              hjust=ifelse(nyxlong[c(3, 4, 7, 8),]$variable=="pass_avg", 1.5, -3), 
              vjust=-1)
  
  ggarrange(gg1, gg2, ncol=1)
  
  

}







