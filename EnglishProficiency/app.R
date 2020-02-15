library(shiny)
library(ggplot2)
library(dplyr)
library(ngram)
library(randomForest)
library(spacyr)
library(gridExtra)

library(xml2)
library(tidyverse)
library(quanteda)
library(readtext)
library(reshape2)
library(ggpubr)
library(DescTools)
library(randomForest)
library(corpus)

library(cowplot)
library(yaml)




text2pred <- function(text){
  assertthat::assert_that(wordcount(text) > 100, msg = "Please write more.")
  text <- as.character(text)
  english <- read.csv("https://raw.githubusercontent.com/mpudil/projects/master/EnglishProficiency/Data/english_results.csv")
  
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
  #hb_dict <- dictionary(file = "C:/Users/Mitchell Pudil/Documents/textstat_tools/dictionaries/hedges_boosters.yml")
  
  #h <- dictionary(file = "https://raw.githubusercontent.com/mpudil/projects/master/EnglishProficiency/Data/hedges_boosters.yml")
  
  hb_dict <- read_yaml("https://raw.githubusercontent.com/mpudil/projects/master/EnglishProficiency/Data/hedges_boosters.yml") %>%
    dictionary
  
  # Use actual tokens instead of POS
  
  hb_tokens <- tokens_lookup(c(tokens(text)), dictionary = hb_dict, levels = 1)
  
  pos_cols$confidencehedged <- sum(hb_tokens=="ConfidenceHedged")
  pos_cols$confidencehigh <- sum(hb_tokens=="ConfidenceHigh")
  
  
  # Fill NA with 0
  pos_cols[is.na(pos_cols)] <- 0
  
  # Random Forest model
  rf <- randomForest(formula = test_score ~ ., data=english)
  
  # Most important variables
  
  imp <-rf$importance %>%
    data.frame() %>%
    rownames_to_column("feature") %>%
    dplyr::arrange(desc(IncNodePurity)) %>%
    dplyr::top_n(10)
  
  # Comments for important variables
  imp$comment <- c("Use more unique words",
                   "Use more words in general",
                   "Use phrases that convey stronger feelings about the concept \n (e.g. 'it is crucial to understand...')",
                   "Use bigger words", 
                   "Use more noun phrases",
                   "Add more sentences", 
                   "Use more auxiliary verbs (e.g. would, could, be, do, have)",
                   "Use more prepositional phrases", 
                   "Use more adjectives", 
                   "Use fewer pronoun-verb phrases (such as 'I ran' or 'he walked')")
                   
  # Make prediction 
  pred <- predict(rf, pos_cols)
  if(pred < 0) {
    pred <- 0
  }
  if(pred > 1) {
    pred <- 1
  }
  
  pred <- round(pred*100, 2)
  
  
  # Top ways to improve
  improve <- cbind(imp, them=NA, you=NA, youscale=NA, score=NA)
  
  
  
  for(i in 1:nrow(improve)){
    c <- which(colnames(pos_cols)==imp$feature[i])
    x <- english[which(english$test_score==1),c]
    #a <- english[,c]
    
    improve$them[i] <- mean(x)
    improve$you[i] <- pos_cols[1,c]
    
    improve$youscale[i] <- (improve$you[i]-min(x))/(max(x)-min(x)) # Compare you with high scorers
    
    dif <- ifelse(improve$feature[i]=="PRON.VERB", improve$youscale[i], 1-improve$youscale[i])
    improve$score[i] <- (dif)*improve$IncNodePurity[i]*-1
    
  }
  
  improve <- arrange(improve, score)
  
  
  # Create table for plots based off of structures most important to improve
  
  struct <- improve$feature[1:3]
  for(i in 1:length(struct)){
    struct[i] <- ifelse(struct[i]=="uniquewords", "Unique Words", struct[i])
    struct[i] <- ifelse(struct[i]=="ADJ.PART", "Adj + Infinitive Phrases \n (e.g. 'crucial to understand')", struct[i])
    struct[i] <- ifelse(struct[i]=="ADP.NOUN", "Noun Phrases", struct[i])
    struct[i] <- ifelse(struct[i]=="would", "Auxiliary verbs", struct[i])
    struct[i] <- ifelse(struct[i]=="ADJ", "Adjectives", struct[i])
    struct[i] <- ifelse(struct[i]=="av_word_len", "Average Word Length", struct[i])
    struct[i] <- ifelse(struct[i]=="sentences", "Sentences", struct[i])
    struct[i] <- ifelse(struct[i]=="of", "Prepositional Phrases", struct[i])
    struct[i] <- ifelse(struct[i]=="PRON.VERB", "Pron-Verb Phrases", struct[i])
    struct[i] <- ifelse(struct[i]=="words", "Words", struct[i])
  }
  

  pass_avg <- sapply(english[english$test_score==1,], mean)[improve$feature[1:3]] 
  names(pass_avg) <- NULL
  you <- pos_cols[improve$feature[1:3]] %>% as.numeric
  all_results <- data.frame(struct, pass_avg, you)
  nyxlong <- reshape2::melt(all_results, id=c("struct"))
  nyxlong$variable <- ifelse(nyxlong$variable=="you", "You", "Average of Exam Passers")
  nyxlong$pred <- pred
  
  return(nyxlong)

  
  ###############################  
  
  
  

  
  
  
  #ggarrange(gg1, gg3, ncol=1, heights=c(2,2,2))
  # grid.arrange(gg1, gg2, gg3, ncol=2)
  plot_grid(gg1, gg3, ncol=1, align="v")
  
  
}




####################

#english <- read.csv("english_cleaned5.csv", stringsAsFactors = FALSE)
library(shinycssloaders)

ui <- fluidPage(
  titlePanel("Are You Prepared for the TOEFL?"),
  sidebarLayout(
    sidebarPanel(
      textAreaInput("text", "Instructions: Write an essay on the sample prompt below or any 
      other TOEFL prompt. When done, click the 'Grade Essay' button to see your results.",
                    "What do you think the future of cars looks like? (Write here)", height="800px"),
      actionButton("run", label = "Grade Essay")
    ),
    mainPanel(
      withSpinner(plotOutput("graph1")), withSpinner(plotOutput("graph2")), withSpinner(plotOutput("graph3"))
    )
  )
)

#####################





#####
server <- shinyServer(function(input, output){
  observeEvent(input$run, {
    dataInput <- reactive({
      nyxlong <- text2pred(input$text)
    })
  
  
    # Graph 1
    output$graph1 <- renderPlot({
      # Plot scores
      nyxlong <- dataInput()
      ggplot(nyxlong[c(1,4),]) +
        geom_bar(aes(x = variable, y = value, fill = variable), 
                 stat="identity", position = "dodge", width = 0.7) +
        scale_fill_manual("", values = c("red","blue"), 
                          labels = c("Average Passing", "You")) +
        labs(x="", y=ifelse(substr(nyxlong$struct[1], 1, 7) == "Adj + I", "Adj + Infinitive Phrases", 
                            nyxlong$struct[1] %>% as.character)) +
         ggtitle(paste0("Your probability of passing the written part of the TOEFL is ", nyxlong$pred[1], "%. \n \n",
                         "Most Important Gramatical/Sentence Structures to Improve: \n \n
                        Priority 1: ", ifelse(nyxlong$struct[1]=="Pron-Verb Phrases", "Decreasing Number of ", "Increasing Number of "),
                         nyxlong$struct[1])) +
        theme_bw(base_size = 14) +
        theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=25),
              axis.title=element_text(size=20),
              axis.text = element_text(size=17)) +
        ylim(0,1.5*max(nyxlong[c(1,4),]$value)) +
        geom_text(aes(x = variable, y = value, label=round(value, 2)),
                  hjust = ifelse(nyxlong[c(1,4),]$variable=="you", -1, 1.25),
                  vjust = -1, size=10)
      
    })
    
    # Plot 2
    output$graph2 <- renderPlot({
      nyxlong <- dataInput()
      ggplot(nyxlong[c(2,5),]) +
        geom_bar(aes(x = variable, y = value, fill = variable), 
                 stat="identity", position = "dodge", width = 0.7) +
        scale_fill_manual("", values = c("red","blue"), 
                          labels = c("Average Passing", "You")) +
        labs(x="", y=ifelse(substr(nyxlong$struct[2], 1, 7) == "Adj + I", "Adj + Infinitive Phrases", 
                            nyxlong$struct[2] %>% as.character), 
             title = paste0("Priority 2: ", ifelse(nyxlong$struct[2]=="Pron-Verb Phrases", "Decreasing Number of ", 
                                                   "Increasing Number of "), nyxlong$struct[2])) +
        theme_bw(base_size = 14) +
        theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=25),
              axis.title=element_text(size=20),
              axis.text = element_text(size=17)) +
        ylim(0,1.5*max(nyxlong[c(2,5),]$value)) +
        geom_text(aes(x = variable, y = value, label=round(value, 2)),
                  hjust = ifelse(nyxlong[c(2,5),]$variable=="you", -1, 1.25),
                  vjust = -1, size=10)
    
    })
    
    
    # Graph 3
    
    output$graph3 <- renderPlot({
      nyxlong <- dataInput()
      ggplot(nyxlong[c(3,6),]) +
        geom_bar(aes(x = variable, y = value, fill = variable), 
                 stat="identity", position = "dodge", width = 0.7) +
        scale_fill_manual("", values = c("red","blue"), 
                          labels = c("Average Passing", "You")) +
        labs(x="", y=ifelse(substr(nyxlong$struct[3], 1, 7) == "Adj + I", "Adj + Infinitive Phrases", 
                            nyxlong$struct[3] %>% as.character), 
             title = paste0("Priority 3: ", ifelse(nyxlong$struct[3]=="Pron-Verb Phrases", "Decreasing Number of ", 
                                                   "Increasing Number of "), nyxlong$struct[3]),
             caption = "*Note that improvements for all comparisons are based on percentile ranking, so 
           even if you seem to be doing better than the average exam passer, it may still help 
           improve your chances to change your writing style or grammar structure accordingly.") +
        theme_bw(base_size = 14) +
        theme(legend.position = "none", plot.title = element_text(hjust=0.5, size=25),
              axis.title=element_text(size=20),
              axis.text = element_text(size=17)) +
        ylim(0,1.5*max(nyxlong[c(3,6),]$value)) +
        geom_text(aes(x = variable, y = value, label=round(value, 2)),
                  hjust = ifelse(nyxlong[c(3,6),]$variable=="you", -1, 1.25),
                  vjust = -1, size=10)
      
      })
    })
    
  })


shinyApp(ui, server)












