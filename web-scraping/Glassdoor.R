install.packages("Rcrawler", INSTALL_opts = "--no-multiarch")
library(Rcrawler)
install.packages('xml2')
library(xml2)
install.packages("rvest")
library(rvest)
install.packages("stringr")
library(stringr)
install.packages('XML')
library(XML)
install.packages('tidyverse')
library(tidyverse)
install.packages("httr")
library(httr)
install.packages('purrr')
library(purrr)
library(dplyr)
install.packages('xtable')
library(xtable)
install.packages('sendmailR')
library(sendmailR)

Rcrawler(Website = "https://www.glassdoor.com/Reviews", no_cores = 4, no_conn = 4)
# Dataframe of company reviews
index2 <- subset(INDEX, grepl("Working-at-", INDEX$Url))

index2$compname <- NA
index2$id <- NA

for(i in 1:nrow(index2)){
  u <- index2$Url[i]
  keywords <- ".*(EI_IE)"
  stage1 <- sub(keywords, "", u)
  id <- sub("\\..*", "", stage1)
  
  company.key <- ".*(Working-at-)"
  str1 <- sub(company.key, "", u)
  id2 <- sub("\\-EI.*", "", str1)
  
  comp.name <- NA
  
  if(grepl("'", id2) & grepl("and", u)==FALSE) {
    comp.name <- gsub("-", "+", id2)
  }
  
  if(grepl("-", id2) & grepl("and", u)==FALSE) {
    comp.name <- gsub("-", "+", id2)
  }
  
  if(grepl("-", id2)==FALSE){
    comp.name <- id2
  }
  
  if(grepl("-", id2) & grepl("and", u)==TRUE) {
    comp.name <- gsub("-", "+", id2)
    comp.name <- ifelse(grepl("and", comp.name), gsub("and", "", comp.name), comp.name)     
    comp.name <- ifelse(grepl("\\++", comp.name),  gsub("\\++", "+", comp.name), comp.name)  
    
  }
    
  if(grepl("-s", id2)) {
      comp.name <- gsub("-s", "s", id2)
    } 
    
  index2$id[i] <- id
  index2$compname[i] <- comp.name
}


index3 <- subset(INDEX, grepl("/Reviews/", INDEX$Url) & grepl("-Reviews-", INDEX$Url))
index3$compname <- NA
index3$id <- NA

for(i in 1:nrow(index3)){
  u <- index3$Url[i]
  keywords <- ".*Reviews/"
  stage1 <- sub(keywords, "", u)
  compname <- sub("-Reviews.*", "", stage1)
  
  sub1 <- sub(".*-Reviews-", "", stage1)
  sub2 <- sub("\\_.*", "", sub1)
  sub3 <- sub("E", "", sub2)
  id <- sub(".htm", "", sub3)
  
  index3$id[i] <- id
  index3$compname[i] <- compname
}

rownames(index3) <- NULL
                

index.useful <- rbind(index2[,11:12], index3[,11:12])
index.useful <- subset(index.useful, index.useful$id!="I")
index.useful <- subset(index.useful, !duplicated(index.useful$compname))
index.useful <- subset(index.useful, grepl("EI_IE", index.useful$compname)==FALSE)
rownames(index.useful) <- NULL

#
index.useful$compAndBenefits <- NA
index.useful$cultureAndValues <- NA
index.useful$careerOpportunities <- NA
index.useful$workLife <- NA
index.useful$seniorManagement <- NA

for(i in 1:nrow(index.useful)){
  ratings <- paste0("https://www.glassdoor.com/api/employer/", index.useful$id[i], "-rating.htm?",
                    "locationStr=&jobTitleStr")
  
  req_obj <- GET(ratings)
  cont <- content(req_obj)
  
  ratings_df <- map(cont$ratings, bind_cols) %>% bind_rows()
  index.useful$compAndBenefits[i] <- ratings_df[5,3]
  index.useful$cultureAndValues[i] <- ratings_df[6,3]
  index.useful$careerOpportunities[i] <- ratings_df[7,3]
  index.useful$workLife[i] <- ratings_df[8,3]
  index.useful$seniorManagement[i] <- ratings_df[9,3]
}



index.useful$compAndBenefits <- as.numeric(index.useful$compAndBenefits)
index.useful$cultureAndValues <- as.numeric(index.useful$cultureAndValue)
index.useful$careerOpportunities <- as.numeric(index.useful$careerOpportunities)
index.useful$workLife <- as.numeric(index.useful$workLife)
index.useful$seniorManagement <- as.numeric(index.useful$seniorManagement)


#

from <- "<mitchellpudil3@gmail.com>"
to <- "<mitchellpudil3@gmail.com>"
subject <- "Performance Result"
body <- write.csv(index.useful)
mailControl=list(smtpServer="ASPMX.L.GOOGLE.COM")

attachmentPath <- "/Users/mitchellpudil/ratings.csv"
attachmentName <- "ratings.csv"

attachmentObject <- mime_part(x=attachmentPath,name=attachmentName)
bodyWithAttachment <- list(body,attachmentObject)
sendmail(from=from,to=to,subject=subject,msg=bodyWithAttachment,control=mailControl)











