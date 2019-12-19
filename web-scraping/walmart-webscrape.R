# Constrained Optimization: Best food for lower price
library(rvest)
library(dplyr)
library(stringr)
library(httr)
library(purrr)
library(parallel)
library(magrittr)

# Food, Price, and Nutrition Dataframe ------------------------------------

nutrients <- c("Total Fat", "Saturated Fat", "Trans Fat", "Polyunsaturated Fat",
               "Monounsaturated Fat", "Cholesterol", "Sodium", "Total Carbohydrate",
               "Fiber", "Sugars", "Protein")

vitamins <- c("Vitamin A", "Vitamin C","Calcium", "Iron", "Vitamin D", "Vitamin B12", "Phosphorous")

nutrition.df <- data.frame(matrix(ncol=4+length(nutrients)+length(vitamins) , data=NA))
colnames(nutrition.df) <- c("Category", "Item", "Price", "Calories", "Calories.Fat", nutrients, vitamins)

page <- "https://www.walmart.com/ip/Rice-Dream-Enriched-Original-Organic-Rice-Milk-64-fl-oz/28645690" # Food item
webpage <- read_html(page)

# Item
item_node <- html_nodes(webpage,'.ProductTitle')[1] %>% as.character()
item <- gsub(pattern = ".*content", replacement = "", x = item_node) 
item <- gsub(pattern = "div.*", replacement = "", x = item)[1] %>% str_replace_all("[^[:alnum:]]", " ") %>% str_trim()

# Calories
calories_node <- html_nodes(webpage, ".nutrition-facts-all-facts-calorie-info") %>% as.character()
calories <- gsub(pattern = ".*Calories</span><span>", replacement = "", x = calories_node) %>%  
  gsub(pattern = "Cal.*", replacement = "") %>% str_trim() %>% as.numeric()

calories.from.fat <- gsub(pattern = ".*Fat", replacement = "", x = calories_node) %>%
  gsub(pattern = "</span><span>", replacement = "") %>% gsub(pattern = "Cal.*", replacement = "") %>% str_trim() %>% as.numeric()

# Nutrition
nutrition_node <- html_nodes(webpage, ".nutrition-facts-all-facts-nutrient-info") %>% as.character()


nutrient.df <- data.frame(matrix(nrow=length(nutrients), ncol=2, data=NA))
colnames(nutrient.df) <- c("nutrient", "value")
for(i in 1:length(nutrients)){
  nutrient.df[i,1] <- nutrients[i]
  nutrient.df[i,2] <- gsub(paste0(".*", nutrients[i]), "", node) %>% substr(8,11) %>% as.numeric() # works sometimes
  
  nut <- gsub(paste0(".*", nutrients[i]), "", node) 
  nut2 <- gsub("(</span>)(\\d+)(.*</span>)", paste0("\\2"), nut)
 
}
nutrient.df





# All nutrient values
nutrient.vals <- nutrient.value(nutrient = nutrients,
                                node = basic.node.dirty(".nutrition-facts-all-facts-nutrient-info")) %>% data.frame()

# Vitamins
vitamin.vals <- nutrient.value(nutrient = vitamins,
                               node = basic.node.dirty(".nutrition-facts-all-facts-vitamins-minerals-info"))

# Missed: Vitamin C, Iron, Phosphorous
gsub(".*Vitamin A", "", vitamin_node)  %>% substr(8,11) %>% as.numeric()

# String w/ numbers -> numbers only
#as.numeric(unlist(regmatches(nutrition_node,
#gregexpr("[[:digit:]]+\\.*[[:digit:]]*",nutrition_node)))) 

nutrition.df$Item <- item
nutrition.df$Calories <- calories
nutrition.df$Calories.Fat <- calories.from.fat
nutrition.df[,5:15] <- nutrient.vals[,2]
nutrition.df[,16:22] <- vitamin.vals[,2]

#Price
price_node <- html_nodes(webpage, ".nutrition-facts-all-facts-calorie-info") %>% as.character()





# Multiple Items on a Page ------------------------------------------------
page <- read_html("https://www.walmart.com/browse/food/976759")
websites <- page %>% html_nodes(".display-block") %>% html_attr('href') %>% na.omit()
websites <- websites[which(grepl("ip", websites))]
websites <- paste0("walmart.com", websites)

page %>% html_nodes(".search-result-gridview-items four-items") %>% html_attr('href') %>% na.omit()







