#Installing required packages for this project
install.packages("rvest")
install.packages("dplyr")
install.packages("httr")
install.packages("tidyverse")
install.packages("writexl")

# Load necessary packages
library(rvest)
library(dplyr)
library(httr)
library(tidyverse)
library("writexl")

#get_abstract() function to extract Abstract of a article
get_abstract <- function(title_link){
  title_page <- read_html(title_link)
  article_abstract <- title_page %>% html_nodes("#Abs1-content p") %>% html_text() %>% 
    paste(collapse = ",")
  return(article_abstract)
}

#Getting the Correspondent Author names of every article using get_corresauthor() function
get_corresauthor <- function(title_link){
  title_page <- read_html(title_link)
  corres_author <- title_page %>% html_nodes("#corresp-c1") %>% html_text() %>% 
    paste(collapse = ",")
  return(corres_author)
}

#Extraction of Published dates of all the articles using get_publisheddate() function
get_publisheddate <- function(title_link){
  title_page <- read_html(title_link)
  publish_date <- title_page %>%
    html_nodes(".c-bibliographic-information__list-item~ .c-bibliographic-information__list-item+ .c-bibliographic-information__list-item time") %>% 
    html_text() %>% paste(collapse = ",")
  return(publish_date)
}

#Extraction of Keywords of all the articles using get_keywords() function
get_keywords <- function(title_link) {
  title_page <- read_html(title_link)
  keywords <- title_page %>% html_nodes(".c-article-subject-list__subject a") %>%
    html_text() %>% paste(collapse = ",")
  return(keywords)
}

#Creating an empty data frame 
Immunity_ageing <- data.frame()

#this for loop is used to traverse through all 12 pages
for(page_result in seq(from = 1, to = 12)){
  
  link <- paste0("https://immunityageing.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=", page_result, "&ref_=adv_nxt")
  page <- read_html(link)

  Title <- page %>% html_nodes(".c-listing__title a") %>% html_text()
  Authors <- page %>% html_nodes(".c-listing__authors-list") %>% html_text()
  
  Title_links <- page %>% html_nodes(".c-listing__title a") %>% html_attr("href") %>%
    paste("https://immunityageing.biomedcentral.com", ., sep = "")

  Abstract <- sapply(Title_links, FUN = get_abstract, USE.NAMES = FALSE)
  correspondence_author <- sapply(Title_links, FUN = get_corresauthor, USE.NAMES = FALSE)
  PublishedDate <- sapply(Title_links, FUN = get_publisheddate, USE.NAMES = FALSE)
  Keywords <- sapply(Title_links, FUN = get_keywords, USE.NAMES = FALSE)

  Immunity_ageing <- rbind(Immunity_ageing, data.frame(Title, Authors, Abstract, correspondence_author, PublishedDate, Keywords, stringsAsFactors = FALSE))

  print(paste("Page:", page_result))
}


# Counting the number of Keywords present in every article
Immunity_ageing$KeyCount <- sapply(strsplit(Immunity_ageing$Keywords, "\\s+"), length)

# Define the bin boundaries
bin_breaks <- seq(0, max(Immunity_ageing$KeyCount) + 1, by = 3)

# Create a new column with bin labels
Immunity_ageing$KeyCountBin <- cut(Immunity_ageing$KeyCount, breaks = bin_breaks, include.lowest = TRUE, right = FALSE)

# Calculate the proportion of abstracts in each bin
bin_proportions <- table(Immunity_ageing$KeyCountBin) / nrow(Immunity_ageing)

# Print the bin proportions
cat("Keyword Count Bin Proportions:\n")
print(bin_proportions)


# Create a bar plot of the proportions
library(ggplot2)
ggplot(Immunity_ageing, aes(x = KeyCountBin)) +
  geom_bar(fill = "red", color = "green") +
  labs(title = "Proportion of Keywords in each bin",
       x = "Keyword count",
       y = "Proportion")

# use the writexl package in order to export your DataFrame to Excel in R:
# install.packages("writexl")
# library("writexl")
write_xlsx(Immunity_ageing,"/Users/aravind/Desktop/Data_Science_Sem1/Immunity&Ageing.xlsx")








