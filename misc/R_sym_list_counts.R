#load the library
library(childesr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyboot)
library(magrittr)
library(tidytext)

install.packages(c('tibble', 'dplyr', 'readr'))

setwd("~\\Desktop\\")

sym_list <- read.csv("C:\\Users\\abima\\Desktop\\sym_list2.csv", header = TRUE)

head(sym_list)

sym_list['Counts'] <- NA
sym_list


word_token_counts <- c()
for (i in 1:length(sym_list$form)) {
  # frequency of production
  word_counts <- get_types(
    collection = "Eng-NA",
    role = "target_child",
    age = c(36, 48),
    type = sym_list$form[i])
    word_token_counts[i] <- sum(word_counts$count)
  }

word_counts
head(word_token_counts)

#check to make sure the vectors are of the same length.
length(word_token_counts)
length(sym_list$form)

#casually add another column with the counts
sym_list['Test_Counts'] <- word_token_counts

#remove a column
sym_list <- subset(sym_list, select = -Counts)
sym_list

#renaming column name
colnames(sym_list)

#renaming test_counts with counts
names(sym_list)[names(sym_list) == "Test_Counts"] <- "counts"

#check to make sure you renamed it
sym_list

#Sanity check
hug_words <- c("hug", "hugs", "hugging", "hugged")

# frequency of production
hug_counts <- get_types(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  type = hug_words)

#num of times hug,hugging,hugs, appears. No data for hugged. Should be 43.
sum(hug_counts$count)
#sum(sym_list$)

#copying the main df sym_list
collapsed_sym_list <- sym_list
collapsed_sym_list
#attempt to sum over the stem
col_counts <- c()
for (i in unique(collapsed_sym_list$stem)) {
  col_counts[i] <- sum(collapsed_sym_list$counts)
}
col_counts

#ehh testing things
#merge(df, within(aggregate(w ~ x, data=df, sum), sw <- rev(cumsum(rev(w))))[-2], by="x")

#collapsed_sym_list <- sym_list

col_sym_list <- collapsed_sym_list %>% group_by(stem) %>%
                  summarize(counts = sum(counts))

col_sym_list

#get all the tokens! Actually, just get the tokens for each unique child, and then sum all their tokens together.
#*********DONT RUN THIS TAKES REALLY LONG****************************
token_counts <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = '*')

#write.csv(col_sym_list, file = "C:\\Users\\abima\\Desktop\\col_sym_list.csv", row.names = FALSE)

# now we want the unique names of the target children, so we can get tokens based on that.
#full get_types df
full_df <- get_types(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  type = sym_list$form)

unique(full_df$target_child_name)
length(unique(full_df$target_child_name))
length(unique(full_df$target_child_id))

all_children_in_EngNA_df <- get_types(
  collection = "Eng-NA",
  role = "target_child"
)

length(unique(all_children_in_EngNA_df$target_child_id))

unique_target_child_id <- unique(full_df$target_child_id)
unique_target_child_id

forms <- unique(collapsed_sym_list$form)
forms
forms2 <- collapsed_sym_list$form.values.tolist()
class(forms2)
forms2

tokes_per_child <- get_tokens(
  collection = "Eng-NA",
  #target_child = "Matt"
  age = c(36, 48),
  token = forms2$form
)

tokes_for_matt <- get_tokens(
  collection = "Eng-NA",
  target_child = "Matt",
  role = "Target_Child",
  age = c(36, 48),
  token = forms2$form
)


#subsetting particular data in a col
#matt_df <- tokes_per_matt[tokes_for_matt$gloss %in% c("bump", "bumped"), ]

matt_df <- select(tokes_for_matt, 'gloss', 'token_order', 'part_of_speech','corpus_name', 'speaker_name'
,'target_child_id')
matt_df

#clean sym_list with no counts
clean_sym_list <- select(sym_list, 'stem', 'form')
clean_sym_list

     
#AUTOMATE THE PROCESS WITH GET_TOKENS
gettoken_matt_counts <- c()
for (i in 1:length(clean_sym_list$form)) {
  # frequency of production
  matt_token_counts <- get_tokens(
    collection = "Eng-NA",
    role = "Matt",
    #corpus = "Weist",
    age = c(36, 48),
    token = clean_sym_list$form[i])
    gettoken_matt_counts[i] <- sum(matt_token_counts$token_order)
}

gettoken_matt_counts

#trying again with no loop.
for (i in 1:length(forms2$form)) {
matt2_token_counts <- get_tokens(
  collection = "Eng-NA",
  role = "Matt",
  corpus = "Weist",
  age = c(36, 48),
  token = c('bump','friends')
)}

matt2_token_counts

#for some reason the counts come out to 0...Ill try again later.
