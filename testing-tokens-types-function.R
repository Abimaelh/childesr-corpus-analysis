#load the library
library(childesr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyboot)
library(magrittr)
library(tidytext)
library(data.table)

setwd("abima\\Desktop\\r-testing")

sym_list <- read.csv("C:\\Users\\abima\\Desktop\\sym_list2.csv", header = TRUE)
sym_list

matt_get_types_counts <- get_types(
  collection = "Eng-NA",
  role = "target_child",
  target_child = "Matt",
  age = c(36, 48),
  type = c("same","friends"))

# looping through matt_column_counts to get the counts for the unique stems.
matt_column_counts <- c()
for (i in unique(matt_get_types_counts$gloss)) {
  matt_column_counts[i] <- sum(matt_get_types_counts$count)
}
matt_column_counts

#making a new df with only the gloss and count column from the childes get types df.
#you can use this df later to get other useful info. such as filtering by POS.
newmattdf <- select(matt_get_types_counts, "gloss", "count","target_child_age", "corpus_id", "target_child_id")

#now I can sum counts over the unique stems
matt_column_counts_updated <- newmattdf %>% group_by(gloss) %>%
  summarize(count = sum(count))

#check the df
matt_column_counts_updated

###

matt_get_token_counts <- get_tokens (
  collection = "Eng-NA",
  role = "target_child",
  target_child = "Matt",
  age = c(36,48),
  token = c("same","friends")
  
)

#making a new df with only the gloss and other useful columns, we are saving for later work.
#it may be easier to extract the pos col, which will make it easier to work with.
newtoken_mattdf <- select(matt_get_token_counts, "gloss", "part_of_speech", "stem", "corpus_id", "target_child_id")

#checking if summarize works for this too. It does! 
new_column_tokens_updated <- newtoken_mattdf %>% group_by(gloss) %>%
  summarize(count = sum(unique(length(gloss))))

#check the df
new_column_tokens_updated

#converting the gloss to numbers so we can get a count for the number of times a word appears. Does the same thing as above.
#j <- matt_get_token_counts$gloss
#j <- data.frame(matt_get_token_counts$gloss, stringsAsFactors = FALSE)
#sum(rowSums(data.matrix(j)))

##### Planning #######

# 1. First find the unique ID's associated with the words we're interested in.
#    Add those unique ID's to a new DF, that we can later feed into the target_child argument.

tokens_main_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = sym_list$form
)

length(unique(tokens_main_df$target_child_id)) #Great! 54 unique children just like in the get_types DF.

#lets store these unique ID's in case we need them for later.
tokens_unique_ID <- unique(tokens_main_df$target_child_id)
class(tokens_unique_ID) #checking the class.
#or as a df
tokens_unique_ID_DF <- as.data.frame(tokens_unique_ID)

# 2. Do the same for the get_types function and compare the lengths of each. We should have the same number of unique IDs.

types_main_df <- get_types(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  type = sym_list$form
)

length(unique(types_main_df$target_child_id)) #54

# storing the types unique ids
types_unique_ID <- unique(types_main_df$target_child_id) #storing the unique ids as a variable
types_unique_ID_DF <- as.data.frame(types_unique_ID) # as a df

## how many totally tokens are we hoping to capture?

length(tokens_main_df$gloss) # 1037 tokens to capture.

#confirming that with the counts in get_types()

sum(types_main_df$count) # yup 1037 here too.

# First, get the counts for each stem.

#### Next, we can use dplyr to arrange the data by the unique ids
arranged_ID_main_token_df <- tokens_main_df %>% arrange(target_child_id)

#summing the token gloss 
arranged_gloss_count_sum <- arranged_ID_main_token_df %>% group_by(gloss) %>%
  summarize(count = sum(unique(length(gloss)))) #produces count column

sum(arranged_gloss_count_sum$count) #1037 GOOD

#YES but I want to do this for every target_child_ID
clean_token_df <- select(arranged_ID_main_token_df, 'target_child_id', 'corpus_name', 'target_child_age',
                         'gloss', 'part_of_speech', 'stem')
clean_token_df

#for loop time

#token counts per child for each stem. ## DIDNT WORK
for (i in clean_token_df$target_child_id) {
  output <- clean_token_df %>% group_by(gloss, i) %>%
    summarize(count = sum(unique(length(gloss))))
  
}

sum(output$count)# equals to 1037!

arranged_tokens <- output %>% arrange(target_child_id) #arranging the columns by target_child_id

# now I want to be able to get 0 for the words they don't produce.

#finally subsetted only the data for 1741
seven_teen_forty_one <- subset(clean_token_df, target_child_id == 1741)

#for loop attempt 2
please_work <- c()
for (i in sym_list$form) {
  please_work[i] <- sum(length(unique(seven_teen_forty_one$gloss[i])))
  
}
as.data.frame(please_work)

#again
#checking if summarize works for this too. It does! 
collapsed_token_count <- seven_teen_forty_one %>% group_by(gloss, target_child_id) %>%
  summarize(count = sum(unique(length(gloss))))

sym_words <- sym_list
sym_words <- as.data.frame(sym_words)
names(sym_words)[names(sym_words) == "form"] <- "gloss"
sym_words

collapsed_token_count
#df2 = collapsed_token_count
#df1 = sym_words

df3 <- dplyr::bind_rows(sym_words, collapsed_token_count)
#close but not quite to what I want.


#trying again, it WORKED. MERGE THE COLUMNS AND THEN CHANGE THE NA VALUES

df4 <- merge(collapsed_token_count, sym_words, all = TRUE)
df4$target_child_id[is.na(df4$target_child_id)] <- 1741

#here we go - now we give zeros for all the counts that are NA!
df4$count[is.na(df4$count)] <- 0

#reordering columns
df4_forms <- df4[, c(2, 4, 1, 3)]
df4_forms

#change the gloss back to forms, we needed gloss in order to merge?
names(df4_forms)[names(df4_forms) == "gloss"] <- "forms"
df4_forms

sum(collapsed_token_count$count)
sum(df4_forms$count)
