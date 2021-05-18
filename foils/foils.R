#load the library
library(childesr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyboot)
library(magrittr)
library(tidytext)
library(plyr)
library(ddply)
library(scales)

setwd("C:\\Users\\abima\\Desktop\\corp-an\\foils")
getwd()

#importing the foil table
foil_list <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\foils\\foil_list.csv", header = TRUE)

#childesdb search of foils
three_year_olds_tokens_foil_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = foil_list$foil_form
)

#trim the database by selecting the columns we are interested in
three_foils_df_trim <- select(three_year_olds_tokens_foil_df, 'target_child_id', 'corpus_name', 'target_child_age',
                              'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')

#rename columns
names(three_foils_df_trim)[names(three_foils_df_trim) == "part_of_speech"] <- "pos"
names(three_foils_df_trim)[names(three_foils_df_trim) == "gloss"] <- "form"

#filter the df by part (past participle) and v (verb)
three_foils_filtered_df <- three_foils_df_trim %>% filter(pos == 'v' | pos == 'part')

#now we want to know what words don't appear in the db
three_foils_not_in_db <- foil_list %>% filter(!foil_form %in% three_foils_filtered_df$form)
#collapsed by stem, there are 11 foils not produced in the three year old db.
length(unique(three_foils_not_in_db$foil_stem))
#Checking how many three-year-old there are.
length(unique(three_foils_filtered_df$target_child_id)) # 73
#We will need to store these child ids so that we don't count the same child twice in the 4 year old df.
three_foil_id <- as.data.frame(unique(three_foils_filtered_df$target_child_id))
#renaming columns
names(three_foil_id)[names(three_foil_id) == "unique(three_foils_filtered_df$target_child_id)"] <- "target_child_id"
#Let's store the utterance_ids in a data frame. This will be useful when we search the db for utterances. 
three_foil_utterance_id <- as.data.frame(unique(three_foils_filtered_df$utterance_id))
#renaming columns
names(three_foil_utterance_id)[names(three_foil_utterance_id) == "unique(three_foils_filtered_df$utterance_id)"] <- "utterance_id"

#getting 3 year old utterances
three_ut_foils <- get_utterances(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36,48)
)
#making a copy so we don't have to run this again if we mess up.
three_ut_foils2 <- three_ut_foils
#checking how many unique child ids there are before filtering
length(unique(three_ut_foils2$target_child_id)) # 112
#filtering by utterance id
three_ut_filtered_by_ut_id <- filter(three_ut_foils2, id %in% three_foil_utterance_id$utterance_id)
#checking how many child ids there are after filtering by utterance id
length(unique(three_ut_filtered_by_ut_id$target_child_id)) #73

#selecting only a subset of columns we need from three_ut_filtered_by_ut_id
mini_three_foil_ut_filtered_frames <- select(three_ut_filtered_by_ut_id, 'id','target_child_id','gloss','stem','type','part_of_speech', 'target_child_id')

#making it easier to add the foil stem to the frames by adding a column to the token data frame.
three_foils_filtered_df$wordstem <- paste(three_foils_filtered_df$stem, three_foils_filtered_df$utterance_id)
#This will be the df we end up adding to the utterance data frame.
three_foil_wordstem_childid_only <- select(three_foils_filtered_df, 'wordstem', 'target_child_id')
#arranging by child id
three_foil_wordstem_childid_only <- three_foil_wordstem_childid_only %>% arrange(target_child_id) #three_foil_wordstem_childid_only has 1715 rows
#making sure there are not any repeated utterance ids for the same word.
three_foil_wordstem_childid_only_distinct <- distinct(three_foil_wordstem_childid_only, wordstem, .keep_all = TRUE) #after filtering the repeated utterance ids, we not have 1684
#breaking apart the word stem now so we can finally filter by distinct utterance id, some that wasn't captured because the utterance id was next to the foil stem (wordstem)
three_foil_wordstem_final <- three_foil_wordstem_childid_only_distinct %>% separate(wordstem, c("stem", "utterance_id"))
#checking the length, trying to get the length equal to the length of the utterance data frame.
length(three_foil_wordstem_final$utterance_id) #1684
#filtering by unique utterance id, since our foils of interest may appear more than once in the utterance gloss.
three_foil_wordstem_final2 <- distinct(three_foil_wordstem_final, utterance_id, .keep_all = TRUE)
#double checking the length
length(three_foil_wordstem_final2$utterance_id) #1674

#preparing to combine the two data frames. Arrange each by utterance id
mini_three_foil_ut_filtered_frames <- mini_three_foil_ut_filtered_frames %>% arrange(id)
three_foil_wordstem_final2 <- three_foil_wordstem_final2 %>% arrange(utterance_id)

#changing the names to help combine more easily
names(three_foil_wordstem_final2)[names(three_foil_wordstem_final2) == "stem"] <- "foil_stem" 

#changing the utterance id from character or string to integer, so we can arrange it by utterance id
three_foil_wordstem_final2$utterance_id <- as.integer(as.character(three_foil_wordstem_final2$utterance_id)) 
#double checking the change worked
sapply(three_foil_wordstem_final2, class)
#arranging again
three_foil_wordstem_final2 <- three_foil_wordstem_final2 %>% arrange(utterance_id)
#checking the frames class'
sapply(mini_three_foil_ut_filtered_frames, class) #good, its an integer.

#checking one last time that they are the same length.
length(mini_three_foil_ut_filtered_frames$id) #1674
length(three_foil_wordstem_final2$utterance_id) #1674

#combine the frames with the word stem data frame.
mini_three_foil_frames_with_wordstem <- cbind(mini_three_foil_ut_filtered_frames,three_foil_wordstem_final2)
#making a copy we can work worth just in case we ruin something.
mini_three_foil_frames_with_wordstem_cleaned <- mini_three_foil_frames_with_wordstem
