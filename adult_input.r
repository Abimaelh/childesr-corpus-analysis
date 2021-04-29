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

getwd()
setwd("C:\\Users\\abima\\Desktop\\corp-an\\adult input")

sym_list <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\sym_list3.csv", header = TRUE)
sym_list$X <- NULL
sym_list$X.1 <- NULL
sym_list$X.2 <- NULL

#save(sym_list, file="sym_list.rdata")
#savehistory(file="adult_input.history")

sym_list_pos <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\sym_list3_pos_no_gerund.csv", header = TRUE)

#Tokens for all roles, we will filter the output later by having the role = MOT | FAT | BRO | SIS
three_year_olds_tokens_ADULT_df <- get_tokens(
  collection = "Eng-NA",
  age = c(36, 48),
  token = sym_list$form
)

Adult_token_three_df <- select(three_year_olds_tokens_ADULT_df, 'target_child_id', 'corpus_name', 'target_child_age',
                               'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id', 'speaker_code', 'speaker_role')

Adult_token_three_df <- Adult_token_three_df %>% filter(speaker_code == 'MOT' | speaker_code == 'FAT')

names(Adult_token_three_df)[names(Adult_token_three_df) == "part_of_speech"] <- "pos"
names(Adult_token_three_df)[names(Adult_token_three_df) == "gloss"] <- "form"

sym_list_pos_paste <- sym_list_pos
sym_list_pos_paste$formpos <- paste(sym_list_pos_paste$form, sym_list_pos_paste$pos)
Adult_token_three_df$formpos <- paste(Adult_token_three_df$form, Adult_token_three_df$pos)

Adult_token_three_df_filtered_pos <- Adult_token_three_df %>%
  filter(formpos %in% sym_list_pos_paste$formpos)

Adult_token_three_df_filtered_pos_verbs <- Adult_token_three_df_filtered_pos %>%
  filter(pos == 'v' | pos == 'part')

adult_ut_ids <- as.data.frame(Adult_token_three_df_filtered_pos$utterance_id)
names(adult_ut_ids)[names(adult_ut_ids) == "Adult_token_three_df_filtered_pos$utterance_id"] <- "utterance_id"


Adult_token_three_df_filtered_pos_verbs$wordstem <- paste(Adult_token_three_df_filtered_pos_verbs$stem, Adult_token_three_df_filtered_pos_verbs$utterance_id)

three_adult_wordstem_only <- select(Adult_token_three_df_filtered_pos_verbs, 'wordstem')

three_adult_wordstem_only_final <- three_adult_wordstem_only %>% separate(wordstem, c("stem", "utterance_id"))

sapply(three_adult_wordstem_only_final, class)

three_adult_wordstem_only_final$utterance_id <- as.integer(as.character(three_adult_wordstem_only_final$utterance_id)) 

sapply(three_adult_wordstem_only_final, class)

three_adult_wordstem_only_final <- three_adult_wordstem_only_final %>% arrange(utterance_id)

length(three_adult_wordstem_only_final$utterance_id) #549

#we may have to use distinct here. im not sure. Yup we have 5 more than we need.
three_adult_wordstem_only_final <- distinct(three_adult_wordstem_only_final, utterance_id, .keep_all = TRUE)

length(three_adult_wordstem_only_final$utterance_id)

three_ut_adult <- get_utterances(
  collection = "Eng-NA",
  age = c(36,48)
)

# filter by speaker code first, MOT and FAT.
three_ut_adult_filtered <- three_ut_adult %>% filter(speaker_code == 'MOT' | speaker_code == 'FAT')
length(three_ut_adult_filtered$id)

# arrange by utterance id, making sure they are numerals.
sapply(three_ut_adult_filtered, class)
three_ut_adult_filtered_final <- three_ut_adult_filtered %>% arrange(id)

#select the columns we are only interested in
three_ut_adult_filtered_final <- select(three_ut_adult_filtered_final, 'id', 'target_child_id', 'gloss', 'stem','type','part_of_speech')

three_ut_adult_filtered_final_mini <- filter(three_ut_adult_filtered_final, id %in% three_adult_wordstem_only_final$utterance_id)

length(three_ut_adult_filtered_final_mini$id) #544
length(three_adult_wordstem_only_final$utterance_id) #544

#add the wordstem only column to the main df!

mini_three_frames_adult_with_wordstem <- cbind(three_ut_adult_filtered_final_mini,three_adult_wordstem_only_final)

age_three_adults <- rep(c(3),times=544)

age_three_adults_final <- as.data.frame(age_three_adults)
names(age_three_adults_final)[names(age_three_adults_final) == "age_three_adults"] <- "child_age"

adult_three_frames <- cbind(mini_three_frames_adult_with_wordstem, age_three_adults_final)

adult_three_frames <- adult_three_frames %>% relocate(child_age, .after = target_child_id)
names(adult_three_frames)[names(adult_three_frames) == "stem.1"] <- "word_stem"

adult_three_frames <- adult_three_frames %>% relocate(word_stem, .after = child_age)
adult_three_frames$utterance_id <- NULL

write.csv(adult_three_frames, "C:\\Users\\abima\\Desktop\\corp-an\\adult input\\adult_three_frames.csv")
