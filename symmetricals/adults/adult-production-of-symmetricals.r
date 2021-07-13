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

#** Skip for now **#
#sym_list_pos_paste <- sym_list_pos
#sym_list_pos_paste$formpos <- paste(sym_list_pos_paste$form, sym_list_pos_paste$pos)
#Adult_token_three_df$formpos <- paste(Adult_token_three_df$form, Adult_token_three_df$pos)
#** **#

Adult_token_three_df_filtered_pos_verbs <- Adult_token_three_df %>%
  filter(pos == 'v' | pos == 'part')
#Adult_token_three_df_filtered_pos <- Adult_token_three_df %>%
  #filter(formpos %in% sym_list_pos_paste$formpos)

#Adult_token_three_df_filtered_pos_verbs <- Adult_token_three_df_filtered_pos %>%
  #filter(pos == 'v' | pos == 'part')

adult_ut_ids <- as.data.frame(Adult_token_three_df_filtered_pos_verbs$utterance_id)
names(adult_ut_ids)[names(adult_ut_ids) == "Adult_token_three_df_filtered_pos_verbs$utterance_id"] <- "utterance_id"


Adult_token_three_df_filtered_pos_verbs$wordstem <- paste(Adult_token_three_df_filtered_pos_verbs$stem, Adult_token_three_df_filtered_pos_verbs$utterance_id)

#make another column with speaker id, utterance id, and stem
Adult_token_three_df_filtered_pos_verbs$with_id <- paste(Adult_token_three_df_filtered_pos_verbs$target_child_id,Adult_token_three_df_filtered_pos_verbs$utterance_id,Adult_token_three_df_filtered_pos_verbs$stem)

three_adult_wordstem_only <- select(Adult_token_three_df_filtered_pos_verbs, 'wordstem')

three_adult_wordstem_only_final <- three_adult_wordstem_only %>% separate(wordstem, c("stem2", "utterance_id"))
sapply(three_adult_wordstem_only_final, class)
three_adult_wordstem_only_final$utterance_id <- as.integer(as.character(three_adult_wordstem_only_final$utterance_id)) 
sapply(three_adult_wordstem_only_final, class)
three_adult_wordstem_only_final <- three_adult_wordstem_only_final %>% arrange(utterance_id)

length(three_adult_wordstem_only_final$utterance_id) #549
length(unique(three_adult_wordstem_only_final$utterance_id)) #544


three_adult_wordstem_with_target_id <- select(Adult_token_three_df_filtered_pos_verbs, 'with_id')
three_adult_wordstem_with_target_id_final <- three_adult_wordstem_with_target_id %>% separate(with_id, c("target_id", "utterance_id", "sym_stem"))
sapply(three_adult_wordstem_with_target_id_final, class)
three_adult_wordstem_with_target_id_final$utterance_id <- as.integer(three_adult_wordstem_with_target_id_final$utterance_id)
sapply(three_adult_wordstem_with_target_id_final, class)
three_adult_wordstem_with_target_id_final$target_id <- as.integer(three_adult_wordstem_with_target_id_final$target_id)
sapply(three_adult_wordstem_with_target_id_final, class)
three_adult_wordstem_with_target_id_final <- three_adult_wordstem_with_target_id_final %>% arrange(utterance_id)

three_adult_wordstem_only_final_distinct <- distinct(three_adult_wordstem_only_final, utterance_id, .keep_all = TRUE)

length(three_adult_wordstem_only_final_distinct$utterance_id) #544
length(three_adult_wordstem_only_final$utterance_id) #549

sep_verbs <- three_adult_wordstem_only_final$utterance_id[duplicated(three_adult_wordstem_only_final$utterance_id)] #only pulling out the list of verbs that have duplicates.

sep_verbs <- as.data.frame(sep_verbs)
length(sep_verbs$sep_verbs) # 5
#this gives me all the duplicate verbs including the ones I already have in the dataframe
sep_stem_and_ut_id <- filter(three_adult_wordstem_only_final, utterance_id %in% sep_verbs$sep_verbs)
length(sep_stem_and_ut_id) # 2
# utterance id, stem, and target_id or target_child_id
sep_stem_and_ut_id_target_id <- filter(three_adult_wordstem_with_target_id_final, utterance_id %in% sep_verbs$sep_verbs)
sep_stem_and_ut_id_target_id <- sep_stem_and_ut_id_target_id %>% arrange(utterance_id)
length(sep_stem_and_ut_id_target_id$target_id) #10

write.csv(sep_stem_and_ut_id_target_id,"C:\\Users\\abima\\Desktop\\corp-an\\adult input\\sep_stem_and_ut_id_target_id_three_syms.csv")

length(unique(sep_stem_and_ut_id$utterance_id)) #5
length(sep_stem_and_ut_id$utterance_id) #10

length(three_adult_wordstem_only_final$utterance_id) #549
length(three_adult_wordstem_only_final_distinct$utterance_id) #544

sep_stem_and_ut_id$utterance_id <- as.factor(sep_stem_and_ut_id$utterance_id) #in order to count how many times an utterance id appears. Instead of just summing the utterance id numbers.
aggregate(utterance_id~utterance_id, data = sep_stem_and_ut_id, count)
summary(sep_stem_and_ut_id, maxsum = 500)
as.data.frame(table(sep_stem_and_ut_id$utterance_id))

# *** Utterances *** #
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

#Adult input for 4 year old children **************************************************************************************************************************************************************

#Tokens for all roles, we will filter the output later by having the role = MOT | FAT | BRO | SIS
four_year_olds_tokens_ADULT_df <- get_tokens(
  collection = "Eng-NA",
  age = c(48, 60),
  token = sym_list$form
)

Adult_token_four_df <- select(four_year_olds_tokens_ADULT_df, 'target_child_id', 'corpus_name', 'target_child_age',
                               'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id', 'speaker_code', 'speaker_role')

Adult_token_four_df <- Adult_token_four_df %>% filter(speaker_code == 'MOT' | speaker_code == 'FAT')

names(Adult_token_four_df)[names(Adult_token_four_df) == "part_of_speech"] <- "pos"
names(Adult_token_four_df)[names(Adult_token_four_df) == "gloss"] <- "form"

#** Skip for now **#
#sym_list_pos_paste <- sym_list_pos
#sym_list_pos_paste$formpos <- paste(sym_list_pos_paste$form, sym_list_pos_paste$pos)
#Adult_token_four_df$formpos <- paste(Adult_token_four_df$form, Adult_token_four_df$pos)
#** **#

#Adult_token_four_df_filtered_pos <- Adult_token_four_df %>%
  #filter(formpos %in% sym_list_pos_paste$formpos)

Adult_token_four_df_filtered_pos_verbs <- Adult_token_four_df %>%
  filter(pos == 'v' | pos == 'part')

adult_ut_ids <- as.data.frame(Adult_token_four_df_filtered_pos_verbs$utterance_id)
names(adult_ut_ids)[names(adult_ut_ids) == "Adult_token_four_df_filtered_pos_verbs$utterance_id"] <- "utterance_id"

Adult_token_four_df_filtered_pos_verbs$wordstem <- paste(Adult_token_four_df_filtered_pos_verbs$stem, Adult_token_four_df_filtered_pos_verbs$utterance_id)

#make another column with speaker id, utterance id, and stem
Adult_token_four_df_filtered_pos_verbs$with_id <- paste(Adult_token_four_df_filtered_pos_verbs$target_child_id,Adult_token_four_df_filtered_pos_verbs$utterance_id,Adult_token_four_df_filtered_pos_verbs$stem)

four_adult_wordstem_only <- select(Adult_token_four_df_filtered_pos_verbs, 'wordstem')

four_adult_wordstem_only_final <- four_adult_wordstem_only %>% separate(wordstem, c("stem2", "utterance_id"))
sapply(four_adult_wordstem_only_final, class)
four_adult_wordstem_only_final$utterance_id <- as.integer(as.character(four_adult_wordstem_only_final$utterance_id)) 
sapply(four_adult_wordstem_only_final, class)
four_adult_wordstem_only_final <- four_adult_wordstem_only_final %>% arrange(utterance_id)

length(four_adult_wordstem_only_final$utterance_id) #645
length(unique(four_adult_wordstem_only_final$utterance_id)) #632


four_adult_wordstem_with_target_id <- select(Adult_token_four_df_filtered_pos_verbs, 'with_id')
four_adult_wordstem_with_target_id_final <- four_adult_wordstem_with_target_id %>% separate(with_id, c("target_id", "utterance_id", "sym_stem"))
sapply(four_adult_wordstem_with_target_id_final, class)
four_adult_wordstem_with_target_id_final$utterance_id <- as.integer(four_adult_wordstem_with_target_id_final$utterance_id)
sapply(four_adult_wordstem_with_target_id_final, class)
four_adult_wordstem_with_target_id_final$target_id <- as.integer(four_adult_wordstem_with_target_id_final$target_id)
sapply(four_adult_wordstem_with_target_id_final, class)
four_adult_wordstem_with_target_id_final <- four_adult_wordstem_with_target_id_final %>% arrange(utterance_id)

four_adult_wordstem_only_final_distinct <- distinct(four_adult_wordstem_only_final, utterance_id, .keep_all = TRUE)

length(four_adult_wordstem_only_final_distinct$utterance_id) #632
length(four_adult_wordstem_only_final$utterance_id) #645

sep_verbs <- four_adult_wordstem_only_final$utterance_id[duplicated(four_adult_wordstem_only_final$utterance_id)] #only pulling out the list of verbs that have duplicates.

sep_verbs <- as.data.frame(sep_verbs)
length(sep_verbs$sep_verbs) # 13
#this gives me all the duplicate verbs including the ones I already have in the dataframe
sep_stem_and_ut_id <- filter(four_adult_wordstem_only_final, utterance_id %in% sep_verbs$sep_verbs)
length(sep_stem_and_ut_id) # 2
# utterance id, stem, and target_id or target_child_id
sep_stem_and_ut_id_target_id <- filter(four_adult_wordstem_with_target_id_final, utterance_id %in% sep_verbs$sep_verbs)
sep_stem_and_ut_id_target_id <- sep_stem_and_ut_id_target_id %>% arrange(utterance_id)
length(sep_stem_and_ut_id_target_id$target_id) #23

write.csv(sep_stem_and_ut_id_target_id,"C:\\Users\\abima\\Desktop\\corp-an\\adult input\\sep_stem_and_ut_id_target_id_four_syms.csv")

length(unique(sep_stem_and_ut_id$utterance_id)) #10
length(sep_stem_and_ut_id$utterance_id) #23

length(four_adult_wordstem_only_final$utterance_id) #549
length(four_adult_wordstem_only_final_distinct$utterance_id) #632

sep_stem_and_ut_id$utterance_id <- as.factor(sep_stem_and_ut_id$utterance_id) #in order to count how many times an utterance id appears. Instead of just summing the utterance id numbers.
aggregate(utterance_id~utterance_id, data = sep_stem_and_ut_id, count)
summary(sep_stem_and_ut_id, maxsum = 500)
as.data.frame(table(sep_stem_and_ut_id$utterance_id))

#**
four_ut_adult <- get_utterances(
  collection = "Eng-NA",
  age = c(48,60)
)

# filter by speaker code first, MOT and FAT.
four_ut_adult_filtered <- four_ut_adult %>% filter(speaker_code == 'MOT' | speaker_code == 'FAT')
length(four_ut_adult_filtered$id)

# arrange by utterance id, making sure they are numerals.
sapply(four_ut_adult_filtered, class)
four_ut_adult_filtered_final <- four_ut_adult_filtered %>% arrange(id)

#select the columns we are only interested in
four_ut_adult_filtered_final <- select(four_ut_adult_filtered_final, 'id', 'target_child_id', 'gloss', 'stem','type','part_of_speech')

four_ut_adult_filtered_final_mini <- filter(four_ut_adult_filtered_final, id %in% four_adult_wordstem_only_final$utterance_id)

length(four_ut_adult_filtered_final_mini$id) #631
length(four_adult_wordstem_only_final$utterance_id) #631

#add the wordstem only column to the main df!

mini_four_frames_adult_with_wordstem <- cbind(four_ut_adult_filtered_final_mini,four_adult_wordstem_only_final)

age_four_adults <- rep(c(4),times=631)

age_four_adults_final <- as.data.frame(age_four_adults)
names(age_four_adults_final)[names(age_four_adults_final) == "age_four_adults"] <- "child_age"

adult_four_frames <- cbind(mini_four_frames_adult_with_wordstem, age_four_adults_final)

adult_four_frames <- adult_four_frames %>% relocate(child_age, .after = target_child_id)
names(adult_four_frames)[names(adult_four_frames) == "stem.1"] <- "word_stem"

adult_four_frames <- adult_four_frames %>% relocate(word_stem, .after = child_age)
adult_four_frames$utterance_id <- NULL

write.csv(adult_four_frames, "C:\\Users\\abima\\Desktop\\corp-an\\adult input\\adult_four_frames.csv")

# Combining the 3 year old and 4 year old adult input dataframes.********************************************************************************************************************

combined_adult_input_frames <- rbind(adult_three_frames,adult_four_frames) #1175 rows 631 + 544
write.csv(combined_adult_input_frames, "C:\\Users\\abima\\Desktop\\corp-an\\adult input\\combined_adult_input_frames.csv")

#save(sym_list, file="sym_list.rdata")

savehistory(file="adult_input.history")
