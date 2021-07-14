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

#how many unique foil stems do we have in total?
length(unique(foil_list$foil_stem)) #27

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
#saving
write.csv(three_foils_not_in_db, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\three_foils_not_in_db.csv")

# *** adding in the foil stems that did not appear in the collapsed df ***
#now we want to know what words don't appear in the db BY STEM
three_foils_not_in_db_STEM <- foil_list %>% filter(!foil_stem %in% three_foils_filtered_df$stem)

three_foils_not_in_db_STEM_final <- unique(three_foils_not_in_db_STEM$foil_stem)
three_foils_not_in_db_STEM_final <- as.data.frame(three_foils_not_in_db_STEM_final)
names(three_foils_not_in_db_STEM_final)[names(three_foils_not_in_db_STEM_final) == "three_foils_not_in_db_STEM_final"] <- "foil_stem"
             
#now insert 2 columns tokens and num_chi
three_foils_not_in_db_STEM_final$tokens = 0
three_foils_not_in_db_STEM_final$num_chi = 0
#now cbind these to the collapsed DF!!!

# *** end ***
                        
#how many foils were not found?
length(unique(three_foils_not_in_db$foil_stem)) #12

#collapsed by stem, there are 11 foils not produced in the three year old db.
length(unique(three_foils_not_in_db$foil_stem)) #12 not found
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

#reorganizing columns and changing column names
mini_three_foil_frames_with_wordstem_cleaned$utterance_id <- NULL
mini_three_foil_frames_with_wordstem_cleaned$target_child_id <- NULL
names(mini_three_foil_frames_with_wordstem_cleaned)[names(mini_three_foil_frames_with_wordstem_cleaned) == "id"] <- "utterance_id"
mini_three_foil_frames_with_wordstem_cleaned <- mini_three_foil_frames_with_wordstem_cleaned %>% relocate(target_child_id, .after = utterance_id)
mini_three_foil_frames_with_wordstem_cleaned <- mini_three_foil_frames_with_wordstem_cleaned %>% relocate(foil_stem, .after = target_child_id)

#adding age as integer
age_three <- as.data.frame(rep(c(3),times=1674))
mini_three_foil_frames_with_wordstem_cleaned2 <- cbind(mini_three_foil_frames_with_wordstem_cleaned, age_three)
mini_three_foil_frames_with_wordstem_cleaned2
names(mini_three_foil_frames_with_wordstem_cleaned2)[names(mini_three_foil_frames_with_wordstem_cleaned2) == "rep(c(3), times = 1674)"] <- "Age" 
mini_three_foil_frames_with_wordstem_cleaned2 <- mini_three_foil_frames_with_wordstem_cleaned2 %>% relocate(Age, .after = target_child_id)
#saving
write.csv(mini_three_foil_frames_with_wordstem_cleaned2, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\mini_three_foil_frames_with_wordstem_cleaned2.csv")

#frequency of each foil per child
detach(package:plyr)
three_foil_counts <- mini_three_foil_frames_with_wordstem_cleaned2 %>% group_by(foil_stem,target_child_id) %>%
  summarize(count = sum(unique(length(foil_stem))))
write.csv(three_foil_counts, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\three_foil_counts.csv")

#Collapsed foil_stem frequency and how many children produce it
three_foil_counts_collapsed <- mini_three_foil_frames_with_wordstem_cleaned2 %>% group_by(foil_stem) %>%
  summarize(tokens = sum(unique(length(foil_stem))), num_chi = length(unique(target_child_id)))
write.csv(three_foil_counts_collapsed, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\three_foil_counts_collapsed.csv")

# combining the foil stems that were not found to the collapsed df
three_foil_counts_collapsed_final <- rbind(three_foils_not_in_db_STEM_final, three_foil_counts_collapsed)
write.csv(three_foil_counts_collapsed_final, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\three_foil_counts_collapsed_final.csv")

#how many foil stems were found?
length(unique(three_foil_counts_collapsed$foil_stem)) #22

length(unique(mini_three_foil_frames_with_wordstem_cleaned2$target_child_id)) #73 three year olds

# ************** 4 year old foils *************************************************************************************************************************************************

#childesdb search of foils
four_year_olds_tokens_foil_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48, 60),
  token = foil_list$foil_form
)

#trim the database by selecting the columns we are interested in
four_foils_df_trim <- select(four_year_olds_tokens_foil_df, 'target_child_id', 'corpus_name', 'target_child_age',
                             'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')

#rename columns
names(four_foils_df_trim)[names(four_foils_df_trim) == "part_of_speech"] <- "pos"
names(four_foils_df_trim)[names(four_foils_df_trim) == "gloss"] <- "form"

#filter the df by part (past participle) and v (verb)
four_foils_filtered_df <- four_foils_df_trim %>% filter(pos == 'v' | pos == 'part')

#now we want to know what words don't appear in the db
four_foils_not_in_db <- foil_list %>% filter(!foil_form %in% four_foils_filtered_df$form)
#saving
write.csv(four_foils_not_in_db, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\four_foils_not_in_db.csv")

# *** adding in the foil stems that did not appear in the collapsed df ***
#now we want to know what words don't appear in the db BY STEM
four_foils_not_in_db_STEM <- foil_list %>% filter(!foil_stem %in% four_foils_filtered_df$stem)

four_foils_not_in_db_STEM_final <- unique(four_foils_not_in_db_STEM$foil_stem)
four_foils_not_in_db_STEM_final <- as.data.frame(four_foils_not_in_db_STEM_final)
names(four_foils_not_in_db_STEM_final)[names(four_foils_not_in_db_STEM_final) == "four_foils_not_in_db_STEM_final"] <- "foil_stem"

#now insert 2 columns tokens and num_chi
four_foils_not_in_db_STEM_final$tokens = 0
four_foils_not_in_db_STEM_final$num_chi = 0
#now cbind these to the collapsed DF!!!

# *** end ***


#collapsed by stem, there are 11 foils not produced in the four year old db.
length(unique(four_foils_not_in_db$foil_stem))
#Checking how many four-year-old there are.
length(unique(four_foils_filtered_df$target_child_id)) # 67 - before removing the repeats
#We will need to store these child ids so that we don't count the same child twice in the 4 year old df.
four_foil_id <- as.data.frame(unique(four_foils_filtered_df$target_child_id))

#renaming columns
names(four_foil_id)[names(four_foil_id) == "unique(four_foils_filtered_df$target_child_id)"] <- "target_child_id"
#Let's store the utterance_ids in a data frame. This will be useful when we search the db for utterances. 
four_foil_utterance_id <- as.data.frame(unique(four_foils_filtered_df$utterance_id))
#renaming columns
names(four_foil_utterance_id)[names(four_foil_utterance_id) == "unique(four_foils_filtered_df$utterance_id)"] <- "utterance_id"
#************************************************************************************************************************************
four_repeats <- four_foils_filtered_df %>% filter(target_child_id %in% three_foil_id$target_child_id)
length(unique(four_repeats$target_child_id)) #14


#checking if any 3 year olds are in the four year old df. 11 repeats.
four_full_no_rep <- four_foil_id %>% filter(target_child_id %in% three_foil_id$target_child_id)

#removing the three year olds in the four year old df.
four_foils_filtered_df <- four_foils_filtered_df %>% filter(!target_child_id %in% three_foil_id$target_child_id)
#checking the number of children after removing the repeats
length(unique(four_foils_filtered_df$target_child_id)) #53 so this is right.

#RUNNING THIS AGAIN BECAUSE I REALIZED WE FILTERED AND HAD TO COLLECT NEW UTTERANCE IDS THAT EXCLUDED THE 3 YEAR OLDS IN THIS DF
four_foil_utterance_id <- as.data.frame(unique(four_foils_filtered_df$utterance_id))
names(four_foil_utterance_id)[names(four_foil_utterance_id) == "unique(four_foils_filtered_df$utterance_id)"] <- "utterance_id"

#getting 3 year old utterances
four_ut_foils <- get_utterances(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48,60)
)
#making a copy so we don't have to run this again if we mess up.
four_ut_foils2 <- four_ut_foils
#checking how many unique child ids there are before filtering
length(unique(four_ut_foils2$target_child_id)) # 78
#********************************************************************************************REDOING THIS
#filtering by utterance id
four_ut_filtered_by_ut_id <- filter(four_ut_foils2, id %in% four_foil_utterance_id$utterance_id)
#checking how many child ids there are after filtering by utterance id
length(unique(four_ut_filtered_by_ut_id$target_child_id)) #53 -AH NEED TO GET THE NEW UTTERANCE IDS AFTER FILTERING (FOR 4 YEAR OLDS)

#selecting only a subset of columns we need from four_ut_filtered_by_ut_id
mini_four_foil_ut_filtered_frames <- select(four_ut_filtered_by_ut_id, 'id','target_child_id','gloss','stem','type','part_of_speech', 'target_child_id')

#making it easier to add the foil stem to the frames by adding a column to the token data frame.
four_foils_filtered_df$wordstem <- paste(four_foils_filtered_df$stem, four_foils_filtered_df$utterance_id)
#This will be the df we end up adding to the utterance data frame.
four_foil_wordstem_childid_only <- select(four_foils_filtered_df, 'wordstem', 'target_child_id')
#arranging by child id
four_foil_wordstem_childid_only <- four_foil_wordstem_childid_only %>% arrange(target_child_id) #four_foil_wordstem_childid_only has 1129 rows
#making sure there are not any repeated utterance ids for the same word.
four_foil_wordstem_childid_only_distinct <- distinct(four_foil_wordstem_childid_only, wordstem, .keep_all = TRUE) #after filtering the repeated utterance ids, we not have 1098
#breaking apart the word stem now so we can finally filter by distinct utterance id, some that wasn't captured because the utterance id was next to the foil stem (wordstem)
four_foil_wordstem_final <- four_foil_wordstem_childid_only_distinct %>% separate(wordstem, c("stem", "utterance_id"))
#checking the length, trying to get the length equal to the length of the utterance data frame.
length(four_foil_wordstem_final$utterance_id) #1098
#filtering by unique utterance id, since our foils of interest may appear more than once in the utterance gloss.
four_foil_wordstem_final2 <- distinct(four_foil_wordstem_final, utterance_id, .keep_all = TRUE)
#double checking the length
length(four_foil_wordstem_final2$utterance_id) #1089

#preparing to combine the two data frames. Arrange each by utterance id
mini_four_foil_ut_filtered_frames <- mini_four_foil_ut_filtered_frames %>% arrange(id)
four_foil_wordstem_final2 <- four_foil_wordstem_final2 %>% arrange(utterance_id)

#changing the names to help combine more easily
names(four_foil_wordstem_final2)[names(four_foil_wordstem_final2) == "stem"] <- "foil_stem" 

#changing the utterance id from character or string to integer, so we can arrange it by utterance id
four_foil_wordstem_final2$utterance_id <- as.integer(as.character(four_foil_wordstem_final2$utterance_id)) 
#double checking the change worked
sapply(four_foil_wordstem_final2, class)
#arranging again
four_foil_wordstem_final2 <- four_foil_wordstem_final2 %>% arrange(utterance_id)
#checking the frames class'
sapply(mini_four_foil_ut_filtered_frames, class) #good, its an integer.

#checking one last time that they are the same length.
length(mini_four_foil_ut_filtered_frames$id) #1089
length(four_foil_wordstem_final2$utterance_id) #1089

#combine the frames with the word stem data frame.
mini_four_foil_frames_with_wordstem <- cbind(mini_four_foil_ut_filtered_frames,four_foil_wordstem_final2)
#making a copy we can work worth just in case we ruin something.
mini_four_foil_frames_with_wordstem_cleaned <- mini_four_foil_frames_with_wordstem

#reorganizing columns and changing column names
mini_four_foil_frames_with_wordstem_cleaned$utterance_id <- NULL
mini_four_foil_frames_with_wordstem_cleaned$target_child_id <- NULL
names(mini_four_foil_frames_with_wordstem_cleaned)[names(mini_four_foil_frames_with_wordstem_cleaned) == "id"] <- "utterance_id"
mini_four_foil_frames_with_wordstem_cleaned <- mini_four_foil_frames_with_wordstem_cleaned %>% relocate(target_child_id, .after = utterance_id)
mini_four_foil_frames_with_wordstem_cleaned <- mini_four_foil_frames_with_wordstem_cleaned %>% relocate(foil_stem, .after = target_child_id)

#adding age as integer
age_four <- as.data.frame(rep(c(4),times=1089))
mini_four_foil_frames_with_wordstem_cleaned2 <- cbind(mini_four_foil_frames_with_wordstem_cleaned, age_four)
mini_four_foil_frames_with_wordstem_cleaned2
names(mini_four_foil_frames_with_wordstem_cleaned2)[names(mini_four_foil_frames_with_wordstem_cleaned2) == "rep(c(4), times = 1089)"] <- "Age" 
mini_four_foil_frames_with_wordstem_cleaned2 <- mini_four_foil_frames_with_wordstem_cleaned2 %>% relocate(Age, .after = target_child_id)
#saving
write.csv(mini_four_foil_frames_with_wordstem_cleaned2, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\mini_four_foil_frames_with_wordstem_cleaned2.csv")

#frequency of each foil per child
detach(package:plyr)
four_foil_counts <- mini_four_foil_frames_with_wordstem_cleaned2 %>% group_by(foil_stem,target_child_id) %>%
  summarize(count = sum(unique(length(foil_stem))))
write.csv(four_foil_counts, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\four_foil_counts.csv")

#Collapsed foil_stem frequency and how many children produce it
four_foil_counts_collapsed <- mini_four_foil_frames_with_wordstem_cleaned2 %>% group_by(foil_stem) %>%
  summarize(tokens = sum(unique(length(foil_stem))), num_chi = length(unique(target_child_id)))
write.csv(four_foil_counts_collapsed, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\four_foil_counts_collapsed.csv")

# combining the foil stems that were not found to the collapsed df
four_foil_counts_collapsed_final <- rbind(four_foils_not_in_db_STEM_final, four_foil_counts_collapsed)
write.csv(four_foil_counts_collapsed_final, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\four_foil_counts_collapsed_final.csv")

length(unique(foil_list$foil_stem)) #27
length(unique(four_foil_counts_collapsed_final$foil_stem))

length(unique(mini_four_foil_frames_with_wordstem_cleaned2$target_child_id)) #53 four year olds



# *** combined checks ***

collapsed_check <- four_foil_counts_collapsed_final %>% filter(!foil_stem %in% three_foil_counts_collapsed_final$foil_stem)

bite_only <- mini_four_foil_frames_with_wordstem_cleaned2 %>% filter(foil_stem == 'bite')

four_foil_counts_collapsed_final2 <- four_foil_counts_collapsed_final %>% filter(!foil_stem == 'bit')

#adding 1 to the token counts because the wrong foil stem was used, but the same child produced the correct stem.
four_foil_counts_collapsed_final2[6, 2] = 63
write.csv(four_foil_counts_collapsed_final2, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\four_foil_counts_collapsed_final2.csv")

foil_collapsed_final <- cbind(four_foil_counts_collapsed_final2[1], four_foil_counts_collapsed_final2[, -1] + three_foil_counts_collapsed_final[match(four_foil_counts_collapsed_final2$foil_stem, three_foil_counts_collapsed_final$foil_stem), -1])
write.csv(foil_collapsed_final, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\foil_collapsed_final.csv")
#finding how many children we have. should be 126
#length(unique(four_foil_counts_collapsed_final2$))

savehistory(file = "C:\\Users\\abima\\Desktop\\corp-an\\foils\\foils.Rhistory")

combined_foil_frames <- rbind(mini_four_foil_frames_with_wordstem_cleaned2,mini_three_foil_frames_with_wordstem_cleaned2)
write.csv(combined_foil_frames, "C:\\Users\\abima\\Desktop\\corp-an\\foils\\combined_foil_frames.csv")


#combined child word_stem frequency
three_foil_counts_for_combining <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\foils\\three_foil_counts.csv", header = TRUE)
four_foil_counts_for_combining <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\foils\\four_foil_counts.csv", header = TRUE)
four_foil_counts_for_combining$X <- NULL
three_foil_counts_for_combining$X <- NULL

combined_child_wordstem_counts <- rbind(three_foil_counts_for_combining, four_foil_counts_for_combining)
common_cols <- intersect(colnames(three_foil_counts_for_combining), colnames(four_foil_counts_for_combining))
combined_child_wordstem_counts2 <- rbind(subset(three_foil_counts_for_combining, select = common_cols), 
                 subset(four_foil_counts_for_combining, select = common_cols))

filter_by_wordstem_counts2_FOILS_COMBINED <- combined_child_wordstem_counts2 %>% group_by(foil_stem,target_child_id) %>%
  summarize(count = sum(unique(length(foil_stem))))

filter_by_wordstem_counts4_FOILS_COMBINED <- combined_child_wordstem_counts2 %>% group_by(foil_stem) %>%
  summarize(tokens = sum(unique(length(foil_stem))), num_chi = length(unique(target_child_id)))

#checking if the rbind worked, by checking the individual dataframes
num <- aggregate(count~foil_stem, three_foil_counts_for_combining,sum)
num4<- aggregate(count~foil_stem, four_foil_counts_for_combining,sum)

combined_child_wordstem_counts <-rbind(three_foil_counts_for_combining, four_foil_counts_for_combining)
write.csv(combined_child_wordstem_counts, "C:\\Users\\abima\\Desktop\\corp-an\\wide predicate search\\wordstem_freq_per_child_FOIL.csv")
combined_child_wordstem_counts_collapsed_final <- aggregate(count~foil_stem, combined_child_wordstem_counts,sum)

# *********************************************** F O I L   I N P U T ********************************************************************************
