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

setwd("C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children")
getwd()

#importing the symmetricals table
sym_list <- read.csv(file = "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\resources\\sym_list3.csv", header = TRUE)
sym_list$X <- NULL
sym_list$X.1 <- NULL
sym_list$X.2 <- NULL
sym_list

#how many unique sym stems do we have?
length(unique(sym_list$stem)) #25 unique stems

#childesdb search of symmetricals for 3 year olds.
three_year_olds_tokens_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = sym_list$form
)
#checking how many children came up in the search.
length(unique(three_year_olds_tokens_df$target_child_id))#55

#trim the database by selecting the columns we are interested in
three_year_olds_tokens_df_trimmed <- select(three_year_olds_tokens_df, 'target_child_id', 'corpus_name', 'target_child_age',
                         'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id','target_child_age')

#renaming columns
names(three_year_olds_tokens_df_trimmed)[names(three_year_olds_tokens_df_trimmed) == "part_of_speech"] <- "pos"
names(three_year_olds_tokens_df_trimmed)[names(three_year_olds_tokens_df_trimmed) == "gloss"] <- "form"
three_year_olds_tokens_df_trimmed

#filter the df by part (past participle) and v (verb)
three_sym_filtered_pos_df <- three_year_olds_tokens_df_trimmed %>% filter(pos == 'v' | pos == 'part')
length(three_sym_filtered_pos_df$target_child_id) #362
#now we want to know what words don't appear in the db
three_sym_not_in_db <- sym_list %>% filter(!form %in% three_sym_filtered_pos_df$form)
# ******************** LEFT OFF HERE ***********************
write.csv(three_sym_not_in_db, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\three_sym_not_in_db.csv")

length(three_sym_filtered_pos_df$target_child_id) #362
length(unique(three_sym_filtered_pos_df$target_child_id)) #33

#making df of unique ids in three year olds
ids_for_threes <- as.data.frame(unique(three_sym_filtered_pos_df$target_child_id))
names(ids_for_threes)[names(ids_for_threes) == "unique(three_sym_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_threes$target_child_id)) #33 unique ids

# generate 90 (because our sym_list has 90 rows) instances of an ID (n # of participants x f # of forms )
# we are trying to create a dataframe that has 2,970 rows (33 * 90)
three_many_ids <- ids_for_threes %>% slice(rep(1:n(), each = 90))
length(three_many_ids$target_child_id) #2970

#generate 46 instances of each symmetrical - because we want the ids and sym words to have the same # of rows before
#we merge them together.
n = 33
threes_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(threes_many_syms$form) #2970

# Merge many IDS with many syms
three_sym_and_id <- cbind(threes_many_syms, target_child_id = three_many_ids$target_child_id) #2970
length(three_sym_and_id$target_child_id) #2970

#the number of participants differ from the original code "child-production-of-symmetricals"
#because we searched for nouns and adjectives in that code. Here we focus on verbs. 
#So the total number of children searched between 3 and 4 should be the same.

#replacing age with 3
three_sym_filtered_pos_df$target_child_age <-  replace(three_sym_filtered_pos_df$target_child_age,
                                                                     three_sym_filtered_pos_df$target_child_age >= 36.00 &
                                                                     three_sym_filtered_pos_df$target_child_age <= 47.99, 3)
length(three_sym_filtered_pos_df$target_child_id)
#you can use three_sym_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#saving
save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")

# We are doing this to get a count for the words that are not~ recorded. So that when we merge with
# the words from sym_list all the words not produced, receive an NA, which we later change to 0.
detach(package:plyr)
three_counts <- three_sym_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #349 rows
#checking we still have the same amount of children.
length(unique(three_counts$target_child_id)) #33

#this could be useful later on.
three_counts_pasted_targetid_uttid_stem <- as.data.frame(paste(three_sym_filtered_pos_df$target_child_id, three_sym_filtered_pos_df$stem, three_sym_filtered_pos_df$utterance_id))
names(three_counts_pasted_targetid_uttid_stem)[names(three_counts_pasted_targetid_uttid_stem) == "paste(three_sym_filtered_pos_df$target_child_id, three_sym_filtered_pos_df$stem, three_sym_filtered_pos_df$utterance_id)"] <- "wordstem"
three_counts_pasted_targetid_uttid_stem_separate <- three_counts_pasted_targetid_uttid_stem %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))

#arranging by target_id helps when merging columns.
three_counts <- three_counts %>% arrange(target_child_id)
length(unique(three_counts$target_child_id)) #33

length(three_counts$form) #349, because this only includes counts for words that were produced by the child.Can we use this to plot data for people that have more than one count?
length(three_sym_and_id$form) #2970

three_full <- merge(three_counts, three_sym_and_id, all = TRUE)
three_full <- three_full %>% arrange(target_child_id)
#we dont really need sex, corpus, and utt id here.
three_full$target_child_sex <-NULL
three_full$corpus_name <- NULL
three_full$utterance_id <- NULL

length(unique(three_full$form)) #90
length(unique(three_full$target_child_id)) #33
length(three_full$form) #3172

#now go in and change NAs for age to 3 and NAs for count to 0
three_full$count[is.na(three_full$count)] <- 0
three_full$target_child_age[is.na(three_full$target_child_age)] <- 3
sum(three_full$count) #362 matches with three_sym_filtered_pos_df - this df doesn't count tokens, so we count the rows which are equal to one token.

#we select our top 12 symmetricals here
filter_by_stem <- three_full %>% filter(stem == 'touch' | stem == 'fight' | stem == 'match' | stem == 'kiss' | stem == 'meet' | stem == 'marry' | stem == 'attach' | stem == 'connect' | stem == 'hug'
                                                      | stem == 'join' | stem == 'separate' | stem == 'trade')
length(filter_by_stem$target_child_id) #1760 rows
length(unique(filter_by_stem$target_child_id)) #33

#collapsed by stem + the number of counts for each stem
three_child_sum <- aggregate(filter_by_stem$count, by=list(filter_by_stem$stem), sum)

#every stem and their count for each child.
three_child_all_stems_per_child <- filter_by_stem %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))

#test2 <- filter_by_stem %>% group_by(stem) %>%
  #summarize(num_chi = )


#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

three_child_all_stems_per_child_no_zeros <- three_child_all_stems_per_child

three_child_all_stems_per_child_no_zeros <- three_child_all_stems_per_child_no_zeros %>% filter(tokens != 0)
#eliminating the zeros worked!
verb_pairs_for_three_child_sheet <- three_child_all_stems_per_child_no_zeros %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))

# SENTENCE FRAMES FOR 3 year olds************************************************************************************************************************
three_ut <- get_utterances(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36,48)
)

three_ut_filtered <- filter(three_ut, target_child_id %in% three_full$target_child_id)
length(unique(three_ut_filtered$target_child_id))
length(unique(three_full$target_child_id))

ids_for_threes

three_ut_filtered_sym <- filter(three_ut_filtered, id %in% three2_year_olds_tokens_df_filtered_pos$utterance_id)
length(unique(three_ut_filtered_sym$target_child_id))
mini_three_ut_filtered_frames <- select(three_ut_filtered_sym, 'id','target_child_id','gloss','stem','type','part_of_speech', 'target_child_id')
write.csv(mini_three_ut_filtered_frames, "C:\\Users\\abima\\Desktop\\corp-an\\threes\\mini_three_ut_filtered_frames.csv")

#cleaned mini_three_ut_filtered_frames.*****************************************************************************************************************************
three2_year_olds_tokens_df_filtered_pos$wordstem <- paste(three2_year_olds_tokens_df_filtered_pos$stem, three2_year_olds_tokens_df_filtered_pos$utterance_id)
three2_wordstem_only <- select(three2_year_olds_tokens_df_filtered_pos, 'wordstem', 'target_child_id')

three2_wordstem_only <- three2_wordstem_only %>% arrange(target_child_id)
#now break this column in 2 and then filter by four_ut_ids, and then add it to mini_four_ut_filtered_frames, make sure they are the same length.
#stopped here.
three2_wordstem_only <- filter(three2_wordstem_only, target_child_id %in% three2_full$target_child_id)
length(unique(three2_wordstem_only$target_child_id))
three2_wordstem_only <- three2_wordstem_only %>% arrange(target_child_id)


three2_wordstem_only2 <- distinct(three2_wordstem_only, wordstem, .keep_all = TRUE)

three2_wordstem_only_final <- three2_wordstem_only2 %>% separate(wordstem, c("stem", "utterance_id"))

three_ut_filtered_frames_with_wordstem <- three2_wordstem_only_final %>%
  filter(utterance_id %in% mini_three_ut_filtered_frames$id)
#
three_ut_filtered_frames_with_wordstem <- distinct(three_ut_filtered_frames_with_wordstem, utterance_id, .keep_all = TRUE)

mini_three_ut_filtered_frames <- mini_three_ut_filtered_frames %>% arrange(id)
three_ut_filtered_frames_with_wordstem <- three_ut_filtered_frames_with_wordstem %>% arrange(utterance_id)

names(three_ut_filtered_frames_with_wordstem)[names(three_ut_filtered_frames_with_wordstem) == "stem"] <- "word stem" 

three_ut_filtered_frames_with_wordstem$utterance_id <- as.integer(as.character(three_ut_filtered_frames_with_wordstem$utterance_id)) 
sapply(three_ut_filtered_frames_with_wordstem, class)
three_ut_filtered_frames_with_wordstem <- three_ut_filtered_frames_with_wordstem %>% arrange(utterance_id)
mini_three_ut_filtered_frames <- mini_three_ut_filtered_frames %>% arrange(id)
#you had to make sure what you were arranging by was of integer type since the utterance ids are of integer type in mini_four_ut_filtered_frames
mini_three_frames_with_wordstem <- cbind(mini_three_ut_filtered_frames,three_ut_filtered_frames_with_wordstem)
mini_three_frames_with_wordstem_cleaned <- mini_three_frames_with_wordstem
##
sapply(mini_three_frames_with_wordstem_cleaned, class)
sapply(mini_three_ut_filtered_frames, class)

mini_three_frames_with_wordstem_cleaned$utterance_id <- NULL
mini_three_frames_with_wordstem_cleaned$id_target <- NULL
mini_three_frames_with_wordstem_cleaned$target_child_id <- NULL
names(mini_three_frames_with_wordstem_cleaned)[names(mini_three_frames_with_wordstem_cleaned) == "id"] <- "utterance_id" 

mini_three_frames_with_wordstem_cleaned<- mini_three_frames_with_wordstem_cleaned %>% relocate(target_child_id, .after = utterance_id)
names(mini_three_frames_with_wordstem_cleaned)[names(mini_three_frames_with_wordstem_cleaned) == "word stem"] <- "word_stem" 

mini_three_frames_with_wordstem_cleaned<- mini_three_frames_with_wordstem_cleaned %>% relocate(word_stem, .after = target_child_id)

age_three <- rep(c(3),times=713)

age_three <- as.data.frame(age_three)

mini_three_frames_with_wordstem_cleaned2 <- cbind(mini_three_frames_with_wordstem_cleaned, age_three)
names(mini_three_frames_with_wordstem_cleaned2)[names(mini_three_frames_with_wordstem_cleaned2) == "age_three"] <- "Age" 
mini_three_frames_with_wordstem_cleaned2 <- mini_three_frames_with_wordstem_cleaned2 %>% relocate(Age, .after = target_child_id)

write.csv(mini_three_frames_with_wordstem_cleaned2, "C:\\Users\\abima\\Desktop\\corp-an\\threes\\mini_three_frames_with_wordstem_cleaned2.csv")

# 3 year old input ************************************************************************************************************************************
three_year_olds_input_tokens_df <- get_tokens(
  collection = "Eng-NA",
  age = c(36, 48),
  token = sym_list$form
)

input_three <- get_transcripts (
  collection = "Eng-NA",
  corpus = "VanKleeck",
  target_child = 'Matthew'
)

input_three_utt <- get_utterances (
  collection = 'Eng-NA',
  corpus = 'VanKleeck',
  target_child = 'Matthew',
)

# *****************************************************************************************************************************************************

#now to get a clean token df for 4 year olds only.
four_year_olds_tokens_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48, 60),
  token = sym_list$form
)

length(unique(four_year_olds_tokens_df$target_child_id)) #63 (including some from the 3 year old db)

four_year_olds_tokens_df_trimmed <- select(four_year_olds_tokens_df, 'target_child_id', 'corpus_name', 'target_child_age',
                                    'gloss', 'part_of_speech','target_child_sex','stem','utterance_id','target_child_age')

#renaming columns
names(four_year_olds_tokens_df_trimmed)[names(four_year_olds_tokens_df_trimmed) == "part_of_speech"] <- "pos"
names(four_year_olds_tokens_df_trimmed)[names(four_year_olds_tokens_df_trimmed) == "gloss"] <- "form"

#filter the df by part (past participle) and v (verb)
four_sym_filtered_pos_df <- four_year_olds_tokens_df_trimmed %>% filter(pos == 'v' | pos == 'part')
length(four_sym_filtered_pos_df$target_child_id) #504
#now we want to know what words don't appear in the db
four_sym_not_in_db <- sym_list %>% filter(!form %in% four_sym_filtered_pos_df$form)
write.csv(four_sym_not_in_db, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\four_sym_not_in_db.csv")

length(four_sym_filtered_pos_df$target_child_id) #504
length(unique(four_sym_filtered_pos_df$target_child_id)) #55
# brb #

#making df of unique ids in four year olds
ids_for_fours <- as.data.frame(unique(four_sym_filtered_pos_df$target_child_id))
names(ids_for_fours)[names(ids_for_fours) == "unique(four_sym_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_fours$target_child_id)) #55 unique ids with duplicate children from the 3 year old db

#storing the 3 year olds in the four year old db in repeats.
repeats <- ids_for_fours %>% filter(target_child_id %in% ids_for_threes$target_child_id)
length(unique(repeats$target_child_id)) #14

#removing the 3 year olds from the 4 year old db.
ids_for_fours_no_rep <- ids_for_fours %>% filter(!target_child_id %in% ids_for_threes$target_child_id)
length(unique(ids_for_fours_no_rep$target_child_id)) #41

#checking if they are gone!
repeats %in% ids_for_fours_no_rep$target_child_id

#generate 90 instances of an ID
four_many_ids <- ids_for_fours_no_rep %>% slice(rep(1:n(), each = 90))
length(four_many_ids$target_child_id) #3690 (41 * 90)

#generate 41 (for each unique id) instances of sym_words
n = 41
four_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(four_many_syms$form) #3690

# Merge many IDS with many syms
four_sym_and_id <- cbind(four_many_syms, target_child_id = four_many_ids$target_child_id)
length(four_sym_and_id$target_child_id) #3690

#replacing age with 4

four_sym_filtered_pos_df$target_child_age <-  replace(four_sym_filtered_pos_df$target_child_age,
                                                      four_sym_filtered_pos_df$target_child_age >= 48.00 &
                                                      four_sym_filtered_pos_df$target_child_age < 60.00, 4)

length(four_sym_filtered_pos_df$target_child_id) #504
#you can use four_sym_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#No! Remove the repeats first!

#saving
#save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")

four_sym_filtered_pos_df <- four_sym_filtered_pos_df %>% filter(!target_child_id %in% ids_for_threes$target_child_id)
length(unique(four_sym_filtered_pos_df$target_child_id)) #41
#now you can use it to extract frames.

detach(package:plyr)
four_counts <- four_sym_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #349 rows
#checking we still have the same amount of children.
length(unique(four_counts$target_child_id)) #41

# Left off here # 

four_counts <- four_counts %>% arrange(target_child_id)

length(unique(four_counts$target_child_id)) #60

length(four_counts$target_child_id) #388
length(four_sym_and_id$form)#5400
length(unique(four_sym_and_id$target_child_id)) #60


four_full <- merge(four_counts, four_sym_and_id, all = TRUE) #Original 
length(four_full$target_child_id) #5400

length(unique(four_full$form)) #90
length(unique(four_full$target_child_id)) #60

length(four_full$target_child_id) #5400
four_full <- four_full %>% arrange(target_child_id)

#now go in and change nas for age to 4 and nas for count to 0

four_full$count[is.na(four_full$count)] <- 0
four_full$target_child_age[is.na(four_full$target_child_age)] <- 4
sum(four_full$count) #1167. Matches four_year_olds_tokendf?, ** 

sum(length(four2_year_olds_tokens_df_filtered_pos$form)) #1167

#length(four_full$form == "friend")
#count(four_full[1:100,], vars = "form")

length(unique(four_full$form))
length(unique(three_full$form))
# ***********************************************************************************************************

## **************************************************Dealing with repeats************************************
three_ids <- unique(three_full$target_child_id)
four_ids <- unique(four_full$target_child_id)
three_ids <- as.data.frame(three_ids)
four_ids <- as.data.frame(four_ids)
names(three_ids)[names(three_ids) == "three_ids"] <- "target_child_id"
names(four_ids)[names(four_ids) == "four_ids"] <- "target_child_id"
unique(four_full$target_child_id) == unique(three_full$target_child_id)
repeats <- four_ids %>% filter(target_child_id %in% three_ids$target_child_id)
repeats#14 repeats


length(unique(four_full$target_child_id)) # 5400 obs before removing the 14 children that are repeats.60!
four_full_no_rep <- four_full %>% filter(!target_child_id %in% three_full$target_child_id)



#four_full_no_rep <- four_full[!(four_full$target_child_id == repeats),]
length(unique(four_full_no_rep$target_child_id)) #46
length(unique(three_full$target_child_id)) #46
# this equals 110, but then when you merge them and create 'new' it becomes 96 because of the repeat subjects in
# 3 year olds and 4 year olds.
four2_full <- four_full_no_rep


length(unique(four2_full$target_child_id)) #60
#four2_full <- subset(four_full, target_child_id != repeats2)
#four2_full <- four2_full[! four2_full$target_child_id %in% repeats,]
#length(unique(four2_full$target_child_id)) #46

# collapsing here for 4 year olds.
detach(package:plyr)
four_col_new <- four2_full %>% group_by(target_child_id, stem) %>%
  summarize(counts = sum(count))
length(unique(four_col_new$target_child_id)) #46
length(four_col_new$target_child_id) #1150

write.csv(four_col_new, "C:\\Users\\abima\\Desktop\\corp-an\\fours\\four_col_new.csv")


#speaker stats for four year olds. **********************************************************************************************************************
speaker_stats_four <- get_speaker_statistics(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48, 60),
)

sub_speaker_stats_four <- speaker_stats_four

sub_speaker_stats_four <- select(sub_speaker_stats_four, "target_child_id", "num_tokens")

#to get only tokens for the children in our data. i.e. no repeats
sub_speaker_stats_four2 <- filter(sub_speaker_stats_four, target_child_id %in% four2_full$target_child_id) #four2_full is the clean one with no repeats.

length(unique(sub_speaker_stats_four2$target_child_id)) # 46, so potentially no repeats
#sub_speaker_stats_three2 is df we want to use when we check for repeats. 
ids_for_sub_speaker_stats_four2 <- unique(sub_speaker_stats_four2$target_child_id) # list of unique ids in ids_for_sub_speaker_stats_three2 
ids_for_sub_speaker_stats_four2_df <- as.data.frame(ids_for_sub_speaker_stats_four2)# as a df

ids_for_sub_speaker_stats_four2_df == ids_for_sub_speaker_stats_three2_df #all false. good.

#getting the sum of tokens per child for 4 year olds.
four_speaker_tokens_for_col_new <- aggregate(sub_speaker_stats_four2$num_tokens, by=list(sub_speaker_stats_four2$target_child_id), sum)
length(unique(four_speaker_tokens_for_col_new$Group.1)) #46

#rename the columns for 4 year olds.
names(four_speaker_tokens_for_col_new)[names(four_speaker_tokens_for_col_new) == "x"] <- "tokens" 
names(four_speaker_tokens_for_col_new)[names(four_speaker_tokens_for_col_new) == "Group.1"] <- "target_child_id_2" 

#4 year olds
four_speaker_tokens_for_col_new <- four_speaker_tokens_for_col_new %>% arrange(target_child_id_2)

#4 year olds
four_speaker_tokens_for_col_new_sliced <- four_speaker_tokens_for_col_new %>% slice(rep(1:n(), each = 25)) #n here should be equal to the number of stems
# the length should be equal to col_new.
length(four_speaker_tokens_for_col_new_sliced$tokens) #1150, 46 children x 25 stems. need to create col_new for 3 and 4 year olds.
length(four_col_new$counts) #1150
sum(four_col_new$counts) #661

#4 year olds 
four_collapsed_stem_prop <- cbind(four_col_new, four_speaker_tokens_for_col_new_sliced)

# CAREFUL TO CHANGE INFO TO FOUR YEAR OLD DATA
four_collapsed_stem_prop$target_child_id_2 <- NULL

four_collapsed_stem_prop2 <- transform(four_collapsed_stem_prop, prop = counts / tokens)

#sum for each count per child.
four_child_sum <- aggregate(four_collapsed_stem_prop2$count, by=list(four_collapsed_stem_prop2$target_child_id), sum)

# then, mean for each child.
sum(four_collapsed_stem_prop2$counts) #661

#sum and mean
library(plyr)
four_child_mean <- transform(four_child_sum, mean = x / 661)
names(four_child_mean)[names(four_child_mean) == "x"] <- "sum" 
names(four_child_mean)[names(four_child_mean) == "Group.1"] <- "target_child_id"
#then slice this 25 times for each stem, and bind it to collapsed_stem_prop2, make a new df full_df just in case.
four_child_mean_for_df <- four_child_mean %>% slice(rep(1:n(), each = 25)) #n here should be equal to the number of stems
four_collapsed_stem_prop3 <- cbind(four_collapsed_stem_prop2, four_child_mean_for_df) #collapsed_stem_prop3 has sum and mean added as another column to collapsed_stem_prop2

# whats the total number of tokens? - use speaker_tokens_for_col_new
#sum(three_speaker_tokens_for_col_new$tokens) # 14459725. This is summing all the repeats. # too big.

#for prop mean denominator. Create a new variable from speaker_tokens_for_col_new before slicing 25 times.
four_speaker_tokens <- aggregate(sub_speaker_stats_four2$num_tokens, by=list(sub_speaker_stats_four2$target_child_id), sum)
length(unique(four_speaker_tokens$Group.1)) #46
sum(four_speaker_tokens$x) #341,851

# sum prop column for each child. Which one are we using?
four_prop_mean <- transform(four_collapsed_stem_prop2, prop_mean = prop / 341851) # sum of the tokens in four_speaker_tokens
sum(four_collapsed_stem_prop2$prop)
four_prop_mean2 <- transform(four_collapsed_stem_prop2, prop_mean = prop / 0.09119984) #this number comes from summing the prop of collapsed_stem_prop2 (code above)

write.csv(four_collapsed_stem_prop2, "C:\\Users\\abima\\Desktop\\corp-an\\fours\\four_collapsed_stem_prop2.csv")
write.csv(four_prop_mean, "C:\\Users\\abima\\Desktop\\corp-an\\fours\\four_prop_mean.csv")

four_collapsed_stem_prop2

library(plyr)
four_sumdata <- ddply(four_collapsed_stem_prop2, .(stem), summarise, sumTokens = sum(counts), meanTokens = mean(counts), minTokens = min(counts), maxTokens = max(counts), stdTokens = sd(counts), meanProp = mean(prop))
write.csv(four_sumdata, "C:\\Users\\abima\\Desktop\\corp-an\\fours\\four_sumdata.csv")

four_sumdata2 <- four_sumdata

four_col_stem_prop <- four_collapsed_stem_prop2
level_order <- factor(four_col_stem_prop$stem, level = c("combine", "chat", "compete", 
                                                          "equal", "marry", "match", 
                                                          "meet", "same", "similar",
                                                          "trade", "fight", "separate",
                                                          "differ", "friend",  "connect", "attach", "argue",
                                                          "split", "kiss",
                                                          "hug", "disagree",
                                                          "agree",
                                                          "touch", "join", "bump"))

plot <- ggplot(four_col_stem_prop, aes(x=level_order, y=counts)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom="point", color = "red", size=2) +
  xlab("stem")+
  ylim(0,20)
plot + theme(legend.position = "none")
ggsave("fours_zoomed.png", width = 15)

# for plotting stems with a count greater than 0.
four_more_than_one <- four_collapsed_stem_prop2 %>% filter(counts > 0)
plot <- ggplot(four_more_than_one, aes(x=stem, y=counts)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', color = "red", size = 2) +
  xlab("stem")+
  ylim(0,20)
plot + theme(legend.position = 'none')
ggsave('fours_more_than_one_zoomed.png', width = 15)

#for plotting # of tokens in their corpora
four_child_corpora_tokes <- four_speaker_tokens
names(four_child_corpora_tokes)[names(four_child_corpora_tokes) == "x"] <- "tokens" 
names(four_child_corpora_tokes)[names(four_child_corpora_tokes) == "Group.1"] <- "target_child_id"
four_ids_tokes <- factor(four_child_corpora_tokes$target_child_id)
plot <- ggplot(four_child_corpora_tokes, aes(x=four_ids_tokes, y=tokens)) +
  geom_point() +
  xlab("child ids")+
  ylab('tokens')
plot + theme(legend.position = 'none')
ggsave('four_child_corpora_tokes.png', width = 15)

# SENTENCE FRAMES FOR 4 year olds *************************************************************************************************************************
four_ut <- get_utterances(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48,60)
)

#filter by the target_child_ids in four2_full
four_ut_filtered <- filter(four_ut, target_child_id %in% four2_full$target_child_id)
length(unique(four_ut_filtered$target_child_id))

#then filter by the utterance id
four_ut_filtered_sym <- filter(four_ut_filtered, id %in% four2_year_olds_tokens_df_filtered_pos$utterance_id)
mini_four_ut_filtered_frames <- select(four_ut_filtered_sym, 'id','target_child_id','gloss','stem','type','part_of_speech', 'target_child_id')
write.csv(mini_four_ut_filtered_frames, "C:\\Users\\abima\\Desktop\\corp-an\\fours\\mini_four_ut_filtered_frames.csv")


four2_year_olds_tokens_df_filtered_pos$wordstem <- paste(four2_year_olds_tokens_df_filtered_pos$stem, four2_year_olds_tokens_df_filtered_pos$utterance_id)
four2_wordstem_only <- select(four2_year_olds_tokens_df_filtered_pos, 'wordstem', 'target_child_id')

four2_wordstem_only <- four2_wordstem_only %>% arrange(target_child_id)
#now break this column in 2 and then filter by four_ut_ids, and then add it to mini_four_ut_filtered_frames, make sure they are the same length.
#stopped here.
four2_wordstem_only <- filter(four2_wordstem_only, target_child_id %in% four2_full$target_child_id)
length(unique(four2_wordstem_only$target_child_id))
four2_wordstem_only <- four2_wordstem_only %>% arrange(target_child_id)

#not yet
#four2_wordstem_only_final <- four2_wordstem_only %>% separate(wordstem, c("stem", "utterance_id"))
#length(unique(four2_wordstem_only_final$target_child_id))
four2_wordstem_only2 <- distinct(four2_wordstem_only, wordstem, .keep_all = TRUE)

four2_wordstem_only_final <- four2_wordstem_only2 %>% separate(wordstem, c("stem", "utterance_id"))

four_ut_filtered_frames_with_wordstem <- four2_wordstem_only_final %>%
  filter(utterance_id %in% mini_four_ut_filtered_frames$id)

four_ut_filtered_frames_with_wordstem <- distinct(four_ut_filtered_frames_with_wordstem, utterance_id, .keep_all = TRUE)

mini_four_ut_filtered_frames <- mini_four_ut_filtered_frames %>% arrange(id)
four_ut_filtered_frames_with_wordstem <- four_ut_filtered_frames_with_wordstem %>% arrange(utterance_id)

names(four_ut_filtered_frames_with_wordstem)[names(four_ut_filtered_frames_with_wordstem) == "stem"] <- "word stem" 

four_ut_filtered_frames_with_wordstem$utterance_id <- as.integer(as.character(four_ut_filtered_frames_with_wordstem$utterance_id)) 
sapply(four_ut_filtered_frames_with_wordstem, class)
four_ut_filtered_frames_with_wordstem <- four_ut_filtered_frames_with_wordstem %>% arrange(utterance_id)
mini_four_ut_filtered_frames <- mini_four_ut_filtered_frames %>% arrange(id)
#you had to make sure what you were arranging by was of integer type since the utterance ids are of integer type in mini_four_ut_filtered_frames
mini_four_frames_with_wordstem <- cbind(mini_four_ut_filtered_frames,four_ut_filtered_frames_with_wordstem)
mini_four_frames_with_wordstem_cleaned <- mini_four_frames_with_wordstem
#you'll have to run the bottom code again. I accidentally ran the bottom code when doing this for threes.
mini_four_frames_with_wordstem_cleaned$utterance_id <- NULL
mini_four_frames_with_wordstem_cleaned$id_target <- NULL
mini_four_frames_with_wordstem_cleaned$target_child_id <- NULL
names(mini_four_frames_with_wordstem_cleaned)[names(mini_four_frames_with_wordstem_cleaned) == "id"] <- "utterance_id" 

mini_four_frames_with_wordstem_cleaned<- mini_four_frames_with_wordstem_cleaned %>% relocate(target_child_id, .after = utterance_id)
names(mini_four_frames_with_wordstem_cleaned)[names(mini_four_frames_with_wordstem_cleaned) == "word stem"] <- "word_stem" 

mini_four_frames_with_wordstem_cleaned<- mini_four_frames_with_wordstem_cleaned %>% relocate(word_stem, .after = target_child_id)

age_four <- rep(c(4),times=636)

age_four <- as.data.frame(age_four)

mini_four_frames_with_wordstem_cleaned2 <- cbind(mini_four_frames_with_wordstem_cleaned, age_four)
names(mini_four_frames_with_wordstem_cleaned2)[names(mini_four_frames_with_wordstem_cleaned2) == "age_four"] <- "Age" 
mini_four_frames_with_wordstem_cleaned2 <- mini_four_frames_with_wordstem_cleaned2 %>% relocate(Age, .after = target_child_id)

write.csv(mini_four_frames_with_wordstem_cleaned2, "C:\\Users\\abima\\Desktop\\corp-an\\fours\\mini_four_frames_with_wordstem_cleaned2.csv")

##
#mini_four_frames_with_wordstem <- cbind(mini_four_ut_filtered_frames,four_ut_filtered_frames_with_wordstem)

#sapply(mini_four_frames_with_wordstem, class)
#sapply(mini_four_ut_filtered_frames, class)

#sapply(mini_four_frames_with_wordstem, class) 

#mini_four_frames_with_wordstem$utterance_id <- as.numeric(as.character(mini_four_frames_with_wordstem$utterance_id)) 


#mini_four_frames_with_wordstem$utterance_id <- as.numeric(as.character(mini_four_frames_with_wordstem$utterance_id)) 
#sapply(mini_four_frames_with_wordstem, class) 

#mini_four_frames_with_wordstem <- mini_four_frames_with_wordstem %>% arrange(utterance_id)

#
#mini_four_ut_filtered_frames$id_target <- paste(mini_four_ut_filtered_frames$id, mini_four_ut_filtered_frames$target_child_id)
#four_ut_filtered_frames_with_wordstem$id_target <- paste(four_ut_filtered_frames_with_wordstem$utterance_id, four_ut_filtered_frames_with_wordstem$target_child_id)

#four_frames_unique <- distinct(four_ut_filtered_frames_with_wordstem, id_target, .keep_all = TRUE)
#length(four_frames_unique$id_target)
#length(mini_four_ut_filtered_frames$target_child_id)

#four_frames_unique$id_target <- NULL
#mini_four_frames_with_wordstem <- cbind(mini_four_ut_filtered_frames,four_frames_unique)

#mini_four_frames_with_wordstem <- mini_four_frames_with_wordstem %>% arrange(target_child_id) 

#mini_four_frames_with_wordstem$target_child_id <- NULL

#mini_four_frames_with_wordstem <- mini_four_frames_with_wordstem %>% arrange(target_child_id)


#SOMETHING WENT WRONG HERE THEY DONT LINE UP.
#COMBINED****************************************************************************************************
master_df <- rbind(three_full, four2_full)
length(three_full$form)#4140
length(four2_full$form)#4140
length(unique(three_full$target_child_id))#46
length(unique(four2_full$target_child_id))#46
length(unique(four_full$target_child_id))#60

length(master_df$form)#8280
length(unique(master_df$target_child_id)) #92
#checking to see if they are no more repeats
#thetruth <- four2_full[four2_full$target_child_id %in% three_full$target_child_id,]
four_true<-unique(four2_full$target_child_id)
three_true <-unique(three_full$target_child_id)
four_true == three_true

# collapsing here again for combined
detach(package:plyr)
col_new <- master_df %>% group_by(target_child_id, stem) %>%
  summarize(counts = sum(count))

#length(unique(four2_full$stem)) #25 thank god.
length(unique(col_new$target_child_id)) #92

#length(unique(four2_full$target_child_id)) #46
#length(unique(three_full$target_child_id)) #46
# well continue this tomorrow...col_new is the only one Alon is interested in.
# Add proportions to this, means, and averages for each row.

#write.csv(col_new, "C:\\Users\\abima\\Desktop\\corp-an\\collapsed_stems.csv")
write.csv(col_new, "C:\\Users\\abima\\Desktop\\corp-an\\combined\\col_new.csv")

#combine the speaker stats for 3 and 4 year olds!*********************************************************************************************************
unique(sub_speaker_stats_four2$target_child_id) == unique(sub_speaker_stats_three2$target_child_id)
full_sub_speaker_stats2 <- rbind(sub_speaker_stats_four2, sub_speaker_stats_three2)

length(sub_speaker_stats_four2$target_child_id) #use sub_speaker_stats_four2 if you want to create a df with the sum of tokens per child for 4 year olds
length(sub_speaker_stats_three2$target_child_id)#use sub_speaker_stats_three2 if you want to create a df with the sum of tokens per child for 3 year olds
length(full_sub_speaker_stats2$target_child_id) #we will be using the combined #899

# getting the sum of tokens per child for 3 and 4 year olds
speaker_tokens_for_col_new <- aggregate(full_sub_speaker_stats2$num_tokens, by=list(full_sub_speaker_stats2$target_child_id), sum)
length(unique(speaker_tokens_for_col_new$Group.1)) # 92

#rename the columns for combined.
names(speaker_tokens_for_col_new)[names(speaker_tokens_for_col_new) == "x"] <- "tokens" 
names(speaker_tokens_for_col_new)[names(speaker_tokens_for_col_new) == "Group.1"] <- "target_child_id_2" 

#************************************************************************************************************************************************************
#combined
speaker_tokens_for_col_new <- speaker_tokens_for_col_new %>% arrange(target_child_id_2) #need this to be the same length as col_new

#combined
speaker_tokens_for_col_new_sliced <- speaker_tokens_for_col_new %>% slice(rep(1:n(), each = 25)) #n here should be equal to the number of stems
# the length should be equal to col_new.
length(speaker_tokens_for_col_new_sliced$tokens) #2300, 92 children x 25 stems.
length(col_new$counts) #2300
sum(col_new$counts) #1406

collapsed_stem_prop <- cbind(col_new, speaker_tokens_for_col_new_sliced)

collapsed_stem_prop$target_child_id_2 <- NULL

collapsed_stem_prop2 <- transform(collapsed_stem_prop, prop = counts / tokens)

#sum for each count per child.
child_sum <- aggregate(collapsed_stem_prop2$count, by=list(collapsed_stem_prop2$target_child_id), sum)

# then, mean for each child.
sum(collapsed_stem_prop2$counts) #1406, so you would do n (that child's sum, divided by 1406)

#sum and mean
child_mean <- transform(child_sum, mean = x / 1406)
names(child_mean)[names(child_mean) == "x"] <- "sum" 
names(child_mean)[names(child_mean) == "Group.1"] <- "target_child_id"
#then slice this 25 times for each stem, and bind it to collapsed_stem_prop2, make a new df full_df just in case.
child_mean_for_df <- child_mean %>% slice(rep(1:n(), each = 25)) #n here should be equal to the number of stems
collapsed_stem_prop3 <- cbind(collapsed_stem_prop2, child_mean_for_df) #collapsed_stem_prop3 has sum and mean added as another column to collapsed_stem_prop2

# whats the total number of tokens? - use speaker_tokens_for_col_new
sum(speaker_tokens_for_col_new$tokens) # 920,240

# sum prop column for each child
prop_mean <- transform(collapsed_stem_prop2, prop_mean = prop / 920240)
sum(collapsed_stem_prop2$prop)
prop_mean2 <- transform(collapsed_stem_prop2, prop_mean = prop / 0.16901) #this number comes from summing the prop of collapsed_stem_prop2 (code above)

write.csv(collapsed_stem_prop2, "C:\\Users\\abima\\Desktop\\corp-an\\combined\\collapsed_stem_prop2.csv")
write.csv(prop_mean, "C:\\Users\\abima\\Desktop\\corp-an\\combined\\prop_mean.csv")

library(plyr)
sumdata <- ddply(collapsed_stem_prop2, .(stem), summarise, sumTokens = sum(counts), meanTokens = mean(counts), minTokens = min(counts), maxTokens = max(counts), stdTokens = sd(counts), meanProp = mean(prop))
write.csv(sumdata, "C:\\Users\\abima\\Desktop\\corp-an\\combined\\sumdata.csv")

#sumdata <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\sumdata.csv", header = TRUE)
sumdata2 <- sumdata
#************************************************************************************************************************************************************

#sumdata2$stem <- as.factor(sumdata2$stem)
#sumdata2$sumTokens <- as.numeric(sumdata2$sumTokens)
col_stem_prop <- collapsed_stem_prop2
level_order <- factor(col_stem_prop$stem, level = c("combine", "chat", "compete", 
                 "equal", "marry", "match", 
                 "meet", "same", "similar",
                 "trade", "fight", "separate",
                 "differ", "friend",  "connect", "attach", "argue",
                 "split", "kiss",
                 "hug", "disagree",
                 "agree",
                 "touch", "join", "bump"))

plot <- ggplot(col_stem_prop, aes(x=level_order, y=counts)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom="point", color = "red", size=2) +
  xlab("stem")+
  ylim(0,20)
plot + theme(legend.position = "none")
ggsave("combined_zoomed.png", width = 15)
#************************************************************************************
# for plotting stems with a count greater than 0.
combined_more_than_one <- collapsed_stem_prop2 %>% filter(counts > 0)
plot <- ggplot(combined_more_than_one, aes(x=stem, y=counts)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', color = "red", size = 2) +
  xlab("stem")+
  ylim(0,20)
plot + theme(legend.position = 'none')
ggsave('combined_more_than_one_zoomed.png', width = 15)

#for plotting # of tokens in their corpora
combined_child_corpora_tokes <- speaker_tokens_for_col_new
#names(combined_child_corpora_tokes)[names(combined_child_corpora_tokes) == "x"] <- "tokens" 
names(combined_child_corpora_tokes)[names(combined_child_corpora_tokes) == "target_child_id_2"] <- "target_child_id"
combined_ids_tokes <- factor(combined_child_corpora_tokes$target_child_id)
plot <- ggplot(combined_child_corpora_tokes, aes(x=combined_ids_tokes, y=tokens)) +
  geom_point() +
  xlab("child ids")+
  ylab('tokens')
plot + theme(legend.position = 'none')
ggsave('combined_child_corpora_tokes.png', width = 15)

#end
totalstem <- aggregate(counts~stem,collapsed_stem_prop2,sum)
three_total_stem <- aggregate(counts~stem,three_collapsed_stem_prop2,sum)
four_total_stem <- aggregate(counts~stem,four_collapsed_stem_prop2,sum)

three_totalid <- aggregate(counts~target_child_id, three_collapsed_stem_prop2, sum)
four_totalid <- aggregate(counts~target_child_id, four_collapsed_stem_prop2, sum)

write.csv(three_totalid, "C:\\Users\\abima\\Desktop\\corp-an\\threes\\three_totalid.csv")
write.csv(four_totalid, "C:\\Users\\abima\\Desktop\\corp-an\\fours\\four_totalid.csv")

#************************************************************************************************************************************
# Combined frames
combined_frames <- rbind(mini_three_frames_with_wordstem_cleaned2, mini_four_frames_with_wordstem_cleaned2)
write.csv(combined_frames, "C:\\Users\\abima\\Desktop\\corp-an\\combined\\combined_frames.csv")

combined_frames2 <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\combined\\combined_frames.csv", header = TRUE)

childesframes34 <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\child2.csv", header = TRUE)
length(childesframes34$target_child_id)
childframes34_2 <- childesframes34 %>% filter(pos == 'adj' | pos == 'n')
#length(childesframes34$word_stem)
#unique(childframes34_2$word_stem)
length(childframes34_2$target_child_id)
child_subset <- childesframes34 %>% filter(pos == 'v')
length(child_subset$target_child_id)
write.csv(child_subset, "C:\\Users\\abima\\Desktop\\corp-an\\child_subset.csv")

# child import FREQ AND NUMBER OF CHILDREN PRODUCED ************************************************************************************************************************
child_subset_import <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\child_subset.csv", header = TRUE)
child_subset_import$X.1 <- NULL
child_subset_import$X <- NULL

length(unique(child_subset_import$target_child_id))
child_subset_import_three <- child_subset_import %>% filter(Age == '3')
child_subset_import_four <- child_subset_import %>% filter(Age == '4')
length(unique(child_subset_import_three$target_child_id)) #33 three-year-olds
length(unique(child_subset_import_four$target_child_id)) #41 four-year-olds
length(unique(child_subset_import$target_child_id)) #74 total children
overlap_ids <- child_subset_import_three$target_child_id %in% child_subset_import_four$target_child_id
overlap_ids

filter_by_wordstem2 <- child_subset_import %>% filter(word_stem == 'touch' | word_stem == 'fight' | word_stem == 'match' | word_stem == 'kiss' | word_stem == 'meet' | word_stem == 'marry' | word_stem == 'attach' | word_stem == 'connect' | word_stem == 'hug'
                                                        | word_stem == 'join' | word_stem == 'separate' | word_stem == 'trade')
length(unique(filter_by_wordstem2$target_child_id)) #71
length(unique(filter_by_wordstem2$word_stem))
#detach(package:plyr) #not sure this one is correct.
#filter_by_wordstem_counts <- filter_by_wordstem %>% group_by(target_child_id) %>%
  #summarize(count = sum(unique(length(word_stem))))

#child word_stem frequency
detach(package:plyr) 
filter_by_wordstem_counts2 <- filter_by_wordstem2 %>% group_by(word_stem,target_child_id) %>%
  summarize(count = sum(unique(length(word_stem))))
write.csv(filter_by_wordstem_counts2, "C:\\Users\\abima\\Desktop\\corp-an\\wide predicate search\\wordstem_freq_per_child.csv")

length(unique(filter_by_wordstem_counts2$word_stem))
unique(filter_by_wordstem_counts2$word_stem)
#filter_by_wordstem_counts3 <- filter_by_wordstem2 %>% group_by(word_stem) %>%
  #summarize(count = sum(unique(length(word_stem))))

#wordstem frequency and how many child produce it
filter_by_wordstem_counts4 <- filter_by_wordstem2 %>% group_by(word_stem) %>%
  summarize(tokens = sum(unique(length(word_stem))), num_chi = length(unique(target_child_id)))
write.csv(filter_by_wordstem_counts4, "C:\\Users\\abima\\Desktop\\corp-an\\wide predicate search\\wordstem_freq_and_num_of_child_prod.csv")


length(unique(filter_by_wordstem_counts$target_child_id))

p<-ggplot(data=filter_by_wordstem_counts2, aes(x=target_child_id, y=count,)) +
  geom_bar(stat="identity", width = 0.5, position = "dodge") + facet_grid(.~word_stem)
p
