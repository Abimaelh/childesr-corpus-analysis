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
length(unique(three_counts_pasted_targetid_uttid_stem_separate$target_child_id)) #33
three_counts_pasted_targetid_uttid_stem_separate <- three_counts_pasted_targetid_uttid_stem_separate %>% filter(stem == 'touch' | stem == 'fight' | stem == 'match' | stem == 'kiss' | stem == 'meet' | stem == 'marry' | stem == 'attach' | stem == 'connect' | stem == 'hug'
                                                                                                              | stem == 'join' | stem == 'separate' | stem == 'trade')
length(unique(three_counts_pasted_targetid_uttid_stem_separate$target_child_id)) #31
length(three_counts_pasted_targetid_uttid_stem_separate$target_child_id) #310 good to use now.

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
length(unique(three_child_all_stems_per_child_no_zeros$target_child_id))
#eliminating the zeros worked!
verb_pairs_for_three_child_sheet <- three_child_all_stems_per_child_no_zeros %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
length(unique(three_child_all_stems_per_child_no_zeros$target_child_id)) #31


#Extracting corpus information so we can exclude atypical children
exclusion_three_info <- three_year_olds_tokens_df_trimmed %>% filter(utterance_id %in% three_counts_pasted_targetid_uttid_stem_separate$utterance_id) 
length(unique(exclusion_three_info$stem))
unique(exclusion_three_info$stem) #nouns and empty stems
exclusion_three_info_final <- exclusion_three_info %>% filter(stem == 'touch' | stem == 'fight' | stem == 'match' | stem == 'kiss' | stem == 'meet' | stem == 'marry' | stem == 'attach' | stem == 'connect' | stem == 'hug'
                                                            | stem == 'join' | stem == 'separate' | stem == 'trade')
length(unique(exclusion_three_info_final$stem)) #12
#good this is the one we need to pull frames!
length(exclusion_three_info_final$target_child_id) #314
length(unique(exclusion_three_info_final$target_child_id)) #31

exclusion_three_info_final$target_child_id %in% three_child_all_stems_per_child_no_zeros$target_child_id
exclusion_three_info_final <- exclusion_three_info_final %>% arrange(target_child_id)
exclusion_three_info_final <- exclusion_three_info_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_three_info_final$target_child_id) #310 should be the same when we add the sum of counts for no zeros three.
sum(three_child_all_stems_per_child_no_zeros$tokens) #310. Yup they match.
length(unique(exclusion_three_info_final$target_child_id))#31

#write.csv(exclusion_three_info_final, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\exclusion_three_info_final.csv")
#check the corpra in this df
unique(exclusion_three_info_final$corpus_name)

#tokens per corpus
corpra_tokens_three <- exclusion_three_info_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_three,"C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\tokens-from-each-corpus-three.csv")

#number of children in each corpus
num_of_children_in_each_corpus_three <- exclusion_three_info_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_three, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\num-of-children-in-each-corpus-three.csv")

#checking one participant
get_participants(
  collection = "Eng-NA",
  corpus = "Demetras1",
  age = 39
)

check_prov <- get_participants(
  collection = "Eng-NA",
  corpus = "Providence",
)

unique(exclusion_three_info_final$target_child_id %in% exclusion_three_info_final$corpus_name == 'Providence')
# SENTENCE FRAMES FOR 3 year olds************************************************************************************************************************
three_ut <- get_utterances(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36,48)
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

#generate 41 (for each unique id) instances of sym_words #should be 40 but i think this (the extra rows) gets filtered out later. 
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
#this could be useful later on.
four_counts_pasted_targetid_uttid_stem <- as.data.frame(paste(four_sym_filtered_pos_df$target_child_id, four_sym_filtered_pos_df$stem, four_sym_filtered_pos_df$utterance_id))
names(four_counts_pasted_targetid_uttid_stem)[names(four_counts_pasted_targetid_uttid_stem) == "paste(four_sym_filtered_pos_df$target_child_id, four_sym_filtered_pos_df$stem, four_sym_filtered_pos_df$utterance_id)"] <- "wordstem"
four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem_separate %>% filter(utterance_id != 704599)
four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem_separate %>% filter(target_child_id != 2591)
four_counts_pasted_targetid_uttid_stem_separate <- four_counts_pasted_targetid_uttid_stem_separate %>% filter(stem == 'touch' | stem == 'fight' | stem == 'match' | stem == 'kiss' | stem == 'meet' | stem == 'marry' | stem == 'attach' | stem == 'connect' | stem == 'hug'
                                                                                                              | stem == 'join' | stem == 'separate' | stem == 'trade')
#USE THIS TO GET THE FRAMES! AND FIND DUPLICATES!
length(unique(four_counts_pasted_targetid_uttid_stem_separate2$target_child_id))
length(four_counts_pasted_targetid_uttid_stem_separate$target_child_id) #289 (THIS SHOULD ADD UP TO THE LENGTH OF EXCLUSION FOR FINAL AND THE SUM OF NO ZEROS DF) it doesn't because this includes words
#not in the top 12. OKAY FIXED
length(unique(four_counts_pasted_targetid_uttid_stem_separate2$target_child_id))
length(four_counts_pasted_targetid_uttid_stem_separate$target_child_id)

#arranging by target_id helps when merging columns.
four_counts <- four_counts %>% arrange(target_child_id)
length(unique(four_counts$target_child_id)) #41

length(four_counts$form) #280
length(four_sym_and_id$form)#3690

#its okay for the rows/columns here to not be equal
four_full <- merge(four_counts, four_sym_and_id, all = TRUE) #Original 
length(four_full$target_child_id) #3805

length(unique(four_full$form)) #91
#deals with the random "thought" that was inserted.
four_full<- four_full %>% filter(form %in% sym_list$form)
length(unique(four_full$form))
length(unique(four_full$target_child_id)) #41

four_full$target_child_sex <-NULL
four_full$corpus_name <- NULL
four_full$utterance_id <- NULL

length(unique(four_full$form)) #90
length(unique(four_full$target_child_id)) #41
length(four_full$form) #3804

#now go in and change nas for age to 4 and nas for count to 0

four_full$count[is.na(four_full$count)] <- 0
four_full$target_child_age[is.na(four_full$target_child_age)] <- 4
sum(four_full$count) #290. Matches four_year_olds_tokendf?, ** 
#taking out think here
four_sym_filtered_pos_df <- four_sym_filtered_pos_df %>% filter(stem != 'think')

#we select our top 12 symmetricals here
filter_by_stem_four <- four_full %>% filter(stem == 'touch' | stem == 'fight' | stem == 'match' | stem == 'kiss' | stem == 'meet' | stem == 'marry' | stem == 'attach' | stem == 'connect' | stem == 'hug'
                                        | stem == 'join' | stem == 'separate' | stem == 'trade')
length(filter_by_stem_four$target_child_id) #2076
length(unique(filter_by_stem_four$target_child_id)) #41
# need to add another constraint. 
# the count must be greater than 0 to be in filter_by_stem_four

#collapsed by stem + the number of counts for each stem
four_child_sum <- aggregate(filter_by_stem_four$count, by=list(filter_by_stem_four$stem), sum)

#every stem and their count for each child.
four_child_all_stems_per_child <- filter_by_stem_four %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))

#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )


#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

four_child_all_stems_per_child_no_zeros <- four_child_all_stems_per_child
#this is key!
four_child_all_stems_per_child_no_zeros <- four_child_all_stems_per_child_no_zeros %>% filter(tokens != 0)
four_child_all_stems_per_child_no_zeros <- four_child_all_stems_per_child_no_zeros %>% arrange(target_child_id)
write.csv(four_child_all_stems_per_child_no_zeros, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\four_child_all_stems_per_child_no_zeros.csv")
length(unique(four_child_all_stems_per_child_no_zeros$target_child_id)) #total children represented
sum(four_child_all_stems_per_child_no_zeros$tokens) #266
#eliminating the zeros worked!
verb_pairs_for_four_child_sheet <- four_child_all_stems_per_child_no_zeros %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
length(unique(four_child_all_stems_per_child_no_zeros$target_child_id)) #40
sum(four_child_all_stems_per_child_no_zeros$tokens) #266
length(exclusion_four_info_final$target_child_id) #266

#Extracting corpus information so we can exclude atypical children
exclusion_four_info <- four_year_olds_tokens_df_trimmed %>% filter(utterance_id %in% four_counts_pasted_targetid_uttid_stem_separate$utterance_id) 
length(unique(exclusion_four_info$stem))
unique(exclusion_four_info$stem) #nouns and empty stems
exclusion_four_info_final <- exclusion_four_info %>% filter(stem == 'touch' | stem == 'fight' | stem == 'match' | stem == 'kiss' | stem == 'meet' | stem == 'marry' | stem == 'attach' | stem == 'connect' | stem == 'hug'
                                                        | stem == 'join' | stem == 'separate' | stem == 'trade')
length(unique(exclusion_four_info_final$stem)) #12
#good this is the one we need to pull frames!
length(exclusion_four_info_final$target_child_id)

exclusion_four_info_final$target_child_id %in% four_child_all_stems_per_child_no_zeros$target_child_id
exclusion_four_info_final <- exclusion_four_info_final %>% arrange(target_child_id)
exclusion_four_info_final <- exclusion_four_info_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_four_info_final$target_child_id)
write.csv(exclusion_four_info_final, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\exclusion_four_info_final.csv")
#check the corpra in this df
unique(exclusion_four_info_final$corpus_name)

length(unique(exclusion_four_info_final$target_child_id))
length(unique(filter_by_stem_four$target_child_id))
missing_child_for_four <- filter_by_stem_four %>% filter(!target_child_id %in% exclusion_four_info_final$target_child_id)
length(unique(missing_child_for_four$target_child_id))

length(exclusion_four_info_final$target_child_id)

#tokens per corpus
corpra_tokens_four <- exclusion_four_info_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_four,"C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\tokens-from-each-corpus_four.csv")

#number of children in each corpus
num_of_children_in_each_corpus_four <- exclusion_four_info_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_four, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\num_of_children_in_each_corpus_four.csv")

#combined verb-pairs
combined_three_four_verb_pair_tokens <- rbind(verb_pairs_for_four_child_sheet,verb_pairs_for_three_child_sheet)

combined_three_four_verb_pair_tokens_summed <- combined_three_four_verb_pair_tokens %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = sum(num_chi))
write.csv(combined_three_four_verb_pair_tokens_summed, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\combined_three_four_verb_pair_tokens_summed.csv")

sum(four_child_all_stems_per_child_no_zeros$tokens) #266
length(exclusion_four_info_final$target_child_id) #266

#combined tokens per copra
combined_corpra_tokens <- rbind(corpra_tokens_three, corpra_tokens_four)
combined_corpra_tokens_final <- combined_corpra_tokens %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = sum(tokens))
write.csv(combined_corpra_tokens_final, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\combined_corpra_tokens_final.csv")

#combined number of children in each corpus
combined_num_of_children_in_each_corpus <- rbind(num_of_children_in_each_corpus_three,num_of_children_in_each_corpus_four)
combined_num_of_children_in_each_corpus_final <- combined_num_of_children_in_each_corpus %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = sum(num_chi))
write.csv(combined_corpra_tokens_final, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\symmetricals\\children\\combined_corpra_tokens_final.csv")

# SENTENCE FRAMES FOR 4 year olds *************************************************************************************************************************
four_ut <- get_utterances(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48,60)
)

four_ut_filtered <- four_ut %>% filter(utterance_id %in% exclusion)

#saving 
save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")
