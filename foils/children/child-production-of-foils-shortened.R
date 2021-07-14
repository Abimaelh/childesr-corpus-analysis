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

setwd("C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children")
getwd()

#importing the foil table
foil_list <- read.csv(file = "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\resources\\foil_list.csv", header = TRUE)

#how many unique foil stems do we have in total?
length(unique(foil_list$stem)) #27

#childesdb search of foils
three_year_olds_tokens_foil_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = foil_list$form
)

#checking how many children came up in the search.
length(unique(three_year_olds_tokens_foil_df$target_child_id)) #79

#trim the database by selecting the columns we are interested in
three_foils_df_trimmed <- select(three_year_olds_tokens_foil_df, 'target_child_id', 'corpus_name', 'target_child_age',
                              'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')

#rename columns
names(three_foils_df_trimmed)[names(three_foils_df_trimmed) == "part_of_speech"] <- "pos"
names(three_foils_df_trimmed)[names(three_foils_df_trimmed) == "gloss"] <- "form"
three_foils_df_trimmed

#filter the df by part (past participle) and v (verb)
three_foils_filtered_pos_df <- three_foils_df_trimmed %>% filter(pos == 'v' | pos == 'part')
length(three_foils_filtered_pos_df$target_child_id) #1715
#now we want to know what words don't appear in the db
three_foils_not_in_db <- foil_list %>% filter(!form %in% three_foils_filtered_pos_df$form)
#saving
write.csv(three_foils_not_in_db, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children\\three_foils_not_in_db")

length(three_foils_filtered_pos_df$target_child_id) #1715
length(unique(three_foils_filtered_pos_df$target_child_id)) #73

#making df of unique ids in three year olds
ids_for_threes_foils <- as.data.frame(unique(three_foils_filtered_pos_df$target_child_id))
names(ids_for_threes_foils)[names(ids_for_threes_foils) == "unique(three_foils_filtered_pos_df$target_child_id)"] <- "target_child_id"
length(unique(ids_for_threes_foils$target_child_id)) #73 unique ids

# generate 105 (because our sym_list has 105 rows) instances of an ID (n # of participants x f # of forms )
# we are trying to create a dataframe that has 2,970 rows (73 * 105)
three_many_ids_foils <- ids_for_threes_foils %>% slice(rep(1:n(), each = 105))
length(three_many_ids_foils$target_child_id) #7665

#generate 73 instances of each symmetrical - because we want the ids and sym words to have the same # of rows before
#we merge them together.
n = 73
threes_many_foils <- do.call("rbind", replicate(n, foil_list, simplify = FALSE))
length(threes_many_foils$form) #7665

# Merge many IDS with many syms
three_foils_and_id <- cbind(threes_many_foils, target_child_id = three_many_ids_foils$target_child_id) #2970
length(three_foils_and_id$target_child_id) #7665

#the number of participants differ from the original code "child-production-of-foils"
#because we searched for nouns and adjectives in that code. Here we focus on verbs. 
#So the total number of children searched between 3 and 4 should be the same.

#replacing age with 3
three_foils_filtered_pos_df$target_child_age <-  replace(three_foils_filtered_pos_df$target_child_age,
                                                       three_foils_filtered_pos_df$target_child_age >= 36.00 &
                                                         three_foils_filtered_pos_df$target_child_age <= 47.99, 3)
length(three_foils_filtered_pos_df$target_child_id) #1715
#you can use three_foils_filtered_pos_df to extract sentence frames for three year olds Using utterance_id
#saving
#save.image("~/GitHub/childesr-corpus-analysis/symmetricals/children/child-production-of-symmetricals-shortened-environment.RData")

# We are doing this to get a count for the words that are not~ recorded. So that when we merge with
# the words from sym_list all the words not produced, receive an NA, which we later change to 0.
detach(package:plyr)
three_counts_foils <- three_foils_filtered_pos_df %>% group_by(form, target_child_id, target_child_age, target_child_sex,corpus_name,utterance_id) %>%
  summarize(count = sum(unique(length(form)))) #349 rows
#checking we still have the same amount of children.
length(unique(three_counts_foils$target_child_id)) #73

#this could be useful later on.
three_counts_pasted_targetid_uttid_stem_foils <- as.data.frame(paste(three_foils_filtered_pos_df$target_child_id, three_foils_filtered_pos_df$stem, three_foils_filtered_pos_df$utterance_id))
names(three_counts_pasted_targetid_uttid_stem_foils)[names(three_counts_pasted_targetid_uttid_stem_foils) == "paste(three_foils_filtered_pos_df$target_child_id, three_foils_filtered_pos_df$stem, three_foils_filtered_pos_df$utterance_id)"] <- "wordstem"
three_counts_pasted_targetid_uttid_stem_separate_foils <- three_counts_pasted_targetid_uttid_stem_foils %>% separate(wordstem, c("target_child_id", "stem", "utterance_id"))
length(unique(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)) #73
three_counts_pasted_targetid_uttid_stem_separate_foils <- three_counts_pasted_targetid_uttid_stem_separate_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle'
                                                                                                                | stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                                                                                  stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                                                                                  stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy')
#change the stems to foil stems!
length(unique(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id)) #73
length(three_counts_pasted_targetid_uttid_stem_separate_foils$target_child_id) #1715 this number might change depending on whether or not there are errors in the filter process!

#arranging by target_id helps when merging columns.
three_counts_foils <- three_counts_foils %>% arrange(target_child_id)
length(unique(three_counts_foils$target_child_id)) #73

length(three_counts_foils$form) #1690, because this only includes counts for words that were produced by the child.Can we use this to plot data for people that have more than one count?
length(three_foils_and_id$form) #7665

three_full_foils <- merge(three_counts_foils, three_foils_and_id, all = TRUE)
three_full_foils <- three_full_foils %>% arrange(target_child_id)
#we dont really need sex, corpus, and utt id here.
three_full_foils$target_child_sex <-NULL
three_full_foils$corpus_name <- NULL
three_full_foils$utterance_id <- NULL

length(unique(three_full_foils$form)) #105
length(unique(three_full_foils$target_child_id)) #73
length(three_full_foils$form) #8,825

#now go in and change NAs for age to 3 and NAs for count to 0
three_full_foils$count[is.na(three_full_foils$count)] <- 0
three_full_foils$target_child_age[is.na(three_full_foils$target_child_age)] <- 3
sum(three_full_foils$count) #1715 matches with three_foils_filtered_pos_df? - this df doesn't count tokens, so we count the rows which are equal to one token.

#we would select our top 12 foils here, but im going to include all of them.
filter_by_stem_foils <- three_full_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                    stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                    stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                    stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy')
length(filter_by_stem_foils$target_child_id) #8825 rows
length(unique(filter_by_stem_foils$target_child_id)) #73

#collapsed by stem + the number of counts for each stem
three_child_sum_foils <- aggregate(filter_by_stem_foils$count, by=list(filter_by_stem_foils$stem), sum)

#every stem and their count for each child.
three_child_all_stems_per_child_foils <- filter_by_stem_foils %>% group_by(target_child_id,stem) %>%
  summarize(tokens = sum(count))

#test2 <- filter_by_stem %>% group_by(stem) %>%
#summarize(num_chi = )


#only_attach <- test %>% filter(stem == 'attach')
#checking <- as.data.frame(only_attach$target_child_id[only_attach$tokens > 0])
#table(checking) #this works but how to scale it up?

three_child_all_stems_per_child_no_zeros_foils <- three_child_all_stems_per_child_foils

three_child_all_stems_per_child_no_zeros_foils <- three_child_all_stems_per_child_no_zeros_foils %>% filter(tokens != 0)
length(unique(three_child_all_stems_per_child_no_zeros_foils$target_child_id))#73
length(three_child_all_stems_per_child_no_zeros_foils$target_child_id) #360
#eliminating the zeros worked!
verb_pairs_for_three_child_sheet_foils <- three_child_all_stems_per_child_no_zeros_foils %>% dplyr::group_by(stem) %>%
  dplyr::summarize(tokens = sum(tokens), num_chi = length(unique(target_child_id)))
length(unique(three_child_all_stems_per_child_no_zeros_foils$target_child_id)) #73


#Extracting corpus information so we can exclude atypical children
exclusion_three_info_foils <- three_foils_df_trimmed %>% filter(utterance_id %in% three_counts_pasted_targetid_uttid_stem_separate_foils$utterance_id) 
length(unique(exclusion_three_info_foils$stem)) #25
unique(exclusion_three_info_foils$stem) #nouns and empty stems
exclusion_three_info_foils_final <- exclusion_three_info_foils %>% filter(stem == 'cover' | stem == 'insert' | stem == 'tie' | stem == 'drop' | stem == 'knot' | stem == 'punch' | stem == 'kick' | stem == 'attack' | stem == 'tickle' |
                                                                            stem == 'pet' | stem == 'stroke' | stem == 'pull' | stem == 'lick' | stem == 'bite' | stem == 'invite' | stem == 'celebrate' | 
                                                                            stem == 'adopt' | stem == 'choose' | stem == 'check' | stem == 'teach' | stem == 'greet' | stem == 'push' | stem == 'tap' | 
                                                                            stem == 'hold' | stem == 'bump' | stem == 'sell' | stem == 'buy')
length(unique(exclusion_three_info_foils_final$stem)) #23 - because we deleted all the foils with zeros.
#good this is the one we need to pull frames!
length(exclusion_three_info_foils_final$target_child_id) #1760
length(unique(exclusion_three_info_foils_final$target_child_id)) #73

exclusion_three_info_foils_final$target_child_id %in% three_child_all_stems_per_child_no_zeros_foils$target_child_id
exclusion_three_info_foils_final <- exclusion_three_info_foils_final %>% arrange(target_child_id)
exclusion_three_info_foils_final <- exclusion_three_info_foils_final %>% filter(pos == 'v' | pos == 'part')
length(exclusion_three_info_foils_final$target_child_id) #1715 should be the same when we add the sum of counts for no zeros three.
sum(three_child_all_stems_per_child_no_zeros_foils$tokens) #1715. Yup they match.
length(unique(exclusion_three_info_foils_final$target_child_id))#73

write.csv(exclusion_three_info_foils_final, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children\\exclusion_three_info_foils_final.csv")
#check the corpra in this df
unique(exclusion_three_info_foils_final$corpus_name)

#tokens per corpus
corpra_tokens_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(tokens = length(corpus_name))
write.csv(corpra_tokens_three_foils,"C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children\\tokens-from-each-corpus-three-foils.csv")

#number of children in each corpus
num_of_children_in_each_corpus_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(corpus_name) %>%
  dplyr::summarize(num_chi = length(unique(target_child_id)))
write.csv(num_of_children_in_each_corpus_three_foils, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children\\num-of-children-in-each-corpus-three-foils.csv")

#token frequency per child to check for outliers
token_freq_per_child_three_foils <- exclusion_three_info_foils_final %>% dplyr::group_by(target_child_id) %>%
  dplyr::summarize(tokens = (length(stem)), corpus_name = (corpus_name))
token_freq_per_child_three_foils_final <- unique(token_freq_per_child_three_foils)
write.csv(token_freq_per_child_three_foils_final, "C:\\Users\\abima\\Documents\\GitHub\\childesr-corpus-analysis\\foils\\children\\token-freq-per-child-three-foils-final.csv")

#checking one participant
get_participants(
  collection = "Eng-NA",
  corpus = "Demetras1",
  age = 39
)

#* 3 year old foil frames here:
#* 

#* 4 year old tokens here:
#*
