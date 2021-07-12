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

setwd("C:\\Users\\abima\\Desktop\\corp-an")
getwd()

sym_list <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\sym_list3.csv", header = TRUE)
sym_list$X <- NULL
sym_list$X.1 <- NULL
sym_list$X.2 <- NULL

sym_list

sym_list_pos <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\sym_list3_pos_no_gerund.csv", header = TRUE)
sym_list_pos

#now to get a clean token df for 3 year olds only.
three_year_olds_tokens_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = sym_list$form
)

length(unique(three_year_olds_tokens_df$target_child_id))#55

three_year_olds_tokens_df <- three_year_olds_tokens_df %>% arrange(target_child_id)

three2_year_olds_tokens_df <- select(three_year_olds_tokens_df, 'target_child_id', 'corpus_name', 'target_child_age',
                         'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')
length(unique(three2_year_olds_tokens_df$target_child_id)) #55 before filtering by POS
length(three_year_olds_tokens_df$gloss) #1047
names(three2_year_olds_tokens_df)[names(three2_year_olds_tokens_df) == "part_of_speech"] <- "pos"
names(three2_year_olds_tokens_df)[names(three2_year_olds_tokens_df) == "gloss"] <- "form"
three2_year_olds_tokens_df

#filter by pos - setting up the column we need.
sym_list_pos_paste <- sym_list_pos
sym_list_pos_paste$formpos <- paste(sym_list_pos_paste$form, sym_list_pos_paste$pos)
three2_year_olds_tokens_df$formpos <- paste(three2_year_olds_tokens_df$form, three2_year_olds_tokens_df$pos)

three2_year_olds_tokens_df_filtered_pos <- three2_year_olds_tokens_df %>%
  filter(formpos %in% sym_list_pos_paste$formpos)
length(three2_year_olds_tokens_df_filtered_pos$target_child_id) #745 it worked!

length(unique(three2_year_olds_tokens_df_filtered_pos$target_child_id)) #46 after filtering by POS
length(three2_year_olds_tokens_df_filtered_pos$target_child_id) # for later comparison with three_full.#745

#making df of unique ids in three year olds
ids_for_threes <- unique(three2_year_olds_tokens_df_filtered_pos$target_child_id)
ids_for_threes <- as.data.frame(ids_for_threes)
length(unique(ids_for_threes$ids_for_threes)) #46 unique ids

# generate 90 instances of an ID (n # of participants x f # of forms )
# we are trying to create a dataframe that has 4,140 rows (46 * 90)
three_many_ids <- ids_for_threes %>% slice(rep(1:n(), each = 90))
length(three_many_ids$ids_for_threes) #4140
names(three_many_ids)[names(three_many_ids) == "ids_for_threes"] <- "unique_ids"
three_many_ids

#generate 46 instances of sym_words - because we want the ids and sym words to have the same # of rows before
# we merge them together.
n = 46
threes_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(threes_many_syms$form) #4140


# Merge many IDS with many syms
three_sym_and_id <- cbind(threes_many_syms, target_child_id = three_many_ids$unique_ids)#4140
length(three_sym_and_id$target_child_id) #4140

#replacing age with 3
three2_year_olds_tokens_df_filtered_pos$target_child_age <-  replace(three2_year_olds_tokens_df_filtered_pos$target_child_age,
                                                                     three2_year_olds_tokens_df_filtered_pos$target_child_age >= 36.00 &
                                                                       three2_year_olds_tokens_df_filtered_pos$target_child_age <= 47.99, 3)

#you can use three_2_year_olds_tokens_df_filtered_pos to extract sentence frames for three year olds Using utterance_id

#before running this change the age of all children to 3! and then merge.
# We are doing this to get a count for the words that are not~ recorded. So that when we merge with
# the words from sym_list all the words not produced, receive an NA, which we later change to 0.
detach(package:plyr)
three_counts <- three2_year_olds_tokens_df_filtered_pos %>% group_by(form, target_child_id, target_child_age, target_child_sex) %>%
  summarize(count = sum(unique(length(form)))) #244
#do we need utterance id?
length(unique(three_counts$target_child_id)) #46

three_counts <- three_counts %>% arrange(target_child_id)
length(unique(three_counts$target_child_id)) #46 good.

length(three_counts$form) #224, because this only includes counts for words that were produced by the child.Can we use this to plot data for people that have more than one count?
length(three_sym_and_id$form) #4140

three_full <- merge(three_counts, three_sym_and_id, all = TRUE)
three_full <- three_full %>% arrange(target_child_id)

length(unique(three_full$form))
length(unique(three_full$target_child_id))
length(three_full$form) #4140
#stopped here - 3/29 - 10:07am
#now go in and change NAs for age to 3 and NAs for count to 0

three_full$count[is.na(three_full$count)] <- 0
three_full$target_child_age[is.na(three_full$target_child_age)] <- 3
sum(three_full$count) #745 does it match with three_year_olds_tokendf? YUP.
length(three2_year_olds_tokens_df_filtered_pos$form) #745 aftering filtering POs.

length(unique(three_full$form)) #90

#now lets get proportions and such for three year olds.
length(three_full$form)#4140
length(unique(three_full$target_child_id))#46

# collpasing here for 3 year olds.
detach(package:plyr)
three_col_new <- three_full %>% group_by(target_child_id, stem) %>%
  summarize(counts = sum(count))
length(unique(three_col_new$target_child_id)) #46
length(three_col_new$target_child_id) #1150

write.csv(three_col_new, "C:\\Users\\abima\\Desktop\\corp-an\\threes\\three_col_new.csv")

#three_summarized_counts <- three2_year_olds_tokens_df_filtered_pos %>% group_by(stem,form, pos) %>%
  #summarize(count = sum(unique(length(stem))))
#three_summarized_counts2 <- three2_year_olds_tokens_df_filtered_pos %>% group_by(stem, pos) %>%
  #summarize(count = sum(unique(length(stem))))

#three_sym_not_found <- sym_list_pos %>% filter(!form %in% three_summarized_counts$form)
#three_sym_not_found['pos'] <- NA
#three_sym_not_found['count'] <- NA
#three_not_found_counts <- three_sym_not_found %>% group_by(form, pos) %>%
  #summarize(count = sum(unique(length(form))))

#three_sym_not_found$count[is.na(three_sym_not_found$count)] <- 0
#names(three_sym_not_found)[names(three_sym_not_found) == "Predicate"] <- "stem"

#three_wide_full <- rbind(three_wide_counts, three_sym_not_found)

#write.csv(three_wide_full, "C:\\Users\\abima\\Desktop\\corp-an\\threes\\three_full.csv") #count for each word + pos

#speaker stats for three year olds.*********************************************************************************************************************
speaker_stats_three <- get_speaker_statistics(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
)

sub_speaker_stats_three <- speaker_stats_three

sub_speaker_stats_three <- select(sub_speaker_stats_three, "target_child_id", "num_tokens")

#to get only tokens for the children in our data.
#sub_speaker_stats_three2 <- filter(sub_speaker_stats_three, target_child_id %in% col_new$target_child_id)
sub_speaker_stats_three2 <- filter(sub_speaker_stats_three, target_child_id %in% three_full$target_child_id)

length(unique(sub_speaker_stats_three2$target_child_id)) # 46, so potentially no repeats
#sub_speaker_stats_three2 is df we want to use when we check for repeats. 
ids_for_sub_speaker_stats_three2 <- unique(sub_speaker_stats_three2$target_child_id) # list of unique ids in ids_for_sub_speaker_stats_three2 
ids_for_sub_speaker_stats_three2_df <- as.data.frame(ids_for_sub_speaker_stats_three2)# as a df

length(sub_speaker_stats_three2$target_child_id)#use sub_speaker_stats_three2 if you want to create a df with the sum of tokens per child for 3 year olds
#822 is that reasonable.

#getting the sum of tokens per child for 3 year olds.
three_speaker_tokens_for_col_new <- aggregate(sub_speaker_stats_three2$num_tokens, by=list(sub_speaker_stats_three2$target_child_id), sum)
length(unique(three_speaker_tokens_for_col_new$Group.1)) #46

#rename the columns for 3 year olds.
names(three_speaker_tokens_for_col_new)[names(three_speaker_tokens_for_col_new) == "x"] <- "tokens" 
names(three_speaker_tokens_for_col_new)[names(three_speaker_tokens_for_col_new) == "Group.1"] <- "target_child_id_2" 

#3 year olds
three_speaker_tokens_for_col_new <- three_speaker_tokens_for_col_new %>% arrange(target_child_id_2)

#3 year olds, n = to number of stems.
three_speaker_tokens_for_col_new <- three_speaker_tokens_for_col_new %>% slice(rep(1:n(), each = 25))
length(three_speaker_tokens_for_col_new$tokens) #1150
length(three_col_new$counts) #1150
sum(three_col_new$counts) #745

#3 year olds
three_collapsed_stem_prop <- cbind(three_col_new, three_speaker_tokens_for_col_new)

three_collapsed_stem_prop$target_child_id_2 <- NULL

three_collapsed_stem_prop2 <- transform(three_collapsed_stem_prop, prop = counts / tokens)

#sum for each count per child.
three_child_sum <- aggregate(three_collapsed_stem_prop2$count, by=list(three_collapsed_stem_prop2$target_child_id), sum)

# then, mean for each child.
sum(three_collapsed_stem_prop2$counts) #... , so you would do n (that child's sum, divided by 745?)

#sum and mean
library(plyr)
three_child_mean <- transform(three_child_sum, mean = x / 745)
names(three_child_mean)[names(three_child_mean) == "x"] <- "sum" 
names(three_child_mean)[names(three_child_mean) == "Group.1"] <- "target_child_id"
#then slice this 25 times for each stem, and bind it to collapsed_stem_prop2, make a new df full_df just in case.
three_child_mean_for_df <- three_child_mean %>% slice(rep(1:n(), each = 25)) #n here should be equal to the number of stems
three_collapsed_stem_prop3 <- cbind(three_collapsed_stem_prop2, three_child_mean_for_df) #collapsed_stem_prop3 has sum and mean added as another column to collapsed_stem_prop2

# whats the total number of tokens? - use speaker_tokens_for_col_new
#sum(three_speaker_tokens_for_col_new$tokens) # 14459725. This is summing all the repeats. # too big.

#for prop mean denominator. Create a new variable from speaker_tokens_for_col_new before slicing 25 times.
three_speaker_tokens <- aggregate(sub_speaker_stats_three2$num_tokens, by=list(sub_speaker_stats_three2$target_child_id), sum)
length(unique(three_speaker_tokens$Group.1)) #46
sum(three_speaker_tokens$x) #578389

# sum prop column for each child. Which one are we using?
three_prop_mean <- transform(three_collapsed_stem_prop2, prop_mean = prop / 578389)
sum(three_collapsed_stem_prop2$prop)
three_prop_mean2 <- transform(three_collapsed_stem_prop2, prop_mean = prop / 0.0778102) #this number comes from summing the prop of collapsed_stem_prop2 (code above)

#how to initiate a package after detaching it?
#mean_prop <- ddply(collapsed_stem_prop2) #what does this even do?


write.csv(three_collapsed_stem_prop2, "C:\\Users\\abima\\Desktop\\corp-an\\threes\\three_collapsed_stem_prop2.csv")
write.csv(three_prop_mean, "C:\\Users\\abima\\Desktop\\corp-an\\threes\\three_prop_mean.csv")

library(plyr)
three_sumdata <- ddply(three_collapsed_stem_prop2, .(stem), summarise, sumTokens = sum(counts), meanTokens = mean(counts), minTokens = min(counts), maxTokens = max(counts), stdTokens = sd(counts), meanProp = mean(prop))
write.csv(three_sumdata, "C:\\Users\\abima\\Desktop\\corp-an\\threes\\three_sumdata.csv")

#sumdata <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\sumdata.csv", header = TRUE)
three_sumdata2 <- three_sumdata

three_col_stem_prop <- three_collapsed_stem_prop2
level_order <- factor(three_col_stem_prop$stem, level = c("combine", "chat", "compete", 
                                                    "equal", "marry", "match", 
                                                    "meet", "same", "similar",
                                                    "trade", "fight", "separate",
                                                    "differ", "friend",  "connect", "attach", "argue",
                                                    "split", "kiss",
                                                    "hug", "disagree",
                                                    "agree",
                                                    "touch", "join", "bump"))

plot <- ggplot(three_col_stem_prop, aes(x=level_order, y=counts)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom="point", color = "red", size=2) +
  xlab("stem")+
  ylim(0,20)
plot + theme(legend.position = "none")
#ggsave("threes_zoomed.png", width = 15)

# for plotting stems with a count greater than 0.
three_more_than_one <- three_collapsed_stem_prop2 %>% filter(counts > 0)
plot <- ggplot(three_more_than_one, aes(x=stem, y=counts)) +
  geom_boxplot() +
  stat_summary(fun = mean, geom = 'point', color = "red", size = 2) +
  xlab("stem")+
  ylim(0,20)
plot + theme(legend.position = 'none')
ggsave('threes_more_than_one_zoomed.png', width = 15)

#for plotting # of tokens in their corpora
three_child_corpora_tokes <- three_speaker_tokens
names(three_child_corpora_tokes)[names(three_child_corpora_tokes) == "x"] <- "tokens" 
names(three_child_corpora_tokes)[names(three_child_corpora_tokes) == "Group.1"] <- "target_child_id"
three_ids_tokes <- factor(three_child_corpora_tokes$target_child_id)
plot <- ggplot(three_child_corpora_tokes, aes(x=three_ids_tokes, y=tokens)) +
  geom_point() +
  xlab("child ids")+
  ylab('tokens')
plot + theme(legend.position = 'none')
ggsave('threes_child_corpora_tokes.png', width = 15)

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

four_year_olds_tokens_df <- four_year_olds_tokens_df %>% arrange(target_child_id)

four2_year_olds_tokens_df <- select(four_year_olds_tokens_df, 'target_child_id', 'corpus_name', 'target_child_age',
                                    'gloss', 'part_of_speech','target_child_sex','stem','utterance_id')
length(unique(four2_year_olds_tokens_df$target_child_id)) #63 unique before filtering
length(four2_year_olds_tokens_df$gloss) #1583
names(four2_year_olds_tokens_df)[names(four2_year_olds_tokens_df) == "part_of_speech"] <- "pos"
names(four2_year_olds_tokens_df)[names(four2_year_olds_tokens_df) == "gloss"] <- "form"


#filter by pos
#filter by pos - setting up the column we need.
#sym_list_pos$formpos <- paste(sym_list_pos$form, sym_list_pos$pos) - Already initiated above.

four2_year_olds_tokens_df$formpos <- paste(four2_year_olds_tokens_df$form, four2_year_olds_tokens_df$pos)

four2_year_olds_tokens_df_filtered_pos <- four2_year_olds_tokens_df %>%
  filter(formpos %in% sym_list_pos_paste$formpos)
length(four2_year_olds_tokens_df_filtered_pos$target_child_id) #1167 it worked!

length(unique(four2_year_olds_tokens_df_filtered_pos$target_child_id)) #60 after filtering by POS (but this still has repeats)
length(four2_year_olds_tokens_df_filtered_pos$target_child_id) # for later comparison with four_full.#1167

#making df of unique ids in four year olds
ids_for_four <- unique(four2_year_olds_tokens_df_filtered_pos$target_child_id)
ids_for_four <- as.data.frame(ids_for_four)
length(unique(ids_for_four$ids_for_four)) #60 unique ids, with repeats

#generate 90 instances of an ID
four_many_ids <- ids_for_four %>% slice(rep(1:n(), each = 90))
length(four_many_ids$ids_for_four) #5400
names(four_many_ids)[names(four_many_ids) == "ids_for_four"] <- "unique_ids"
four_many_ids

#generate 62 (for each unique id) instances of sym_words
n = 60
four_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(four_many_syms$form) #5400

# Merge many IDS with many syms
four_sym_and_id <- cbind(four_many_syms, target_child_id = four_many_ids$unique_ids)
length(four_sym_and_id$target_child_id) #5400

#merging three_sym_and_id with three_year_olds_token

#replacing age with 4

#peace <- four_year_olds_tokens_df
four2_year_olds_tokens_df_filtered_pos$target_child_age <-  replace(four2_year_olds_tokens_df_filtered_pos$target_child_age,
                                                                    four2_year_olds_tokens_df_filtered_pos$target_child_age >= 48.00 &
                                                                    four2_year_olds_tokens_df_filtered_pos$target_child_age < 60.00, 4)

#length(peace$target_child_id) #1196
#before running this change the age of all children to 4! and then merge
detach(package:plyr)
four_counts <- four2_year_olds_tokens_df_filtered_pos %>% group_by(form, target_child_id, target_child_age, target_child_sex) %>%
  summarize(count = sum(unique(length(form))))

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
