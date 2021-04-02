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
sym_list

sym_list_pos <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\sym_list3_pos_no_gerund.csv", header = TRUE)

#now to get a clean token df for 3 year olds only.
three_year_olds_tokens_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = sym_list$form
)

three_year_olds_tokens_df <- three_year_olds_tokens_df %>% arrange(target_child_id)

three2_year_olds_tokens_df <- select(three_year_olds_tokens_df, 'target_child_id', 'corpus_name', 'target_child_age',
                         'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')
length(unique(three2_year_olds_tokens_df$target_child_id)) #55 before filtering by POS
length(three_year_olds_tokens_df$gloss) #1047
names(three2_year_olds_tokens_df)[names(three2_year_olds_tokens_df) == "part_of_speech"] <- "pos"
names(three2_year_olds_tokens_df)[names(three2_year_olds_tokens_df) == "gloss"] <- "form"

#filter by pos - setting up the column we need.
sym_list_pos$formpos <- paste(sym_list_pos$form, sym_list_pos$pos)
three2_year_olds_tokens_df$formpos <- paste(three2_year_olds_tokens_df$form, three2_year_olds_tokens_df$pos)

three2_year_olds_tokens_df_filtered_pos <- three2_year_olds_tokens_df %>%
  filter(formpos %in% sym_list_pos$formpos)
length(three2_year_olds_tokens_df_filtered_pos$target_child_id) #745 it worked!

length(unique(three2_year_olds_tokens_df_filtered_pos$target_child_id)) #46 after filtering by POS
length(three2_year_olds_tokens_df_filtered_pos$target_child_id) # for later comparison with three_full.#745

#making df of unique ids in three year olds
ids_for_threes <- unique(three2_year_olds_tokens_df_filtered_pos$target_child_id)
ids_for_threes <- as.data.frame(ids_for_threes)
length(unique(ids_for_threes$ids_for_threes)) #46 unique ids

#generate 90 instances of an ID (n # of participants x f # of words )
# we are trying to create a dataframe that has 4,140 rows
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


#before running this change the age of all children to 3! and then merge.
# We are doing this to get a count for the words that are recorded. So that when we merge with
# the words from sym_list all the words not produced, receive an NA, which we later change to 0.
detach(package:plyr)
three_counts <- three2_year_olds_tokens_df_filtered_pos %>% group_by(form, target_child_id, target_child_age, target_child_sex) %>%
  summarize(count = sum(unique(length(form))))
#do we need utterance id?
length(unique(three_counts$target_child_id)) #46

three_counts <- three_counts %>% arrange(target_child_id)
length(unique(three_counts$target_child_id)) #46 good.

length(three_counts$form) #224
length(three_sym_and_id$form) #4140

three_full <- merge(three_counts, three_sym_and_id, all = TRUE)
three_full <- three_full %>% arrange(target_child_id)

length(unique(three_full$form))

length(three_full$form) #4140
#stopped here - 3/29 - 10:07am
#now go in and change NAs for age to 3 and NAs for count to 0

three_full$count[is.na(three_full$count)] <- 0
three_full$target_child_age[is.na(three_full$target_child_age)] <- 3
sum(three_full$count) #745 does it match with three_year_olds_tokendf?
length(three2_year_olds_tokens_df_filtered_pos$form) #745 aftering filtering POs.

length(unique(three_full$form)) #90

# *********************************************************************************************************************

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
  filter(formpos %in% sym_list_pos$formpos)
length(four2_year_olds_tokens_df_filtered_pos$target_child_id) #1167 it worked!

length(unique(four2_year_olds_tokens_df_filtered_pos$target_child_id)) #60 after filtering by POS (but this still has repeats)
length(four2_year_olds_tokens_df_filtered_pos$target_child_id) # for later comparison with three_full.#1167

#making df of unique ids in four year olds
ids_for_four <- unique(four2_year_olds_tokens_df_filtered_pos$target_child_id)
ids_for_four <- as.data.frame(ids_for_four)
length(unique(ids_for_four$ids_for_four)) #60 unique ids

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
four_counts <- four2_year_olds_tokens_df_filtered_pos %>% group_by(form, target_child_id, target_child_age, target_child_sex) %>%
  summarize(count = sum(unique(length(form))))

four_counts <- four_counts %>% arrange(target_child_id)

length(unique(four_counts$target_child_id)) #60

length(four_counts$target_child_id) #388
length(four_sym_and_id$form)#5400
length(unique(four_sym_and_id$target_child_id)) #60

#stem_dropped <- subset(four_sym_and_id, select = -c(stem))

four_full <- merge(four_counts, four_sym_and_id, all = TRUE) #Original 
length(four_full$target_child_id) #5400
# Testing whether stem is creating the extra row. NO.
#four_full <- merge(four_counts, stem_dropped, all = TRUE)


# remove row 4589 ************************************************************************
#four_full<- four_full[-c(5085),] #"removes thought"



# testing whether sex is adding an extra row or age.
#sex_dropped <- subset(four_counts, select = -c(target_child_sex,target_child_age))

# nope, still one extra row.
#four_full <- merge(sex_dropped, stem_dropped, all = TRUE)

length(unique(four_full$form)) #90
length(unique(four_full$target_child_id)) #60

length(four_full$target_child_id) #5400
four_full <- four_full %>% arrange(target_child_id)

#now go in and change nas for age to 4 and nas for count to 0

four_full$count[is.na(four_full$count)] <- 0
four_full$target_child_age[is.na(four_full$target_child_age)] <- 4
sum(four_full$count) #1167. Matches four_year_olds_tokendf?, ** 
#it should be 1214 because 'thought' was in four_year_old_token.Not anymore. We are using a table to filter now. 

sum(length(four2_year_olds_tokens_df_filtered_pos$form)) #1167

#length(four_full$form == "friend")
#count(four_full[1:100,], vars = "form")

length(unique(four_full$form))
length(unique(three_full$form))
# ***********************************************************************************************************

## **************************************************Dealing with repeats************************************
length(unique(four_full$target_child_id)) # 5400 obs before removing the 14 children that are repeats.60!
four_full_no_rep <- four_full[!(four_full$target_child_id == repeats),]

length(unique(four_full_no_rep$target_child_id)) #60
length(unique(three_full$target_child_id)) #46
# this equals 110, but then when you merge them and create 'new' it becomes 96 because of the repeat subjects in
# 3 year olds and 4 year olds.
four2_full <- four_full_no_rep


length(unique(four2_full$target_child_id)) #60
#four2_full <- subset(four_full, target_child_id != repeats2)
four2_full <- four2_full[! four2_full$target_child_id %in% repeats,]
length(unique(four2_full$target_child_id)) #46

master_df <- rbind(three_full, four2_full)
length(three_full$form)#4140
length(four2_full$form)#4140
length(unique(three_full$target_child_id))#46
length(unique(four2_full$target_child_id))#46
length(unique(four_full$target_child_id))#60

length(master_df$form)#8280

#checking to see if they are no more repeats
thetruth <- four2_full[four2_full$target_child_id %in% three_full$target_child_id,]
four_true<-unique(four2_full$target_child_id)
three_true <-unique(three_full$target_child_id)
four_true == three_true

# collapsing here again for combined
detach(package:plyr)
col_new <- master_df %>% group_by(target_child_id, stem) %>%
  summarize(counts = sum(count))

length(unique(four2_full$stem)) #25 thank god.
length(unique(col_new$target_child_id)) #92

length(unique(four2_full$target_child_id)) #46
length(unique(three_full$target_child_id)) #46
# well continue this tomorrow...col_new is the only one Alon is interested in.
# Add proportions to this, means, and averages for each row.

#write.csv(col_new, "C:\\Users\\abima\\Desktop\\corp-an\\collapsed_stems.csv")
write.csv(col_new, "C:\\Users\\abima\\Desktop\\corp-an\\col_new.csv")

# collpasing here for 3 year olds.
detach(package:plyr)
three_col_new <- three_full %>% group_by(target_child_id, stem) %>%
  summarize(counts = sum(count))
length(unique(three_col_new$target_child_id)) #46
length(three_col_new$target_child_id) #1150

# collapsing here for 4 year olds.
detach(package:plyr)
four_col_new <- four2_full %>% group_by(target_child_id, stem) %>%
  summarize(counts = sum(count))
length(unique(four_col_new$target_child_id)) #46
length(four_col_new$target_child_id) #1150


#*********************************************************************************************************************************************************
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

ids_for_sub_speaker_stats_four2_df == ids_for_sub_speaker_stats_three2_df 

#combine the speaker stats for 3 and 4 year olds!*********************************************************************************************************
full_sub_speaker_stats2 <- rbind(sub_speaker_stats_four2, sub_speaker_stats_three2)

length(sub_speaker_stats_four2$target_child_id) #use sub_speaker_stats_four2 if you want to create a df with the sum of tokens per child for 4 year olds
length(sub_speaker_stats_three2$target_child_id)#use sub_speaker_stats_three2 if you want to create a df with the sum of tokens per child for 3 year olds
length(full_sub_speaker_stats2$target_child_id) #we will be using the combined 

# getting the sum of tokens per child for 3 and 4 year olds
speaker_tokens_for_col_new <- aggregate(full_sub_speaker_stats2$num_tokens, by=list(full_sub_speaker_stats2$target_child_id), sum)
length(unique(speaker_tokens_for_col_new$Group.1)) # 92

#getting the sum of tokens per child for 4 year olds.
four_speaker_tokens_for_col_new <- aggregate(sub_speaker_stats_four2$num_tokens, by=list(sub_speaker_stats_four2$target_child_id), sum)
length(unique(four_speaker_tokens_for_col_new$Group.1)) #46

#getting the sum of tokens per child for 3 year olds.
three_speaker_tokens_for_col_new <- aggregate(sub_speaker_stats_three2$num_tokens, by=list(sub_speaker_stats_three2$target_child_id), sum)
length(unique(three_speaker_tokens_for_col_new$Group.1)) #46

#rename the columns for combined.
names(speaker_tokens_for_col_new)[names(speaker_tokens_for_col_new) == "x"] <- "tokens" 
names(speaker_tokens_for_col_new)[names(speaker_tokens_for_col_new) == "Group.1"] <- "target_child_id_2" 

#rename the columns for 4 year olds.
names(four_speaker_tokens_for_col_new)[names(four_speaker_tokens_for_col_new) == "x"] <- "tokens" 
names(four_speaker_tokens_for_col_new)[names(four_speaker_tokens_for_col_new) == "Group.1"] <- "target_child_id_2" 

#rename the columns for 3 year olds.
names(three_speaker_tokens_for_col_new)[names(three_speaker_tokens_for_col_new) == "x"] <- "tokens" 
names(three_speaker_tokens_for_col_new)[names(three_speaker_tokens_for_col_new) == "Group.1"] <- "target_child_id_2" 


#************************************************************************************************************************************************************
#combined
speaker_tokens_for_col_new <- speaker_tokens_for_col_new %>% arrange(target_child_id_2) #need this to be the same length as col_new

#4 year olds
four_speaker_tokens_for_col_new <- four_speaker_tokens_for_col_new %>% arrange(target_child_id_2)

#3 year olds
three_speaker_tokens_for_col_new <- three_speaker_tokens_for_col_new %>% arrange(target_child_id_2)

#combined
speaker_tokens_for_col_new_sliced <- speaker_tokens_for_col_new %>% slice(rep(1:n(), each = 25)) #n here should be equal to the number of stems
# the length should be equal to col_new.
length(speaker_tokens_for_col_new_sliced$tokens) #2300, 92 children x 25 stems.
length(col_new$counts) #2300
sum(col_new$counts)

#4 year olds
four_speaker_tokens_for_col_new_sliced <- four_speaker_tokens_for_col_new %>% slice(rep(1:n(), each = 25)) #n here should be equal to the number of stems
# the length should be equal to col_new.
length(four_speaker_tokens_for_col_new_sliced$tokens) #1150, 46 children x 25 stems. need to create col_new for 3 and 4 year olds.
length(four_col_new$counts) #1150
sum(four_col_new$counts) #661

#3 year olds
three_speaker_tokens_for_col_new <- three_speaker_tokens_for_col_new %>% slice(rep(1:n(), each = 25))
length(three_speaker_tokens_for_col_new$tokens) #1150
length(three_col_new$counts) #1150
sum(three_col_new$counts) #745

#stopped here - need to do this for 3 and 4 separaetly. not this bottom code trhough.
collapsed_stem_prop <- cbind(col_new, speaker_tokens_for_col_new_sliced)

collapsed_stem_prop$target_child_id_2 <- NULL

collapsed_stem_prop2 <- transform(collapsed_stem_prop, prop = counts / tokens)

#sum for each count per child.
child_sum <- aggregate(collapsed_stem_prop2$count, by=list(collapsed_stem_prop2$target_child_id), sum)

# then, mean for each child.
sum(collapsed_stem_prop2$counts) #1406, so you would do n (that child's sum, divided by 1583)

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

#how to initiate a package after detaching it?
#mean_prop <- ddply(collapsed_stem_prop2) #what does this even do?


write.csv(collapsed_stem_prop2, "C:\\Users\\abima\\Desktop\\corp-an\\collapsed_stem_prop2.csv")


sumdata <- ddply(collapsed_stem_prop2, .(stem), summarise, sumTokens = sum(counts), meanTokens = mean(counts), minTokens = min(counts), maxTokens = max(counts), stdTokens = sd(counts), meanProp = mean(prop))
write.csv(sumdata, "C:\\Users\\abima\\Desktop\\corp-an\\sumdata.csv")

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
  xlab("stem")#+
  #ylim(0,20)
plot + theme(legend.position = "none")
#ggsave("ggplot_categoryv2_zoomed.png", width = 15)
# *************************************************
pure_stems <- c("combine", "chat", "compete", 
           "equal", "marry", "match", 
           "meet", "same", "similar",
           "trade", "fight", "separate")
mix_stems <- c("split","attach", "kiss",
                "hug", "friend", "disagree",
                "differ", "connect", "argue",
                "agree")
nonsym_stems <- c("touch", "join", "bump")
a <- ifelse(col_stem_prop$stem %in% pure_stems, "red","blue")
b <- ifelse(col_stem_prop$stem %in% mix_stems, "blue", "orange")
c <- ifelse(col_stem_prop$stem %in% nonsym_stems, "green", "purple")


plot <- ggplot(col_stem_prop, aes(x=level_order, y=counts, fill = level_order)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom="point", color = "red", size=2) +
  xlab("stem")
plot + theme(axis.text.x = element_text(colour = a))
#ggsave("ggplot_category.png", width = 15)

#write.csv(collapsed_stem_prop2, "C:\\Users\\abima\\Desktop\\corp-an\\collapsed_stem_prop2.csv")

length(unique(four2_full$target_child_id))
length(unique(three_full$target_child_id))

#getting target_child_sex
target_child_sex_three <- select(three_full, "target_child_id", "target_child_sex")
target_child_sex_three <- filter(target_child_sex_three, target_child_sex %in% "male")
length(unique(target_child_sex_three$target_child_id)) #24 males in three year olds df. Which means, 22 females., 2 missing sex

target_child_sex_three <- select(three_full, "target_child_id", "target_child_sex")
target_child_sex_three_female <- filter(target_child_sex_three, target_child_sex %in% "female")
length(unique(target_child_sex_three_female$target_child_id)) #22


target_child_sex_four <- select(four2_full, "target_child_id", "target_child_sex")
target_child_sex_four <- filter(target_child_sex_four, target_child_sex %in% "male")
length(unique(target_child_sex_four$target_child_id)) #31, which means we have 16 females in four year old df. 1 missing sex.

target_child_sex_four <- select(four2_full, "target_child_id", "target_child_sex")
target_child_sex_four_female <- filter(target_child_sex_four, target_child_sex %in% "female")
length(unique(target_child_sex_four_female$target_child_id)) #16 - 1 data point missing sex.

#getting corpus info
length(unique(four2_year_olds_tokens_df$corpus_name)) #16 - 13 = 3 unique corpora
length(unique(three2_year_olds_tokens_df$corpus_name)) #22 - 13 = 9 unique corpora 

repeats <- four2_year_olds_tokens_df$corpus_name[four2_year_olds_tokens_df$corpus_name %in% three2_year_olds_tokens_df$corpus_name]
length(unique(repeats))
unique(repeats)

#four_counts <- four2_year_olds_tokens_df %>% group_by(form, target_child_id, target_child_age, target_child_sex) %>%
  #summarize(count = sum(unique(length(form))))

collapsed_stem_prop2$stem <- as.factor(collapsed_stem_prop2$stem)
p <- collapsed_stem_prop2 %>%
  mutate(stem = fct_relevel(stem, 
                            "combine", "chat", "compete", 
                            "equal", "marry", "match", 
                            "meet", "same", "similar",
                            "trade", "fight", "separate",
                            "differ", "friend", "connect",
                            "argue", "attach",
                            "split", "kiss",
                            "hug", "disagree",
              
                            "agree",
                            "touch", "join", "bump")) %>%
  ggplot( aes(x=stem, y=counts)) +
  geom_boxplot()
  plot + stat_summary(fun.y=mean, geom="point", color = "red", size=2)
  ggsave("ggplot_categoryv2.png", width = 15)
  
  
#og plot                                               
plot <- ggplot(collapsed_stem_prop2, aes(x=stem, y=counts)) +
  geom_boxplot()
  plot +   stat_summary(fun.y=mean, geom="point", color = "red", size=2)
  ggsave("ggplot_wide.png", width = 15)
  
sum(master_df$count)

#### ***********************************************************************

#three_attach_sum <- aggregate(col_new$counts, by=list(collapsed_stem_prop2$stem == "attach"), sum)
#aggregate(collapsed_stem_prop2[,sapply(df,is.numeric)],collapsed_stem_prop2["stem"],sum)

totalstem <- aggregate(counts~stem,collapsed_stem_prop2,sum)
names(totalB)[2] <- 'totalB'