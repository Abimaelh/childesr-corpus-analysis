#load the library
library(childesr)
library(dplyr)
library(ggplot2)
library(tidyverse)
library(tidyboot)
library(magrittr)
library(tidytext)
library(plyr)

setwd("C:\\Users\\abima\\Desktop\\corp-an")
getwd()

sym_list <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\sym_list3.csv", header = TRUE)
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
  age = c(36, 60),
  token = sym_list$form
)

length(unique(tokens_main_df$target_child_id)) #Great! 54 unique children just like in the get_types DF.
# now 102 children

#lets store these unique ID's in case we need them for later.
tokens_unique_ID <- unique(tokens_main_df$target_child_id)
class(tokens_unique_ID) #checking the class.
#or as a df
tokens_unique_ID_DF <- as.data.frame(tokens_unique_ID)

# 2. Do the same for the get_types function and compare the lengths of each. We should have the same number of unique IDs.

types_main_df <- get_types(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 60),
  type = sym_list$form
)
## can you extend the age range?

length(unique(types_main_df$target_child_id)) #102


# storing the types unique ids
types_unique_ID <- unique(types_main_df$target_child_id) #storing the unique ids as a variable
types_unique_ID_DF <- as.data.frame(types_unique_ID) # as a df

## how many totally tokens are we hoping to capture?

length(tokens_main_df$gloss) # 2592 tokens to capture.

#confirming that with the counts in get_types()

sum(types_main_df$count) # yup 2592 here too.

# First, get the counts for each stem.

#### Next, we can use dplyr to arrange the data by the unique ids
arranged_ID_main_token_df <- tokens_main_df %>% arrange(target_child_id)

#summing the token gloss 
arranged_gloss_count_sum <- arranged_ID_main_token_df %>% group_by(gloss) %>%
  summarize(count = sum(unique(length(gloss)))) #produces count column

sum(arranged_gloss_count_sum$count) #2592 GOOD

#YES but I want to do this for every target_child_ID
clean_token_df <- select(arranged_ID_main_token_df, 'target_child_id', 'corpus_name', 'target_child_age',
                         'gloss', 'part_of_speech', 'stem','target_child_sex')
clean_token_df

#for loop time

##skip this************************************************************************
#token counts per child for each stem. ## DIDNT WORK
for (i in clean_token_df$target_child_id) {
  output <- clean_token_df %>% group_by(gloss, i) %>%
    summarize(count = sum(unique(length(gloss))))
  
}

sum(output$count)# equals to 1037!

arranged_tokens <- output %>% arrange(target_child_id) #arranging the columns by target_child_id

# now I want to be able to get 0 for the words they don't produce.

## *************************** CHECKING DATA FOR ONE PERSON **********************************
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
## ***********************END CHECKING DATA FOR ONE PERSON ***************************************


### ******************** REPEAT THIS FOR YOUR FULL DF

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

#### **************** REPEAT ABOVE FOR FULL_DF ****************

## what iF you just merge your cleaned_DF with the sym_list?

all_children_collapsed <- clean_token_df %>% group_by(target_child_id,target_child_age,target_child_sex,corpus_name,part_of_speech,gloss) %>%
  summarize(count = sum((unique(length(gloss)))))


# The above code is not summing together each unique gloss...Hmmm. It could be because of the small age differences. You are better

# convert months to years.
all_children_collapsed2 <- clean_token_df

#FOR 3 YEAR OLDS
all_children_collapsed2$target_child_age <-  replace(all_children_collapsed2$target_child_age,
                                                all_children_collapsed2$target_child_age >= 36.00 &
                                                all_children_collapsed2$target_child_age <= 47.99, 3)

#FOR 4 YEAR OLDS
all_children_collapsed2$target_child_age <-  replace(all_children_collapsed2$target_child_age,
                                                     all_children_collapsed2$target_child_age >= 48.00 &
                                                     all_children_collapsed2$target_child_age <= 59.99, 4)

# off converting age in months to year, so that you have the same data in each row. 

all_children_collapsed2 <- all_children_collapsed2 %>% group_by(target_child_id,target_child_age,target_child_sex,corpus_name,part_of_speech,gloss) %>%
  summarize(count = sum((unique(length(gloss)))))

length(unique(all_children_collapsed2$target_child_id)) #still 102, good.
sum(all_children_collapsed2$count) #still 2592, good.

## Now we have a DF that includes 4 year olds. If I merge this now with the sym_word DF. It will be hard
## to fill in the NA's since, I can't change the entire column to one ID when the value is equal to NA.

df_all_children <- merge(all_children_collapsed2, sym_words, all = TRUE)
# the problem with this is that now the IDS are not arranged, and I don't know when a child's ID
# starts or ends. I will need a for-loop to subset the data by ID, then merge with sym_words, and finally
# merges all my newly created DFs with each other into one main df. 

# **************************Adding target_child_id column to sym_words***************************

sym_words_id <- sym_words

unique_ids <- unique(all_children_collapsed2$target_child_id)
length(unique_ids)

for (i in 1:unique(unique_ids)) {
  sym_words_id['target_child_id'] = i
  do.call("rbind", replicate(102, sym_words_id[i], simplify = FALSE))
}

u_ids <- as.data.frame(unique_ids)

#this works great for creating 83 instances of a unique ID.
many_ids <- u_ids %>% slice(rep(1:n(), each = 83))
length(many_ids$unique_ids) #8466


# now to copy the character sym_words_id df

#class(sym_words_id$gloss)
#many_symwords <- coredata(sym_words_id)[rep(seq(nrow(sym_words_id)),102),]

n = 102
many_symwords2 <- do.call("rbind", replicate(n, sym_words_id, simplify = FALSE))
length(many_symwords2$gloss) #8466

bothdfs <- cbind(many_symwords2, target_child_id = many_ids$unique_ids)

ulti_df <- merge(all_children_collapsed2, bothdfs[, c("stem", "gloss", "target_child_id")], BY = "target_child_id", all = TRUE)

# im going to separate the 3 and 4 year olds again, and get tokens for each, and then merge the two dfs so I can get the correct age.
# but first I need to check how many observations we're left with after filtering pos tags.

filter_clean_token_df <- clean_token_df #2592 obs

target <- c("n", "v", "adj")
filter_clean_token_df <- filter(filter_clean_token_df, part_of_speech %in% target)
# after filtering we are left with 2068 observations.

# *********************************************************************************************************************

#now to get a clean token df for 3 year olds only.
three_year_olds_tokens_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = sym_list$form
)

three_year_olds_tokens_df <- three_year_olds_tokens_df %>% arrange(target_child_id)

three2_year_olds_tokens_df <- select(three_year_olds_tokens_df, 'target_child_id', 'corpus_name', 'target_child_age',
                         'gloss', 'part_of_speech', 'stem','target_child_sex')
length(unique(three2_year_olds_tokens_df$target_child_id)) #48 after filtering

#filter by pos
target <- c("n", "v", "adj")
three2_year_olds_tokens_df <- filter(three2_year_olds_tokens_df, part_of_speech %in% target) #872 rows
length(unique(three2_year_olds_tokens_df$target_child_id))
length(three2_year_olds_tokens_df$target_child_id)

#renaming gloss to form
names(three2_year_olds_tokens_df)[names(three2_year_olds_tokens_df) == "gloss"] <- "form"
three2_year_olds_tokens_df
#merge with sym_words_id (need to create this) must be 82 x 48 rows long.

#making df of unique ids in three year olds
ids_for_threes <- unique(three2_year_olds_tokens_df$target_child_id)
ids_for_threes <- as.data.frame(ids_for_threes)
length(unique(ids_for_threes$ids_for_threes)) #48 unique ids

#generate 82 instances of an ID
three_many_ids <- ids_for_threes %>% slice(rep(1:n(), each = 82))
length(three_many_ids$ids_for_threes) #3936
names(three_many_ids)[names(three_many_ids) == "ids_for_threes"] <- "unique_ids"
three_many_ids

#generate 54 instances of sym_words
n = 48
threes_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(threes_many_syms$form) #3936


# Merge many IDS with many syms
three_sym_and_id <- cbind(threes_many_syms, target_child_id = three_many_ids$unique_ids)#4428
length(three_sym_and_id$target_child_id) #3936


#merging three_sym_and_id with three_year_olds_token

#replacing age with 3
three2_year_olds_tokens_df$target_child_age <-  replace(three2_year_olds_tokens_df$target_child_age,
                                                       three2_year_olds_tokens_df$target_child_age >= 36.00 &
                                                      three2_year_olds_tokens_df$target_child_age <= 47.99, 3)


#before running this change the age of all children to 3! and then merge
detach(package:plyr)
three_counts <- three2_year_olds_tokens_df %>% group_by(form, target_child_id, target_child_age, target_child_sex) %>%
  summarize(count = sum(unique(length(form))))


three_counts <- three_counts %>% arrange(target_child_id)
length(unique(three_counts$target_child_id))

length(three_counts$form) #243
length(three_sym_and_id$form) #3936

three_full <- merge(three_counts, three_sym_and_id, all = TRUE)
three_full <- three_full %>% arrange(target_child_id)

length(unique(three_full$form))

length(three_full$form) #3936

#now go in and change nas for age to 3 and nas for count to 0

three_full$count[is.na(three_full$count)] <- 0
three_full$target_child_age[is.na(three_full$target_child_age)] <- 3
sum(three_full$count) #872! Yes. Matches three_year_olds_tokendf
length(unique(three_full$form))

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
                                    'gloss', 'part_of_speech','target_child_sex','stem')
length(unique(four_year_olds_tokens_df$target_child_id)) #63 unique before filtering , x after filtering

#filter by pos
target <- c("n", "v", "adj")
four2_year_olds_tokens_df <- filter(four2_year_olds_tokens_df, part_of_speech %in% target) #872 rows
length(unique(four2_year_olds_tokens_df$target_child_id)) #62 unique after filtering
length(four2_year_olds_tokens_df$target_child_id) #1196 unique tokens after filtering? - Yes

#renaming gloss to form
names(four2_year_olds_tokens_df)[names(four2_year_olds_tokens_df) == "gloss"] <- "form"
four2_year_olds_tokens_df
#merge with sym_words_id (need to create this) must be x rows long

#making df of unique ids in four year olds
ids_for_four <- unique(four2_year_olds_tokens_df$target_child_id)
ids_for_four <- as.data.frame(ids_for_four)
length(unique(ids_for_four$ids_for_four)) #62 unique ids

#generate 82 instances of an ID
four_many_ids <- ids_for_four %>% slice(rep(1:n(), each = 82))
length(four_many_ids$ids_for_four) #5084
names(four_many_ids)[names(four_many_ids) == "ids_for_four"] <- "unique_ids"
four_many_ids

#generate 62 (for each unique id) instances of sym_words
n = 62
four_many_syms <- do.call("rbind", replicate(n, sym_list, simplify = FALSE))
length(four_many_syms$form) #5084

# Merge many IDS with many syms
four_sym_and_id <- cbind(four_many_syms, target_child_id = four_many_ids$unique_ids)
length(four_sym_and_id$target_child_id) #5084

#merging three_sym_and_id with three_year_olds_token

#replacing age with 3

#peace <- four_year_olds_tokens_df
four2_year_olds_tokens_df$target_child_age <-  replace(four2_year_olds_tokens_df$target_child_age,
                                                       four2_year_olds_tokens_df$target_child_age >= 48.00 &
                                                        four2_year_olds_tokens_df$target_child_age < 60.00, 4)

#length(peace$target_child_id) #1196
#before running this change the age of all children to 4! and then merge
four_counts <- four2_year_olds_tokens_df %>% group_by(form, target_child_id, target_child_age, target_child_sex) %>%
  summarize(count = sum(unique(length(form))))

four_counts <- four_counts %>% arrange(target_child_id)

length(unique(four_counts$target_child_id)) #62

length(four_counts$target_child_id) #367
length(four_sym_and_id$form)#5084
length(unique(four_sym_and_id$target_child_id))

#stem_dropped <- subset(four_sym_and_id, select = -c(stem))

four_full <- merge(four_counts, four_sym_and_id, all = TRUE) #Original 
# Testing whether stem is creating the extra row. NO.
#four_full <- merge(four_counts, stem_dropped, all = TRUE)


# remove row 4589 ************************************************************************
four_full<- four_full[-c(4589),]



# testing whether sex is adding an extra row or age.
#sex_dropped <- subset(four_counts, select = -c(target_child_sex,target_child_age))

# nope, still one extra row.
#four_full <- merge(sex_dropped, stem_dropped, all = TRUE)

length(unique(four_full$form)) #82
length(unique(four_full$target_child_id)) #62

length(four_full$target_child_id) #5084
four_full <- four_full %>% arrange(target_child_id)

#now go in and change nas for age to 4 and nas for count to 0

four_full$count[is.na(four_full$count)] <- 0
four_full$target_child_age[is.na(four_full$target_child_age)] <- 4
sum(four_full$count) #1196 Yes. Matches four_year_olds_tokendf, ** 
#it should be 1195 because 'thought' was in four_year_old_token


sum(four_counts$count)
sum(length(four2_year_olds_tokens_df$form)) #1196 because this is without removing 'thought.'
#why is there one more row?

#length(four_full$form == "friend")
#count(four_full[1:100,], vars = "form")

length(unique(four_full$form))
length(unique(three_full$form))
# *****************************************************************************************
# now just combine four_full and three_full
# keep stem so that you can collapse across each stem. 
#three_full <- subset(three_full, select = -c(stem))

new <- rbind(three_full, four_full)

#check to make sure the # of tokens are the same. -1 tokens for the four year olds. maybe add the stems back so you can collapse?

sum(four_full$count)
sum(three_full$count)
#length(three_year_olds_tokens_df$form)
sum(new$count) #2067
# making sure the count for types is the same as the token length.

four_year_olds_types2_df <- get_types(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48, 60),
  type = sym_list$form
)
#types count
sum(four_year_olds_types2_df$count) #1555
#tokens count
sum(length(four_year_olds_tokens_df$gloss)) #1555

#after filtering pos - correct
sum(length(four2_year_olds_tokens_df$form)) #1196 again, because this one still has 'thought' in it.
# four_full is 1195

sum(three_full$count) #872

sum(new$count) # 2067. goood 872 + 1195.

three_year_olds_types2_df <- get_types(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  type = sym_list$form
)
#types count
sum(three_year_olds_types2_df$count) #1037
#tokens count
sum(length(three_year_olds_tokens_df$form)) #872 (after filtering?)

test_three <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = sym_list$form
)

sum(length(test_three$gloss)) #yes 1037. which means that after filtering, we lose about 165 tokens.

# ************************************* COLLAPSING ********************************************
col_new <- new %>% group_by(target_child_id, stem) %>%
  summarize(counts = sum(count))

sumcounts <- tapply(col_new$counts, col_new$target_child_id, sum)
sumcounts <- as.data.frame(sumcounts)
#How many kids do we have?
length(unique(col_new$target_child_id)) #96 children

#plyr option
library(plyr)
sumcounts2 <- ddply(col_new, .(target_child_id), summarise, totalcount = sum(col_new$count))

write.csv(new, "C:\\Users\\abima\\Desktop\\corp-an\\all_tokens.csv")

# *************** getting speaker statistics *********************
speaker_stats <- get_speaker_statistics(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 60),
)

# For 1741, 1627 + 2349 + 3488 + 3052 + 3133 + 2107 + 1715 = 17,471 tokens.

# compare this to the gloss count in get_tokens!
speaker_tokes <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 60),
  token = "*"
)
#17,471 length of tokens in get_tokens! They match up!

# Filter the data by target_child_id, create a list!, and subset the data based on this

all_ids2 <- as.data.frame(unique(col_new$target_child_id))
all_ids <- (unique(col_new$target_child_id))

length(unique(all_ids))
names(all_ids2)[names(all_ids2) == "unique(col_new$target_child_id)"] <- "target_child_id" 

names(all_ids)[names(all_ids) == "unique(col_new$target_child_id)"] <- "target_child_id" 

sub_speaker_stats <- speaker_stats

sub_speaker_stats <- select(sub_speaker_stats, "target_child_id", "num_tokens")

sub_speaker_stats2 <- filter(sub_speaker_stats, target_child_id %in% col_new$target_child_id) 

length(unique(sub_speaker_stats2$target_child_id)) # not all of the children have a token count. #49
# Will have to get them by counting the length of the gloss in get_types or get_tokens
# get_types would already come with a count, but no filtered by POS tho. So maybe use the filtered POS df to count.


#sub_speaker_stats <- sub_speaker_stats[sub_speaker_stats$target_child_id == all_ids,]
letmesee <- unique(sub_speaker_stats2$target_child_id)
letmesee
all_ids

speaker_types <- get_types(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 60),
  type = "*"
)

shem_stats <- select(speaker_types, 'target_child_id', 'count')
shem_stats <- shem_stats %>%
  filter(target_child_id == 1741)

sum(shem_stats$count)

all_types_count <- select(speaker_types, 'target_child_id', 'count')
all_types_count <- all_types_count %>%
  filter(target_child_id %in% all_ids)
length(unique(all_types_count$target_child_id))#49 still! Lets see if we can get a raw count for get_tokens by summing the length of the gloss.

#from the code above. 
speaker_tokes <- select(speaker_tokes, 'target_child_id', 'gloss')

shemy_tokes <- speaker_tokes
shemy_tokes <- shemy_tokes %>%
  filter(target_child_id %in% all_ids)
length(unique(shemy_tokes$target_child_id))
token_count <- as.data.frame(tapply(all_types_count$count, all_types_count$target_child_id, sum))
?tapply
token2_count <- aggregate(all_types_count$count, by=list(all_types_count$target_child_id), sum)
## *** comparing the match up with get_speaker_statistics + get_tokens ***

sub_speaker_stats <- speaker_stats

sub_speaker_stats <- select(sub_speaker_stats, "target_child_id", "num_tokens")

sub_speaker_stats2 <- filter(sub_speaker_stats, target_child_id %in% all_ids)
#
#
#
#
#we need to use the token counts for get_tokens - so we can filter out the words we don't want. - it will narrow down the kids we are using to hopefully 96!
#
#
#
names(token2_count)[names(token2_count) == "x"] <- "tokens" 
names(token2_count)[names(token2_count) == "Group.1"] <- "target_child_id" 

#arrange them first by id.
col_new <- col_new %>% arrange(target_child_id) #2208 rows
token2_count <- token2_count %>% arrange(target_child_id) #need this to be the same length as col_new

token2_sliced <- token2_count %>% slice(rep(1:n(), each = 23)) #2208 rows! great.

full_df <- cbind(col_new, token2_sliced)

full_df$target_child_id...4 <- NULL

names(full_df)[names(full_df) == "target_child_id...1"] <- "target_child_id"

full_df_prop <- transform(full_df, prop = counts / tokens)
write.csv(full_df_prop, "C:\\Users\\abima\\Desktop\\corp-an\\full_df_prop.csv")

length(unique(four_full$target_child_id))
length(unique(three_full$target_child_id))

length(unique(col_new$target_child_id))

(unique(four_full$target_child_id))
(unique(three_full$target_child_id))

repeats <- four_full$target_child_id[four_full$target_child_id %in% three_full$target_child_id]
length(unique(repeats))
repeats <- unique(repeats)
## In three_full and four_full you are getting 110 children. But when you rbind them to create df 'new' you get 96 children because some of the 3 year olds turned 4 and are also in
# the 4 year old df - with the same id. So rbind is just merging them together. there are 14 children that have data in both three_full and four_full. So you really
#The question is, how do we treat these date? Do we assign these children to only one age group or delete them from one group for the analysis?
# keep only the children from 3 year old group.

four_full # 5084 obs before removing the 14 children that are repeats.
four_full <- four_full[!(four_full$target_child_id == repeats),]

length(unique(four_full$target_child_id)) #62
length(unique(three_full$target_child_id)) #48
# this equals 110, but then when you merge them and create 'new' it becomes 96 because of the repeat subjects in
# 3 year olds and 4 year olds.
four2_full <- four_full


length(unique(four2_full$target_child_id)) #62
#four2_full <- subset(four_full, target_child_id != repeats2)
four2_full <- four2_full[! four2_full$target_child_id %in% repeats,]
length(unique(four2_full$target_child_id)) #14 something went wrong. 62 - 14 should equal 48
