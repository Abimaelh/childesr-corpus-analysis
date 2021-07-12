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
setwd("C:\\Users\\abima\\Desktop\\corp-an\\wide predicate search")

wide_sym_list <- read.csv(file = "C:\\Users\\abima\\Desktop\\corp-an\\wide predicate search\\wide_sym_list.csv", header = TRUE)

# 3-year-olds ****************************************************************************************************************************************
three_year_old_wide_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(36, 48),
  token = wide_sym_list$Predicate
)

three_wide_preprocess_df <- three_year_old_wide_df %>% arrange(target_child_id)
three_wide_select_df <- select(three_wide_preprocess_df, 'target_child_id', 'corpus_name', 'target_child_age',
                                     'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')
names(three_wide_select_df)[names(three_wide_select_df) == "part_of_speech"] <- "pos"
names(three_wide_select_df)[names(three_wide_select_df) == "gloss"] <- "form"

detach(package:plyr)
three_wide_counts <- three_wide_select_df %>% group_by(stem, pos) %>%
  summarize(count = sum(unique(length(stem))))

three_sym_not_found <- wide_sym_list %>% filter(!Predicate %in% three_wide_counts$stem)
three_sym_not_found['pos'] <- NA
three_sym_not_found['count'] <- NA
three_sym_not_found$count[is.na(three_sym_not_found$count)] <- 0
names(three_sym_not_found)[names(three_sym_not_found) == "Predicate"] <- "stem"

three_wide_full <- rbind(three_wide_counts, three_sym_not_found)

three_wide_verbs <- three_wide_full %>% filter(pos == 'v')

three_wide_verbs_factor <- three_wide_verbs
three_wide_verbs_factor$count <- as.factor(three_wide_verbs_factor$count)
three_wide_verbs_factor$pos <- NULL
three_wide_verbs_factor$stem <- as.factor(three_wide_verbs_factor$stem)


plot <- ggplot(three_wide_verbs_factor, aes(x=stem, y=count)) +
  geom_boxplot() + 
  stat_summary(fun = mean, geom="point", color = "red", size=2) +
  xlab("stem")#+
  #ylim(0,100)
plot + theme(legend.position = "none")

#write.csv(three_wide_full, "C:\\Users\\abima\\Desktop\\corp-an\\wide predicate search\\three_wide_full.csv")

# 4-year-olds ********************************************************************************************************************************************
four_year_old_wide_df <- get_tokens(
  collection = "Eng-NA",
  role = "target_child",
  age = c(48, 60),
  token = wide_sym_list$Predicate
)

four_wide_preprocess_df <- four_year_old_wide_df %>% arrange(target_child_id)
four_wide_select_df <- select(four_wide_preprocess_df, 'target_child_id', 'corpus_name', 'target_child_age',
                               'gloss', 'part_of_speech', 'stem','target_child_sex','utterance_id')
names(four_wide_select_df)[names(four_wide_select_df) == "part_of_speech"] <- "pos"
names(four_wide_select_df)[names(four_wide_select_df) == "gloss"] <- "form"

detach(package:plyr)
four_wide_counts <- four_wide_select_df %>% group_by(stem, pos) %>%
  summarize(count = sum(unique(length(stem))))

four_sym_not_found <- wide_sym_list %>% filter(!Predicate %in% four_wide_counts$stem)
four_sym_not_found['pos'] <- NA
four_sym_not_found['count'] <- NA
four_sym_not_found$count[is.na(four_sym_not_found$count)] <- 0
names(four_sym_not_found)[names(four_sym_not_found) == "Predicate"] <- "stem"

four_wide_full <- rbind(four_wide_counts, four_sym_not_found)

write.csv(four_wide_full, "C:\\Users\\abima\\Desktop\\corp-an\\wide predicate search\\four_wide_full.csv")
