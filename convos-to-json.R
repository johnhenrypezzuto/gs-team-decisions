# convert convos in json
library(tidyverse)
library(jsonlite)
library(haven)
library(glue)
library(babynames)

set.seed(42)

n_names = 500

# master name assignment db based on membernum and gender 
names_df <- tibble(pos = rep(seq(1,3), 3), 
                   convo = c(rep("convo1", 3), rep("convo2", 3), rep("convo3", 3)),
                   male_name= c("Michael",
                                "Christopher",
                                "Matthew",
                                "Joshua",
                                "Jacob",
                                "Nicholas",
                                "Andrew",
                                "Daniel",
                                "Tyler"), 
                   female_name = c("Jessica",
                                   "Ashley",
                                   "Emily",
                                   "Sarah",
                                   "Samantha",
                                   "Amanda",
                                   "Brittany",
                                   "Elizabeth",
                                   "Megan")
                   ) %>% 
  pivot_longer(cols = male_name:female_name, names_to = "gender") %>% 
  pivot_wider(id_cols = c(pos, gender), names_from = convo, values_from = value) %>% 
  mutate(female = case_when(gender == "male_name" ~ 0, 
                            gender == "female_name" ~ 1)) %>% 
  rename(membernum = pos)

convo <- read_dta("/Users/jhap/Dropbox (Harvard University)/katie/Consideration of Ideas/Data/Chat Data.dta")



## if I use membernum here it will always be 1, 2, 3
gender <- read_dta("/Users/jhap/Dropbox (Harvard University)/katie/Consideration of Ideas/Data/main_data_reshaped_kyra.dta") %>% 
  distinct(unique_group, subject, female, membernum) %>% 
  left_join(names_df) %>%  # each membernum gets assigned 3 names for each convo
  left_join(names_df %>% 
              select(-gender) %>% 
              mutate(female = 1 - female) %>%  # reverse gender and join again
              rename_at(vars(starts_with("convo")), function(x){str_c(x, "_rev")})
            )



topic_df <- tibble(question = c(3, 8, 5, 2, 4, 1, 7, 6), 
                   topic = c("Onions", "Laundry", "Fire", "Hot or Cold", "Shoes", "Bar", "Manly", "Name"))



# 
convo_clean <- 
  convo %>%
    select(unique_group, subject, chatentry, secondsintochat, question) %>% 
    left_join(select(gender, subject, unique_group, female, membernum)) %>% # add female /membernum column
    drop_na(chatentry, subject) %>% 
    group_by(unique_group) %>% 
    mutate(subject = dense_rank(subject)) %>%  # renumber Member 1:3
    left_join(topic_df) %>% # import topics
  arrange(topic, unique_group, secondsintochat)
  

convo_prep <-
  convo_clean %>%
  mutate(
    subject = glue("<b>Member {subject}: </b>"), # bold the subject thing
    chatentry = str_replace_all(chatentry, "\\\\q.*?\\\\q", # fix quote glitch
                                function(m) paste0("<q>", gsub("\\\\q", "", m), "</q>")),
    chatentry = str_c(chatentry, "<br>")
  )



### create name df for convo mapping
names_df
convo_clean %>% 
  group_by(topic, unique_group) %>% 
  mutate(subject = str_c("Member ", subject)) %>% 
  distinct(unique_group, female, subject, membernum) %>% 
  left_join(select(gender, unique_group, membernum, starts_with("convo"))) %>% 
  select(subject, starts_with("convo"), everything()) %>% 
  summarise(subject = list(subject),
            name1 = list(convo1),
            name2 = list(convo2),
            name3 = list(convo3),
            name_rev1 = list(convo1_rev),
            name_rev2 = list(convo2_rev),
            name_rev3 = list(convo3_rev),
            female = list(female)) %>% 
  toJSON(.) %>% 
  write("convo/pseudonyms-subject-match.json")


### export anon convos as json, one group per row
convo_prep %>% 
  group_by(topic, unique_group) %>% 
  mutate(chatentry = str_c(subject, chatentry)) %>% 
  summarise(chat = paste0(chatentry, collapse = "")) %>% 
  toJSON(.) %>% 
  write("convo/chat-anon.json")


### export gender convos as json, one group per row
convo_prep %>% 
  left_join(select(gender, "unique_group", "membernum", convo1:convo3), 
            by = c("unique_group", "membernum")) %>% # import names
  group_by(topic, unique_group) %>% 
  mutate_at(vars(convo1:convo3), ~str_c("<b>", ., ": </b>")) %>% # format names
  mutate(chatentry1 = str_c(convo1, chatentry), # add names to convos
         chatentry2 = str_c(convo2, chatentry),
         chatentry3 = str_c(convo3, chatentry)) %>% 
  summarise(chat1 = paste0(chatentry1, collapse = ""), # collapse convos
            chat2 = paste0(chatentry2, collapse = ""),
            chat3 = paste0(chatentry3, collapse = "")) %>% 
  toJSON(.) %>% 
  write("convo/chat-gender.json")

### export reversed gender convos as json  
convo_prep %>% 
  left_join(select(gender, "unique_group", "membernum", convo1_rev:convo3_rev), # we import the reversed names
            by = c("unique_group", "membernum")) %>% # import names
  group_by(topic, unique_group) %>% 
  mutate_at(vars(convo1_rev:convo3_rev), ~str_c("<b>", ., ": </b>")) %>% 
  mutate(chatentry1 = str_c(convo1_rev, chatentry), 
         chatentry2 = str_c(convo2_rev, chatentry),
         chatentry3 = str_c(convo3_rev, chatentry)) %>% 
  summarise(chat1 = paste0(chatentry1, collapse = ""),
            chat2 = paste0(chatentry2, collapse = ""),
            chat3 = paste0(chatentry3, collapse = "")) %>% 
  toJSON(.) %>% 
  write("convo/chat-gender-rev.json")



### get convo IDs for selecting groups
convo_clean %>% 
  ungroup() %>% 
  distinct(unique_group, topic) %>% 
  mutate(n = row_number() - 1) %>% 
  group_by(topic) %>% 
  filter(n == min(n) | n == max(n))