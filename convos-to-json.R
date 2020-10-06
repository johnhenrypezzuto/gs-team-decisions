# convert convos in json
library(tidyverse)
library(jsonlite)
library(haven)
library(glue)
library(babynames)

set.seed(42)

n_names = 500

names_df <- bind_rows(
  babynames %>% 
    filter(sex == "F",
           year > 1980) %>%
    group_by(name, sex) %>% 
    summarise(prop = sum(prop)) %>% 
    arrange(desc(prop)) %>% 
    head(n_names) %>% 
    ungroup() %>% 
    transmute(female = 1,
              name,
              n = row_number()),
    babynames %>% 
    filter(sex == "M",
           year > 1980) %>%
    group_by(name, sex) %>% 
    summarise(prop = sum(prop)) %>% 
    arrange(desc(prop)) %>% 
    head(n_names) %>% 
    ungroup() %>% 
    transmute(female = 0,
              name,
              n = row_number())
)
convo <- read_dta("/Users/jhap/Dropbox (Harvard University)/katie/Consideration of Ideas/Data/Chat Data.dta")

## if I use membernum here it will always be 1, 2, 3
gender <- read_dta("/Users/jhap/Dropbox (Harvard University)/katie/Consideration of Ideas/Data/main_data_reshaped_kyra.dta") %>% 
  distinct(unique_group, subject, female) %>% 
  mutate(n = as.integer(runif(nrow(.), min = 1, max = n_names))) %>% 
  left_join(names_df) %>% 
  select(-n)



topic_df <- tibble(question = c(3, 8, 5, 2, 4, 1, 7, 6), 
                   topic = c("Onions", "Laundry", "Fire", "Hot or Cold", "Shoes", "Bar", "Manly", "Name"))




convo_clean <- 
convo %>%
  select(unique_group, subject, chatentry, secondsintochat, question) %>% 
  drop_na(chatentry, subject) %>% 
  group_by(unique_group) %>% 
  left_join(gender) %>% # import names
  mutate(subject = dense_rank(subject), # renumber Member 1:3
         subject = glue("<b>Member {subject}: </b>"),
         name = glue("<b>{name}: </b>"),
         chatentry = str_c(chatentry, "<br>")) %>%  # bold the subject thing
  arrange(unique_group, secondsintochat) %>% 
  left_join(topic_df) # import topics
  


### export anon convos as json  
convo_clean %>% 
  group_by(unique_group, topic) %>% 
  mutate(chatentry = str_c(subject, chatentry)) %>% 
  summarise(chat = paste0(chatentry, collapse = "")) %>% 
  toJSON(.) %>% 
  write("convo/chat-anon.json")


### export gender convos as json  
convo_clean %>% 
  group_by(unique_group, topic) %>% 
  mutate(chatentry = str_c(name, chatentry)) %>% 
  summarise(chat = paste0(chatentry, collapse = "")) %>% 
  toJSON(.) %>% 
  write("convo/chat-gender.json")