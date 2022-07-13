library(tidyverse)
library(tidytext)
opd_survey <- read_csv("lab-1/data/opd_survey.csv")
opd_survey 
glimpse(opd_survey)
write_csv(opd_survey, "lab-1/data/opd_survey_copy.csv")
opd_selected <- select(opd_survey, Role, Resource...6, Q21)
opd_renamed <- rename(opd_selected, text = Q21)
opd_sliced <- slice(opd_renamed, -1, -2) # the - sign indicates to NOT keep rows 1 and 2

head(opd_sliced)
opd_complete <- na.omit(opd_sliced)
opd_teacher <- filter(opd_complete, Role == "Teacher")

head(opd_teacher)

opd_tidy <- unnest_tokens(opd_teacher, word, text)

head(opd_tidy)
opd_tidy <- opd_survey %>%
  select(Role, Resource...6, Q21) %>%
  rename(text = Q21) %>%
  slice(-1, -2) %>%
  na.omit() %>%
  filter(Role == "Teacher") %>%
  unnest_tokens(word, text)

head(opd_tidy)
head(stop_words)
view(stop_words)
opd_clean <- anti_join(opd_tidy, stop_words)
head(opd_clean)
opd_counts <- count(opd_clean, word, sort = TRUE)
opd_counts <- opd_clean %>% 
  count(word, sort = TRUE)

opd_counts

opd_resource_counts <- opd_clean %>%
  count(Resource...6, word, sort = TRUE)

view(opd_resource_counts)

opd_frequencies <- opd_clean %>%
  count(Resource...6, word, sort = TRUE) %>%
  group_by(Resource...6) %>%
  mutate(proportion = n / sum(n))

opd_frequencies

opd_words <- opd_teacher %>%
  unnest_tokens(word, text) %>%
  count(Resource...6, word, sort = TRUE)

head(opd_words)

total_words <- opd_words %>%
  group_by(Resource...6) %>%
  summarise(total = sum(n))

total_words
opd_totals <- left_join(opd_words, total_words)
opd_totals

opd_tf_idf <- opd_totals %>%
  bind_tf_idf(word, Resource...6, n)

opd_tf_idf
view(opd_tf_idf)
opd_quotes <- opd_teacher %>%
  select(text) %>% 
  filter(grepl('online', text))

view(opd_quotes)

sample_n(opd_quotes, 20)

opd_quotes <- opd_teacher %>%
  select(text) %>% 
  filter(grepl('inform*', text))

view(opd_quotes)

sample_n(opd_quotes, 20)

library(wordcloud2)

wordcloud2(opd_counts)
opd_counts %>%
  filter(n > 500) %>% # keep rows with word counts greater than 500
  mutate(word = reorder(word, n)) %>% #reorder the word variable by n and replace with new variable called word
  ggplot(aes(n, word)) + # create a plot with n on x axis and word on y axis
  geom_col() # make it a bar plot


library(forcats)

opd_frequencies %>%
  filter(Resource...6 != "Calendar") %>% # remove Calendar responses, too few. 
  group_by(Resource...6) %>%
  slice_max(proportion, n = 5) %>%
  ungroup() %>%
  ggplot(aes(proportion, fct_reorder(word, proportion), fill = Resource...6)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~Resource...6, ncol = 3, scales = "free")
