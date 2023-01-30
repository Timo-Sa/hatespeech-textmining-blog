source("funs/utility.R")

# Rezept 1
# Wenige steps als Basisrezept mit tf-idf
# Tokenisierung, Stopwords, Stemming, Tokenfilter, Prädiktoren normalisieren

def_recipe1 <- function(data_train) {

  d_rec <- data_train %>% select(text, c1, id)
  
  recipe(c1 ~ ., data = d_rec) %>% 
    update_role(id, new_role = "id") %>% 
    step_tokenize(text) %>% 
    step_stopwords(text, language = "de", stopword_source = "snowball") %>% 
    step_stem(text) %>% 
    step_tokenfilter(text, max_tokens = 1e2) %>% 
    step_tfidf(text) %>% 
    step_normalize(all_numeric_predictors())

}

# Rezept 2
# Weitere Prädiktoren hinzufügen, mit ft-idf
# Rezept 1 + Schimpfwörter, Emojis, negative Emojis, emotionalität der Wörter, Sentiment

def_recipe2 <- function(data_train) {
  
  sentiws <- read.csv("data/sentiws.csv")
  profanities <- read.csv("data/schimpfwoerter.csv")
  emojis <- emoji(list_emoji())
  
  d_rec <- data_train %>% select(text, c1, id)
  
  recipe(c1 ~ ., data = d_rec) %>% 
    update_role(id, new_role = "id") %>%
    step_mutate(text_copy = text) %>% 
    step_mutate(profanities_n = map_int(text_copy, ~ count_lexicon(.x, profanities$word))) %>% 
    step_mutate(emojis_n = map_int(text_copy, ~ count_lexicon(.x, emojis))) %>% 
    step_mutate(wild_emojis_n = map_int(text_copy, ~ count_lexicon(.x, wild_emojis))) %>% 
    step_mutate(emotional_n = map_int(text_copy, ~ count_lexicon(.x, sentiws$word))) %>% 
    step_mutate(sentiment = map_dbl(text_copy, ~ calculate_sentiment(.x, sentiws))) %>% 
    step_textfeature(text_copy) %>%
    step_tokenize(text) %>% 
    step_stopwords(text, language = "de", stopword_source = "snowball") %>%
    step_stem(text) %>% 
    step_tokenfilter(text, max_tokens = 1e2) %>% 
    step_tfidf(text) %>% 
    step_normalize(all_numeric_predictors(), -starts_with("textfeature"), -ends_with("_n"))
  
}

# Rezept 3
# Rezept 2 mit Word embeddings statt tf-idf

def_recipe3 <- function(data_train) {
  
  sentiws <- read.csv("data/sentiws.csv")
  profanities <- read.csv("data/schimpfwoerter.csv")
  emojis <- emoji(list_emoji())
  
  d_rec <- data_train %>% select(text, c1, id)
  
  recipe(c1 ~ ., data = d_rec) %>% 
    update_role(id, new_role = "id") %>%
    step_mutate(text_copy = text) %>% 
    step_mutate(profanities_n = map_int(text_copy, ~ count_lexicon(.x, profanities$word))) %>% 
    step_mutate(emojis_n = map_int(text_copy, ~ count_lexicon(.x, emojis))) %>% 
    step_mutate(wild_emojis_n = map_int(text_copy, ~ count_lexicon(.x, wild_emojis))) %>% 
    step_mutate(emotional_n = map_int(text_copy, ~ count_lexicon(.x, sentiws$word))) %>% 
    step_mutate(sentiment = map_dbl(text_copy, ~ calculate_sentiment(.x, sentiws))) %>% 
    step_textfeature(text_copy) %>%
    step_tokenize(text) %>% 
    step_stopwords(text, language = "de", stopword_source = "snowball") %>%
    step_stem(text) %>% 
    step_tokenfilter(text, max_tokens = 1e2) %>% 
    step_word_embeddings(text, embeddings = word_embeddings) %>% 
    step_normalize(all_numeric_predictors(), -starts_with("textfeature"), -ends_with("_n"))
  
}
