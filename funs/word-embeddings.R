library(fastrtext)
library(tidyverse)

# Creating word embeddings | for recipe 3

fast_text_model_path <- "data/fasttext-model.bin"
fasttext_model <- load_model(fast_text_model_path)

word_embeddings <- tibble(word = get_dictionary(fasttext_model))

word_vecs <- get_word_vectors(fasttext_model)

word_embeddings <- word_embeddings %>% 
  bind_cols(word_vecs)

names(word_embeddings) <- c("word", paste0("v", sprintf("%03d", 1:300)))

write_rds(word_embeddings, file = paste0("data/word_embeddings.rds"))