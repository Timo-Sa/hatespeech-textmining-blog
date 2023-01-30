suppressPackageStartupMessages(library(tidymodels))
library(remoji)

# lambda grid | für model 1
lambda_grid <- grid_regular(penalty(), levels = 30)

# Liste negativer Emojis | für recipe 2
wild_emojis <- 
  c(emoji(find_emoji("gun")),
    emoji(find_emoji("bomb")),
    emoji(find_emoji("fist")),
    emoji(find_emoji("knife"))[1],
    emoji(find_emoji("ambulance")),
    "😠",
    "👹",
    "💩",
    "☠",
    "🖕",
    emoji(find_emoji("middle finger")),
    "😡",
    "🤢",
    "🤮",
    "😖",
    "😣",
    "😩",
    "😨",
    "😝",
    "😳",
    "😬",
    "😱",
    "😵",
    "😤",
    "🤦‍♀️",
    "🤦‍♂️")


# Verarbeitete Wordembeddings laden
word_embeddings <- readRDS("data/word_embeddings.rds")


# Funktion, um Wörter in Strings zu Zählen | recipe 2 & 3

count_lexicon <- function(txt, lexicon){
  
  txt <- tolower(txt)
  lexicon <- tolower(lexicon)
  lexicon_regex <- paste0("^", lexicon, "$", collapse = "|")
  string_in_words <- unlist(str_split(txt, pattern = boundary("word"))) 
  pattern_detected_in_string_count <- sum(str_detect(string_in_words, pattern = lexicon_regex))
  return(pattern_detected_in_string_count)
}


# Sentiment berechnen | recipe 2 & 3

calculate_sentiment <- function(txt, sentiws) {
  txt <- tolower(txt)
  sentiws$word <- tolower(sentiws$word)
  words <- unlist(str_split(txt, pattern = boundary("word"))) 
  sentiment_score <- sum(sentiws[sentiws[["word"]] %in% words, "value"], na.rm = TRUE) 
  return(sentiment_score)
}
