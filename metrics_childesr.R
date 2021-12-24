library(childesr)
library(tidyverse)
library(koRpus)
library(koRpus.lang.fr)

############################ variables

#lang="fra"
#included_corpora=
#dan, eng, deu, ita, ell, nor, por, rus, spa, swe, tur, hrv, ces, zho, yue, heb, kor, cat, nld, hun, ind, ron, tam, afr, ara, eus, isl, gle, pol, srp, cym, est, gla, glg, haw, jam, jpn, jav, kik, xmm, mar, nan, esk, fas, que, slv, sot, tgl, taq, tha, und, yuw, yid
corpus_args <- list(corpus = included_corpora, token = "*")

############################ metric functions

compute_mlu <- function(metric_data) {
  print("Computing mean utterance length...")
  metric_data |> group_by(transcript_id, speaker_role) |> summarise(mlu = mean(utterance_length))
}

compute_count <- function(metric_data) {
  print("Computing count...")
  metric_data |> group_by(transcript_id, speaker_role) |> count(token, name = "count")
}

compute_duration <- function(metric_data){
  print("Computing duration...")
  metric_data |>
    filter(!is.na(media_start), !is.na(media_end)) |>
    group_by(transcript_id) |>
    mutate(start=min(media_start), end=max(media_end)) |>
    mutate(duration=end-start) |>
    select(transcript_id, duration) |>
    distinct()
}

MATTR <- function(txt, window=100, char=FALSE, ...){
  if(isTRUE(char)){
    char.value <- "MATTR"
  } else {
    char.value <- c()
  }
  results <- lex.div(txt=txt, window=window, measure="MATTR", char=char.value)
  return(results)
}

MATTR_wrapper <- function(text_){
  total_words = sapply(strsplit(text_, " "), length)
  if (total_words >100){
    text_obj <- tokenize(text_, format="obj", lang="fr") #not all languages -  TODO check how to do without, for now commented
    ttr_<- MATTR(text_obj, window=100)[1]
    return(ttr_)
  }
  else {
    ttr_<- NA
    return(ttr_)
  }
}

compute_mattr <- function(metric_data) {
  print("Computing mattr...")
  metric_data |> group_by(transcript_id, speaker_role) |>
    mutate(text_block = paste0(utterance, collapse = " ")) |>
    select(transcript_id, speaker_role, text_block) |>
    distinct() |>
    mutate(mattr=MATTR_wrapper(text_block))
}

########################### main functions

get_childes_data <- function(childes_lang ="fra", corpus_args) { #gets data from childes
  utterances <- get_utterances(language = childes_lang,
                               corpus = corpus_args$corpus)
  tokens <- get_tokens(language = childes_lang,
                       corpus = corpus_args$corpus,
                       token = corpus_args$token)
  return(list("utterances" = utterances, "tokens" = tokens))
}


get_childes_metrics <- function(childes_data) { #computes metrics on childes data

  utterances <- childes_data$utterances |>
    mutate(gloss = tolower(gloss)) |>
    select(utterance_id = id, utterance = gloss, utterance_length = num_tokens, transcript_id, speaker_role, media_start, media_end)

  tokens <- childes_data$tokens |>
    filter(gloss != "") |>
    mutate(gloss = tolower(gloss))|>
    select(token_id = id, token = gloss, token_stem = stem, token_order,
           token_phonemes = actual_phonology, utterance_id, transcript_id, speaker_role)

  metric_tokens <- compute_count(tokens) |>
    summarize(token_sum = sum(count),
              type_sum = n()) |>
    distinct()

  metric_utts <- compute_mlu(utterances) |>
    full_join(compute_duration(utterances)) |>
    #full_join(compute_mattr(utterances)) |>
    distinct()

  return(metric_tokens |>
           full_join(metric_utts))
}

###############################

transcripts <- get_transcripts()

childes_data <- get_childes_data(corpus_args) #lang, 

childes_metrics <- get_childes_metrics(childes_data) |>
  left_join(transcripts)


write.csv(childes_metrics,"childes_metrics.csv")

d_participants <- get_participants()

write.csv(d_participants,"childes_participants.csv")






