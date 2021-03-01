```{r}
library(childesr)
library(wordbankr)
```

annotations<- read.csv("/Users/lscpuser/Documents/fyssen-project/Childes_corpora_Clara.csv")
annotations<- rename(annotations, corpus_name = Corpus)
annotations<- rename(annotations, corpus_name = Corpus)


d_transcripts <- get_transcripts()
head(d_transcripts)

mergedChiAnnot <- annotations %>% left_join(d_transcripts)

if ( length(unique(mergedChiAnnot$corpus_name)) < nrow(annotations) ) {
corpora_not_merged <- nrow(annotations) - length(unique(mergedChiAnnot$corpus_name))
print(corpora_not_merged)}


if (mergedChiAnnot$Inclusion=="yes"){
mergedbyLang<- mergedChiAnnot %>% group_by(Language) %>% nest()}
mergedbyLang<-mergedbyLang[!(is.na(mergedbyLang$Language) | mergedbyLang$Language=="" ), ]

mergedbyLang_ <- mergedbyLang %>% mutate (count_target_child = map(.x=data, .f=~length(unique(.x$target_child_id)), na.rm = T))
mergedbyLang_<-mergedbyLang_[!(is.na(mergedbyLang_$count_target_child) | mergedbyLang_$count_target_child=="" ), ]
mergedbyLang_<-mergedbyLang_ %>% unnest(count_target_child)
mergedbyLang_ <- mergedbyLang_ %>% mutate (corpus_id = map(.x=data, .f=~length(unique(.x$corpus_id)), na.rm = T))
mergedbyLang_<-mergedbyLang_ %>% unnest(corpus_id)


ggplot(mergedbyLang_, aes(y=Language, x=count_target_child)) + geom_point()
ggplot(mergedbyLang_, aes(y=Language, x=corpus_id)) + geom_point()

ggplot(mergedbyLang_, aes(y=Language, x=count_target_child)) + geom_point()



#target_child_id, target_child_age}
