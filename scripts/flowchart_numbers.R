#Script for flowchart numbers Short report titled "XX"
#authors : Georgia Loukatou | Alex Cristia | Naomi Havron| Camila Scaff 
#last date: "19/02/2022"
#RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for macOS Mozilla/5.0 (Macintosh; Intel Mac OS X 11_6_1) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.10 Chrome/69.0.3497.128 Safari/537.36
#R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"
#Using current database version: '2021.1' of CHILDES

## LOAD PACKAGES ####
library(dplyr)
library(readxl)
library(stringr)
library(childesr)


## READ IN DATA AND ORGANIZE ####
rm(list=ls()) #clean your environment
# Read in data

##1.CHILDES annotations ####
annotations<- read.csv("data/Table for authors - annotations.csv") 
#original link https://docs.google.com/spreadsheets/d/1s-ytfQf7WsZFDDZ6QkOQnZhTodHpQp7D2Wja8YFvjQY/edit?usp=sharing

#some cleaning  of the col names
colnames(annotations)=gsub("\\.\\..*","",colnames(annotations))
colnames(annotations)[colnames(annotations)=="Number.of.participants"]<-"Nb.of.participants"
colnames(annotations)[colnames(annotations)=="Language.or.Languages.spoken.in.recordings..be.specific.if.possible.e.g..French.Quebec."]<-"Language"
colnames(annotations)[colnames(annotations)=="Language.or.Languages.spoken.in.recordings..be.specific.if.possible.e.g..French.Quebec."]<-"Language"
colnames(annotations)[colnames(annotations)=="Location..Neighbourhood..village.city..province..state..country."]<-"Location"
colnames(annotations)[colnames(annotations)=="X..of.children.with.siblings"]<-"Nb.of.children.with.siblings"
colnames(annotations)[colnames(annotations)=="X..of.children.with.older.siblings"]<-"Nb.of.children.with.older.siblings"
colnames(annotations)[colnames(annotations)=="Our.coding.of"]<-"Our.coding.of.community.type"

#2.CHILDES information about corpora
total_corpus <- read.csv("data/Childes_corpora - Total CHILDES.csv")
#Merge
merge(x= total_corpus,y = annotations, by.x= "Corpus", by.y= "Corpus", all.x = T)-> all


#Corpus
annotations$Corpus <- as.factor(annotations$Corpus) #310 levels -- repeated corpora name
all <- all %>%  #create numbered first column
  mutate(number = 1:n()) %>% 
  select(number, everything()) 
#Creating unique key for each row 
all$key=paste0(all$number,"_",all$Corpus)

#status
xtabs(~status, all)  #excluded 109 include 339 

##7th Cluster: Reliability ####
#Inclusion
annotations$Inclusion[annotations$Inclusion %in% c("No","no")] <- "exclude"
annotations$Inclusion[annotations$Inclusion %in% c("Yes","yes")] <- "include"
#why_exclude
excluded <-  subset(annotations, (Inclusion %in% c("exclude")))
xtabs(~why_exclude, excluded)


#Who checked column
#Comment

#Reason for exclusion
#