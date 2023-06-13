#Script for reading the different tables of the Short report titled "XX"
#authors : Georgia Loukatou | Alex Cristia | Naomi Havron| Camila Scaff 
#last date: "12/11/2022"
#RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for macOS Mozilla/5.0 (Macintosh; Intel Mac OS X 11_6_1) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.10 Chrome/69.0.3497.128 Safari/537.36
#R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"
#Using current database version: '2021.1' of CHILDES

## LOAD PACKAGES ####
library(dplyr)
library(readxl)
library(stringr)
library(childesr)
library(ggplot2)
library(dichromat)

## READ IN DATA AND ORGANIZE ####
rm(list=ls()) #clean your environment
# Read in data

##1.CHILDES annotations ####
annotations_inc<- read.csv("derived/annotations_included", sep="")

#Education level info
education <- as.data.frame((table(annotations_inc$Education.ac, annotations_inc$country)))
names(education)<-c("Education.ac","country","Freq")
education_withdata <- subset(education, Freq > 0)   
write.table(education_withdata,"derived/education_withdata", row.names = FALSE, col.names = TRUE)

education_withdata<- read.csv("derived/education_withdata", sep="")
levels(as.factor(education_withdata$country))#28 countries
education_withdata$Education.ac = factor(education_withdata$Education.ac, levels=c("Some primary", "Some secondary", "Some college", "College and above"))

# Plot this categorywise bar chart
g <- ggplot(data=subset(annotations_inc, !is.na(Education.ac)), aes(country))
# g + geom_bar(aes(fill=Education.ac), width = 0.6, position = "fill") + 
#   theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() +
#   labs(title="Education of caregivers") 
g + geom_bar(aes(fill=Education.ac), width = 0.9) + 
  theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() + 
  #facet_grid(~ scenario_f) +
  theme_minimal()  +
  #ggtitle("Education of caregivers")  + 
  theme(plot.title = element_text(size = 10, face = "bold"),strip.text.x = element_text(size = 10)) +
  coord_flip() + 
  #guides(fill=FALSE ) + 
  scale_fill_manual(breaks=c("Some primary", "Some secondary", "Some college", "College and above"),values = c("#E6FFFF", "#99E6FF", "#4CA6FF", "#0040FF" )) +
  scale_x_discrete(name = "Number of corpus") +
  scale_y_continuous(name = "Countries") 
  labs(title="Education of caregivers") +  scale_y_continuous( breaks = seq(0,13, 1))


#SES level info
ses <- as.data.frame((table(annotations_inc$SES.STDZD, annotations_inc$country)))
names(ses)<-c("SES.STDZD","country","Freq")
ses_withdata <- subset(ses, Freq > 0)   
write.table(ses_withdata,"derived/ses_withdata", row.names = FALSE, col.names = TRUE)

ses_withdata<- read.csv("derived/ses_withdata", sep="")
levels(as.factor(ses_withdata$country))#35 countries

#Profession level info
teach <- as.data.frame((table(annotations_inc$is.teacher, annotations_inc$country)))
health <- as.data.frame((table(annotations_inc$is.health, annotations_inc$country)))
academ <- as.data.frame((table(annotations_inc$is.academic, annotations_inc$country)))

names(teach)<-c("teach","country","Freq_teach")
names(health)<-c("health","country","Freq_health")
names(academ)<-c("academ","country","Freq_acad")
merge(x=teach, y=health,  by = "country") -> temp
merge(x=temp, y=academ,  by = "country") -> profession
profession_withdata <-  profession[profession$Freq_teach>0 |  profession$Freq_health>0 | profession$Freq_acad>0,]
write.table(profession_withdata,"derived/profession_withdata", row.names = FALSE, col.names = TRUE)

profession_withdata<- read.csv("derived/profession_withdata", sep="")
levels(as.factor(profession_withdata$country))#24 countries

#household structure
household <- as.data.frame((table(annotations_inc$Household.structure, annotations_inc$country)))
names(household)<-c("Household.structure","country","Freq")
household_withdata <- subset(household, Freq > 0)   
write.table(household_withdata,"derived/household_withdata", row.names = FALSE, col.names = TRUE)

household_withdata<- read.csv("derived/household_withdata", sep="")
levels(as.factor(household_withdata$country))#28 countries
                     
#Percentage sibling
perc_sib <- as.data.frame((table(annotations_inc$Proportion.With.Siblings, annotations_inc$country)))
names(perc_sib)<-c("Proportion.With.Siblings","country","Freq")
perc_sib_withdata <- subset(perc_sib, Freq > 0)   
perc_sib_withdata$dummy_sib <- NA
perc_sib_withdata$dummy_sib[perc_sib_withdata$Proportion.With.Siblings==0]<-"none"
perc_sib_withdata$dummy_sib[perc_sib_withdata$Proportion.With.Siblings!=0]<-"sibs in corpus"


write.table(perc_sib_withdata,"derived/perc_sib_withdata", row.names = FALSE, col.names = TRUE)

perc_sib_withdata<- read.csv("derived/perc_sib_withdata", sep="")
levels(as.factor(perc_sib_withdata$country))#34 countries

#Language spoken
language <- as.data.frame((table(annotations_inc$Language, annotations_inc$country)))
names(language)<-c("Language","country","Freq")
language<- subset(language, Freq > 0)   
write.table(language,"derived/language", row.names = FALSE, col.names = TRUE)

language<- read.csv("derived/language", sep="")
levels(as.factor(language$country))#49 countries


#Lingual status
lingual_status <- as.data.frame((table(annotations_inc$Bilingualism.Multilingualism.in.corpus, annotations_inc$country)))
names(lingual_status)<-c("Bilingualism.Multilingualism.in.corpus","country","Freq")
lingual_status_withdata<- subset(lingual_status, Freq > 0)   
write.table(lingual_status_withdata,"derived/lingual_status_withdata", row.names = FALSE, col.names = TRUE)

lingual_status_withdata<- read.csv("derived/lingual_status_withdata", sep="")
levels(as.factor(lingual_status_withdata$country))#35 countries

#type of community
community <- as.data.frame((table(annotations_inc$Type.of.community, annotations_inc$country)))
names(community)<-c("Type.of.community","country","Freq")
community_withdata<- subset(community, Freq > 0)   
write.table(community_withdata,"derived/community_withdata", row.names = FALSE, col.names = TRUE)

community_withdata<- read.csv("derived/community_withdata", sep="")
levels(as.factor(community_withdata$country))#28 countries


## Figure
install.packages("ggplot2")
install.packages("maps")
install.packages("mapdata")
install.packages("geosphere")
install.packages("rgeos")

library(ggplot2)
library(maps)
library(mapdata)
library(geosphere)
library(rgeos)

country_coords <- data.frame(
  Country <- c("Argentina", "Austria", "Belgium", "Canada", "China", "Croatia", "Czech Republic",
             "Denmark", "Egypt", "Estonia", "France", "Germany", "Greece", "Hungary", "Iceland",
             "India", "Indonesia", "Iran", "Ireland", "Israel", "Italy", "Jamaica", "Japan",
             "Korea", "Kuwait", "Lesotho", "Mexico", "Netherlands", "Norway", "Papua New Guinea",
             "Poland", "Portugal", "Romania", "Russia", "Serbia", "Singapore", "Slovenia",
             "South Africa", "Spain", "Sweden", "Switzerland", "Taiwan", "Thailand", "Turkey",
             "United Kingdom", "United States", "Hong-Kong"),
  Latitude <- c(-34.6037, 47.5162, 50.5039, 56.1304, 35.8617, 45.8150, 49.8175, 55.6761, 26.8206,
                58.5953, 46.6034, 51.1657, 39.0742, 47.1625, 64.9631, 20.5937, -0.7893, 32.4279,
                53.3498, 31.0461, 41.8719, 18.1096, 36.2048, 35.9078, 29.3117, -29.6099, 23.6345,
                52.3702, 60.4720, -6.3146, 51.9194, 39.3999, 45.9432, 44.0165, 44.7866, 1.3521,
                46.1512, -30.5595, 40.4168, 59.3293, 46.8182, 23.6978, 38.7223, 39.9334, 51.5074,
                37.0902, 22.3964),
  Longitude <- c(-58.3816, 14.5501, 4.4699, -106.3468, 104.1954, 15.9785, 15.4730, 12.5683,
                 30.8025, 25.0136, 2.3522, 10.4515, 21.8243, 19.0402, -21.9426, 78.9629,
                 113.9213, 53.6880, -8.2439, 34.8516, 12.4964, -76.7926, 139.6503, 127.7669,
                 126.9770, 28.2336, -29.6099, -102.5528, 4.8952, 19.5033, 138.2529, 21.0122,
                 34.7695, 105.3188, 20.4489, 20.9975, 1.3521, 46.1512, 14.9955, 25.2048, 8.2275,
                 120.9605, 100.9925, 35.2433, -3.7038, 55.3781, -95.7129)
)



               

world_map <- map_data("world")

merged_data <- merge(world_map, participant_data, by.x = "region", by.y = "Country", all.x = TRUE)

ggplot() +
  geom_polygon(data = merged_data, aes(x = long, y = lat, group = group), fill = "lightgray") +
  geom_point(data = merged_data, aes(x = Longitude, y = Latitude, size = Participants), color = "blue", alpha = 0.6) +
  coord_map() +
  theme_void() +
  labs(title = "Participants by Country")
