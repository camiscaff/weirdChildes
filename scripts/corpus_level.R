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
library(maps)
library(mapdata)
library(geosphere)
## READ IN DATA AND ORGANIZE ####
rm(list=ls()) #clean your environment
# Read in data

##1.CHILDES annotations ####
annotations_inc<- read.csv("../derived/annotations_included", sep="")
ocde <- read.csv("../data/ocde_country.csv", sep=",")


##countries and corpora
corpora <- as.data.frame((table(annotations_inc$country)))
names(corpora)<-c("country","Freq")
#write.table(corpora,"derived/corpora", row.names = FALSE, col.names = TRUE)


merge(x=corpora, y=ocde,  by = "country", all.x = T) -> ocde_country
sum(ocde_country$Freq[ocde_country$ocde == "yes"],na.rm=T)
#important to add the 2 bil corpora! 149 + 2 = 151


##Education level info 
education <- as.data.frame((table(annotations_inc$Education.ac, annotations_inc$country)))
names(education)<-c("Education.ac","country","Freq")
education_withdata <- subset(education, Freq > 0)   
write.table(education_withdata,"derived/education_withdata", row.names = FALSE, col.names = TRUE)

education_withdata<- read.csv("derived/education_withdata", sep="")
levels(as.factor(education_withdata$country))#28 countries
education_withdata$Education.ac = factor(education_withdata$Education.ac, levels=c("Some primary", "Some secondary", "Some college", "College and above"))
annotations_inc$Education.ac = factor(annotations_inc$Education.ac, levels=c("College and above", "Some college", "Some secondary","Some primary"  ))

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
  scale_x_discrete(name = "Countries or Territories") +
  scale_y_continuous(name = "Number of corpus with Education data") +
  labs(title="Education of caregivers", fill = "Education Level")


#SES level info
ses <- as.data.frame((table(annotations_inc$SES.STDZD, annotations_inc$country)))
names(ses)<-c("SES.STDZD","country","Freq")
ses_withdata <- subset(ses, Freq > 0)   
#write.table(ses_withdata,"derived/ses_withdata", row.names = FALSE, col.names = TRUE)

ses_withdata<- read.csv("../derived/ses_withdata", sep="")
levels(as.factor(ses_withdata$country))#35 countries


# Plot this categorywise bar chart
ses <- ggplot(data=subset(annotations_inc, !is.na(SES.STDZD)), aes(country))
# g + geom_bar(aes(fill=Education.ac), width = 0.6, position = "fill") + 
#   theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() +
#   labs(title="Education of caregivers") 
ses + geom_bar(aes(fill=SES.STDZD), width = 0.9) + 
  theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() + 
  #facet_grid(~ scenario_f) +
  theme_minimal()  +
  #ggtitle("Education of caregivers")  + 
  theme(plot.title = element_text(size = 10, face = "bold"),strip.text.x = element_text(size = 10)) +
  coord_flip() + 
  #guides(fill=FALSE ) + 
  scale_fill_manual(breaks=c("1", "1-2", "2"),values = c( "#99E6FF", "#4CA6FF", "#0040FF" )) +
  scale_x_discrete(name = "Number of corpus") +
  scale_y_continuous(name = "Countries") 
labs(title="SES of caregivers") +  scale_y_continuous( breaks = seq(0,13, 1))



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

community_withdata<- read.csv("../derived/community_withdata", sep="")
levels(as.factor(community_withdata$country))#28 countries

# Plot this categorywise bar chart
urbans <- ggplot(data=subset(annotations_inc, !is.na(Type.of.community)), aes(country))
# g + geom_bar(aes(fill=Education.ac), width = 0.6, position = "fill") + 
#   theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() +
#   labs(title="Education of caregivers") 
urbans + geom_bar(aes(fill=Type.of.community), width = 0.9) + 
  theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() + 
  #facet_grid(~ scenario_f) +
  theme_minimal()  +
  #ggtitle("Education of caregivers")  + 
  theme(plot.title = element_text(size = 10, face = "bold"),strip.text.x = element_text(size = 10)) +
  coord_flip() + 
  #guides(fill=FALSE ) + 
  scale_fill_manual(breaks=c("rural", "both", "urban"),values = c( "#99E6FF", "#4CA6FF", "#0040FF" )) +
  scale_x_discrete(name = "Number of corpus") +
  scale_y_continuous(name = "Countries") 
labs(title="Urbanization") +  scale_y_continuous( breaks = seq(0,13, 1))


## Figure
# Creating a subset with specific columns
map_info <- annotations_inc[, c("country", "Corpus", "Nb.of.participants", "Location")]
write.table(map_info, file = "derived/map_info.txt", sep = "\t", quote = FALSE)

country_coords<- read.csv("data/map_coordinates.csv", sep=",")

world_map <- map_data("world")

ggplot() +
  geom_polygon(data = world_map, aes(x = long, y = lat, group = group), fill = "lightgray") +
  geom_point(data = country_coords, aes(x = Longitude, y = Latitude, size = Nb.of.participants), color = "blue", alpha = 0.6) +
  theme_void() +
  labs(title = "Participants by Country")

ggsave("figures/childes_country_participants.png", width = 6, height = 4, dpi = 300)


library(ggplot2)

# Assuming 'data' is your dataset

library(ggplot2)

# Assuming 'data' is your dataset

# Plot for Education
g <- ggplot(data=subset(annotations_inc, !is.na(Education.ac)), aes(country))
# g + geom_bar(aes(fill=Education.ac), width = 0.6, position = "fill") + 
#   theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() +
#   labs(title="Education of caregivers") 
 
education_plot <- g + geom_bar(aes(fill=Education.ac), width = 0.9) + 
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


# Plot for SES
ses <- ggplot(data=subset(annotations_inc, !is.na(SES.STDZD)), aes(country))
# g + geom_bar(aes(fill=Education.ac), width = 0.6, position = "fill") + 
#   theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() +
#   labs(title="Education of caregivers") 
ses_plot <- ses + geom_bar(aes(fill=SES.STDZD), width = 0.9) + 
  theme(axis.text.x = element_text(vjust=0.4)) +  coord_flip() + 
  #facet_grid(~ scenario_f) +
  theme_minimal()  +
  #ggtitle("Education of caregivers")  + 
  theme(plot.title = element_text(size = 10, face = "bold"),strip.text.x = element_text(size = 10)) +
  coord_flip() + 
  #guides(fill=FALSE ) + 
  scale_fill_manual(breaks=c("1", "1-2", "2"),values = c( "#99E6FF", "#4CA6FF", "#0040FF" )) +
  scale_x_discrete(name = "Number of corpus") +
  scale_y_continuous(name = "Countries") 
labs(title="SES of caregivers") +  scale_y_continuous( breaks = seq(0,13, 1))


# Combine the plots into a mirrored bar chart
combined_plot <- cowplot::plot_grid(education_plot, ses_plot, nrow = 1)

# Show the combined plot
print(combined_plot)



