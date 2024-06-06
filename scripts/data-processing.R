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

## READ IN DATA AND ORGANIZE ####
rm(list=ls()) #clean your environment
# Read in data

##1.CHILDES annotations ####
annotations<- read.csv("data/Table for authors - annotations.csv") ###FIX FILE!
#original link https://docs.google.com/spreadsheets/d/1s-ytfQf7WsZFDDZ6QkOQnZhTodHpQp7D2Wja8YFvjQY/edit?usp=sharing
#https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
read.csv("data/macro_level_measures/ISO-3166-Countries-with-Regional-Codes.csv")->regions

#some cleaning  of the col names
#colnames(annotations)=gsub("\\.\\..*","",colnames(annotations))
colnames(annotations)[colnames(annotations)=="Number.of.participants"]<-"Nb.of.participants"
colnames(annotations)[colnames(annotations)=="Language.or.Languages.spoken.in.recordings..be.specific.if.possible.e.g..French.Quebec."]<-"Language"
colnames(annotations)[colnames(annotations)=="Location..Neighbourhood..village.city..province..state..country."]<-"Location"
colnames(annotations)[colnames(annotations)=="Place.of.recordings..home..nursery...."]<-"Place.of.recording"
colnames(annotations)[colnames(annotations)=="Where.children.spend.their.time..home..nursery..playing.by.the.river...."]<-"common_place"
colnames(annotations)[colnames(annotations)=="Mean.child.age.at.beginning.of.recordings..in.months."]<-"age_beg"
colnames(annotations)[colnames(annotations)=="Bilingualism.Multilingualism.in.corpus..yes..no."]<-"Bilingualism.Multilingualism.in.corpus"
colnames(annotations)[colnames(annotations)=="X..of.children.with.siblings"]<-"Nb.of.children.with.siblings"
colnames(annotations)[colnames(annotations)=="X..of.children.with.older.siblings"]<-"Nb.of.children.with.older.siblings"
colnames(annotations)[colnames(annotations)=="Household.structure..nuclear..extended."]<-"Household.structure"
colnames(annotations)[colnames(annotations)=="Mean.Duration.of.sessions..in.minutes."]<-"Mean.Duration.of.sessions"
colnames(annotations)[colnames(annotations)=="Range.Duration.of.sessions..in.minutes."]<-"Range.Duration.of.sessions"
colnames(annotations)[colnames(annotations)=="Range.number.of..sessions.per.child"]<-"Range.number.of.sessions.per.child"
colnames(annotations)[colnames(annotations)=="Type.of.community.at.the.time.of.the.recordings..hunter.forager.herder.farmer.work.for.pay.industrial....."]<-"Type.of.community"
colnames(annotations)[colnames(annotations)=="Fertility.rate.of.community.at.the.time.of.the.recordings"]<-"Fertility.rate"
colnames(annotations)[colnames(annotations)=="Interbirth.intervals.in.community.at.the.time.of.the.recordings"]<-"Interbirth.intervals"
colnames(annotations)[colnames(annotations)=="Access.to.schooling.for.recorded.children..yes.no.only.elementary."]<-"Access.to.schooling"
colnames(annotations)[colnames(annotations)=="Access.to.health.service.for.recorded.children...yes.no.....answer..yes..if.the.answer.is.an.obvious.yes..for.example.a.child.growing.up.in.a.middle.class.family.in.France..or..no..if.the.answer.is.an.obvious.no..for.example.a.remote.village.where.many.people.do.not.have.access.to.health.services.e.g..the.Tsimane.."]<-"Access.to.health.service"
colnames(annotations)[colnames(annotations)=="Number.of.speakers.of.the.language..e.g..millions.for.English..22k.for.Tsimane.."]<-"Number.of.speakers.of.the.language"
colnames(annotations)[colnames(annotations)=="Our.coding.of"]<-"Our.coding.of.community.type"

                                                                                                                                                                                                                                                                                                        
colnames(annotations)#some cleaning of the data
#remove all the excluded corpus
xtabs(~Inclusion, annotations)  #excluded 109 include 339 
annotations$Inclusion[annotations$Inclusion %in% c("No","no")] <- "exclude"
annotations$Inclusion[annotations$Inclusion %in% c("Yes","yes")] <- "include"

annotations_inc <-  subset(annotations, (Inclusion %in% c("include")))


## Columns of the annotations file
## 1st Cluster : Corpus Information #### 
#Number of participants 
#needs cleaning
xtabs(~Nb.of.participants, annotations_inc) #7 empty
annotations_inc$Nb.of.participants[annotations_inc$Nb.of.participants==""]<-NA
annotations_inc$Nb.of.participants=as.numeric(as.character(annotations_inc$Nb.of.participants))
#add missing info for USA corpora
annotations_inc$Nb.of.participants[annotations_inc$Corpus%in% c("Demetras - Trevor")]<-1
annotations_inc$Nb.of.participants[annotations_inc$Corpus%in% c("McMillan")]<-2
annotations_inc$Nb.of.participants[annotations_inc$Corpus%in% c("Post")]<-3
annotations_inc$Nb.of.participants[annotations_inc$Corpus%in% c("Morisset")]<-206
annotations_inc$Nb.of.participants[annotations_inc$Corpus%in% c("Feldman (Andrea) ")]<-1
annotations_inc$Nb.of.participants[annotations_inc$Corpus%in% c("Demetras - Working")]<-3
annotations_inc$Nb.of.participants[annotations_inc$Corpus%in% c("Davis")]<-21


#Language or languages spoken in recordings 
#needs cleaning
xtabs(~Language, annotations_inc) 
#we rewrite some of the cases in which multiple subgroups exist so that all combinations are represented
#unique(sort(annotations_inc$Language.or.Languages.spoken.in.recordings))

annotations_inc$Language[annotations_inc$Language%in% c("British English","American English (Middle Atlantic mother, Texas father)")]<-"English"
annotations_inc$Language[annotations_inc$Language=="Catalan (and the variant of Spanish spoken in Barcelona)"]<-"Catalan/Spanish"
annotations_inc$Language[annotations_inc$Language=="Japanese (Kyoto dialect)"]<-"Japanese"
annotations_inc$Language[annotations_inc$Language=="Mandarin Chinese, influenced by the dialects of Jianghuai Mandarin and Cantonese to a minor degree"]<-"Mandarin"
annotations_inc$Language[annotations_inc$Language=="Norwegian (Children: mostly trøndersk, parents/care-takers: trøndersk, vestnorsk, nordnorsk, austnorsk)"]<-"Norwegian"
annotations_inc$Language[annotations_inc$Language=="Spanish Spain"]<-"Spanish"
annotations_inc$Language[annotations_inc$Language %in% c("European Portuguese","Brazilian Portuguese")]<-"Portuguese (Brazilian or European)"
annotations_inc$Language[annotations_inc$Language %in% c("Kuwaiti Arabic","Egyptian Arabic")]<-"Arabic (Egyptian or Kuwaiti)"
annotations_inc$Language[annotations_inc$Language %in% c("Spanish & Spanish/English")]<-"Spanish/English"
annotations_inc$Language[annotations_inc$Language %in% c("Dutch/French (N=31) and Dutch/English (N=3)")]<-"Dutch/English, Dutch/French"
annotations_inc$Language[annotations_inc$Language %in% c("Dutch/French & Dutch/Italian")]<-"Dutch/Italian"

byPart<- annotations_inc %>%
  select(Language, Nb.of.participants) %>%
  filter(!is.na(Nb.of.participants)) %>%
  group_by(Language) %>%
  summarise(numpar = sum(as.numeric(Nb.of.participants))) 


#Location
annotations_inc$country <-NA
annotations_inc$continent <-NA
xtabs(~Location, annotations_inc) 
annotations_inc$country[grep("Ireland", annotations_inc$Location)]  <- "Ireland"
annotations_inc$country[grep("England|Arfon area Gwynedd, North Wales|Belfast, Northern Ireland|Nottingham/Manchester, England|England, Brighton|Wales|Cambridge, UK", annotations_inc$Location)]  <- "United Kingdom"
annotations_inc$country[grep("Portugal", annotations_inc$Location)]  <- "Portugal"
annotations_inc$country[grep("Spain|Madrid, Spain ; Tenerife, Canary Islands|Madrid, Spain|Navarra, Spain|Alt penedes, region of catalonia|spain| Lloret de mar|Barcelona| SPAIN|Salamanca|Lugo", annotations_inc$Location)]  <- "Spain"
annotations_inc$country[grep("France|France (Normandy, Marseille, + places visited)", annotations_inc$Location)]  <- "France"
annotations_inc$country[grep("Naples|italy|Roma|Italy", annotations_inc$Location)]  <- "Italy"
annotations_inc$country[grep("Switzerland", annotations_inc$Location)]  <- "Switzerland"
annotations_inc$country[grep("Belgium", annotations_inc$Location)]  <- "Belgium"
annotations_inc$country[grep("Germany", annotations_inc$Location)]  <- "Germany"
annotations_inc$country[grep("Netherlands", annotations_inc$Location)]  <- "Netherlands"
annotations_inc$country[grep("Austria", annotations_inc$Location)]  <- "Austria"
annotations_inc$country[grep("Poznań, Poland", annotations_inc$Location)]  <- "Poland"
annotations_inc$country[grep("Estonia|Tartu, Estonia ; Rapla, Estonia|Southern Estonia|Tartu, Estonia", annotations_inc$Location)]  <- "Estonia"
annotations_inc$country[grep("Hungary", annotations_inc$Location)]  <- "Hungary"
annotations_inc$country[grep("Czech Republic", annotations_inc$Location)]  <- "Czech Republic"
annotations_inc$country[grep("Bucharest, Romania|Romania", annotations_inc$Location)]  <- "Romania"
annotations_inc$country[grep("Serbia", annotations_inc$Location)]  <- "Serbia"
annotations_inc$country[grep("Croatia", annotations_inc$Location)]  <- "Croatia"
annotations_inc$country[grep("Slovenia", annotations_inc$Location)]  <- "Slovenia"
annotations_inc$country[grep("Sweden", annotations_inc$Location)]  <- "Sweden"
annotations_inc$country[grep("Iceland", annotations_inc$Location)]  <- "Iceland"
annotations_inc$country[grep("Norway", annotations_inc$Location)]  <- "Norway"
annotations_inc$country[grep("Denmark", annotations_inc$Location)]  <- "Denmark"
annotations_inc$country[grep("Athens, Greece", annotations_inc$Location)]  <- "Greece"
annotations_inc$country[grep("Moscow, Russia", annotations_inc$Location)]  <- "Russia"
annotations_inc$country[grep("Turkey", annotations_inc$Location)]  <- "Turkey"
annotations_inc$country[grep("Iran", annotations_inc$Location)]  <- "Iran"
annotations_inc$country[grep("Israel|Yahud, Israel", annotations_inc$Location)]  <- "Israel"
annotations_inc$country[grep("Kuwait", annotations_inc$Location)]  <- "Kuwait"
annotations_inc$country[grep("Bombay, India", annotations_inc$Location)]  <- "India"
annotations_inc$country[grep("China", annotations_inc$Location)]  <- "China"
annotations_inc$country[grep("Singapore", annotations_inc$Location)]  <- "Singapore"
annotations_inc$country[grep("Indonesia", annotations_inc$Location)]  <- "Indonesia"
annotations_inc$country[grep("Bangkok, Thailand", annotations_inc$Location)]  <- "Thailand"
annotations_inc$country[grep("Korea", annotations_inc$Location)]  <- "South Korea" #NOTE: one corpus just says "Korea", data collected in 2009-2011, assuming it's South Korean
annotations_inc$country[grep("Taiwan", annotations_inc$Location)]  <- "Taiwan"
annotations_inc$country[grep("Papua-New Guinea", annotations_inc$Location)]  <- "Papua New Guinea"
annotations_inc$country[grep("Alexandria, Egypt", annotations_inc$Location)]  <- "Egypt"
annotations_inc$country[grep("Mokhotlong, Lesotho", annotations_inc$Location)]  <- "Lesotho"
annotations_inc$country[grep("South Africa", annotations_inc$Location)]  <- "South Africa"
annotations_inc$country[grep("Rio Cuarto, Cordoba, Argentina", annotations_inc$Location)]  <- "Argentina"
annotations_inc$country[grep("Sao Paulo", annotations_inc$Location)]  <- "Brazil"
annotations_inc$country[grep("Mexico", annotations_inc$Location)]  <- "Mexico"
annotations_inc$country[grep("Jamaica", annotations_inc$Location)]  <- "Jamaica"
annotations_inc$country[grep("Michigan, USA|USA, Northern Virginia|California, USA|washington dc|United States|USA|Washington|Maryland|San Fran|Cambridge MA|Honolulu, HI|usa|UCLA", annotations_inc$Location)]  <- "United States"
annotations_inc$country[grep("Canada|Montreal", annotations_inc$Location)]  <- "Canada"
annotations_inc$country[grep("Osaka|Tokyo|Nagoya|osaka|Kusatsu City, Shiga Pref", annotations_inc$Location)]  <- "Japan"
annotations_inc$country[grep("Hong-Kong|Hong Kong", annotations_inc$Location)]  <- "Hong Kong"
annotations_inc$country[annotations_inc$Location==""]<-NA

#Special cases
annotations_inc$country[grep("Sweden ; Portugal", annotations_inc$Location)]  <- "Sweden & Portugal"
annotations_inc$country[annotations_inc$Location %in% c("Spain (Lloret de Mar), Hungary (Kecskemét)")] <- "Spain & Hungary"
xtabs(~country, annotations_inc) 

country_info<- as.data.frame(xtabs(~country, annotations_inc)) 
#to check if any left
#annotations_inc[is.na(annotations_inc$country),"Location"]
#annotations_inc[is.na(annotations_inc$continent),"Location"]

#Following the EuroVoc classification https://en.wikipedia.org/wiki/EuroVoc + Italy and Spain and Portugal
# annotations_inc$continent[grep("Andorra|Austria|Belgium|France|Germany|Ireland|Italy|Liechtenstein|Luxembourg|Monaco|Netherlands|Portugal|Spain|Switzerland|United Kingdom", annotations_inc$country)]  <- "Western Europe"
# annotations_inc$continent[grep("Poland|Estonia|Hungary|Czech|Romania|Serbia|Croatia|Slovenia|Sweden|Iceland|Norway|Denmark|Greece|Russia", annotations_inc$country)]  <- "Non-Western Europe"
# annotations_inc$continent[grep("Turkey|Iran|Israel|Kuwait|India|China|Singapore|Indonesia|Thailand|Japan|South Korea|Hong Kong|Taiwan", annotations_inc$country)]  <- "Asia"
# annotations_inc$continent[grep("Papua New Guinea", annotations_inc$country)]  <- "Oceania"
# annotations_inc$continent[grep("United States|Canada|Mexico|Jamaica", annotations_inc$country)]  <- "North America & the Caribbean"
# annotations_inc$continent[grep("Argentina|Brazil", annotations_inc$country)]  <- "South America"
# annotations_inc$continent[grep("Egypt|Lesotho|Africa", annotations_inc$country)]  <- "Africa"

#We are leaving continent as NA (because one country is Western & the other Eastern Europe)


##2nd Cluster : information about the recording ####

#Place of recordings (home, nursery...)
#Mean number of sessions per child
#Range number of  sessions per child
#Mean Duration of sessions (in minutes)
#Range Duration of sessions (in minutes)

##3rd Cluster: information about the language of the recording ####
lang_temp=levels(factor(annotations_inc$Language))

#split up &
lang_temp2=NULL
for(x in lang_temp) lang_temp2=c(lang_temp2,unlist(strsplit(x," & ")))
lang_temp2=levels(factor(lang_temp2))

#put mono separately from multi
mono=lang_temp2[grep("/",lang_temp2, invert=T)]
multi=lang_temp2[grep("/",lang_temp2)]

mono_langs=mono[1]
for(x in 2:length(mono)) mono_langs=paste0(mono_langs,", ",mono[x])

multi_langs=multi[1]
for(x in 2:length(multi)) multi_langs=paste0(multi_langs,", ",multi[x])

xtabs(~Bilingualism.Multilingualism.in.corpus, annotations_inc) 
annotations_inc$Bilingualism.Multilingualism.in.corpus<-tolower(annotations_inc$Bilingualism.Multilingualism.in.corpus)
annotations_inc$Bilingualism.Multilingualism.in.corpus[grep("yes",annotations_inc$Bilingualism.Multilingualism.in.corpus)]<-"yes"
annotations_inc$Bilingualism.Multilingualism.in.corpus[grep("trilinguism",annotations_inc$Bilingualism.Multilingualism.in.corpus)]<-"yes"
annotations_inc$Bilingualism.Multilingualism.in.corpus[grep("no; a few words of yiddish and spanish interjected, less than once per session.",annotations_inc$Bilingualism.Multilingualism.in.corpus)]<-"no"
annotations_inc$Bilingualism.Multilingualism.in.corpus[annotations_inc$Bilingualism.Multilingualism.in.corpus==""]<-NA


#Language minority
#Bilingualism/Multilingualism in corpus (yes/ no)
#Bilingualism/Multilingualism in community (yes/ no)
#Bilingualism/Multilingualism in family (yes/ no)
#Number of speakers of the language (e.g. millions for English, 22k for Tsimane’)

##4rd Cluster: information about the recorded children####

#Where children spend their time (home, nursery, playing by the river...)
#Mean child age at beginning of recordings (in months)
#Mean child age at end of recordings (in months)
# of children with siblings
xtabs(~Nb.of.children.with.siblings, annotations_inc) 
annotations_inc$Nb.of.children.with.siblings[annotations_inc$Nb.of.children.with.siblings %in% c("all firstborn (some newborn sibs)", "don't know", "at least 1 (triplet)", "")] <- "NA"
annotations_inc$Nb.of.children.with.siblings=as.numeric(as.character(annotations_inc$Nb.of.children.with.siblings))
annotations_inc$Proportion.With.Siblings=annotations_inc$Nb.of.children.with.siblings/annotations_inc$Nb.of.participants

# of children with older siblings
#Average number of siblings
xtabs(~Average.number.of.siblings, annotations_inc) 
annotations_inc$Average.number.of.siblings[annotations_inc$Average.number.of.siblings %in% c("some newborns", "don't know", "")] <- "NA"

#Average number of older siblings
#Access to schooling for recorded children (yes/no/only elementary) 
#Access to health service for recorded children  (yes/no) - (answer ‘yes’ if the answer is an obvious yes, for example a child growing up in a middle-class family in France, or 'no' if the answer is an obvious no, for example a remote village where many people do not have access to health services e.g. the Tsimane')
#our coding of access to health
#our coding of access to school
#Household structure (nuclear, extended)
xtabs(~Household.structure, annotations_inc) 

#annotations_inc$Household.structure=tolower(annotations_inc$Household.structure)
annotations_inc$Household.structure[grep("nuclear|nucear|single|Nuclear|Nucear|don't know in detail; largely nuclear|",annotations_inc$Household.structure)]<-"nuclear" # including single parent

annotations_inc$Household.structure[annotations_inc$Household.structure %in% c("nuclear, extended","extended/nuclear", "extended, two grandparents, parents and the child")]<-"varied"
annotations_inc$Household.structure[grep("extended|Nuclear family with a strong bond and contact with her grandparents, uncles, aunts and cousins. 
",annotations_inc$Household.structure)]<-"extended"
annotations_inc$Household.structure[annotations_inc$Household.structure==""]<-NA


##5th Cluster: information about SES or related measures ####
#Parental education
#Education STDZD
# Education & SES cleaning
xtabs(~Education.STDZD, annotations_inc) 

annotations_inc$Education.STDZD[annotations_inc$Education.STDZD==""]<-NA
annotations_inc$Education.STDZD[annotations_inc$Education.STDZD=="4-6"]<-"4-5" #fix typo
annotations_inc$Education.min<-as.numeric(as.character(gsub("-.*","",annotations_inc$Education.STDZD)))
annotations_inc$Education.max<-as.numeric(as.character(gsub(".*-","",annotations_inc$Education.STDZD)))
annotations_inc$Education.mid<-annotations_inc$Education.min+(annotations_inc$Education.max-annotations_inc$Education.min)/2
# 1 = primary
# 2 = secondary school
# 3 = some college
# 4 = university
# 5 = postgraduate

annotations_inc$Education.ac=NA
annotations_inc$Education.ac[annotations_inc$Education.min==1 & !is.na(annotations_inc$Education.min)]<-"Some primary"
annotations_inc$Education.ac[annotations_inc$Education.min==2 & !is.na(annotations_inc$Education.min)]<-"Some secondary"
annotations_inc$Education.ac[annotations_inc$Education.min==3 & !is.na(annotations_inc$Education.min)]<-"Some college"
annotations_inc$Education.ac[annotations_inc$Education.min>3 & !is.na(annotations_inc$Education.min)]<-"College and above"

#Parental socioeconomic status
#SES STDZD
xtabs(~SES.STDZD, annotations_inc) 
annotations_inc$SES.STDZD[annotations_inc$SES.STDZD==""]<-NA
annotations_inc$SES.STDZD[annotations_inc$SES.STDZD=="middle class"]<-2


#Parental profession
xtabs(~Parental.profession, annotations_inc) 
annotations_inc$Parental.profession=tolower(annotations_inc$Parental.profession)
annotations_inc$is.academic=annotations_inc$is.health=annotations_inc$is.teacher=NA
annotations_inc$is.academic[grep("stud|professor|ling|investig|resear|sociol|academ|scienti|university|universities|pi|PI|psycholing|gradua",annotations_inc$Parental.profession)]<-"yes"
annotations_inc$is.health[grep("psychology|doctor|therapist|nurse|psycholo",annotations_inc$Parental.profession)]<-"yes"
annotations_inc$is.teacher[grep("teach",annotations_inc$Parental.profession)]<-"yes"
annotations_inc$is.academic[annotations_inc$Parental.profession=="pi"]<-"yes"
annotations_inc$is.education[annotations_inc$is.academic=="yes"| annotations_inc$is.teacher=="yes" ]<-"yes"
annotations_inc$Parental.profession[annotations_inc$Parental.profession %in% c("don't know", "")]<-NA


##6th Cluster: Information about the recorded community ####
#Type of community at the time of the recordings (hunter/forager/herder/farmer/work-for-pay/industrial/...)
xtabs(~Type.of.community, annotations_inc) 
annotations_inc$Type.of.community[is.na(annotations_inc$Type.of.community)]<-annotations_inc$STDZD.community.type[is.na(annotations_inc$Type.of.community)]
annotations_inc$Type.of.community[annotations_inc$Type.of.community %in% c("academic","capital city of the Soviet Union","city","industial","industrial ","work-for-pay",
                                                                                                        "work-for-pay/industrial","industrial","Northeastern US urban","urban western", "industrial & service &trade activities...", "Mediterranean city of 1.6 million inhabitants", "modern city families, usually both working parents","Seaside tourist town","Industrial city (population: around 272,000 inhabitants)","mainly urban", "industrial city (3.000.000 inhabitants")]<-"urban"

annotations_inc$Type.of.community[annotations_inc$Type.of.community %in% c("farmer","rural","rural farming village")]<-"rural"
annotations_inc$Type.of.community[annotations_inc$Type.of.community %in% c("industrial, farmers")]<-"both"
annotations_inc$Type.of.community[annotations_inc$Type.of.community==""]<-NA
#table(annotations_inc$Type.of.community.at.the.time.of.the.recordings)

#STDZD community type
#Fertility rate of community at the time of the recordings 
#Interbirth intervals in community at the time of the recordings 

#merge with international codes
#fix the names
regions$name=gsub("United States of America","United States",regions$name)
regions$name=gsub("United Kingdom of Great Britain and Northern Ireland","United Kingdom",regions$name)
regions$name=gsub("Czechia","Czech Republic",regions$name)
regions$name[regions$name == "Iran (Islamic Republic of)"] <- "Iran" #regular code didnt work
regions$name=gsub("Russian Federation","Russia",regions$name)
regions$name=gsub("Korea, Republic of","South Korea",regions$name)
regions$name=gsub("Taiwan, Province of China","Taiwan",regions$name)

merge(x=annotations_inc, y=regions,  by.x = "country", by.y = "name", all.x = T) -> annotations_inc

write.table(annotations_inc,"derived/annotations_included", row.names = FALSE, col.names = TRUE)

