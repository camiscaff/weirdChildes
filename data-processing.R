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
annotations<- read.csv("data/Table for authors - annotations.csv") 
#original link https://docs.google.com/spreadsheets/d/1s-ytfQf7WsZFDDZ6QkOQnZhTodHpQp7D2Wja8YFvjQY/edit?usp=sharing

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
annotations_inc$country[grep("Osaka|Tokyo|Nagoya|osaka|Kusatsu City, Shiga Pref", annotations_inc$Location)]  <- "Japan"
annotations_inc$country[grep("Korea", annotations_inc$Location)]  <- "Korea" #NOTE: one corpus just says "Korea", data collected in 2009-2011, assuming it's South Korean
annotations_inc$country[grep("Hong-Kong, Hong Kong", annotations_inc$Location)]  <- "Hong Kong"
annotations_inc$country[grep("Taiwan", annotations_inc$Location)]  <- "Taiwan"
annotations_inc$country[grep("Papua-New Guinea", annotations_inc$Location)]  <- "Papua New Guinea"
annotations_inc$country[grep("Alexandria, Egypt", annotations_inc$Location)]  <- "Egypt"
annotations_inc$country[grep("Mokhotlong, Lesotho", annotations_inc$Location)]  <- "Lesotho"
annotations_inc$country[grep("South Africa", annotations_inc$Location)]  <- "South Africa"
annotations_inc$continent[grep("Egypt|Lesotho|Africa", annotations_inc$country)]  <- "Africa"
annotations_inc$country[grep("Rio Cuarto, Cordoba, Argentina", annotations_inc$Location)]  <- "Argentina"
annotations_inc$country[grep("Sao Paulo", annotations_inc$Location)]  <- "Brazil"
annotations_inc$country[grep("Mexico", annotations_inc$Location)]  <- "Mexico"
annotations_inc$country[grep("Jamaica", annotations_inc$Location)]  <- "Jamaica"
annotations_inc$country[grep("Michigan, USA|USA, Northern Virginia|California, USA|washington dc|United States|USA|Washington|Maryland|San Fran|Cambridge MA|Honolulu, HI|usa|UCLA", annotations_inc$Location)]  <- "United States"
annotations_inc$country[grep("Canada|Montreal", annotations_inc$Location)]  <- "Canada"
annotations_inc$country[annotations_inc$Location==""]<-NA

#Special cases
annotations_inc$country[grep("Sweden ; Portugal", annotations_inc$Location)]  <- "Sweden & Portugal"
annotations_inc$country[annotations_inc$Location %in% c("Spain (Lloret de Mar), Hungary (Kecskemét)")] <- "Spain & Hungary"

country_info<- as.data.frame(xtabs(~country, annotations_inc)) 
#to check if any left
#annotations_inc[is.na(annotations_inc$country),"Location"]
#annotations_inc[is.na(annotations_inc$continent),"Location"]

#Following the EuroVoc classification https://en.wikipedia.org/wiki/EuroVoc + Italy and Spain and Portugal
annotations_inc$continent[grep("Andorra|Austria|Belgium|France|Germany|Ireland|Italy|Liechtenstein|Luxembourg|Monaco|Netherlands|Portugal|Spain|Switzerland|United Kingdom", annotations_inc$country)]  <- "Western Europe"
annotations_inc$continent[grep("Poland|Estonia|Hungary|Czech|Romania|Serbia|Croatia|Slovenia|Sweden|Iceland|Norway|Denmark|Greece|Russia", annotations_inc$country)]  <- "Non-Western Europe"
annotations_inc$continent[grep("Turkey|Iran|Israel|Kuwait|India|China|Singapore|Indonesia|Thailand|Japan|Korea|Hong Kong|Taiwan", annotations_inc$country)]  <- "Asia"
annotations_inc$continent[grep("Papua New Guinea", annotations_inc$country)]  <- "Oceania"
annotations_inc$continent[grep("United States|Canada", annotations_inc$country)]  <- "North America"
annotations_inc$continent[grep("Argentina|Brazil|Mexico|Jamaica", annotations_inc$country)]  <- "Latin America"


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
annotations_inc$Household.structure[annotations_inc$Household.structure %in% c("nuclear, extended","extended/nuclear")]<-"varied"
annotations_inc$Household.structure[grep("nuclear|nucear|single|Nuclear|Nucear",annotations_inc$Household.structure)]<-"nuclear" # including single parent
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
annotations_inc$is.academic[grep("stud|prof|ling|investig|resear|sociol|academ|scienti|university|universities|pi|PI|psycholing",annotations_inc$Parental.profession)]<-"yes"
annotations_inc$is.health[grep("psychology|doctor|therapist|nurse|psycholo",annotations_inc$Parental.profession)]<-"yes"
annotations_inc$is.teacher[grep("teacher",annotations_inc$Parental.profession)]<-"yes"
annotations_inc$is.academic[annotations_inc$Parental.profession=="pi"]<-"yes"
annotations_inc$Parental.profession[annotations_inc$Parental.profession %in% c("don't know", "")]<-NA


##6th Cluster: Information about the recorded community ####
#Type of community at the time of the recordings (hunter/forager/herder/farmer/work-for-pay/industrial/...)
xtabs(~Type.of.community, annotations_inc) 
annotations_inc$Type.of.community[is.na(annotations_inc$Type.of.community)]<-annotations_inc$STDZD.community.type[is.na(annotations_inc$Type.of.community)]
annotations_inc$Type.of.community[annotations_inc$Type.of.community %in% c("academic","capital city of the Soviet Union","city","industial","industrial ","work-for-pay",
                                                                                                                               "industrial & service &trade activities...","industrial city (3.000.000 inhabitants",
                                                                                                                               "Industrial city (population: around 272,000 inhabitants)",
                                                                                                                               "Mediterranean city of 1.6 million inhabitants",
                                                                                                                               "modern city families, usually both working parents",
                                                                                                                               "Orthodox Jews",
                                                                                                                               "Seaside tourist town",
                                                                                                                               "work-for-pay/industrial","industrial","Northeastern US urban","urban western")]<-"urban"

annotations_inc$Type.of.community[annotations_inc$Type.of.community %in% c("farmer","rural","rural farming village")]<-"rural"
annotations_inc$Type.of.community[annotations_inc$Type.of.community %in% c("industrial, farmers","mainly urban")]<-"both"
annotations_inc$Type.of.community[annotations_inc$Type.of.community==""]<-NA
#table(annotations_inc$Type.of.community.at.the.time.of.the.recordings)

#STDZD community type
#Fertility rate of community at the time of the recordings 
#Interbirth intervals in community at the time of the recordings 


## Country level information ####
## Merge ISO of country with our names for Countries
# This is necessary to merge info with World Bank
read.csv("country_iso.csv")->iso_lookup
codes=iso_lookup$Code
iso_lookup$Name<-as.character(iso_lookup$Name)
iso_lookup$Name=gsub(", Islamic Republic of","",iso_lookup$Name)
iso_lookup$Name=gsub("Russian Federation","Russia",iso_lookup$Name)
iso_lookup$Name=gsub(", Province of China","",iso_lookup$Name)
iso_lookup$Name=gsub(", Republic of","",iso_lookup$Name)
iso_lookup$Name=gsub("Congo, the Democratic Republic of the","Democratic Republic of Congo",iso_lookup$Name)

names(codes)<-iso_lookup$Name

## Country name for Wordbank
annotations_inc$country_WB<-codes[annotations_inc$country]

annotations_inc$country_WB <-factor(annotations_inc$country_WB)
#annotations_inc[is.na(annotations_inc$country_WB),"country"]
#check - this one is NA bec it's data from 2 countries

annotations_inc$continent <-factor(annotations_inc$continent)


read.csv("wdi-data.csv")->wdi_all

#remove areas
wdi_all=wdi_all[!(wdi_all$country %in% c( "Africa Eastern and Southern" ,"Africa Western and Central",
                                          "Arab World", "Caribbean small states", "Central Europe and the Baltics" ,
                                          "European Union", "Fragile and conflict affected situations",
                                          "OECD members", "West Bank and Gaza",
                                          "Small states"  , "Pacific island small states" 
                                          , "Caribbean small states" , "Other small states",
                                          "Latin America & the Caribbean (IDA & IBRD countries)", "Middle East & North Africa (IDA & IBRD countries)"  ,
                                          "East Asia & Pacific (IDA & IBRD countries)"     ,      "South Asia (IDA & IBRD)"    ,                        
                                          "Sub-Saharan Africa (IDA & IBRD countries)"    ,        "Europe & Central Asia (IDA & IBRD countries)" ,
                                          "Turks and Caicos Islands", 
                                          "Euro area"      ,                                     
                                          "High income"                               ,           "Heavily indebted poor countries (HIPC)"              
                                          , "IBRD only"                                ,            "IDA total"                                           
                                          , "IDA blend"                               ,            "IDA only"                                            
                                          , "Latin America & Caribbean (excluding high income)",
                                          "Middle East & North Africa"                          
                                          , "IDA & IBRD total"  ,
                                          "Europe & Central Asia"  ,  "Sub-Saharan Africa (excluding high income)"  ,"Sub-Saharan Africa"    ,                               "Africa Eastern and Southern",
                                          "Least developed countries: UN classification"   ,      "Low income"                                          
                                          , "Lower middle income"      ,                            "Low & middle income"                                 
                                          , "Middle income"          ,                              "Middle East & North Africa (excluding high income)"  
                                          , "Upper middle income"     ,                             "North America"                                       
                                          , "Not classified"                                             
                                          , "East Asia & Pacific", "Pre-demographic dividend" , "Early-demographic dividend" ,"Late-demographic dividend"       , "Post-demographic dividend" , "World","East Asia & Pacific (excluding high income)"  , "Europe & Central Asia (excluding high income)","Latin America & Caribbean","Western Sahara","World" ,"South Asia"
)),]
#cleanup
wdi_all$country[wdi_all$country=="Congo, Dem. Rep."]<-"Democratic Republic of Congo"
wdi_all$country[wdi_all$country=="Congo, Rep."]<-"Congo"
wdi_all$country[wdi_all$country=="Lao PDR"]<-"Laos"
wdi_all$country[wdi_all$country=="Virgin Islands (U.S.)"]<-"United States Virgin Islands"
wdi_all$country<-gsub(", The","",wdi_all$country)
wdi_all$country<-gsub(", RB","",wdi_all$country)
wdi_all$country<-gsub(", Arab Rep.","",wdi_all$country)
wdi_all$country<-gsub(", Islamic Rep.","",wdi_all$country)
wdi_all$country<-gsub(" SAR, China","",wdi_all$country)
wdi_all$country<-gsub("St.","Saint",wdi_all$country,fixed=T)
wdi_all$country<-gsub(", Fed. Sts.","",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Kyrgyz Republic","Kyrgyzstan",wdi_all$country,fixed=T)
wdi_all$country<-gsub(" Darussalam","",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Cabo Verde","Cape Verde",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Czech Republic","Czechia",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Korea, Dem. People's Rep.","North Korea",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Korea, Rep.","South Korea",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Yemen, Rep.","Yemen",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Syrian Arab Republic","Syria",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Russian Federation","Russia",wdi_all$country,fixed=T)
wdi_all$country<-gsub("Slovak Republic","Slovakia",wdi_all$country,fixed=T)

## Read in Ourworld in data info on Education - Educated ####
# https://ourworldindata.org/primary-and-secondary-education
read.csv("completion-rate-of-lower-secondary-education-OWID-20220303.csv")->ed_basic
ed_basic[ed_basic$Year>2006 & ed_basic$Year<2015,]->ed_basic
ed_basic[!duplicated(ed_basic$Entity),]->ed_basic
colnames(ed_basic)[colnames(ed_basic)=="Lower.secondary.completion.rate..total....of.relevant.age.group."]<-"Compl.LS"
ed_basic$Compl.LS[ed_basic$Compl.LS>100]<-100  #cap to 100%

#https://ourworldindata.org/tertiary-education
read.csv("share-of-the-population-with-completed-tertiary-education-OWID-20220217.csv")->ed
ed[ed$Year==2010,]->ed #note, no data for 2011, data only every 10 years
colnames(ed)[colnames(ed)=="Barro.Lee..Percentage.of.population.age.15..with.tertiary.schooling..Completed.Tertiary"]<-"College"

## Read in Ourworld in data info on Democracy - Democratic ####

#https://ourworldindata.org/grapher/political-regimes
read.csv("political-regimes-OWID-20220215.csv")->democr
democr[democr$Year==2011,]->democr

## Read in Ourworld in data info on Population Size  ####

#https://ourworldindata.org/grapher/population-past-future
read.csv("population-past-future-OWID-20220217.csv")->pop
pop[pop$Year==2011,]->pop
colnames(pop)[colnames(pop)=="Population..historical.estimates.and.future.projections."]<-"Population"

## Read in Ourworld in data info on Children born per woman - Fertility ####

read.csv("children-born-per-woman-OWID-20220112.csv")->cpw
cpw[cpw$Year==2011,]->cpw
colnames(cpw)[colnames(cpw)=="Fertility.rate..Select.Gapminder..v12...2017."]<-"Fertility"
cpw$Entity<-gsub("Slovak Republic","Slovakia",cpw$Entity,fixed=T)

merge(ed_basic,democr[,c("Entity","Political.regime")],all=T)->owid
merge(owid,pop[,c("Entity","Population")],all=T)->owid
merge(owid,ed[,c("Entity","College")],all=T)->owid
merge(owid,cpw[,c("Entity","Fertility")],all=T)->owid

#clean up
owid=owid[grep("(Urban)",owid$Entity,invert=T),]
owid=owid[grep("(Rural)",owid$Entity,invert=T),]
owid=owid[!(owid$Entity%in% c("World","Western Sahara","Africa" ,"Asia","Oceania","South America",
                              "Europe", "North America"  , "Arab World" ,"Caribbean Small States"  , "Central Europe and the Baltics" ,"Early-demographic dividend", "East Asia & Pacific" ,
                              "East Asia & Pacific (excluding high income)", "East Asia & Pacific (IDA & IBRD)", "Euro area", "Europe & Central Asia", "Europe & Central Asia (excluding high income)",
                              "Europe & Central Asia (IDA & IBRD)", "European Union",
                              "Fragile and conflict affected situations" , "Heavily indebted poor countries (HIPC)","High income" , "IBRD only"    ,                                     
                              "IDA & IBRD total"    ,                              
                              "IDA blend"           ,                              
                              "IDA only"           ,                               
                              "Late-demographic dividend"              ,           
                              "Latin America & Caribbean"     ,                    
                              "Latin America & Caribbean (excluding high income)" ,
                              "Latin America & Caribbean (IDA & IBRD)"       ,     
                              "Least developed countries: UN classification"      ,
                              "Low & middle income"         ,                      
                              "Low income"             ,                           
                              "Lower middle income"     ,
                              "Middle East & North Africa"        ,                
                              "Middle East & North Africa (excluding high income)",
                              "Middle East & North Africa (IDA & IBRD)"           ,
                              "Middle income",
                              "OECD members"   ,                                   
                              "Other small states"       ,                         
                              "Pacific island small states"    , "Post-demographic dividend"    ,                     
                              "Pre-demographic dividend"     ,
                              "South Asia"   ,                                     
                              "South Asia (IDA & IBRD)"       ,                    
                              "Sub-Saharan Africa"    ,                            
                              "Sub-Saharan Africa (excluding high income)"        ,
                              "Sub-Saharan Africa (IDA & IBRD)"  ,   
                              "Upper middle income" 
)),]

owid$Political.regime_t<-NA
owid$Political.regime_t[owid$Political.regime %in% c(0,1)]<-"autocracies"
#owid$Political.regime_t[owid$Political.regime==1]<-"electoral \n autocracies"
owid$Political.regime_t[owid$Political.regime%in% c(2,3)]<-"democracies"
#owid$Political.regime_t[owid$Political.regime==3]<-"liberal \n democracies"


# check no missing countries across WB & OWID merge
#owid$Entity[!(owid$Entity %in% levels(factor(wdi_all$country)))]
#lots of dependencies; but also Palestine -- unclear how to merge
# note: ,"Palestine/Gaza","Palestine/West Bank" -- we are leaving them in and not matching them with other OWID data, which has just Palestine

#2 data points lost: Channel Islands & Kosovo
wdi_all$country[!(wdi_all$country %in% levels(factor(owid$Entity)))]


ind_all = merge(wdi_all,owid,by.x="country",by.y="Entity",all=T)
#ind_all$country

#https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
read.csv("ISO-3166-Countries-with-Regional-Codes.csv")->regions

ind_all=merge(ind_all,regions,by.x="iso2c",by.y="alpha.2")

ind_all$simple_region=ind_all$sub.region
ind_all$simple_region[ind_all$sub.region%in% c("Australia and New Zealand","Melanesia","Micronesia","Polynesia")]<-"Oceania"

ind_all$simple_region[grep("Asia",ind_all$sub.region)]<-"Asia"
ind_all$simple_region[grep("Africa",ind_all$sub.region)]<-"Africa"

ind_all$simple_region[ind_all$sub.region%in% c("Southern Europe","Eastern Europe","Northern Europe")]<-"Non-Western Europe"

ind_all$simple_region[ind_all$country%in% c("Spain","Italy","Portugal")]<-"Western Europe"

ind_all$log_gdp=log(ind_all$NY.GDP.PCAP.PP.KD,10)
ind_all$log2_fert=log(ind_all$Fertility,2)
ind_all$log_pop=log(ind_all$Population,10)

ind_all$western<-"non-Western"
ind_all$western[ind_all$simple_region %in% c('Western Europe',"Northern America")]<-"Western"
ind_all$western[ind_all$country %in% c('Norway',"Australia","New Zealand","Iceland","Sweden","Denmark")]<-"Western"
ind_all$western[ind_all$country %in% c('Bermuda',"Greenland")]<-"non-Western"
ind_all$western=factor(ind_all$western)
ind = ind_all[ind_all$iso2c %in% levels(annotations_inc$country_WB),]

#final check
#levels(factor(ind_all$country))


annotations_inc=merge(annotations_inc,ind_all,by="country",all.x=T)


read.csv("Households-by-number-of-children-2015-OECD-20220217.csv",sep=";")->nkids
for(i in 2:dim(nkids)[2]) nkids[,c(i)]<-as.numeric(as.character(nkids[,c(i)]))
nkids[grep("average",nkids$country,invert=T),]->nkids
nkids$anychildren<-rowSums(nkids[,c("X1.child","X2.children","X3.or.more.children")],na.rm=T)
nkids$total=nkids$X0.children+nkids$anychildren # this mostly checks out -- total is 100%
#% of single kids' household, out of households with any children
nkids$single=(nkids$X1.child/nkids$anychildren)*100
#summary(nkids)

#https://www.oecd.org/about/document/ratification-oecd-convention.htm
read.csv("oecd.txt",sep="\t",header=F, skip=1)->countries
countries$V1=tolower(gsub("^ ","", countries$V1))

annotations_inc$country_l=tolower(annotations_inc$country)

annotations_inc$oecd<-annotations_inc$country_l %in% countries$V1