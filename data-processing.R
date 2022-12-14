#Script for reading the different tables of the Short report titled "XX"
#authors : Georgia Loukatou | Alex Cristia | Naomi Havron| Camila Scaff 
#last date: "12/11/2022"
#RStudio 2022.07.1+554 "Spotted Wakerobin" Release (7872775ebddc40635780ca1ed238934c3345c5de, 2022-07-22) for macOS Mozilla/5.0 (Macintosh; Intel Mac OS X 11_6_1) AppleWebKit/537.36 (KHTML, like Gecko) QtWebEngine/5.12.10 Chrome/69.0.3497.128 Safari/537.36
#R version 4.1.0 (2021-05-18) -- "Camp Pontanezen"

## LOAD PACKAGES ####
library(dplyr)
library(readxl)
library(stringr)


## READ IN DATA AND ORGANIZE ####
# Read in data of CHILDES annotations
annotations<- read.csv("Table for authors - annotations.csv") 

#Clean columns
colnames(annotations)[colnames(annotations)=="X..of.children.with.siblings"]<-"Nb.of.children.with.siblings"
colnames(annotations)[colnames(annotations)=="X..of.children.with.older.siblings"]<-"Nb.of.children.with.older.siblings"
colnames(annotations)=gsub("\\.\\..*","",colnames(annotations))
colnames(annotations)[colnames(annotations)=="Our.coding.of"]<-"Our.coding.of.community.type"

annotations$Number.of.participants[annotations$Number.of.participants=="1 or 5"]<-NA
annotations$Number.of.participants[annotations$Number.of.participants=="1 CHI + MOT/FAT/INV"]<-1
annotations$Number.of.participants[annotations$Number.of.participants=="4 CHI + MOTs, some FATs, some SIBs"]<-4
annotations$Number.of.participants=as.numeric(as.character(annotations$Number.of.participants))

annotations$Nb.of.children.with.siblings=as.numeric(as.character(annotations$Nb.of.children.with.siblings))

annotations$Proportion.With.Siblings=annotations$Nb.of.children.with.siblings/annotations$Number.of.participants

annotations$Parental.profession=tolower(annotations$Parental.profession)
annotations$is.academic=annotations$is.health=annotations$is.teacher=NA
annotations$is.academic[grep("stud|prof|ling|investig|resear|sociol|academ|scienti|university|universities",annotations$Parental.profession)]<-"yes"
annotations$is.health[grep("psychology|doctor|therapist|nurse",annotations$Parental.profession)]<-"yes"
annotations$is.health[grep("teacher",annotations$Parental.profession)]<-"yes"
annotations$is.academic[annotations$Parental.profession=="pi"]<-"yes"
annotations$Parental.profession[annotations$Parental.profession %in% c("don't know")]<-NA

#table(annotations$Household.structure)
annotations$Household.structure=tolower(annotations$Household.structure)
annotations$Household.structure[grep("different|diverse|varied",annotations$Household.structure)] <- NA #turn to NA?
annotations$Household.structure[annotations$Household.structure %in% c("nuclear, extended","extended/nuclear")]<-"varied"
annotations$Household.structure[grep("nuclear|nucear|single",annotations$Household.structure)]<-"nuclear" # including single parent
annotations$Household.structure[grep("extended",annotations$Household.structure)]<-"extended"
annotations$Household.structure[annotations$Household.structure==""]<-NA


# Education & SES cleaning
annotations$Education.STDZD[annotations$Education.STDZD==""]<-NA
annotations$Education.STDZD[annotations$Education.STDZD=="4-6"]<-"4-5" #fix typo
annotations$Education.min<-as.numeric(as.character(gsub("-.*","",annotations$Education.STDZD)))
annotations$Education.max<-as.numeric(as.character(gsub(".*-","",annotations$Education.STDZD)))
annotations$Education.mid<-annotations$Education.min+(annotations$Education.max-annotations$Education.min)/2
# 1 = primary
# 2 = secondary school
# 3 = some college
# 4 = university
# 5 = postgraduate

annotations$Education.ac=NA
annotations$Education.ac[annotations$Education.min==1 & !is.na(annotations$Education.min)]<-"Some primary"
annotations$Education.ac[annotations$Education.min==2 & !is.na(annotations$Education.min)]<-"Some secondary"
annotations$Education.ac[annotations$Education.min==3 & !is.na(annotations$Education.min)]<-"Some college"
annotations$Education.ac[annotations$Education.min>3 & !is.na(annotations$Education.min)]<-"College and above"

annotations$SES.STDZD[annotations$SES.STDZD==""]<-NA
annotations$SES.STDZD[annotations$SES.STDZD=="middle class"]<-2

annotations$Average.number.of.siblings=as.numeric(as.character(gsub(",",".",annotations$Average.number.of.siblings)))

all_annotations <- annotations

annotations <- all_annotations %>%
  filter(Inclusion=="yes"|Inclusion=="Yes")

table(all_annotations$why_exclude)->excl

annotations$country <-NA
annotations$continent <-NA


annotations$country[grep("Ireland", annotations$Location)]  <- "Ireland"
annotations$country[grep("England|Arfon area Gwynedd, North Wales|Belfast, Northern Ireland|Nottingham/Manchester, England|England, Brighton|Wales|Cambridge, UK", annotations$Location)]  <- "United Kingdom"
annotations$country[grep("Portugal", annotations$Location)]  <- "Portugal"
annotations$country[grep("Spain|Madrid, Spain ; Tenerife, Canary Islands|Madrid, Spain|Navarra, Spain|Alt penedes, region of catalonia|spain| Lloret de mar|Barcelona| SPAIN|Salamanca|Lugo", annotations$Location)]  <- "Spain"
annotations$country[grep("France|France (Normandy, Marseille, + places visited)", annotations$Location)]  <- "France"
annotations$country[grep("Naples|italy|Roma|Italy", annotations$Location)]  <- "Italy"
annotations$country[grep("Switzerland", annotations$Location)]  <- "Switzerland"
annotations$country[grep("Belgium", annotations$Location)]  <- "Belgium"
annotations$country[grep("Germany", annotations$Location)]  <- "Germany"
annotations$country[grep("Netherlands", annotations$Location)]  <- "Netherlands"
annotations$country[grep("Austria", annotations$Location)]  <- "Austria"

#Following the EuroVoc classification https://en.wikipedia.org/wiki/EuroVoc + Italy and Spain and Portugal
annotations$continent[grep("Andorra|Austria|Belgium|France|Germany|Ireland|Italy|Liechtenstein|Luxembourg|Monaco|Netherlands|Portugal|Spain|Switzerland|United Kingdom", annotations$country)]  <- "Western Europe"


annotations$country[grep("Poznań, Poland", annotations$Location)]  <- "Poland"
annotations$country[grep("Estonia|Tartu, Estonia ; Rapla, Estonia|Southern Estonia|Tartu, Estonia", annotations$Location)]  <- "Estonia"
annotations$country[grep("Hungary", annotations$Location)]  <- "Hungary"
annotations$country[grep("Czech Republic", annotations$Location)]  <- "Czech Republic"
annotations$country[grep("Bucharest, Romania|Romania", annotations$Location)]  <- "Romania"
annotations$country[grep("Serbia", annotations$Location)]  <- "Serbia"
annotations$country[grep("Croatia", annotations$Location)]  <- "Croatia"
annotations$country[grep("Slovenia", annotations$Location)]  <- "Slovenia"

annotations$country[grep("Sweden", annotations$Location)]  <- "Sweden"
annotations$country[grep("Iceland", annotations$Location)]  <- "Iceland"
annotations$country[grep("Norway", annotations$Location)]  <- "Norway"
annotations$country[grep("Denmark", annotations$Location)]  <- "Denmark"

annotations$country[grep("Athens, Greece", annotations$Location)]  <- "Greece"

annotations$country[grep("Moscow, Russia", annotations$Location)]  <- "Russia"

annotations$continent[grep("Poland|Estonia|Hungary|Czech|Romania|Serbia|Croatia|Slovenia|Sweden|Iceland|Norway|Denmark|Greece|Russia", annotations$country)]  <- "Non-Western Europe"


annotations$country[grep("Turkey", annotations$Location)]  <- "Turkey"
annotations$country[grep("Iran", annotations$Location)]  <- "Iran"
annotations$country[grep("Israel|Yahud, Israel", annotations$Location)]  <- "Israel"
annotations$country[grep("Kuwait", annotations$Location)]  <- "Kuwait"

annotations$country[grep("Bombay, India", annotations$Location)]  <- "India"

annotations$country[grep("China", annotations$Location)]  <- "China"

annotations$country[grep("Singapore", annotations$Location)]  <- "Singapore"
annotations$country[grep("Indonesia", annotations$Location)]  <- "Indonesia"
annotations$country[grep("Bangkok, Thailand", annotations$Location)]  <- "Thailand"
annotations$country[grep("Osaka|Tokyo|Nagoya|osaka|Kusatsu City, Shiga Pref", annotations$Location)]  <- "Japan"
annotations$country[grep("Korea", annotations$Location)]  <- "Korea" #NOTE: one corpus just says "Korea", data collected in 2009-2011, assuming it's South Korean
annotations$country[grep("Hong-Kong", annotations$Location)]  <- "Hong Kong"
annotations$country[grep("Hong Kong", annotations$Location)]  <- "Hong Kong"
annotations$country[grep("Taiwan", annotations$Location)]  <- "Taiwan"

annotations$continent[grep("Turkey|Iran|Israel|Kuwait|India|China|Singapore|Indonesia|Thailand|Japan|Korea|Hong Kong|Taiwan", annotations$country)]  <- "Asia"


annotations$country[grep("Papua-New Guinea", annotations$Location)]  <- "Papua New Guinea"
annotations$continent[grep("Papua New Guinea", annotations$country)]  <- "Oceania"


annotations$country[grep("Alexandria, Egypt", annotations$Location)]  <- "Egypt"
annotations$country[grep("Mokhotlong, Lesotho", annotations$Location)]  <- "Lesotho"
annotations$country[grep("South Africa", annotations$Location)]  <- "South Africa"
annotations$continent[grep("Egypt|Lesotho|Africa", annotations$country)]  <- "Africa"


annotations$country[grep("Rio Cuarto, Cordoba, Argentina", annotations$Location)]  <- "Argentina"
annotations$country[grep("Sao Paulo", annotations$Location)]  <- "Brazil"
annotations$country[grep("Mexico", annotations$Location)]  <- "Mexico"
annotations$country[grep("Jamaica", annotations$Location)]  <- "Jamaica"
annotations$continent[grep("Argentina|Brazil|Mexico|Jamaica", annotations$country)]  <- "Latin America"

annotations$country[grep("Michigan, USA|USA, Northern Virginia|California, USA|washington dc|United States|USA|Washington|Maryland|San Fran|Cambridge MA|Honolulu, HI|usa|UCLA", annotations$Location)]  <- "United States"
annotations$country[grep("Canada|Montreal", annotations$Location)]  <- "Canada"
annotations$continent[grep("United States|Canada", annotations$country)]  <- "North America"


#Special cases
annotations$country[grep("Sweden ; Portugal", annotations$Location)]  <- "Sweden & Portugal"
annotations$country[grep("Spain (Lloret de Mar), Hungary (Kecskemét)", annotations$Location)]  <- "Spain & Hungary"
#We are leaving continent as NA (because one country is Western & the other Eastern Europe)


#to check if any left
#annotations[is.na(annotations$country),"Location"]
#annotations[is.na(annotations$continent),"Location"]



#we rewrite some of the cases in which multiple subgroups exist so that all combinations are represented
#unique(sort(annotations$Language.or.Languages.spoken.in.recordings))

annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings%in% c("British English","American English (Middle Atlantic mother, Texas father)")]<-"English"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings=="Catalan (and the variant of Spanish spoken in Barcelona)"]<-"Catalan/Spanish"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings=="Japanese (Kyoto dialect)"]<-"Japanese"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings=="Mandarin Chinese, influenced by the dialects of Jianghuai Mandarin and Cantonese to a minor degree"]<-"Mandarin"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings=="Norwegian (Children: mostly trøndersk, parents/care-takers: trøndersk, vestnorsk, nordnorsk, austnorsk)"]<-"Norwegian"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings=="Spanish Spain"]<-"Spanish"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings %in% c("European Portuguese","Brazilian Portuguese")]<-"Portuguese (Brazilian or European)"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings %in% c("Kuwaiti Arabic","Egyptian Arabic")]<-"Arabic (Egyptian or Kuwaiti)"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings %in% c("Spanish & Spanish/English")]<-"Spanish/English"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings %in% c("Dutch/French (N=31) and Dutch/English (N=3)")]<-"Dutch/English, Dutch/French"
annotations$Language.or.Languages.spoken.in.recordings[annotations$Language.or.Languages.spoken.in.recordings %in% c("Dutch/French & Dutch/Italian")]<-"Dutch/Italian"


lang_temp=levels(factor(annotations$Language.or.Languages.spoken.in.recordings))

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

annotations$Bilingualism.Multilingualism.in.corpus<-tolower(annotations$Bilingualism.Multilingualism.in.corpus)
annotations$Bilingualism.Multilingualism.in.corpus[grep("yes",annotations$Bilingualism.Multilingualism.in.corpus)]<-"yes"
annotations$Bilingualism.Multilingualism.in.corpus[grep("trilinguism",annotations$Bilingualism.Multilingualism.in.corpus)]<-"yes"
annotations$Bilingualism.Multilingualism.in.corpus[grep("no; a few words of yiddish and spanish interjected, less than once per session.",annotations$Bilingualism.Multilingualism.in.corpus)]<-"no"
annotations$Bilingualism.Multilingualism.in.corpus[annotations$Bilingualism.Multilingualism.in.corpus==""]<-NA




annotations$Type.of.community.at.the.time.of.the.recordings[is.na(annotations$Type.of.community.at.the.time.of.the.recordings)]<-annotations$STDZD.community.type[is.na(annotations$Type.of.community.at.the.time.of.the.recordings)]

annotations$Type.of.community.at.the.time.of.the.recordings[annotations$Type.of.community.at.the.time.of.the.recordings %in% c("academic","capital city of the Soviet Union","city","industial","industrial ","work-for-pay",
                                                                                                                               "industrial & service &trade activities...","industrial city (3.000.000 inhabitants",
                                                                                                                               "Industrial city (population: around 272,000 inhabitants)",
                                                                                                                               "Mediterranean city of 1.6 million inhabitants",
                                                                                                                               "modern city families, usually both working parents",
                                                                                                                               "Orthodox Jews",
                                                                                                                               "Seaside tourist town",
                                                                                                                               "work-for-pay/industrial","industrial","Northeastern US urban","urban western")]<-"urban"

annotations$Type.of.community.at.the.time.of.the.recordings[annotations$Type.of.community.at.the.time.of.the.recordings %in% c("farmer","rural","rural farming village")]<-"rural"

annotations$Type.of.community.at.the.time.of.the.recordings[annotations$Type.of.community.at.the.time.of.the.recordings %in% c("industrial, farmers","mainly urban")]<-"both"
annotations$Type.of.community.at.the.time.of.the.recordings[annotations$Type.of.community.at.the.time.of.the.recordings==""]<-NA
#table(annotations$Type.of.community.at.the.time.of.the.recordings)


byPart<- annotations %>%
  select(Language.or.Languages.spoken.in.recordings, Number.of.participants) %>%
  filter(!is.na(Number.of.participants)) %>%
  group_by(Language.or.Languages.spoken.in.recordings) %>%
  summarise(numpar = sum(as.numeric(Number.of.participants))) 

read.csv("country_iso.csv")->iso_lookup
codes=iso_lookup$Code
iso_lookup$Name<-as.character(iso_lookup$Name)
iso_lookup$Name=gsub(", Islamic Republic of","",iso_lookup$Name)
iso_lookup$Name=gsub("Russian Federation","Russia",iso_lookup$Name)
iso_lookup$Name=gsub(", Province of China","",iso_lookup$Name)
iso_lookup$Name=gsub(", Republic of","",iso_lookup$Name)
iso_lookup$Name=gsub("Congo, the Democratic Republic of the","Democratic Republic of Congo",iso_lookup$Name)

names(codes)<-iso_lookup$Name


annotations$country_WB<-codes[annotations$country]

annotations$country_WB <-factor(annotations$country_WB)
#annotations[is.na(annotations$country_WB),"country"]
#check - this one is NA bec it's data from 2 countries

annotations$continent <-factor(annotations$continent)


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


# https://ourworldindata.org/primary-and-secondary-education
read.csv("completion-rate-of-lower-secondary-education-OWID-20220303.csv")->ed_basic
ed_basic[ed_basic$Year>2006 & ed_basic$Year<2015,]->ed_basic
ed_basic[!duplicated(ed_basic$Entity),]->ed_basic
colnames(ed_basic)[colnames(ed_basic)=="Lower.secondary.completion.rate..total....of.relevant.age.group."]<-"Compl.LS"
ed_basic$Compl.LS[ed_basic$Compl.LS>100]<-100  #cap to 100%



#https://ourworldindata.org/grapher/political-regimes
read.csv("political-regimes-OWID-20220215.csv")->democr
democr[democr$Year==2011,]->democr

#https://ourworldindata.org/grapher/population-past-future
read.csv("population-past-future-OWID-20220217.csv")->pop
pop[pop$Year==2011,]->pop
colnames(pop)[colnames(pop)=="Population..historical.estimates.and.future.projections."]<-"Population"

#https://ourworldindata.org/tertiary-education
read.csv("share-of-the-population-with-completed-tertiary-education-OWID-20220217.csv")->ed
ed[ed$Year==2010,]->ed #note, no data for 2011, data only every 10 years
colnames(ed)[colnames(ed)=="Barro.Lee..Percentage.of.population.age.15..with.tertiary.schooling..Completed.Tertiary"]<-"College"




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
ind = ind_all[ind_all$iso2c %in% levels(annotations$country_WB),]

#final check
#levels(factor(ind_all$country))


annotations=merge(annotations,ind_all,by="country",all.x=T)


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

annotations$country_l=tolower(annotations$country)

annotations$oecd<-annotations$country_l %in% countries$V1