## Country level information script####

## LOAD PACKAGES ####
library(dplyr)
library(tidyr)
library(readxl)
library(stringr)
library(childesr)
library(ggplot2)
library(purrr)
require(scales)
library(kableExtra)
library(ggpubr)
library(plotly)
library(GGally)
library(ggthemes)
library(rjson)

## READ IN DATA AND ORGANIZE ####
rm(list=ls()) #clean your environment
# Read in data
annotations_inc<- read.csv("derived/annotations_included", sep="")
read.csv("ISO-3166-Countries-with-Regional-Codes.csv")->regions #https://github.com/lukes/ISO-3166-Countries-with-Regional-Codes/blob/master/all/all.csv
read.csv("wdi-data.csv")->wdi_all
## Read in Ourworld in data info on Education - Educated ####
# https://ourworldindata.org/primary-and-secondary-education
read.csv("completion-rate-of-lower-secondary-education-OWID-20220303.csv")->owid_ed_basic
UU_hh_composition <- read.csv("data/hh-size-composition.csv" ,sep=",") #https://population.un.org/household/#/countries/
#https://www.oecd.org/about/document/ratification-oecd-convention.htm
ocde <- read.csv("data/ocde_country.csv", sep=",")
read.csv("oecd.txt",sep="\t",header=F, skip=1)->countries

#Merge and cleanup codes
##Remove regions and areas from WDI and OWID 
merge(regions, wdi_all, by.x = "alpha.2",  by.y = "iso2c", all.x=T) -> wdi

#Choose only data between 2006 and 2015 for ed_basic 
owid_ed_basic[owid_ed_basic$Year>2006 & owid_ed_basic$Year<2015,]->owid_ed_basic

filtered_ed_basic <- owid_ed_basic %>%
  group_by(Entity) %>%
  filter(Year == max(Year)) #pic the latest available data if duplicates

merge(regions, filtered_ed_basic, by.x  = "alpha.3",by.y  = "Code", all.x=T) -> ed_basic

filtered_hh_composition <- UU_hh_composition %>%
  group_by(ISO.Code) %>%
  filter(year == max(year))
#some entries for the same country and year duplicates
#the DYB database seems the most complete so we are keeping that one in case of duplicates
filtered_hh_composition2 <- filtered_hh_composition %>%
  group_by(ISO.Code) %>%
  arrange(!("DYB" %in% Data.source.category), year) %>%
  distinct(ISO.Code, .keep_all = TRUE)

merge(regions, filtered_hh_composition2, by.x = "country.code", by.y = "ISO.Code", all.x=T) -> hh_composition

##ALL DATA SET: 
temp = merge(x=hh_composition,y=wdi[ , c("alpha.3","year","NY.GDP.PCAP.PP.KD","SE.SEC.CUAT.UP.ZS","SP.URB.TOTL.ZS"),],by = "alpha.3",all=T)

ind_all = merge(x=temp,y=ed_basic[ , c("alpha.3","Entity","Year","Lower.secondary.completion.rate..total....of.relevant.age.group."),],by = "alpha.3",all=T)

##SES####
#Education##
colnames(ind_all)[colnames(ind_all)=="Lower.secondary.completion.rate..total....of.relevant.age.group."]<-"Compl.LS"
ind_all$Compl.LS[ind_all$Compl.LS>100]<-100  #cap to 100%
#GDP##
ind_all$log_gdp=log(ind_all$NY.GDP.PCAP.PP.KD,10)
##Urbanization####
#ind_all$SP.URB.TOTL.ZS

##Family structure####
#Average number of household members of selected ages
#Under age 15 years among all households	:The average number of members under age 15 years (aged 0-14) per household, among all households
colnames(ind_all)[colnames(ind_all)=="Average.household.size..number.of.members."]<-"Avg.hs"
colnames(ind_all)[colnames(ind_all)=="Under.age.15.years.among.all.households"]<-"Avg.und15"

##Languages####
#NA
#CHILDES
ind = ind_all[ind_all$alpha.3 %in% annotations_inc$alpha.3,] #50 -2 (2 corpora biling) ALL IN :)


##Figure 3: Density plots ####

#GDP
dens_dat=data.frame(rbind(cbind(ind_all$log_gdp[!is.na(ind_all$log_gdp)],0),
                          cbind(ind$log_gdp[!is.na(ind$log_gdp)],1) ))
colnames(dens_dat)<-c("GDP.per.capita","in.childes_T")
dens_dat$in.childes=ifelse(dens_dat$in.childes,"CHILDES","all")
gdpt = t.test(dens_dat[,1]~dens_dat$in.childes)

rich <- ggplot(dens_dat, aes(x = GDP.per.capita, fill = in.childes)) + geom_density(alpha = 0.5) + theme(legend.position = "none", axis.title.y=element_blank() ) +labs( x = "GDP per capita (log 10)")+ scale_fill_colorblind()


#Education
dens_dat=data.frame(rbind(cbind(ind_all$Compl.LS[!is.na(ind_all$Compl.LS)],0),
                          cbind(ind$Compl.LS[!is.na(ind$Compl.LS)],1) ))
colnames(dens_dat)<-c("completed.LSS","in.childes_T")
dens_dat$in.childes=ifelse(dens_dat$in.childes,"CHILDES","all")
edt = t.test(dens_dat[,1]~dens_dat$in.childes)

ed <- ggplot(dens_dat, aes(x = completed.LSS, fill = in.childes)) + geom_density(alpha = 0.5) + theme(legend.position = c(0.25, 0.8), axis.title.y=element_blank() , legend.title = element_blank())+labs( x = "% compl. LSS")+ scale_fill_colorblind()

#urbanization
dens_dat=data.frame(rbind(cbind(ind_all$SP.URB.TOTL.ZS[!is.na(ind_all$SP.URB.TOTL.ZS)],0),
                          cbind(ind$SP.URB.TOTL.ZS[!is.na(ind$SP.URB.TOTL.ZS)],1) ))
colnames(dens_dat)<-c("pc.urban","in.childes_T")
dens_dat$in.childes=ifelse(dens_dat$in.childes,"CHILDES","all")
urbt = t.test(dens_dat[,1]~dens_dat$in.childes)

urb <- ggplot(dens_dat, aes(x = pc.urban, fill = in.childes)) + geom_density(alpha = 0.5) + theme(legend.position = "none", axis.title.y=element_blank() )+labs( x = "% urban")+ scale_fill_colorblind()

#Family structure
#% nuclear family
ind_all$nuclear = (ind_all$Couple.with.children + ind_all$Single.parent.with.children)
ind$nuclear = (ind$Couple.with.children + ind$Single.parent.with.children)

dens_dat=data.frame(rbind(cbind(ind_all$nuclear[!is.na(ind_all$nuclear)],0),
                          cbind(ind$nuclear[!is.na(ind$nuclear)],1) ))
colnames(dens_dat)<-c("nuclear","in.childes_T")
dens_dat$in.childes=ifelse(dens_dat$in.childes,"CHILDES","all")
nuclear = t.test(dens_dat[,1]~dens_dat$in.childes)

NUCLEAR <- ggplot(dens_dat, aes(x = nuclear, fill = in.childes)) + geom_density(alpha = 0.5)+ theme(legend.position = "none", axis.title.y=element_blank() ) +labs( x = "% Nuclear household with children")+ scale_fill_colorblind()

#% non nuclear family
ind_all$nnuclear = (ind_all$Extended.family + ind_all$Non.relatives)
ind$nnuclear = (ind$Extended.family + ind$Non.relatives)

dens_dat=data.frame(rbind(cbind(ind_all$nnuclear[!is.na(ind_all$nnuclear)],0),
                          cbind(ind$nnuclear[!is.na(ind$nnuclear)],1) ))
colnames(dens_dat)<-c("nnuclear","in.childes_T")
dens_dat$in.childes=ifelse(dens_dat$in.childes,"CHILDES","all")
nnuclear = t.test(dens_dat[,1]~dens_dat$in.childes)

NNUCLEAR <- ggplot(dens_dat, aes(x = nnuclear, fill = in.childes)) + geom_density(alpha = 0.5)+ theme(legend.position = "none", axis.title.y=element_blank() ) +labs( x = "% Extended or Non-relatives in the household")+ scale_fill_colorblind()


#Average household size
dens_dat=data.frame(rbind(cbind(ind_all$Avg.hs[!is.na(ind_all$Avg.hs)],0),
                          cbind(ind$Avg.hs[!is.na(ind$Avg.hs)],1) ))
colnames(dens_dat)<-c("Avg.hs","in.childes_T")
dens_dat$in.childes=ifelse(dens_dat$in.childes,"CHILDES","all")
hh = t.test(dens_dat[,1]~dens_dat$in.childes)

HH <- ggplot(dens_dat, aes(x = Avg.hs, fill = in.childes)) + geom_density(alpha = 0.5)+ theme(legend.position = "none", axis.title.y=element_blank() ) +labs( x = "Avg Household size")+ scale_fill_colorblind()

#Average household size
dens_dat=data.frame(rbind(cbind(ind_all$Avg.und15[!is.na(ind_all$Avg.und15)],0),
                          cbind(ind$Avg.und15[!is.na(ind$Avg.und15)],1) ))
colnames(dens_dat)<-c("Avg.und15","in.childes_T")
dens_dat$in.childes=ifelse(dens_dat$in.childes,"CHILDES","all")
und15 = t.test(dens_dat[,1]~dens_dat$in.childes)

UND15 <- ggplot(dens_dat, aes(x = Avg.und15, fill = in.childes)) + geom_density(alpha = 0.5)+ theme(legend.position = "none", axis.title.y=element_blank() ) +labs( x = "Avg # members >15 years per household ")+ scale_fill_colorblind()


# combined
x <- ggarrange(ed, urb, rich, HH,  
               labels = c("A", "B", "C", "D"),
               ncol = 2, nrow = 2)

write.table(ind_all,"derived/ind_all", row.names = FALSE, col.names = TRUE)
write.table(ind,"derived/ind", row.names = FALSE, col.names = TRUE)

##other education measure####

#https://ourworldindata.org/tertiary-education
# read.csv("share-of-the-population-with-completed-tertiary-education-OWID-20220217.csv")->ed
# ed[ed$Year==2010,]->ed #note, no data for 2011, data only every 10 years
# colnames(ed)[colnames(ed)=="Barro.Lee..Percentage.of.population.age.15..with.tertiary.schooling..Completed.Tertiary"]<-"College"
# 
##other family structure measure ####

# read.csv("Households-by-number-of-children-2015-OECD-20220217.csv",sep=";")->nkids
# for(i in 2:dim(nkids)[2]) nkids[,c(i)]<-as.numeric(as.character(nkids[,c(i)]))
# nkids[grep("average",nkids$country,invert=T),]->nkids
# nkids$anychildren<-rowSums(nkids[,c("X1.child","X2.children","X3.or.more.children")],na.rm=T)
# nkids$total=nkids$X0.children+nkids$anychildren # this mostly checks out -- total is 100%
# #% of single kids' household, out of households with any children
# nkids$single=(nkids$X1.child/nkids$anychildren)*100
# #summary(nkids)
# 
# ## Read in Ourworld in data info on Democracy - Democratic ####
# #https://ourworldindata.org/grapher/political-regimes
# read.csv("political-regimes-OWID-20220215.csv")->democr
# democr[democr$Year==2011,]->democr
# 
# ## Read in Ourworld in data info on Population Size  ####
# #https://ourworldindata.org/grapher/population-past-future
# read.csv("population-past-future-OWID-20220217.csv")->pop
# pop[pop$Year==2011,]->pop
# colnames(pop)[colnames(pop)=="Population..historical.estimates.and.future.projections."]<-"Population"
# 
# ## Read in Ourworld in data info on Children born per woman - Fertility ####
# read.csv("children-born-per-woman-OWID-20220112.csv")->cpw
# cpw[cpw$Year==2011,]->cpw
# colnames(cpw)[colnames(cpw)=="Fertility.rate..Select.Gapminder..v12...2017."]<-"Fertility"
# cpw$Entity<-gsub("Slovak Republic","Slovakia",cpw$Entity,fixed=T)
# 
# merge(ed_basic,democr[,c("Entity","Political.regime")],all=T)->owid
# merge(owid,pop[,c("Entity","Population")],all=T)->owid
# merge(owid,ed[,c("Entity","College")],all=T)->owid
# merge(owid,cpw[,c("Entity","Fertility")],all=T)->owid

#clean up

# owid$Political.regime_t<-NA
# owid$Political.regime_t[owid$Political.regime %in% c(0,1)]<-"autocracies"
# #owid$Political.regime_t[owid$Political.regime==1]<-"electoral \n autocracies"
# owid$Political.regime_t[owid$Political.regime%in% c(2,3)]<-"democracies"
# #owid$Political.regime_t[owid$Political.regime==3]<-"liberal \n democracies"




##archive ####

#remove areas
# wdi_all=wdi_all[!(wdi_all$country %in% c( "Africa Eastern and Southern" ,"Africa Western and Central",
#                                           "Arab World", "Caribbean small states", "Central Europe and the Baltics" ,
#                                           "European Union", "Fragile and conflict affected situations",
#                                           "OECD members", "West Bank and Gaza",
#                                           "Small states"  , "Pacific island small states" 
#                                           , "Caribbean small states" , "Other small states",
#                                           "Latin America & the Caribbean (IDA & IBRD countries)", "Middle East & North Africa (IDA & IBRD countries)"  ,
#                                           "East Asia & Pacific (IDA & IBRD countries)"     ,      "South Asia (IDA & IBRD)"    ,                        
#                                           "Sub-Saharan Africa (IDA & IBRD countries)"    ,        "Europe & Central Asia (IDA & IBRD countries)" ,
#                                           "Turks and Caicos Islands", 
#                                           "Euro area"      ,                                     
#                                           "High income"                               ,           "Heavily indebted poor countries (HIPC)"              
#                                           , "IBRD only"                                ,            "IDA total"                                           
#                                           , "IDA blend"                               ,            "IDA only"                                            
#                                           , "Latin America & Caribbean (excluding high income)",
#                                           "Middle East & North Africa"                          
#                                           , "IDA & IBRD total"  ,
#                                           "Europe & Central Asia"  ,  "Sub-Saharan Africa (excluding high income)"  ,"Sub-Saharan Africa"    ,                               "Africa Eastern and Southern",
#                                           "Least developed countries: UN classification"   ,      "Low income"                                          
#                                           , "Lower middle income"      ,                            "Low & middle income"                                 
#                                           , "Middle income"          ,                              "Middle East & North Africa (excluding high income)"  
#                                           , "Upper middle income"     ,                             "North America"                                       
#                                           , "Not classified"                                             
#                                           , "East Asia & Pacific", "Pre-demographic dividend" , "Early-demographic dividend" ,"Late-demographic dividend"       , "Post-demographic dividend" , "World","East Asia & Pacific (excluding high income)"  , "Europe & Central Asia (excluding high income)","Latin America & Caribbean","Western Sahara","World" ,"South Asia"
# )),]
# #cleanup
# wdi_all$country[wdi_all$country=="Congo, Dem. Rep."]<-"Democratic Republic of Congo"
# wdi_all$country[wdi_all$country=="Congo, Rep."]<-"Congo"
# wdi_all$country[wdi_all$country=="Lao PDR"]<-"Laos"
# wdi_all$country[wdi_all$country=="Virgin Islands (U.S.)"]<-"United States Virgin Islands"
# wdi_all$country<-gsub(", The","",wdi_all$country)
# wdi_all$country<-gsub(", RB","",wdi_all$country)
# wdi_all$country<-gsub(", Arab Rep.","",wdi_all$country)
# wdi_all$country<-gsub(", Islamic Rep.","",wdi_all$country)
# wdi_all$country<-gsub(" SAR, China","",wdi_all$country)
# wdi_all$country<-gsub("St.","Saint",wdi_all$country,fixed=T)
# wdi_all$country<-gsub(", Fed. Sts.","",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Kyrgyz Republic","Kyrgyzstan",wdi_all$country,fixed=T)
# wdi_all$country<-gsub(" Darussalam","",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Cabo Verde","Cape Verde",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Czech Republic","Czechia",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Korea, Dem. People's Rep.","North Korea",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Korea, Rep.","South Korea",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Yemen, Rep.","Yemen",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Syrian Arab Republic","Syria",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Russian Federation","Russia",wdi_all$country,fixed=T)
# wdi_all$country<-gsub("Slovak Republic","Slovakia",wdi_all$country,fixed=T)
# 
# 
# owid=owid[!(owid$Entity%in% c("World","Western Sahara","Africa" ,"Asia","Oceania","South America",
#                               "Europe", "North America"  , "Arab World" ,"Caribbean Small States"  , "Central Europe and the Baltics" ,"Early-demographic dividend", "East Asia & Pacific" ,
#                               "East Asia & Pacific (excluding high income)", "East Asia & Pacific (IDA & IBRD)", "Euro area", "Europe & Central Asia", "Europe & Central Asia (excluding high income)",
#                               "Europe & Central Asia (IDA & IBRD)", "European Union",
#                               "Fragile and conflict affected situations" , "Heavily indebted poor countries (HIPC)","High income" , "IBRD only"    ,                                     
#                               "IDA & IBRD total"    ,                              
#                               "IDA blend"           ,                              
#                               "IDA only"           ,                               
#                               "Late-demographic dividend"              ,           
#                               "Latin America & Caribbean"     ,                    
#                               "Latin America & Caribbean (excluding high income)" ,
#                               "Latin America & Caribbean (IDA & IBRD)"       ,     
#                               "Least developed countries: UN classification"      ,
#                               "Low & middle income"         ,                      
#                               "Low income"             ,                           
#                               "Lower middle income"     ,
#                               "Middle East & North Africa"        ,                
#                               "Middle East & North Africa (excluding high income)",
#                               "Middle East & North Africa (IDA & IBRD)"           ,
#                               "Middle income",
#                               "OECD members"   ,                                   
#                               "Other small states"       ,                         
#                               "Pacific island small states"    , "Post-demographic dividend"    ,                     
#                               "Pre-demographic dividend"     ,
#                               "South Asia"   ,                                     
#                               "South Asia (IDA & IBRD)"       ,                    
#                               "Sub-Saharan Africa"    ,                            
#                               "Sub-Saharan Africa (excluding high income)"        ,
#                               "Sub-Saharan Africa (IDA & IBRD)"  ,   
#                               "Upper middle income" 
# )),]
# 

# ind_all$simple_region=ind_all$sub.region
# ind_all$simple_region[ind_all$sub.region%in% c("Australia and New Zealand","Melanesia","Micronesia","Polynesia")]<-"Oceania"
# 
# ind_all$simple_region[grep("Asia",ind_all$sub.region)]<-"Asia"
# ind_all$simple_region[grep("Africa",ind_all$sub.region)]<-"Africa"
# 
# ind_all$simple_region[ind_all$sub.region%in% c("Southern Europe","Eastern Europe","Northern Europe")]<-"Non-Western Europe"
# 
# ind_all$simple_region[ind_all$country%in% c("Spain","Italy","Portugal")]<-"Western Europe"
# 
# 
# ind_all$log2_fert=log(ind_all$Fertility,2)
# ind_all$log_pop=log(ind_all$Population,10)
# 
# ind_all$western<-"non-Western"
# ind_all$western[ind_all$simple_region %in% c('Western Europe',"Northern America")]<-"Western"
# ind_all$western[ind_all$country %in% c('Norway',"Australia","New Zealand","Iceland","Sweden","Denmark")]<-"Western"
# ind_all$western[ind_all$country %in% c('Bermuda',"Greenland")]<-"non-Western"
# ind_all$western=factor(ind_all$western)
