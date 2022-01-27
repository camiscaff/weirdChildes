library(WDI)

# Indices
# WDIsearch('gdp')
#"GDP per capita, PPP (constant 2005 international $)"
# WDIsearch('Educational attainment, at least')
# Naomi: secondary, some college, university, 
# WDI:  SE.SEC.CUAT.LO.ZS    "Educational attainment, at least completed lower secondary, population 25+, total (%) (cumulative)" -- 62 countries are NOT NA
# WDI:  SE.SEC.CUAT.UP.ZS   Educational attainment, at least completed upper secondary, population 25+, total (%) (cumulative) -- 66 countries are NOT NA
# WDI:  SE.TER.CUAT.BA.ZS   Educational attainment, at least Bachelor's or equivalent, population 25+, total (%) (cumulative) -- only 25 countries are not NA
# note, only 23 countries have all three... 

## ADD
# "SH.FPL.IDLC.Q3" "Mean ideal number of children (per woman): Q3"
# "SP.DYN.CEBN.Q3" "Mean number of children ever born to women aged 40-49: Q3" 


dat_all = WDI(indicator=c('NY.GDP.PCAP.PP.KD','SE.SEC.CUAT.UP.ZS','SP.RUR.TOTL.ZS','5.01.01.01.indust','SP.DYN.CEBN.Q3'), start=2011, end=2011)

#test= WDI(indicator=c('SE.SEC.CUAT.UP.ZS'))
#sort(table(test$year[!is.na(test$SE.SEC.CUAT.UP.ZS)]))

# flip %rur
dat_all$SP.URB.TOTL.ZS=100-dat_all$SP.RUR.TOTL.ZS

write.csv(dat_all,"wdi-data.csv")

