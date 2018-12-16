library(readr)

#Čiščenje wiki tabele
tabela_wiki$Rank <- NULL
tabela_wiki$'Prize in pound' <- NULL
sl <- locale(date_format = "%AD", decimal_mark=",", grouping_mark=".")
tabela_wiki[['Date']] <- parse_number(tabela_wiki[['Date']], locale=sl)
#tabela_wiki[['Prize in euro']]  Da bo numerično
colnames(tabela_wiki) <- c("Leto","Nagrada(€)","Država")


tabela <-rbind(tabela2012,tabela2013,tabela2014,tabela2015,tabela2016,tabela2017,tabela2018)
tabela[['Datum']] <- parse_date(tabela[['Datum']],"%d.%m.%Y")

#1.RAZPREDELNICA(DATUM in KROG)
razpredelnica1 <- tabela[c(1,2)]

#2.RAZPREDELNICA(DATUM, STEVILKA, TIP)
razpredelnica2 <-tabela[c(1,3,4)]
colnames(razpredelnica2)<- c("Datum","Številka","Tip")
