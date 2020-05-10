library(readr)
library(dplyr)
library(tidyr)


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
razpredelnica2 <-tabela[c(1,3)]
colnames(razpredelnica2)<- c("Datum","Številka")
locena <- razpredelnica2 %>% separate('Številka', c("Stevilka", "Stevilka2"), "- ", extra = "merge");
prvih_pet <- subset(locena, select = -c(Stevilka2))
prvih_pet$Tip <- c(rep('prvih_pet', 351))
prvih_pet <- prvih_pet %>% separate('Stevilka', c("Stevilka", "Stevilka2","Stevilka3","Stevilka4","Stevilka5"), ",");
prvih_pet <- prvih_pet%>% gather(Nepomembno,Stevilka, Stevilka:Stevilka5)
prvih_pet <- subset(prvih_pet, select = -Nepomembno)

zadnje_dve<-subset(locena, select = -Stevilka)
zadnje_dve$Tip <- c(rep('zadnje_dve', 351))
zadnje_dve<- zadnje_dve %>% separate('Stevilka2', c("Stevilka1", "Stevilka2"), ",");
zadnje_dve<- zadnje_dve%>% gather(Nepomembno,Stevilka, Stevilka1:Stevilka2)
zadnje_dve <- subset(zadnje_dve, select = -Nepomembno)

tabela2 <-rbind(prvih_pet, zadnje_dve)
#3.RAZPREDELNICA(DATUM,DOBITEK, VREDNOST)
razpredelnica3 <- tabela[c(1,2,5)]
colnames(razpredelnica3)<- c("Datum","Dobitek","Vrednost")
stevila <-apply(razpredelnica3[c(-1,-2)], 2, function(x){unlist(strsplit(x,"-"))})
stevila <- stevila[stevila != ""] 

#4.RAZPREDELNICA(DATUM, DOBITEK, STEVILO, DRŽAVA)
