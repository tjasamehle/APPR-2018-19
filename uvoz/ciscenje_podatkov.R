library(readr)
library(dplyr)
library(tidyr)
library(rvest)
library(gsubfn)


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
tabela1 <- tabela[c(1,2)]

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
tabela2$Stevilka <- as.numeric(tabela2$Stevilka)
#3.RAZPREDELNICA(DATUM,DOBITEK, VREDNOST)

razpredelnica3 <- tabela[c(1,2,5)]
colnames(razpredelnica3)<- c("Datum","Dobitek","Vrednost")
razpredelnica3 <-razpredelnica3 %>% filter(Vrednost != '--')  #kjer ni bilo dobitka damo ven
#write_csv(razpredelnica3, "razpredelnica.csv", na="")
razpredelnica <- read_csv("razpredelnica.csv")
razpredelnica3<- razpredelnica %>% separate(!!'Vrednost', c("JACKPOT", "5+1"), "-");
razpredelnica3<- razpredelnica3%>% gather(Dobitek,Vrednost, "JACKPOT":"5+1")
tabela3 <- razpredelnica3 %>% filter(Vrednost != "")
sl <- locale("sl", decimal_mark=",", grouping_mark=".")
tabela3[['Vrednost']] <- parse_number(tabela3[['Vrednost']], na="-", locale=sl)
tabela3$Vrednost<- as.numeric(tabela3$Vrednost)


#4.RAZPREDELNICA(DATUM, DOBITEK, STEVILO, DRŽAVA)
tabela4 <- tabela[c(1,4,6)]
colnames(tabela4)<- c("Datum","Stevilo","Drzava")
tabela4 <-tabela4 %>% filter(Stevilo != '--')
#write_csv(tabela4, "razpredelnica2.csv", na="")
razpredelnica4 <- read_csv("razpredelnica2.csv")
tabelaa<- razpredelnica4 %>% separate('Stevilo', c("JACKPOT", "5+1"), "-")
tabelaa<-tabelaa %>% separate('Drzava', c("dr_JACKPOT", "dr_5"), "-")
tabelaa<- tabelaa%>% gather(Dobitek,Stevilo, "JACKPOT":"5+1")
tabelaa<-tabelaa %>% filter(Stevilo != "");
tabelaa<- tabelaa%>% gather(Nepomembno,Drzava, "dr_JACKPOT":"dr_5")
tabelaa<-tabelaa %>% filter(Drzava != "");
tabelaa<-subset(tabelaa, select = -Nepomembno)


tabelaa$Stevilo <- as.numeric(tabelaa$Stevilo)
u <-tabelaa %>% filter(Stevilo >1);
u<-u %>% separate('Drzava', c("a", "b","c","d","e","f","g","h"), ",", extra="merge")
u<- u%>% gather(Nepomembno,Drzava, "a":"h")
u<-u %>% drop_na(Drzava);
u<-subset(u, select = -Nepomembno)

#drzava <- tabelaa$Drzava %>% strapplyc("([[:alpha:]]+)")
#stevilo

