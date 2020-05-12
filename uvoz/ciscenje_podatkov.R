library(readr)
library(dplyr)
library(tidyr)
library(rvest)
library(gsubfn)


#Čiščenje wiki tabele 
#tabela_wiki$Rank <- NULL
#tabela_wiki$'Prize in pound' <- NULL
#sl <- locale(date_format = "%AD", decimal_mark=",", grouping_mark=".")
#tabela_wiki[['Date']] <- parse_number(tabela_wiki[['Date']], locale=sl)
#tabela_wiki[['Prize in euro']]  Da bo numerično
#colnames(tabela_wiki) <- c("Leto","Nagrada(€)","Država")



tabela <-rbind(tabela2012,tabela2013,tabela2014,tabela2015,tabela2016,tabela2017,tabela2018)
tabela[['Datum']] <- parse_date(tabela[['Datum']],"%d.%m.%Y")

#1.RAZPREDELNICA(DATUM, KROG, DOBITEK)
tabela1 <- tabela[c(1,2)]


#2.RAZPREDELNICA(DATUM, STEVILKA, TIP)
razpredelnica2 <-tabela[c(1,3)]
colnames(razpredelnica2)<- c("Datum","Številka")
locena <- razpredelnica2 %>% separate('Številka', c("Stevilka", "Stevilka2"), "- ", extra = "merge");
prvih_pet <- subset(locena, select = -c(Stevilka2))
prvih_pet$Tip <- c(rep('prvih_pet', 351))
prvih_pet <- prvih_pet %>% separate('Stevilka', c("Stevilka", "Stevilka2","Stevilka3","Stevilka4","Stevilka5"), ",");
prvih_pet <- prvih_pet %>% gather(Nepomembno,Stevilka, Stevilka:Stevilka5)
prvih_pet <- subset(prvih_pet, select = -Nepomembno)

zadnje_dve <- subset(locena, select = -Stevilka)
zadnje_dve$Tip <- c(rep('zadnje_dve', 351))
zadnje_dve <- zadnje_dve %>% separate('Stevilka2', c("Stevilka1", "Stevilka2"), ",");
zadnje_dve <- zadnje_dve%>% gather(Nepomembno,Stevilka, Stevilka1:Stevilka2)
zadnje_dve <- subset(zadnje_dve, select = -Nepomembno)

tabela2 <-rbind(prvih_pet, zadnje_dve)
tabela2$Stevilka <- as.numeric(tabela2$Stevilka)

#3.RAZPREDELNICA(DATUM,DOBITEK, VREDNOST)

razpredelnica3 <- tabela[c(1,2,5)]
colnames(razpredelnica3)<- c("Datum","Dobitek","Vrednost")
razpredelnica3 <-razpredelnica3 %>% filter(Vrednost != '--')  #kjer ni bilo dobitka damo ven
#write_csv(razpredelnica3, "razpredelnica.csv", na="")
razpredelnica <- read_csv("razpredelnica.csv")
razpredelnica3 <- razpredelnica %>% separate(!!'Vrednost', c("JACKPOT", "5+1"), "-");
razpredelnica3 <- razpredelnica3%>% gather(Dobitek,Vrednost, "JACKPOT":"5+1")
tabela3 <- razpredelnica3 %>% filter(Vrednost != "")
sl <- locale("sl", decimal_mark=",", grouping_mark=".")
tabela3[['Vrednost']] <- parse_number(tabela3[['Vrednost']], na="-", locale=sl)



#4.RAZPREDELNICA(DATUM, DOBITEK, STEVILO, DRŽAVA)
tabela4 <- tabela[c(1,4,6)]
colnames(tabela4)<- c("Datum","Stevilo","Drzava")
tabela4 <-tabela4 %>% filter(Stevilo != '--')
#write_csv(tabela4, "razpredelnica2.csv", na="")
razpredelnica4 <- read_csv("razpredelnica2.csv")
tabelaa<- razpredelnica4 %>% separate('Stevilo', c("JACKPOT", "5+1"), "-")
tabelaaa<-razpredelnica4 %>% separate('Drzava', c("dr_JACKPOT", "dr_5"), "-")

jackpot<-tabelaa[c(1,2)]
jackpot$Drzava<-tabelaaa$dr_JACKPOT
jackpot <- jackpot %>% drop_na(JACKPOT)
jackpot <- jackpot %>% filter(JACKPOT != "")
colnames(jackpot) <- c("datum","stevilo","drzava")
jackpot <- jackpot %>% separate('drzava', c("a", "b","c","d"), ",", extra="merge")
jackpot <- jackpot %>% gather(Nepomembno,drzava, "a":"d")
jackpot <- jackpot %>% drop_na(drzava)
jackpot <- subset(jackpot, select = -Nepomembno)
c <- parse_number(jackpot$drzava)      #parse_number-vzame število, če ni je NA
is.na_replace_1 <- c                            
is.na_replace_1[is.na(is.na_replace_1)] <- 1 
jackpot$stevilo <- is.na_replace_1
jackpot <- jackpot %>% mutate(dobitek = 'jackpot')

pet<-tabelaa[c(1,3)]
pet$Drzava <- tabelaaa$dr_5
colnames(pet) <- c("datum","stevilo","drzava")
pet <- pet %>%filter(stevilo != "")
pet <- pet %>% separate('drzava', c("a", "b","c","d","e", "f","g","h"), ",", extra="merge")
pet <- pet%>% gather(Nepomembno,drzava, "a":"h")
pet <- pet %>%drop_na(drzava)
pet <- subset(pet, select = -Nepomembno)
c <- parse_number(pet$drzava)
is.na_replace_1 <- c                            
is.na_replace_1[is.na(is.na_replace_1)] <- 1 
pet$stevilo<-is.na_replace_1
pet <- pet %>%filter(drzava != "")  %>% mutate(dobitek = '5+1')


tabela_koncna <- rbind(jackpot,pet)
tabela_koncna$drzava <- gsub("[[:digit:]]x", "", tabela_koncna$drzava)


#RAZPREDELNICA ZA SLOVENIJO (dodan tudi kraj)
tabela_koncna <- tabela_koncna %>% separate('drzava', c("drzava","kraj"), "\\(", extra="merge")
tabela_koncna$kraj<- gsub("\\)", "", tabela_koncna$kraj)
slovenija<- tabela_koncna %>%filter(drzava == "Slovenija")

tabela4 <- subset(tabela_koncna, select = - kraj)

