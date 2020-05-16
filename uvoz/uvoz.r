# 2. faza: Uvoz podatkov
library(rvest)
library(gsubfn)
library(readr)
library(dplyr)
library(tidyr)


# WIKIPEDIJA : tabela zabeleženih dobitkov
# link <- "https://en.wikipedia.org/wiki/Eurojackpot"
# stran <- html_session(link) %>% read_html()
# tabela_wiki <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
#   .[[1]] %>% html_table(dec=",")
# 
# #2012
# link <-"https://www.loterija.si/LOTERIJA.asp?leto=2012&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
# stran <- html_session(link) %>% read_html()
# tabela2012 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()
# 
# #2013
# link <-"https://www.loterija.si/LOTERIJA.asp?leto=2013&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
# stran <- html_session(link) %>% read_html()
# tabela2013 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()
# 
# #2014
# link <-"https://www.loterija.si/LOTERIJA.asp?leto=2014&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
# stran <- html_session(link) %>% read_html()
# tabela2014 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()
# 
# #2015
# link <-"https://www.loterija.si/LOTERIJA.asp?leto=2015&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
# stran <- html_session(link) %>% read_html()
# tabela2015 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()
# 
# #2O16
# link <-"https://www.loterija.si/LOTERIJA.asp?leto=2016&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
# stran <- html_session(link) %>% read_html()
# tabela2016 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()
# 
# #2017
# link <-"https://www.loterija.si/LOTERIJA.asp?leto=2017&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
# stran <- html_session(link) %>% read_html()
# tabela2017 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()
# 
# #2018
# link <-"https://www.loterija.si/LOTERIJA.asp?leto=2018&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
# stran <- html_session(link) %>% read_html()
# tabela2018 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()

#ČIŠČENJE PODATKOV

#Čiščenje wiki tabele 
#tabela_wiki$Rank <- NULL
#tabela_wiki$'Prize in pound' <- NULL
#sl <- locale(date_format = "%AD", decimal_mark=",", grouping_mark=".")
#tabela_wiki[['Date']] <- parse_number(tabela_wiki[['Date']], locale=sl)
#tabela_wiki[['Prize in euro']]  Da bo numerično
#colnames(tabela_wiki) <- c("Leto","Nagrada(€)","Država")
#tabela <-rbind(tabela2012,tabela2013,tabela2014,tabela2015,tabela2016,tabela2017,tabela2018)

tabela <- NULL
for (leto in 2012:2018) {
  # pripravimo povezavo
  link <- sprintf("https://www.loterija.si/LOTERIJA.asp?leto=%d&lytoo=igre%%2Figre_z_zrebanji%%2Feurojackpot%%2Fstatistika%%2Feurojackpot_arhiv", leto)
  # pridobimo tabelo
  html.tabela <- html_session(link) %>% read_html() %>%
    html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]]
  # pripravimo seznam značk s podatki za 5+1
  spans <- html.tabela %>% html_nodes(xpath=".//span[@class='stats5-1']")
  # podatke za 5+1 spravimo v matriko
  stats51 <- spans %>% html_text() %>% matrix(ncol=3, byrow=TRUE) %>% trimws()
  # pobrišemo podatke za 5+1
  xml_text(spans) <- ""
  # pridobimo ostale podatke in dodamo stolpce za 5+1
  tabela.leto <- html.tabela %>% html_table() %>% cbind(stats51)
  # pridružimo k podatkom za prejšnja leta
  tabela <- rbind(tabela, tabela.leto)
}
tabela[['Datum']] <- parse_date(tabela[['Datum']],"%d.%m.%Y")
colnames(tabela) <- c('Datum','Krog', 'Številke','Število jackpot','Vrednost jackpot','Država jackpot',
                      'Število 5+1','Vrednost 5+1','Država 5+1')
#1.RAZPREDELNICA(DATUM, KROG, DOBITEK)
tabela1 <- tabela[c(1,2)]


#2.RAZPREDELNICA(DATUM, STEVILKA, TIP)
razpredelnica2 <-tabela[c(1,3)]
colnames(razpredelnica2)<- c("Datum","Številka")
locena <- razpredelnica2 %>% separate('Številka', c("Stevilka", "Stevilka2"), "- ", extra = "merge");
prvih_pet <- subset(locena, select = -c(Stevilka2))
prvih_pet <- prvih_pet %>% mutate(Tip = 'prvih_pet')
prvih_pet <- prvih_pet %>% separate('Stevilka', c("Stevilka", "Stevilka2","Stevilka3","Stevilka4","Stevilka5"), ",");
prvih_pet <- prvih_pet %>% gather(Nepomembno,Stevilka, Stevilka:Stevilka5)
prvih_pet <- subset(prvih_pet, select = -Nepomembno)

zadnje_dve <- subset(locena, select = -Stevilka)
zadnje_dve <- zadnje_dve %>% mutate(Tip = 'zadnje_dve')
zadnje_dve <- zadnje_dve %>% separate('Stevilka2', c("Stevilka1", "Stevilka2"), ",");
zadnje_dve <- zadnje_dve%>% gather(Nepomembno,Stevilka, Stevilka1:Stevilka2)
zadnje_dve <- subset(zadnje_dve, select = -Nepomembno)

tabela2 <-rbind(prvih_pet, zadnje_dve)
tabela2$Stevilka <- as.numeric(tabela2$Stevilka)

#3.RAZPREDELNICA(DATUM,DOBITEK, VREDNOST)

razpredelnica3 <- tabela[c(1,5)]
colnames(razpredelnica3)<- c("Datum","Vrednost")
razpredelnica3 <-razpredelnica3 %>% filter(Vrednost != '-')  #kjer ni bilo dobitka damo ven
razpredelnica3 <- razpredelnica3 %>% mutate(Dobitek = 'JACKPOT')

razpredelnicaa3 <- tabela[c(1,8)]
colnames(razpredelnicaa3)<- c("Datum","Vrednost")
razpredelnicaa3 <-razpredelnicaa3 %>% filter(Vrednost != '-')  #kjer ni bilo dobitka damo ven
razpredelnicaa3 <- razpredelnicaa3 %>% mutate(Dobitek = '5+1')

tabela3 <- rbind(razpredelnica3,razpredelnicaa3)
sl <- locale("sl", decimal_mark=",", grouping_mark=".")
tabela3[['Vrednost']] <- parse_number(tabela3[['Vrednost']], na="-", locale=sl)

#4.RAZPREDELNICA(DATUM, DOBITEK, STEVILO, DRŽAVA)
tabela4_jackpot <- tabela[c(1,4,6)]
colnames(tabela4_jackpot)<- c("Datum","Stevilo","Drzava")
tabela4_jackpot <- tabela4_jackpot %>% filter(Stevilo != '-')
tabela4_jackpot <- tabela4_jackpot %>% separate('Drzava', c("a", "b","c","d"), ",", extra="merge") %>% 
  gather(Nepomembno,Drzava, "a":"d") %>% drop_na(Drzava)
tabela4_jackpot <- subset(tabela4_jackpot, select = -Nepomembno)
vec_dobitkov <- parse_number(tabela4_jackpot$Drzava)      #parse_number-vzame število, če ni je NA
is.na_replace_1 <- vec_dobitkov                            
is.na_replace_1[is.na(is.na_replace_1)] <- 1 
tabela4_jackpot$Stevilo <- is.na_replace_1
tabela4_jackpot <- tabela4_jackpot %>% mutate(Dobitek = 'jackpot')

tabela4_pet <- tabela[c(1,7,9)]
colnames(tabela4_pet)<- c("Datum","Stevilo","Drzava")
tabela4_pet <- tabela4_pet %>% filter(Stevilo != '-')
tabela4_pet <- tabela4_pet %>%filter(Stevilo != "")
tabela4_pet <- tabela4_pet %>% separate('Drzava', c("a", "b","c","d","e","f","g","h"), ",", extra="merge") %>% 
  gather(Nepomembno,Drzava, "a":"h") %>% drop_na(Drzava)
tabela4_pet <- tabela4_pet %>% filter(Drzava != '')
tabela4_pet <- subset(tabela4_pet, select = -Nepomembno)
vec_dobitkov <- parse_number(tabela4_pet$Drzava)      #parse_number-vzame število, če ni je NA
is.na_replace_1 <- vec_dobitkov                            
is.na_replace_1[is.na(is.na_replace_1)] <- 1 
tabela4_pet$Stevilo <- is.na_replace_1
tabela4_pet <- tabela4_pet %>% mutate(Dobitek = '5+1')
tabela4_pet$Stevilo <- gsub("-", "", tabela4_pet$Stevilo)


tabela_koncna <- rbind(tabela4_jackpot,tabela4_pet)
tabela_koncna$Drzava <- gsub("[[:digit:]]x", "", tabela_koncna$Drzava)
tabela_koncna$Drzava<- gsub("-", "", tabela_koncna$Drzava)
tabela_koncna$Drzava<- gsub(":", "", tabela_koncna$Drzava)


#RAZPREDELNICA ZA SLOVENIJO (dodan tudi kraj)
c <- unlist(tabela_koncna$Drzava)
c <- trimws(c, which = c("both"))     #motijo presledki
tabela_koncna$Drzava <- c

tabela_koncna <- tabela_koncna %>% separate('Drzava', c("Drzava","Kraj"), "\\s", extra="merge")
tabela_koncna$Drzava<- gsub("Koper", "", tabela_koncna$Drzava)
slovenija<- tabela_koncna %>%filter(Drzava == "Slovenija")
slovenija$Drzava <- NULL
slovenija$Stevilo <- NULL
slovenija$Kraj<- gsub("\\)", "", slovenija$Kraj)
slovenija$Kraj<- gsub("\\(", "", slovenija$Kraj)

tabela4 <- subset(tabela_koncna, select = - Kraj)




# Če bi imeli več funkcij za uvoz in nekaterih npr. še ne bi
# potrebovali v 3. fazi, bi bilo smiselno funkcije dati v svojo
# datoteko, tukaj pa bi klicali tiste, ki jih potrebujemo v
# 2. fazi. Seveda bi morali ustrezno datoteko uvoziti v prihodnjih
# fazah.
