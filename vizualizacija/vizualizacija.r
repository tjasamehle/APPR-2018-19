# 3. faza: Vizualizacija podatkov
require(dplyr)
require(tidyr)
require(readr)
require(reshape2)

library(rgdal)
library(maptools)
library(ggpubr)
library(ggplot2)
library(digest)


#Število dobitkov JACKPOT in 5+1 v zadnjih letih
u <- tabela3[c(1,3)]
u$Datum<-gsub("-([[:digit:]]{1,2})-([[:digit:]]{1,2})","", u$Datum) #rabim samo leta
t <- rbind(u %>% filter(Dobitek == 'JACKPOT') %>% group_by(Datum) %>%
             summarise(Stevilo = sum(Dobitek == 'JACKPOT' )) %>% mutate(Kategorija="JACKPOT"),
           u %>% filter(Dobitek == '5+1') %>% group_by(Datum) %>% summarise(Stevilo = sum(Dobitek == '5+1' )) %>% mutate(Kategorija="5+1"))
t$Datum <- as.factor(t$Datum)

graf_dobitki <- ggplot(data = t, aes(x = Datum, y = Stevilo, fill= Kategorija))+ geom_col(position = "dodge") + 
  xlab("Leto") + ylab("Število dobitkov") + 
  scale_fill_manual("Kategorija",values = c('darkblue', 'lightblue')) +
  labs(title ="Število dobitkov") + theme(plot.title = element_text(hjust = 0.5)) 


#Pogostost glavnih številk
prvih<- tabela2 %>% filter(Tip == 'prvih_pet') %>% count(Stevilka)
prvih$n <- prvih$n/50
theme_set(theme_bw())
pogostost_prvih <- ggplot(prvih, aes(x=Stevilka, y= n)) + 
  geom_point(aes(col=Stevilka, size=n)) + 
  geom_smooth(method="loess", color ='red', se=F,fullrange = TRUE) + 
  labs(title="Frekvenca glavnih številk", 
       y="Frekvenca", 
       x="Številka")
pogostost_prvih<- pogostost_prvih + scale_x_discrete(limits=c((1:50),2))

#Pogostost euro(zadnjih dveh) številk
zadnje<- tabela2 %>% filter(Tip == 'zadnje_dve') %>% count(Stevilka)
zadnje$n <- zadnje$n/10
theme_set(theme_bw())
pogostost_zadnjih <- ggplot(zadnje, aes(x=Stevilka, y= n)) +
                              geom_point(aes(col=Stevilka, size=n)) + 
                              geom_smooth(method="loess", color ='blue', se=F,fullrange = TRUE) + 
                              labs(title="Frekvenca euro številk", y="Frekvenca",x="Številka")
pogostost_zadnjih<- pogostost_zadnjih + scale_x_discrete(limits=c(1:10))

#višina eurojackpot dobitka po letih
visina <- tabela3
options(scipen=999)
visina$Vrednost <- visina$Vrednost/1000000
visina_jackpot <- ggplot(visina, aes(x=Datum, y= Vrednost)) + 
  geom_point(aes(col=Dobitek)) + 
  labs(title="Višina dobitkov v mio €", 
       y="mio €", 
       x="Leto")


#Koliko denarja letno izplača Eurojackpot

denar <- tabela3[c(1,2)]
denar$Datum<-gsub("-([[:digit:]]{1,2})-([[:digit:]]{1,2})","", u$Datum)
denar <- denar %>% group_by(Datum) %>% summarise(Vsota = sum(Vrednost)) 
denar$Vsota <- denar$Vsota /1000000
denar$Datum <- as.factor(denar$Datum)

graf<- ggplot(data = denar, aes(x = Datum, y =Vsota, fill ='lightpink'))+ 
  geom_col() + 
  xlab("Leto") + ylab("Vsota dobitkov v mio €") + 
  labs(title ="Vsota dobitkov po letih") + theme(plot.title = element_text(hjust = 0.5)) 


 
# Uvozimo zemljevid.
#source("https://raw.githubusercontent.com/jaanos/APPR-2018-19/master/lib/uvozi.zemljevid.r")

zemljevid <- uvozi.zemljevid("http://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/cultural/ne_50m_admin_0_countries.zip",
                             "ne_50m_admin_0_countries", mapa = "zemljevidi", pot.zemljevida = "", encoding = "UTF-8") %>% 
  fortify() %>% filter(CONTINENT == "Europe" | SOVEREIGNT %in% c("Cyprus"), long < 45 & long > -20 & lat > 30 & lat < 75)
colnames(zemljevid)[11] <- 'drzava'
zemljevid$drzava <- as.character(zemljevid$drzava)



#Zemljevid dobljenih jackpotov po državi

zadetki_po_drzavah <- tabela4 %>% filter(Dobitek == 'jackpot')
zadetki_po_drzavah <- zadetki_po_drzavah %>% group_by(Drzava) %>% summarise(Stevilo = sum(Dobitek == 'jackpot' ))
zadetki_po_drzavah$Drzava <- gsub("Češka", "Czechia", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Danska", "Denmark", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Finska", "Finland", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Hrvaška", "Croatia", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Italija", "Italy", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Madžarska", "Hungary", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Nemčija", "Germany", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Nizozemska", "Netherlands", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Norveška", "Norway", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Slovenija", "Slovenia", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Španija", "Spain", zadetki_po_drzavah$Drzava)
zadetki_po_drzavah$Drzava <- gsub("Švedska", "Sweden", zadetki_po_drzavah$Drzava)

zemljevid_zadetki_jackpot <- ggplot() +
  geom_polygon(data = zadetki_po_drzavah %>% right_join(zemljevid, by = c("Drzava" = "drzava")),
               aes(x = long, y = lat, group = group, fill = Stevilo), alpha = 0.8, color = "black")+
  scale_fill_gradient2(low = "green", mid = "yellow", high = "red", midpoint = 7) + 
  xlab("") + ylab("") + ggtitle("Jackpot zadetki po državah")+
  guides(fill=guide_legend(title="Število zadetkov")) + theme(plot.title = element_text(hjust = 0.5))

#zemljevid_zadetki_jackpot

#Število zadetkov '5+1' po državah
zadetki_po_drzavah2 <- tabela4 %>% filter(Dobitek == '5+1')
zadetki_po_drzavah2 <- zadetki_po_drzavah2 %>% group_by(Drzava) %>% summarise(Stevilo = sum(Dobitek == '5+1' ))
zadetki_po_drzavah2 <- zadetki_po_drzavah2[-c(12,13),]
zadetki_po_drzavah2$Drzava <- gsub("Češka", "Czechia", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Danska", "Denmark", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Finska", "Finland", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Hrvaška", "Croatia", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Italija", "Italy", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Madžarska", "Hungary", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Nemčija", "Germany", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Nizozemska", "Netherlands", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Norveška", "Norway", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Slovenija", "Slovenia", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Španija", "Spain", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Švedska", "Sweden", zadetki_po_drzavah2$Drzava)

zadetki_po_drzavah2$Drzava <- gsub("Slovaška", "Slovakia", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Poljska", "Poland", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Litva", "Lithuania", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Latvija", "Latvia", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Islandija", "Iceland", zadetki_po_drzavah2$Drzava)
zadetki_po_drzavah2$Drzava <- gsub("Estonija", "Estonia", zadetki_po_drzavah2$Drzava)


zemljevid_zadetki_pet <- ggplot() +
  geom_polygon(data = zadetki_po_drzavah2 %>% right_join(zemljevid, by = c("Drzava" = "drzava")),
               aes(x = long, y = lat, group = group, fill = Stevilo), alpha = 0.8, color = "black")+
  scale_fill_gradient2(low = "pink", mid = "yellow", high = "red", midpoint = 10) + 
  xlab("") + ylab("") + ggtitle("Zadetki 5+1 po državah")+
  guides(fill=guide_legend(title="Število zadetkov")) + theme(plot.title = element_text(hjust = 0.5))

#zemljevid_zadetki_pet

