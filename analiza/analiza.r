# 4. faza: Analiza podatkov
pojavi_v_jackpot <- tabela[c(1,3,4)]
colnames(pojavi_v_jackpot) <- c('Datum','Številke','Število')
pojavi_v_jackpot <- pojavi_v_jackpot %>% separate('Številke', c("JACKPOT", "5+1"), "-") %>%
  filter(Število != '--') %>% filter(Število >0)      #dobila števila ki so bila na dobitnih listkih
pojavi_v_jackpot <- subset(pojavi_v_jackpot, select = -Število)

pojavi_v_jackpot_prvih<- pojavi_v_jackpot[,-3] %>% separate('JACKPOT', c("a", "b","c","d","e"), ",") %>% 
  gather(Nepomembno,Stevilka, "a":"e")
pojavi_v_jackpot_prvih <- subset(pojavi_v_jackpot_prvih, select = -Nepomembno)

pojavi_v_jackpot_zadnje<- pojavi_v_jackpot[,-2] %>% separate('5+1', c("a", "b"), ",") %>% 
  gather(Nepomembno,Stevilka, "a":"b")
pojavi_v_jackpot_zadnje <- subset(pojavi_v_jackpot_zadnje, select = -Nepomembno)
pojavi_v_jackpot_zadnje$Stevilka <- as.numeric(pojavi_v_jackpot_zadnje$Stevilka)
pojavi_v_jackpot_prvih$Stevilka <- as.numeric(pojavi_v_jackpot_prvih$Stevilka)

#kolikokrat na dobitnih listkih
pojavi_v_jackpot_prvih <- pojavi_v_jackpot_prvih %>% count(Stevilka)
pojavi_v_jackpot_zadnje <- pojavi_v_jackpot_zadnje %>% count(Stevilka)
colnames(pojavi_v_jackpot_prvih) <- c('Stevilka','Bila na dobitnem Jackpot listku')
colnames(pojavi_v_jackpot_zadnje) <- c('Stevilka','Bila na dobitnem Jackpot listku')

#kolikokrat na listkih
pojavi_prva<- tabela2 %>% filter(Tip == 'prvih_pet') %>% count(Stevilka)
pojavi_zadnja <- tabela2 %>% filter(Tip == 'zadnje_dve') %>% count(Stevilka)
colnames(pojavi_prva) <- c('Stevilka','Bila izžrebana')
colnames(pojavi_zadnja) <- c('Stevilka','Bila izžrebana')
#nazadnje zrebana
nazadnje_prva <-tabela2 %>% filter(Tip == 'prvih_pet') %>% group_by(Stevilka) %>% 
  summarise('Bila nazadnje izžrebana' = max(Datum) )

nazadnje_zadnja <- nazadnje_zadnja <-tabela2 %>% filter(Tip == 'zadnje_dve') %>% group_by(Stevilka) %>% 
  summarise('Bila nazadnje izžrebana' = max(Datum) )  

tabela_prve <- left_join(pojavi_prva,pojavi_v_jackpot_prvih, by = c('Stevilka'='Stevilka') )
tabela_prve <- left_join(tabela_prve, nazadnje_prva, by = c('Stevilka'='Stevilka') )


tabela_zadnje <- left_join(pojavi_zadnja,pojavi_v_jackpot_zadnje, by = c('Stevilka'='Stevilka') )
tabela_zadnje <- left_join(tabela_zadnje, nazadnje_zadnja, by = c('Stevilka'='Stevilka') )

#s katero številko največkrat v paru(za zadnje dve)
par <- tabela2 %>% filter(Tip == 'zadnje_dve') %>% group_by(Datum) %>% summarise ( 
  'par1' = min(Stevilka), 'par2' =max(Stevilka))
par <-par %>% group_by(par1) %>% count(par2)

table <- function(x){
vparu <- par %>% filter(par1 == x | par2 == x) %>% gather(krneki, Stevilka, par1:par2) %>% filter(Stevilka != x )
vparu$krneki <- NULL
vparu <-vparu[,c(2,1)]
colnames(vparu) <- c('Stevilka','Pojavila v paru z analizirano stevilko')
vparu
}


# podatki <- obcine %>% transmute(obcina, povrsina, gostota,
#                                 gostota.naselij=naselja/povrsina) %>%
#   left_join(povprecja, by="obcina")
# row.names(podatki) <- podatki$obcina
# podatki$obcina <- NULL
# 
# # Število skupin
# n <- 5
# skupine <- hclust(dist(scale(podatki))) %>% cutree(n)
