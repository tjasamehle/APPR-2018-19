library(rvest)
library(gsubfn)
library(readr)
library(dplyr)

# WIKIPEDIJA : tabela zabeleženih dobitkov
link <- "https://en.wikipedia.org/wiki/Eurojackpot"
stran <- html_session(link) %>% read_html()
tabela_wiki <- stran %>% html_nodes(xpath="//table[@class='wikitable sortable']") %>%
       .[[1]] %>% html_table(dec=",")

#2012
link <-"https://www.loterija.si/LOTERIJA.asp?leto=2012&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
stran <- html_session(link) %>% read_html()
tabela2012 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()

#2013
link <-"https://www.loterija.si/LOTERIJA.asp?leto=2013&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
stran <- html_session(link) %>% read_html()
tabela2013 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()

#2014
link <-"https://www.loterija.si/LOTERIJA.asp?leto=2014&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
stran <- html_session(link) %>% read_html()
tabela2014 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()

#2015
link <-"https://www.loterija.si/LOTERIJA.asp?leto=2015&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
stran <- html_session(link) %>% read_html()
tabela2015 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()

#2O16
link <-"https://www.loterija.si/LOTERIJA.asp?leto=2016&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
stran <- html_session(link) %>% read_html()
tabela2016 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()

#2017
link <-"https://www.loterija.si/LOTERIJA.asp?leto=2017&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
stran <- html_session(link) %>% read_html()
tabela2017 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()

#2018
link <-"https://www.loterija.si/LOTERIJA.asp?leto=2018&lytoo=igre%2Figre_z_zrebanji%2Feurojackpot%2Fstatistika%2Feurojackpot_arhiv"
stran <- html_session(link) %>% read_html()
tabela2018 <- stran %>% html_nodes(xpath="//table[@class='stdtable']") %>% .[[1]] %>% html_table()

