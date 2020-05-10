datum = c()
stevilka = c()
tip = c()
kam <- 1
for(i in c(1:length(razpredelnica2$Datum))){
  stevilke <- unlist(strsplit(razpredelnica2$Številka[i], "-"))
  prve <- gsub(" ", "", unlist(strsplit(stevilke[1], ",")))
  druge <- gsub(" ", "", unlist(strsplit(stevilke[2], ",")))
  for(prva in prve){
    datum[kam] <- razpredelnica2$Datum[i]
    stevilka[kam] <- strtoi(prva)
    tip[kam] <- 1
    kam = kam + 1
  }
  for(druga in druge){
    datum[kam] <- razpredelnica2$Datum[i]
    stevilka[kam] <- strtoi(druga)
    tip[kam] <- 2
    kam = kam + 1
  }
}
tabela2<-data.frame(datum,stevilka,tip)
tabela2$"Datum" <- datum
tabela2$"Stevilka" <- stevilka
tabela2$"Tip" <- tip
