#install.packages('congressbr')

library(congressbr)
library(dplyr)

lista_senadores<-sen_senator_list()

names(lista_senadores)
table(lista_senadores$gender)
table(lista_senadores$party_abbr)
table(lista_senadores$status)

##================================================================================================

barplot(table(sen_parties()$party_abbr))
barplot(table(lista_senadores$party_abbr),col='red')

# Construindo as barras
contagem = table(lista_senadores$party_abbr)
porcent = round(contagem/sum(contagem)*100,2)

par(cex=0.7)
rotulo=paste0(porcent,"%")
bp<-barplot(table(lista_senadores$party_abbr), ylab="Frequência",col=c("lightgreen","skyblue"),main="Gráfico 1")
text(bp, 0, rotulo,cex=1,pos=3)

##================================================================================================
# faca o mesmo para os deputados

lista_deputados<-cham_legislator_list()

names(lista_deputados)
table(lista_deputados$legislator_gender)
contagem = table(lista_deputados$legislator_gender)
porcent = round(contagem/sum(contagem)*100,2)
rotulo=paste0(porcent,"%")
bp<-barplot(table(lista_deputados$legislator_gender), ylab="Frequência",col=c("pink","skyblue"),main="Gráfico 1")
text(bp, 0, rotulo,cex=1,pos=3)

##================================================================================================

lista_senadores %>% filter(status != "Titular") %>% group_by(state) %>% 
  summarise(totals = n()) %>% arrange(desc(totals))

lista_deputados %>% filter(legislator_status != "Titular") %>% group_by(legislator_state) %>% 
  summarise(totals = n()) %>% arrange(desc(totals))

##================================================================================================

sup_cid_gomes<-sen_senator_suplentes(id=5973)
sup_cid_gomes$senator_suplente_name

##================================================================================================

#The type of the bill. 
#"PL" for law proposal ("projeto de lei"), 
#"PEC" for constitutional amendments ("projeto de emenda constitucional"), 
#"PDC" for legislative decree ("decreto legislativo"), and 
#"PLP" for supplementary laws ("projeto de lei complementar).


#####################################################################
# PEC congelamento dos gastos por 20 anos (PEC fim do mundo)
#####################################################################
PEC_congelamento<-cham_votes(type = "PEC", number = "241", year = "2016")
PEC_congelamento$decision_summary[[1]]

# PT
pt_congelamento <- PEC_congelamento %>%
  mutate(legislator_party = legislator_party) %>%
  filter(legislator_party == "PT")
table(pt_congelamento$legislator_vote)
table(pt_congelamento$legislator_vote,pt_congelamento$legislator_name)

# PSDB
psdb_congelamento <- PEC_congelamento %>%
  mutate(legislator_party = legislator_party) %>%
  filter(legislator_party == "PSDB")
table(psdb_congelamento$legislator_vote)

# MDB
mdb_congelamento <- PEC_congelamento %>%
  mutate(legislator_party = legislator_party) %>%
  filter(legislator_party == "PMDB")
table(mdb_congelamento$legislator_vote)



load("C:/Users/Steven/Desktop/congressbr/cham_nominal_votes.rda")
head(cham_nominal_votes)


library(ggplot2)
aaaa<-cham_nominal_votes %>%
  dplyr::mutate(year = lubridate::year(vote_date)) %>%
  dplyr::distinct(year, legislator_party) %>%
  dplyr::count(year) %>%
  ggplot2::ggplot(ggplot2::aes(x = year, y = n)) +
  ggplot2::geom_col(fill="steelblue") +
  ggplot2::theme_light() +
  scale_x_continuous(breaks=1991:2017) +
  ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 90, hjust = 1)) +
  scale_y_continuous(breaks = 0:33)


#---------------------------------------------------------------------------------------
# REFORMA DA PREVIDENCIA
#---------------------------------------------------------------------------------------

deputados<-cham_legislator_list()

# The type of the bill. 
# For example, "PL" for law proposal ("projeto de lei"), 
# "PEC" for constitutional amendments ("projeto de emenda constitucional"), 
# "PDC" for legislative decree ("decreto legislativo"), and 
# "PLP" for supplementary laws ("projeto de lei complementar).

# REforma da previdência PEC 06/2019
votos_deputados<-cham_votes(type="PEC",number=06,year = 2019)
votos_previdencia<- table(votos_deputados$legislator_party,votos_deputados$legislator_vote) 

# Tabata Amaral id = 204534
tabata<-votos_deputados[votos_deputados$legislator_id==204534,]
# PDT
PDT<-votos_deputados[votos_deputados$legislator_party=="PDT",]
# PDT (sem tabata)
PDT<-PDT[PDT$legislator_id!=204534,]
