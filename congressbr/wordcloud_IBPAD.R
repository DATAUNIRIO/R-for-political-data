# https://www.ibpad.com.br/blog/politica/dados-congresso-brasileiro-parte-1/

#Carregando o pacote
library("congressbr")

#Buscando a lista de PLs na Câmara em 2017. Você pode escolher entre "PEC" e outras proposições.

PLsCamara <- cham_bills(type = "PL", year = 2017)

#Buscando as informações detalhadas de cada PL
datalist = list() #criando uma lista vazia

for(i in 1:nrow(PLsCamara)){
  cat(i, "\n ")
  dat2 <- cham_bill_info_id(PLsCamara$bill_id[i])
  datalist[[i]] <- dat2 # adicionando cada chamada na minha lista
  
}

#transformando minha lista em um dataframe
PLDetalhadosCamara <- dplyr::bind_rows(datalist)

# Depois de ter rodado esse código, você terá um data.frame com informações detalhadas de todos os PLs que foram apresentados em 2017. Uma variável que eu acho super legal é a variável de bill_index . Essa é uma variável de texto com a indexação em termos da proposição. Quem faz a inserção é a Mesa da Câmara dos Deputados, o que nos garante uma certa qualidade nos termos. Vou utilizar essa variável para fazer alguns tratamentos de texto e entender um pouco o que nossos Deputados Federais andaram apresentando em 2017.


#carregando o pacote TM
library(tm)

#definindo meu corpus
myCorpus <- Corpus(VectorSource(PLDetalhadosCamara$bill_index))

#procedimentos de limpeza e tratamento

#deixando tudo minúsculo, removendo espaços e pontuação
myCorpus <- tm_map(myCorpus, content_transformer(tolower))
myCorpus <- tm_map(myCorpus, stripWhitespace)
myCorpus <- tm_map(myCorpus,removePunctuation)

#removendo palavras que atrapalhariam a análise, atente-se que estou utilizando um dicionário padrão de "stopwords" do pacote e um conjunto de outros termos, ok? você pode continuar essa lista ou remover alguns termos daí também.
limpeza = c(stopwords("portuguese"), 
            "federal", "lei", "nacional", "alteracao", "criacao")
myCorpus = tm_map(myCorpus, removeWords, limpeza)

#gerando as tabelas de frequencia
myCorpus <- tm_map(myCorpus, PlainTextDocument)
tdm <- TermDocumentMatrix(myCorpus,control = list(wordLengths = c(1, Inf)))
tdm

dtm <- as.DocumentTermMatrix(tdm)


(freq.terms <- findFreqTerms(tdm, lowfreq = 2)) 
term.freq <- rowSums(as.matrix(tdm))
#optei por deixar um valor mínimo de frequencia de 2 nos termos. você pode trabalhar isso conforme desejar.
term.freq <- subset(term.freq, term.freq >= 2) 
term.freqdf <- data.frame(term = names(term.freq), freq = term.freq)

# Os tratamentos feitos aqui são “padrões” e é muito provável que você vá utilizá-los em bases diferentes. Vou mostrar nesse post uma simples nuvem de palavras e nos próximos posts pretendo avançar um pouco na área de “text mining”.
#carregando o pacote
library(wordcloud)

#vou colocar uma semente aqui para quando você gerar aí a nuvem ficar igual
set.seed(1234)

#gerando a nuvem
wordcloud(words = term.freqdf$term, freq = term.freqdf$freq, min.freq = 1,
          max.words=200, random.order=FALSE, rot.per=0, 
          colors=brewer.pal(8, "Dark2"))
