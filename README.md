# ScriptTCC2
# Carregando pacotes necessário
library(tm)
library(readxl)
library(e1071)
library(naivebayes)
library(data.table)
library(dplyr)
library(randomForest)

# Lendo o arquivo de dados
dados_brutos <- data.table::fread("~/DOCUMENTOS EMERSON/Documentos/TCC Banco de dados/ANOTAÇÃO CORPUS TCC csv.csv", encoding = "UTF-8") %>%
  select(q007, tag1)

# Visualizando os dados brutos
View(dados_brutos)

# Cria uma cópia com dados a serem limpos
dados_clean <- dados_brutos

# Colocando tudo em minúsculo
dados_clean$q007 <- stringr::str_to_lower(dados_clean$q007)

# Resolvendo de forma mais ampla
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'aãão' , 'ação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'aãães' , 'ações')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'aããµes' , 'ações')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'uãão' , 'ução')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'nãão' , 'nção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãª' , 'ê')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãº' , 'ú')

# Resolvendo palavras específicas
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'opãão' , 'opção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'opiãães' , 'opções')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'atenãão' , 'atenção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'descriãães' , 'descrições')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'seãão' , 'seção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'confecãão' , 'confecção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'locomoãão' , 'locomoção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'percepãão' , 'percepção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'restriãão' , 'restrição')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'condiãães' , 'condições')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'opããµes' , 'opções')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'opãães' , 'opções')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'preããrio' , 'precário')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'fãsico' , 'físico')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'perãodo' , 'período')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'serviãos' , 'serviços')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ã³rgão' , 'orgãos')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'vocã' , 'você')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'horãrio' , 'horário')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'não' , 'não')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'recepãão' , 'recepção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'resoluãão' , 'resolução')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'necessãrio' , 'necessário')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'climatizaãão' , 'climatização')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'confortãvel' , 'confortável')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'condiããµes' , 'condições')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'habitaãão' , 'habilitação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'instalaãão' , 'instalação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'informaããµes' , 'informações')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'educaãão' , 'educação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'habilitaãão ' , 'habilitação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'organizaãão' , 'organização')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'desorganizaãão' , 'certa')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'manutenãão' , 'manutenção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'insatisfaãão ' , 'insatisfação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'fiscalizaãão ' , 'fiscalização')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'marcaããµes' , 'marcações')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'otimizaãão' , 'otimização')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'informaãães' , 'informações')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'instituiãão' , 'instituição')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãgua' , 'água')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'marcaãão' , 'marcação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'reclamaãão' , 'reclamação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'isenãão' , 'isenção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'disposiãão' , 'disposição')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'apresentaãão' , 'apresentação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'notificaãão' , 'notificação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'capacitaãão' , 'capacitação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'modernizaãão ' , 'modernização')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'digitalizaãão' , 'digitalização')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'produãão' , 'produção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'comunicaãão' , 'comunicação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'localizaãão' , 'localização')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'pãããssimo' , 'péssimo')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'exceãães' , 'exceções')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'eleiãão' , 'eleição')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'recpãão' , 'recepção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'posiãão' , 'posição')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'contribuiãão' , 'contribuição')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'decepãão' , 'decepção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'seããµes' , 'seções')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'petiãão' , 'petição')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'seleãão' , 'seleção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'correãão' , 'correção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'servião' , 'serviço')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'condiãão' , 'condição')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'pããããsimoooooo' , 'péssimo')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'vocêªs' , 'vocês')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ag~encia' , 'agência')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'soluãao~' , 'solução')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'jã' , 'já')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'prãtico' , 'prático')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãtimo' , 'ótimo')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãtima' , 'ótima')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãrgão' , 'órgão')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãltimo' , 'último')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãrea' , 'área')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãgeis' , 'ágil')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãgil' , 'ágil')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãltima' , 'última')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãgil' , 'ágil')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'parabãns' , 'parabéns')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãnica' , 'única')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'prãximo' , 'próximo')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'prãxima' , 'próxima')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'pãssima' , 'péssima')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ã-te' , 'até')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'idãia' , 'ideia')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'nãs' , 'nos')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ã­mpar' , 'ímpar')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ãculos' , 'óculos')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'lamentãvel' , 'lamentável')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ã­cone ' , 'ícone')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' ã ' , ' é ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'atã ' , 'até ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'hã ' , 'há ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' lã ' , ' lá ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' dã ' , ' dá ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' sã ' , ' só ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'estã ' , 'está ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'mã ' , 'má ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'recepãaã' , 'recepção')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' prã ' , ' pré ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'cidadã .' , 'cidadão.')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' pã ' , ' pé ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' serã ' , ' será ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'comeãando ' , 'começando')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'alvarã' , 'alvará')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'dã ' , 'dá ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'mãquina' , 'máquina')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'bebã ' , 'bebê ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'sã ' , 'só ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'informaçã ode' , 'informação de')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'terã ' , 'terá ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'porquã' , 'porque')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' tã ' , ' está ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' vã ' , ' vá ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'por quã ' , 'porque ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'seguranãa' , 'segurança')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'falarã de comunicação' , 'falha de comunicação')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' irã ' , ' irá ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'motos nã ' , 'motos no ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'quã nã' , 'que né')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'ã ' , 'é ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'infirmacoes' , 'informacoes')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'usurarios' , 'usuarios')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'d3bito' , 'débito')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'agilidadegentileza' , 'agilidade gentileza')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'maueducado' , 'mal educado')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'disponã­vel' , 'disponível')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')
#dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'errado' , 'certo')

# Removendo acentos
library(abjutils)
dados_clean$q007 <- abjutils::rm_accent(dados_clean$q007)

# Removendo pontuação
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'\\.' , ' ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'\\,' , ' ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'\\:' , ' ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'\\;' , ' ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,' - ' , ' ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'!' , ' ')

# Removendo espaços extras
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'    ' , ' ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'   ' , ' ')
dados_clean$q007 <- stringr::str_replace_all(dados_clean$q007,'  ' , ' ')
dados_clean$q007 <- stringr::str_trim(string = dados_clean$q007, side = 'both')

# criando 
dados <- dados_clean
View(dados)
dados$tag1 <- as.factor(dados$tag1)


set.seed(123)

indices <- sample(1:nrow(dados), 0.8 * nrow(dados))

dados_treino <- dados[indices, ]
dados_teste <- dados[-indices, ]

corpus_treino <- Corpus(VectorSource(dados_treino$q007))
corpus_treino <- tm_map(corpus_treino, content_transformer(tolower))
corpus_treino <- tm_map(corpus_treino, removePunctuation)
corpus_treino <- tm_map(corpus_treino, removeNumbers)
corpus_treino <- tm_map(corpus_treino, removeWords, stopwords("portuguese"))
corpus_treino <- tm_map(corpus_treino, stemDocument, language = "portuguese")

# Criando o corpus de teste e fazendo ajustes necessários
corpus_teste <- Corpus(VectorSource(dados_teste$q007))
#corpus_teste <- tm_map(corpus_teste, content_transformer(tolower))
corpus_teste <- tm_map(corpus_teste, removePunctuation)
corpus_teste <- tm_map(corpus_teste, removeNumbers)
corpus_teste <- tm_map(corpus_teste, removeWords, stopwords("portuguese"))
corpus_teste <- tm_map(corpus_teste, stemDocument, language = "portuguese")


## Naive Bayes ##

dtm_treino <- DocumentTermMatrix(corpus_treino)
dtm_teste <- DocumentTermMatrix(corpus_teste, control = list(dictionary = Terms(dtm_treino)))



#Usando tokens com frequência mínima igual a 3 e com Dicotomia para cada token (sim ou não)

tresfreq <- findFreqTerms(dtm_treino, 3, highfreq=Inf)

dtm_treino_3 <- dtm_treino[, tresfreq]
dtm_teste_3 <- dtm_teste[, tresfreq]

convert_count <- function(x) {
  y <- ifelse(x > 0, "Yes", "No")
}


dtm_treino_3 <- apply(as.matrix(dtm_treino_3), 2, convert_count)
dtm_treino_3 <- as.data.frame(dtm_treino_3)
dtm_treino_3$tag1 <- dados_treino$tag1

# Aplicando a conversão à matriz do documento de teste
dtm_teste_3 <- apply(as.matrix(dtm_teste3), 2, convert_count)
dtm_teste_3 <- as.data.frame(dtm_teste3)
dtm_teste_3$tag1 <- dados_teste$tag1



colnames(dtm_treino_3) <- make.names(colnames(dtm_treino_3))
colnames(dtm_teste_3) <- make.names(colnames(dtm_teste_3))

Formula3 <- as.formula(paste("tag1 ~", paste(colnames(dtm_treino_3)[colnames(dtm_treino_3) != "tag1"], collapse = " + ")))

trainNB3 <- as.data.frame(dtm_treino_3)
testNB3 <- as.data.frame(dtm_teste_3)

trainNB3$tag1 <- as.factor(trainNB3$tag1)
testNB3$tag1 <- as.factor(testNB3$tag1)

classifier3 <- naiveBayes(Formula3, data = trainNB3)

pred3 <- predict(classifier3, newdata = testNB3)

# Avaliando as predições
caret::confusionMatrix(data = table(pred3, testNB3$tag1))



#Usando tokens com frequência mínima igual a 5 e com Dicotomia para cada token (sim ou não)

cincofreq <- findFreqTerms(dtm_treino, 5, highfreq=Inf)

dtm_treino_5 <- dtm_treino[, cincofreq]
dtm_teste_5 <- dtm_teste[, cincofreq]

convert_count <- function(x) {
  y <- ifelse(x > 0, "Yes", "No")
}


dtm_treino_5 <- apply(as.matrix(dtm_treino_5), 2, convert_count)
dtm_treino_5 <- as.data.frame(dtm_treino_5)
dtm_treino_5$tag1 <- dados_treino$tag1

# Aplicando a conversão à matriz do documento de teste
dtm_teste_5 <- apply(as.matrix(dtm_teste5), 2, convert_count)
dtm_teste_5 <- as.data.frame(dtm_teste5)
dtm_teste_5$tag1 <- dados_teste$tag1



colnames(dtm_treino_5) <- make.names(colnames(dtm_treino_5))
colnames(dtm_teste_5) <- make.names(colnames(dtm_teste_5))

Formula5 <- as.formula(paste("tag1 ~", paste(colnames(dtm_treino_5)[colnames(dtm_treino_5) != "tag1"], collapse = " + ")))

trainNB5 <- as.data.frame(dtm_treino_5)
testNB5 <- as.data.frame(dtm_teste_5)

trainNB5$tag1 <- as.factor(trainNB5$tag1)
testNB5$tag1 <- as.factor(testNB5$tag1)

classifier5 <- naiveBayes(Formula5, data = trainNB5)

pred5 <- predict(classifier5, newdata = testNB5)

# Avaliando as predições
caret::confusionMatrix(data = table(pred5, testNB5$tag1))




#Usando tokens com frequência mínima igual a 7 e com Dicotomia para cada token (sim ou não)

setefreq <- findFreqTerms(dtm_treino, 7, highfreq=Inf)

dtm_treino_7 <- dtm_treino[, setefreq]
dtm_teste_7 <- dtm_teste[, setefreq]

convert_count <- function(x) {
  y <- ifelse(x > 0, "Yes", "No")
}


dtm_treino_7 <- apply(as.matrix(dtm_treino_7), 2, convert_count)
dtm_treino_7 <- as.data.frame(dtm_treino_7)
dtm_treino_7$tag1 <- dados_treino$tag1

# Aplicando a conversão à matriz do documento de teste
dtm_teste_7 <- apply(as.matrix(dtm_teste7), 2, convert_count)
dtm_teste_7 <- as.data.frame(dtm_teste7)
dtm_teste_7$tag1 <- dados_teste$tag1



colnames(dtm_treino_7) <- make.names(colnames(dtm_treino_7))
colnames(dtm_teste_7) <- make.names(colnames(dtm_teste_7))

Formula7 <- as.formula(paste("tag1 ~", paste(colnames(dtm_treino_7)[colnames(dtm_treino_7) != "tag1"], collapse = " + ")))

trainNB7 <- as.data.frame(dtm_treino_7)
testNB7 <- as.data.frame(dtm_teste_7)

trainNB7$tag1 <- as.factor(trainNB7$tag1)
testNB7$tag1 <- as.factor(testNB7$tag1)

classifier7 <- naiveBayes(Formula7, data = trainNB7)

pred7 <- predict(classifier7, newdata = testNB7)

# Avaliando as predições
caret::confusionMatrix(data = table(pred7, testNB7$tag1))




#Usando tokens com frequência mínima igual a 10 e com Dicotomia para cada token (sim ou não)

dezfreq <- findFreqTerms(dtm_treino, 10, highfreq=Inf)

dtm_treino_10 <- dtm_treino[, dezfreq]
dtm_teste_10 <- dtm_teste[, dezfreq]

convert_count <- function(x) {
  y <- ifelse(x > 0, "Yes", "No")
}


dtm_treino_10 <- apply(as.matrix(dtm_treino_10), 2, convert_count)
dtm_treino_10 <- as.data.frame(dtm_treino_10)
dtm_treino_10$tag1 <- dados_treino$tag1

# Aplicando a conversão à matriz do documento de teste
dtm_teste_10 <- apply(as.matrix(dtm_teste10), 2, convert_count)
dtm_teste_10 <- as.data.frame(dtm_teste10)
dtm_teste_10$tag1 <- dados_teste$tag1



colnames(dtm_treino_10) <- make.names(colnames(dtm_treino_10))
colnames(dtm_teste_10) <- make.names(colnames(dtm_teste_10))

Formula10 <- as.formula(paste("tag1 ~", paste(colnames(dtm_treino_10)[colnames(dtm_treino_10) != "tag1"], collapse = " + ")))

trainNB10 <- as.data.frame(dtm_treino_10)
testNB10 <- as.data.frame(dtm_teste_10)

trainNB10$tag1 <- as.factor(trainNB10$tag1)
testNB10$tag1 <- as.factor(testNB10$tag1)

classifier10 <- naiveBayes(Formula10, data = trainNB10)

pred10 <- predict(classifier10, newdata = testNB10)

# Avaliando as predições
caret::confusionMatrix(data = table(pred10, testNB10$tag1))




#Usando tokens com frequência mínima igual a 15 e com Dicotomia para cada token (sim ou não)

quinzefreq <- findFreqTerms(dtm_treino, 15, highfreq=Inf)

dtm_treino_15 <- dtm_treino[, quinzefreq]
dtm_teste_15 <- dtm_teste[, quinzefreq]

convert_count <- function(x) {
  y <- ifelse(x > 0, "Yes", "No")
}


dtm_treino_15 <- apply(as.matrix(dtm_treino_15), 2, convert_count)
dtm_treino_15 <- as.data.frame(dtm_treino_15)
dtm_treino_15$tag1 <- dados_treino$tag1

# Aplicando a conversão à matriz do documento de teste
dtm_teste_15 <- apply(as.matrix(dtm_teste15), 2, convert_count)
dtm_teste_15 <- as.data.frame(dtm_teste15)
dtm_teste_15$tag1 <- dados_teste$tag1



colnames(dtm_treino_15) <- make.names(colnames(dtm_treino_15))
colnames(dtm_teste_15) <- make.names(colnames(dtm_teste_15))

Formula15 <- as.formula(paste("tag1 ~", paste(colnames(dtm_treino_15)[colnames(dtm_treino_15) != "tag1"], collapse = " + ")))

trainNB15 <- as.data.frame(dtm_treino_15)
testNB15 <- as.data.frame(dtm_teste_15)

trainNB15$tag1 <- as.factor(trainNB15$tag1)
testNB15$tag1 <- as.factor(testNB15$tag1)

classifier15 <- naiveBayes(Formula15, data = trainNB15)

pred15 <- predict(classifier15, newdata = testNB15)

# Avaliando as predições
caret::confusionMatrix(data = table(pred15, testNB15$tag1))





## Random Forest ##

# RANDOM FOREST usando todas as tokens e com TF-IDF

dtm_tfidf_treino <- DocumentTermMatrix(corpus_treino)
dtm_tfidf_teste <- DocumentTermMatrix(corpus_teste, control = list(dictionary = Terms(dtm_tfidf_treino)))

dtm_tfidf_treino <- weightTfIdf(dtm_tfidf_treino)
dtm_tfidf_teste <- weightTfIdf(dtm_tfidf_teste)


texto_treino <- as.data.frame(as.matrix(dtm_tfidf_treino))
texto_treino$tag1 <- dados_treino$tag1
#View(texto_treino)

texto_teste <- as.data.frame(as.matrix(dtm_tfidf_teste))
texto_teste$tag1 <- dados_teste$tag1
#View(texto_teste)

colnames(texto_treino) <- make.names(colnames(texto_treino))
colnames(texto_teste) <- make.names(colnames(texto_teste))

Formula <- as.formula(paste("tag1 ~", paste(colnames(texto_treino)[colnames(texto_treino) != "tag1"], collapse = " + ")))


# treinando modelos com 5 arvores -----------------------------------------

# Modelo Random Forest com 5 árvores e 30 features

# Definindo modelo
modelo_rf_5_30 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 5,
  mtry = 30
)

# Visualizando modelo
modelo_rf_5_30

# Realizando predições
predicoes_rf_5_30 <- predict(modelo_rf_5_30, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_30, texto_teste$tag1))


# Modelo Random Forest com 5 árvores e 60 features

# Definindo modelo
modelo_rf_5_60 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 5,
  mtry = 60
)

# Visualizando modelo
modelo_rf_5_60

# Realizando predições
predicoes_rf_5_60 <- predict(modelo_rf_5_60, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_60, texto_teste$tag1))


# Modelo Random Forest com 5 árvores e 100 features

# Definindo modelo
modelo_rf_5_100 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 5,
  mtry = 100
)

# Visualizando modelo
modelo_rf_5_100

# Realizando predições
predicoes_rf_5_100 <- predict(modelo_rf_5_100, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_100, texto_teste$tag1))






# treinando modelos com 100 arvores -----------------------------------------

# Modelo Random Forest com 100 árvores e 30 features

# Definindo modelo
modelo_rf_100_30 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 100,
  mtry = 30
)

# Visualizando modelo
modelo_rf_100_30

# Realizando predições
predicoes_rf_100_30 <- predict(modelo_rf_100_30, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_30, texto_teste$tag1))


# Modelo Random Forest com 100 árvores e 60 features

# Definindo modelo
modelo_rf_100_60 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 100,
  mtry = 60
)

# Visualizando modelo
modelo_rf_100_60

# Realizando predições
predicoes_rf_100_60 <- predict(modelo_rf_100_60, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_60, texto_teste$tag1))


# Modelo Random Forest com 100 árvores e 100 features

# Definindo modelo
modelo_rf_100_100 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 100,
  mtry = 100
)

# Visualizando modelo
modelo_rf_100_100

# Realizando predições
predicoes_rf_100_100 <- predict(modelo_rf_100_100, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_100, texto_teste$tag1))








# treinando modelos com 1000 arvores -----------------------------------------

# Modelo Random Forest com 1000 árvores e 30 features

# Definindo modelo
modelo_rf_1000_30 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 1000,
  mtry = 30
)

# Visualizando modelo
modelo_rf_1000_30

# Realizando predições
predicoes_rf_1000_30 <- predict(modelo_rf_1000_30, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_30, texto_teste$tag1))


# Modelo Random Forest com 1000 árvores e 60 features

# Definindo modelo
modelo_rf_1000_60 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 1000,
  mtry = 60
)

# Visualizando modelo
modelo_rf_1000_60

# Realizando predições
predicoes_rf_1000_60 <- predict(modelo_rf_1000_60, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_60, texto_teste$tag1))


# Modelo Random Forest com 1000 árvores e 100 features

# Definindo modelo
modelo_rf_1000_100 <- randomForest(
  formula = Formula, 
  data = texto_treino, 
  ntree = 1000,
  mtry = 100
)

# Visualizando modelo
modelo_rf_1000_100

# Realizando predições
predicoes_rf_1000_100 <- predict(modelo_rf_1000_100, newdata = texto_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_100, texto_teste$tag1))


# RANDOM FOREST usando tokens com frequência mínima igual a 5 e com Dicotomia para cada token (sim ou não)


dtm_treino <- DocumentTermMatrix(corpus_treino)
dtm_teste <- DocumentTermMatrix(corpus_teste, control = list(dictionary = Terms(dtm_treino)))

fivefreq <- findFreqTerms(dtm_treino, 5, highfreq=Inf)

dtm_treino <- dtm_treino[, fivefreq]
dtm_teste <- dtm_teste[, fivefreq]

convert_count <- function(x) {
  y <- ifelse(x > 0, "Yes", "No")
}


dtm_treino <- apply(as.matrix(dtm_treino), 2, convert_count)
dtm_treino <- as.data.frame(dtm_treino)
dtm_treino$tag1 <- dados_treino$tag1

# Aplicando a conversão à matriz do documento de teste
dtm_teste <- apply(as.matrix(dtm_teste), 2, convert_count)
dtm_teste <- as.data.frame(dtm_teste)
dtm_teste$tag1 <- dados_teste$tag1


View(dtm_teste)

colnames(dtm_treino) <- make.names(colnames(dtm_treino))
colnames(dtm_teste) <- make.names(colnames(dtm_teste))

Formula <- as.formula(paste("tag1 ~", paste(colnames(dtm_treino)[colnames(dtm_treino) != "tag1"], collapse = " + ")))

# treinando modelos com 5 arvores -----------------------------------------


# Modelo Random Forest com 5 árvores e 30 features

# Definindo modelo
modelo_rf_5_30 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 5,
  mtry = 30
)

# Visualizando modelo
modelo_rf_5_30

# Realizando predições
predicoes_rf_5_30 <- predict(modelo_rf_5_30, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_30, dtm_teste$tag1))



# Modelo Random Forest com 5 árvores e 60 features

# Definindo modelo
modelo_rf_5_60 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 5,
  mtry = 60
)

# Visualizando modelo
modelo_rf_5_60

# Realizando predições
predicoes_rf_5_60 <- predict(modelo_rf_5_60, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_60, dtm_teste$tag1))



# Modelo Random Forest com 5 árvores e 100 features

# Definindo modelo
modelo_rf_5_100 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 5,
  mtry = 100
)

# Visualizando modelo
modelo_rf_5_100

# Realizando predições
predicoes_rf_5_100 <- predict(modelo_rf_5_100, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_100, dtm_teste$tag1))

# treinando modelos com 100 arvores -----------------------------------------

# Modelo Random Forest com 100 árvores e 30 features

# Definindo modelo
modelo_rf_100_30 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 100,
  mtry = 30
)

# Visualizando modelo
modelo_rf_100_30

# Realizando predições
predicoes_rf_100_30 <- predict(modelo_rf_100_30, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_30, dtm_teste$tag1))

# Modelo Random Forest com 100 árvores e 60 features

# Definindo modelo
modelo_rf_100_60 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 100,
  mtry = 60
)

# Visualizando modelo
modelo_rf_100_60

# Realizando predições
predicoes_rf_100_60 <- predict(modelo_rf_100_60, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_60, dtm_teste$tag1))

# Modelo Random Forest com 100 árvores e 100 features

# Definindo modelo
modelo_rf_100_100 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 100,
  mtry = 100
)

# Visualizando modelo
modelo_rf_100_100

# Realizando predições
predicoes_rf_100_100 <- predict(modelo_rf_100_100, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_100, dtm_teste$tag1))






# treinando modelos com 1000 arvores -----------------------------------------

# Modelo Random Forest com 1000 árvores e 30 features

# Definindo modelo
modelo_rf_1000_30 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 1000,
  mtry = 30
)

# Visualizando modelo
modelo_rf_1000_30

# Realizando predições
predicoes_rf_1000_30 <- predict(modelo_rf_1000_30, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_30, dtm_teste$tag1))



# Modelo Random Forest com 1000 árvores e 60 features

# Definindo modelo
modelo_rf_1000_60 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 1000,
  mtry = 60
)

# Visualizando modelo
modelo_rf_1000_60

# Realizando predições
predicoes_rf_1000_60 <- predict(modelo_rf_1000_60, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_60, dtm_teste$tag1))

# Modelo Random Forest com 1000 árvores e 100 features

# Definindo modelo
modelo_rf_1000_100 <- randomForest(
  formula = Formula, 
  data = dtm_treino, 
  ntree = 1000,
  mtry = 100
)

# Visualizando modelo
modelo_rf_1000_100

# Realizando predições
predicoes_rf_1000_100 <- predict(modelo_rf_1000_100, newdata = dtm_teste)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_100, dtm_teste$tag1))




# RANDOM FOREST usando tokens com frequência mínima igual a 5 e com TF-IDF

dtm_treino <- DocumentTermMatrix(corpus_treino)
dtm_teste <- DocumentTermMatrix(corpus_teste, control = list(dictionary = Terms(dtm_treino)))

dtm_treino <- weightTfIdf(dtm_treino)
dtm_teste <- weightTfIdf(dtm_teste)

fivefreq <- findFreqTerms(dtm_treino, 5, highfreq=Inf)

dtm_treino5 <- dtm_treino[, fivefreq]
dtm_teste5 <- dtm_teste[, fivefreq]


dtm_treino5 <- as.data.frame(as.matrix(dtm_treino5))
dtm_treino5$tag1 <- dados_treino$tag1

dtm_teste5 <- as.data.frame(as.matrix(dtm_teste5))
dtm_teste5$tag1 <- dados_teste$tag1

colnames(dtm_treino5) <- make.names(colnames(dtm_treino5))
colnames(dtm_teste5) <- make.names(colnames(dtm_teste5))

Formula <- as.formula(paste("tag1 ~", paste(colnames(dtm_treino5)[colnames(dtm_treino5) != "tag1"], collapse = " + ")))


# treinando modelos com 5 arvores -----------------------------------------

# Modelo Random Forest com 5 árvores e 30 features

# Definindo modelo
modelo_rf_5_30 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 5,
  mtry = 30
)

# Visualizando modelo
modelo_rf_5_30

# Realizando predições
predicoes_rf_5_30 <- predict(modelo_rf_5_30, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_30, dtm_teste5$tag1))


# Modelo Random Forest com 5 árvores e 60 features

# Definindo modelo
modelo_rf_5_60 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 5,
  mtry = 60
)

# Visualizando modelo
modelo_rf_5_60

# Realizando predições
predicoes_rf_5_60 <- predict(modelo_rf_5_60, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_60, dtm_teste5$tag1))


# Modelo Random Forest com 5 árvores e 100 features

# Definindo modelo
modelo_rf_5_100 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 5,
  mtry = 100
)

# Visualizando modelo
modelo_rf_5_100

# Realizando predições
predicoes_rf_5_100 <- predict(modelo_rf_5_100, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_5_100, dtm_teste5$tag1))


# treinando modelos com 100 arvores -----------------------------------------

# Modelo Random Forest com 100 árvores e 30 features

# Definindo modelo
modelo_rf_100_30 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 100,
  mtry = 30
)

# Visualizando modelo
modelo_rf_100_30

# Realizando predições
predicoes_rf_100_30 <- predict(modelo_rf_100_30, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_30, dtm_teste5$tag1))


# Modelo Random Forest com 100 árvores e 60 features

# Definindo modelo
modelo_rf_100_60 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 100,
  mtry = 60
)

# Visualizando modelo
modelo_rf_100_60

# Realizando predições
predicoes_rf_100_60 <- predict(modelo_rf_100_60, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_60, dtm_teste5$tag1))


# Modelo Random Forest com 100 árvores e 100 features

# Definindo modelo
modelo_rf_100_100 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 100,
  mtry = 100
)

# Visualizando modelo
modelo_rf_100_100

# Realizando predições
predicoes_rf_100_100 <- predict(modelo_rf_100_100, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_100_100, dtm_teste5$tag1))


# treinando modelos com 1000 arvores -----------------------------------------

# Modelo Random Forest com 1000 árvores e 30 features

# Definindo modelo
modelo_rf_1000_30 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 1000,
  mtry = 30
)

# Visualizando modelo
modelo_rf_1000_30

# Realizando predições
predicoes_rf_1000_30 <- predict(modelo_rf_1000_30, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_30, dtm_teste5$tag1))


# Modelo Random Forest com 1000 árvores e 60 features

# Definindo modelo
modelo_rf_1000_60 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 1000,
  mtry = 60
)

# Visualizando modelo
modelo_rf_1000_60

# Realizando predições
predicoes_rf_1000_60 <- predict(modelo_rf_1000_60, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_60, dtm_teste5$tag1))


# Modelo Random Forest com 1000 árvores e 100 features

# Definindo modelo
modelo_rf_1000_100 <- randomForest(
  formula = Formula, 
  data = dtm_treino5,
  ntree = 1000,
  mtry = 100
)

# Visualizando modelo
modelo_rf_1000_100

# Realizando predições
predicoes_rf_1000_100 <- predict(modelo_rf_1000_100, newdata = dtm_teste5)

# Avaliando as predições
caret::confusionMatrix(data = table(predicoes_rf_1000_100, dtm_teste5$tag1))



