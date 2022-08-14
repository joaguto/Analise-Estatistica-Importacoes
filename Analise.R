# Definindo diretorio de trabalho
setwd("C:/Users/JoaoA/OneDrive - SENAC - SP/CoisasdoJoao/Curso/Data Science/Meus_Projetos/Projects/Importacao")
getwd()

dir()

# Importando base de dados
df = read.csv('imports.csv', sep = ';')
View(df)
class(df)
str(df)

# Arrumando formatação dos dados
df$PO.NUMBER = as.character(df$PO.NUMBER)
df$LEAD_TIME_ENTREGA_DOCS = as.double(df$LEAD_TIME_ENTREGA_DOCS)
df$LEAD_TIME_DESEMBARACO.informacao. = as.double(df$LEAD_TIME_DESEMBARACO.informacao.)
df$LEAD_TIME_TRANSPORTE_INTERCIOL = as.double(df$LEAD_TIME_TRANSPORTE_INTERCIOL)
df$QTDE = as.double(df$QTDE)


# Utilizando um dataset apenas com os numeros (sem datas)
names(df)
df2 = df[,c(1,3,5,7, 8:15, 20, 25, 32, 33, 34:44)]
View(df2)

# Analise de dados
str(df2)
df2$PRODUCT_ME = as.character(df$PRODUCT_ME)
df2$DIF_ETA_ATA = as.double(df2$DIF_ETA_ATA)

library(dplyr)


View(df2)
unique(df$STATUS)
boxplot(df$LEAD_TIME_TOTAL)


# Filtro numero 1
df2 = df2 %>%
  filter(STATUS == 'ENTREGUE', LEAD_TIME_TOTAL > 0, LEAD_TIME_TOTAL < 500 )

# Filtro numero 2
df2 = df2%>%
  filter(DIF_ETA_ATA > -20, DIF_ETA_ATA < 300)

View(df2)


df2$DIF_ETA_ATA <- NULL
df2$SOLICITANTE <- NULL

library(ggplot2)


atendeNaoatende <- ggplot(df2, aes(x = ATENDE.vs.NAO_ATENDE, fill = ATENDE.vs.NAO_ATENDE)) +
  geom_bar(stat = 'count') +
  ggtitle('Atende vs Não Atende em 2020 e 2021') +
  xlab('Atende vs Não Atende') +
  ylab('Não Atende')



teste = df2 %>%
  select(SELLER, ATENDE.vs.NAO_ATENDE) %>%
  group_by(SELLER)


teste2 = df2 %>%
  select(SELLER, ATENDE.vs.NAO_ATENDE) %>%
  group_by(SELLER) %>%
  summarise(Atende = sum(ATENDE.vs.NAO_ATENDE == 'ATENDE'),
                            Nao_Atende = sum( ATENDE.vs.NAO_ATENDE == 'NAO ATENDE'))
  



unique(teste$SELLER)


Erlanger = teste %>%
  filter(SELLER == 'WILD Erlanger')

Heidelberg = teste %>%
  filter(SELLER == 'WILD Heidelberg')

Decatur = teste %>%
  filter(SELLER == 'ADM DECATUR')

Amylum <- teste %>%
  filter(SELLER == 'AMYLUM NISASTA')


library(grid)


Erlang <- ggplot(Erlanger, aes(x = ATENDE.vs.NAO_ATENDE)) +
  geom_bar(stat = 'count') +
  ggtitle('Atende vs Não Atende em 2020 e 2021 (WILD Erlanger)') +
  xlab('Atende vs Não Atende') +
  ylab('Não Atende')

Heide <- ggplot(Heidelberg, aes(x = ATENDE.vs.NAO_ATENDE, fill = ATENDE.vs.NAO_ATENDE)) +
  geom_bar(stat = 'count') +
  ggtitle('Atende vs Não Atende em 2020 e 2021 (WILD Heidelberg)') +
  xlab('Atende vs Não Atende') +
  ylab('Não Atende')

deca <- ggplot(Decatur, aes(x = ATENDE.vs.NAO_ATENDE, fill = ATENDE.vs.NAO_ATENDE)) +
  geom_bar(stat = 'count') +
  ggtitle('Atende vs Não Atende em 2020 e 2021 (ADM DECATUR)') +
  xlab('Atende vs Não Atende') +
  ylab('Não Atende')

amy <- ggplot(Amylum, aes(x = ATENDE.vs.NAO_ATENDE, fill = ATENDE.vs.NAO_ATENDE)) +
  geom_bar(stat = 'count') +
  ggtitle('Atende vs Não Atende em 2020 e 2021 (AMYLUM NISASTA)') +
  xlab('Atende vs Não Atende') +
  ylab('Não Atende')

# Montando o Dashboard
(Erlang | Heide) / (deca | amy)



View(df2)
names(df2)
df3 = df %>%
  select(SELLER, PRODUCT_ME, QTDE, MODAL, Cal, PROJETO, LEAD_TIME_TOTAL,
         DIF_REQUEST_VS_ORDER_CONFIRMATION, DIF_REQUEST_VS_ORDER_CONFIRMATION, 
         LEAD_TIME_LOGISTICA_INTERCIOL, LEAD_TIME_TRANSPORTE_INTERCIOL, LEAD_TIME_NF_ENTRADA,
         LEAD_TIME_NF_REMESSA, LEAD_TIME_LOGISTICA_NACIONAL)

View(df3)




####################

summary(df3)

# Tratando dados
teste = na.omit(df3) %>%
  filter(LEAD_TIME_LOGISTICA_INTERCIOL < 300, LEAD_TIME_TRANSPORTE_INTERCIOL <300, LEAD_TIME_LOGISTICA_NACIONAL < 300,  DIF_REQUEST_VS_ORDER_CONFIRMATION < 50)

View(teste)

summary(teste)


seller = teste %>%
  group_by(SELLER) %>%
  summarise(contagem = length(SELLER)) %>%
  arrange(desc(contagem))


pie(c(59, 44, 43, 5, 4), labels = c('WILD Erlanger', 'WILD Heidelberg', 'ADM Decatur', 'AMYLUM NISASTA', 'WILD Berlin'), main = "Top 5 fornecedores Com Mais Importação")



lead = teste %>%
  select(LEAD_TIME_TOTAL, SELLER,
         DIF_REQUEST_VS_ORDER_CONFIRMATION, 
         LEAD_TIME_LOGISTICA_INTERCIOL, LEAD_TIME_TRANSPORTE_INTERCIOL, LEAD_TIME_NF_ENTRADA,
         LEAD_TIME_NF_REMESSA, LEAD_TIME_LOGISTICA_NACIONAL) %>%
  group_by(SELLER) %>%
  summarise(Total = mean(LEAD_TIME_TOTAL),
            Request_Order = mean(DIF_REQUEST_VS_ORDER_CONFIRMATION),
            Logistica_Internacional = mean(LEAD_TIME_LOGISTICA_INTERCIOL),
            Nf_Entrada = mean(LEAD_TIME_NF_ENTRADA),
            Nf_remessa = mean(LEAD_TIME_NF_REMESSA),
            logistica_nacional = mean(LEAD_TIME_LOGISTICA_NACIONAL)) %>%
  filter(Total != 0)


final = teste %>%
  group_by(SELLER) %>%
  summarise(Total = mean(LEAD_TIME_TOTAL),
            desv_Padrao = sd(LEAD_TIME_TOTAL),
            Variancia = var(LEAD_TIME_TOTAL)) %>%
  filter(Total != 0)
  

teste = filter(teste, LEAD_TIME_TOTAL < 250)
ggplot(teste, aes(x = SELLER, y = LEAD_TIME_TOTAL, fill = SELLER)) +
  geom_boxplot() +
  guides(fill=FALSE)



teste2 = na.omit(df2) %>%
  select(PROJETO, ATENDE.vs.NAO_ATENDE) %>%
  group_by(PROJETO) %>%
  summarise(Atende = sum(ATENDE.vs.NAO_ATENDE == 'ATENDE'),
            Nao_Atende = sum( ATENDE.vs.NAO_ATENDE == 'NAO ATENDE'))


ggplot(teste2, aes(y = Nao_Atende, x= PROJETO, fill = PROJETO)) +
  geom_bar(stat = 'identity') +
  ggtitle('Pedidos Nao Atendidos Por Planta') +
  xlab('PROJETO') +
  ylab('Não Atentidos')


# Teste de Hipotese para saber se ADM DECATUR é a planta que mais atrasa
# Que a planta de ADM Heilderg
# h0 = ADM Decatur <= ADM Heilberg
# H1 = ADM decatur > ADM Heidererg


decatur = teste %>%
  select(SELLER, LEAD_TIME_TOTAL) %>%
  filter(SELLER == 'ADM DECATUR') %>%
  mutate(sample_id = 1)

Heidelberg = teste %>%
  select(SELLER, LEAD_TIME_TOTAL) %>%
  filter(SELLER == 'WILD Heidelberg') %>%
  mutate(sample_id = 2)


erro_padrao_amostra1 = sd(decatur$LEAD_TIME_TOTAL) / sqrt(nrow(decatur))
erro_padrao_amostra2 = sd(Heidelberg$LEAD_TIME_TOTAL) / sqrt(nrow(Heidelberg))

lim_superior1 = mean(decatur$LEAD_TIME_TOTAL) + 1.96 * erro_padrao_amostra1
lim_inferior1 = mean(decatur$LEAD_TIME_TOTAL) - 1.96 * erro_padrao_amostra1

ic1 = c(lim_inferior1, lim_superior1)
ic1
mean(decatur$LEAD_TIME_TOTAL)

lim_superior2 = mean(Heidelberg$LEAD_TIME_TOTAL) + 1.96 * erro_padrao_amostra2
lim_inferior2 = mean(Heidelberg$LEAD_TIME_TOTAL) - 1.96 * erro_padrao_amostra2


ic2= c(lim_inferior2, lim_superior2)
ic2
mean(Heidelberg$LEAD_TIME_TOTAL)

df = rbind(decatur, Heidelberg)

toPlot = summarise(group_by(df, sample_id), mean = mean(LEAD_TIME_TOTAL))
toPlot = mutate(toPlot, lim_inferior2 = ifelse(toPlot$sample_id == 1, ic1[1], ic2[1]))
toPlot = mutate(toPlot, lim_superior2= ifelse(toPlot$sample_id == 1, ic1[2], ic2[2]))
View(toPlot)

ggplot(toPlot, aes(x = sample_id, y=mean, colour = as.factor(c('Decatur', 'Heildeberg')))) +
  geom_point() +
  geom_errorbar(aes(ymin = lim_inferior2, ymax = lim_superior2), width = .1) +
  labs(colour = NULL) +
  ggtitle('Intervalo de Confiança')


# h0 = ADM Decatur <= ADM Heilberg
# H1 = ADM decatur > ADM Heidererg

t.test(decatur$LEAD_TIME_TOTAL, Heidelberg$LEAD_TIME_TOTAL, alternative = 'greater')



###########################################

# Probabilidades
dir()
df = read.csv('probabilidades.csv', sep = ';')
View(df)


df$Data_Requeted <- NULL
df$REQUESTED_ETA = NULL
df$ATA = NULL
df$X = NULL
df$X.1 = NULL


prob_Modal = df  %>%
  select(SELLER, Grupo, Modal, Atende.vs.não.atende) %>%
  group_by(SELLER, Grupo,Modal) %>%
  summarise(prob = sum(Atende.vs.não.atende == 'Atende')/ length(Atende.vs.não.atende)) %>%
  filter(prob != 0)
  
prob = df  %>%
  select(SELLER, Grupo, Atende.vs.não.atende) %>%
  group_by(SELLER, Grupo) %>%
  summarise(prob = sum(Atende.vs.não.atende == 'Atende')/ length(Atende.vs.não.atende))

View(prob_Modal)
View(prob)

filter(prob, SELLER == "ADM DECATUR")


graph <- function(n,p,g){
  x <- dbinom(0:n, size = n, prob = p)
  barplot(x,ylim=c(0,1),names.arg=0:n,
          main= g)
}

# Distribuição PROB Decatur
pdf("Distribuicao_Probabilidade_Decatur.pdf", width = 10, height = 8)

graph(10, 0.154,  "Probabilidade do Pedido Ser Atentido Para ADM Decatur em até 45 dias")
graph(10, 0.462,  "Probabilidade do Pedido Ser Atentido Para ADM Decatur em até 70 dias")
graph(10, 0.333,  "Probabilidade do Pedido Ser Atentido Para ADM Decatur em até 90 dias")
graph(10, 0.52,  "Probabilidade do Pedido Ser Atentido Para ADM Decatur em mais de 90 dias")

dev.off()
