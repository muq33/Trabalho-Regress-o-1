library(tidyverse)
library(hrbrthemes)
library(viridis)
set.seed(2023)
dados <- read.table("dados.txt", sep = "\t", header = T)

#Declarando algumas coisas úteis
var_resp <- list(
  nome_usavel = "Tempo de vida",
  nome_dados = "LifeSpan",
  cor = rgb(0,0,1,0.5)
  )
covar1 <- list(
  nome_usavel = "Peso do cérebro",
  nome_dados = "BrainWt",
  cor = rgb(1,0,0,0.5)
  )
covar2 <- list( 
  nome_usavel = "Tempo de gestação",
  nome_dados = "Gestation",
  cor = "#4e9c68"
  )



#Removendo as colunas indesejadas

dados <- dados[,-c(1)]

#Bloxplot das variáveis
boxplot(dados[,covar1$nome_dados], xlab = covar1$nome_usavel, col = covar1$cor)
boxplot(dados[,covar2$nome_dados], xlab = covar2$nome_usavel, col = covar2$cor)
boxplot(dados[,var_resp$nome_dados], xlab = var_resp$nome_usavel, col = var_resp$cor)

#Histogramas das variáveis
hist(dados[,covar1$nome_dados], breaks=30, col=covar1$cor, xlab=covar1$nome_usavel, main= paste0("Distribuição de ", covar1$nome_usavel))
hist(dados[,covar2$nome_dados], breaks=30, col=covar2$cor, xlab=covar2$nome_usavel, main= paste0("Distribuição de ", covar2$nome_usavel))
hist(dados[,var_resp$nome_dados], breaks=30, col=var_resp$cor, xlab=var_resp$nome_usavel, main= paste0("Distribuição de ", var_resp$nome_usavel))

#Gráfico de dipersão
ggplot(dados, aes(x=BrainWt, y=LifeSpan)) + geom_point() + xlab(covar1$nome_usavel) + ylab(var_resp$nome_usavel) + ggtitle(paste0(covar1$nome_usavel, " x ", var_resp$nome_usavel))
ggplot(dados, aes(x=Gestation, y=LifeSpan)) + geom_point() + xlab(covar2$nome_usavel) + ylab(var_resp$nome_usavel) + ggtitle(paste0(covar2$nome_usavel, " x ", var_resp$nome_usavel))


#Correlação
cor(dados[,covar1$nome_dados], dados[,var_resp$nome_dados], use = "complete.obs")
cor(dados[,covar2$nome_dados], dados[,var_resp$nome_dados], use = "complete.obs")

#Dividindo os dados em 50/50
tamanho_total <- nrow(dados)
tamanho_treino <- floor(tamanho_total/2)

ind_treino <- sample(tamanho_total, tamanho_treino, replace = F)
ind_teste <- setdiff(seq_len(tamanho_total), ind_treino)

treino <- dados[ind_treino,]
teste <- dados[ind_teste,]


#Ajuste dos modelos
ajuste_cov1_resp <- lm(treino[, var_resp$nome_dados]~treino[,covar1$nome_dados])
summary(ajuste_cov1_resp)
ajuste_cov2_resp <- lm(treino[, var_resp$nome_dados]~treino[,covar2$nome_dados])
summary(ajuste_cov2_resp)

#

