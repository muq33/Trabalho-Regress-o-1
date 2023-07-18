library(tidyverse)
library(hrbrthemes)
library(viridis)
library(ggplot2)
library(xtable)
library(nortest)
library(lmtest)
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

#Inputando dados
dados[,covar2$nome_dados][is.na(dados[,covar2$nome_dados])] <- mean(dados[,covar2$nome_dados], na.rm = TRUE)
dados[,var_resp$nome_dados][is.na(dados[,var_resp$nome_dados])] <- mean(dados[,var_resp$nome_dados], na.rm = TRUE)


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

#intervalos de confiança para os parâmetros dos modelos
confint(ajuste_cov1_resp) #Lembrar de escrever beta_1 no lugar dessa saída horrível
confint(ajuste_cov2_resp)

#Análise de diagnóstico

#Resíduos x predito
# Gráfico Resíduos vs. Valores Ajustados
plot(ajuste_cov1_resp$fitted.values, ajuste_cov1_resp$residuals,
     xlab = "Valores Ajustados", ylab = "Resíduos",
     main = "Resíduos vs. Valores Ajustados")
abline(0, 0, col = "red")
plot(ajuste_cov2_resp$fitted.values, ajuste_cov1_resp$residuals,
     xlab = "Valores Ajustados", ylab = "Resíduos",
     main = "Resíduos vs. Valores Ajustados")
abline(0, 0, col = "red")

#Histograma dos resíduos
hist(ajuste_cov1_resp$residuals, main = "Histograma de Resíduos",
     xlab = "Resíduos", col = covar1$cor)
hist(ajuste_cov2_resp$residuals, main = "Histograma de Resíduos",
     xlab = "Resíduos", col = covar2$cor)


#QQ plot
qqnorm(ajuste_cov1_resp$residuals)
qqline(ajuste_cov1_resp$residuals)

qqnorm(ajuste_cov2_resp$residuals)
qqline(ajuste_cov2_resp$residuals)

#teste de normalidade dos resíduos
shapiro.test(ajuste_cov1_resp$residuals) 
lillie.test(ajuste_cov1_resp$residuals)
ad.test(ajuste_cov1_resp$residuals)

shapiro.test(ajuste_cov2_resp$residuals) 
lillie.test(ajuste_cov2_resp$residuals)
ad.test(ajuste_cov2_resp$residuals)



#Encontrando a melhor transformação que leve a normalidade das covariaveis
norm_covar1 <- bestNormalize::bestNormalize(dados[,covar1$nome_dados])
norm_covar2 <- bestNormalize::bestNormalize(dados[,covar2$nome_dados])

dados[,covar1$nome_dados] <- norm_covar1$x.t
dados[,covar2$nome_dados] <- norm_covar2$x.t

#Dividindo conjunto em teste treino novamente
tamanho_total <- nrow(dados)
tamanho_treino <- floor(tamanho_total/2)

ind_treino <- sample(tamanho_total, tamanho_treino, replace = F)
ind_teste <- setdiff(seq_len(tamanho_total), ind_treino)

treino <- dados[ind_treino,]
teste <- dados[ind_teste,]

#Ajuste dos modelos 2
ajuste_cov1_resp <- lm(treino[, var_resp$nome_dados]~treino[,covar1$nome_dados])
summary(ajuste_cov1_resp)
ajuste_cov2_resp <- lm(treino[, var_resp$nome_dados]~treino[,covar2$nome_dados])
summary(ajuste_cov2_resp)

#Gráfico da reta estimada
ggplot(treino, aes(x = BrainWt, y = LifeSpan)) +
  geom_point() +                     # Pontos de dispersão
  geom_abline(intercept = ajuste_cov1_resp$coefficients[1], slope = ajuste_cov1_resp$coefficients[2], color = "red") +  # Reta estimada
  labs(title = "Reta estimada na amostra de treino",
       x = covar1$nome_usavel,
       y = covar2$nome_usavel)
ggplot(treino, aes(x = Gestation, y = LifeSpan)) +
  geom_point() +                     # Pontos de dispersão
  geom_abline(intercept = ajuste_cov2_resp$coefficients[1], slope = ajuste_cov2_resp$coefficients[2], color = "red") +  # Reta estimada
  labs(title = "Reta estimada na amostra de treino",
       x = covar1$nome_usavel,
       y = covar2$nome_usavel)

#Análise de diagnóstico 2

#Resíduos x predito
# Gráfico Resíduos vs. Valores Ajustados
plot(ajuste_cov1_resp$fitted.values, ajuste_cov1_resp$residuals,
     xlab = "Valores Ajustados", ylab = "Resíduos",
     main = "Resíduos vs. Valores Ajustados")
abline(0, 0, col = "red")
plot(ajuste_cov2_resp$fitted.values, ajuste_cov1_resp$residuals,
     xlab = "Valores Ajustados", ylab = "Resíduos",
     main = "Resíduos vs. Valores Ajustados")
abline(0, 0, col = "red")

#QQ plot
qqnorm(ajuste_cov1_resp$residuals)
qqline(ajuste_cov1_resp$residuals)

qqnorm(ajuste_cov2_resp$residuals)
qqline(ajuste_cov2_resp$residuals)

#teste de normalidade dos resíduos
shapiro.test(ajuste_cov1_resp$residuals) 
lillie.test(ajuste_cov1_resp$residuals)
ad.test(ajuste_cov1_resp$residuals) #Não é normal


shapiro.test(ajuste_cov2_resp$residuals) #Não é normal
lillie.test(ajuste_cov2_resp$residuals)
ad.test(ajuste_cov2_resp$residuals)


#ANOVA
summary(aov(ajuste_cov1_resp))

summary(aov(ajuste_cov2_resp))
