# Carregar as librarys

library(tidyverse)
library(readr)
library(ggplot2)

# Carregar o dataset 

dados <- read.csv(file.choose(), sep = ";", dec = ",", header = TRUE)

# Converti tudo em dados numéricos para poder fazer os testes sem ter problemas

dados <- dados %>% mutate(across(c(PSE.A,PSE.R,PSE.RA,LD.A,LD.R,LD.RA,PE,TP,ANS), as.numeric))

labels <- paste(names(table(dados$Sex)), " (", table(dados$Sex), " p)", sep = "")
pie(table(dados$Sex),
    labels = labels,
    col = c("pink", "lightblue"),
    main = "Distribuição por Sexo")

colMeans(dados$Age)
mean(dados$Age)
boxplot(dados$Age,
        col = "orange",
        main = "Distribuição por idade",
        ylab = "Idade")

# Curva psicofisica alegria

# Selecionar apenas as colunas das intensidades
dados_alegria<-dados[,5:11]
dados_alegria

# Calcular as médias para cada intensidade
medias_a <- colMeans(dados_alegria, na.rm = TRUE)
medias_a

estimulos_a <- as.numeric(gsub("A", "", names(medias_a)))
estimulos_a

plot(estimulos_a, medias_a,
     type = "b",                # pontos e linhas
     col = "blue",
     pch = 19,
     xlab = "Intensidade do Estímulo",
     ylab = "Proporção de Respostas 'sim' ",
     main = "Curva Psicometrica - Alegria")
for (y in medias_a) {
  abline(h = y, lty = 2, col = "gray")
}

z_scores <- qnorm(medias_a)
medias_a_corr <- pmin(pmax(medias_a, 0.01), 0.99)
z_scores <- qnorm(medias_a_corr)
modelo <- lm(z_scores ~ estimulos_a)
summary(modelo)
z_targets <- qnorm(c(0.25, 0.5, 0.75))
a <- coef(modelo)[1]
b <- coef(modelo)[2]
x_targets <- (z_targets - a) / b
z_targets
x_targets
z_scores

declive <- coef(modelo)[2]
declive


# Curva psicofisica raiva

# Selecionar apenas as colunas das intensidades
dados_raiva<-dados[,12:18]
dados_raiva

# Calcular as médias para cada intensidade
medias_r <- colMeans(dados_raiva, na.rm = TRUE)
medias_r

estimulos_r <- as.numeric(gsub("R", "", names(medias_r)))
estimulos_r

plot(estimulos_r, medias_r,
     type = "b",                # pontos e linhas
     col = "blue",
     pch = 19,
     xlab = "Intensidade do Estímulo",
     ylab = "Proporção de Respostas 'sim' ",
     main = "Curva Psicometrica - Raiva")
for (y in medias_r) {
  abline(h = y, lty = 2, col = "gray")
}

z_scores <- qnorm(medias_r)
medias_r_corr <- pmin(pmax(medias_r, 0.01), 0.99)
z_scores <- qnorm(medias_r_corr)
modelo <- lm(z_scores ~ estimulos_r)
summary(modelo)
z_targets <- qnorm(c(0.25, 0.5, 0.75))
a <- coef(modelo)[1]
b <- coef(modelo)[2]
x_targets <- (z_targets - a) / b
z_targets
x_targets
z_scores

declive <- coef(modelo)[2]
declive


# Curva psicofisica alegria-raiva

# Selecionar apenas as colunas das intensidades
dados_a_r<-dados[,19:25]
dados_a_r

# Calcular as médias para cada intensidade
medias_a_r <- colMeans(dados_a_r, na.rm = TRUE)
medias_a_r

estimulos_a_r <- as.numeric(gsub("RA", "", names(medias_a_r)))
estimulos_a_r

plot(estimulos_a_r, medias_a_r,
     type = "b",                # pontos e linhas
     col = "blue",
     pch = 19,
     xlab = "Intensidade de Alegria relativa a Raiva",
     ylab = "Proporção de Respostas 'Alegria'",
     main = 'Curva Psicometrica Alegria - Raiva')
for (y in medias_a_r) {
  abline(h = y, lty = 2, col = "gray")
}

z_scores <- qnorm(medias_a_r)
medias_ar_corr <- pmin(pmax(medias_a_r, 0.01), 0.99)
z_scores <- qnorm(medias_ar_corr)
modelo <- lm(z_scores ~ estimulos_a_r)
summary(modelo)
z_targets <- qnorm(c(0.25, 0.5, 0.75))
a <- coef(modelo)[1]
b <- coef(modelo)[2]
x_targets <- (z_targets - a) / b
z_targets
x_targets
z_scores

declive <- coef(modelo)[2]
declive
