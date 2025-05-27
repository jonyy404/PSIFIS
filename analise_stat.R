# Carregar as librarys

library(tidyverse)
library(readr)
library(ggplot2)

# Carregar o dataset 

dados <- read.csv("C:/Users/jmrc2/Desktop/dados_stat.csv", sep = ";", dec = ",", header = TRUE)

# Converti tudo em dados numéricos para poder fazer os testes sem ter problemas

dados <- dados %>% mutate(across(c(PSE.A,PSE.R,PSE.RA,LD.A,LD.R,LD.RA,PE,TP,ANS), as.numeric))

# --------------------------------------------------------------------------------
# CORRELAÇÕES EMPATIA E ANSIEDADE COM PSEs (PONTOS DE EQUIVALÊNCIA SUBJETIVA)
# --------------------------------------------------------------------------------

# Correlação 1 - Empatia (Tomada de Perspetiva) e Alegria

cor.test(dados$TP, dados$PSE.A)  

# Correlação 2 - Empatia (Preocupação Empática) e Raiva

cor.test(dados$PE, dados$PSE.R) 

# Correlação 3 - Empatia (Preocupação Empática) e Raiva-Alegria

cor.test(dados$PE, dados$PSE.RA) 

# Correlação 4 - Ansiedade (Ansiedade - Traço) e Raiva-Alegria

cor.test(dados$ANS, dados$PSE.RA) 

# Estas correlações investigam se as variáveis psicológicas empatia e ansiedade têm relação com o enviesamento (PSE) na perceção das emoções. 
# Valores significativos mostram que quanto maior a empatia ou ansiedade, maior ou menor o ponto de equivalência subjetiva, indicando modulação do enviesamento , portanto, apoio à hipótese 1 e 2.

# --------------------------------------------------------------------------------
# T-TESTS COM PONTOS DE EQUIVALÊNCIA SUBJETIVA (PSE) E LIMIARES DIFERENCIAIS (LD)
# --------------------------------------------------------------------------------

# T-test 1 - PSE da Alegria e da Raiva

t.test(dados$PSE.A, dados$PSE.R, paired = TRUE)

# T-test 2 - LD da Alegria e da Raiva

t.test(dados$LD.A, dados$LD.R, paired = TRUE)

# T-test 3 - LD da Alegria e da Raiva-Alegria

t.test(dados$LD.A, dados$LD.RA, paired = TRUE)

# Estes t-tests com medidas pareadas comparam diretamente o enviesamento (PSE) e a sensibilidade (LD) entre emoções Alegria e Raiva. 
# Resultados significativos indicam que as emoções são percebidas de forma diferente, validando a hipótese 1 e 2. 

# --------------------------------------------------------------------------------
# MODELO DE REGRESSÃO LINEAR
# --------------------------------------------------------------------------------

modelo <- lm(PSE.RA ~ TP + PE + ANS, data = dados)
summary(modelo)

# O p.value do ANS é 0.031 logo é significativo, o que indica que a ansiedade é um preditor significativo mesmo controlado pela empatia
# Quanto maior a ansiedade, menor o ponto de equivalência subjetiva → ou seja, os participantes com mais ansiedade percebem a transição entre raiva e alegria de forma diferente
# Defende a Hipotese 1!!!

# --------------------------------------------------------------------------------
# GGPLOTS
# --------------------------------------------------------------------------------

# Grafico de barras com o erro padrão (LD por condição)

dados_ld <- dados %>% select(Subject, LD.A, LD.R, LD.RA) %>% pivot_longer(cols = starts_with("LD."), names_to = "Condicao", values_to = "LD")

ggplot(dados_ld, aes(x = Condicao, y = LD)) +
  stat_summary(fun = mean, geom = "bar", fill = "skyblue") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  ylab("Limiar Condicional (LD)") +
  xlab("Condição Emocional") +
  theme_minimal()

# Gráfico de barras com o PSE (PSE por condição)

dados_pse <- dados %>% select(Subject, PSE.A, PSE.R, PSE.RA) %>% pivot_longer(cols = starts_with("PSE."), names_to = "Condicao", values_to = "PSE")

ggplot(dados_pse, aes(x = Condicao, y = PSE)) +
  stat_summary(fun = mean, geom = "bar", fill = "red") +
  stat_summary(fun.data = mean_se, geom = "errorbar", width = 0.2) +
  ylab("Pontos de Equivalência Subjetiva (PSE)") +
  xlab("Condição Emocional") +
  theme_minimal()

# Gráfico da Correlação 1 - Empatia (Tomada de Perspetiva) e Alegria

ggplot(dados, aes(x = TP, y = PSE.A)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "blue") +
  labs(x = "Tomada de Perspetiva", y = "PSE - Alegria", title = "Correlação entre TP e PSE-Alegria") +
  theme_minimal()

# Gráfico da Correlação 2 - Empatia (Preocupação Empática) e Raiva-Alegria

ggplot(dados, aes(x = PE, y = PSE.RA)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "orange") +
  labs(x = "Preocupação Empática", y = "PSE - RaivaAlegria", title = "Correlação entre PE e PSE-RaivaAlegria") +
  theme_minimal()

# Gráfico da Correlação 3 - Ansiedade (Ansiedade - Traço) e Raiva-Alegria

ggplot(dados, aes(x = ANS, y = PSE.RA)) +
  geom_point() +
  geom_smooth(method = "lm", se = TRUE, color = "green") +
  labs(x = "Ansiedade - Traço", y = "PSE - RaivaAlegria", title = "Correlação entre ANS e PSE-RaivaAlegria") +
  theme_minimal()
