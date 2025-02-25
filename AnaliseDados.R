# Carregar bibliotecas necessárias
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
library(multcomp)
library(xtable)

#rm(list=ls())

# Load data
file <- read.csv("resultados/pilotos/estudo_piloto_3.csv",
                     header=TRUE)
data <- file %>%rename(Algoritmo = Classifier,
                       X = X,
                       acuracia = Accuracy,
                       percentualRuidoTreinamento = `Noise.Level`)
head(data)

# Gráfico ##################################################
# Transformar Percentual de Ruído em fator
data$percentualRuidoTreinamento <- as.factor(data$percentualRuidoTreinamento)

# Agregar os dados: média da acurácia por algoritmo e nível de ruído
aggdata <- aggregate(x = data$acuracia, 
                     by = list(Algoritmo = data$Algoritmo, 
                               percentualRuidoTreinamento = data$percentualRuidoTreinamento), 
                     FUN = mean)

# Rename columns
names(aggdata) <- c("Algoritmo", 
                    "PercentualRuidoTreinamento",
                    "Acuracia_Media")

# Coerce categorical variables to factors
for (i in 1:2){
  aggdata[, i] <- as.factor(aggdata[, i])
}

#pdf("./figs/ggplot_piloto.pdf", width = 5, height = 5) # <-- uncomment to save plot

# Gráfico de linhas para visualizar o impacto do ruído na acurácia
p <- ggplot(aggdata, aes(x = PercentualRuidoTreinamento, 
                         y = Acuracia_Media, 
                         group = Algoritmo, 
                         colour = Algoritmo))
p <- p + geom_line(linetype=2) + geom_point(size=5)
p <- p + labs(title = "Desempenho dos Algoritmos por Nível de Ruído",
         x = "Percentual de Ruído no Treinamento",
         y = "Acurácia Média")
print(p)

# Calculo dos blocos  ####################
d = 0.5        # Mínima diferença de importância prática (padronizada)
alpha <- 0.05  # Nível de significância
p <- 1 - alpha # Nível de confiança

n <- 2
power <- 0
while (power < 0.8)
{
  df <- n - 1         # n - 1 graus de liberdade
  ncp <- d * sqrt(n)  # Non-centrality parameter
  
  power <- pt(qt(p,df), df, ncp=ncp, lower.tail=FALSE)
  n <- n + 1
}
n
power

# Calculo das repeticoes  ####################
alpha <- 0.05 # nível de significância
d <- 0.5
a <- length(unique(aggdata$Algoritmo))  # Número de algoritmos
n_max <- 1000 # Número máximo de epetições por instância

# Dataframe para armazenar os resultados
# Criar dataframe para armazenar os resultados
repeticoes <- data.frame(
  Algoritmo = character(),
  Repeticoes = integer(),
  Poder = numeric()
)

for (ruido in unique(aggdata$PercentualRuidoTreinamento)) {
  # Calcula n para cada instância para atingir poder >= 0.8
  n <- 2
  power <- 0
  
  # Obter desvio padrão da acurácia média nos algoritmos para esse nível de ruído
  sd <- sd(aggdata$Acuracia_Media[aggdata$PercentualRuidoTreinamento == ruido])
  
  while (power < 0.8)
  {
    df1 <- a - 1 # Graus de liberdade do numerador
    df2 <- a * (n - 1) # Graus de liberdade do denominador
    
    # Calcular a média da acurácia para cada algoritmo no nível de ruído atual
    mu_rf  <- aggdata$Acuracia_Media[aggdata$PercentualRuidoTreinamento == ruido & aggdata$Algoritmo == "Random Forest"]
    mu_svm <- aggdata$Acuracia_Media[aggdata$PercentualRuidoTreinamento == ruido & aggdata$Algoritmo == "SVM (RBF)"]
    mu_xgb <- aggdata$Acuracia_Media[aggdata$PercentualRuidoTreinamento == ruido & aggdata$Algoritmo == "XGBoost"]
    mu_mlp <- aggdata$Acuracia_Media[aggdata$PercentualRuidoTreinamento == ruido & aggdata$Algoritmo == "MLP"]
    
    # Média global considerando os quatro classificadores
    mu <- mean(c(mu_rf, mu_svm, mu_xgb, mu_mlp))
    
    # Calcula o NCP
    ncp <- (n*((mu_rf - mu)^2 + (mu_svm - mu)^2 + (mu_xgb - mu)^2 + (mu_mlp - mu)^2)) / (sd^2)
    
    F.crit <- qf(alpha, df1, df2, lower.tail = FALSE)
    
    power <- pf(F.crit, df1, df2, ncp, lower.tail = FALSE)

    if (n >= n_max) {
      break
    } else {
      n = n + 1
    }

  }
  
  # Armazenar resultados
  repeticoes <- rbind(repeticoes, 
                      data.frame(PercentualRuidoTreinamento = ruido, 
                                 Repeticoes = n, 
                                 Poder = power))
  # Salvar resultados em um arquivo CSV
  write.csv(repeticoes, "repeticoes.csv", row.names = FALSE)
  
}

# Análise Estatística ######################################
model <- aov(Acuracia_Media ~ Algoritmo + PercentualRuidoTreinamento, data = aggdata)
summary(model)
par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)

shapiro.test(model$residuals)

fligner.test(acuracia ~ interaction(Algoritmo,percentualRuidoTreinamento), 
             data = data)



# Experimento 1 ###########################
file <- read.csv("resultados/experimento/resultadosExperimento1.csv",
                 header=TRUE)
data <- file %>%rename(Algoritmo = Classifier,
                       X = X,
                       acuracia = Accuracy,
                       percentualRuidoTreinamento = `Noise.Level`)
head(data)
data$percentualRuidoTreinamento <- round(data$percentualRuidoTreinamento,2)
data$percentualRuidoTreinamento <- as.factor(data$percentualRuidoTreinamento)
unique(data$percentualRuidoTreinamento)
# Agregar os dados: média da acurácia por algoritmo e nível de ruído
aggdata <- aggregate(x = data$acuracia, 
                     by = list(Algoritmo = data$Algoritmo, 
                               percentualRuidoTreinamento = data$percentualRuidoTreinamento), 
                     FUN = mean)

# Rename columns
names(aggdata) <- c("Algoritmo", 
                    "PercentualRuidoTreinamento",
                    "Acuracia_Media")
# Coerce categorical variables to factors
for (i in 1:2){
  aggdata[, i] <- as.factor(aggdata[, i])
}


#pdf("./figs/ggplot_piloto.pdf", width = 5, height = 5) # <-- uncomment to save plot

# Gráfico de linhas para visualizar o impacto do ruído na acurácia
p <- ggplot(aggdata, aes(x = PercentualRuidoTreinamento, 
                         y = Acuracia_Media, 
                         group = Algoritmo, 
                         colour = Algoritmo))
p <- p + geom_line(linetype=2) + geom_point(size=5)
p + labs( subtitle = "Desempenho dos Algoritmos por Nível de Ruído",
          title = "Experimento 1",
         x = "Percentual de Ruído no Treinamento",
         y = "Acurácia Média")

## Experimento 1 - Analise estatistica #########################################

model <- aov(Acuracia_Media ~ Algoritmo + PercentualRuidoTreinamento, data = aggdata)
summary(model)
# Convert to LaTeX
latex_table <- xtable(model)

# Print LaTeX code
print(latex_table, type = "latex")

plot(model, which = 1,pch=16)  # Residuals vs Fitted
plot(model, which = 2,pch=16)  # Q-Q Plot of Residuals

shapiro.test(model$residuals)

fligner.test(acuracia ~ interaction(Algoritmo,percentualRuidoTreinamento), 
             data = data)

par(mfrow = c(1,1))
png("figuras/experimento1_normalidadeResiduos.png", width = 800, height = 600, res = 150)  # Save as PNG
par(cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)  # Increase text sizes
plot(model, which = 2, main="Experimento 1: Q-Q Plot of Residuals",pch=16)  # Residuals vs Fitted
dev.off() 
png("figuras/experimento1_varianciaResiduos.png", width = 800, height = 600, res = 150)  # Save as PNG
par(cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)  # Increase text sizes
plot(model, which = 1, main = "Experimento 1: Residuals vs Fitted",pch=16)  # Residuals vs Fitted
dev.off() 


ggplot(data, aes(x = factor(percentualRuidoTreinamento), y = acuracia, fill = Algoritmo)) +
  geom_boxplot() +
  facet_wrap(~ Algoritmo) +
  theme_minimal() +
  labs(title = "Experimento 1",
       subtitle = "Accuracy Distribution by LevelNoise and Algorithm",
       x = "Level of Noise", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

### Experimento 1 - Comparação Múltipla #########################################
# Situation 1: all vs. all
mc1    <- glht(model, 
               linfct = mcp(Algoritmo = "Tukey"))
mc1_CI <- confint(mc1, level = 0.95)

 png(filename = "figuras/tukey_experimento1.png",
     width = 900, height = 600)
par(cex.main = 1.5, cex.lab = 1.5, cex.axis = 2)  # Reset text sizes to default
par(mar = c(5, 15, 4, 2))  # Adjust margins: bottom, left, top, right
plot(mc1_CI, 
     xlab       = "Diferença de Acurácia (%)",
     sub        = "Breast Cancer",
     cex.axis   = 1.2,
     main="Experimento 1: Comparação Múltipla da Acurácia Média",
     cex        = 2)
title("Intervalo de confiança de 95%", line = 0.5, cex.main=1)

dev.off()
summary(mc1)

# Experimento 2 ####################################################################
file <- read.csv("resultados/experimento/resultadosExperimento2.csv",
                 header=TRUE)
data <- file %>%rename(Algoritmo = Classifier,
                       X = X,
                       acuracia = Accuracy,
                       percentualRuidoTreinamento = `Noise.Level`)
head(data)
data$percentualRuidoTreinamento <- round(data$percentualRuidoTreinamento,2)
data$percentualRuidoTreinamento <- as.factor(data$percentualRuidoTreinamento)
unique(data$percentualRuidoTreinamento)
# Agregar os dados: média da acurácia por algoritmo e nível de ruído
aggdata <- aggregate(x = data$acuracia, 
                     by = list(Algoritmo = data$Algoritmo, 
                               percentualRuidoTreinamento = data$percentualRuidoTreinamento), 
                     FUN = mean)

# Rename columns
names(aggdata) <- c("Algoritmo", 
                    "PercentualRuidoTreinamento",
                    "Acuracia_Media")
# Coerce categorical variables to factors
for (i in 1:2){
  aggdata[, i] <- as.factor(aggdata[, i])
}


#pdf("./figs/ggplot_piloto.pdf", width = 5, height = 5) # <-- uncomment to save plot

# Gráfico de linhas para visualizar o impacto do ruído na acurácia
p <- ggplot(aggdata, aes(x = PercentualRuidoTreinamento, 
                         y = Acuracia_Media, 
                         group = Algoritmo, 
                         colour = Algoritmo))
p <- p + geom_line(linetype=2) + geom_point(size=5)
p + labs( subtitle= "Desempenho dos Algoritmos por Nível de Ruído",
          title= "Experimento 2",
         x = "Percentual de Ruído no Treinamento",
         y = "Acurácia Média")




## Experimento 2 - Analise estatistica ####

model <- aov(Acuracia_Media ~ Algoritmo + PercentualRuidoTreinamento, data = aggdata)
summary(model)

# Convert to LaTeX
latex_table <- xtable(model)

# Print LaTeX code
print(latex_table, type = "latex")

par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)

shapiro.test(model$residuals)

fligner.test(acuracia ~ interaction(Algoritmo,percentualRuidoTreinamento), 
             data = data)

par(mfrow = c(1,1))
png("figuras/experimento2_normalidadeResiduos.png", width = 800, height = 600, res = 150)  # Save as PNG
par(cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)  # Increase text sizes
plot(model, which = 2, main="Experimento 2: Q-Q Plot of Residuals",pch=16)  # Residuals vs Fitted
dev.off() 
png("figuras/experimento2_varianciaResiduos.png", width = 800, height = 600, res = 150)  # Save as PNG
par(cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)  # Increase text sizes
plot(model, which = 1, main = "Experimento 2: Residuals vs Fitted",pch=16)  # Residuals vs Fitted
dev.off() 


par(mfrow = c(1,1))
ggplot(data, aes(x = factor(percentualRuidoTreinamento), y = acuracia, fill = Algoritmo)) +
  geom_boxplot() +
  facet_wrap(~ Algoritmo) +
  theme_minimal() +
  labs(title="Experimento 3",
       subtitle = "Accuracy Distribution by LevelNoise and Algorithm",
       x = "Level of Noise", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Experimento 3 #################################################################################
file <- read.csv("resultados/experimento/resultadosExperimento3.csv",
                 header=TRUE)
data <- file %>%rename(Algoritmo = Classifier,
                       X = X,
                       acuracia = Accuracy,
                       percentualRuidoTreinamento = `Noise.Level`)
head(data)
data$percentualRuidoTreinamento <- round(data$percentualRuidoTreinamento,2)
data$percentualRuidoTreinamento <- as.factor(data$percentualRuidoTreinamento)
unique(data$percentualRuidoTreinamento)
# Agregar os dados: média da acurácia por algoritmo e nível de ruído
aggdata <- aggregate(x = data$acuracia, 
                     by = list(Algoritmo = data$Algoritmo, 
                               percentualRuidoTreinamento = data$percentualRuidoTreinamento), 
                     FUN = mean)

# Rename columns
names(aggdata) <- c("Algoritmo", 
                    "PercentualRuidoTreinamento",
                    "Acuracia_Media")
# Coerce categorical variables to factors
for (i in 1:2){
  aggdata[, i] <- as.factor(aggdata[, i])
}


#pdf("./figs/ggplot_piloto.pdf", width = 5, height = 5) # <-- uncomment to save plot

# Gráfico de linhas para visualizar o impacto do ruído na acurácia
p <- ggplot(aggdata, aes(x = PercentualRuidoTreinamento, 
                         y = Acuracia_Media, 
                         group = Algoritmo, 
                         colour = Algoritmo))
p <- p + geom_line(linetype=2) + geom_point(size=5)
p + labs( subtitle= "Desempenho dos Algoritmos por Nível de Ruído",
          title= "Experimento 3",
          x = "Percentual de Ruído no Treinamento",
          y = "Acurácia Média")

## Experimento 3 - Analise estatistica ####

model <- aov(Acuracia_Media ~ Algoritmo + PercentualRuidoTreinamento, data = aggdata)
summary(model)

# Convert to LaTeX
latex_table <- xtable(model)

# Print LaTeX code
print(latex_table, type = "latex")



par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)

shapiro.test(model$residuals)

fligner.test(acuracia ~ interaction(Algoritmo,percentualRuidoTreinamento), 
             data = data)

par(mfrow = c(1,1))
png("figuras/experimento3_normalidadeResiduos.png", width = 800, height = 600, res = 150)  # Save as PNG
par(cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)  # Increase text sizes
plot(model, which = 2, main = "Experimento 3: Q-Q Plot of Residuals",pch=16)  # Residuals vs Fitted
dev.off() 
png("figuras/experimento3_varianciaResiduos.png", width = 800, height = 600, res = 150)  # Save as PNG
par(cex.main = 1.5, cex.lab = 1.3, cex.axis = 1.2)  # Increase text sizes
plot(model, which = 1, main = "Experimento 3: Residuals vs Fitted",pch=16)  # Residuals vs Fitted
dev.off() 



par(mfrow = c(1,1))
ggplot(data, aes(x = factor(percentualRuidoTreinamento), y = acuracia, fill = Algoritmo)) +
  geom_boxplot() +
  facet_wrap(~ Algoritmo) +
  theme_minimal() +
  labs(title="Experimento 3",
       subtitle = "Accuracy Distribution by LevelNoise and Algorithm",
       x = "Level of Noise", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


### Experimento 3 - Comparação Múltipla #########################################
mc1    <- glht(model, 
               linfct = mcp(Algoritmo = "Tukey"))
mc1_CI <- confint(mc1, level = 0.95)

png(filename = "figuras/tukey_experimento3.png",
    width = 900, height = 600)
par(cex.main = 1.5, cex.lab = 1.5, cex.axis = 2)  # Reset text sizes to default
par(mar = c(5, 15, 4, 2))  # Adjust margins: bottom, left, top, right
plot(mc1_CI, 
     xlab       = "Diferença de Acurácia (%)",
     sub        = "Breast Cancer",
     cex.axis   = 1.2,
     main="Experimento 3: Comparação Múltipla da Acurácia Média",
     cex        = 2)
title("Intervalo de confiança de 95%", line = 0.5, cex.main=1)

dev.off()
summary(mc1)