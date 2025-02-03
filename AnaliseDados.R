# Carregar bibliotecas necessárias
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)

# Load data


file <- read.csv("pilotos e teste/estudo_piloto.csv", header=TRUE)
#file <- read.csv("resultados/pilotos/estudo_piloto_3.csv", header=TRUE)
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
         y = "Acurácia Média") + theme(axis.text.x = element_text(angle = 90, hjust = 1))
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
n_max <- 1000 # Número máximo de repetições por instância

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

## Experimento 1 - Analise estatistica ####

model <- aov(Acuracia_Media ~ Algoritmo + PercentualRuidoTreinamento, data = aggdata)
summary(model)
par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)

shapiro.test(model$residuals)

fligner.test(acuracia ~ interaction(Algoritmo,percentualRuidoTreinamento), 
             data = data)
par(mfrow = c(1,1))
ggplot(data, aes(x = factor(percentualRuidoTreinamento), y = acuracia, fill = Algoritmo)) +
  geom_boxplot() +
  facet_wrap(~ Algoritmo) +
  theme_minimal() +
  labs(title = "Experimento 1",
       subtitle = "Accuracy Distribution by LevelNoise and Algorithm",
       x = "Level of Noise", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Experimento 2 ###########################
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
par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)

shapiro.test(model$residuals)

fligner.test(acuracia ~ interaction(Algoritmo,percentualRuidoTreinamento), 
             data = data)
par(mfrow = c(1,1))
ggplot(data, aes(x = factor(percentualRuidoTreinamento), y = acuracia, fill = Algoritmo)) +
  geom_boxplot() +
  facet_wrap(~ Algoritmo) +
  theme_minimal() +
  labs(title="Experimento 3",
       subtitle = "Accuracy Distribution by LevelNoise and Algorithm",
       x = "Level of Noise", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Experimento 3 ###########################
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
par(mfrow = c(2, 2))
plot(model, pch = 20, las = 1)

shapiro.test(model$residuals)

fligner.test(acuracia ~ interaction(Algoritmo,percentualRuidoTreinamento), 
             data = data)
par(mfrow = c(1,1))
ggplot(data, aes(x = factor(percentualRuidoTreinamento), y = acuracia, fill = Algoritmo)) +
  geom_boxplot() +
  facet_wrap(~ Algoritmo) +
  theme_minimal() +
  labs(title="Experimento 3",
       subtitle = "Accuracy Distribution by LevelNoise and Algorithm",
       x = "Level of Noise", y = "Accuracy") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


# Carregar pacotes necessários
library(dplyr)

df <- read.csv("pilotos e teste/estudo_piloto.csv", header=TRUE)

# Definir parâmetros do experimento
alpha <- 0.05  # Nível de significância
power <- 0.8   # Poder do teste (1 - beta)
d_min <- 0.02  # Diferença mínima detectável (exemplo: 2% na acurácia)

# Contar número de classificadores (fatores) e blocos (níveis de ruído)
num_classifiers <- length(unique(df$Classifier))
num_blocks <- length(unique(df$Noise.Level))

# Média global da acurácia
X_global <- mean(df$Accuracy)

# Média por bloco (nível de ruído)
mean_per_block <- df %>%
  group_by(Noise.Level) %>%
  summarise(mean_acc = mean(Accuracy))

# Variância entre blocos (S^2_b)
S2_b <- num_classifiers * sum((mean_per_block$mean_acc - X_global)^2) / (num_blocks - 1)

# Variância dentro dos blocos (S^2_w)
S2_w <- df %>%
  group_by(Noise.Level) %>%
  summarise(SSE = sum((Accuracy - mean(Accuracy))^2)) %>%
  summarise(S2_w = sum(SSE) / (num_classifiers * (num_blocks - 1))) %>%
  pull(S2_w)

# Graus de liberdade
df1 <- num_classifiers - 1
df2 <- (num_classifiers - 1) * (num_blocks - 1)

# Obter o valor crítico da distribuição F
F_critical <- qf(1 - alpha, df1, df2)

# Calcular o número necessário de repetições por bloco
r <- (S2_b / S2_w) * ((df1 * F_critical) / (d_min^2))

# Arredondar para um número inteiro
r <- ceiling(r)

# Exibir os resultados
print(paste("Variância entre blocos (S^2_b):", S2_b))
print(paste("Variância dentro dos blocos (S^2_w):", S2_w))
print(paste("Número de repetições por bloco necessário:", r))
