# Carregar bibliotecas necessárias
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)


#rm(list = ls())
# Leitua Inicial dos arquivos ##################################################
# Definir o diretório onde os arquivos estão localizados
diretorio <- "resultados/"  # Substitua pelo caminho correto

# Listar todos os arquivos CSV no diretório
file <- read.csv("resultados/Experimento_Breast_piloto.csv",
                     header=TRUE, stringsAsFactors=FALSE)


niveisDeRuido=unique(file$Noise.Level)
algoritmos=unique(file$Classifier)
repeticoes=unique(file$IndexRep)

df_summary <- file %>%
  group_by(Classifier, IndexRep) %>%
  summarise(Mean_Accuracy = mean(Accuracy, na.rm = TRUE))

# View the summary
print(df_summary)

file <- file %>% dplyr::select(Classifier, X, Accuracy, Noise.Level)%>% 
  arrange(Classifier) 
dadosArquivo <- file %>%rename(Algoritmo = Classifier,
                               X = X,
                               acuracia = Accuracy,
                               percentualRuidoTreinamento = `Noise.Level`)

# Verificar os dados carregados
str(dadosArquivo)
head(dadosArquivo)

# Gráfico ##################################################
# Transformar Percentual de Ruído em fator (se ainda não estiver)
dadosArquivo$percentualRuidoTreinamento <- as.factor(dadosArquivo$percentualRuidoTreinamento)

# Agregar os dados: média da acurácia por algoritmo e nível de ruído
aggdata <- aggregate(x = dadosArquivo$acuracia, 
                     by = list(Algoritmo = dadosArquivo$Algoritmo, 
                               percentualRuidoTreinamento = dadosArquivo$percentualRuidoTreinamento), 
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
p + labs(title = "Desempenho dos Algoritmos por Nível de Ruído",
         x = "Percentual de Ruído no Treinamento",
         y = "Acurácia Média")



# Calculo das repeticoes  ####################
alpha <- 0.05 # nível de significância
d <- 0.5
a <- length(unique(aggdata$Algoritmo))  # Número de algoritmos
n_max <- 100 # Número máximo de epetições por instância

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
    
    # Calcular a média da acurácia para cada algoritmo
    mu_algoritmo <- tapply(aggdata$Acuracia_Media[aggdata$PercentualRuidoTreinamento == ruido],
                           unique(aggdata$Algoritmo), mean)
    
    # Média global da acurácia nesse nível de ruído
    mu_global <- mean(mu_algoritmo)
    
    # Calcula o NCP
    ncp <- (n * sum((mu_algoritmo - mu_global)^2)) / (sd^2)
    
    F.crit <- qf(alpha, df1, df2, lower.tail = FALSE)
    
    power <- pf(F.crit, df1, df2, ncp, lower.tail = FALSE)
    
    if (n >= n_max) {
      break
    } else {
      n = n + 1
    }
    
    # Armazenar resultados
    repeticoes <- rbind(repeticoes, 
                        data.frame(PercentualRuidoTreinamento = ruido, 
                                   Repeticoes = n, 
                                   Poder = power))
  }
  
  # Salvar resultados em um arquivo CSV
  write.csv(repeticoes, "repeticoes.csv", row.names = FALSE)
  
}
