# Carregar bibliotecas necessárias
library(dplyr)
library(readr)
library(stringr)

# Definir o diretório onde os arquivos estão localizados
diretorio <- "resultados/"  # Substitua pelo caminho correto

# Listar todos os arquivos CSV no diretório
file <- read.csv("resultados/Experimento_Breast_piloto.csv",
                     header=TRUE, stringsAsFactors=FALSE)

niveisDeRuido=unique(file$Noise.Level)
algoritmos=unique(file$Classifier)

for (i in 1:nrow(dados1)) {
  
  
}


# Função para extrair o nome do algoritmo do nome do arquivo
extrair_algoritmo <- function(file) {
  # Remove o caminho e mantém apenas o nome do arquivo
  nome_limpo <- basename(nome_arquivo)
  # Extrai o algoritmo baseado no padrão do nome do arquivo
  algoritmo <- str_extract(nome_limpo, "MLP|RF|SVM|XGB")
  return(algoritmo)
}

# Ler e combinar os arquivos em um único dataframe, adicionando a coluna "Algoritmo"
dados <- lapply(arquivos, function(arquivo) {
  df <- read.csv(arquivo)
  df$Algoritmo <- extrair_algoritmo(arquivo)  # Adiciona a coluna Algoritmo
  df <- df[, c("Algoritmo", setdiff(names(df), "Algoritmo"))]  # Reorganiza as colunas
  return(df)
}) %>%
  bind_rows()

# Verificar os dados carregados
str(dados)
head(dados)



# Calculo das repeticoes  ####################
alpha <- 0.05 # nível de significância
d <- 0.5
a <- 4 # Niveis do fator
n_max <- 100 # Número máximo de epetições por instância

# Dataframe para armazenar os resultados
repeticoes <- data.frame(
  Dimensao = integer(),
  Repeticoes = integer(),
  Poder = numeric()
)

for (dim in dimensoes) {
  # Calcula n para cada dimensão para atingir poder >= 0.8
  n <- 2
  power <- 0
  sd <- estatisticas_piloto_df$DesvioPadrao[estatisticas_piloto_df$Dimensao == dim]
  
  while (power < 0.8)
  {
    df1 <- a - 1 # Graus de liberdade do numerador
    df2 <- a * (n - 1) # Graus de liberdade do denominador
    
    # Calcula as médias observadas para a dimensão atual
    mu_config1 <- mean(estudo_piloto$Fbest[estudo_piloto$Dimensao == dim &
                                             estudo_piloto$Configuracao == "Config1"])
    mu_config2 <- mean(estudo_piloto$Fbest[estudo_piloto$Dimensao == dim &
                                             estudo_piloto$Configuracao == "Config2"])
    # Média global
    mu <- mean(c(mu_config1, mu_config2))
    
    # Calcula o NCP
    ncp <- (n*((mu_config1 - mu)^2 + (mu_config2 - mu)^2)) / (sd^2)
    
    F.crit <- qf(alpha, df1, df2, lower.tail = FALSE)
    
    power <- pf(F.crit, df1, df2, ncp, lower.tail = FALSE)
    
    if (n >= n_max) {
      break
    } else {
      n = n + 1
    }
  }
  
  # Armazena os resultados no dataframe
  repeticoes <- rbind(repeticoes, data.frame(Dimensao = dim, Repeticoes = n, Poder = power))
  # Salvar resultados em um arquivo CSV
  write.csv(repeticoes, "repeticoes.csv", row.names = FALSE)
  
}