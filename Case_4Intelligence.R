install.packages("readxl")
install.packages("tidyverse")
install.packages("tidyr")
install.packages("dplyr")
install.packages("zoo")
install.packages("seasonal")

library(readxl)
library(tidyverse)
library(tidyr)
library(dplyr)
library(zoo)
library(seasonal)

df_diesel <- read_xlsx("dados_desafiodatascientistintern_vendas_distribuidoras_anp.xlsx")
df_gasolina <- read_xlsx("dados_desafiodatascientistintern_vendas_distribuidoras_anp.xlsx", sheet = "gasolina")
df_etanol <- read_xlsx("dados_desafiodatascientistintern_vendas_distribuidoras_anp.xlsx", sheet = "etanol")

df_diesel_tidy <- gather(df_diesel, Ano, Diesel, -regiao, -meses)
df_gasolina_tidy <- gather(df_gasolina, Ano, Gasolina, -regiao, -meses)
df_etanol_tidy <- gather(df_etanol, Ano, Etanol, -regiao, -meses)

df_diesel_tidy$Data <- as.yearmon(paste(df_diesel_tidy$Ano, df_diesel_tidy$meses), "%Y %m")
df_gasolina_tidy$Data <- as.yearmon(paste(df_gasolina_tidy$Ano, df_gasolina_tidy$meses), "%Y %m")
df_etanol_tidy$Data <- as.yearmon(paste(df_etanol_tidy$Ano, df_etanol_tidy$meses), "%Y %m")

df_diesel_tidy <- df_diesel_tidy[ ,-2:-3]
df_gasolina_tidy <- df_gasolina_tidy[ ,-2:-3]
df_etanol_tidy <- df_etanol_tidy[ ,-2:-3]

str(df_diesel_tidy)
str(df_gasolina_tidy)
str(df_etanol_tidy)

df_diesel_tidy$Data <- as.Date(df_diesel_tidy$Data)
df_gasolina_tidy$Data <- as.Date(df_gasolina_tidy$Data)
df_etanol_tidy$Data <- as.Date(df_etanol_tidy$Data)

#série original Diesel
for (i in unique(df_diesel_tidy$regiao)) {
  print(ggplot(df_diesel_tidy %>% filter(regiao == i), aes(Data, Diesel)) + 
          geom_line() + ggtitle(paste("Evolução Vendas Diesel - Região", i, sep = " ")) + theme(plot.title = element_text(hjust = 0.5)))
}

#Por região: série original vs ajuste sazonal (Diesel)
for (i in unique(df_diesel_tidy$regiao)) {
  df_diesel_tidy_plot <- df_diesel_tidy %>% filter(regiao == i)
  plot(df_diesel_tidy_plot$Data, df_diesel_tidy_plot$Diesel, main = paste("Evolução Vendas Diesel - Região", i, sep = " "))
  
  regiao_br <- ts(df_diesel_tidy_plot$Diesel, start = c(2000, 1) , frequency = 12) 
  dessaz_br <- seas(regiao_br, x11 = "")
  plot(dessaz_br, main = paste("Evolução Vendas Diesel com Ajuste Sazonal - Região", i, sep = " "))
  grid(nx = 23)
  legend("topleft", legend = c("Original", "Ajustada"), col = c(1, 2), lwd = c(1, 2), 
         lty = 1, bty = "n", cex = 0.6)
}

#Consumo de Diesel por estado média dos últimos 20 anos
df_diesel_tidy_semBR <- df_diesel_tidy %>% filter(!regiao == "br")
barplot(tapply(df_diesel_tidy_semBR$Diesel, df_diesel_tidy_semBR$regiao, mean),
        col = "orange", horiz = T, main = "Média das vendas de Diesel por estado 2000 - 2020")

#série original Gasolina
for (i in unique(df_gasolina_tidy$regiao)) {
  print(ggplot(df_gasolina_tidy %>% filter(regiao == i), aes(Data, Gasolina)) + 
          geom_line() + ggtitle(paste("Evolução Vendas Gasolina - Região", i, sep = " ")) + theme(plot.title = element_text(hjust = 0.5)))
}

#Por região: série original vs ajuste sazonal (Gasolina)
for (i in unique(df_gasolina_tidy$regiao)) {
  df_gasolina_tidy_plot <- df_gasolina_tidy %>% filter(regiao == i)
  plot(df_gasolina_tidy_plot$Data, df_gasolina_tidy_plot$Gasolina, main = paste("Evolução Vendas Gasolina - Região", i, sep = " "))
  
  regiao_br <- ts(df_gasolina_tidy_plot$Gasolina, start = c(2000, 1) , frequency = 12) 
  dessaz_br <- seas(regiao_br, x11 = "")
  plot(dessaz_br, main = paste("Evolução Vendas Gasolina com Ajuste Sazonal - Região", i, sep = " "))
  grid(nx = 23)
  legend("topleft", legend = c("Original", "Ajustada"), col = c(1, 2), lwd = c(1, 2), 
         lty = 1, bty = "n", cex = 0.6)
}

#Consumo de Gasolina por estado média dos últimos 20 anos
df_gasolina_tidy_semBR <- df_gasolina_tidy %>% filter(!regiao == "br")
barplot(tapply(df_gasolina_tidy_semBR$Gasolina, df_gasolina_tidy_semBR$regiao, mean),
        col = "orange", horiz = T, main = "Média das vendas de Gasolina por estado 2000 - 2020")

#série original Etanol
for (i in unique(df_etanol_tidy$regiao)) {
  print(ggplot(df_etanol_tidy %>% filter(regiao == i), aes(Data, Etanol)) + 
          geom_line() + ggtitle(paste("Evolução Vendas Etanol - Região", i, sep = " ")) + theme(plot.title = element_text(hjust = 0.5)))
}

#Por região: série original vs ajuste sazonal (Gasolina)
for (i in unique(df_etanol_tidy$regiao)) {
  df_etanol_tidy_plot <- df_etanol_tidy %>% filter(regiao == i)
  plot(df_etanol_tidy_plot$Data, df_etanol_tidy_plot$Etanol, main = paste("Evolução Vendas Etanol - Região", i, sep = " "))
  
  regiao_br <- ts(df_etanol_tidy_plot$Etanol, start = c(2000, 1) , frequency = 12) 
  dessaz_br <- seas(regiao_br, x11 = "")
  plot(dessaz_br, main = paste("Evolução Vendas Etanol com Ajuste Sazonal - Região", i, sep = " "))
  grid(nx = 23)
  legend("topleft", legend = c("Original", "Ajustada"), col = c(1, 2), lwd = c(1, 2), 
         lty = 1, bty = "n", cex = 0.6)
}

#Consumo de Etanol por estado, média dos últimos 20 anos
df_etanol_tidy_semBR <- df_etanol_tidy %>% filter(!regiao == "br")
bp_Etanol <- barplot(tapply(df_etanol_tidy_semBR$Etanol, df_etanol_tidy_semBR$regiao, mean),
        col = "orange", horiz = T, main = "Média das vendas de Etanol por estado 2000 - 2020")

#Analisando o tamanho total do mercado# 
df_consolidado <- full_join(x = df_diesel_tidy, y = df_gasolina_tidy)
df_consolidado <- full_join(x = df_consolidado, y = df_etanol_tidy)
df_consolidado_tidy_BR <- df_consolidado %>% filter(regiao == "br")
df_tamanho_do_mercado <- df_consolidado_tidy_BR %>% group_by(Data) %>% summarise(Combustivel_Agregados=sum(Diesel, Gasolina, Etanol))

ggplot(df_tamanho_do_mercado %>% filter(Data >= "2016-01-01"), aes(Data, Combustivel_Agregados)) + 
  geom_line() + geom_smooth(method = lm) + ggtitle("Evolução Vendas de Combustíveis Total") + theme(plot.title = element_text(hjust = 0.5))

mean(df_tamanho_do_mercado$Combustivel_Agregados)
