
library(tidyverse)
library(basesCursoR)
library(abjData)

df <- abjData::pnud_muni

x <- df %>% 
  filter(ano == 2010) |> 
  select(municipio, rdpc)

par(mfrow = c(1,2))

{

hist(df$rdpc, 
     probability = TRUE,
     main = "Distribuição de Renda Per Capita (2010)", 
     xlab = "", 
     ylab = "Densidade", 
     col = "lightblue", 
     border = "black", 
     #breaks = 10,
     #xlim = c(7.6, 9.2),
     #ylim = c(0, 230)
     )
  
normal <- rnorm(1000)
  
  hist(normal, 
     probability = TRUE,
     main = "Distribuição Normal", 
     xlab = "", 
     ylab = "Densidade", 
     col = "lightblue", 
     border = "black", 
     #breaks = 10,
     #xlim = c(7.6, 9.2),
     #ylim = c(0, 230)
     )
}

media_verdadeira <- x %>% summarise(media = mean(rdpc)) %>% pull()

glue::glue(
  "Média verdadeira: {media_verdadeira}"
  )

par(mfrow = c(1,2))

vetor_medias <- c()

for (i in 1:1000) {
  
  Sys.sleep(0.05)
  
  media <- sample(x$rdpc, size = 100)
  
  vetor_medias <- append(vetor_medias, mean(media))
  
  # densidade <- density(vetor_medias)
  
  # Plotar o histograma das médias
  hist(vetor_medias, 
       probability = TRUE,
       main = "Distribuição das Médias das Amostras", 
       xlab = "Média da Amostra", 
       ylab = "Densidade", 
       col = "lightblue", 
       border = "black", 
       breaks = 15,
       #xlim = c(7.6, 9.2),
       ylim = c(0, 0.4)
  )
  
  hist(normal,
       probability = TRUE,
       main = "Distribuição Normal",
       xlab = "",
       ylab = "Densidade",
       col = "lightblue",
       border = "black",
       #breaks = 10,
       #xlim = c(7.6, 9.2),
       #ylim = c(0, 230)
  )
  
  if (length(vetor_medias) > 1) {
    lines(density(vetor_medias), col = "red", lwd = 2)
  }
  
  abline(v = media_verdadeira, col = "royalblue", lwd = 2, lty = 2)
  
  media_amostras <- mean(vetor_medias)
  abline(v = media_amostras, col = "black", lwd = 2, lty = 2)
  
  # text(
  #   media_verdadeira, 
  #   max(table(vetor_medias)), 
  #   labels = "Média Verdadeira", 
  #   col = "royalblue", 
  #   #pos = 1, 
  #   cex = 0.8
  # )
  # 
  # text(
  #   media_amostras,
  #   max(table(vetor_medias)),
  #   labels = "Média das Amostras", 
  #   col = "black", 
  #   #pos = 1, 
  #   cex = 0.8
  # )
  
  # text(glue::glue("Amostra: {i}"),labels = "Amostra")
  
  # Garantir que o gráfico seja exibido após cada iteração
  dev.flush()
  
}



