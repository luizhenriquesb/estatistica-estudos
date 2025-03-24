

# 1. Pacotes --------------------------------------------------------------

library(tidyverse)
library(abjData)

# 2. Dados ----------------------------------------------------------------

df <- abjData::pnud_muni

df <- df |> 
  filter(ano == 2010) |> 
  mutate(codmun7 = as.character(codmun7))

muni <- abjData::muni
muni <- muni |> 
  filter(existia_2010 == 1)

dados <- left_join(
  x = df,
  y = muni,
  by = c("codmun7" = "muni_id")
)

# 3. Análises -------------------------------------------------------------

dados |> 
  select(municipio, idhm) |> 
  view()

quantile(dados$rdpc, probs = seq(0, 1, by = 0.01))

dados |> 
  filter(municipio == "SÃO PAULO") |> 
  select(idhm)

library



