
library(tidyverse)
library(abjData)
library(basesCursoR)
library(haven)

dados <- haven::read_sav("04849.zip")

dados <- dados |>
  mutate(
  across(
    .cols = -DATA_ENTREVISTA,
    .fns = as_factor
  )
)

View(dados)

df <- dados |>
  select(CLASSE_SOCIAL, ESCOLARIDADE, IDADE, SEXO, RACA, RELIGIÃO, P2_1)

df <- df |>
  #distinct(P2_1) |>
  mutate(
    pergunta = case_when(
      P2_1 == "Não sofri/ não vi alguém sofrer alguma situação de preconceito" ~ "Não viu/Não sofreu",
      P2_1 == "Não respondeu" ~ "Não respondeu",
      .default = "Viu/Sofreu"
    )
  )

df |>
  count(pergunta, name = "total") |>
  arrange(total)

df <- df |>
  filter(pergunta != "Não respondeu")

df |>
  count(
    SEXO, RACA, pergunta
  ) |>
  group_by(SEXO, RACA) |>
  mutate(percentual = n / sum(n) * 100)


df |>
  count(
    SEXO, RELIGIÃO, pergunta
  ) |>
  group_by(SEXO, RELIGIÃO) |>
  mutate(percentual = n / sum(n) * 100)

df <- as_tibble(titanic::titanic_train)

df <- df |>
  mutate(
    across(
      where(is.character),
      ~ na_if(., "")
    )
  )

df

help("")




pnud <- readxl::read_xlsx("base_de_dados.xlsx")

glimpse(pnud)

pnud |>
  distinct(ANO, AGREGACAO) |>
  arrange(desc(ANO))

pnud |>
  filter(AGREGACAO == "RM_RIDE") |>
  arrange(desc(ANO)) |>
  ggplot() +
  aes(x = GINI, y = IDHM_E) +
  geom_jitter() +
  geom_smooth(method = "lm") +
  facet_wrap(~REGIAO) +
  theme_bw()

pnud |>
  filter(AGREGACAO == "RM_RIDE", str_detect(NOME, "\\(SP\\)")) |>
  distinct(NOME)



dados <- abjData::pnud_muni

tab <- abjData::muni

dados <- left_join(
  x = dados |> mutate(codmun7 = as.character(codmun7)),
  y = tab,
  by = c('codmun7' = 'muni_id')
)

glimpse(dados)

dados |>
  ggplot() +
  aes(x = gini, group = ano) +
  # geom_histogram(colour = 'white') +
  geom_density(fill = ano) +
  theme_bw() +
  scale_x_continuous(breaks = seq(0,1, by = 0.1))

dados |>
  ggplot() +
  aes(x = gini, group = ano, fill = factor(ano)) +
  geom_density(alpha = 0.5) +
  scale_x_continuous(breaks = seq(0, 1, by = 0.1)) +
  theme_bw()

pnud |>
  distinct()


tse <- read_csv("resultado_eleicoes_sp.csv")


ssp <- basesCursoR::pegar_base("ssp")

ssp |>
  group_by(ano) |>
  summarise(
    total_latrocinio = sum(latrocinio)
  )

left_join(
  x = tse,
  y = ssp,
  by = c("id_municipio_nome" = "municipio_nome", "ano")
)

ssp |> distinct(municipio_nome)
tse |> distinct(id_municipio_nome)
