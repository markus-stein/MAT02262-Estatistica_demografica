## Censo 2010 no Brasil

## carregar pacotes - instalar se necessario
# install.packages(c("censobr", "geobr", "dplyr", "ggplot2", "sf", "tidyr"))
library(censobr)
library(geobr)
library(dplyr)
library(ggplot2)
library(sf)
library(tidyr)

## microdados da populacao censo 2010
pop <- read_population(year = 2010)   # Pode demorar alguns minutos

## idade e sexo para piramide etaria
piramide <- pop %>%
  select(idade = V6036, sexo = V0601) %>%
  filter(!is.na(idade), idade <= 100) %>%
  mutate(
    faixa = cut(idade,
                breaks = seq(0, 100, 5),
                right = FALSE),
    sexo = ifelse(sexo == 1, "Homens", "Mulheres")
  ) %>%
  group_by(faixa, sexo) %>%
  summarise(pop = n(), .groups = "drop") %>%
  mutate(pop = ifelse(sexo == "Homens", -pop, pop))

## piramide
ggplot(piramide, aes(x = faixa, y = pop, fill = sexo)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    title = "Pirâmide Etária — Brasil (Censo 2010)",
    x = "Faixa etária",
    y = "População",
    fill = ""
  ) +
  theme_minimal()

## mapas por municipio
## malha municipal
mun <- read_municipality(year = 2010)

## agrupa dados por municipio
pop_mun <- pop %>%
  group_by(code_muni) %>%
  summarise(pop = n(), .groups = "drop")

## junta malha e pop
mapa <- mun %>%
  left_join(pop_mun, by = c("code_muni" = "code_muni"))

## mapa
ggplot(mapa) +
  geom_sf(aes(fill = pop), color = NA) +
  scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
  labs(
    title = "População por Município — Brasil (Censo 2010)",
    fill = "População"
  ) +
  theme_minimal()

####
install.packages(c("sidrar","geobr","dplyr","ggplot2","stringr"))

library(sidrar)
library(geobr)
library(dplyr)
library(ggplot2)
library(stringr)

##censo 2010, pop municipio 
## rabela sidra

pop2010 <- get_sidra(
  api = "/t/1378/n6/all/v/93/p/2010"
) %>%
  select(
    cod_mun = `Município (Código)`,
    municipio = Município,
    pop2010 = Valor
  )

## censo 2022
pop2022 <- get_sidra(
  api = "/t/4709/n6/all/v/93/p/2022"
) %>%
  select(
    cod_mun = `Município (Código)`,
    municipio = Município,
    pop2022 = Valor
  )

## juntar bases e calcular crescimento
pop_total <- pop2010 %>%
  inner_join(pop2022, by = "cod_mun") %>%
  mutate(
    crescimento_abs = pop2022 - pop2010,
    crescimento_pct = (pop2022/pop2010 - 1)*100
  )

## crescimento populacional do Brasil (total)
brasil <- pop_total %>%
  summarise(
    pop2010 = sum(pop2010, na.rm=TRUE),
    pop2022 = sum(pop2022, na.rm=TRUE)
  ) %>%
  tidyr::pivot_longer(everything(),
                      names_to = "ano",
                      values_to = "pop")
ggplot(brasil, aes(x = ano, y = pop, group = 1)) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(labels = scales::comma) +
  labs(
    title = "Crescimento Populacional — Brasil",
    subtitle = "Censos Demográficos 2010 e 2022",
    x = "Ano",
    y = "População"
  ) +
  
  ## mapa de crescimento municipal
  mun <- read_municipality(year = 2020)

mapa <- mun %>%
  left_join(pop_total, by = c("code_muni" = "cod_mun"))
ggplot(mapa) +
  geom_sf(aes(fill = crescimento_pct), color = NA) +
  scale_fill_viridis_c(option="plasma") +
  labs(
    title = "Crescimento Populacional Municipal (%)",
    subtitle = "Brasil — 2010 a 2022",
    fill = "% crescimento"
  ) +
  theme_minimal()