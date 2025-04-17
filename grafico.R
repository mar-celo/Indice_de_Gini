---
title: "Índice de Gini"
subtitle: "Remuneração dos Servidores Públicos Federais"
author:
  - name: "CGINF - Coordenação Geral de Informações Gerenciais "
    affiliation: "DIGID - Diretoria de Governança e Inteligência de dados"
date: "`r Sys.Date()`"
abstract: |
  Este tutorial aborda o cálculo e a visualização do Índice de Gini, uma medida estatística amplamente utilizada para avaliar a desigualdade na distribuição de renda em populações ou regiões.
categories: [Carreiras, Remuneração, Funcionalismo Público, Índice de Gini, Desigualdade Salarial, Servidores Públicos Federais, SIAPE]  
format:
  html:
    toc: true
    toc-depth: 2
    number-sections: true
    number-depth: 1
    theme: cosmo  # Ou journal para um estilo mais formal
    highlight-style: github
    css: styles.css  # Arquivo customizado para ajustes
    code_folding: show
    code-fold: true
    toc_float:
      collapsed: false
---

# Carregamento da base, limpeza e manipulação dos dados

```{r, warning = FALSE, message = FALSE,  fig.width = 10, fig.height = 10, fig.align = "center"}
# carregar a base de dados em excel e compilar todas as abas em um único arquivo


# Carregando as bibliotecas necessárias
library(readxl)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(ggdist)
library(glue)
library(patchwork)
library(camcorder)
library(gt)
library(ggalt)
library(ggstatsplot)
library(plotly)

#gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

# Define o caminho do arquivo
file_path <- "data/BaseDados_EstudoDesigualdades.xlsx"

# Obtém os nomes de todas as abas no Excel
sheet_names <- excel_sheets(file_path)

# excluir abas que não serão utilizadas 
# aba FORA 

sheet_names <- sheet_names[!sheet_names %in% c("FORA")]

# Lê todas as abas e combina em uma única base usando purrr::map
BaseDados_Unica <- sheet_names %>%
  map_df(~ read_excel(file_path, sheet = .x) %>%
           mutate(sheet_name = .x)) # Adiciona o nome da aba como coluna (opcional)

# preencher valores NA na coluna escolaridade_cargo com "Não Informado"



# filtrar apenas campos que possuem servidores

df_gini <- BaseDados_Unica |> janitor::clean_names() |> 
  replace_na(list(escolaridade_cargo = "Sem informação")) |>
  rename( "2026" = ano_2026,  "2023"= mai_23 ) |>
  pivot_longer(`2023`:`2026`, names_to = "Ano", values_to = "remun") |>
  filter(qtd > 0) 

df_gini %>%
    DT::datatable(rownames = FALSE, filter = "top",
                  options = list(pageLength = 5, scrollX = TRUE)) %>%
    DT::formatStyle(
        columns = names(df_gini), 
        target = "row",
        lineHeight = "12px",  # Reduz a altura das linhas
        fontSize = "12px"  # Reduz o tamanho da fonte
    )



```

# Gráfico anterior de dumbbell

```{r, warning = FALSE, message = FALSE,  fig.width = 10, fig.height = 10, fig.align = "center"}

# Expandir a base de dados com base na quantidade de observações
df_expandido <- df_gini %>%
  uncount(qtd)

# Calculo do indice de gini por ano
df_gini_orgao <- df_expandido |> 
  #filter(Ano == "2023") |> 
  group_by(orgao_area, Ano) |>  
  summarise(
    total_remun = sum(remun, na.rm = TRUE),
    gini_index = ifelse(total_remun > 0, reldist::gini(remun), NA)
  ) |> 
  select(-total_remun) |>
  ungroup() |> 
  mutate(gini_index = round(gini_index, 2))
  


# pivotar wide 

df_gini_orgao_wide <- df_gini_orgao |> 
  pivot_wider(names_from = Ano, values_from = gini_index) |> 
  mutate(gap = `2026` - `2023`) |> 
  group_by(orgao_area) |>
  mutate(
    max=max(`2023`, `2026`)) |>
  ungroup() 
 

# Reordenando a variável orgao_area com base no gap em ordem decrescente

df_long_i <- df_gini_orgao_wide |> 
  arrange(gap) |> 
  mutate(labels=forcats::fct_reorder(orgao_area , gap)) |>
  ungroup()
 

df_long <- df_long_i %>% select(-c(orgao_area))%>% 
  pivot_longer(`2023`: `2026`, names_to = "name", values_to = "value") 

library(forcats)
library(scales)

nudge_value = 0.2

p_main <- df_long %>%
  ggplot(aes(x = value, y = labels)) +
  
  # Linhas entre os pontos
  geom_line(aes(group = labels), color = "gray70", linewidth = 2) +
  
  # Pontos com cores personalizadas
  geom_point(aes(color = name), size = 3) +
  
  # Rótulos para os valores
  geom_text(aes(label = label_number(accuracy = 0.001)(value), color = name),
            size = 3,
            fontface = "bold",
            nudge_x = if_else(df_long$value == df_long$max, 0.02, -0.02),
            hjust = if_else(df_long$value == df_long$max, 0, 1)) +
  
  # Adicionando legenda personalizada ao lado direito do gráfico
  geom_text(aes(label = name, color = name),
            data = df_long %>% filter(gap == max(gap)),
            nudge_y = 1,
            fontface = "bold",
            size = 3.5) +
  
  # Estilizando tema
  theme_light(base_size = 12) + 
  theme(legend.position = "none",
        axis.text.y = element_text(color = "black", size = 8),
        axis.text.x = element_text(color = "#666666", size = 8),
        axis.title = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
        panel.grid.minor = element_blank()) +
  
  # Título e legendas
  labs(
    title = "Índice de Gini por Órgão (2023 e 2026)",
    caption = "Fonte: SIAPE",
  ) +
  
  # Ajustando cores
  scale_color_manual(values = c("#BF2F24", "#436685")) +
  
  # Ajustando limites do gráfico
  coord_cartesian(ylim = c(0, 50)) 

#knitr::include_graphics("tidytuesday-temp/2025_02_03_14_45_21.663065.png")

```

# Versão mais limpa e genérica do gráfico dumbbell

```{r}
library(tidyverse)
library(scales)

plot_dumbbell <- function(df, titulo = "Título do Gráfico", org_ordenar = TRUE) {
  df_plot <- df
  
  # Se quiser ordenar os labels pelo valor máximo
  if (org_ordenar) {
    df_plot <- df_plot %>%
      mutate(labels = fct_reorder(labels, max))
  }

  ggplot(df_plot, aes(x = value, y = labels)) +
    geom_line(aes(group = labels), color = "gray70", linewidth = 2) +
    geom_point(aes(color = name), size = 3) +
    geom_text(aes(label = label_number(accuracy = 0.001)(value), color = name),
              size = 3,
              fontface = "bold",
              nudge_x = if_else(value == max, 0.02, -0.02),
              hjust = if_else(value == max, 0, 1)) +
    theme_light(base_size = 12) + 
    theme(
      legend.position = "none",
      axis.text.y = element_text(color = "black", size = 8),
      axis.text.x = element_text(color = "#666666", size = 8),
      axis.title = element_blank(),
      panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
      panel.grid.minor = element_blank()
    ) +
    labs(
      title = titulo,
      caption = "Fonte: SIAPE"
    ) +
    scale_color_manual(values = c("2023" = "#BF2F24", "2026" = "#436685"))
}

```

```{r}
plot_dumbbell(df_long, titulo = "Índice de Gini por Órgão (2023 e 2026)")

```

```{r}
library(ggplot2)
library(dplyr)
library(forcats)
library(rlang)  # necessário para usar `{{ }}`

plot_dumbbell <- function(data, nome_col, valor_col, label_col, gap_col) {
  data <- data %>%
    mutate({{ nome_col }} := fct_reorder({{ nome_col }}, {{ gap_col }}))
  
  ggplot(data, aes(x = {{ valor_col }}, y = {{ nome_col }}, group = {{ nome_col }})) +
    geom_line(aes(color = {{ gap_col }} > 0), linewidth = 1.5, show.legend = FALSE) +
    geom_point(aes(color = {{ label_col }}), size = 3) +
    scale_color_manual(values = c("2023" = "#1f77b4", "2026" = "#ff7f0e", "TRUE" = "green", "FALSE" = "red")) +
    theme_minimal() +
    labs(
      title = "Comparativo de Valores por Categoria: 2023 vs 2026",
      x = "Valor",
      y = NULL,
      caption = "Fonte: Dados simulados"
    ) +
    theme(
      plot.title = element_text(face = "bold", size = 14),
      axis.text.y = element_text(size = 10)
    )
}


```

```{r}
plot_dumbbell(df_long, name, value, labels, gap)

```

```{r}

library(tidyverse)

library(scales)

```

```{r}
# Reordena os fatores para organizar no gráfico
df_long <- df_long %>%
  mutate(labels = fct_reorder(labels, max))  # organiza do menor pro maior valor máximo

# Junta os dados de cada grupo (2023 e 2026) lado a lado
df_plot <- df_long %>%
  select(labels, name, value) %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(
    destaque = if_else(`2026` > 0.5, "Acima de 50%", "Abaixo de 50%"),
    gap_label = paste0("+", round(`2026` - `2023`, 2) * 100, "pp")
  )

# Cores (você pode ajustar)
cores <- c("Acima de 50%" = "#B22222", "Abaixo de 50%" = "#4169E1")

# Gráfico tipo dumbbell
ggplot(df_plot, aes(y = labels)) +
  geom_dumbbell(aes(x = `2023`, xend = `2026`, color = destaque),
                size = 2, dot_guide = TRUE, dot_guide_size = 0.25,
                colour_x = "grey60", colour_xend = "grey60") +
  geom_text(aes(x = `2026`, label = gap_label),
            hjust = -0.2, size = 3, color = "black") +
  scale_color_manual(values = cores, name = "2026") +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "gray50") +
  scale_x_continuous(labels = percent_format(accuracy = 1)) +
  labs(
    title = "Comparação entre 2023 e 2026",
    subtitle = "Mudança percentual por órgão",
    x = NULL,
    y = NULL,
    caption = "Fonte: Vozes dos Servidores"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "top",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

```

```{r}
# Expandir a base de dados com base na quantidade de observações
df_expandido <- df_gini %>%
  uncount(qtd)

# Calculo do indice de gini por ano
df_gini_orgao <- df_expandido |> 
  #filter(Ano == "2023") |> 
  group_by(orgao_area, Ano) |>  
  summarise(
    total_remun = sum(remun, na.rm = TRUE),
    gini_index = ifelse(total_remun > 0, reldist::gini(remun), NA)
  ) |> 
  select(-total_remun) |>
  ungroup() |> 
  mutate(gini_index = round(gini_index, 2))
  


# pivotar wide 

df_gini_orgao_wide <- df_gini_orgao |> 
  pivot_wider(names_from = Ano, values_from = gini_index) |> 
  mutate(gap = `2023` - `2026`) |> 
  group_by(orgao_area) |>
  mutate(
    max=max(`2023`, `2026`)) |>
  ungroup() 
 

# Reordenando a variável orgao_area com base no gap em ordem decrescente

df_long_i <- df_gini_orgao_wide |> 
  arrange(desc(abs(gap))) |> 
 
  mutate(labels=forcats::fct_reorder(orgao_area , abs(gap))) |>
  ungroup()
 

df_long <- df_long_i %>% select(-c(orgao_area))%>% 
  pivot_longer(`2023`: `2026`, names_to = "name", values_to = "value") 

library(forcats)
library(scales)

nudge_value = 0.2

p_main <- df_long %>%
  ggplot(aes(x = value, y = labels)) +
  
  # Linhas entre os pontos
  geom_line(aes(group = labels), color = "gray70", linewidth = 2) +
  
  # Pontos com cores personalizadas
  geom_point(aes(color = name), size = 3) +
  
  # Rótulos para os valores
  geom_text(aes(label = label_number(accuracy = 0.001)(value), color = name),
            size = 3,
            fontface = "bold",
            nudge_x = if_else(df_long$value == df_long$max, 0.02, -0.02),
            hjust = if_else(df_long$value == df_long$max, 0, 1)) +
  
  # Adicionando legenda personalizada ao lado direito do gráfico
  geom_text(aes(label = name, color = name),
            data = df_long %>% filter(gap == max(gap)),
            nudge_y = 1,
            fontface = "bold",
            size = 3.5) +
  
  # Estilizando tema
  theme_light(base_size = 12) + 
  theme(legend.position = "none",
        axis.text.y = element_text(color = "black", size = 8),
        axis.text.x = element_text(color = "#666666", size = 8),
        axis.title = element_blank(),
        panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
        panel.grid.minor = element_blank()) +
  
  # Título e legendas
  labs(
    title = "Índice de Gini por Órgão (2023 e 2026)",
    caption = "Fonte: SIAPE",
  ) +
  
  # Ajustando cores
  scale_color_manual(values = c("#BF2F24", "#436685")) +
  
  # Ajustando limites do gráfico
  coord_cartesian(ylim = c(0, 50)) 
```

```{r}
library(tidyr)
df_wide <- df_long |> 
  pivot_wider(names_from = name, values_from = value)

```

```{r}
p_main <- df_wide %>%
  ggplot(aes(y = fct_reorder(labels, `2026`))) +
  
  # Segmento horizontal entre os dois anos
  geom_segment(aes(x = `2023`, xend = `2026`, yend = labels),
               color = "gray70", linewidth = 3) +

  # Pontos
  geom_point(aes(x = `2023`), color = "#BF2F24", size = 3) +
  geom_point(aes(x = `2026`), color = "#436685", size = 3) +

  # Textos com os valores
  geom_text(aes(x = `2023`, label = label_number(accuracy = 0.001)(`2023`)),
            hjust = 1.2, color = "#BF2F24", size = 3, fontface = "bold") +
  geom_text(aes(x = `2026`, label = label_number(accuracy = 0.001)(`2026`)),
            hjust = -0.2, color = "#436685", size = 3, fontface = "bold") +
    # Anotação do gap à esquerda
  geom_text(aes(x = min(`2023`, `2026`) - 0.03, 
                label = paste0("+", label_number(accuracy = 0.001)(gap))),
            size = 3, fontface = "bold", color = "gray30") + 


  # Título e tema
  labs(
    title = "Índice de Gini por Órgão (2023 e 2026)",
    caption = "Fonte: SIAPE"
  ) +
  theme_light(base_size = 12) +
  theme(
    axis.title = element_blank(),
    axis.text.y = element_text(size = 8, color = "black"),
    axis.text.x = element_text(size = 8, color = "gray40"),
    legend.position = "none",
    panel.grid.major.y = element_line(color = "gray90", linetype = "dashed"),
    panel.grid.minor = element_blank()
  )

```

```{r}

```
