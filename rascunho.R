---
title: "rascunho"
---


```{r}

criar_grafico_percentis <- function(df, coluna_facet = "subgrupo", p_numerador = 0.8, p_denominador = 0.2) {
  
  library(ggplot2)
  library(dplyr)
  library(tidyr)
  library(scales)
  
  df_subgrupo <- df |> 
    group_by(Ano, !!sym(coluna_facet)) |> 
    summarise(
      percentil_numerador = quantile(remun, p_numerador, na.rm = TRUE),
      percentil_denominador = quantile(remun, p_denominador, na.rm = TRUE),
      .groups = "drop"
    ) |> 
    mutate(
      razao = percentil_numerador / percentil_denominador
    )
  
  df_long <- df_subgrupo %>%
    pivot_longer(cols = c(percentil_numerador, percentil_denominador), 
                 names_to = "Percentil", 
                 values_to = "Remuneracao")
  
  ggplot(df_long, aes(x = Ano, y = Remuneracao, fill = Percentil)) +
    geom_col(position = "dodge") +  
    geom_text(aes(label = scales::label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")(Remuneracao)), 
              position = position_dodge(width = 0.9), vjust = -0.5, size = 3) +  
    scale_fill_manual(values = c("percentil_numerador" = "blue", "percentil_denominador" = "red"),
                      labels = c("percentil_numerador", "percentil_denominador")) +
    geom_line(data = df_subgrupo, aes(x = Ano, y = razao * 5000, group = 1), 
              color = "black", linetype = "dashed", size = 1, inherit.aes = FALSE) +  
    geom_point(data = df_subgrupo, aes(x = Ano, y = razao * 5000), 
               color = "black", size = 3, inherit.aes = FALSE) +  
    geom_text(data = df_subgrupo, aes(x = Ano, y = razao * 5000, label = round(razao, 2)), 
              color = "black", size = 4, vjust = -0.5, inherit.aes = FALSE) +  
    scale_y_continuous(
      name = "Remuneração (R$)",
      labels = label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ","),  
      sec.axis = sec_axis(~ . / 5000, name = "Razão P80/P20")  
    ) +
    labs(
      title = "Distribuição das Remunerações nos Percentis",
      x = "Ano",
      fill = "Percentis"
    ) +
    facet_wrap(as.formula(paste("~", coluna_facet)), scales = "free_y", ncol = 2) +  
    expand_limits(y = max(df_long$Remuneracao, na.rm = TRUE) * 1.1) +  
    theme_minimal()
}




```


```{r}
glimpse(df_expandido)
# Exemplo de chamada da função:
criar_grafico_percentis(df_expandido, coluna_facet = "escolaridade_cargo", p_numerador = 0.9, p_denominador = 0.1)
```


```{r}
criar_grafico_percentis(df_expandido, coluna_facet = "escolaridade_cargo", p_numerador = 0.9, p_denominador = 0.1)
```
```


```{r,echo=FALSE, warning = FALSE, message = FALSE}
library(dplyr)
library(DT)
library(scales)

df_subgrupo <- df_expandido |> 
   group_by(Ano, subgrupo) |>
   summarise(
     media_90 = mean(remun[remun >= quantile(remun, 0.9, na.rm = TRUE)], na.rm = TRUE),
     media_80 = mean(remun[remun >= quantile(remun, 0.8, na.rm = TRUE)], na.rm = TRUE),
     media_60 = mean(remun[remun >= quantile(remun, 0.6, na.rm = TRUE)], na.rm = TRUE),
     media_10 = mean(remun[remun <= quantile(remun, 0.1, na.rm = TRUE)], na.rm = TRUE),
     media_20 = mean(remun[remun <= quantile(remun, 0.2, na.rm = TRUE)], na.rm = TRUE)
   ) |> 
   mutate(
     razao_90_10 = media_90 / media_10,
     razao_80_20 = media_80 / media_20,
     razao_60_10 = media_60 / media_10
   ) |> 
   ungroup() |> 
   mutate(
     across(starts_with("media"), ~ label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")(.x)),
     across(starts_with("razao"), ~ format(round(.x, 2), nsmall = 2))
   )

# Exibir a tabela com DT::datatable()
datatable(df_subgrupo |> filter(Ano == '2023'), options = list(pageLength = 10, scrollX = TRUE))

```


```{r,echo=FALSE, warning = FALSE, message = FALSE}
# Exibir a tabela com DT::datatable()
datatable(df_subgrupo |> filter(Ano == '2026'), options = list(pageLength = 10, scrollX = TRUE))
```

```{r,echo=FALSE, warning = FALSE, message = FALSE,  fig.width = 10, fig.height = 20, fig.align = "center"}

library(ggplot2)
library(dplyr)
library(tidyr)

# Transformar os dados para formato longo
df_long <- df_subgrupo |> 
  pivot_longer(cols = starts_with("razao"), 
               names_to = "Razao", 
               values_to = "Valor") |> 
  mutate(Valor = as.numeric(Valor))  # Converter para numérico

# Criar o gráfico com facet_wrap por subgrupo
ggplot(df_long, aes(x = Ano, y = Valor, color = Razao, group = Razao)) +
  geom_line(size = 1) +  # Linhas representando as razões
  geom_point(size = 3) +  # Pontos para destacar os valores
  geom_text(aes(label = round(Valor, 2)), 
            vjust = -0.5, size = 3, show.legend = FALSE) +  # Rótulos com os valores
  scale_color_manual(values = c("razao_90_10" = "red", 
                                "razao_80_20" = "blue", 
                                "razao_60_10" = "green"),
                     labels = c("P90/P10", "P80/P20", "P60/P10")) + 
  labs(
    title = "Evolução das Razões das Remunerações por Subgrupo",
    x = "Ano",
    y = "Razão",
    color = "Razões"
  ) +
  facet_wrap(~subgrupo, scales = "free_y", ncol = 2) +  # Facet_wrap por subgrupo, 2 colunas
  theme_minimal()


```

# Escolaridade

```{r,echo=FALSE, warning = FALSE, message = FALSE}
library(dplyr)
library(DT)
library(scales)

df_subgrupo <- df_expandido |> 
   group_by(Ano, escolaridade_cargo) |>
   summarise(
     media_90 = mean(remun[remun >= quantile(remun, 0.9, na.rm = TRUE)], na.rm = TRUE),
     media_80 = mean(remun[remun >= quantile(remun, 0.8, na.rm = TRUE)], na.rm = TRUE),
     media_60 = mean(remun[remun >= quantile(remun, 0.6, na.rm = TRUE)], na.rm = TRUE),
     media_10 = mean(remun[remun <= quantile(remun, 0.1, na.rm = TRUE)], na.rm = TRUE),
     media_20 = mean(remun[remun <= quantile(remun, 0.2, na.rm = TRUE)], na.rm = TRUE)
   ) |> 
   mutate(
     razao_90_10 = media_90 / media_10,
     razao_80_20 = media_80 / media_20,
     razao_60_10 = media_60 / media_10
   ) |> 
   ungroup() |> 
   mutate(
     across(starts_with("media"), ~ label_dollar(prefix = "R$ ", big.mark = ".", decimal.mark = ",")(.x)),
     across(starts_with("razao"), ~ format(round(.x, 2), nsmall = 2))
   )

# Exibir a tabela com DT::datatable()
datatable(df_subgrupo |> filter(Ano == '2023'), options = list(pageLength = 10, scrollX = TRUE))

```


```{r,echo=FALSE, warning = FALSE, message = FALSE}
# Exibir a tabela com DT::datatable()
datatable(df_subgrupo |> filter(Ano == '2026'), options = list(pageLength = 10, scrollX = TRUE))
```

```{r,echo=FALSE, warning = FALSE, message = FALSE,  fig.width = 10, fig.height = 20, fig.align = "center"}

library(ggplot2)
library(dplyr)
library(tidyr)

# Transformar os dados para formato longo
df_long <- df_subgrupo |> 
  pivot_longer(cols = starts_with("razao"), 
               names_to = "Razao", 
               values_to = "Valor") |> 
  mutate(Valor = as.numeric(Valor))  # Converter para numérico

# Criar o gráfico com facet_wrap por subgrupo
ggplot(df_long, aes(x = Ano, y = Valor, color = Razao, group = Razao)) +
  geom_line(size = 1) +  # Linhas representando as razões
  geom_point(size = 3) +  # Pontos para destacar os valores
  geom_text(aes(label = round(Valor, 2)), 
            vjust = -0.5, size = 3, show.legend = FALSE) +  # Rótulos com os valores
  scale_color_manual(values = c("razao_90_10" = "red", 
                                "razao_80_20" = "blue", 
                                "razao_60_10" = "green"),
                     labels = c("P90/P10", "P80/P20", "P60/P10")) + 
  labs(
    title = "Evolução das Razões das Remunerações por Subgrupo",
    x = "Ano",
    y = "Razão",
    color = "Razões"
  ) +
  facet_wrap(~escolaridade_cargo, scales = "free_y", ncol = 2) +  # Facet_wrap por subgrupo, 2 colunas
  theme_minimal()


```
