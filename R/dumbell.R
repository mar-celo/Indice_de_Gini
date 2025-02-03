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
 



library(dplyr)

df_long <- df_long_i %>% select(-c(orgao_area))%>% 
  pivot_longer(`2023`: `2026`, names_to = "name", values_to = "value") 

# set a custom nudge value
nudge_value=.2

p_main=
  df_long %>% 
  
  # the following 3 lines of code are the same
  ggplot(aes(x=value,y=labels)) +
  geom_line(aes(group=labels), color="#E7E7E7", linewidth=2.5) +
  geom_point(aes(color=name), size=3) +
  
  # but we want geom_text for the data callouts and the legend
  
  # data callout
  geom_text(aes(label=value, color=name),
            size=2.25,
            nudge_x=if_else(
              df_long$value==df_long$max, 
              0.02,  # Valor menor para aproximar o rótulo do ponto
              -0.02), 
            hjust=if_else(
              df_long$value==df_long$max, 
              0, 
              1)) + 

  
  # legend
  geom_text(aes(label=name, color=name),
            data=. %>% filter(gap==max(gap)),
            nudge_y =.8,
            fontface="bold",
            size=3.25)+
  
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.y = element_text(color="black"),
        axis.text.x = element_text(color="#989898"),
        axis.title = element_blank(),
        panel.grid = element_blank()
  ) +
  labs(x= NULL,
       y=NULL, 
       title = "Índice de Gini por subgrupo de servidores públicos no ano de 2023 e 2026") +
  scale_color_manual(values=c( "#BF2F24", "#436685")) +
  
 
  coord_cartesian(ylim=c(0, 50)) 
  
  

p_main


library(ggplot2)
library(dplyr)
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

p_main
