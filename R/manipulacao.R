
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

gg_record(dir = "tidytuesday-temp", device = "png", width = 10, height = 8, units = "in", dpi = 320)

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





# Visualiza os primeiros registros da base unificada
head(BaseDados_Unica)

df_gini <- BaseDados_Unica |> janitor::clean_names() |> 
  rename( "2026" = ano_2026,  "2023"= mai_23 ) |>
  pivot_longer(`2023`:`2026`, names_to = "Ano", values_to = "remun") |>
  filter(qtd > 0) 

glimpse(df_gini)


# Expandir a base de dados com base na quantidade de observações
df_expandido <- df_gini %>%
  uncount(qtd)

# Verificando o resultado
head(df_expandido)


# Calculo do indice de gini por ano
 df_gini_subgrupo <- df_expandido |> 
  filter(Ano == "2023") |> 
  group_by(subgrupo) |>  
  mutate(
    total_remun = sum(remun, na.rm = TRUE),
    gini_index = ifelse(total_remun > 0, reldist::gini(remun), NA)
  ) |> 
  ungroup()
 
 df_gini_subgrupo <- df_gini_subgrupo %>% arrange(desc(gini_index))
 df_gini_subgrupo$subgrupo <- factor(df_gini_subgrupo$subgrupo, levels = unique(df_gini_subgrupo$subgrupo))
 
 
 ano <- unique(df_gini_subgrupo$Ano)
 mean_remun <- mean(df_gini_subgrupo$remun, na.rm = TRUE)
 median_remun <- median(df_gini_subgrupo$remun, na.rm = TRUE)

 

 
 bg_color <- "grey97"
 font_family <- "Fira Sans"
 
 
 # Formatar {mean_remun} para moeda brasileira R$ e {median_remun} para moeda brasileira R$ com o pacate scales formato brasil
 
 library(scales)
 
 # Criando um formatador para moeda brasileira
 format_brl <- label_number(big.mark = ".", decimal.mark = ",", prefix = "R$ ", , accuracy = 0.01)
 
 plot_subtitle <- glue("A remuneração média é de {format_brl(mean_remun)} e a mediana é de {format_brl(median_remun)}.
                       <br>")
 
 
 # Gráfico com o boxplot e outras informações
 
 p <- df_gini_subgrupo%>% 
   ggplot(aes(subgrupo, remun)) +
 stat_halfeye(fill_type = "segments", alpha = 0.3, scale = 6) +
   geom_boxplot(alpha = 0.1, outlier.shape = NA, colour = "grey" ) +
  stat_interval(  ) +
   stat_summary(geom = "point", fun = median) +
   annotate("text", x =11, y = 0, label = "(\U00F8 Índice de Gini)",
            family = "Fira Sans", size = 3, hjust = 0.5) +
   stat_summary(
     aes(y = gini_index),
     geom = "text",
     fun.data = function(x) {
       data.frame(
         y = 0,
         label = sprintf("(%s)", scales::number(mean(ifelse(x > 0, x, NA), na.rm = TRUE), accuracy = 0.010)))},
     family = font_family, size = 2.5
   ) +
   geom_hline(yintercept = median_remun, col = "grey30", lty = "dashed") +
   annotate("text", x = 11, y = median_remun + 50, label = "Remuneração Mediana Geral",
            family = "Fira Sans", size = 3, hjust = 0)  +
   scale_x_discrete(labels = toupper, expand = c(0, 1)) +
   scale_y_continuous(breaks = seq(2500, 40000, 2500)) +
   scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
   coord_flip(ylim = c(0, 40000), clip = "off") +
   guides(col = "none") +
   labs(
     title = toupper(glue("Índice de Gini por subgrupo de servidores públicos no ano de {ano}.")),
     subtitle = plot_subtitle,
     caption = "Fonte: SIAPE",
     x = NULL,
     y = "Remuneração (R$)"
   ) +
   #theme_minimal(base_family = font_family) +
   theme(
     plot.background = element_rect(color = NA, fill = bg_color),
     panel.grid = element_blank(),
     panel.grid.major.x = element_line(linewidth = 0.1, color = "grey75"),
     plot.title = element_text(family = "Fira Sans SemiBold"),
     plot.title.position = "plot",
     plot.subtitle = element_textbox_simple(
       margin = margin(t = 4, b = 4), size = 8),
     plot.caption = element_textbox_simple(
       margin = margin(t = 12), size = 7
     ),
     plot.caption.position = "plot",
     axis.text.y = element_text(hjust = 0, margin = margin(r = 1.5), family = "Fira Sans SemiBold"),
     plot.margin = margin(4, 14, 4, 4),
    axis.text.x = element_text(angle = 45, hjust = 1)
   )
 


# p

# create the dataframe for the legend (inside plot)
df_for_legend <- rent_title_words %>% 
  filter(word == "beautiful")

p_legend <- df_for_legend %>% 
  ggplot(aes(word, price)) +
  stat_halfeye(fill_type = "segments", alpha = 0.3) +
  stat_interval() +
  stat_summary(geom = "point", fun = median) +
  annotate(
    "richtext",
    x = c(0.8, 0.8, 0.8, 1.4, 1.8),
    y = c(1000, 5000, 3000, 2400, 4000),
    label = c("50 % dos servidores<br>recebem abaixo desse valor", "95 % dos servidores", 
              "80 % dos servidores", "Mediana", "Distribuição<br>da remuneração"),
    fill = NA, label.size = 0, family = font_family, size = 2, vjust = 1,
  ) +
  geom_curve(
    data = data.frame(
      x = c(0.9, 0.80, 0.80, 1.225, 1.8),
      xend = c(0.95, 0.95, 0.95, 1.075, 1.8),
      y = c(1000, 5000, 3000, 2300, 3800),
      yend = c(1800, 5000, 3000, 2100, 2500)),
    aes(x = x, xend = xend, y = y, yend = yend),
    stat = "unique", curvature = 0.2, size = 0.2, color = "grey12",
    arrow = arrow(angle = 20, length = unit(1, "mm"))
  ) +
  scale_color_manual(values = MetBrewer::met.brewer("VanGogh3")) +
  coord_flip(xlim = c(0.75, 1.3), ylim = c(0, 6000), expand = TRUE) +
  guides(color = "none") +
  labs(title = "Legenda") +
  theme_void(base_family = font_family) +
  theme(plot.title = element_text(family = "Fira Sans SemiBold", size = 9,
                                  hjust = 0.075),
        plot.background = element_rect(color = "grey30", size = 0.1, fill = bg_color))

# p_legend
# Insert the custom legend into the plot
p + inset_element(p_legend, l = 0.6, r = 1.0,  t = 0.86, b = 0.6, clip = FALSE)


