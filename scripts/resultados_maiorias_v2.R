## Resultados - pesquisa de maiorias nas camaras de vereadores ----

library(tidyverse)

### Bancos de dados ----

dados_tse <- readRDS("base/dados_tse.rds") |> 
  mutate(
    tam_distrito = case_when(
      n_aptos_1_prefeito < 50000 ~ "Abaixo de 50 mil",
      n_aptos_1_prefeito >= 50000  & n_aptos_1_prefeito < 200000 ~ "Entre 50 mil e 200 mil",
      n_aptos_1_prefeito >= 200000 & n_aptos_1_prefeito < 500000 ~ "Entre 200 mil e 500 mil",
      n_aptos_1_prefeito >= 500000 ~ "Acima de 500 mil",
      TRUE ~ NA_character_
    ),
    tam_distrito = factor(
      tam_distrito,
      levels = c("Abaixo de 50 mil",
                 "Entre 50 mil e 200 mil",
                 "Entre 200 mil e 500 mil",
                 "Acima de 500 mil"),
      ordered = TRUE
    )
  ) |> 
  filter(!is.na(tam_distrito) & total_vereadores >= 9)  |> 
  mutate(
    prefeito_reeleito = ifelse(prefeito_reeleito == 999, NA, prefeito_reeleito)
  )

# Graficos ==================================================================

nerd_theme <- (
  theme_minimal() +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 16, color = "black", face = 'bold'),
      axis.text.y = element_text(size = 12),
      axis.title.y = element_text(size = 15),
      legend.text = element_text(size = 14),
      strip.text = element_text(size = 15, face = "bold")
    )
)


### plot 1: N médio de partidos na disputa ----

( plot_1 <- dados_tse |>
    select(ano_eleicao, cd_municipio,
           tam_distrito, n_partidos_vereadores_eleitoral) |>
    mutate(
      n_partidos_vereadores_eleitoral = ifelse(n_partidos_vereadores_eleitoral <= 1 | is.infinite(n_partidos_vereadores_eleitoral), NA, n_partidos_vereadores_eleitoral)
    ) |>
    group_by(ano_eleicao, tam_distrito) |>
    summarise(
      `Média de partidos` = mean(n_partidos_vereadores_eleitoral, na.rm = TRUE)
    ) |> 
    ggplot(aes(x = as.numeric(ano_eleicao), y = `Média de partidos`, color = tam_distrito, shape = tam_distrito)) +
    geom_line() +
    geom_vline(xintercept = 2016, linetype = 'dotdash', color = "firebrick", size = 1.25) +
    geom_line(size = 2) +
    geom_point(size = 6.5) +
    theme_minimal() +
    labs(
      x = NULL,
      y = "Nº médio de partidos na disputa",
      color = NULL,
      shape = NULL
      
    ) +
    scale_color_manual(
      values = c("orange", "seagreen", "grey10", "firebrick")
    ) + 
    scale_shape_manual(
      values = c(
        15:18
      )
    ) +
    ylim(0, 40) +
    scale_x_continuous(breaks = seq(2000, 2024, 4)) +
    nerd_theme
)

### plot 2: NEPs ----

( plot_2 <- dados_tse |> 
    select(
      ano_eleicao, 
      tam_distrito, 
      nep_cadeiras_vereadores, 
      nep_eleitoral_vereador_1,
      nep_candidatos_1_vereador
    ) |> 
    group_by(ano_eleicao, tam_distrito) |> 
    summarise(
      `NEP Cadeiras` = mean(nep_cadeiras_vereadores, na.rm = TRUE),
      `NEP Eleitoral` = mean(nep_eleitoral_vereador_1, na.rm = TRUE),
      `NEP Candidaturas` = mean(nep_candidatos_1_vereador, na.rm = TRUE)
    ) |> 
    pivot_longer(cols = `NEP Cadeiras`:`NEP Candidaturas`) |> 
    ggplot(aes(x = as.numeric(ano_eleicao), y = value, linetype = name, shape = name, color = name)) +
    #scale_color_manual(values = c("#525252", "gray3", "gray"))+
    geom_vline(xintercept = 2016, linetype = 'dotdash', color = "firebrick", size = 0.8) +
    geom_line(size = 1.25) +
    geom_point(size = 3.6, stroke = 2.5, fill = 'white') +
    facet_wrap(.~tam_distrito)+
    labs(
      x = "",
      y = NULL,
      linetype = "",
      shape    = "",
      color    = ""
    ) +
    ylim(0, 25) +
    scale_x_continuous(breaks = seq(2000, 2024, 4)) + 
    scale_shape_manual(
      values = c(17, 15, 16)
    ) +
    nerd_theme +
    scale_color_manual(
      values = c("firebrick", "seagreen", "grey10")
    )
  
)

### plot 3: índice de desproporcionalidade ----

( plot_3 <- dados_tse |> 
    select(ano_eleicao, cd_municipio, tam_distrito, gh_desproporcionalidade) |>
    group_by(ano_eleicao, tam_distrito) |> 
    summarise(
      gh_desproporcionalidade = mean(gh_desproporcionalidade, na.rm = T)
    ) |> 
    ggplot(aes(x = as.numeric(ano_eleicao), y = gh_desproporcionalidade, color = tam_distrito, shape = tam_distrito)) +
    geom_line() +
    geom_vline(xintercept = 2016, linetype = 'dashed', color = "firebrick", size = 1.0) +
    geom_line(size = 2) +
    geom_point(size = 6) +
    theme_minimal() +
    labs(
      x = NULL,
      y = "Índice de desproporcionalidade",
      color = NULL,
      shape = NULL
    ) +
    ylim(0, 20) +
    scale_x_continuous(breaks = seq(2000, 2024, 4)) + 
    scale_color_manual(
      values = c("orange", "seagreen", "grey10", "firebrick")
    ) + 
    scale_shape_manual(
      values = c(15:18)
    ) +
    nerd_theme
)

### plot 4: N médio de partidos nas coligações/federações dos prefeitos ----

(plot_4 <- dados_tse |> 
   select(ano_eleicao, cd_municipio, tam_distrito, n_partidos_colig_prefeito) |> 
   group_by(ano_eleicao, tam_distrito) |> 
   summarise(
     n = n(),
     n_partidos_colig_prefeito = mean(n_partidos_colig_prefeito, na.rm = TRUE)
   ) |> 
   ggplot(aes(x = as.numeric(ano_eleicao), y = n_partidos_colig_prefeito, color = tam_distrito, shape = tam_distrito)) +
   geom_line() +
   geom_vline(xintercept = 2016, linetype = 'dashed', color = "firebrick", size = 1.0) +
   geom_line(size = 2.0) +
   geom_point(size = 6.5) +
   theme_minimal() +
   labs(
     x = NULL,
     y = "Nº de partidos na coligação do prefeito",
     color = NULL,
     shape = NULL
   ) +
   coord_cartesian(
     expand = FALSE,
     xlim = c(1998.5, 2026.5),
     ylim = c(0, 10)
   ) +
   scale_color_manual(
     values = c("orange", "seagreen", "grey10", "firebrick")
   ) + 
   scale_shape_manual(
     values = c(15:18)
   ) +
   coord_cartesian(ylim = c(0, 15)) +
   scale_x_continuous(breaks = seq(2000, 2024, 4)) + 
   nerd_theme
)

### plot 5: Indicadores de governabilidade ----

( plot_5 <- dados_tse |> 
    select(
      ano_eleicao, 
      tam_distrito,
      p_vereadores_governistas,
      p_vereadores_correligionarios
    ) |> 
    group_by(ano_eleicao, tam_distrito) |> 
    summarise(
      p_vereadores_governistas      = mean(p_vereadores_governistas, na.rm = TRUE),
      p_vereadores_correligionarios = mean(p_vereadores_correligionarios, na.rm = TRUE)
    ) |>
    pivot_longer(cols = p_vereadores_governistas:p_vereadores_correligionarios) |> 
    mutate(name = factor(name, levels = c("p_vereadores_governistas", "p_vereadores_correligionarios"), 
                         labels = c("% Bancada Governista",
                                    "% Bancada do Partido"))) |> 
    ggplot(aes(x = as.numeric(ano_eleicao), y = value, shape = name, color = name)) +
    geom_vline(xintercept = 2016, linetype = 'dashed', color = "firebrick", size = 0.5) +
    geom_line(size = 1.5) +
    geom_point(size = 5.5) +
    facet_wrap(.~tam_distrito)+
    labs(x = "",
         y = NULL,
         linetype = NULL,
         shape = NULL,
         color = NULL)+
    scale_x_continuous(breaks = seq(2000, 2024, 4)) + 
    scale_shape_manual(
      values = c(17, 15)
    ) +
    scale_color_manual(
      values = c("firebrick", "grey10")
    ) + 
    coord_cartesian(ylim = c(0, 100)) +
    nerd_theme
)

# Modelos ==================================================================

dados_modelos <- dados_tse |> 
  ungroup() |> 
  filter(ano_eleicao %in% c(2012:2024)) |> 
  mutate(
    pt_dummy    = as.numeric(prefeito_partido_padronizado == "PT"),
    psdb_dummy  = as.numeric(prefeito_partido_padronizado == "PSDB"),
    mdb_dummy   = as.numeric(prefeito_partido_padronizado == "MDB"),
    ano_eleicao = relevel(factor(ano_eleicao), ref = "2016")   # ano de referência: 2016
  )

modelos <- list(
  "% Partidos Governistas" = fixest::feols(
    p_vereadores_governistas ~ n_partidos_colig_prefeito + nep_eleitoral_prefeito_1 +
      n_partidos_vereadores_eleitoral + log(n_aptos_1_prefeito) + total_vereadores +
      pt_dummy + psdb_dummy + mdb_dummy + ano_eleicao | cd_municipio,
    data = dados_modelos,
    vcov = ~cd_municipio   # erros-padrão clusterizados por município
  ),
  "% Partido do Prefeito" = fixest::feols(
    p_vereadores_correligionarios ~ n_partidos_colig_prefeito + nep_eleitoral_prefeito_1 +
      n_partidos_vereadores_eleitoral + log(n_aptos_1_prefeito) + total_vereadores +
      pt_dummy + psdb_dummy + mdb_dummy + ano_eleicao | cd_municipio,
    data = dados_modelos,
    vcov = ~cd_municipio   # erros-padrão clusterizados por município
  )
)

rotulos <- c(
  "n_partidos_colig_prefeito"       = "Nº de partidos na coligação do prefeito",
  "nep_eleitoral_prefeito_1"        = "NEP eleitoral",
  "n_partidos_vereadores_eleitoral" = "Nº de partidos na eleição para vereador",
  "log(n_aptos_1_prefeito)"         = "Log. nº de eleitores aptos",
  "total_vereadores"                = "Nº de vereadores",
  "pt_dummy"                        = "PT",
  "psdb_dummy"                      = "PSDB",
  "mdb_dummy"                       = "MDB",
  "ano_eleicao2012"                 = "Eleição de 2012",
  "ano_eleicao2020"                 = "Eleição de 2020",
  "ano_eleicao2024"                 = "Eleição de 2024"
)

plot_6 <- bind_rows(
  modelos[[1]] |> 
    broom::tidy() |> 
    mutate(modelo = "% Partidos Governistas"),
  modelos[[2]] |> 
    broom::tidy() |> 
    mutate(modelo = "% Partido do Prefeito")
) |>
  mutate(
    term_label = recode(term, !!!rotulos)
  ) |> 
  ggplot(aes(x = estimate, y = term_label, color = modelo)) +
  geom_point(size = 2.5) +
  geom_errorbar(
    aes(
      xmin = estimate - (1.96 * std.error), 
      xmax = estimate + (1.96 * std.error)
    ),
    width = 0
  ) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey25") +
  nerd_theme +
  labs(
    y = NULL,
    x = "Estimadores",
    color = NULL
  ) +
  scale_color_manual(
    values = c("firebrick2", "seagreen3")
  )

modelos |> 
  modelsummary::modelsummary(stars = TRUE)

### salvar plots -----

dir.create("plots/maiorias", recursive = TRUE, showWarnings = FALSE)

for (i in 1:6){
  plot_df_name <- paste0("plot_", i)
  plot_df <- get(plot_df_name)
  ggsave(filename = paste0("plots/maiorias/", plot_df_name, ".png"), plot = plot_df, width = 10, height = 7)
}


# Apêndices ==================================================================

dados_tse |> 
  select(
    n_partidos_vereadores_eleitoral, 
    nep_candidatos_1_vereador, nep_eleitoral_vereador_1, nep_cadeiras_vereadores,
    gh_desproporcionalidade,
    n_partidos_colig_prefeito,
    p_vereadores_governistas,
    p_vereadores_correligionarios
  ) |> 
  ungroup() |> 
  pivot_longer(2:last_col()) |> 
  group_by(name) |> 
  mutate(
    x_2 = ((value - mean(value, na.rm = TRUE)) ^ 2)
  ) |> 
  summarise(
    n = n(),
    media = mean(value, na.rm = TRUE), 
    sd    = sqrt(sum(x_2, na.rm = TRUE)/(n - 1)),
    se    = sd/sqrt(n),
    min   = min(value, na.rm = TRUE),
    max   = max(value, na.rm = TRUE)
  ) |> 
  kableExtra::kable() |> 
  kableExtra::kable_classic_2()


dados_tse |> 
  filter(ano_eleicao %in% c(2012:2024)) |> 
  mutate(
    n_partidos_colig_cat = case_when(
      n_partidos_colig_prefeito %in% 0:1 ~ "0 a 1",
      n_partidos_colig_prefeito %in% 2:5 ~ "2 a 5",
      n_partidos_colig_prefeito %in% 6:10 ~ "6 a 10",
      n_partidos_colig_prefeito > 10 ~ "Acima de 10",
      TRUE ~ as.character(n_partidos_colig_prefeito)
    )
  ) |> 
  group_by(ano_eleicao, n_partidos_colig_cat) |> 
  summarise(
    n = n()
  ) |> 
  pivot_wider(names_from = ano_eleicao, values_from = n) |> 
  kableExtra::kable() |> 
  kableExtra::kable_classic_2()
