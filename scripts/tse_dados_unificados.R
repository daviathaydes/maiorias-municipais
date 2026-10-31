# -------------------------------------------------------------------------- #
#                             TSE DADOS UNIFICADOS                           #
# -------------------------------------------------------------------------- #

library(dplyr)
library(tidyr)
library(purrr)
library(stringi)
library(stringr)
library(ineq)

# ============================================================================
# IMPORTAÇÃO DE DADOS ========================================================

ideologia_partidos <- openxlsx::read.xlsx("data/partidos_ideologia_compilado.xlsx") |> 
  mutate(
    partido = toupper(partido)
  )

{
  eleicoes_apuracao <- list()
  for (x in list.files("data/raw/tse_apuracao/")){
    df <- readRDS(paste0("data/raw/tse_apuracao/", x)) 
    eleicoes_apuracao[[x]] <- df |> 
      filter(nm_tipo_eleicao == "Eleição Ordinária")
  }
}

{
  eleicoes_votos <- list()
  for (x in list.files("data/raw/tse_voto/")){
    print(x)
    df <- readRDS(paste0("data/raw/tse_voto/", x)) |> 
      janitor::clean_names() |> 
      filter(ds_cargo == "Prefeito" & toupper(nm_tipo_eleicao) == "ELEIÇÃO ORDINÁRIA") |> 
      select(
        ano_eleicao, cd_municipio, nr_turno, 
        ds_cargo, 
        nr_candidato, nm_urna_candidato, 
        sg_partido, qt_votos_nominais
      ) |> 
      group_by(
        ano_eleicao, cd_municipio, nr_turno, sg_partido,
        nr_candidato
      ) |> 
      summarise(qt_votos_nominais = sum(qt_votos_nominais)) |> 
      ungroup()
    
    eleicoes_votos[[x]] <- df
    rm(df)
  }
}

eleicoes_votos[["data/raw/tse_voto/votacao_candidato_munzona_2000_BRASIL.rds"]] <- readRDS("data/raw/tse_voto/votacao_candidato_munzona_2000_BRASIL.rds") |> 
  janitor::clean_names() |> 
  filter(ds_cargo == "Prefeito" & toupper(nm_tipo_eleicao) == "ELEIÇÃO ORDINÁRIA") |> 
  select(
    ano_eleicao, cd_municipio, nr_turno, 
    ds_cargo, 
    nr_candidato, nm_urna_candidato, 
    sg_partido, qt_votos_nominais_validos
  ) |> 
  group_by(
    ano_eleicao, cd_municipio, nr_turno, sg_partido,
    nr_candidato
  ) |> 
  summarise(qt_votos_nominais = sum(qt_votos_nominais_validos)) |> 
  ungroup()

base_margem <- eleicoes_votos |>
  bind_rows() |> 
  group_by(cd_municipio, ano_eleicao, nr_turno) |> 
  arrange(cd_municipio, ano_eleicao, nr_turno, -qt_votos_nominais) |> 
  mutate(
    total_votos = sum(qt_votos_nominais),
    posicao = row_number(),
    p_votos = qt_votos_nominais/total_votos * 100
  ) |> 
  filter(
    posicao %in% c(1, 2)
  ) |> 
  select(
    ano_eleicao, cd_municipio, posicao, nr_turno, sg_partido, qt_votos_nominais
  )

margem_votos <- base_margem |> 
  select(-sg_partido) |> 
  pivot_wider(names_prefix = "cand_", names_from = posicao, values_from = qt_votos_nominais) |> 
  mutate(
    cand_2 = ifelse(is.na(cand_2), 0, cand_2),
    margem_votos = (1 - (cand_2/cand_1)) * 100
  ) |> 
  select(-c(cand_1, cand_2)) |> 
  #filter(ano_eleicao != "2000") |> 
  pivot_wider(names_from = "nr_turno", values_from = "margem_votos", names_prefix = "margem_votos_turno_") |> 
  mutate(
    ano_eleicao = as.character(ano_eleicao),
    cd_municipio = as.character(cd_municipio)
  )

polarizacao <- base_margem |> 
  select(-qt_votos_nominais) |> 
  filter(nr_turno != 1) |> 
  mutate(
    sg_partido = toupper(sg_partido)
  ) |> 
  left_join(
    ideologia_partidos,
    by = c("sg_partido" = "partido")
  ) |> 
  group_by(ano_eleicao, cd_municipio, posicao, ideologia) |> 
  summarise(
    ideologia = mean(ideologia)
  ) |> 
  ungroup() |> 
  pivot_wider(names_prefix = "cand_", names_from = posicao, values_from = ideologia) |>
  mutate(
    polarizacao_2_turno = abs(cand_1 - cand_2)
  ) |> 
  select(
    -c(cand_1, cand_2)
  ) |> 
  mutate(
    ano_eleicao = as.character(ano_eleicao),
    cd_municipio = as.character(cd_municipio)
  )

rm(eleicoes_votos)
# 71072

{
  eleicoes_cand <- list()
  for (x in list.files("data/raw/tse_cand/")){
    print(x)
    df <- readRDS(paste0("data/raw/tse_cand/", x)) |> 
      filter(nm_tipo_eleicao == "ELEIÇÃO ORDINÁRIA") |> 
      select(
        ano_eleicao,
        sg_ue, 
        nr_turno, 
        nr_titulo_eleitoral_candidato,
        nm_candidato, nm_urna_candidato,
        sg_partido, ds_composicao_coligacao,
        ds_cargo,
        ds_sit_tot_turno
      ) |> 
      mutate(
        across(ds_cargo:ds_sit_tot_turno, ~ stringi::stri_trans_general(tolower(.x), "ascii"))
      ) |> 
      #filter(
      #  #ds_cargo %in% c("prefeito", "vereador")
      #) |> 
      janitor::clean_names()
    
    eleicoes_cand[[x]] <- df
  }
}

tse_partido <- list()
for(file in list.files("data/raw/tse_partido/", pattern = ".rds")){
  tse_partido[[file]] <- readRDS(paste0("data/raw/tse_partido/", file)) |> 
    filter(toupper(nm_tipo_eleicao) == "ELEIÇÃO ORDINÁRIA")
}

tse_partido_00_16 <- tse_partido
tse_partido_20_24 <- list(
  tse_partido[["votacao_partido_munzona_2020_BRASIL.rds"]],
  tse_partido[["votacao_partido_munzona_2024_BRASIL.rds"]]
)

tse_partido_00_16[["votacao_partido_munzona_2020_BRASIL.rds"]] <- NULL
tse_partido_00_16[["votacao_partido_munzona_2024_BRASIL.rds"]] <- NULL


df_partido_00_16 <- tse_partido_00_16 |>
  map_df(
    ~ .x |>
      select(ano_eleicao, cd_municipio, nr_turno, nr_zona, ds_cargo, sg_partido, qt_votos_legenda, qt_votos_nominais)
  )

df_partido_20_24 <- tse_partido_20_24 |>
  map_df(
    ~ .x |>
      select(ano_eleicao, cd_municipio, nr_turno, nr_zona, ds_cargo, sg_partido, qt_votos_legenda = qt_votos_legenda_validos, qt_votos_nominais = qt_votos_nominais_validos)
  )

tse_partido_completo <- bind_rows(
  df_partido_00_16,
  df_partido_20_24
) 

# ============================================================================
# TRATAMENTO DE DADOS ========================================================

eleicoes_cand_df <- eleicoes_cand |> 
  map_df(~ mutate(.x,, across( everything(), as.character)))


# ============================================================================
# INDICADORES APURAÇÃO =======================================================


apuracao_df_f <- eleicoes_apuracao |> 
  bind_rows() |>
  select(ano_eleicao, cd_municipio, sg_uf, nm_municipio, nr_turno, ds_cargo, contains("qt")) |> 
  janitor::clean_names() |> 
  pivot_longer(cols = c(contains("qt"))) |> 
  mutate(
    name = glue::glue("p_{name}_{nr_turno}_{tolower(ds_cargo)}") |> 
      stringi::stri_replace(regex = "_qt_", replacement = "_")
  ) |> 
  select(
    -c(nr_turno, ds_cargo)
  ) |> 
  pivot_wider(values_fn = sum) |> 
  janitor::clean_names() |> 
  select(
    ano_eleicao, 
    cd_municipio, sg_uf, nm_municipio,
    
    n_aptos_1_prefeito = p_aptos_1_prefeito,
    p_comparecimento_1_prefeito, 
    p_votos_nulos_1_prefeito, p_votos_brancos_1_prefeito,
    p_votos_nulos_1_vereador, p_votos_brancos_1_vereador,
    
    p_comparecimento_2_prefeito, 
    p_votos_nulos_2_prefeito, p_votos_brancos_2_prefeito,
    
  ) |> 
  mutate(
    across(p_comparecimento_1_prefeito:p_votos_brancos_2_prefeito, function(x) x/n_aptos_1_prefeito * 100)
  )

# ============================================================================
# INDICADORES EXECUTIVO ======================================================

# ============================================================================
## Prefeitos =================================================================
message("PREFEITOS")

prefeitos_df <- eleicoes_cand_df |> 
  filter(ds_cargo == "prefeito" & stringi::stri_detect(ds_sit_tot_turno, regex = "^eleito")) |> 
  rename(
    prefeito_eleito_nr_turno = nr_turno, 
    prefeito_eleito_nr_titulo_eleitoral_candidato = nr_titulo_eleitoral_candidato,
    prefeito_eleito_nm_candidato = nm_candidato,
    prefeito_eleito_nm_urna_candidato = nm_urna_candidato,
    prefeito_eleito_sg_partido = sg_partido
  ) |> 
  select(
    -c(ds_cargo, ds_sit_tot_turno)
  ) |> 
  distinct(ano_eleicao, sg_ue, .keep_all = TRUE) |> 
  group_by(sg_ue) |>
  arrange(sg_ue, ano_eleicao) |>
  mutate(
    prefeito_eleito_nr_titulo_eleitoral_candidato = as.character(as.numeric(prefeito_eleito_nr_titulo_eleitoral_candidato)),
    prefeito_reeleito = as.numeric(prefeito_eleito_nr_titulo_eleitoral_candidato == lag(prefeito_eleito_nr_titulo_eleitoral_candidato)),
    prefeito_eleicao_anterior = lag(prefeito_eleito_nr_titulo_eleitoral_candidato),
    
    # MAIORIAS
    prefeito_eleito_sg_partido = toupper(prefeito_eleito_sg_partido),
    ds_composicao_coligacao = ifelse(stringi::stri_detect(tolower(ds_composicao_coligacao), regex = ".nulo."), prefeito_eleito_sg_partido, toupper(ds_composicao_coligacao)),
    partidos_coligacao_prefeito = ds_composicao_coligacao %>%
      stri_replace_all_regex("FEDERAÇÃO.*?\\((.*?)\\)", "$1") %>%
      stri_replace_all_regex("\\s*/\\s*", " / ") %>%
      stri_replace_all_regex(" / ", "|") %>%
      stri_replace_all_regex("^[|]", "") %>%
      stri_trim_both()
    
  ) |> 
  ungroup()


prefeitos_tentou_reeleicao <- eleicoes_cand_df |> 
  filter(ds_cargo == "prefeito") |> 
  select(-ds_composicao_coligacao) |> 
  filter(
    nr_titulo_eleitoral_candidato != "-4"
  ) |> 
  left_join(prefeitos_df) |> 
  mutate(
    prefeito_eleicao_anterior_tentou = as.numeric(as.numeric(nr_titulo_eleitoral_candidato) == as.numeric(prefeito_eleicao_anterior))
  ) |> 
  select(
    ano_eleicao,
    sg_ue,
    prefeito_eleicao_anterior_tentou
  ) |> 
  arrange(sg_ue, ano_eleicao, -prefeito_eleicao_anterior_tentou) |> 
  distinct(sg_ue, ano_eleicao, .keep_all = TRUE) |> 
  ungroup()

prefeitos_ideologia <- prefeitos_df |> 
  select(ano_eleicao, sg_ue, prefeito_eleito_sg_partido) |> 
  mutate(
    prefeito_eleito_sg_partido = toupper(prefeito_eleito_sg_partido)
  ) |> 
  left_join(
    ideologia_partidos,
    by = c("prefeito_eleito_sg_partido" = "partido")
  ) |> 
  select(-prefeito_eleito_sg_partido) |> 
  rename(ideologia_prefeito_eleito = ideologia) |> 
  mutate(
    ano_eleicao = as.character(ano_eleicao),
    sg_ue = as.character(sg_ue)
  )

prefeitos_df <- prefeitos_df |>
  select(
    -c(
      prefeito_reeleito,
      prefeito_eleicao_anterior
    )
  )

# ============================================================================
# MAIORIAS EXECUTIVO =========================================================
message("MAIORIAS LEGISLATIVAS")

vereadores_base <- eleicoes_cand_df %>%  
  filter(
    ds_cargo == "vereador" & stringi::stri_detect(stri_trim_both(ds_sit_tot_turno), regex = "^(eleito|media)")
  ) %>% 
  select(ano_eleicao, sg_ue, nr_titulo_eleitoral_candidato, nm_urna_candidato, sg_partido) %>% 
  left_join(prefeitos_df |> select(ano_eleicao, sg_ue, prefeito_eleito_sg_partido, partidos_coligacao_prefeito), by = c("ano_eleicao", "sg_ue")) %>% 
  mutate(
    sg_partido = toupper(stri_trim_both(sg_partido)),
    partidos_lista = str_split(partidos_coligacao_prefeito, "\\|"),
    partidos_lista = map(partidos_lista, ~ toupper(stri_trim_both(.x))),
    vereador_coligacao_prefeito = map2_lgl(sg_partido, partidos_lista, ~ .x %in% .y),
    vereador_partido_prefeito   = toupper(stri_trim_both(sg_partido)) == toupper(stri_trim_both(prefeito_eleito_sg_partido)),
  ) |> 
  select(-partidos_lista)

vereadores_governistas <- vereadores_base |> 
  group_by(ano_eleicao, sg_ue, partidos_coligacao_prefeito, vereador_coligacao_prefeito) |>
  summarise(n = n()) |> 
  group_by(ano_eleicao, sg_ue) |> 
  mutate(
    total_vereadores = sum(n),
    vereador_coligacao_prefeito = case_when(
      vereador_coligacao_prefeito == TRUE ~ "p_vereadores_governistas",
      vereador_coligacao_prefeito == FALSE ~ "p_vereadores_oposicao"
    )
  ) |> 
  pivot_wider(names_from = vereador_coligacao_prefeito, values_from = n) |> 
  mutate(
    across(p_vereadores_governistas:p_vereadores_oposicao, ~ ifelse(is.na(.x), 0, .x/total_vereadores * 100)),
    n_partidos_colig_prefeito = stringi::stri_count(partidos_coligacao_prefeito, regex = "[|]")
  ) |> 
  filter(!is.na(partidos_coligacao_prefeito)) |> 
  select(-p_vereadores_oposicao) |> 
  ungroup()

vereadores_correligionarios <- vereadores_base |> 
  group_by(ano_eleicao, sg_ue, partidos_coligacao_prefeito, vereador_partido_prefeito) |>
  summarise(n = n()) |> 
  group_by(ano_eleicao, sg_ue) |> 
  mutate(
    
    total_vereadores = sum(n),
    vereador_partido_prefeito = case_when(
      vereador_partido_prefeito == TRUE ~ "p_vereadores_correligionarios",
      vereador_partido_prefeito == FALSE ~ "p_outros_vereadores"
    )
    
  ) |> 
  filter(!is.na(partidos_coligacao_prefeito)) |> 
  pivot_wider(names_from = vereador_partido_prefeito, values_from = n) |> 
  mutate(
    across(p_vereadores_correligionarios:p_outros_vereadores, ~ ifelse(is.na(.x), 0, .x/total_vereadores * 100))
  ) |> 
  #select(-p_outros_vereadores) |> 
  ungroup()

maiorias_legislativas <- vereadores_governistas |> 
  left_join(vereadores_correligionarios |> select(ano_eleicao, sg_ue, p_vereadores_correligionarios))

rm(
  vereadores_base,
  vereadores_governistas,  
  vereadores_correligionarios
)

# ============================================================================
## Vice-Prefeitos ============================================================
message("VICE PREFEITOS")

vice_prefeitos_df <- eleicoes_cand_df |> 
  filter(ds_cargo == "vice-prefeito") |> 
  rename(
    vice_eleito_nr_titulo_eleitoral_candidato = nr_titulo_eleitoral_candidato,
    vice_eleito_nm_candidato = nm_candidato,
    vice_eleito_nm_urna_candidato = nm_urna_candidato,
    vice_eleito_sg_partido = sg_partido
  ) |>
  select(
    -c(nr_turno, ds_cargo, ds_sit_tot_turno)
  ) 

prefeitos_df_completo <- prefeitos_df |> 
  left_join(vice_prefeitos_df)


# ============================================================================
## N de CAND. ================================================================

cand_df <- eleicoes_cand_df |> 
  filter(
    ds_cargo %in% c("prefeito", "vereador"), 
    nr_turno == 1
  ) |> 
  filter(nr_turno == 1) |> 
  group_by(ano_eleicao, sg_ue, nr_turno, ds_cargo) |> 
  summarise(
    n_candidatos = n()
  ) |> 
  ungroup() |> 
  mutate(
    name = paste0("n_candidatos_", nr_turno, "_", ds_cargo)
  ) |> 
  select(
    -c(nr_turno, ds_cargo)
  ) |> 
  pivot_wider(
    values_from = n_candidatos
  ) |> 
  janitor::clean_names() |> 
  select(ano_eleicao, sg_ue, contains("_1_"))

nep_cand_df <- eleicoes_cand_df |> 
  filter(
    ds_cargo %in% c("prefeito", "vereador"), 
    nr_turno == 1
  ) |> 
  group_by(ano_eleicao, sg_ue, nr_turno, ds_cargo, sg_partido) |> 
  summarise(
    n_candidatos = n()
  ) |> 
  group_by(ano_eleicao, sg_ue, nr_turno, ds_cargo) |> 
  mutate(
    p   = n_candidatos/sum(n_candidatos),
    p_2 = p**2
  ) |> 
  summarise(
    nep = 1/sum(p_2)
  ) |> 
  ungroup() |> 
  mutate(
    name = paste0("nep_candidatos_", nr_turno, "_", ds_cargo)
  ) |> 
  select(
    -c(nr_turno, ds_cargo)
  ) |> 
  pivot_wider(
    values_from = nep
  ) |> 
  janitor::clean_names()


# ============================================================================
# INDICADORES LEGISLATIVO ====================================================
# 
# partido_prefeito <- eleicoes_cand_df |> 
#   group_by(ano_eleicao, sg_ue, sg_partido) |> 
#   filter(
#     ds_cargo == "vereador"  & stringi::stri_detect(ds_sit_tot_turno, regex = "^eleito")
#   ) |> 
#   summarise(
#     n_vereadores= n()
#   ) |> 
#   group_by(ano_eleicao, sg_ue) |> 
#   mutate(
#     total_vereadores = sum(n_vereadores)
#   ) |> 
#   left_join(
#     prefeitos_df |> select(ano_eleicao, sg_ue, prefeito_eleito_sg_partido),
#     by = c("ano_eleicao", "sg_ue")
#   ) |> 
#   mutate(
#     partido_comp = prefeito_eleito_sg_partido == sg_partido,
#     p_vereadores_partido_prefeito = (n_vereadores/total_vereadores) * 100
#   ) |> 
#   filter(partido_comp == TRUE) |> 
#   select(
#     ano_eleicao, 
#     sg_ue,
#     p_vereadores_partido_prefeito
#   ) |> 
#   arrange(sg_ue, ano_eleicao)


vereadores_eleitoral <- eleicoes_cand_df |> 
  group_by(ano_eleicao, sg_ue, sg_partido) |> 
  filter(
    ds_cargo == "vereador" 
  ) |> 
  summarise(
    vereadores_eleitoral = n()
  ) |> 
  ungroup()

vereadores_camara <- eleicoes_cand_df |> 
  group_by(ano_eleicao, sg_ue, sg_partido) |> 
  filter(
    ds_cargo == "vereador" & stringi::stri_detect(ds_sit_tot_turno, regex = "^eleito")
  ) |> 
  summarise(
    vereadores_camara = n()
  ) |> 
  ungroup()

partidos_vereadores_eleitoral <- vereadores_eleitoral |> 
  group_by(ano_eleicao, sg_ue) |> 
  summarise(
    n_partidos_vereadores_eleitoral = n()
  ) |> 
  ungroup()

# ============================================================================
## NEP - CAMARA ==============================================================

message("NEP - CAMARA")
nep_cadeiras <- vereadores_camara |> 
  left_join(
    vereadores_camara
  ) |> 
  group_by(ano_eleicao, sg_ue) |> 
  mutate(
    p = (vereadores_camara/sum(vereadores_camara, na.rm = TRUE))**2
  ) |> 
  summarise(
    nep_cadeiras_vereadores = 1 / sum(p, na.rm = TRUE)
  ) |> 
  arrange(sg_ue, ano_eleicao)


# =============================================================================
## NEP - ELEITORAL ============================================================

message("NEP - ELEITORAL")
nep_eleitoral <- tse_partido_completo |> 
  group_by(ano_eleicao, cd_municipio, nr_turno, ds_cargo, sg_partido) |> 
  summarise(
    votos = sum(qt_votos_legenda + qt_votos_nominais)
  ) |> 
  group_by(ano_eleicao, cd_municipio, nr_turno, ds_cargo) |> 
  mutate(
    p = votos/sum(votos)
  ) |> 
  summarise(
    nep_eleitoral = 1/sum(p^2)
  ) |> 
  ungroup() |> 
  mutate(
    name = tolower(paste("nep_eleitoral", ds_cargo, nr_turno, sep = "_"))
  ) |> 
  select(
    -c(nr_turno, ds_cargo)
  ) |> 
  pivot_wider(names_from = name, values_from = nep_eleitoral) |> 
  mutate(
    ano_eleicao = as.character(ano_eleicao),
    cd_municipio = as.character(cd_municipio)
  )

# =============================================================================
## Desproporcionalidade =======================================================

message("Calculando Desproporcionalidade")
desproporcionalidade <- tse_partido_completo |> 
  filter(
    tolower(ds_cargo) == "vereador" 
  ) |> 
  group_by(ano_eleicao, cd_municipio, sg_partido) |> 
  summarise(
    votos = sum(qt_votos_legenda + qt_votos_nominais)
  ) |> 
  group_by(ano_eleicao, cd_municipio) |> 
  mutate(
    p_votos      = votos/sum(votos) * 100,
    ano_eleicao  = as.character(ano_eleicao),
    cd_municipio = as.character(cd_municipio)
  ) |> 
  ungroup() |> 
  left_join(
    vereadores_camara |> 
      group_by(ano_eleicao, sg_ue) |> 
      mutate(
        p_cadeiras = vereadores_camara/sum(vereadores_camara, na.rm = TRUE) * 100
      ) |> 
      ungroup() |> 
      mutate(
        ano_eleicao = as.character(ano_eleicao),
        sg_ue = as.character(sg_ue)
      ),
    by = c("ano_eleicao", "cd_municipio" = "sg_ue", "sg_partido")
  ) |> 
  mutate(
    p_cadeiras = ifelse(is.na(p_cadeiras), 0, p_cadeiras),
    p_votos = ifelse(is.na(p_votos), 0, p_votos),
    diff = p_votos - p_cadeiras # V - S
  ) |>
  group_by(ano_eleicao, cd_municipio) |> 
  summarise(
    lh_desproporcionalidade = (1/2)*sum(abs(diff), na.rm = TRUE),
    gh_desproporcionalidade = sqrt((1/2)*(sum(diff^2, na.rm = TRUE)))
  )  |>
  mutate(
    ano_eleicao = as.character(ano_eleicao),
    cd_municipio = as.character(cd_municipio)
  )

# ============================================================================
# EXPORTANDO DADOS ===========================================================
message("Fazendo Join Final")

print(cand_df)
colnames(cand_df)

dados_tse <- apuracao_df_f |> 
  filter(ano_eleicao %in% c(2000:2024)) |> 
  mutate(
    ano_eleicao = as.character(ano_eleicao),
    cd_municipio = as.character(cd_municipio)
  ) |> 
  
  left_join(prefeitos_df_completo, by = c("ano_eleicao", "cd_municipio" = "sg_ue")) |> 
  
  left_join(cand_df, by = c("ano_eleicao", "cd_municipio" = "sg_ue")) |>
  left_join(nep_cand_df, by = c("ano_eleicao", "cd_municipio" = "sg_ue")) |>
  left_join(prefeitos_tentou_reeleicao, by = c("ano_eleicao", "cd_municipio" = "sg_ue")) |> 
  left_join(margem_votos, by = c("ano_eleicao", "cd_municipio")) |> 
  left_join(polarizacao, by = c("ano_eleicao", "cd_municipio")) |> 
  left_join(prefeitos_ideologia, by = c("ano_eleicao", "cd_municipio" = "sg_ue")) |> 
  
  left_join(nep_cadeiras, by = c("ano_eleicao", "cd_municipio" = "sg_ue")) |>
  left_join(nep_eleitoral, by = c("ano_eleicao", "cd_municipio")) |>
  left_join(desproporcionalidade, by = c("ano_eleicao", "cd_municipio")) |>
  left_join(maiorias_legislativas, by = c("ano_eleicao", "cd_municipio" = "sg_ue")) |>
  left_join(partidos_vereadores_eleitoral, by = c("ano_eleicao", "cd_municipio" = "sg_ue")) |>

  janitor::clean_names() |>
  group_by(cd_municipio) |>
  arrange(cd_municipio, ano_eleicao) |>
  mutate(
    
    prefeito_eleito_nr_titulo_eleitoral_candidato = as.character(as.numeric(prefeito_eleito_nr_titulo_eleitoral_candidato)),
    prefeito_reeleito = as.numeric(prefeito_eleito_nr_titulo_eleitoral_candidato == lag(prefeito_eleito_nr_titulo_eleitoral_candidato)),
    vice_virou_prefeito = as.numeric(prefeito_eleito_nr_titulo_eleitoral_candidato == lag(vice_eleito_nr_titulo_eleitoral_candidato)),
    vice_reeleito = as.numeric(vice_eleito_nr_titulo_eleitoral_candidato == lag(vice_eleito_nr_titulo_eleitoral_candidato)),
    
    prefeito_partido_padronizado = case_when(
      prefeito_eleito_sg_partido %in% c("PFL", "DEM") ~ "UNIÃO",
      prefeito_eleito_sg_partido %in% c("PSL") ~ "UNIÃO",
      prefeito_eleito_sg_partido %in% c("PRB") ~ "REPUBLICANOS",
      prefeito_eleito_sg_partido %in% c("PTdoB", "AVANTE") ~ "AVANTE",
      prefeito_eleito_sg_partido %in% c("PRP", "PATRI") ~ "PATRIOTA",
      prefeito_eleito_sg_partido %in% c("PHS", "PODE") ~ "PODEMOS",
      prefeito_eleito_sg_partido %in% c("PTN", "PODE") ~ "PODEMOS",
      prefeito_eleito_sg_partido %in% c("PMDB") ~ "MDB",
      TRUE ~ prefeito_eleito_sg_partido
    ),
    
    prefeito_continuidade_partido  = as.numeric(
      prefeito_partido_padronizado == lag(prefeito_partido_padronizado)
    ),
    across(prefeito_reeleito:prefeito_continuidade_partido, ~ ifelse(is.na(.x), 999, .x)),
    prefeito_continuidade_politica = as.numeric(
      prefeito_reeleito == 1 |
      prefeito_continuidade_partido == 1 |
      vice_virou_prefeito == 1 |
      vice_reeleito == 1
    )
  ) |>
  distinct(ano_eleicao, cd_municipio, .keep_all = TRUE)
  
saveRDS(dados_tse, "data/processed/dados_tse.rds")
