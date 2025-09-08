#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Selección y descriptivos iniciales
# Institution: PNUD
# Responsable: Consultor técnico - MVM
# Executive Summary: Este script contiene el código para un procesamiento inicial de los datos
# Date: 8 de septiembre de 2025
#******************************************************************************************************************************************************

rm(list = ls())

# 1. Load packages ------------------------------------------------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    sjlabelled,
    sjmisc,
    sjPlot,
    janitor,
    glue,
    srvyr,
    openxlsx,
    scales
)

# 2. Load data and functions ----------------------------------------------------------------------------------------------------------------------------

# enusc_original <- read_sav("input/data/original/base-de-datos---enusc-2024.sav")
# saveRDS(enusc_original, "input/data/original/base-de-datos---enusc-2024.RDS")

enusc_original <- readRDS("input/data/original/base-de-datos---enusc-2024.RDS")
source("processing/helpers/functions.R")

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Select variables -----------------------------------------------------------------------------------------------------------------------------------

# DIMENSIONES
# 1a. EMOCIONAL- PERSONAL (emper) = P_EXPOS_DELITO (P4), P_INSEG (P3) -> Contextos en los que las personas se sienten inseguras
# 1b EMOCIONAL- GENERAL (emgen) = ?

# 2a. COGNITIVA/PERCEPCTUAL- PERSONAL (perper) = P_EXPOS_DELITO (P7), P_DELITO_PRONOSTICO (P8) -> Percepción de la probabilidad de ser victima de delito o de aumentos de delito
# 2b. COGNITIVA/PERCEPTUAL - GENERAL (pergen) = P_AUMENTO (P1)

# 3a. COMPORTAMIENTO - PERSONAL (comper) = P_MOD_ACTIVIDADES (P9), COSTOS_MEDIDAS (MDC6)
# 3b. COMPORTAMIENTO - GENERAL (comgen) =  ANTIG_SECTOR (MDC1), MEDIDAS (MDC2), ADOPTADAS (MDC3), VECINOS (MDC4), VECINOS_ADOPTADAS (MDC5)

emper <- "P_INSEG"
perper <- "P_EXPOS_DELITO|P_DELITO_PRONOSTICO"
pergen <- "P_AUMENTO"
comper <- "P_MOD_ACTIVIDADES|COSTOS_MEDIDAS"
comgen <- "MEDIDAS|ADOPTADAS|VECINOS_MEDIDAS|VECINOS_ADOPTADAS"

enusc <- enusc_original %>%
    filter(Kish == 1) %>% # ! IMPORTANTE
    select(
        rph_ID, idhogar, enc_region, Conglomerado, VarStrat, starts_with("Fact"),
        matches(emper), matches(perper), matches(pergen), matches(comper), matches(comgen),
        starts_with("rph")
    ) %>%
    rename_with(~ glue("emper_{.x}"), matches(emper)) %>%
    rename_with(~ glue("perper_{.x}"), matches(perper)) %>%
    rename_with(~ glue("pergen_{.x}"), matches(pergen)) %>%
    rename_with(~ glue("comper_{.x}"), matches(comper)) %>%
    rename_with(~ glue("comgen_{.x}"), matches(comgen)) %>%
    clean_names() %>%
    rename("comper_costos_medidas" = comgen_comper_costos_medidas) %>% # ! PARCHE: Ya que se trabaja con regex laxos, COSTOS_MEDIDAS se renombra dos veces, quedando con dos sufijos.
    mutate(
        comgen_vecinos_adoptadas_privad = set_label(
            .$comgen_vecinos_adoptadas_privad,
            label = "¿Cuál/es de estas medidas adoptó en los últimos doce meses junto a sus vecinos? Tenemos contratados vigilantes privados"
        )
    ) # ! PARCHE: Fix label.

rm(emper, perper, comper, comgen)

# 4. Descriptivos iniciales ------------------------------------------------------------------------------------------------------------------------------

# Crear objeto encuesta
# enusc_svy <- enusc %>%
#     as_survey_design(ids = conglomerado, stata = varstrat, weights = fact_pers_reg)

# Probar funciones
# tab_frq1(var = emper_p_inseg_lugares_1, verbose = TRUE, sep_verbose = FALSE, sort.frq = "desc")
# tab_frq2(var = emper_p_inseg_lugares_1, verbose = TRUE, sep_verbose = TRUE, vartype = c("se", "ci"))

# Vectores de variables
dim_names <- c("emper", "perper", "pergen", "comper", "comgen")

emper_vars <- enusc %>%
    select(starts_with("emper")) %>%
    names()
perper_vars <- enusc %>%
    select(starts_with("perper")) %>%
    names()
pergen_vars <- enusc %>%
    select(starts_with("pergen")) %>%
    names()
comper_vars <- enusc %>%
    select(starts_with("comper")) %>%
    names()
comgen_vars <- enusc %>%
    select(
        starts_with("comgen"), -comgen_medidas_na, -comgen_medidas_ns, -comgen_medidas_nr,
        -comgen_adoptadas_na, -comgen_adoptadas_otro, -comgen_vecinos_medidas_na,
        -comgen_vecinos_medidas_ns, -comgen_medidas_nr, -comgen_vecinos_adoptadas_na
    ) %>%
    names()

# Iterar! # no genera bien algunas tablas - revisar
emper_tabs <- map(emper_vars, ~ tab_frq1(var = {{ .x }})) %>% set_names(emper_vars)
perper_tabs <- map(perper_vars, ~ tab_frq1(var = {{ .x }}, pattern_verbose = "(\\?|en su|en el)\\s*")) %>% set_names(perper_vars)
pergen_tabs <- map(pergen_vars, ~ tab_frq1(var = {{ .x }}, pattern_verbose = "(\\?|en su|en el)\\s*")) %>% set_names(pergen_vars)
comper_tabs <- map(comper_vars, ~ tab_frq1(var = {{ .x }})) %>% set_names(comper_vars)
comgen_tabs <- map(comgen_vars, ~ tab_frq1(var = {{ .x }}, pattern_verbose = "(\\?|\\.)\\s*")) %>% set_names(comgen_vars)


# Guardar todas
all_tabs <- list(emper_tabs, perper_tabs, pergen_tabs, comper_tabs, comgen_tabs) %>% set_names(dim_names)

# 4.  Save things ----------------------------------------------------------------------------------------------------------------------------------------

# Guardar bbdd enusc
saveRDS(enusc, "input/data/proc/enusc_1_select_desc.RDS")

# Guardar tablas en excel
map2(
    seq_along(all_tabs),
    dim_names,
    ~ format_tab_excel(all_tabs[[.x]] %>% list_rbind() %>% pre_proc_excel(), path = glue("output/tables/{date}_{.y}_tab_format.xlsx", sheet = .y))
)

# Guardar lista con las tablas
rm(list = ls()[!ls() %in% c("all_tabs")])
save.image("output/tables/all_tabs.RData")
