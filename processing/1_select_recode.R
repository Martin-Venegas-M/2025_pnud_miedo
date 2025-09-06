#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Selección y recodificación
# Institution: PNUD
# Responsable: Consultor técnico - MVM
# Executive Summary: Este script contiene el código para un procesamiento inicial de los datos
# Date: August 18, 2025
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
    survey,
    srvyr
)

# 2. Load data and functions ----------------------------------------------------------------------------------------------------------------------------

# enusc_original <- read_sav("input/data/original/base-de-datos---enusc-2024.sav")
# saveRDS(enusc_original, "input/data/original/base-de-datos---enusc-2024.RDS")

enusc_original <- readRDS("input/data/original/base-de-datos---enusc-2024.RDS")
source("processing/helpers/functions.R")

# 3. Select variables -----------------------------------------------------------------------------------------------------------------------------------

# DIMENSIONES
# 1a. EMOCIONAL- PERSONAL (emper) = P_EXPOS_DELITO (P4), P_INSEG (P3) -> Contextos en los que las personas se sienten inseguras
# 1b EMOCIONAL- GENERAL (emgen) = ?

# 2a. COGNITIVA/PERCEPCTUAL- PERSONAL (cogper) = P_EXPOS_DELITO (P7), P_DELITO_PRONOSTICO (P8), P_AUMENTO (P1) -> Percepción de la probabilidad de ser victima de delito o de aumentos de delito
# 2b. COGNITIVA/PERCEPTUAL - GENERAL (coggen) = ?

# 3a. COMPORTAMIENTO - PERSONAL (comper) = P_MOD_ACTIVIDADES (P9), COSTOS_MEDIDAS (MDC6)
# 3b. COMPORTAMIENTO - GENERAL (comgen) =  ANTIG_SECTOR (MDC1), MEDIDAS (MDC2), ADOPTADAS (MDC3), VECINOS (MDC4),VECINOS_ADOPTADAS (MDC5)

emper <- "P_INSEG"
cogper <- "P_EXPOS_DELITO|P_DELITO_PRONOSTICO|P_AUMENTO"
comper <- "P_MOD_ACTIVIDADES|COSTO_MEDIDAS"
comgen <- "MEDIDAS|ADOPTADAS|VECINOS_MEDIDAS|VECINOS_ADOPTADAS"

enusc <- enusc_original %>%
    filter(Kish == 1) %>% # ! IMPORTANTE
    select(
        rph_ID, idhogar, enc_region, Conglomerado, VarStrat, starts_with("Fact"),
        matches(emper), matches(cogper), matches(comper), matches(comgen),
        starts_with("rph")
    ) %>%
    rename_with(~ glue("emper_{.x}"), matches(emper)) %>%
    rename_with(~ glue("cogper_{.x}"), matches(cogper)) %>%
    rename_with(~ glue("comper_{.x}"), matches(comper)) %>%
    rename_with(~ glue("comgen_{.x}"), matches(comgen)) %>%
    clean_names()

# 4. Descriptivos iniciales ------------------------------------------------------------------------------------------------------------------------------

# Crear objeto encuesta
enusc_svy <- enusc %>%
    as_survey_design(ids = conglomerado, stata = varstrat, weights = fact_pers_reg)

# Probar funciones
tab_frq1(var = emper_p_inseg_lugares_1, verbose = TRUE, sort.frq = "desc")
tab_frq2(var = emper_p_inseg_lugares_1, verbose = TRUE, vartype = c("se", "ci"))
