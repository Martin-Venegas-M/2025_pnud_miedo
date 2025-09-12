#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Descriptivos de variables recodificadas
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
    glue,
    srvyr,
    openxlsx,
    scales
)

# 2. Load data and functions ----------------------------------------------------------------------------------------------------------------------------

enusc <- readRDS("input/data/proc/enusc_2_recode.RDS")
source("processing/helpers/functions.R")
enusc_svy <- enusc %>% as_survey_design(ids = conglomerado, stata = varstrat, weights = fact_pers_reg)

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Crear tablas ---------------------------------------------------------------------------------------------------------------------------------------

# Crear vector con nombres de variables recodificadas
vars_rec <- enusc %>%
    select(emper_transporte:comgen_medidas_com) %>%
    names()

# Crear tabla con resultados
results <- map(vars_rec, ~ tab_frq1(var = !!sym(.x), verbose = FALSE)) %>%
    list_rbind() %>%
    pre_proc_excel()

results_unweighted <- map(vars_rec, ~ tab_frq1(var = !!sym(.x), w = NULL, verbose = FALSE)) %>%
    list_rbind()

# Formatear y guardar tablas
format_tab_excel(results, path = glue("output/tables/{date}_vars_rec_tab_format.xlsx", sheet = .y))
format_tab_excel(results_unweighted, path = glue("output/tables/{date}_vars_rec_tab_format_unweighted.xlsx", sheet = .y))
