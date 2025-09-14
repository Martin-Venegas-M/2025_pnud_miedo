#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: Descriptivos de variables recodificadas
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para la generación de descriptivos de las variables recodificadas
# Fecha: 14 de septiembre de 2025
#******************************************************************************************************************************************************

rm(list = ls())

# 1. Cargar paquetes ------------------------------------------------------------------------------------------------------------------------------------
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

# 2. Cargar datos y funciones ----------------------------------------------------------------------------------------------------------------------------

enusc <- readRDS("input/data/proc/enusc_2_recode.RDS")
source("processing/helpers/functions.R")
enusc_svy <- enusc %>% as_survey_design(ids = conglomerado, stata = varstrat, weights = fact_pers_reg)

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Ejecutar código -------------------------------------------------------------------------------------------------------------------------------------

# Crear vector con nombres de variables recodificadas
rec_vars <- enusc %>%
    select(emper_transporte:comgen_medidas_com) %>%
    names()

# Crear tabla con resultados
results <- map(rec_vars, ~ tab_frq1(var = !!sym(.x), verbose = FALSE)) %>%
    list_rbind() %>%
    pre_proc_excel()

results_unw <- map(rec_vars, ~ tab_frq1(var = !!sym(.x), w = NULL, verbose = FALSE)) %>%
    list_rbind()

# 4. Guardar --------------------------------------------------------------------------------------------------------------------------------------------

# Formatear y guardar tablas
format_tab_excel(results, sheet = "Recodificadas ponderadas", save = TRUE, path = glue("output/tables/{date}_rec_vars_tabs.xlsx"))
format_tab_excel(results_unw, sheet = "Recodificadas muestrales", save = TRUE, path = glue("output/tables/{date}_rec_vars_tabs_unw.xlsx"))
