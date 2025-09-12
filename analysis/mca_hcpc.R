#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: MCA de variables recodificadas
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para un análisis de correspondencias multiples
# Fecha: 8 de septiembre de 2025
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
    scales,
    FactoMineR,
    factoextra,
    visdat
)

# 2. Cargar datos y funciones ----------------------------------------------------------------------------------------------------------------------------

enusc <- readRDS("input/data/proc/enusc_2_recode.RDS")
source("processing/helpers/functions.R")
enusc_svy <- enusc %>% as_survey_design(ids = conglomerado, stata = varstrat, weights = fact_pers_reg)

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Ejecutar código -------------------------------------------------------------------------------------------------------------------------------------

# Vector de variables a incluir
rec_vars <- enusc %>%
    select(emper_transporte:comgen_vecinos_adopta_medidas) %>%
    names()

# Data para prueba
data <- enusc %>%
    filter(perper_p_expos_delito == 1) %>%
    select(all_of(rec_vars)) %>%
    mutate(across(everything(), ~ sjlabelled::to_label(.)))

# Ver casos perdidos
visdat::vis_miss(data)

# Correr y plotear mca
mca <- MCA(data, graph = FALSE)
plot_mca(mca)
