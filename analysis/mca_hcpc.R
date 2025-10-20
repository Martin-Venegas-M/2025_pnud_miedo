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
    factoextra
)

# 2. Cargar datos y funciones ----------------------------------------------------------------------------------------------------------------------------

enusc <- readRDS("input/data/proc/enusc_na_2_recode.RDS")
source("processing/helpers/functions.R")
enusc_svy <- enusc %>% as_survey_design(ids = conglomerado, stata = varstrat, weights = fact_pers_reg)

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Ejecutar código -------------------------------------------------------------------------------------------------------------------------------------

# 3.1 Preparar data --------------------------------------------------------------------------------------------------------------------------------------

# Vector de variables a incluir
rec_vars <- c(
    "emper_ep_pct_rec", "emper_barrio_pct_rec", "emper_casa_pct_rec",
    "perper_delito",
    #"pergen_pais", "pergen_comuna", "pergen_barrio",
    "comper_pct_rec", "comper_gasto_medidas",
    "comgen_per_pct_rec", "comgen_com_pct_rec"
)

# Data para prueba
df <- enusc %>%
    select(rph_id, all_of(rec_vars)) %>%
    mutate(
        across(everything(), ~ if_else(. == 85, NA, .)), # Pasar a NA los no aplica
        across(perper_delito, ~ if_else(. %in% c(4, 5), NA, .)), # Pasar a NA categorías especificas de perper_delito
        across(all_of(rec_vars), ~ sjlabelled::to_label(.)) # Usar las categorías para los factores
    ) %>%
    drop_na()

# Test!

# # Correr mca y plotear
# mca <- MCA(df, graph = FALSE)
# plot_mca(mca)

# # Correr cluster y ploteae
# clust <- FactoMineR::HCPC(mca, nb.clust = 5, consol = FALSE, graph = FALSE)
# plot_cluster(clust)

# # Probar funcción
# test <- mca_hcpc(df, nclass = 5)

# 3.2 Iterar --------------------------------------------------------------------------------------------------------------------------------------------

results_all <- map(
    6:2,
    ~ mca_hcpc(df, n_class = .x)
) %>% set_names(c("class6", "class5", "class4", "class3", "class2"))

# 4. Guardar --------------------------------------------------------------------------------------------------------------------------------------------
rm(list = ls()[!ls() %in% c("results_all")])
save.image(glue("output/models/results_mca_hcpc.RData"))
