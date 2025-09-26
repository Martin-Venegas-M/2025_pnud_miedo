#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: Recodificación
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para añadir1
# Fecha: 26 de septiembre de 2025
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
    glue
)

# 2. Cargar datos y funciones ----------------------------------------------------------------------------------------------------------------------------
enusc <- readRDS("input/data/proc/enusc_2_recode.RDS")
load("output/models/results_mca_hcpc.RData")

# 3. Ejecutar código -------------------------------------------------------------------------------------------------------------------------------------

df_clust <- results_all$class5$data %>% select(rph_id, clusters_5)

enusc <- enusc %>% 
    left_join(df_clust) %>% 
    mutate(
    clusters_5 = factor(
        clusters_5,
        levels = c(1:5),
        labels = paste0("Cluster ", 1:5)
        )
)

sjmisc::frq(enusc$clusters_5)

# 4. Guardar bbdd -----------------------------------------------------------------------------------------------------------------------------------------
saveRDS(enusc, "input/data/proc/enusc_3_add_vars.RDS")
