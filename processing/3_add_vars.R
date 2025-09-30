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

# Add nclust variable to the dataset
add_clust <- function(data, nclust) {
    df_clust <- results_all[[glue("class{nclust}")]]$data %>% select(all_of(c("rph_id", glue("clusters_{nclust}"))))

    data <- data %>%
        left_join(df_clust) %>%
        mutate(across(glue("clusters_{nclust}"), ~ factor(., levels = c(1:nclust), labels = paste0("Cluster ", 1:nclust))))

    return(data)
}

# Iterate function
enusc <- reduce(
    2:5,
    \(data, nclust) add_clust(data, nclust),
    .init = enusc
)

# 4. Guardar bbdd -----------------------------------------------------------------------------------------------------------------------------------------
saveRDS(enusc, "input/data/proc/enusc_3_add_vars.RDS")
