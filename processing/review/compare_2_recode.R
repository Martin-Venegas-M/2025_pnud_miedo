#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: Comparación de estrategias de recodificación
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para comparar las estrategias utilizadas para recodificar las variables
# Date: 12 de septiembre de 2025
#******************************************************************************************************************************************************

#! ################################################################ DEPRECATED ########################################################################
#! Esta replicación queda obsoleta al hacer el manejo explicito de los NS/NR y 85 en 2_recode.R
#! ####################################################################################################################################################

rm(list = ls())

# 1. Cargar paquetes ----------------------------------------------------------------------------------------------------------------------------------
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

# 2. Cargar datos y funciones ---------------------------------------------------------------------------------------------------------------------------
enusc <- readRDS("input/data/proc/enusc_2_recode.RDS")
enusc_rlang <- readRDS("input/data/proc/enusc_2_recode_rlang.RDS")

# 3. Ejecutar código ------------------------------------------------------------------------------------------------------------------------------------

# Comparar
all.equal(enusc, enusc_rlang) #* TRUE
