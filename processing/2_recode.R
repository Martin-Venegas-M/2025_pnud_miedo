#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Recodificación
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
    glue
)

# 2. Load data and functions ----------------------------------------------------------------------------------------------------------------------------

enusc <- readRDS("input/data/proc/enusc_1_select_desc.RDS")
source("processing/helpers/functions.R")

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Ejecutar código --------------------------------------------------------------------------------------------------------------------------------------

# 3.0 General: remover NSNR -------------------------------------------------------------------------------------------------------------------------------

enusc_rec <- reduce(
    c(77, 88, 99, 96), # Códigos a remover
    \(data, code){
        print(glue("Removiendo el código {code} para las siguientes varables:"))
        data %>% mutate(across(matches("emper|perper|pergen|comper|comgen"), ~ if_else(. %in% code, NA, .)))
    },
    .init = enusc
) %>% select(-ends_with("_na"), -ends_with("_ns"), -ends_with("_nr")) # Eliminar columnas sin info

# 3.1 Crear vectores --------------------------------------------------------------------------------------------------------------------------------------

rec_vars <- list(
    emper_transporte = paste0("emper_p_inseg_lugares_", 1:6),
    emper_recreacion = paste0("emper_p_inseg_lugares_", c(7, 10, 12)),
    emper_barrio = c("emper_p_inseg_oscuro_1", "emper_p_inseg_dia_1"),
    emper_casa = c("emper_p_inseg_oscuro_2", "emper_p_inseg_dia_2"),
    perper_delito = list(
        "perper_p_expos_delito",
        paste0("perper_p_delito_pronostico_", c(1, 4, 6, 8:11)),
        paste0("perper_p_delito_pronostico_", c(2, 5, 7))
    ),
    pergen_pais = c("pergen_p_aumento_pais"),
    pergen_comuna = c("pergen_p_aumento_com"),
    pergen_barrio = c("pergen_p_aumento_barrio"),
    comper_vida_cotidiana = paste0("comper_p_mod_actividades_", c(1:2, 8)),
    comper_transporte = paste0("comper_p_mod_actividades_", c(4:6, 13)),
    comper_gasto_medidas = c("comper_costos_medidas"),
    comgen_medidas_per = paste0("comgen_medidas_", c("cerco", "rejas", "proteccion")),
    comgen_medidas_com = paste0("comgen_vecinos_medidas_", c("vigilancia", "al_comunit", "coord_pol", "coord_mun", "televig"))
)

# 3.2 Recodificar ----------------------------------------------------------------------------------------------------------------------------------------

recs <- enusc_rec %>%
    transmute(
        emper_transporte = if_any(rec_vars[["emper_transporte"]], ~ . %in% c(1:2)),
        emper_recreacion = if_any(rec_vars[["emper_recreacion"]], ~ . %in% c(1:2)),
        emper_barrio = if_any(rec_vars[["emper_barrio"]], ~ . %in% c(1:2)),
        emper_casa = if_any(rec_vars[["emper_casa"]], ~ . %in% c(1:2)),
        perper_delito = case_when(
            perper_p_expos_delito == 2 ~ 1, # No cree que será victima de delito
            if_any(rec_vars[["perper_delito"]][[2]], ~ . == 1) ~ 2, # Cree que será victima de un delito no violento
            if_any(rec_vars[["perper_delito"]][[3]], ~ . == 1) ~ 3, # Cree que será victima de un delito violento
            TRUE ~ NA
        ),
        pergen_pais = if_any(rec_vars[["pergen_pais"]], ~ . == 1),
        pergen_comuna = if_any(rec_vars[["pergen_comuna"]], ~ . == 1),
        pergen_barrio = if_any(rec_vars[["pergen_barrio"]], ~ . == 1),
        comper_vida_cotidiana = if_any(rec_vars[["comper_vida_cotidiana"]], ~ . == 1),
        comper_transporte = if_any(rec_vars[["comper_transporte"]], ~ . == 1),
        comper_gasto_medidas = if_any(rec_vars[["comper_gasto_medidas"]], ~ . %in% c(1:5)),
        comgen_medidas_per = if_any(rec_vars[["comgen_medidas_per"]], ~ . == 1),
        comgen_medidas_com = if_any(rec_vars[["comgen_medidas_com"]], ~ . == 1)
    ) %>%
    mutate(
        across(everything(), ~ as.integer(.))
    )

# 3.4 Etiquetar --------------------------------------------------------------------------------------------------------------------------------------------

# Crear etiquetas de variables
etiquetas_variables <- c(
    "Inseguridad en Transporte"                          = "emper_transporte",
    "Inseguridad en Recreación"                          = "emper_recreacion",
    "Inseguridad en Barrio"                              = "emper_barrio",
    "Inseguridad en Casa"                                = "emper_casa",
    "Expectativa de ser victima delito"                  = "perper_delito",
    "Aumento delincuencia en el país"                    = "pergen_pais",
    "Aumento delincuencia en el comuna"                  = "pergen_comuna",
    "Aumento delincuencia en el barrio"                  = "pergen_barrio",
    "Modifica comportamiento en vida cotidiana"          = "comper_vida_cotidiana",
    "Modifica comportamiento en transporte"              = "comper_transporte",
    "Gasta en medidas de seguridad"                      = "comper_gasto_medidas",
    "Dispone de medidas de seguridad (personales)"       = "comgen_medidas_per",
    "Disponen de medidas de seguridad (comunitarias)"    = "comgen_medidas_com"
)

# Aplicar etiquetas variables
recs <- reduce2(
    unname(etiquetas_variables),
    names(etiquetas_variables),
    \(data, var, etiqueta) data %>%
        mutate("{var}" := set_label(.data[[var]], label = etiqueta)),
    .init = recs
)

etiquetas_valores <- list(
    "emper_transporte"                 = c("Muy inseguro/Inseguro en Transporte" = 1, "Muy seguro/Seguro en Transporte" = 0),
    "emper_recreacion"                 = c("Muy inseguro/Inseguro en Recreación" = 1, "Muy seguro/Seguro en Recreación" = 0),
    "emper_barrio"                     = c("Muy inseguro/Inseguro en el Barrio" = 1, "Muy seguro/Seguro en el Barrio" = 0),
    "emper_casa"                       = c("Muy inseguro/Inseguro en la Casa" = 1, "Muy seguro/Seguro en Casa" = 0),
    "perper_delito"                    = c("No cree que será victima de delito" = 1, "Cree que será victima de delito no violento" = 2, "Cree que será victima de delito violento" = 3),
    "pergen_pais"                      = c("Aumentó delincuencia en País" = 1, "Se mantuvo/Disminuyó delincuencia en País" = 0),
    "pergen_comuna"                    = c("Aumentó delincuencia en Comuna" = 1, "Se mantuvo/Disminuyó delincuencia en Comuna" = 0),
    "pergen_barrio"                    = c("Aumentó delincuencia en Barrio" = 1, "Se mantuvo/Disminuyó delincuencia en Barrio" = 0),
    "comper_vida_cotidiana"            = c("Modifica comportamiento en Vida Cotidiana" = 1, "No modifica comportamiento en Vida Cotidiana" = 0),
    "comper_transporte"                = c("Modifica comportamiento en Transporte" = 1, "No modifica comportamiento en Transporte" = 0),
    "comper_gasto_medidas"             = c("Gasta en medidas de seguridad" = 1, "No gasta en medidas de seguridad" = 0),
    "comgen_medidas_per"               = c("Dispone de medidas personales" = 1, "No dispone de medidas personales" = 0),
    "comgen_medidas_com"               = c("Dispone de medidas comunitarias" = 1, "No dispone de medidas comunitarias" = 0)
)

# Aplicar etiquetas valores
recs <- reduce2(
    names(etiquetas_valores),
    etiquetas_valores,
    \(data, var, etiqueta) data %>% mutate("{var}" := set_labels(.data[[var]], labels = etiqueta)),
    .init = recs
)

# 3.5 Join -------------------------------------------------------------------------------------------------------------------------------------------------
enusc <- enusc %>% bind_cols(recs)

# 4. Guardar bbdd ------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(enusc, "input/data/proc/enusc_2_recode_tidy.RDS")
