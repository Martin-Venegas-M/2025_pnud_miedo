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

# Cargar labels
source("processing/helpers/labels.R")

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

# 3.1 Crear funciones de utilidad  -----------------------------------------------------------------------------------------------------------------------
# Función para crear expresiones desde texto...
gen_expr <- function(var, n, operator = "==", val = "1", return_all = FALSE) {
    # Guardar vector de variables a usar en la recodificación
    vector <- glue("{var}_{n}") %>% as.character()

    # Guardar expresión para recodificar
    expr <- glue("{var}_{n} {operator} {val}") %>%
        paste(., collapse = " | ") %>%
        parse_expr()

    if (return_all) {
        all <- list(expr = expr, vector = vector)
        return(all)
    } else {
        return(expr)
    }
}

# 3.2 Crear expresiones ----------------------------------------------------------------------------------------------------------------------------------

# Lugar donde se siene inseguridad
expr_transporte <- gen_expr("emper_p_inseg_lugares", 1:6, "%in%", "c(1:2)")
expr_recreacion <- gen_expr("emper_p_inseg_lugares", c(7, 10, 12), "%in%", "c(1:2)")

# Probabilidad ser victima de delito
expr_delito_no_violento <- gen_expr("perper_p_delito_pronostico", c(1, 4, 6, 8:11))
expr_delito_violento <- gen_expr("perper_p_delito_pronostico", c(2, 5, 7))

# Dejar de hacer cosas
expr_vida_cotidiana <- gen_expr("comper_p_mod_actividades", c(1:2, 8))
expr_transporte2 <- gen_expr("comper_p_mod_actividades", c(4:6, 13))

# Medidas (personales y comunitarias)
expr_medidas_per <- gen_expr("comgen_medidas", c("cerco", "rejas", "proteccion"))
expr_medidas_comun <- gen_expr("comgen_vecinos_medidas", c("vigilancia", "al_comunit", "coord_pol", "coord_mun", "televig"))

# 3.3 Recodificar ----------------------------------------------------------------------------------------------------------------------------------------

recs <- enusc_rec %>% transmute(
    emper_transporte = if_else(!!expr_transporte, 1, 0),
    emper_recreacion = if_else(!!expr_recreacion, 1, 0),
    emper_barrio = if_else(emper_p_inseg_oscuro_1 %in% c(1:2) | emper_p_inseg_dia_1 %in% c(1:2), 1, 0),
    emper_casa = if_else(emper_p_inseg_oscuro_2 %in% c(1:2) | emper_p_inseg_dia_2 %in% c(1:2), 1, 0),
    perper_delito = case_when(
        perper_p_expos_delito == 2 ~ 1, # No cree que será victima de delito
        !!expr_delito_no_violento ~ 2, # Cree que será victima de un delito no violento
        !!expr_delito_violento ~ 3, # Cree que será victima de un delito violento
        TRUE ~ NA
    ),
    pergen_pais = if_else(pergen_p_aumento_pais == 1, 1, 0),
    pergen_comuna = if_else(pergen_p_aumento_com == 1, 1, 0),
    pergen_barrio = if_else(pergen_p_aumento_barrio == 1, 1, 0),
    comper_vida_cotidiana = if_else(!!expr_vida_cotidiana, 1, 0),
    comper_transporte = if_else(!!expr_transporte2, 1, 0),
    comper_gasto_medidas = if_else(comper_costos_medidas %in% c(1:5), 1, 0),
    comgen_medidas_per = if_else(!!expr_medidas_per, 1, 0),
    comgen_medidas_com = if_else(!!expr_medidas_comun, 1, 0)
)

rm(list = ls(pattern = "^expr"))

# 3.4 Etiquetar --------------------------------------------------------------------------------------------------------------------------------------------

# Aplicar etiquetas variables
recs <- reduce2(
    unname(etiquetas_variables),
    names(etiquetas_variables),
    \(data, var, etiqueta) data %>%
        mutate("{var}" := set_label(.data[[var]], label = etiqueta)),
    .init = recs
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
saveRDS(enusc, "input/data/proc/enusc_2_recode_rlang.RDS")
