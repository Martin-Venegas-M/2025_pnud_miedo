#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Title: Recodificación
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
        data %>% mutate(across(matches("emper|perper|comper|comgen"), ~ if_else(. %in% code, NA, .)))
    },
    .init = enusc
) %>% select(-ends_with("_na"), -ends_with("_ns"), -ends_with("_nr")) # Eliminar columnas sin info

# 3.1 Crear funciones de utilidad  -----------------------------------------------------------------------------------------------------------------------
# Función para crear expresiones desde texto...
gen_expr <- function(var, n, operator = "==", val = "1") {
    glue("{var}_{n} {operator} {val}") %>%
        paste(., collapse = " | ") %>%
        parse_expr()
}

# Extraer lo que está después del "_"
sust <- function(x) str_extract(x, "(?<=_).*")

# 3.2 Crear expresiones ----------------------------------------------------------------------------------------------------------------------------------

# Lugar donde se siene inseguridad
expr_transporte <- gen_expr("emper_p_inseg_lugares", 1:6, "%in%", "c(1:2)")
expr_recreacion <- gen_expr("emper_p_inseg_lugares", c(7, 10, 12), "%in%", "c(1:2)")

# Probabilidad ser vitima de delito
expr_delito <- gen_expr("perper_p_delito_pronostico", c(1:11))
expr_delito_violento <- gen_expr("perper_p_delito_pronostico", c(2, 5, 7))
expr_delito_no_violento <- gen_expr("perper_p_delito_pronostico", c(1:2, 4, 6, 8:11))

# Dejar de hacer cosas
expr_vida_cotidiana <- gen_expr("comper_p_mod_actividades", c(1:3, 7:8))
expr_transporte2 <- gen_expr("comper_p_mod_actividades", c(4:6, 13))

# Medidas (alarmas y camaras)
expr_medidas_alarm_cam <- gen_expr("comgen_medidas", c("alarma_privada", "camaras_vigilancia"))
expr_vecinos_medidas_alarm_cam <- gen_expr("comgen_vecinos_medidas", c("al_comunit", "televig"))

# ! NOTA: Ahora que ya terminé de aplicar esta estrategia, me doy cuenta que era más tidyverse-friendly crear los vectores de las variables dinamicamente
# ! con if_any(). De todos modos igual me gusta la estrategia, siento que queda claro el procedimiento :).

# 3.3 Recodificar ----------------------------------------------------------------------------------------------------------------------------------------

recs <- enusc_rec %>% transmute(
    # EMPER
    emper_transporte = if_else(!!expr_transporte, 1, 0),
    emper_recreacion = if_else(!!expr_recreacion, 1, 0),
    emper_barrio = if_else(emper_p_inseg_oscuro_1 %in% c(1:2) | emper_p_inseg_dia_1 %in% c(1:2), 1, 0),
    emper_casa = if_else(emper_p_inseg_oscuro_2 %in% c(1:2) | emper_p_inseg_dia_2 %in% c(1:2), 1, 0),
    # PERPER
    perper_delito = if_else(!!expr_delito, 1, 0),
    perper_delito_violento = if_else(!!expr_delito_violento, 1, 0),
    perper_delito_no_violento = if_else(!!expr_delito_no_violento, 1, 0),
    # PERGEN
    # ! CAMBIAR AQUI perper por pergen
    pergen_pais = if_else(pergen_p_aumento_pais == 1, 1, 0),
    pergen_comuna = if_else(pergen_p_aumento_com == 1, 1, 0),
    pergen_barrio = if_else(pergen_p_aumento_barrio == 1, 1, 0),
    # COMPER
    comper_vida_cotidiana = if_else(!!expr_vida_cotidiana, 1, 0),
    comper_transporte = if_else(!!expr_transporte2, 1, 0),
    comper_gasto_medidas = if_else(comper_costos_medidas %in% c(1:5), 1, 0),
    # COMGEN
    # Personales
    comgen_medidas_alarm_cam = if_else(!!expr_medidas_alarm_cam, 1, 0),
    comgen_adopta_medidas = if_any(starts_with("comgen_adoptadas"), ~ . == 1) %>% as.numeric(),
    # Comunitarias
    comgen_vecinos_medidas_alarm_cam = if_else(!!expr_vecinos_medidas_alarm_cam, 1, 0),
    comgen_vecinos_adopta_medidas = if_any(starts_with("comgen_vecinos_adoptadas"), ~ . == 1) %>% as.numeric()
) %>%
    # Recodificaciones de las variables
    mutate(
        # EMPER
        across(starts_with("emper"), ~ set_label(., c(glue("Inseguridad en {sust(cur_column())}")))),
        # PERPER
        perper_delito = set_label(perper_delito, "Probabilidad victima delito"),
        perper_delito_violento = set_label(perper_delito_violento, "Probabilidad victima delito violento"),
        perper_delito_no_violento = set_label(perper_delito, "Probabilidad victima delito no violento"),
        # PERGEN
        across(starts_with("pergen"), ~ set_label(., c(glue("Aumento delincuencia en {sust(cur_column())}")))),
        # COMPER
        comper_vida_cotidiana = set_label(comper_vida_cotidiana, "Modifica comportamiento en vida cotidiana"),
        comper_transporte = set_label(comper_transporte, "Modifica comportamiento en transporte"),
        comper_gasto_medidas = set_label(comper_gasto_medidas, "Gasta en medidas de seguridad"),
        # COMEGEN
        comgen_medidas_alarm_cam = set_label(comgen_medidas_alarm_cam, "Dispone de alarmas o camaras"),
        comgen_adopta_medidas = set_label(comgen_adopta_medidas, "Adopta alguna medida de seguridad"),
        comgen_vecinos_medidas_alarm_cam = set_label(comgen_vecinos_medidas_alarm_cam, "Vecinos disponen de alarmas y camaras comunitarias"),
        comgen_vecinos_adopta_medidas = set_label(comgen_vecinos_adopta_medidas, "Vecinos adoptan alguna medida")
    ) %>%
    # Recodificaciones de los valores
    mutate(
        across(everything(), ~ set_labels(., labels = c("Sí" = 1, "No" = 0)))
    )

rm(list = ls(pattern = "^expr"))

# 3.4 Join -------------------------------------------------------------------------------------------------------------------------------------------------
enusc <- enusc %>% bind_cols(recs)

# 4. Guardar bbdd ------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(enusc, "input/data/proc/enusc_2_recode.RDS")
