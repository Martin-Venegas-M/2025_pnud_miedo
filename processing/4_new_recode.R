#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: Recodificación
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para la nueva propuesta de recodificación
# Fecha: 4 de octubre de 2025
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

enusc <- readRDS("input/data/proc/enusc_3_add_vars.RDS")

# Cargar labels
source("processing/helpers/labels_new.R")

# 3. Ejecutar código --------------------------------------------------------------------------------------------------------------------------------------

# 3.1 Crear función ---------------------------------------------------------------------------------------------------------------------------------------

# Crear variable de porcentaje de items con cierta categoría por total de validos
create_var_pct <- function(
    data,
    id.col = rph_id,
    success.cats, # Categorías de las variables fuente que se consideran "exito". P.ej: c(1, 2) -> 1 = Muy inseguro y 2 = Inseguro
    source.cols, # Variables fuente para construir la variable nueva
    name.var.pct, # Nombre de la variable nuueva
    output = c("data", "details", "insumo", "all")) {
    # Match args
    output <- match.arg(output)

    # Crear tabla long para hacer los calculos
    details <- data %>%
        select({{ id.col }}, {{ source.cols }}) %>%
        pivot_longer(
            cols = {{ source.cols }},
            names_to = "variable",
            values_to = "value"
        ) %>%
        group_by({{ id.col }}) %>%
        mutate(
            not_valid = sum(if_else(value == 85, 1, 0)),
            n_valid = n() - not_valid,
            n_success = sum(if_else(value %in% success.cats, 1, 0)),
            "{name.var.pct}" := (n_success / n_valid) * 100
        ) %>%
        ungroup() %>%
        select(-not_valid)

    # Guardar variable en insumo wide
    insumo <- details %>%
        select({{ id.col }}, {{ name.var.pct }}) %>%
        distinct({{ id.col }}, .keep_all = T)

    # Añadir a data
    data <- data %>% left_join(insumo)

    # Retornar!
    if (output == "data") {
        return(data)
    } else if (output == "details") {
        return(details)
    } else if (output == "insumo") {
        return(insumo)
    } else if (output == "all") {
        return(list(data = data, details = details, insumo = insumo))
    }
}

# 3.2 Crear variables ---------------------------------------------------------------------------------------------------------------------------------

enusc <- enusc %>%
    create_var_pct(
        success.cats = c(1, 2),
        source.cols = starts_with("emper_p_inseg_lugares"),
        name.var.pct = "emper_pct"
    ) %>%
    create_var_pct(
        success.cats = c(1, 2),
        source.cols = paste0("emper_p_inseg_lugares_", 1:6),
        name.var.pct = "emper_transporte_pct"
    ) %>%
    create_var_pct(
        success.cats = c(1, 2),
        source.cols = paste0("emper_p_inseg_lugares_", c(7, 10, 12)),
        name.var.pct = "emper_recreacion_pct"
    ) %>%
    mutate(
        perper_delito_new = case_when(
            perper_p_expos_delito == 2 ~ 1, # No cree que será victima de delito
            if_any(glue("perper_p_delito_pronostico_{c(1:4, 6, 9:11)}"), ~ . == 1) ~ 2, # Cree que será victima de un delito no violento
            if_any(glue("perper_p_delito_pronostico_{c(5, 7:8)}"), ~ . == 1) ~ 3, # Cree que será victima de un delito violento
            perper_p_expos_delito %in% c(88, 99) ~ 4, # No sabe/No responde si cree que será victima de delito
            if_any(glue("perper_p_delito_pronostico_{c(77, 88, 99)}"), ~ . == 1) ~ 5 # No sabe/No responde de qué delito será victima / Cree que será victima de otro tipo de delito
        )
    ) %>%
    create_var_pct(
        success.cats = 1,
        source.cols = starts_with("comper_p_mod_actividades"),
        name.var.pct = "comper_pct"
    ) %>%
    create_var_pct(
        success.cats = 1,
        source.cols = paste0("comper_p_mod_actividades_", c(1:2, 8)),
        name.var.pct = "comper_vida_cotidiana_pct"
    ) %>%
    create_var_pct(
        success.cats = 1,
        source.cols = paste0("comper_p_mod_actividades_", c(4:6, 13)),
        name.var.pct = "comper_transporte_pct"
    ) %>%
    mutate(
        across(ends_with("_pct"), ~ if_else(. > 50, 1, 0), .names = "{.col}_rec")
    )

# 3.3 Etiquetar ----------------------------------------------------------------------------------------------------------------------------------------

enusc <- reduce2(
    unname(etiquetas_variables),
    names(etiquetas_variables),
    \(data, var, etiqueta) data %>%
        mutate("{var}" := set_label(.data[[var]], label = etiqueta)),
    .init = enusc
)

# Aplicar etiquetas valores
enusc <- reduce2(
    names(etiquetas_valores),
    etiquetas_valores,
    \(data, var, etiquetas) data %>% mutate("{var}" := set_labels(.data[[var]], labels = etiquetas)),
    .init = enusc
)

# Check
sjmisc::frq(enusc$emper_pct_rec)
sjmisc::frq(enusc$emper_transporte_pct_rec)
sjmisc::frq(enusc$emper_recreacion_pct_rec)

sjmisc::frq(enusc$comper_pct_rec)
sjmisc::frq(enusc$comper_vida_cotidiana_pct_rec)
sjmisc::frq(enusc$comper_transporte_pct_rec)

# 4. Guardar bbdd --------------------------------------------------------------------------------------------------------------------------------------

saveRDS(enusc, "input/data/proc/enusc_4_new_recode.RDS")
