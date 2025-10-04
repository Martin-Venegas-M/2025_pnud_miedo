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
            n_insecure = sum(if_else(value %in% success.cats, 1, 0)),
            "{name.var.pct}" := (n_insecure / n_valid) * 100
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
        name.var.pct = "emper_lugares_pct"
    )

# 4. Guardar bbdd --------------------------------------------------------------------------------------------------------------------------------------

saveRDS(enusc, "input/data/proc/enusc_4_new_recode.RDS")
