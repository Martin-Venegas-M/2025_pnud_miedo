#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: Recodificación
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para la recodificación de las variables principales
# Date: 12 de septiembre de 2025
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

enusc <- readRDS("input/data/proc/enusc_1_select_desc.RDS")
source("processing/helpers/functions.R")

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# Cargar labels
source("processing/helpers/labels.R")

# 3. Ejecutar código --------------------------------------------------------------------------------------------------------------------------------------

# 3.1 Crear insumo --------------------------------------------------------------------------------------------------------------------------------------
#* NOTA: Este insumo contiene los vectores de variables que se utilizan en la creación de las variables recodificadas
rec_vars <- list(
    emper_transporte = paste0("emper_p_inseg_lugares_", 1:6),
    emper_recreacion = paste0("emper_p_inseg_lugares_", c(7, 10, 12)),
    emper_barrio = c("emper_p_inseg_oscuro_1", "emper_p_inseg_dia_1"),
    emper_casa = c("emper_p_inseg_oscuro_2", "emper_p_inseg_dia_2"),
    perper_delito = list(
        "perper_p_expos_delito",
        paste0("perper_p_delito_pronostico_", c(1, 3:4, 6, 8:11)),
        paste0("perper_p_delito_pronostico_", c(2, 5, 7)),
        "perper_p_expos_delito",
        paste0("perper_p_delito_pronostico_", c(77, 88, 99))
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

#* NOTA TÉCNICA:
#* Debido a la naturaleza de la variable perper_delito, el elemento rec_vars[["perper_delito"]]] corresponde a una lista de
#* strings/vectores, en vez de solo un vector (como las demás variables). En esta sub-lista ( rec_vars[["perper_delito"]]] ),
#* cada elemento corresponden a las variables que se utilizarán para recodificar cada categoría del case_when(). Por ejemplo,
#* el elemento 1 ( rec_vars[["perper_delito"]][[1]] ) corresponde a perper_p_expos_delito. Esta es la variable que permitirá
#* generar la categoría "1. No cree que será victima de delito", cuando perper_p_expos_delito == 2.

# 3.2 Recodificar ----------------------------------------------------------------------------------------------------------------------------------------
enusc <- enusc %>%
    mutate(
        emper_transporte = if_any(rec_vars[["emper_transporte"]], ~ . %in% c(1:2)),
        emper_recreacion = if_any(rec_vars[["emper_recreacion"]], ~ . %in% c(1:2)),
        emper_barrio = if_any(rec_vars[["emper_barrio"]], ~ . %in% c(1:2)),
        emper_casa = if_any(rec_vars[["emper_casa"]], ~ . %in% c(1:2)),
        perper_delito = case_when(
            if_all(rec_vars[["perper_delito"]][[1]], ~ . == 2) ~ 1, # No cree que será victima de delito
            if_any(rec_vars[["perper_delito"]][[2]], ~ . == 1) ~ 2, # Cree que será victima de un delito no violento
            if_any(rec_vars[["perper_delito"]][[3]], ~ . == 1) ~ 3, # Cree que será victima de un delito violento
            if_all(rec_vars[["perper_delito"]][[4]], ~ . %in% c(88, 99)) ~ 4, # No sabe/No responde si cree que será victima de delito
            if_any(rec_vars[["perper_delito"]][[5]], ~ . == 1) ~ 5, # No sabe/No responde de qué victima será victima / Cree que será vicrtima de otro tipo de delito
            TRUE ~ NA
        ),
        pergen_pais = if_all(rec_vars[["pergen_pais"]], ~ . == 1),
        pergen_comuna = if_all(rec_vars[["pergen_comuna"]], ~ . == 1),
        pergen_barrio = if_all(rec_vars[["pergen_barrio"]], ~ . == 1),
        comper_vida_cotidiana = if_any(rec_vars[["comper_vida_cotidiana"]], ~ . == 1),
        comper_transporte = if_any(rec_vars[["comper_transporte"]], ~ . == 1),
        comper_gasto_medidas = case_when(
            if_all(rec_vars[["comper_gasto_medidas"]], ~ . %in% c(1:5)) ~ 1,
            if_all(rec_vars[["comper_gasto_medidas"]], ~ . == 85) ~ 0,
            if_all(rec_vars[["comper_gasto_medidas"]], ~ . == 88) ~ 88,
            if_all(rec_vars[["comper_gasto_medidas"]], ~ . == 99) ~ 99,
            TRUE ~ NA
        ),
        comgen_medidas_per = if_any(rec_vars[["comgen_medidas_per"]], ~ . == 1),
        comgen_medidas_com = if_any(rec_vars[["comgen_medidas_com"]], ~ . == 1)
    ) %>%
    mutate(
        across(where(is.logical), ~ as.numeric(.)) # Pasar de TRUE/FALSE a 1/0
    )

#* NOTA TÉCNICA:
#* Para las variables de pergen_pais, pergen_comuna, pergen_comuna y comper_gasto_medidas no es estrictamente necesario utilizar
#* la estrategia de if_all() + rec_vars[[]], ya que para la creación de estas variables recodificadas se utiliza solo una variable
#* del cuestionario (en vez de un conjunto de variables). Por ejemplo, para crear comper_gastos_medidas solo se utiliza comper_costos_medidas.
#* Sin embargo, para estas variables preferí mantener la estrategia por consistencia, de tal manera que el código de recodificación no incluya
#* variables del cuestionario y solo se haga referencia al insumo.

# ! IMPORTANTE La nota anterior también aplica para las categorías 1 y 4 de la variable perper_delito.

# 3.3 Imputar 85, 88 y 99 --------------------------------------------------------------------------------------------------------------------------------

# Excluir variables de la imputación
rec_vars_torec <- rec_vars[!names(rec_vars) %in% c("perper_delito", "comper_gasto_medidas")]

#* NOTAS:
#* perper_delito tiene su propio manejo explicito de los NS/NR, no es necesario incluirlo en esta imputación.
#* comper_gasto_medidas tiene un sentido sustantivo para el 85, por lo que el NS/NR se manejó explicitamente en la recodificación

# Imputar!
enusc <- reduce2(
    names(rec_vars_torec),
    rec_vars_torec,
    \(data, rec_var, dim_vars) {
        data %>%
            mutate("{rec_var}" := case_when(
                if_all(all_of(dim_vars), ~ . == 85) ~ 85,
                if_all(all_of(dim_vars), ~ . == 88) ~ 88,
                if_all(all_of(dim_vars), ~ . == 99) ~ 99,
                TRUE ~ .data[[rec_var]]
            ))
    },
    .init = enusc
)

# 3.4 Etiquetar --------------------------------------------------------------------------------------------------------------------------------------------

# Aplicar etiquetas variables
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

# 3.4 Generar output de recode -----------------------------------------------------------------------------------------------------------------------------

# Crear string de variables fuente
vars_fuente <- map(
    seq_along(rec_vars),
    \(i) paste(rec_vars[[i]], collapse = "; ")
) %>% set_names(names(rec_vars))

# Cambiar manualmente string de variables para perper_delito
vars_fuente[["perper_delito"]] <- paste(rec_vars[["perper_delito"]], collapse = "; ")

metadata_recode <- tibble(
    variable_recodificada = names(rec_vars),
    variables_fuente = as.character(vars_fuente)
)

# 4. Guardar bbdd ------------------------------------------------------------------------------------------------------------------------------------------
saveRDS(enusc, "input/data/proc/enusc_2_recode.RDS")

# Guardar versión con recodificación de NA
enusc_na <- reduce(
    c(88, 99), # Códigos a remover
    \(data, code){
        print(glue("Removiendo el código {code} para las siguientes varables:"))
        data %>% mutate(across(matches("emper|perper|pergen|comper|comgen"), ~ if_else(. %in% code, NA, .)))
    },
    .init = enusc
)

saveRDS(enusc_na, "input/data/proc/enusc_na_2_recode.RDS")

# Guardar metadata
writexl::write_xlsx(metadata_recode, "output/metadata_recode.xlsx")
