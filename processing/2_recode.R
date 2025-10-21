#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: Recodificación
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para la recodificación de las variables principales
# Fecha: 20 de octubre de 2025
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
    openxlsx
)

# 2. Cargar datos y funciones ----------------------------------------------------------------------------------------------------------------------------

enusc <- readRDS("input/data/proc/enusc_1_select_desc.RDS") %>%
    select(-contains("_adoptadas_")) # Eliminar columnas que no usaremos (Medidas personales y comunitarias adoptadas en los últimos 12 meses)

source("processing/helpers/functions.R")

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# Cargar labels
source("processing/helpers/labels.R")

# 3. Ejecutar código --------------------------------------------------------------------------------------------------------------------------------------

## Replicar
# Sacar 2 ítemes de barrio de otra escala
# incluir gasto?

# sacando gasto


# 3.1 Crear insumo --------------------------------------------------------------------------------------------------------------------------------------
#* NOTA: Este insumo contiene los vectores de variables que se utilizan en la creación de las variables recodificadas
rec_vars <- list(
    emper_ep_pct = c(paste0("emper_p_inseg_lugares_", 1:11)), # Todos los lugares menos plazas del barrio y negocios del barrio
    emper_barrio_pct = c("emper_p_inseg_oscuro_1", "emper_p_inseg_dia_1"), # Caminando por el barrio día y noche
    emper_casa_pct = c("emper_p_inseg_oscuro_2", "emper_p_inseg_dia_2"), # Estando en su casa día y noche
    perper_delito = list(
        "perper_p_expos_delito",
        paste0("perper_p_delito_pronostico_", c(1:4, 6, 9:11)),
        paste0("perper_p_delito_pronostico_", c(5, 7:8)),
        "perper_p_expos_delito",
        paste0("perper_p_delito_pronostico_", c(77, 88, 99))
    ),
    pergen_pais = c("pergen_p_aumento_pais"),
    pergen_comuna = c("pergen_p_aumento_com"),
    pergen_barrio = c("pergen_p_aumento_barrio"),
    comper_pct = paste0("comper_p_mod_actividades_", 1:13),
    comper_gasto = c("comper_costos_medidas"),
    comgen_per_pct = c(
        "comgen_medidas_perro", "comgen_medidas_alarma_privada",
        "comgen_medidas_camaras_vigilancia", "comgen_medidas_rejas",
        "comgen_medidas_cerco", "comgen_medidas_proteccion",
        "comgen_medidas_seguro", "comgen_medidas_foco"
    ), # Todas las medidas personales
    comgen_com_pct = c(
        "comgen_vecinos_medidas_whatsapp", "comgen_vecinos_medidas_vigilancia",
        "comgen_vecinos_medidas_al_comunit", "comgen_vecinos_medidas_coord_pol",
        "comgen_vecinos_medidas_coord_mun", "comgen_vecinos_medidas_televig",
        "comgen_vecinos_medidas_privad"
    ) # Todas las medidas comunitarias
)

#* NOTAS TÉCNICAS:
#* Debido a la naturaleza de la variable perper_delito, el elemento rec_vars[["perper_delito"]]] corresponde a una lista de
#* strings/vectores, en vez de solo un vector (como las demás variables). En esta sub-lista ( rec_vars[["perper_delito"]]] ),
#* cada elemento corresponden a las variables que se utilizarán para recodificar cada categoría del case_when(). Por ejemplo,
#* el elemento 1 ( rec_vars[["perper_delito"]][[1]] ) corresponde a perper_p_expos_delito. Esta es la variable que permitirá
#* generar la categoría "1. No cree que será victima de delito", cuando perper_p_expos_delito == 2.

# 3.2 Recodificar ----------------------------------------------------------------------------------------------------------------------------------------
enusc <- enusc %>%
    create_var_pct(
        success.cats = c(1, 2),
        source.cols = rec_vars[["emper_ep_pct"]],
        name.var.pct = "emper_ep_pct"
    ) %>%
    create_var_pct(
        success.cats = c(1, 2),
        source.cols = rec_vars[["emper_barrio_pct"]],
        name.var.pct = "emper_barrio_pct"
    ) %>%
    create_var_pct(
        success.cats = c(1, 2),
        source.cols = rec_vars[["emper_casa_pct"]],
        name.var.pct = "emper_casa_pct"
    ) %>%
    mutate(
        perper_delito = case_when(
            if_all(rec_vars[["perper_delito"]][[1]], ~ . == 2) ~ 1, # No cree que será victima de delito
            if_any(rec_vars[["perper_delito"]][[2]], ~ . == 1) ~ 2, # Cree que será victima de un delito no violento
            if_any(rec_vars[["perper_delito"]][[3]], ~ . == 1) ~ 3, # Cree que será victima de un delito violento
            if_all(rec_vars[["perper_delito"]][[4]], ~ . %in% c(88, 99)) ~ 4, # No sabe/No responde si cree que será victima de delito
            if_any(rec_vars[["perper_delito"]][[5]], ~ . == 1) ~ 5, # No sabe/No responde de qué delito será victima / Cree que será victima de otro tipo de delito
            TRUE ~ NA
        ),
        pergen_pais = if_all(rec_vars[["pergen_pais"]], ~ . == 1),
        pergen_comuna = if_all(rec_vars[["pergen_comuna"]], ~ . == 1),
        pergen_barrio = if_all(rec_vars[["pergen_barrio"]], ~ . == 1)
    ) %>%
    create_var_pct(
        success.cats = 1,
        source.cols = rec_vars[["comper_pct"]],
        name.var.pct = "comper_pct"
    ) %>%
    mutate(
        comper_gasto = case_when(
            if_all(rec_vars[["comper_gasto"]], ~ . %in% c(1:5)) ~ 1,
            if_all(rec_vars[["comper_gasto"]], ~ . == 85) ~ 0,
            if_all(rec_vars[["comper_gasto"]], ~ . == 88) ~ 88,
            if_all(rec_vars[["comper_gasto"]], ~ . == 99) ~ 99,
            TRUE ~ NA
        )
    ) %>%
    create_var_pct(
        success.cats = 1,
        source.cols = rec_vars[["comgen_per_pct"]],
        name.var.pct = "comgen_per_pct"
    ) %>%
    create_var_pct(
        success.cats = 1,
        source.cols = rec_vars[["comgen_com_pct"]],
        name.var.pct = "comgen_com_pct"
    )

# ! AJUSTES!
vec_comgen_per <- c("comgen_medidas_na", "comgen_medidas_ns", "comgen_medidas_nr")
vec_comgen_com <- c("comgen_vecinos_medidas_na", "comgen_vecinos_medidas_ns", "comgen_vecinos_medidas_nr")

enusc <- enusc %>%
    mutate(
        # Pasar de TRUE/FALSE a 1/0
        across(where(is.logical), ~ as.numeric(.)),
        # Pasar a NA las variables de comgen cuando se selecciona la columna de No aplica, No sabe o No responde
        comgen_per_pct = if_else(if_any(all_of(vec_comgen_per), ~ . == 1), NA, comgen_per_pct),
        comgen_com_pct = if_else(if_any(all_of(vec_comgen_com), ~ . == 1), NA, comgen_com_pct),
        # ! IMPORTANTE: CREAR VARIABLE DICOTOMICA, ESTA ES LA QUE USAMOS EN MCA
        across(ends_with("_pct"), ~ if_else(. > 50, 1, 0), .names = "{.col}_rec"),
        # Manejo explicito de No aplica, No sabe y No responde para variables de comgen (es necesario ya que son preguntas de opción múltiple)
        comgen_per_pct_rec = case_when(
            comgen_medidas_na == 1 ~ 85,
            comgen_medidas_ns == 1 ~ 88,
            comgen_medidas_nr == 1 ~ 99,
            TRUE ~ comgen_per_pct_rec
        ),
        comgen_com_pct_rec = case_when(
            comgen_vecinos_medidas_na == 1 ~ 85,
            comgen_vecinos_medidas_ns == 1 ~ 88,
            comgen_vecinos_medidas_nr == 1 ~ 99,
            TRUE ~ comgen_com_pct_rec
        )
    )

# 3.3 Imputar 85, 88 y 99 --------------------------------------------------------------------------------------------------------------------------------

# Excluir variables de la imputación
excluir <- c("perper_delito", "comper_gasto")
rec_vars_torec <- rec_vars[!names(rec_vars) %in% excluir]

# Agregar sufijjo "_rec" a los nombres de las variables
names(rec_vars_torec) <- if_else(
    str_detect(names(rec_vars_torec), "_pct$"),
    paste0(names(rec_vars_torec), "_rec"),
    names(rec_vars_torec)
)

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

# 3.5 Generar metadata recode -----------------------------------------------------------------------------------------------------------------------------
source("processing/helpers/gen_metadata_recode.R")

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
saveWorkbook(
    format_tab_excel(metadata_recode, wb = createWorkbook(), sheet = "Metadata", var_col = "variable_recodificada", color_header = "#fcd5b4", sep_style = "dashed"),
    "output/metadata_recode.xlsx",
    overwrite = TRUE
)
