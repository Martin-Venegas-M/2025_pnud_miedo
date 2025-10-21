#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: LCA de variables recodificadas
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para un análisis de correspondencias multiples
# Fecha: 8 de septiembre de 2025
#******************************************************************************************************************************************************

rm(list = ls())

# 1. Cargar paquetes ------------------------------------------------------------------------------------------------------------------------------------
if (!require("pacman")) install.packages("pacman") # if pacman es missing, install

pacman::p_load(
    tidyverse,
    haven,
    tidylog,
    rlang,
    glue,
    openxlsx,
    poLCA
)

# 2. Cargar datos y funciones ----------------------------------------------------------------------------------------------------------------------------

enusc <- readRDS("input/data/proc/enusc_na_2_recode.RDS")
# source("processing/helpers/functions.R")

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# 3. Ejecutar código -------------------------------------------------------------------------------------------------------------------------------------

# 3.1 Preparar data --------------------------------------------------------------------------------------------------------------------------------------

# Vector de variables a incluir
rec_vars <- c(
    "emper_ep_pct_rec", "emper_barrio_pct_rec", "emper_casa_pct_rec",
    "perper_delito",
    # "pergen_pais", "pergen_comuna", "pergen_barrio",
    "comper_pct_rec", # "comper_gasto_medidas",
    # "comgen_per_pct_rec",
    "comgen_com_pct_rec"
)

# Data para prueba
df <- enusc %>%
    dplyr::select(rph_id, all_of(rec_vars)) %>%
    mutate(
        across(everything(), ~ if_else(. == 85, NA, .)), # Pasar a NA los no aplica
        across(perper_delito, ~ if_else(. %in% c(4, 5), NA, .)), # Pasar a NA categorías especificas de perper_delito
        across(all_of(rec_vars), ~ sjlabelled::to_label(.)) # Usar las categorías para los factores
    ) %>%
    drop_na()

# 3.2 Probar LCA ---------------------------------------------------------------------------------------------------------------------------------------

# Insumos
f <- cbind(
    emper_ep_pct_rec,
    emper_barrio_pct_rec,
    emper_casa_pct_rec,
    perper_delito,
    comper_pct_rec,
    comgen_com_pct_rec
) ~ 1

# Plantear lca
create_lca_tab <- function(n.class) {
    # Eliminar identificador
    data <- df[-1]

    # Categorías sustantivas
    cats <- map(
        data,
        \(x) levels(x)[
            !levels(x) %in% c(
                "No aplica", "No sabe", "No responde",
                "No sabe/No responde si cree que será victima de delito",
                "No sabe/No responde de qué delito será victima / Cree que será victima de otro tipo de delito"
            )
        ]
    )

    # Df con categorías sustantivas
    cats_sust <- imap(
        cats,
        ~ tibble(
            variable = .y,
            categoria = paste0("Pr(", seq_along(.x), ")"),
            cat_sust = .x,
        )
    ) %>%
        list_rbind()

    # Df con etiquetas
    var_labs <- map2(
        names(data),
        sjlabelled::get_label(data),
        \(x, y) tibble(
            variable = x,
            label = y
        )
    ) %>% list_rbind()


    # Estimar LCA
    lca <- poLCA(f, data, nclass = n.class, verbose = F)

    # Vector con las clases
    class_vector <- rep(glue("class{1:n.class}"), length(data))

    # Crear tabla
    lca_tab <- lca$probs %>%
        map(., as_tibble) %>%
        list_rbind() %>%
        mutate(
            class = class_vector,
            variable = rep(names(data), times = 1, each = n.class)
        ) %>%
        relocate(variable, class, everything()) %>%
        pivot_longer(
            cols = starts_with("Pr("),
            names_to = "categoria",
            values_to = "probabilidad"
        ) %>%
        filter(!is.na(probabilidad)) %>%
        left_join(cats_sust, by = c("variable", "categoria")) %>%
        relocate(cat_sust, .after = categoria) %>%
        left_join(var_labs, by = "variable") %>%
        relocate(label, .after = variable)

    # Return
    return(lca_tab)
}

# Test!
lca_tab <- create_lca_tab(3)
