#******************************************************************************************************************************************************
# 0. Identification -------------------------------------------------------
# Título: Descriptivos de variables recodificadas
# Institución: PNUD
# Responsable: Consultor técnico - MVM
# Resumen ejecutivo: Este script contiene el código para la generación de descriptivos de las variables recodificadas
# Fecha: 14 de septiembre de 2025
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
    srvyr,
    openxlsx,
    scales,
    rlang
)

# 2. Cargar datos y funciones ----------------------------------------------------------------------------------------------------------------------------

enusc <- readRDS("input/data/proc/enusc_3_add_vars.RDS")
source("processing/helpers/functions.R")

enusc_svy <- enusc %>%
    as_survey_design(ids = conglomerado, stata = varstrat, weights = fact_pers_reg)

# Declarar fecha y usuario
date <- format(Sys.Date(), "%y%m%d")
user <- tolower(Sys.info()["user"])

# Cargar metadata
metadata_recode <- readxl::read_excel("output/metadata_recode.xlsx")

# 3. Ejecutar código -------------------------------------------------------------------------------------------------------------------------------------

# 3.1 Variables recodificadas ----------------------------------------------------------------------------------------------------------------------------

# Ponderadas
rec_vars <- map(
    gen_vct(emper_transporte:comgen_medidas_com),
    ~ tab_frq1(var = !!sym(.x), verbose = FALSE)
) %>%
    list_rbind() %>%
    pre_proc_excel(type = "tab_frq1")

# Muestrales
rec_vars_unw <- map(
    gen_vct(emper_transporte:comgen_medidas_com),
    ~ tab_frq1(var = !!sym(.x), w = NULL, verbose = FALSE)
) %>%
    list_rbind()

# 3.2 Variables secundarias -----------------------------------------------------------------------------------------------------------------------------

# Ponderadas
sec_vars <- map(
    c("rph_sexo", "rph_nivel", "rph_edad", "rph_nse", "vp_dc", "vp_dv"),
    ~ tab_frq1(var = !!sym(.x), verbose = TRUE, sep_verbose = FALSE)
) %>%
    list_rbind() %>%
    pre_proc_excel()

# Muestrales
sec_vars_unw <- map(
    c("rph_sexo", "rph_nivel", "rph_edad", "rph_nse", "vp_dc", "vp_dv"),
    ~ tab_frq1(var = !!sym(.x), w = NULL, verbose = TRUE, sep_verbose = FALSE)
) %>%
    list_rbind()

# 3.3 Cruces --------------------------------------------------------------------------------------------------------------------------------------------

df <- expand_grid(
    rec_vars = gen_vct(emper_transporte:comgen_medidas_com),
    sec_vars = c("rph_sexo", "rph_nivel", "rph_edad", "rph_nse", "vp_dc", "vp_dv")
)

# Ponderadas
rec_sec_vars <- map2(
    df$rec_vars,
    df$sec_vars,
    ~ tab_frq2(var = !!sym(.x), grp = TRUE, grp_var = !!sym(.y), verbose = FALSE, vartype = NULL),
    .progress = TRUE
) %>%
    set_names(str_trunc(glue("{df$rec_vars} x {str_replace(df$sec_vars, 'rph_', '')}"), 30))

# 4. Guardar --------------------------------------------------------------------------------------------------------------------------------------------

# 4.1 Tablas ponderadas ---------------------------------------------------------------------------------------------------------------------------------

wb <- format_tab_excel(rec_vars, sheet = "Recodificadas ponderadas")
wb <- format_tab_excel(metadata_recode, wb = wb, sheet = "Metadata", var_col = "variable_recodificada", color_header = "#fcd5b4", sep_style = "dashed")
wb <- format_tab_excel(sec_vars, wb = wb, sheet = "Secundarias ponderadas")

saveWorkbook(wb, glue("output/tables/{date}_all_vars_tabs.xlsx"), overwrite = TRUE)

wb_tabs <- reduce(
    seq_along(rec_sec_vars),
    \(workbook, i) {
        format_tab_excel(
            pre_proc_excel(rec_sec_vars[[i]], type = "tab_frq2"),
            wb = workbook,
            sheet = names(rec_sec_vars)[[i]],
            var_col = names(rec_sec_vars[[i]][2]),
            sep_style = "dashed"
        )
    },
    .init = createWorkbook()
)

saveWorkbook(wb_tabs, glue("output/tables/{date}_recxsec_vars_tabs.xlsx"), overwrite = TRUE)

# 4.2 Tablas muestrales ---------------------------------------------------------------------------------------------------------------------------------
wb_unw <- format_tab_excel(
    unw,
    sheet = "Recodificadas muestrales",
    add_metadata = TRUE,
    metadata = metadata_recode,
    save = TRUE
)

# ! PENDIENTE

saveWorkbook(wb_unw, glue("output/tables/{date}_all_vars_tabs_unw.xlsx"))
