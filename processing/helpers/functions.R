# Tabla frecuencias basada en sjmisc::frq() - Solo muestra las estimaciones ponderadas
tab_frq1 <- function(data = enusc, var, w = fact_pers_reg, verbose = TRUE, sep_verbose = TRUE, pattern_verbose = "\\? ", ...) {
    # Extraer el df que da sjmisc::frq() por defecto (muy bueno!)
    tab <- data %>%
        sjmisc::frq({{ var }}, weights = {{ w }}, ...) # * Se pasan los argumentos por si se quiere usar las funcionalidades de sjmisc::frq()

    # Crear col de variable y reordenar
    tab <- tab[[1]] %>%
        tibble::as_tibble() %>%
        dplyr::mutate(variable = rlang::as_label(ensym(var))) %>%
        dplyr::relocate(variable, everything())

    # Si se quiere la etiqueta de la variable, incorporar y reordenar
    if (verbose) {
        var_label <- sjlabelled::get_label(
            data %>% pull({{ var }})
        )

        tab <- tab %>%
            dplyr::mutate(desc = var_label) %>%
            dplyr::relocate(desc, everything())

        # Si se quiere separar la etiqueta de la variable en dos columnas: la pregunta constante y la categoria
        if (sep_verbose) {
            tab <- tab %>% separate(desc,
                into = c("pregunta", "categoria"),
                sep = pattern_verbose
            )
        }
    }

    return(tab)
}

# Tabla frecuencias basada en srvyr - Muestra estimaciones ponderadas y de calidad
tab_frq2 <- function(svyobj = enusc_svy, var, verbose = TRUE, sep_verbose = TRUE, pattern_verbose = "\\? ", ...) {
    # Crear tabla de frecuencias desde del objeto encuesta
    tab <- svyobj %>%
        srvyr::group_by({{ var }}) %>%
        srvyr::summarise(
            frq = survey_total(),
            prop = survey_mean(...), # * Se pasan los argumentos por si se quieren otras medidas de calidad para la proporción
        ) %>%
        # Crear cols informativas, formatear y ordenar tabla
        srvyr::mutate(
            label = get_labels({{ var }}),
            across(starts_with("prop"), ~ round((. * 100), 2)),
            across(starts_with("frq"), ~ round(.)),
            variable = rlang::as_label(ensym(var))
        ) %>%
        dplyr::relocate(variable, val = {{ var }}, label, everything()) %>%
        sjlabelled::remove_all_labels() %>%
        tibble::as_tibble()

    # Si se quiere la etiqueta de la variable, incorporar y reordenar
    if (verbose) {
        var_label <- sjlabelled::get_label(
            svyobj %>% pull({{ var }})
        )

        tab <- tab %>%
            dplyr::mutate(desc = var_label) %>%
            dplyr::relocate(desc, everything())

        # Si se quiere separar la etiqueta de la variable en dos columnas: la pregunta constante y la categoria
        if (sep_verbose) {
            tab <- tab %>% separate(desc,
                into = c("pregunta", "categoria"),
                sep = pattern_verbose
            )
        }
    }

    return(tab)
}


# Función de formateo excel (hecha por Chat GPT y adaptada por mi)
format_tab_excel <- function(df, path, sheet = "Tabla", var_col = "variable") {
    stopifnot(var_col %in% names(df))

    wb <- createWorkbook()
    addWorksheet(wb, sheet)

    # Escribimos los datos (sin filtros todavía)
    writeData(wb, sheet, x = df, withFilter = FALSE)

    n_cols <- ncol(df)

    # 1) Encabezado: azul claro, negrita, borde inferior grueso
    header_style <- createStyle(
        fgFill = "#478ec5", # azul claro
        textDecoration = "bold",
        halign = "center",
        valign = "center",
        border = "bottom",
        borderStyle = "thick"
    )
    addStyle(
        wb, sheet, header_style,
        rows = 1, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE
    )

    # 2) Filtros en columnas
    addFilter(wb, sheet, row = 1, cols = 1:n_cols)

    # 3) Ancho automático de columnas
    setColWidths(wb, sheet, cols = 1:n_cols, widths = "auto")

    # 4) Borde inferior grueso al cambiar de 'variable' #! INTERESANTE, PROFUNDIZAR
    #    (línea gruesa al final de cada bloque de la variable)
    ends <- which(df[[var_col]] != dplyr::lead(df[[var_col]], default = tail(df[[var_col]], 1)))
    if (length(ends)) {
        group_border <- createStyle(border = "bottom", borderStyle = "thick")
        # +1 porque los datos comienzan en la fila 2 (fila 1 = encabezado)
        addStyle(
            wb, sheet,
            style = group_border,
            rows = ends + 1, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE
        )
    }

    # Guardar
    saveWorkbook(wb, file = path, overwrite = TRUE)
    invisible(path)
}

# Test
# format_tab_excel(emper_tabs, path = "empr_tabs_formateada.xlsx", sheet = "EMPER")
