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
tab_frq2 <- function(svyobj = enusc_svy, grp = FALSE, grp_var, var, verbose = TRUE, sep_verbose = TRUE, pattern_verbose = "\\? ", ...) {
    # Crear tabla de frecuencias desde del objeto encuesta
    tab <- svyobj %>%
        srvyr::group_by({{ grp_var }}, {{ var }}) %>%
        srvyr::summarise(
            frq = survey_total(...),
            prop = survey_mean(...), # * Se pasan los argumentos por si se quieren otras medidas de calidad para la proporción
        ) %>%
        # Crear cols informativas, formatear y ordenar tabla
        srvyr::mutate(
            label = to_label({{ var }}),
            across(starts_with("prop"), ~ round((. * 100), 2)),
            across(starts_with("frq"), ~ round(.)),
            variable = rlang::as_label(ensym(var))
        ) %>%
        dplyr::relocate(variable, val = {{ var }}, label, everything())

    if (grp) {
        tab <- tab %>%
            mutate(
                {{ grp_var }} := sjlabelled::to_label({{ grp_var }})
            ) %>%
            relocate({{ grp_var }}, .after = variable)
    }

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

# Función de formateo excel
format_tab_excel <- function(df, wb = openxlsx::createWorkbook(), sheet, color_header = "#478ec5", var_col = "variable", sep_style = "thick", save = FALSE, path) {
    stopifnot(var_col %in% names(df))

    # Añadir pestaña y escribr datos
    addWorksheet(wb, sheet)
    writeData(wb, sheet, x = df, withFilter = FALSE)
    n_cols <- ncol(df)

    # Generar y añadir estilo header
    header_style <- createStyle(
        fgFill = color_header,
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

    # Añadir filtros y setear anchos
    addFilter(wb, sheet, row = 1, cols = 1:n_cols)
    setColWidths(wb, sheet, cols = 1:n_cols, widths = "auto")

    # Añadir borde inferior grueso por cada "variable"
    ends <- which(df[[var_col]] != dplyr::lead(df[[var_col]], default = tail(df[[var_col]], 1)))

    if (length(ends)) {
        group_border <- createStyle(border = "bottom", borderStyle = sep_style)

        addStyle(
            wb, sheet,
            style = group_border,
            rows = ends + 1, cols = 1:n_cols, gridExpand = TRUE, stack = TRUE # +1 porque los datos comienzan en la fila 2 (fila 1 = encabezado)
        )
    }

    # Guardar si asi se solicita
    if (save) {
        saveWorkbook(wb, path, overwrite = TRUE)
    } else {
        return(wb)
    }
}

# Formateo español para excel
pre_proc_excel <- function(x, type = "tab_frq1") {
    # Preprocesamiento
    if (type == "tab_frq1") {
        x <- x %>%
            filter(!is.na(val)) %>% # Sacamos la fila de NA
            mutate(
                # Pasamos a formato español
                frq = number(frq, big.mark = ".", decimal.mark = ","),
                across(c(raw.prc, valid.prc, cum.prc), ~ number(., big.mark = ".", decimal.mark = ",", accuracy = 0.01))
            ) %>%
            select(-starts_with("raw")) %>% # Eliminamos la columna del raw
            rename(prc = valid.prc)
        return(x)
    }

    if (type == "tab_frq2") {
        x %>%
            mutate(
                # Pasamos a formato español
                frq = number(frq, big.mark = ".", decimal.mark = ","),
                prc = number(prop, big.mark = ".", decimal.mark = ",", accuracy = 0.01)
            ) %>%
            select(-starts_with("prop"))
    }
}

mca_hcpc <- function(data, n_class = 6, select = FALSE, id_col = "rph_id", ...) {
    if (select) {
        data <- data %>%
            dplyr::select(...) %>%
            dplyr::mutate(across(everything(), ~ sjlabelled::to_label(.)))
    }

    # Run MCA analysis
    acm <- FactoMineR::MCA(data %>% select(-{{ id_col }}), graph = FALSE)

    # Run cluster analysis
    clust <- FactoMineR::HCPC(acm, nb.clust = n_class, consol = FALSE, graph = FALSE)

    # Save acm scores and clusters in df
    data <- data %>% mutate(
        acm_scores1 = acm$ind$coord[, 1],
        "clusters_{n_class}" := clust$data.clust$clust
    )

    # Save all
    results <- list(data = data, acm = acm, clust = clust)

    return(results)
}

plot_mca <- function(obj) {
    obj %>%
        fviz_mca_var(
            repel = TRUE,
            col.var = "cos2", # color = calidad de representación
            gradient.cols = c("#B3CDE3", "#6497B1", "#03396C"),
            ggtheme = theme_minimal()
        ) +
        ggtitle("MCA: categorías (Dim1 vs Dim2)") +
        theme(legend.position = "right")
}

plot_cluster <- function(obj) {
    obj %>%
        fviz_cluster(clust, geom = "point", main = "Factor map")
}

gen_vct <- function(...) {
    enusc %>%
        select(...) %>%
        names()
}
