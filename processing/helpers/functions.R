# Tabla frecuencias basada en sjmisc::frq() - Solo muestra las estimaciones ponderadas
tab_frq1 <- function(data = enusc, var, w = fact_pers_reg, verbose = FALSE, ...) {
    tab <- data %>%
        frq({{ var }}, weights = {{ w }}, ...)

    tab <- tab[[1]] %>% as_tibble()

    if (verbose) {
        print(get_label(data %>% pull({{ var }})))
    }

    return(tab)
}

# Tabla frecuencias basada en srvyr - Muestra estimaciones ponderadas y de calidad
tab_frq2 <- function(svyobj = enusc_svy, var, verbose = FALSE, ...) {
    tab <- svyobj %>%
        group_by({{ var }}) %>%
        summarise(
            frq = survey_total(),
            prop = survey_mean(...),
        ) %>%
        mutate(
            label = get_labels({{ var }}),
            across(starts_with("prop"), ~ round((. * 100), 2)),
            across(starts_with("frq"), ~ round(.))
        ) %>%
        relocate(var = {{ var }}, label, everything()) %>%
        remove_all_labels() %>%
        as_tibble()

    if (verbose) {
        print(get_label(svyobj$variables %>% pull({{ var }})))
    }

    return(tab)
}
