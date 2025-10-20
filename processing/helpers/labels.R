# Crear etiquetas de variables
etiquetas_variables <- c(
    "Inseguridad en Espacio público"                             = "emper_ep_pct",
    "Inseguridad en Espacio público (rec)"                       = "emper_ep_pct_rec",
    "Inseguridad en Barrio"                                      = "emper_barrio_pct",
    "Inseguridad en Barrio (rec)"                                = "emper_barrio_pct_rec",
    "Inseguridad en Casa"                                        = "emper_casa_pct",
    "Inseguridad en Casa (rec)"                                  = "emper_casa_pct_rec",
    "Expectativa de ser victima delito"                          = "perper_delito",
    "Aumento delincuencia en el país"                            = "pergen_pais",
    "Aumento delincuencia en el comuna"                          = "pergen_comuna",
    "Aumento delincuencia en el barrio"                          = "pergen_barrio",
    "Modifica comportamiento"                                    = "comper_pct",
    "Modifica comportamiento (rec)"                              = "comper_pct_rec",
    "Gasta en medidas de seguridad"                              = "comper_gasto_medidas",
    "Dispone de medidas de seguridad (personales)"               = "comgen_per_pct",
    "Dispone de medidas de seguridad (personales) (rec)"         = "comgen_per_pct_rec",
    "Disponen de medidas de seguridad (comunitarias)"            = "comgen_com_pct",
    "Disponen de medidas de seguridad (comunitarias) (rec)"      = "comgen_com_pct_rec"
)

etiquetas_valores <- list(
    "emper_ep_pct_rec" = c(
        "Alta inseguridad en espacio publico" = 1,
        "Baja inseguridad en espacio publico" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "emper_barrio_pct_rec" = c(
        "Alta inseguridad en el barrio" = 1,
        "Baja inseguridad en el barrio" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "emper_casa_pct_rec" = c(
        "Alta inseguridad en la casa" = 1,
        "Baja de inseguridad en la casa" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "perper_delito" = c(
        "No cree que será victima de delito" = 1,
        "Cree que será victima de delito no violento" = 2,
        "Cree que será victima de delito violento" = 3,
        "No sabe/No responde si cree que será victima de delito" = 4,
        "No sabe/No responde de qué delito será victima / Cree que será victima de otro tipo de delito" = 5
    ),
    "pergen_pais" = c(
        "Aumentó delincuencia en País" = 1,
        "Se mantuvo/Disminuyó delincuencia en País" = 0,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "pergen_comuna" = c(
        "Aumentó delincuencia en Comuna" = 1,
        "Se mantuvo/Disminuyó delincuencia en Comuna" = 0,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "pergen_barrio" = c(
        "Aumentó delincuencia en Barrio" = 1,
        "Se mantuvo/Disminuyó delincuencia en Barrio" = 0,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "comper_pct_rec" = c(
        "Alta modificación de prácticas" = 1,
        "Baja modificación de prácticas" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "comper_gasto_medidas" = c(
        "Gasta en medidas de seguridad" = 1,
        "No gasta en medidas de seguridad" = 0,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "comgen_per_pct_rec" = c(
        "Alta disposición de medidas personales" = 1,
        "Baja disposición de medidas personales" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "comgen_com_pct_rec" = c(
        "Alta disposición de medidas comunitarias" = 1,
        "Baja disposición de medidas comunitarias" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    )
)
