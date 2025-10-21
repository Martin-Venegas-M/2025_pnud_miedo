# Crear etiquetas de variables
etiquetas_variables <- c(
    "Inseguridad en Transporte"                                  = "emper_transporte",
    "Inseguridad en Recreación"                                  = "emper_recreacion",
    "Inseguridad en Barrio"                                      = "emper_barrio",
    "Inseguridad en Casa"                                        = "emper_casa",
    "Expectativa de ser victima delito"                          = "perper_delito",
    "Aumento delincuencia en el país"                            = "pergen_pais",
    "Aumento delincuencia en el comuna"                          = "pergen_comuna",
    "Aumento delincuencia en el barrio"                          = "pergen_barrio",
    "Modifica comportamiento en vida cotidiana"                  = "comper_vida_cotidiana",
    "Modifica comportamiento en transporte"                      = "comper_transporte",
    "Gasta en medidas de seguridad"                              = "comper_gasto_medidas",
    "Dispone de medidas de seguridad (personales)"               = "comgen_medidas_per",
    "Disponen de medidas de seguridad (comunitarias)"            = "comgen_medidas_com",
    "Inseguridad en todos los lugares (pct)"                     = "emper_pct",
    "Inseguridad en Transporte (pct)"                            = "emper_transporte_pct",
    "Inseguridad en Recreación (pct)"                            = "emper_recreacion_pct",
    "Modifica comportamiento en todas las situaciones (pct)"     = "comper_pct",
    "Modifica comportamiento en vida cotidiana (pct)"            = "comper_vida_cotidiana_pct",
    "Modifica comportamiento en transporte (pct)"                = "comper_transporte_pct",
    "Inseguridad en todos los lugares (pct rec)"                 = "emper_pct_rec",
    "Inseguridad en Transporte (pct rec)"                        = "emper_transporte_pct_rec",
    "Inseguridad en Recreación (pct rec)"                        = "emper_recreacion_pct_rec",
    "Modifica comportamiento en todas las situaciones (pct rec)" = "comper_pct_rec",
    "Modifica comportamiento en vida cotidiana (pct rec)"        = "comper_vida_cotidiana_pct_rec",
    "Modifica comportamiento en transporte (pct rec)"            = "comper_transporte_pct_rec"
)

etiquetas_valores <- list(
    "emper_transporte" = c(
        "Muy inseguro/Inseguro en Transporte" = 1,
        "Muy seguro/Seguro en Transporte" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "emper_recreacion" = c(
        "Muy inseguro/Inseguro en Recreación" = 1,
        "Muy seguro/Seguro en Recreación" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "emper_barrio" = c(
        "Muy inseguro/Inseguro en el Barrio" = 1,
        "Muy seguro/Seguro en el Barrio" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "emper_casa" = c(
        "Muy inseguro/Inseguro en la Casa" = 1,
        "Muy seguro/Seguro en Casa" = 0,
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
    "comper_vida_cotidiana" = c(
        "Modifica comportamiento en Vida Cotidiana" = 1,
        "No modifica comportamiento en Vida Cotidiana" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "comper_transporte" = c(
        "Modifica comportamiento en Transporte" = 1,
        "No modifica comportamiento en Transporte" = 0,
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
    "comgen_medidas_per" = c(
        "Dispone de medidas personales" = 1,
        "No dispone de medidas personales" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "comgen_medidas_com" = c(
        "Dispone de medidas comunitarias" = 1,
        "No dispone de medidas comunitarias" = 0,
        "No aplica" = 85,
        "No sabe" = 88,
        "No responde" = 99
    ),
    "emper_pct_rec" = c(
        "Sobre el 50% en todos los lugares" = 1,
        "Igual o bajo el 50% en todos los lugares" = 0
    ),
    "emper_transporte_pct_rec" = c(
        "Sobre el 50% en Transporte" = 1,
        "Igual o bajo el 50% en Transporte" = 0
    ),
    "emper_recreacion_pct_rec" = c(
        "Sobre el 50% en Transporte" = 1,
        "Igual o bajo el 50% en Transporte" = 0
    ),
    "comper_pct_rec" = c(
        "Sobre el 50% en todas las situaciones" = 1,
        "Igual o bajo el 50% en todas las situaciones" = 0
    ),
    "comper_vida_cotidiana_pct_rec" = c(
        "Sobre el 50% en Vida cotidiana" = 1,
        "Igual o bajo el 50% en Vida cotidiana" = 0
    ),
    "comper_transporte_pct_rec" = c(
        "Sobre el 50% en Transporte" = 1,
        "Igual o bajo el 50% en Transporte" = 0
    )
)
