etiquetas_variables <- c(
    "Inseguridad en todos los lugares (pct)" = "emper_pct",
    "Inseguridad en Transporte (pct)" = "emper_transporte_pct",
    "Inseguridad en Recreación (pct)" = "emper_recreacion_pct",
    "Expectativa de ser victima delito (new)" = "perper_delito_new",
    "Modifica comportamiento en todas las situaciones (pct)" = "comper_pct",
    "Modifica comportamiento en vida cotidiana (pct)" = "comper_vida_cotidiana_pct",
    "Modifica comportamiento en transporte (pct)" = "comper_transporte_pct",
    "Inseguridad en todos los lugares (pct rec)" = "emper_pct_rec",
    "Inseguridad en Transporte (pct rec)" = "emper_transporte_pct_rec",
    "Inseguridad en Recreación (pct rec)" = "emper_recreacion_pct_rec",
    "Modifica comportamiento en todas las situaciones (pct rec)" = "comper_pct_rec",
    "Modifica comportamiento en vida cotidiana (pct rec)" = "comper_vida_cotidiana_pct_rec",
    "Modifica comportamiento en transporte (pct rec)" = "comper_transporte_pct_rec"
)

etiquetas_valores <- list(
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
    "perper_delito_new" = c(
        "No cree que será victima de delito" = 1,
        "Cree que será victima de delito no violento" = 2,
        "Cree que será victima de delito violento" = 3,
        "No sabe/No responde si cree que será victima de delito" = 4,
        "No sabe/No responde de qué delito será victima / Cree que será victima de otro tipo de delito" = 5
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
