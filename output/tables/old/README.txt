# NOTAS

- En 250913_rec_vars_tabs_unw.xlsx se pueden ver las frecuencias muestrales de las variables recodificadas previo al manejo explicito
de los 85, 88 y 99. En estas tablas el 0 suele estár sobreestimado (ya que incluyen los casos 85, 88 y 99). Y en particular, pára la variable de perper_delito los 85, 88 y 99 quedaban como NA.

- En 250914_rec_vars_unw.xlsx se pueden ver las frecuencias muestrales de las variables recodificadas posterior al manejo explicito de los 85, 88 y 99. Sin embargo, aqui me había faltado hacer el manejo explicito de las variables comgen_medidas_per y comgen_medidas_com. Esto ya que la estrategia que había escogido no iba de la mano la naturaleza de la pregunta (opción múltiple).

- En 250914b_rec_vars_unw.xlsx se pueden ver las frecuencias muestrales de las variables recodificadas posterior al manejo explicito de los 85, 88 y 99 para todas las variables. Solucioné el manejo de 85, 88 y 99 para comgen_medidas_per y comgen_medidas_per incorporando una estrategia que considera que estos códigos se encuentran en columnas a parte y no como categorías (ej. incorporar la existencia de comgen_medidas_na en la creación de comgen_medidas_per).

- En 250915_rec_vars_unw.xlsx y en 250915_rec_vars.xlsx se agrega una pestaña con la metadata de las variables fuentes que componen cada variable recodificada