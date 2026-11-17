# Contenido de las guías de indicar() --------------------------------------
#
# Cada guía es una lista con los campos:
#   titulo   texto que aparece en el menú
#   ruta     ruta(s) de menús en R Commander (uno o varios textos)
#   pasos    vector de texto, o lista con nombre de vectores si hay varias fases
#   ejemplos vector de texto (puede ser character(0))
#   codigo   vector de texto, una línea por elemento
#   consejos vector de texto
#
# Para añadir una opción nueva al menú basta con añadir un elemento a esta
# lista. El orden de la lista es el orden del menú.

guias_indicar <- function() {
  list(
    # 1. Filtrar ------------------------------------------------------------
    filtrar = list(
      titulo = "Filtrar la base de datos",
      ruta = "Datos > Conjunto de datos activo > Filtrar el conjunto de datos activo...",
      pasos = c(
        "Comprueba que está activo el conjunto de datos correcto (botón 'Conjunto de datos' en la barra superior).",
        "Ve a: Datos > Conjunto de datos activo > Filtrar el conjunto de datos activo...",
        "Deja marcado 'Incluir todas las variables', salvo que quieras quedarte solo con algunas columnas.",
        "En 'Expresión de selección' escribe la condición que deben cumplir las filas que quieres CONSERVAR (ver ejemplos).",
        "En 'Nombre del nuevo conjunto de datos' escribe un nombre nuevo (p. ej. datos_mujeres). No sobrescribas la base original.",
        "Pulsa 'Aceptar'. El nuevo conjunto pasa a ser el conjunto activo.",
        "Comprueba el resultado: 'Ver conjunto de datos' o Estadísticos > Resúmenes > Conjunto de datos activo (mira el número de filas)."
      ),
      ejemplos = c(
        'sexo == "Mujer"                  # solo mujeres',
        'edad >= 65                       # 65 años o más',
        'edad >= 18 & edad < 65           # entre 18 y 64 años (se cumplen las dos)',
        'fumador == "Si" | exfumador == "Si"  # se cumple al menos una',
        'provincia %in% c("Madrid", "Toledo") # uno de varios valores',
        '!is.na(imc)                      # sin valores perdidos en imc'
      ),
      codigo = 'datos_mujeres <- subset(datos, subset = sexo == "Mujer")',
      consejos = c(
        "Para comparar se usa doble igual (==). Un solo '=' da error.",
        "Los valores de texto o de factor van entre comillas y deben coincidir exactamente (mayúsculas, tildes, espacios). Los números van sin comillas y con punto decimal.",
        "Las filas con NA en la variable de la condición se EXCLUYEN del nuevo conjunto. Tenlo en cuenta al describir la muestra.",
        "Tras filtrar, el conjunto activo es el filtrado. Si después quieres analizar a todos, vuelve a seleccionar la base original."
      )
    ),

    # 2. Recodificar --------------------------------------------------------
    recodificar = list(
      titulo = "Recodificar una variable",
      ruta = "Datos > Modificar variables del conjunto de datos activo > Recodificar variables...",
      pasos = c(
        "Ve a: Datos > Modificar variables del conjunto de datos activo > Recodificar variables...",
        "Selecciona la variable que quieres recodificar.",
        "En 'Nombre de la nueva variable' escribe un nombre nuevo (p. ej. edad_cat) para no perder la variable original.",
        "Deja marcado 'Convertir cada nueva variable en factor' si el resultado son categorías (lo habitual).",
        "En 'Introducir directrices de recodificación' escribe una regla por línea con el formato: valor_antiguo = valor_nuevo (ver ejemplos).",
        "Pulsa 'Aceptar'.",
        "Comprueba el resultado cruzando la variable original con la nueva (p. ej. Estadísticos > Resúmenes > Tabla de estadísticas, o una tabla de contingencia si ambas son categóricas)."
      ),
      ejemplos = c(
        'lo:39 = "<40"               # desde el mínimo hasta 39',
        '40:64 = "40-64"             # de 40 a 64, ambos incluidos',
        '65:hi = ">=65"              # desde 65 hasta el máximo',
        '"Hombre" = "H"              # cambiar una etiqueta',
        'c("Leve", "Moderado") = "No grave"  # agrupar categorías',
        'else = NA                   # todo lo no mencionado pasa a NA'
      ),
      codigo = c(
        "datos <- within(datos, {",
        "  edad_cat <- Recode(edad, 'lo:39 = \"<40\"; 40:64 = \"40-64\"; 65:hi = \">=65\"',",
        "                     as.factor = TRUE)",
        "})"
      ),
      consejos = c(
        "'lo' significa el valor mínimo y 'hi' el máximo. Los intervalos incluyen ambos extremos.",
        "Si la variable tiene decimales (p. ej. edad = 39.5), con lo:39 y 40:64 ese valor no cae en ningún grupo y se queda sin recodificar. Cuando una variable cumple varias reglas se aplica la PRIMERA, así que puedes escribirlas de mayor a menor: 65:hi = \">=65\", 40:65 = \"40-64\", lo:40 = \"<40\".",
        "Los valores que no menciones se quedan como estaban (salvo que uses 'else'). Los NA siguen siendo NA.",
        "Los valores de texto van entre comillas y deben coincidir exactamente con los de la base.",
        "Las categorías del nuevo factor quedarán en orden alfabético, que rara vez es el lógico. Para cambiarlo usa la opción 'Reordenar las categorías de un factor'."
      )
    ),

    # 3. Reordenar ----------------------------------------------------------
    reordenar = list(
      titulo = "Reordenar las categorías de un factor",
      ruta = "Datos > Modificar variables del conjunto de datos activo > Reordenar niveles de un factor...",
      pasos = c(
        "Ve a: Datos > Modificar variables del conjunto de datos activo > Reordenar niveles de un factor...",
        "Selecciona el factor. Si tu variable no aparece en la lista es porque no es un factor: conviértela antes (Datos > Modificar variables del conjunto de datos activo > Convertir variable numérica en factor...).",
        "En 'Nombre del factor' puedes dejar el mismo nombre (se sobrescribe) o escribir uno nuevo.",
        "Deja SIN marcar 'Crear factor ordenado', salvo que sepas que necesitas un factor ordinal.",
        "Pulsa 'Aceptar'. Se abrirá una ventana con las categorías: escribe junto a cada una el número de orden que quieres (1 = la primera).",
        "Pulsa 'Aceptar' y comprueba el nuevo orden con una tabla de frecuencias (Estadísticos > Resúmenes > Distribución de frecuencias...)."
      ),
      ejemplos = character(0),
      codigo = 'datos$edad_cat <- factor(datos$edad_cat, levels = c("<40", "40-64", ">=65"))',
      consejos = c(
        "La PRIMERA categoría es la categoría de referencia en los modelos de regresión (lineal, logística...): elige como primera la que quieras usar como comparación (p. ej. 'No expuesto').",
        "El orden también determina qué proporción calculan los tests de proporciones de R Commander (siempre la de la PRIMERA categoría) y cómo aparecen las categorías en tablas y gráficos.",
        "Un 'factor ordenado' no es solo un factor con otro orden: en los modelos de regresión R lo analiza con contrastes polinómicos (.L, .Q...) en lugar de comparar cada categoría con la de referencia. Si no es lo que buscas, no marques esa casilla.",
        "Reordenar no cambia los datos, solo el orden de las categorías."
      )
    ),

    # 4. Resumir una variable numérica ---------------------------------------
    resumir_numerica = list(
      titulo = "Resumir una variable numérica",
      ruta = "Estadísticos > Resúmenes > Resúmenes numéricos...",
      pasos = c(
        "Ve a: Estadísticos > Resúmenes > Resúmenes numéricos...",
        "En la pestaña 'Datos' selecciona la variable (o variables) numérica que quieres resumir.",
        "Si quieres los resultados separados por grupos (p. ej. por sexo), pulsa 'Resumir por grupos...' y selecciona la variable por la que quieres agrupar. Debe ser un factor.",
        "Para elegir qué estadísticos se calculan, pincha en la pestaña 'Opciones' y marca los que necesites: media, desviación típica, error típico, rango intercuartílico, coeficiente de variación, asimetría, apuntamiento y cuantiles.",
        "Si marcas 'Cuantiles', escribe en la casilla los percentiles que quieres separados por comas y expresados como proporción (ver ejemplos).",
        "Pulsa 'Aceptar'. En la salida, 'n' es el número de observaciones válidas y 'NA' el de valores perdidos."
      ),
      ejemplos = c(
        "0, .25, .5, .75, 1      # mínimo, P25, mediana, P75 y máximo (opción por defecto)",
        ".05, .5, .95            # P5, mediana y P95",
        ".1, .9                  # P10 y P90"
      ),
      codigo = c(
        'numSummary(datos[, "imc", drop = FALSE], groups = datos$sexo,',
        '           statistics = c("mean", "sd", "IQR", "quantiles"),',
        '           quantiles = c(0, .25, .5, .75, 1))'
      ),
      consejos = c(
        "Los percentiles se escriben como proporción, no como porcentaje: el percentil 90 es .9 (o 0.9), no 90.",
        "Los percentiles se separan con COMA y los decimales se escriben con PUNTO. '0,05' se leería como dos valores (0 y 05).",
        "El 50% (.5) es la mediana; 0 es el mínimo y 1 el máximo.",
        "Si la variable es asimétrica o tiene valores extremos, describe con mediana y rango intercuartílico en lugar de media y desviación típica.",
        "Si la variable que quieres usar para agrupar no aparece, no es un factor: conviértela antes (Datos > Modificar variables del conjunto de datos activo > Convertir variable numérica en factor...)."
      )
    ),

    # 5. Resumir una variable cualitativa -----------------------------------
    resumir_cualitativa = list(
      titulo = "Resumir una variable cualitativa",
      ruta = "Estadísticos > Resúmenes > Distribución de frecuencias...",
      pasos = c(
        "Ve a: Estadísticos > Resúmenes > Distribución de frecuencias...",
        "Selecciona la variable (o variables) cualitativa. Solo aparecen los factores.",
        "Deja sin marcar 'Test de bondad de ajuste chi-cuadrado', salvo que te pidan comparar la distribución observada con una teórica.",
        "Pulsa 'Aceptar'. Obtendrás las frecuencias absolutas (número de casos) y relativas (porcentajes) de cada categoría.",
        "Si además quieres un gráfico: Gráficas > Gráfica de barras..."
      ),
      ejemplos = character(0),
      codigo = c(
        "local({",
        "  .Table <- with(datos, table(fumador))",
        '  cat("\\ncounts:\\n"); print(.Table)',
        '  cat("\\npercentages:\\n"); print(round(100 * .Table / sum(.Table), 2))',
        "})"
      ),
      consejos = c(
        "Los porcentajes se calculan sobre los casos con dato: los valores perdidos (NA) no aparecen en la tabla ni cuentan en el denominador. Comprueba cuántos NA hay (Estadísticos > Resúmenes > Conjunto de datos activo) y decláralo al describir la variable.",
        "Si tu variable no aparece en la lista, está guardada como número (p. ej. 0/1): conviértela en factor antes (Datos > Modificar variables del conjunto de datos activo > Convertir variable numérica en factor...).",
        "Las categorías aparecen en orden alfabético. Si quieres otro orden, usa la opción 'Reordenar las categorías de un factor'.",
        "No resumas una variable cualitativa con media o desviación típica aunque esté codificada con números."
      )
    ),

    # 6. Media de una muestra ------------------------------------------------
    media_una_muestra = list(
      titulo = "Estimar la media para una muestra (test t para una muestra)",
      ruta = "Estadísticos > Medias > Test t para una muestra...",
      pasos = c(
        "Ve a: Estadísticos > Medias > Test t para una muestra...",
        "Selecciona la variable numérica.",
        "En 'Hipótesis alternativa' elige la que corresponde a tu pregunta: 'Media poblacional != mu0' (bilateral, ¿es distinta?), 'Media poblacional < mu0' (¿es menor?) o 'Media poblacional > mu0' (¿es mayor?).",
        "En 'Hipótesis nula: mu =' escribe el valor de referencia con el que quieres comparar la media (por defecto es 0, casi nunca es el valor correcto).",
        "Comprueba el 'Nivel de confianza' (por defecto .95).",
        "Pulsa 'Aceptar'. En la salida mira el valor p ('p-value'), la media muestral ('mean of x') y el intervalo de confianza ('confidence interval')."
      ),
      ejemplos = c(
        "Pregunta: ¿el IMC medio de la población es distinto de 25?",
        "  -> Hipótesis alternativa: Media poblacional != mu0;  Hipótesis nula: mu = 25",
        "Pregunta: ¿la presión sistólica media es mayor de 120 mmHg?",
        "  -> Hipótesis alternativa: Media poblacional > mu0;   Hipótesis nula: mu = 120"
      ),
      codigo = 'with(datos, t.test(imc, alternative = "two.sided", mu = 25, conf.level = .95))',
      consejos = c(
        "Si dejas 'mu = 0' estarás contrastando si la media es 0, lo que casi nunca tiene sentido. El valor de la hipótesis nula sale del enunciado o de un valor de referencia, nunca de tu propia muestra.",
        "Decide la hipótesis alternativa según la pregunta, antes de mirar los resultados. Si el enunciado no indica una dirección, usa la bilateral (!=).",
        "Si eliges una hipótesis unilateral (< o >), el intervalo de confianza también será unilateral (uno de sus extremos será -Inf o Inf). Para estimar la media con un intervalo de confianza habitual usa la bilateral.",
        "El intervalo de confianza y la media estimada no dependen del valor de mu0; el valor p sí.",
        "Si la muestra tiene menos de 30 observaciones, comprueba antes la normalidad de la variable (Estadísticos > Resúmenes > Test de normalidad...)."
      )
    ),

    # 7. Proporción de una muestra ------------------------------------------
    proporcion_una_muestra = list(
      titulo = "Estimar la proporción para una muestra (test de proporciones para una muestra)",
      ruta = "Estadísticos > Proporciones > Test de proporciones para una muestra...",
      pasos = c(
        "Antes de empezar: la variable debe ser un factor con DOS categorías, y R Commander calcula siempre la proporción de la PRIMERA categoría. Si la que te interesa (p. ej. 'Si') no es la primera, reordénala antes (opción 'Reordenar las categorías de un factor').",
        "Ve a: Estadísticos > Proporciones > Test de proporciones para una muestra...",
        "Selecciona la variable.",
        "En 'Hipótesis alternativa' elige la que corresponde a tu pregunta: 'Proporción poblacional != p0' (bilateral), '< p0' o '> p0'.",
        "En 'Hipótesis nula: p =' escribe la proporción de referencia, como proporción entre 0 y 1 (p. ej. .3 para un 30%). Por defecto es .5.",
        "Comprueba el 'Nivel de confianza' (por defecto .95) y el 'Tipo de prueba' (aproximación normal, aproximación normal con corrección de continuidad o binomial exacta).",
        "Pulsa 'Aceptar'. En la salida, comprueba primero en la tabla de frecuencias para qué categoría se ha hecho el test; después mira el valor p, la proporción muestral ('p') y el intervalo de confianza."
      ),
      ejemplos = c(
        "Pregunta: ¿la prevalencia de tabaquismo es distinta del 25%?",
        "  -> Hipótesis alternativa: != p0;  Hipótesis nula: p = .25",
        "Pregunta: ¿la cobertura vacunal es inferior al 95%?",
        "  -> Hipótesis alternativa: < p0;   Hipótesis nula: p = .95"
      ),
      codigo = c(
        "local({",
        "  .Table <- xtabs(~ fumador, data = datos)",
        "  print(rbind(.Table, percentage = round(100 * .Table / sum(.Table), 2)))",
        '  prop.test(rbind(.Table), alternative = "two.sided", p = .25,',
        "            conf.level = .95, correct = FALSE)",
        "})"
      ),
      consejos = c(
        "Si la categoría que te interesa no es la primera, obtendrás la proporción complementaria (p. ej. la de NO fumadores en lugar de la de fumadores) y el resultado se interpretará al revés.",
        "La proporción de la hipótesis nula se escribe entre 0 y 1 y con punto decimal: 25% es .25, no 25 ni 0,25.",
        "Decide la hipótesis alternativa según la pregunta, antes de mirar los resultados. Si el enunciado no indica una dirección, usa la bilateral (!=).",
        "Si eliges una hipótesis unilateral (< o >), el intervalo de confianza también será unilateral. Para estimar la proporción con un intervalo de confianza habitual usa la bilateral.",
        "Con muestras pequeñas o proporciones muy cercanas a 0 o a 1, la aproximación normal no es adecuada: usa la prueba binomial exacta."
      )
    ),

    # 8. Comparar dos medias independientes ---------------------------------
    dos_medias = list(
      titulo = "Comparar las medias de dos muestras independientes",
      ruta = c(
        "1) Estadísticos > Varianzas > Test de Levene...",
        "2) Estadísticos > Resúmenes > Test de normalidad...           (solo si algún grupo tiene n < 30)",
        "3) Estadísticos > Medias > Test t para muestras independientes...",
        "   o bien Estadísticos > Test no paramétricos > Test de Wilcoxon para dos muestras..."
      ),
      pasos = list(
        "PASO 1. Test de Levene (¿son iguales las varianzas?)" = c(
          "Ve a: Estadísticos > Varianzas > Test de Levene...",
          "En 'Factores' selecciona la variable que define los dos grupos (p. ej. sexo).",
          "En 'Variable explicada' selecciona la variable numérica (p. ej. imc).",
          "Deja 'Centro' en 'Mediana' (opción por defecto) y pulsa 'Aceptar'.",
          "Mira el valor p ('Pr(>F)'): si p >= 0.05 asumimos varianzas IGUALES; si p < 0.05, varianzas DISTINTAS. Apúntalo: lo necesitarás en el paso 3."
        ),
        "PASO 2. Normalidad (solo si algún grupo tiene menos de 30 observaciones)" = c(
          "Comprueba el tamaño de cada grupo (p. ej. con Estadísticos > Resúmenes > Resúmenes numéricos..., 'Resumir por grupos', columna 'n'). Si los dos grupos tienen 30 o más, salta al paso 3.",
          "Ve a: Estadísticos > Resúmenes > Test de normalidad...",
          "Selecciona la variable numérica y deja marcado el test de Shapiro-Wilk.",
          "Pulsa 'Test por grupos...' y selecciona la variable que define los grupos, para obtener un test en cada grupo.",
          "Pulsa 'Aceptar' y mira el valor p de cada grupo: si p >= 0.05 en ambos, asumimos normalidad; si p < 0.05 en alguno, NO asumimos normalidad."
        ),
        "PASO 3. Elegir y hacer el test" = c(
          "Si se asume normalidad (o ambos grupos tienen n >= 30): Estadísticos > Medias > Test t para muestras independientes...",
          "En 'Grupos' selecciona la variable de grupo y en 'Variable explicada' la variable numérica.",
          "En la pestaña 'Opciones' elige la 'Hipótesis alternativa' según tu pregunta (por defecto bilateral) y el nivel de confianza.",
          "En '¿Suponer varianzas iguales?' marca 'Sí' si el test de Levene dio p >= 0.05, o 'No' si dio p < 0.05 (en este caso R aplica la corrección de Welch).",
          "Pulsa 'Aceptar'. Mira el valor p, las medias de cada grupo y el intervalo de confianza de la DIFERENCIA de medias.",
          "Si NO se asume normalidad: Estadísticos > Test no paramétricos > Test de Wilcoxon para dos muestras..., selecciona grupo y variable, elige la hipótesis alternativa y pulsa 'Aceptar' (es la prueba U de Mann-Whitney)."
        )
      ),
      ejemplos = character(0),
      codigo = c(
        '# Paso 1',
        'leveneTest(imc ~ sexo, data = datos, center = "median")',
        '# Paso 2',
        'normalityTest(imc ~ sexo, test = "shapiro.test", data = datos)',
        '# Paso 3 (paramétrico; var.equal = TRUE si Levene p >= 0.05)',
        't.test(imc ~ sexo, alternative = "two.sided", conf.level = .95,',
        '       var.equal = FALSE, data = datos)',
        '# Paso 3 (no paramétrico)',
        'wilcox.test(imc ~ sexo, alternative = "two.sided", data = datos)'
      ),
      consejos = c(
        "La variable de grupo debe ser un factor con exactamente DOS categorías. Si tiene más, filtra antes la base o recodifica la variable.",
        "La diferencia de medias se calcula como PRIMERA categoría menos SEGUNDA (p. ej. Hombre - Mujer). Ten en cuenta el signo al interpretar el intervalo y al elegir una hipótesis alternativa unilateral.",
        "En el test de Levene la hipótesis nula es que las varianzas son iguales, y en el de Shapiro-Wilk que la distribución es normal: un p < 0.05 significa que se rechaza esa igualdad o esa normalidad.",
        "El tamaño de 30 se valora en CADA grupo, no en el total de la muestra.",
        "Si usas el test de Wilcoxon, describe los grupos con mediana y rango intercuartílico, no con media y desviación típica."
      )
    ),

    # 9. Comparar dos proporciones independientes ----------------------------
    dos_proporciones = list(
      titulo = "Comparar las proporciones de dos muestras independientes",
      ruta = c(
        "1) Estadísticos > Tablas de contingencia > Tabla de doble entrada...",
        "2) Estadísticos > Proporciones > Test de proporciones para dos muestras..."
      ),
      pasos = list(
        "ANTES DE EMPEZAR" = c(
          "Las dos variables deben ser factores con DOS categorías. R Commander calcula en cada grupo la proporción de la PRIMERA categoría de la variable explicada. Si la que te interesa (p. ej. 'Si') no es la primera, reordénala antes (opción 'Reordenar las categorías de un factor')."
        ),
        "PASO 1. Tabla de contingencia de doble entrada" = c(
          "Ve a: Estadísticos > Tablas de contingencia > Tabla de doble entrada...",
          "En la pestaña 'Datos', en 'Variable de filas' selecciona la variable que define las dos muestras (p. ej. sexo) y en 'Variable de columnas' la variable cuya proporción quieres comparar (p. ej. fumador).",
          "En la pestaña 'Estadísticos' selecciona 'Porcentajes por filas'.",
          "Pulsa 'Aceptar'. Obtendrás la tabla de frecuencias y, en cada fila (cada grupo), el porcentaje de cada categoría de la variable explicada. Esos porcentajes son las proporciones que vas a comparar en el paso 2."
        ),
        "PASO 2. Test de proporciones para dos muestras" = c(
          "Ve a: Estadísticos > Proporciones > Test de proporciones para dos muestras...",
          "En 'Grupos' selecciona la variable que define las dos muestras (p. ej. sexo).",
          "En 'Variable explicada' selecciona la variable cuya proporción quieres comparar (p. ej. fumador).",
          "En la pestaña 'Opciones' elige la 'Hipótesis alternativa' según tu pregunta (por defecto bilateral), el nivel de confianza y el 'Tipo de prueba' (aproximación normal, con o sin corrección de continuidad).",
          "Pulsa 'Aceptar'. Comprueba que las proporciones de la salida coinciden con los porcentajes por filas del paso 1 de la categoría que te interesa; después mira el valor p y el intervalo de confianza de la DIFERENCIA de proporciones."
        )
      ),
      ejemplos = character(0),
      codigo = c(
        "# Paso 1",
        "local({",
        "  .Table <- xtabs(~ sexo + fumador, data = datos)",
        "  print(.Table)",
        "  rowPercents(.Table)",
        "})",
        "# Paso 2",
        "local({",
        "  .Table <- xtabs(~ sexo + fumador, data = datos)",
        "  print(rowPercents(.Table))",
        '  prop.test(.Table, alternative = "two.sided", conf.level = .95, correct = FALSE)',
        "})"
      ),
      consejos = c(
        "Si la categoría de interés de la variable explicada no es la primera, compararás las proporciones complementarias (p. ej. de NO fumadores) y el signo de la diferencia cambiará.",
        "La diferencia se calcula como proporción del PRIMER grupo menos la del SEGUNDO. Ten en cuenta el signo al interpretar el intervalo y al elegir una hipótesis alternativa unilateral.",
        "Pon la variable de grupo en las FILAS de la tabla del paso 1. Si la pones en columnas, los porcentajes por filas no serán las proporciones de cada grupo.",
        "La tabla del paso 1 incluye por defecto un test chi-cuadrado ('Test de independencia chi-cuadrado'). Su valor p es el mismo que el del test de proporciones del paso 2 sin corrección de continuidad: son dos formas del mismo contraste.",
        "Si alguna frecuencia esperada es menor de 5, la aproximación normal no es adecuada: usa el test exacto de Fisher (Estadísticos > Tablas de contingencia > Tabla de doble entrada..., marcando 'Test exacto de Fisher')."
      )
    ),

    # 10. Correlación entre dos variables numéricas ----------------------------
    correlacion = list(
      titulo = "Estudiar la correlación entre dos variables numéricas (test de correlación)",
      ruta = c(
        "1) Estadísticos > Resúmenes > Test de normalidad...",
        "2) Estadísticos > Resúmenes > Test de correlación..."
      ),
      pasos = list(
        "PASO 1. Normalidad de cada variable" = c(
          "Ve a: Estadísticos > Resúmenes > Test de normalidad...",
          "Selecciona la PRIMERA de las dos variables y deja marcado el test de Shapiro-Wilk. No agrupes: aquí no hay grupos.",
          "Pulsa 'Aceptar' y apunta el valor p.",
          "Repite los pasos anteriores con la SEGUNDA variable.",
          "Si p >= 0.05 en las dos variables, asumimos normalidad. Si p < 0.05 en alguna de ellas, NO asumimos normalidad."
        ),
        "PASO 2. Elegir y hacer el test de correlación" = c(
          "Ve a: Estadísticos > Resúmenes > Test de correlación...",
          "Selecciona las dos variables (mantén pulsada la tecla Ctrl, o Cmd en Mac, para seleccionar la segunda).",
          "En 'Tipo de correlación' elige: 'Coeficiente de Pearson' si se asume normalidad en las dos variables, o 'Rho de Spearman' si alguna no es normal (o si alguna es ordinal).",
          "En 'Hipótesis alternativa' elige la que corresponde a tu pregunta: 'Bilateral' (¿hay correlación?), 'Correlación < 0' (¿es negativa?) o 'Correlación > 0' (¿es positiva?).",
          "Pulsa 'Aceptar'. Mira el coeficiente de correlación ('cor' en Pearson, 'rho' en Spearman), su signo y el valor p. Con Pearson también obtendrás el intervalo de confianza del coeficiente."
        )
      ),
      ejemplos = c(
        "Interpretación orientativa del valor absoluto del coeficiente (Regla de Evans 1996):",
        "  < 0.2 muy débil |  0.2 - 0.4 débil |  0.4 - 0.6  moderada |  0.6 - 0.8 fuerte  |  > 0.8 muy fuerte",
        "Signo positivo: cuando una variable aumenta, la otra también.",
        "Signo negativo: cuando una variable aumenta, la otra disminuye."
      ),
      codigo = c(
        "# Paso 1",
        'normalityTest(~ imc, test = "shapiro.test", data = datos)',
        'normalityTest(~ pas, test = "shapiro.test", data = datos)',
        "# Paso 2 (Pearson)",
        'with(datos, cor.test(imc, pas, alternative = "two.sided", method = "pearson"))',
        "# Paso 2 (Spearman)",
        'with(datos, cor.test(imc, pas, alternative = "two.sided", method = "spearman"))'
      ),
      consejos = c(
        "Antes del test conviene mirar el diagrama de dispersión (Gráficas > Diagrama de dispersión...). Pearson solo mide relación LINEAL: si la nube de puntos es curva o hay valores extremos, el coeficiente puede ser engañoso.",
        "El valor p solo indica si la correlación es distinta de 0, no si es fuerte. Con muestras grandes, correlaciones muy débiles salen significativas. Interpreta siempre el valor del coeficiente.",
        "Con Spearman es frecuente el aviso 'Cannot compute exact p-value with ties' (hay valores repetidos). No es un error: el resultado es válido y R usa un valor p aproximado.",
        "Correlación no implica causalidad: una correlación entre dos variables puede deberse a una tercera variable.",
        "Las observaciones con NA en cualquiera de las dos variables se excluyen del cálculo."
      )
    ),

    # 11. ANOVA de un factor ----------------------------------------------------
    anova = list(
      titulo = "Comparar las medias de tres o más grupos (ANOVA de un factor)",
      ruta = c(
        "1) Estadísticos > Resúmenes > Test de normalidad...           (solo si algún grupo tiene n < 30)",
        "2) Estadísticos > Varianzas > Test de Levene...",
        "3) Estadísticos > Medias > ANOVA de un factor...",
        "   o bien Estadísticos > Test no paramétricos > Test de Kruskal-Wallis..."
      ),
      pasos = list(
        "ANTES DE EMPEZAR" = c(
          "La variable explicativa (los grupos) debe ser un factor con TRES o más categorías. Si tiene dos, usa la opción 'Comparar las medias de dos muestras independientes'.",
          "Comprueba el tamaño de cada grupo (Estadísticos > Resúmenes > Resúmenes numéricos..., 'Resumir por grupos', columna 'n')."
        ),
        "PASO 1. Normalidad (solo si algún grupo tiene menos de 30 observaciones)" = c(
          "Si todos los grupos tienen 30 o más observaciones, salta al paso 2.",
          "Ve a: Estadísticos > Resúmenes > Test de normalidad...",
          "Selecciona la variable numérica y deja marcado el test de Shapiro-Wilk.",
          "Pulsa 'Test por grupos...' y selecciona la variable que define los grupos.",
          "Pulsa 'Aceptar' y mira el valor p de cada grupo: si p >= 0.05 en todos, asumimos normalidad; si p < 0.05 en alguno, NO asumimos normalidad y debes usar el test de Kruskal-Wallis (opción 'Pruebas no paramétricas')."
        ),
        "PASO 2. Test de Levene (¿son iguales las varianzas?)" = c(
          "Ve a: Estadísticos > Varianzas > Test de Levene...",
          "En 'Factores' selecciona la variable de grupo y en 'Variable explicada' la variable numérica.",
          "Deja 'Centro' en 'Mediana' y pulsa 'Aceptar'.",
          "Mira el valor p ('Pr(>F)'): si p >= 0.05 asumimos varianzas IGUALES; si p < 0.05, varianzas DISTINTAS. Lo necesitarás en el paso 3."
        ),
        "PASO 3. ANOVA de un factor" = c(
          "Ve a: Estadísticos > Medias > ANOVA de un factor...",
          "Deja el nombre del modelo que propone R Commander (p. ej. AnovaModel.1) o escribe otro.",
          "En 'Grupos' selecciona la variable de grupo y en 'Variable explicada' la variable numérica.",
          "Marca 'Comparaciones dos a dos de las medias' para obtener las comparaciones post hoc de Tukey (te dirán QUÉ grupos son distintos).",
          "Si el test de Levene dio p < 0.05, marca también 'Test F de Welch que no asume varianzas iguales'.",
          "Pulsa 'Aceptar'. Mira: la tabla ANOVA (estadístico 'F value' y valor p 'Pr(>F)'), las medias y desviaciones típicas de cada grupo y, si el ANOVA es significativo, las comparaciones dos a dos (diferencia, IC y valor p ajustado de cada par).",
          "Si marcaste el test de Welch, usa SU valor p (en lugar del de la tabla ANOVA) para decidir."
        )
      ),
      ejemplos = c(
        "Pregunta: ¿el IMC medio es distinto según el nivel de estudios (primarios, secundarios, universitarios)?",
        "  H0: las tres medias son iguales.  H1: al menos una media es distinta.",
        "  Si p < 0.05 -> mira las comparaciones dos a dos para saber qué pares de grupos difieren."
      ),
      codigo = c(
        "AnovaModel.1 <- aov(imc ~ estudios, data = datos)",
        "summary(AnovaModel.1)",
        'with(datos, numSummary(imc, groups = estudios, statistics = c("mean", "sd")))',
        "# Welch (si las varianzas son distintas)",
        "oneway.test(imc ~ estudios, data = datos)",
        "# Comparaciones dos a dos (Tukey)",
        "local({",
        '  .Pairs <- glht(AnovaModel.1, linfct = mcp(estudios = "Tukey"))',
        "  print(summary(.Pairs))",
        "  print(confint(.Pairs))",
        "})"
      ),
      consejos = c(
        "Un ANOVA significativo solo indica que AL MENOS una media es distinta, no cuál. Para eso están las comparaciones dos a dos.",
        "No sustituyas el ANOVA por varias pruebas t entre pares de grupos: al hacer muchas comparaciones aumenta la probabilidad de encontrar alguna diferencia por azar. Las comparaciones de Tukey ya corrigen ese problema (valores p ajustados).",
        "Si el ANOVA no es significativo, no interpretes las comparaciones dos a dos.",
        "Las comparaciones de Tukey suponen varianzas iguales. Si usaste el test de Welch, R Commander no ofrece una prueba post hoc específica: interpreta las comparaciones de Tukey con cautela.",
        "Si la variable de grupo está codificada con números (1, 2, 3), no aparecerá en 'Grupos': conviértela antes en factor (Datos > Modificar variables del conjunto de datos activo > Convertir variable numérica en factor...).",
        "Tamaño del efecto: eta cuadrado = suma de cuadrados del factor / suma de cuadrados total (columna 'Sum Sq' de la tabla ANOVA). Indica la proporción de la variabilidad explicada por los grupos."
      )
    ),

    # 12. Pruebas no paramétricas ---------------------------------------------
    no_parametricas = list(
      titulo = "Pruebas no paramétricas: Wilcoxon (dos grupos) y Kruskal-Wallis (tres o más grupos)",
      ruta = c(
        "Dos grupos:         Estadísticos > Test no paramétricos > Test de Wilcoxon para dos muestras...",
        "Tres o más grupos:  Estadísticos > Test no paramétricos > Test de Kruskal-Wallis..."
      ),
      pasos = list(
        "¿CUÁNDO SE USAN?" = c(
          "Para comparar una variable numérica (u ordinal) entre grupos INDEPENDIENTES cuando no se cumplen las condiciones de las pruebas paramétricas (prueba t o ANOVA).",
          "El caso habitual: algún grupo tiene menos de 30 observaciones y el test de normalidad (Shapiro-Wilk) da p < 0.05 en alguno de los grupos.",
          "También cuando la variable es ordinal (p. ej. una escala de 1 a 5) o hay valores extremos muy marcados.",
          "No comparan medias, sino si los valores de un grupo tienden a ser mayores que los de otro (comparan rangos). Por eso los grupos se describen con mediana y rango intercuartílico."
        ),
        "PASO 1. ¿Cuántas categorías tiene la variable explicativa?" = c(
          "Comprueba cuántos grupos tiene la variable explicativa (Estadísticos > Resúmenes > Distribución de frecuencias...).",
          "DOS categorías (p. ej. sexo)            -> Test de Wilcoxon para dos muestras (también llamado U de Mann-Whitney). Es la alternativa a la prueba t para muestras independientes. Sigue el paso 2A.",
          "TRES o más categorías (p. ej. estudios) -> Test de Kruskal-Wallis. Es la alternativa al ANOVA de un factor. Sigue el paso 2B."
        ),
        "PASO 2A. Dos grupos: test de Wilcoxon para dos muestras" = c(
          "Ve a: Estadísticos > Test no paramétricos > Test de Wilcoxon para dos muestras...",
          "En 'Grupos' selecciona la variable de grupo y en 'Variable explicada' la variable numérica.",
          "En la pestaña 'Opciones' elige la 'Hipótesis alternativa' según tu pregunta (por defecto bilateral) y deja 'Tipo de test' en 'Por defecto'.",
          "Pulsa 'Aceptar'. Mira las medianas de cada grupo, el estadístico W y el valor p ('p-value')."
        ),
        "PASO 2B. Tres o más grupos: test de Kruskal-Wallis" = c(
          "Ve a: Estadísticos > Test no paramétricos > Test de Kruskal-Wallis...",
          "En 'Grupos' selecciona la variable de grupo y en 'Variable explicada' la variable numérica.",
          "Pulsa 'Aceptar'. Mira las medianas de cada grupo, el estadístico ('Kruskal-Wallis chi-squared'), los grados de libertad ('df') y el valor p ('p-value').",
          "Si p < 0.05, al menos un grupo es distinto. Para saber cuáles, compara los grupos dos a dos con el test de Wilcoxon (paso 2A, filtrando antes la base para quedarte con cada par) y corrige los valores p: multiplica cada p por el número de comparaciones (corrección de Bonferroni)."
        )
      ),
      ejemplos = c(
        "Dos grupos:   ¿es distinta la puntuación de calidad de vida entre hombres y mujeres?  -> Wilcoxon",
        "Tres grupos:  ¿es distinta la estancia hospitalaria entre los hospitales A, B y C?      -> Kruskal-Wallis",
        "Bonferroni:   3 grupos = 3 comparaciones (A-B, A-C, B-C). Si una da p = 0.02 -> 0.02 x 3 = 0.06 (no significativa)."
      ),
      codigo = c(
        "# Dos grupos (Wilcoxon / Mann-Whitney)",
        "Tapply(imc ~ sexo, median, na.action = na.omit, data = datos)",
        'wilcox.test(imc ~ sexo, alternative = "two.sided", data = datos)',
        "# Tres o más grupos (Kruskal-Wallis)",
        "Tapply(imc ~ estudios, median, na.action = na.omit, data = datos)",
        "kruskal.test(imc ~ estudios, data = datos)",
        "# Comparaciones dos a dos (no lo genera R Commander; equivalente en código)",
        'pairwise.wilcox.test(datos$imc, datos$estudios, p.adjust.method = "bonferroni")'
      ),
      consejos = c(
        "Estas pruebas son para grupos INDEPENDIENTES. Si son mediciones repetidas en las mismas personas (antes/después), la prueba es otra (Test de Wilcoxon para muestras pareadas).",
        "No exigen normalidad, pero sí que las observaciones sean independientes.",
        "Describe los grupos con mediana y rango intercuartílico, no con media y desviación típica.",
        "Es frecuente el aviso 'cannot compute exact p-value with ties' (hay valores repetidos). No es un error: R usa un valor p aproximado.",
        "Si los datos cumplen las condiciones de la prueba paramétrica, usa la paramétrica: las no paramétricas tienen menos potencia (detectan peor las diferencias reales).",
        "La variable de grupo debe ser un factor. Si está codificada con números, conviértela antes (Datos > Modificar variables del conjunto de datos activo > Convertir variable numérica en factor...)."
      )
    )
  )
}
