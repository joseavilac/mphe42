#' Prueba de Wilcoxon (U de Mann-Whitney) para dos muestras independientes
#'
#' Versión interactiva y comentada del test de Wilcoxon para dos muestras de
#' R Commander (Estadísticos > Test no paramétricos > Test de Wilcoxon para
#' dos muestras), basada en \code{wilcox.test()}. Es la prueba U de
#' Mann-Whitney. Pregunta por consola el data.frame, la variable
#' cuantitativa, la variable de grupo (con dos categorías) y su orden, la
#' hipótesis alternativa y el tipo de prueba (por defecto, exacta o
#' aproximación normal con o sin corrección de continuidad), y presenta un
#' informe completo: medianas, rangos intercuartílicos y rangos medios por
#' grupo, condiciones de aplicación (con la comprobación de la normalidad
#' por grupo, ajustada por Holm, para valorar si la prueba t era
#' aplicable), hipótesis, cálculo del estadístico W, valor p, decisión y una
#' frase de redacción de resultados.
#'
#' @param alpha Nivel de significación. Por defecto 0.05.
#' @return De forma invisible, una lista con los descriptivos por grupo, el
#'   resultado de \code{wilcox.test()}, el método aplicado, la comprobación
#'   de la normalidad y \code{rechaza_H0}.
#' @export
#' @importFrom stats wilcox.test median quantile qnorm
#' @importFrom utils menu
np_mw <- function(alpha = 0.05) {

  if (!interactive())
    stop("np_mw() es interactiva: ejec\u00fatala desde la consola de R.",
         call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
    stop("'alpha' debe ser un n\u00famero entre 0 y 1.", call. = FALSE)

  ## 1. Base de datos -----------------------------------------------------
  sel <- .elegir_df()
  if (is.null(sel)) return(.cancelar())
  nombre_df <- sel$nombre
  datos <- sel$datos

  ## 2. Variable cuantitativa ---------------------------------------------
  var_y <- .pedir_variable(datos, "cuantitativa",
    "\u00bfQu\u00e9 variable cuantitativa (u ordinal codificada con n\u00fameros) quieres comparar entre los dos grupos?")
  if (is.null(var_y)) return(.cancelar())

  ## 3. Variable de grupo (2 categorías) y orden de los grupos ----------
  grp <- .pedir_dos_grupos(datos, var_y,
    "\u00bfQu\u00e9 variable define los dos grupos?", "medianas",
    extra_mas_de_dos = paste0(" Para comparar tres o m\u00e1s grupos se usa ",
                              "la prueba de Kruskal-Wallis (np_kw())."))
  if (is.null(grp)) return(.cancelar())
  var_g <- grp$var_g
  gA <- grp$gA
  gB <- grp$gB
  invertido <- grp$invertido

  ## 4. Hipótesis alternativa --------------------------------------------
  meA <- paste0("Me[", gA, "]")
  meB <- paste0("Me[", gB, "]")
  i <- menu(c(paste0("Bilateral:   los valores de '", gA, "' tienden a ser ",
                     "distintos de los de '", gB, "'  (", meA, " \u2260 ", meB, ")"),
              paste0("Unilateral:  los valores de '", gA, "' tienden a ser ",
                     "menores que los de '", gB, "'  (", meA, " < ", meB, ")"),
              paste0("Unilateral:  los valores de '", gA, "' tienden a ser ",
                     "mayores que los de '", gB, "'  (", meA, " > ", meB, ")")),
            title = "\n\u00bfCu\u00e1l es la hip\u00f3tesis alternativa (H1)?")
  if (i == 0) return(.cancelar())
  alternativa <- c("two.sided", "less", "greater")[i]
  simbolo <- c("\u2260", "<", ">")[i]
  sentido <- c("distintos de", "menores que", "mayores que")[i]
  simbolo_H0 <- c("=", "\u2265", "\u2264")[i]
  sentido_H0 <- c("iguales a", "mayores o iguales que",
                  "menores o iguales que")[i]

  ## 5. Datos ---------------------------------------------------------------
  y_todo <- datos[[var_y]]
  g_todo <- datos[[var_g]]
  completos <- !is.na(y_todo) & !is.na(g_todo)
  n_excl <- sum(!completos)
  y <- y_todo[completos]
  g <- factor(g_todo[completos], levels = c(gA, gB))
  yA <- y[g == gA]
  yB <- y[g == gB]
  nA <- length(yA)
  nB <- length(yB)
  N <- nA + nB
  if (nA < 1 || nB < 1)
    stop("Cada grupo necesita al menos 1 observaci\u00f3n.", call. = FALSE)
  if (length(unique(y)) == 1)
    stop("Todos los valores de '", var_y, "' son iguales: la prueba no ",
         "puede calcularse.", call. = FALSE)

  ## 6. Normalidad (para valorar si la prueba t era aplicable) y tipo de
  ##    prueba ----------------------------------------------------------
  norm <- .shapiro_grupos(list(yA, yB), alpha)
  rownames(norm) <- c(gA, gB)
  n_sw <- sum(!is.na(norm$p))
  t_aplicable <- all(norm$n >= 30 | (!is.na(norm$p) & !norm$no_normal))

  rangos <- rank(y)
  empates <- table(y)
  hay_empates <- any(empates > 1)
  exacta_defecto <- nA < 50 && nB < 50 && !hay_empates

  cat("\nComprobaci\u00f3n de la normalidad en cada grupo (Shapiro-Wilk; p ",
      "ajustados por Holm):\n", sep = "")
  for (j in 1:2) .imprimir(.texto_shapiro(rownames(norm)[j], norm$n[j],
                                          norm$p[j], norm$p_holm[j], alpha),
                           sangria = 4)
  if (t_aplicable) {
    .imprimir(paste0(
      "ATENCI\u00d3N: en los dos grupos se puede asumir normalidad o n >= 30, ",
      "as\u00ed que se cumplen las condiciones de la prueba t, que es preferible ",
      "(tiene m\u00e1s potencia). Considera media_2muestra() (en R Commander: ",
      "Estad\u00edsticos > Medias > Test t para muestras independientes). Si ",
      "contin\u00faas, el informe lo advertir\u00e1. Excepci\u00f3n: si la variable es ordinal (p. ej. una escala de 1 a 5), la prueba no param\u00e9trica es la adecuada."))
  } else {
    .imprimir(paste0(
      "Alg\u00fan grupo con n < 30 no sigue una distribuci\u00f3n normal: la prueba ",
      "de Mann-Whitney es la adecuada."))
  }
  if (hay_empates)
    .imprimir(paste0("Hay valores repetidos (empates): no se puede calcular ",
                     "el valor p exacto."))
  i <- menu(c(paste0("Por defecto: exacta si ambos grupos tienen n < 50 y no ",
                     "hay empates; si no, aproximaci\u00f3n normal con correcci\u00f3n ",
                     "de continuidad   <-- RECOMENDADO"),
              paste0("Exacta", if (hay_empates)
                " (no es posible con empates: R usar\u00e1 la aproximaci\u00f3n normal)"
                else ""),
              "Aproximaci\u00f3n normal",
              "Aproximaci\u00f3n normal con correcci\u00f3n de continuidad"),
            title = "\n\u00bfQu\u00e9 tipo de prueba quieres aplicar? (en R Commander: 'Tipo de test')")
  if (i == 0) return(.cancelar())
  tipo <- c("default", "exact", "normal", "correct")[i]
  exacta <- switch(tipo, default = exacta_defecto, exact = !hay_empates,
                   normal = FALSE, correct = FALSE)
  correccion <- switch(tipo, default = !exacta_defecto, exact = hay_empates,
                       normal = FALSE, correct = TRUE)
  ## Lo que pasa realmente a wilcox.test() (igual que la orden de Rcmdr)
  arg_exact <- switch(tipo, default = NULL, exact = TRUE, normal = FALSE,
                      correct = FALSE)
  arg_correct <- switch(tipo, default = TRUE, exact = TRUE, normal = FALSE,
                        correct = TRUE)

  ## 7. Cálculos ------------------------------------------------------------
  resumen <- function(x, r) {
    q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
    c(n = length(x), mediana = q[2], p25 = q[1], p75 = q[3], min = min(x),
      max = max(x), suma_rangos = sum(r), rango_medio = mean(r))
  }
  dA <- resumen(yA, rangos[g == gA])
  dB <- resumen(yB, rangos[g == gB])
  dif_med <- unname(dA["mediana"] - dB["mediana"])

  prueba <- suppressWarnings(
    wilcox.test(y ~ g, alternative = alternativa,
                exact = arg_exact, correct = arg_correct))
  W <- unname(prueba$statistic)
  p <- prueba$p.value
  rechaza <- p < alpha

  ## Aproximación normal (con corrección por empates), como wilcox.test()
  media_W <- nA * nB / 2
  sd_W <- sqrt(nA * nB / 12 *
               ((N + 1) - sum(empates^3 - empates) / (N * (N - 1))))
  corr_cc <- if (!correccion) 0 else switch(alternativa,
    two.sided = sign(W - media_W) * 0.5, greater = 0.5, less = -0.5)
  z <- (W - media_W - corr_cc) / sd_W
  z_crit <- switch(alternativa, two.sided = qnorm(1 - alpha / 2),
                   less = qnorm(alpha), greater = qnorm(1 - alpha))

  nombre_metodo <- if (exacta) "exacta" else if (correccion)
    "aproximaci\u00f3n normal con correcci\u00f3n de continuidad" else
    "aproximaci\u00f3n normal sin correcci\u00f3n de continuidad"

  ## Formato
  f3 <- function(x) .fmt_num(x, 3)
  f1 <- function(x) .fmt_num(x, 1)
  linea <- paste(rep("-", 70), collapse = "")
  seccion <- function(titulo) cat("\n", linea, "\n", titulo, "\n\n", sep = "")

  ## 8. Informe -------------------------------------------------------------
  cat("\n", linea, "\n", sep = "")
  cat("Prueba de Wilcoxon para dos muestras independientes (U de Mann-Whitney)\n")
  cat("Variable: ", var_y, "   Grupos: ", var_g, " (", gA, " / ", gB,
      ")   Base de datos: ", nombre_df, "\n", sep = "")
  cat("M\u00e9todo: ", nombre_metodo, "   Nivel de significaci\u00f3n: alfa = ", alpha,
      "\n", sep = "")
  if (n_excl > 0)
    cat("Se excluye", if (n_excl == 1) " 1 observaci\u00f3n" else
          paste0("n ", n_excl, " observaciones"), " con '", var_y, "' o '",
        var_g, "' perdido.\n", sep = "")

  ## 8.1 Descriptivos
  seccion("1. Descripci\u00f3n de los grupos")
  fila <- function(dd) c(dd["n"], f3(dd["mediana"]),
                         paste0(f3(dd["p25"]), " - ", f3(dd["p75"])),
                         paste0(f3(dd["min"]), " - ", f3(dd["max"])),
                         f1(dd["suma_rangos"]), f3(dd["rango_medio"]))
  desc <- data.frame(fila(dA), fila(dB),
    row.names = c("n (valores v\u00e1lidos)", "Mediana",
                  "Rango intercuart\u00edlico (P25 - P75)", "M\u00ednimo - M\u00e1ximo",
                  "Suma de rangos", "Rango medio"))
  names(desc) <- c(gA, gB)
  print(desc)
  cat("\n")
  .imprimir(paste0("Diferencia de medianas observada (", gA, " - ", gB, "): ",
                   f3(dA["mediana"]), " - ", f3(dB["mediana"]), " = ",
                   f3(dif_med), "."))
  .imprimir(paste0(
    "La prueba no usa los valores originales sino sus rangos: se ordenan ",
    "todas las observaciones de menor a mayor (1 = la menor) y se compara ",
    "el rango medio de cada grupo. Un rango medio mayor indica que los ",
    "valores de ese grupo tienden a ser m\u00e1s altos. A los valores repetidos ",
    "se les asigna el promedio de sus rangos. Describe los grupos con ",
    "mediana y rango intercuart\u00edlico, no con media y desviaci\u00f3n t\u00edpica."))

  ## 8.2 Condiciones de aplicación
  seccion("2. Condiciones de aplicaci\u00f3n")
  .imprimir(paste0(
    "a) Independencia: los dos grupos deben estar formados por individuos ",
    "distintos y no emparejados (si son las mismas personas medidas dos ",
    "veces, corresponde el test de Wilcoxon para muestras pareadas). No se ",
    "puede comprobar con los datos; depende del dise\u00f1o."))
  cat("\n")
  cat("  b) \u00bfEra aplicable la prueba t? Normalidad en cada grupo (Shapiro-Wilk):\n")
  for (j in 1:2) .imprimir(.texto_shapiro(rownames(norm)[j], norm$n[j],
                                          norm$p[j], norm$p_holm[j], alpha),
                           sangria = 4)
  .imprimir(.nota_holm(n_sw), sangria = 4)
  if (t_aplicable) {
    .imprimir(paste0(
      "ATENCI\u00d3N: se cumplen las condiciones de la prueba t (normalidad o ",
      "n >= 30 en ambos grupos). La prueba t tiene m\u00e1s potencia y es la ",
      "recomendada: media_2muestra() o, en R Commander, Estad\u00edsticos > ",
      "Medias > Test t para muestras independientes. Mann-Whitney sigue ",
      "siendo v\u00e1lida, pero detecta peor las diferencias reales. Si la variable es ordinal (p. ej. una escala de 1 a 5), la no param\u00e9trica es la adecuada aunque se cumplan esas condiciones."),
      sangria = 4)
  } else {
    .imprimir(paste0(
      "Hay alg\u00fan grupo con n < 30 en el que no se puede asumir normalidad ",
      "(", paste0("'", rownames(norm)[norm$no_normal & norm$n < 30], "'",
                  collapse = ", "),
      "): la prueba t no es fiable y Mann-Whitney es la adecuada."),
      sangria = 4)
  }
  cat("\n")
  .imprimir(paste0(
    "c) Forma de las distribuciones: la prueba no exige normalidad. Si las ",
    "distribuciones de los dos grupos tienen una forma parecida (solo ",
    "desplazadas), el resultado se puede interpretar como una comparaci\u00f3n ",
    "de medianas. Si la forma o la dispersi\u00f3n son muy distintas, la prueba ",
    "compara si los valores de un grupo tienden a ser mayores que los del ",
    "otro, no las medianas. Comp\u00e1ralo con un diagrama de caja por grupos."))
  cat("\n")
  .imprimir(paste0(
    "d) M\u00e9todo: ", nombre_metodo, ". ",
    switch(tipo,
      default = paste0("Es la opci\u00f3n por defecto: R usa la prueba exacta si ",
                       "ambos grupos tienen n < 50 y no hay empates, y la ",
                       "aproximaci\u00f3n normal con correcci\u00f3n de continuidad en ",
                       "los dem\u00e1s casos."),
      exact = if (hay_empates) paste0("ATENCI\u00d3N: has pedido la prueba exacta, ",
                                      "pero hay empates y no se puede ",
                                      "calcular: R ha usado la aproximaci\u00f3n ",
                                      "normal (con correcci\u00f3n de continuidad).")
              else "Has elegido la prueba exacta.",
      normal = paste0("Has elegido la aproximaci\u00f3n normal sin correcci\u00f3n de ",
                      "continuidad.",
                      if (exacta_defecto) paste0(" Con estos tama\u00f1os y sin ",
                        "empates, la opci\u00f3n por defecto (exacta) habr\u00eda sido ",
                        "preferible.") else ""),
      correct = paste0("Has elegido la aproximaci\u00f3n normal con correcci\u00f3n de ",
                       "continuidad.",
                       if (exacta_defecto) paste0(" Con estos tama\u00f1os y sin ",
                         "empates, la opci\u00f3n por defecto (exacta) habr\u00eda ",
                         "sido preferible.") else ""))))
  if (hay_empates)
    .imprimir(paste0(
      "Hay ", sum(empates > 1), " valores repetidos. En R Commander ver\u00e1s ",
      "el aviso 'cannot compute exact p-value with ties': no es un error, ",
      "R usa un valor p aproximado."), sangria = 4)

  ## 8.3 Hipótesis
  seccion("3. Hip\u00f3tesis")
  .imprimir(paste0("H0: ", meA, " ", simbolo_H0, " ", meB,
                   "   (los valores de '", var_y, "' en '", gA,
                   "' tienden a ser ", sentido_H0, " los de '", gB, "')"))
  .imprimir(paste0("H1: ", meA, " ", simbolo, " ", meB,
                   "   (los valores de '", var_y, "' en '", gA,
                   "' tienden a ser ", sentido, " los de '", gB, "')"))
  cat("\n")
  .imprimir(paste0(
    "Contraste ", if (alternativa == "two.sided") "bilateral" else "unilateral",
    ". Formalmente, H0 dice que las dos poblaciones tienen la misma ",
    "distribuci\u00f3n, de modo que un valor tomado al azar de '", gA, "' tiene ",
    "la misma probabilidad de ser mayor que uno de '", gB, "' que de ser ",
    "menor. Expresarlo con medianas (Me) es correcto si las distribuciones ",
    "tienen forma parecida (apartado 2c)."))
  if (alternativa != "two.sided") {
    .imprimir(paste0(
      "H0 recoge todos los valores que no est\u00e1n en H1. El estad\u00edstico y el ",
      "valor p se calculan en el l\u00edmite de H0 (", meA, " = ", meB, "), que ",
      "es el caso m\u00e1s desfavorable: si los datos permiten rechazar la ",
      "igualdad, tambi\u00e9n permiten rechazar cualquier otra situaci\u00f3n incluida ",
      "en H0."))
  }

  ## 8.4 Estadístico
  seccion("4. Estad\u00edstico de contraste")
  cat("  Suma de rangos de '", gA, "': R = ", f1(dA["suma_rangos"]), "\n",
      sep = "")
  cat("  W = R - nA x (nA + 1) / 2 = ", f1(dA["suma_rangos"]), " - ", nA,
      " x ", nA + 1, " / 2 = ", f1(W), "\n", sep = "")
  cat("  Valor esperado de W si H0 es cierta: nA x nB / 2 = ", nA, " x ", nB,
      " / 2 = ", f1(media_W), "\n", sep = "")
  .imprimir(paste0(
    "W (o U de Mann-Whitney) cuenta, de todos los pares formados por una ",
    "observaci\u00f3n de '", gA, "' y otra de '", gB, "' (", nA * nB, " pares), ",
    "en cu\u00e1ntos gana la de '", gA, "' (los empates cuentan 0.5). Si H0 es ",
    "cierta, gana en la mitad de los pares (", f1(media_W), "). Aqu\u00ed gana ",
    "en ", f1(W), " (", formatC(100 * W / (nA * nB), format = "f",
                                digits = 1), " %)."))
  cat("\n")
  if (exacta) {
    .imprimir(paste0(
      "Prueba exacta: el valor p se obtiene de la distribuci\u00f3n exacta de W ",
      "bajo H0 (todas las formas posibles de repartir los rangos entre los ",
      "dos grupos)."))
  } else {
    cat("  Desviaci\u00f3n t\u00edpica de W bajo H0",
        if (hay_empates) " (corregida por empates)" else "", ": ",
        f3(sd_W), "\n", sep = "")
    cat("  Z = (W - nA x nB / 2", if (correccion) paste0(" - ", f1(corr_cc))
        else "", ") / DE = (", f1(W), " - ", f1(media_W),
        if (correccion) paste0(" - ", f1(corr_cc)) else "", ") / ",
        f3(sd_W), " = ", f3(z), "\n", sep = "")
    cat("  Valor cr\u00edtico: z", switch(alternativa,
          two.sided = paste0("(", 1 - alpha / 2, ") = \u00b1", f3(abs(z_crit))),
          less = paste0("(", alpha, ") = ", f3(z_crit)),
          greater = paste0("(", 1 - alpha, ") = ", f3(z_crit))),
        "\n", sep = "")
    if (correccion)
      .imprimir(paste0("La correcci\u00f3n de continuidad (", f1(corr_cc),
                       ") ajusta que W solo toma valores discretos."))
  }

  ## 8.5 Valor p y decisión
  seccion("5. Valor p y decisi\u00f3n")
  cat("  Valor p: ", .fmt_p(p), "\n\n", sep = "")
  .imprimir(paste0(
    "Interpretaci\u00f3n del valor p: si los valores de '", var_y, "' tuvieran ",
    "la misma distribuci\u00f3n en los dos grupos",
    if (alternativa == "two.sided") "" else " (el l\u00edmite de H0)",
    ", la probabilidad de obtener una W ",
    switch(alternativa,
           two.sided = "tan alejada de su valor esperado como la observada (o m\u00e1s), en cualquier direcci\u00f3n,",
           less = "tan peque\u00f1a como la observada (o menor)",
           greater = "tan grande como la observada (o mayor)"),
    " ser\u00eda ", .fmt_p(p), "."))
  cat("\n")
  if (rechaza) {
    .imprimir(paste0(
      "Decisi\u00f3n: se rechaza H0 (p", .comparar_p(p), " < ", alpha, "). Hay ",
      "evidencia de que los valores de '", var_y, "' en '", gA, "' tienden ",
      "a ser ", sentido, " los de '", gB, "'."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No ",
      "hay evidencia suficiente para afirmar que los valores de '", var_y,
      "' en '", gA, "' tiendan a ser ", sentido, " los de '", gB, "'. Esto ",
      "no demuestra que H0 sea cierta (que las dos distribuciones sean ",
      "iguales)."))
  }
  if ((alternativa == "less" && W > media_W) ||
      (alternativa == "greater" && W < media_W)) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: el resultado va en la direcci\u00f3n contraria a la planteada en ",
      "H1 (el rango medio de '", gA, "' es ",
      if (W > media_W) "mayor" else "menor", " que el de '", gB, "'), por ",
      "eso el valor p es tan alto. La hip\u00f3tesis alternativa debe fijarse ",
      "antes de ver los datos; no la cambies a posteriori."))
  }
  cat("\n")
  .imprimir(paste0(
    "El valor p indica si la diferencia es estad\u00edsticamente significativa, ",
    "pero no si es importante: valora la relevancia con las medianas y los ",
    "rangos intercuart\u00edlicos de cada grupo (secci\u00f3n 1)."))

  ## 8.6 Redacción
  seccion("6. C\u00f3mo redactar el resultado")
  desc_g <- function(nom, dd) paste0(nom, " (mediana = ", f3(dd["mediana"]),
                                     "; RIC = ", f3(dd["p25"]), " - ",
                                     f3(dd["p75"]), "; n = ", dd["n"], ")")
  frase <- if (rechaza) {
    paste0("Los valores de ", var_y, " fueron significativamente ",
           if (W > media_W) "mayores" else "menores", " en el grupo ",
           desc_g(gA, dA), " que en el grupo ", desc_g(gB, dB))
  } else {
    paste0("No se observaron diferencias significativas en ", var_y,
           " entre el grupo ", desc_g(gA, dA), " y el grupo ", desc_g(gB, dB))
  }
  .imprimir(paste0(
    frase, " (prueba U de Mann-Whitney: W = ", f1(W), "; ", .p_redaccion(p),
    if (alternativa != "two.sided") ", contraste unilateral" else "", ")."))

  ## 8.7 Órdenes equivalentes
  cat("\n", linea, "\n", sep = "")
  .imprimir(paste0("En R Commander: Estad\u00edsticos > Test no param\u00e9tricos > ",
                   "Test de Wilcoxon para dos muestras... (pesta\u00f1a ",
                   "'Opciones': hip\u00f3tesis alternativa y 'Tipo de test')."),
            sangria = 0)
  cat("\u00d3rdenes equivalentes:\n")
  termino_g <- if (invertido)
    paste0("relevel(", var_g, ", ref=\"", gA, "\")") else var_g
  cat("  Tapply(", var_y, " ~ ", var_g, ", median, na.action=na.omit, data=",
      nombre_df, ") # medians by group\n", sep = "")
  cat("  wilcox.test(", var_y, " ~ ", termino_g, ", alternative=\"",
      alternativa, "\"", switch(tipo, default = "", exact = ", exact=TRUE",
                                normal = ", exact=FALSE, correct=FALSE",
                                correct = ", exact=FALSE, correct=TRUE"),
      ", data=", nombre_df, ")\n", sep = "")
  if (invertido) {
    cat("\n")
    .nota_orden_rcmdr(var_g, gA, gB)
  }
  if (!is.factor(g_todo)) {
    cat("\n")
    .imprimir(paste0(
      "'", var_g, "' no es un factor en '", nombre_df, "'. Para que ",
      "aparezca en 'Grupos' en R Commander, convi\u00e9rtela antes (Datos > ",
      "Modificar variables del conjunto de datos activo > Convertir variable ",
      "num\u00e9rica en factor...)."), sangria = 0)
  }
  cat(linea, "\n", sep = "")

  descriptivos <- rbind(dA, dB)
  rownames(descriptivos) <- c(gA, gB)
  invisible(list(
    descriptivos = descriptivos,
    prueba = prueba,
    metodo = nombre_metodo,
    normalidad = norm,
    rechaza_H0 = rechaza
  ))
}
