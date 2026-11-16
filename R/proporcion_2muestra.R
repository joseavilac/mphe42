#' Comparación interactiva de dos proporciones independientes
#'
#' Versión interactiva y comentada de la prueba de proporciones para dos
#' muestras de R Commander (Estadísticos > Proporciones > Test de
#' proporciones para dos muestras), basada en \code{prop.test()}
#' (aproximación normal con corrección de continuidad) y, como alternativa
#' exacta, en \code{fisher.test()} (prueba exacta de Fisher). Pregunta por
#' consola el data.frame, la variable de respuesta y la categoría cuya
#' proporción se compara, la variable de grupo y el orden de los grupos, la
#' hipótesis alternativa y el método, indicando cuándo se recomienda la
#' prueba exacta. Presenta un informe completo: tabla 2 x 2, condiciones de
#' aplicación, hipótesis, estadístico, decisión, intervalos de confianza,
#' medidas de efecto (diferencia de proporciones, riesgo relativo, odds
#' ratio, h de Cohen) y una frase de redacción de resultados.
#'
#' @param alpha Nivel de significación. El nivel de confianza de los
#'   intervalos es 1 - alpha. Por defecto 0.05.
#' @return De forma invisible, una lista con la tabla 2 x 2, el resultado de
#'   la prueba y las medidas de efecto.
#' @export
#' @importFrom stats prop.test fisher.test qnorm
#' @importFrom utils menu
proporcion_2muestra <- function(alpha = 0.05) {

  if (!interactive())
    stop("proporcion_2muestra() es interactiva: ejec\u00fatala desde la consola de R.",
         call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
    stop("'alpha' debe ser un n\u00famero entre 0 y 1.", call. = FALSE)
  conf <- 1 - alpha
  conf_txt <- paste0(round(100 * conf, 1), " %")

  ## 1. Base de datos -----------------------------------------------------
  sel <- .elegir_df()
  if (is.null(sel)) return(.cancelar())
  nombre_df <- sel$nombre
  datos <- sel$datos

  ## 2. Variable de respuesta y categoría --------------------------------
  var_y <- .pedir_variable(datos, "cualitativa",
    "\u00bfQu\u00e9 variable cualitativa (respuesta) quieres comparar entre los dos grupos?")
  if (is.null(var_y)) return(.cancelar())
  categoria <- .elegir_categoria(datos, var_y,
    paste0("\u00bfPara qu\u00e9 categor\u00eda de '", var_y,
           "' quieres calcular y comparar la proporci\u00f3n?"))
  if (is.null(categoria)) return(.cancelar())

  ## 3. Variable de grupo y orden de los grupos ---------------------------
  grp <- .pedir_dos_grupos(datos, var_y,
    "\u00bfQu\u00e9 variable define los dos grupos?", "proporciones",
    extra_mas_de_dos = paste0(" Para comparar m\u00e1s de dos proporciones se ",
                              "usa la prueba chi cuadrado en una tabla de ",
                              "contingencia."))
  if (is.null(grp)) return(.cancelar())
  var_g <- grp$var_g
  gA <- grp$gA
  gB <- grp$gB
  invertido <- grp$invertido

  ## 4. Hipótesis alternativa --------------------------------------------
  piA <- paste0("\u03c0[", gA, "]")
  piB <- paste0("\u03c0[", gB, "]")
  i <- menu(c(paste0("Bilateral:   la proporci\u00f3n en '", gA, "' es distinta de la de '",
                     gB, "'  (", piA, " \u2260 ", piB, ")"),
              paste0("Unilateral:  la proporci\u00f3n en '", gA, "' es menor que la de '",
                     gB, "'  (", piA, " < ", piB, ")"),
              paste0("Unilateral:  la proporci\u00f3n en '", gA, "' es mayor que la de '",
                     gB, "'  (", piA, " > ", piB, ")")),
            title = "\n\u00bfCu\u00e1l es la hip\u00f3tesis alternativa (H1)?")
  if (i == 0) return(.cancelar())
  alternativa <- c("two.sided", "less", "greater")[i]
  simbolo <- c("\u2260", "<", ">")[i]
  sentido <- c("distinta de", "menor que", "mayor que")[i]
  simbolo_H0 <- c("=", "\u2265", "\u2264")[i]
  sentido_H0 <- c("igual a", "mayor o igual que", "menor o igual que")[i]

  ## 5. Datos: tabla 2 x 2 ------------------------------------------------
  y_todo <- datos[[var_y]]
  g_todo <- datos[[var_g]]
  completos <- !is.na(y_todo) & !is.na(g_todo)
  n_excl <- sum(!completos)
  exito <- y_todo[completos] == categoria
  g <- factor(g_todo[completos], levels = c(gA, gB))
  niveles_y <- levels(droplevels(factor(y_todo[completos])))
  ## Nombre de la columna complementaria de la tabla 2 x 2
  otra <- if (length(niveles_y) == 2) setdiff(niveles_y, categoria) else "Resto"

  a <- sum(exito & g == gA)          # A con la categor\u00eda
  nA <- sum(g == gA)
  b <- nA - a                        # A sin la categor\u00eda
  c_ <- sum(exito & g == gB)         # B con la categor\u00eda
  nB <- sum(g == gB)
  d <- nB - c_                       # B sin la categor\u00eda
  N <- nA + nB
  if (a + c_ == 0 || b + d == 0)
    stop("Todas las observaciones ", if (a + c_ == 0) "no tienen" else "tienen",
         " la categor\u00eda '", categoria, "': no hay nada que comparar.",
         call. = FALSE)

  pA <- a / nA
  pB <- c_ / nB
  dif <- pA - pB
  p_comb <- (a + c_) / N

  ## 6. Elección del método ----------------------------------------------
  esperadas <- matrix(c(nA * p_comb, nB * p_comb,
                        nA * (1 - p_comb), nB * (1 - p_comb)), nrow = 2,
                      dimnames = list(c(gA, gB),
                                      c(categoria, otra)))
  exacta_rec <- any(esperadas < 5)
  f2 <- function(v) formatC(v, format = "f", digits = 2)
  ## Imprime una matriz como tabla de contingencia, con el nombre de la
  ## variable de grupo en el margen de las filas y el de la variable de
  ## respuesta en el de las columnas
  imprimir_tabla <- function(m, filas, columnas) {
    m <- matrix(as.character(m), nrow = nrow(m),
                dimnames = stats::setNames(list(filas, columnas),
                                           c(var_g, var_y)))
    print(as.table(m), quote = FALSE, right = TRUE)
  }

  cat("\nComprobaci\u00f3n para elegir el m\u00e9todo (frecuencias esperadas si H0 fuera cierta):\n\n")
  imprimir_tabla(f2(esperadas), c(gA, gB), c(categoria, otra))
  cat("\n")
  if (exacta_rec) {
    .imprimir(paste0("Alguna frecuencia esperada es < 5: la aproximaci\u00f3n ",
                     "normal no es fiable. Se recomienda la prueba exacta de ",
                     "Fisher."))
  } else {
    .imprimir(paste0("Todas las frecuencias esperadas son >= 5: la ",
                     "aproximaci\u00f3n normal es adecuada. La prueba exacta de ",
                     "Fisher tambi\u00e9n es v\u00e1lida."))
  }
  i <- menu(c("Aproximaci\u00f3n normal con correcci\u00f3n de continuidad",
              paste0("Exacta (prueba exacta de Fisher)",
                     if (exacta_rec) "   <-- RECOMENDADA" else "")),
            title = "\n\u00bfQu\u00e9 m\u00e9todo quieres utilizar?")
  if (i == 0) return(.cancelar())
  metodo <- c("normal", "exacta")[i]

  ## 7. Cálculos ------------------------------------------------------------
  tabla <- matrix(c(a, c_, b, d), nrow = 2,
                  dimnames = list(c(gA, gB), c(categoria, otra)))
  ## Prueba aproximada: se calcula siempre, porque da el IC de la diferencia
  ## (los avisos de R sobre la aproximación se sustituyen por la
  ## comprobación propia de frecuencias esperadas)
  pt <- suppressWarnings(prop.test(c(a, c_), c(nA, nB), alternative = alternativa,
                                   conf.level = conf, correct = TRUE))
  pt_bil <- suppressWarnings(prop.test(c(a, c_), c(nA, nB), conf.level = conf,
                                       correct = TRUE))
  ic_dif <- pt$conf.int
  ic_dif_bil <- pt_bil$conf.int

  ee0 <- sqrt(p_comb * (1 - p_comb) * (1 / nA + 1 / nB))
  cc <- 0.5 * (1 / nA + 1 / nB)
  z <- sign(dif) * max(0, abs(dif) - cc) / ee0
  chi2 <- unname(pt$statistic)
  z_crit <- switch(alternativa,
                   two.sided = qnorm(1 - alpha / 2),
                   less = qnorm(alpha),
                   greater = qnorm(1 - alpha))

  if (metodo == "normal") {
    prueba <- pt
    nombre_metodo <- "aproximaci\u00f3n normal con correcci\u00f3n de continuidad"
  } else {
    prueba <- fisher.test(tabla, alternative = alternativa, conf.level = conf)
    ft_bil <- fisher.test(tabla, conf.level = conf)
    nombre_metodo <- "prueba exacta de Fisher"
  }
  p <- prueba$p.value
  rechaza <- p < alpha

  ## Medidas de efecto
  z_ic <- qnorm(1 - alpha / 2)
  hay_cero <- any(c(a, b, c_, d) == 0)
  RR <- pA / pB
  OR <- (a * d) / (b * c_)
  if (!hay_cero) {
    ee_lrr <- sqrt(1 / a - 1 / nA + 1 / c_ - 1 / nB)
    ic_RR <- exp(log(RR) + c(-1, 1) * z_ic * ee_lrr)
    ee_lor <- sqrt(1 / a + 1 / b + 1 / c_ + 1 / d)
    ic_OR <- exp(log(OR) + c(-1, 1) * z_ic * ee_lor)
  } else {
    ic_RR <- ic_OR <- c(NA, NA)
  }
  h <- 2 * asin(sqrt(pA)) - 2 * asin(sqrt(pB))

  ## Formato
  f3 <- function(v) .fmt_num(v, 3)
  fpct <- function(v) paste0(formatC(100 * v, format = "f", digits = 1), " %")
  fpp <- function(v) formatC(100 * v, format = "f", digits = 1)
  linea <- paste(rep("-", 70), collapse = "")
  seccion <- function(titulo) cat("\n", linea, "\n", titulo, "\n\n", sep = "")

  ## 8. Informe -------------------------------------------------------------
  cat("\n", linea, "\n", sep = "")
  cat("Comparaci\u00f3n de dos proporciones independientes\n")
  cat("Respuesta: ", var_y, " (categor\u00eda '", categoria, "')   Grupos: ", var_g,
      " (", gA, " / ", gB, ")\n", sep = "")
  cat("Base de datos: ", nombre_df, "   M\u00e9todo: ", nombre_metodo,
      "   alfa = ", alpha, "\n", sep = "")
  if (n_excl > 0)
    cat("Se excluye", if (n_excl == 1) " 1 observaci\u00f3n" else
          paste0("n ", n_excl, " observaciones"), " con '", var_y, "' o '",
        var_g, "' perdido.\n", sep = "")

  ## 8.1 Descripción
  seccion("1. Descripci\u00f3n: tabla de contingencia 2 x 2")
  imprimir_tabla(cbind(c(a, c_, a + c_), c(b, d, b + d), c(nA, nB, N),
                       c(fpct(pA), fpct(pB), fpct(p_comb))),
                 c(gA, gB, "Total"),
                 c(categoria, otra, "Total", paste0("% ", categoria)))
  cat("\n")
  cat("  Proporci\u00f3n en '", gA, "': pA = ", a, " / ", nA, " = ", f3(pA),
      " (", fpct(pA), ")\n", sep = "")
  cat("  Proporci\u00f3n en '", gB, "': pB = ", c_, " / ", nB, " = ", f3(pB),
      " (", fpct(pB), ")\n", sep = "")
  cat("  Diferencia observada (", gA, " - ", gB, "): ", f3(dif), " (",
      fpp(dif), " puntos porcentuales)\n", sep = "")
  cat("\n")
  .imprimir(paste0("Las diferencias se calculan como '", gA, "' menos '", gB,
                   "', seg\u00fan el orden elegido. Una diferencia positiva ",
                   "significa que la proporci\u00f3n de '", categoria,
                   "' es mayor en '", gA, "'."))
  if (length(niveles_y) > 2) {
    .imprimir(paste0("'", var_y, "' tiene ", length(niveles_y), " categor\u00edas: ",
                     "se compara '", categoria, "' frente al resto de ",
                     "categor\u00edas juntas."))
  }

  ## 8.2 Condiciones
  seccion("2. Condiciones de aplicaci\u00f3n")
  .imprimir(paste0(
    "a) Independencia: los dos grupos deben estar formados por individuos ",
    "distintos y no emparejados (si son las mismas personas medidas dos ",
    "veces, o pares emparejados, corresponde la prueba de McNemar). No se ",
    "puede comprobar con los datos; depende del dise\u00f1o."))
  cat("\n")
  .imprimir(paste0(
    "b) Cada individuo pertenece a un solo grupo y tiene o no tiene la ",
    "categor\u00eda '", categoria, "'."))
  cat("\n")
  .imprimir(paste0(
    "c) Frecuencias esperadas bajo H0: la menor es ", f2(min(esperadas)), ". ",
    if (exacta_rec) "Hay alguna < 5, por lo que se recomienda la prueba exacta de Fisher."
    else "Todas son >= 5, por lo que la aproximaci\u00f3n normal es adecuada."))
  if (metodo == "normal" && exacta_rec) {
    .imprimir(paste0(
      "ATENCI\u00d3N: has elegido la aproximaci\u00f3n normal aunque no se cumplen sus ",
      "condiciones. El valor p puede ser poco fiable; repite el an\u00e1lisis con ",
      "la prueba exacta de Fisher."), sangria = 4)
  } else if (metodo == "exacta") {
    .imprimir(paste0(
      "La prueba exacta de Fisher no necesita esta condici\u00f3n: calcula las ",
      "probabilidades directamente (distribuci\u00f3n hipergeom\u00e9trica)."),
      sangria = 4)
  }

  ## 8.3 Hipótesis
  seccion("3. Hip\u00f3tesis")
  .imprimir(paste0("H0: ", piA, " ", simbolo_H0, " ", piB,
                   "   (la proporci\u00f3n poblacional de '", categoria, "' en '",
                   gA, "' es ", sentido_H0, " la de '", gB, "')"))
  .imprimir(paste0("H1: ", piA, " ", simbolo, " ", piB,
                   "   (la proporci\u00f3n poblacional de '", categoria, "' en '",
                   gA, "' es ", sentido, " la de '", gB, "')"))
  cat("\n")
  .imprimir(paste0("Contraste ", if (alternativa == "two.sided") "bilateral"
                   else "unilateral", ". Expresado como diferencia: H0: ",
                   piA, " - ", piB, " ", simbolo_H0, " 0 frente a H1: ",
                   piA, " - ", piB, " ", simbolo, " 0."))
  if (alternativa != "two.sided") {
    .imprimir(paste0(
      "H0 recoge todos los valores que no est\u00e1n en H1. El valor p se calcula ",
      "en el l\u00edmite de H0 (", piA, " = ", piB, "), que es el caso m\u00e1s ",
      "desfavorable: si los datos permiten rechazar la igualdad, tambi\u00e9n ",
      "permiten rechazar cualquier otra situaci\u00f3n incluida en H0."))
  }

  ## 8.4 Estadístico
  seccion("4. Estad\u00edstico de contraste")
  if (metodo == "normal") {
    cat("  Proporci\u00f3n combinada (bajo H0): p = (", a, " + ", c_, ") / ", N,
        " = ", f3(p_comb), "\n", sep = "")
    cat("  EE bajo H0 = \u221a(p(1 - p)(1/nA + 1/nB)) = ", f3(ee0), "\n", sep = "")
    cat("  Correcci\u00f3n de continuidad = \u00bd(1/nA + 1/nB) = ",
        formatC(cc, format = "f", digits = 4), "\n", sep = "")
    cat("  z = (|pA - pB| - correcci\u00f3n) / EE = (", f3(abs(dif)), " - ",
        formatC(cc, format = "f", digits = 4), ") / ", f3(ee0), " = ",
        f3(abs(z)), "\n", sep = "")
    if (z < 0)
      cat("  Con el signo de pA - pB (la proporci\u00f3n de '", gA,
          "' es menor): z = ", f3(z), "\n", sep = "")
    cat("  Valor cr\u00edtico: z", switch(alternativa,
          two.sided = paste0("(", 1 - alpha / 2, ") = \u00b1", f3(abs(z_crit))),
          less = paste0("(", alpha, ") = ", f3(z_crit)),
          greater = paste0("(", 1 - alpha, ") = ", f3(z_crit))), "\n", sep = "")
    cat("\n")
    .imprimir(paste0(
      "El estad\u00edstico z indica a cu\u00e1ntos errores est\u00e1ndar est\u00e1 la diferencia ",
      "observada de una diferencia nula. R (y R Commander) lo presentan como ",
      "chi cuadrado con 1 grado de libertad: X\u00b2 = z\u00b2 = ", f3(chi2), "."))
  } else {
    cat("  Tabla observada: ", a, " y ", b, " en '", gA, "'; ", c_, " y ", d,
        " en '", gB, "'\n", sep = "")
    cat("  Totales fijos: ", nA, " y ", nB, " por grupo; ", a + c_, " con '",
        categoria, "' en total\n", sep = "")
    cat("\n")
    .imprimir(paste0(
      "La prueba exacta de Fisher no usa un estad\u00edstico aproximado: ",
      "manteniendo fijos los totales de filas y columnas, calcula la ",
      "probabilidad exacta de obtener una tabla como la observada o m\u00e1s ",
      "extrema si las proporciones de los dos grupos fueran iguales."))
  }

  ## 8.5 Valor p y decisión
  seccion("5. Valor p y decisi\u00f3n")
  cat("  Valor p: ", .fmt_p(p), "\n\n", sep = "")
  .imprimir(paste0(
    "Interpretaci\u00f3n del valor p: si las proporciones poblacionales de los dos ",
    "grupos fueran iguales",
    if (alternativa == "two.sided") "" else " (el l\u00edmite de H0)",
    ", la probabilidad de obtener una diferencia ",
    switch(alternativa,
           two.sided = "tan grande como la observada (o mayor), en cualquier direcci\u00f3n,",
           less = paste0("a favor de '", gB, "' tan grande como la observada (o mayor)"),
           greater = paste0("a favor de '", gA, "' tan grande como la observada (o mayor)")),
    " ser\u00eda ", .fmt_p(p), "."))
  cat("\n")
  if (rechaza) {
    .imprimir(paste0(
      "Decisi\u00f3n: se rechaza H0 (p", .comparar_p(p), " < ", alpha, "). Hay ",
      "evidencia de que la proporci\u00f3n poblacional de '", categoria, "' en '",
      gA, "' es ", sentido, " la de '", gB, "'."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No ",
      "hay evidencia suficiente para afirmar que la proporci\u00f3n poblacional de '",
      categoria, "' en '", gA, "' sea ", sentido, " la de '", gB, "'. Esto no ",
      "demuestra que H0 sea cierta (que sea ", sentido_H0, " la de '", gB,
      "')."))
  }
  if ((alternativa == "less" && dif > 0) ||
      (alternativa == "greater" && dif < 0)) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: la diferencia observada (", f3(dif), ") va en la direcci\u00f3n ",
      "contraria a la planteada en H1, por eso el valor p es tan alto. La ",
      "hip\u00f3tesis alternativa debe fijarse antes de ver los datos; no la ",
      "cambies a posteriori."))
  }

  ## 8.6 Intervalo de confianza
  seccion(paste0("6. Intervalo de confianza al ", conf_txt,
                 " para la diferencia de proporciones (", gA, " - ", gB, ")"))
  cat("  M\u00e9todo: aproximaci\u00f3n normal con correcci\u00f3n de continuidad\n")
  if (alternativa == "two.sided") {
    cat("  IC ", conf_txt, ": ", f3(ic_dif[1]), " a ", f3(ic_dif[2]), "  (",
        fpp(ic_dif[1]), " a ", fpp(ic_dif[2]), " puntos porcentuales)\n\n",
        sep = "")
    .imprimir(paste0(
      "Con una confianza del ", conf_txt, ", la diferencia entre la proporci\u00f3n ",
      "poblacional de '", categoria, "' en '", gA, "' y en '", gB,
      "' est\u00e1 entre ", fpp(ic_dif[1]), " y ", fpp(ic_dif[2]),
      " puntos porcentuales."))
  } else {
    limite <- if (alternativa == "less") ic_dif[2] else ic_dif[1]
    cat("  IC unilateral ", conf_txt, ": ",
        if (alternativa == "less") paste0("-1 a ", f3(limite))
        else paste0(f3(limite), " a 1"), "\n", sep = "")
    cat("  IC bilateral ", conf_txt, ":  ", f3(ic_dif_bil[1]), " a ",
        f3(ic_dif_bil[2]), "  (", fpp(ic_dif_bil[1]), " a ", fpp(ic_dif_bil[2]),
        " puntos porcentuales)\n\n", sep = "")
    .imprimir(paste0(
      "Con una confianza del ", conf_txt, ", la diferencia de proporciones es ",
      if (alternativa == "less") "menor que " else "mayor que ", f3(limite),
      " (", fpp(limite), " puntos porcentuales). Es el intervalo que ",
      "corresponde a un contraste unilateral; en las publicaciones suele ",
      "darse el bilateral."))
  }
  if (metodo == "normal") {
    dentro <- 0 >= ic_dif[1] && 0 <= ic_dif[2]
    if (dentro != rechaza) {
      .imprimir(paste0(
        "El 0 (ninguna diferencia) ", if (dentro) "queda dentro" else
        "queda fuera", " del intervalo, coherente con que ",
        if (rechaza) "se rechace" else "no se rechace", " H0."))
    } else {
      .imprimir(paste0(
        "El 0 ", if (dentro) "queda dentro" else "queda fuera",
        " del intervalo aunque ", if (rechaza) "se rechaza" else
        "no se rechaza", " H0. Ocurre en casos l\u00edmite porque el intervalo y ",
        "la prueba se calculan de forma ligeramente distinta; la decisi\u00f3n se ",
        "toma con el valor p."))
    }
  } else {
    ic_or_f <- prueba$conf.int
    cat("\n")
    .imprimir(paste0(
      "La prueba de Fisher no da un intervalo para la diferencia de ",
      "proporciones; el anterior es aproximado y solo orientativo. Su ",
      "intervalo exacto es para la odds ratio: ",
      f3(unname(prueba$estimate)), " (IC ", conf_txt, ": ", f3(ic_or_f[1]),
      " a ", if (is.finite(ic_or_f[2])) f3(ic_or_f[2]) else "Inf",
      "). Esta odds ratio es la estimaci\u00f3n condicional de Fisher y puede ",
      "diferir ligeramente de la odds ratio muestral de la secci\u00f3n 7."))
    dentro <- 1 >= ic_or_f[1] && 1 <= ic_or_f[2]
    .imprimir(paste0(
      "El 1 (ninguna asociaci\u00f3n) ", if (dentro) "queda dentro" else
      "queda fuera", " de este intervalo exacto",
      if (dentro != rechaza) paste0(", coherente con que ",
        if (rechaza) "se rechace" else "no se rechace", " H0.")
      else ". Ocurre en casos l\u00edmite; la decisi\u00f3n se toma con el valor p."))
  }

  ## 8.7 Tamaño del efecto
  seccion("7. Medidas de efecto")
  .imprimir(paste0("Diferencia de proporciones (", gA, " - ", gB, "): ",
                   fpp(dif), " puntos porcentuales (IC ", conf_txt,
                   " bilateral: ", fpp(ic_dif_bil[1]), " a ", fpp(ic_dif_bil[2]),
                   ")"))
  if (!hay_cero) {
    .imprimir(paste0("Riesgo relativo (RR = pA / pB): ", f3(RR), " (IC ",
                     conf_txt, ": ", f3(ic_RR[1]), " a ", f3(ic_RR[2]), ")"))
    .imprimir(paste0("Odds ratio (OR = (a x d) / (b x c)): ", f3(OR), " (IC ",
                     conf_txt, ": ", f3(ic_OR[1]), " a ", f3(ic_OR[2]), ")"))
  } else {
    .imprimir(paste0("Riesgo relativo (RR = pA / pB): ",
                     if (is.finite(RR)) f3(RR) else "no calculable",
                     ". Odds ratio: ", if (is.finite(OR) && OR > 0) f3(OR)
                     else "no calculable", ". Hay alguna celda con 0, por lo ",
                     "que no se pueden calcular sus intervalos de confianza."))
  }
  mag <- if (abs(h) < 0.2) "despreciable" else if (abs(h) < 0.5) "peque\u00f1o"
         else if (abs(h) < 0.8) "mediano" else "grande"
  .imprimir(paste0("h de Cohen = 2\u00b7arcsen(\u221apA) - 2\u00b7arcsen(\u221apB) = ",
                   f3(h), "  (efecto ", mag, ")"))
  cat("\n")
  if (!hay_cero) {
    .imprimir(paste0(
      "Interpretaci\u00f3n del RR: la proporci\u00f3n de '", categoria, "' en '", gA,
      "' es ", f3(RR), " veces la de '", gB, "'",
      if (RR > 1) paste0(" (un ", fpp(RR - 1), " % mayor)")
      else if (RR < 1) paste0(" (un ", fpp(1 - RR), " % menor)") else "",
      ". En un estudio transversal es una raz\u00f3n de prevalencias. Un ",
      "intervalo del RR o de la OR que no incluye el 1 indica asociaci\u00f3n en ",
      "un contraste bilateral; estos intervalos son aproximados (m\u00e9todo ",
      "logar\u00edtmico) y en casos l\u00edmite, o con contrastes unilaterales, ",
      "pueden no coincidir con la prueba. La OR se aproxima al RR solo cuando ",
      "la categor\u00eda es poco frecuente (< 10 %)."))
    cat("\n")
  }
  .imprimir(paste0(
    "El valor p indica si la diferencia es estad\u00edsticamente significativa, ",
    "pero no si es importante. Juzga la relevancia con la diferencia en ",
    "puntos porcentuales, el RR y sus intervalos. Los umbrales de Cohen ",
    "para h (0.2 peque\u00f1o, 0.5 mediano, 0.8 grande) son solo orientativos."))
  if (rechaza && abs(h) < 0.2) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: la diferencia es significativa pero muy peque\u00f1a. Con ",
      "muestras grandes, diferencias sin relevancia pr\u00e1ctica pueden ",
      "resultar significativas."))
  }
  if (!rechaza && abs(h) >= 0.5) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: el efecto observado no es peque\u00f1o, pero no es significativo ",
      "(n = ", nA, " y ", nB, "). Puede deberse a poca potencia, no a la ",
      "ausencia de diferencia."))
  }

  ## 8.8 Redacción
  seccion("8. C\u00f3mo redactar el resultado")
  desc_g <- function(nom, k, n, pr) paste0(nom, " (", fpct(pr), "; ", k, "/", n, ")")
  frase <- if (rechaza) {
    paste0("La proporci\u00f3n de '", categoria, "' fue significativamente ",
           if (dif > 0) "mayor" else "menor", " en el grupo ",
           desc_g(gA, a, nA, pA), " que en el grupo ", desc_g(gB, c_, nB, pB))
  } else {
    paste0("No se observaron diferencias significativas en la proporci\u00f3n de '",
           categoria, "' entre el grupo ", desc_g(gA, a, nA, pA),
           " y el grupo ", desc_g(gB, c_, nB, pB))
  }
  .imprimir(paste0(
    frase, " (",
    if (metodo == "normal") paste0("aproximaci\u00f3n normal con correcci\u00f3n de ",
                                   "continuidad: z = ", f3(z), "; ")
    else "prueba exacta de Fisher: ",
    .p_redaccion(p),
    if (alternativa != "two.sided") ", contraste unilateral" else "",
    "). Diferencia: ", fpp(dif), " puntos porcentuales (IC ", conf_txt, ": ",
    fpp(ic_dif_bil[1]), " a ", fpp(ic_dif_bil[2]), ")",
    if (!hay_cero) paste0("; RR = ", f3(RR), " (IC ", conf_txt, ": ",
                          f3(ic_RR[1]), " a ", f3(ic_RR[2]), ")") else "",
    "."))

  ## 8.9 R Commander
  cat("\n", linea, "\n", sep = "")
  if (metodo == "normal") {
    cat("En R Commander: Estad\u00edsticos > Proporciones > Test de proporciones para dos muestras...\n")
    cat("  (opci\u00f3n 'Aproximaci\u00f3n normal con correcci\u00f3n de continuidad')\n")
    cat("Orden equivalente:\n  prop.test(x=c(", a, ", ", c_, "), n=c(", nA, ", ",
        nB, "), alternative='", alternativa, "', conf.level=", conf,
        ", correct=TRUE)\n", sep = "")
  } else {
    cat("En R Commander: Estad\u00edsticos > Tablas de contingencia > Tabla de doble entrada...\n")
    cat("  (fila: ", var_g, "; columna: ", var_y,
        "; en Estad\u00edsticos marca 'Test exacto de Fisher')\n", sep = "")
    cat("Orden equivalente:\n  fisher.test(matrix(c(", a, ", ", c_, ", ", b, ", ",
        d, "), nrow=2), alternative='", alternativa, "', conf.level=", conf,
        ")\n", sep = "")
    if (alternativa != "two.sided")
      .imprimir(paste0("R Commander solo da el contraste bilateral de Fisher; ",
                       "para el unilateral usa la orden anterior."),
                sangria = 0)
  }
  notas <- character(0)
  if (length(niveles_y) > 2 || categoria != niveles_y[1]) {
    notas <- c(notas, paste0(
      "R Commander compara la proporci\u00f3n de la primera categor\u00eda de la ",
      "variable de respuesta. ",
      if (length(niveles_y) > 2)
        paste0("Recodifica antes '", var_y, "' en dos categor\u00edas (Datos > ",
               "Modificar variables del conjunto de datos activo > Recodificar ",
               "variables), dejando '", categoria, "' como primera.")
      else
        paste0("Reordena antes '", var_y, "' (Datos > Modificar variables del ",
               "conjunto de datos activo > Reordenar niveles de factor) para ",
               "que '", categoria, "' sea la primera; si no, R Commander ",
               "comparar\u00e1 la proporci\u00f3n de '", niveles_y[1], "'.")))
  }
  for (nt in notas) { cat("\n"); .imprimir(nt, sangria = 0) }
  if (invertido) {
    cat("\n")
    .nota_orden_rcmdr(var_g, gA, gB)
  }
  cat(linea, "\n", sep = "")

  invisible(list(
    tabla = tabla,
    proporciones = c(pA = pA, pB = pB),
    prueba = prueba,
    metodo = metodo,
    exacta_recomendada = exacta_rec,
    diferencia = dif,
    RR = RR, ic_RR = ic_RR,
    OR = OR, ic_OR = ic_OR,
    h_cohen = h,
    rechaza_H0 = rechaza
  ))
}
