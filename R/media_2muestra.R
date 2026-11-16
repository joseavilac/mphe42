#' Prueba t para dos muestras independientes interactiva
#'
#' Versión interactiva y comentada de la prueba t para muestras
#' independientes que aplica R Commander (Estadísticos > Medias > Test t
#' para muestras independientes), basada en \code{t.test()}. Pregunta por
#' consola el data.frame, la variable cuantitativa, la variable de grupo
#' (con dos categorías), la hipótesis alternativa y si se asumen varianzas
#' iguales, y presenta un informe completo: descriptivos por grupo,
#' comprobación de supuestos (normalidad, homogeneidad de varianzas con la
#' prueba de Levene, valores atípicos), hipótesis, cálculo del estadístico,
#' decisión, intervalo de confianza, tamaño del efecto y una frase de
#' redacción de resultados.
#'
#' @param alpha Nivel de significación. El nivel de confianza de los
#'   intervalos es 1 - alpha. Por defecto 0.05.
#' @return De forma invisible, una lista con los descriptivos por grupo, el
#'   resultado de \code{t.test()}, la prueba de Levene y los indicadores
#'   calculados.
#' @export
#' @importFrom stats t.test sd median quantile qt shapiro.test var
#' @importFrom grDevices boxplot.stats
#' @importFrom car leveneTest
#' @importFrom utils menu
media_2muestra <- function(alpha = 0.05) {

  if (!interactive())
    stop("media_2muestra() es interactiva: ejec\u00fatala desde la consola de R.",
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

  ## 2. Variable cuantitativa ---------------------------------------------
  var_y <- .pedir_variable(datos, "cuantitativa",
    "\u00bfQu\u00e9 variable cuantitativa quieres comparar entre los dos grupos?")
  if (is.null(var_y)) return(.cancelar())

  ## 3. Variable de grupo (2 categorías) y orden de los grupos ----------
  grp <- .pedir_dos_grupos(datos, var_y,
    "\u00bfQu\u00e9 variable define los dos grupos?", "medias",
    extra_mas_de_dos = paste0(" Para comparar m\u00e1s de dos medias se usa ",
                              "el ANOVA de un factor."))
  if (is.null(grp)) return(.cancelar())
  var_g <- grp$var_g
  gA <- grp$gA
  gB <- grp$gB
  invertido <- grp$invertido

  ## 4. Hipótesis alternativa --------------------------------------------
  muA <- paste0("\u03bc[", gA, "]")
  muB <- paste0("\u03bc[", gB, "]")
  i <- menu(c(paste0("Bilateral:   la media de '", gA, "' es distinta de la de '",
                     gB, "'  (", muA, " \u2260 ", muB, ")"),
              paste0("Unilateral:  la media de '", gA, "' es menor que la de '",
                     gB, "'  (", muA, " < ", muB, ")"),
              paste0("Unilateral:  la media de '", gA, "' es mayor que la de '",
                     gB, "'  (", muA, " > ", muB, ")")),
            title = "\n\u00bfCu\u00e1l es la hip\u00f3tesis alternativa (H1)?")
  if (i == 0) return(.cancelar())
  alternativa <- c("two.sided", "less", "greater")[i]
  simbolo <- c("\u2260", "<", ">")[i]
  sentido <- c("distinta de", "menor que", "mayor que")[i]
  ## H0 es el complementario de H1: en un contraste unilateral incluye la
  ## igualdad y el sentido opuesto a H1
  simbolo_H0 <- c("=", "\u2265", "\u2264")[i]
  sentido_H0 <- c("igual a", "mayor o igual que", "menor o igual que")[i]

  ## 5. Varianzas ---------------------------------------------------------
  i <- menu(c("No: prueba t de Welch (opci\u00f3n por defecto en R Commander; recomendada)",
              "S\u00ed: prueba t de Student cl\u00e1sica (varianza combinada)",
              "Decidir seg\u00fan la prueba de Levene"),
            title = "\n\u00bfQuieres suponer que las varianzas de los dos grupos son iguales?")
  if (i == 0) return(.cancelar())
  criterio_var <- c("welch", "student", "levene")[i]

  ## 6. Datos ---------------------------------------------------------------
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
  if (nA < 2 || nB < 2)
    stop("Cada grupo necesita al menos 2 observaciones (n de '", gA, "' = ",
         nA, ", n de '", gB, "' = ", nB, ").", call. = FALSE)
  if (sd(yA) == 0 && sd(yB) == 0)
    stop("Los valores de '", var_y, "' son constantes en ambos grupos: ",
         "la prueba t no puede calcularse.", call. = FALSE)

  ## 7. Cálculos ------------------------------------------------------------
  resumen <- function(x) {
    q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
    c(n = length(x), media = mean(x), de = sd(x), ee = sd(x) / sqrt(length(x)),
      mediana = q[2], p25 = q[1], p75 = q[3], min = min(x), max = max(x))
  }
  dA <- resumen(yA)
  dB <- resumen(yB)

  ## Normalidad por grupo
  sw <- function(x) if (length(x) >= 3 && length(x) <= 5000)
    shapiro.test(x)$p.value else NA
  swA <- sw(yA)
  swB <- sw(yB)

  ## Homogeneidad de varianzas (Levene centrado en la mediana, como Rcmdr)
  lev <- leveneTest(y ~ g, center = median)
  lev_F <- lev[1, "F value"]
  lev_gl1 <- lev[1, "Df"]
  lev_gl2 <- lev[2, "Df"]
  lev_p <- lev[1, "Pr(>F)"]
  razon_var <- max(dA["de"], dB["de"])^2 / min(dA["de"], dB["de"])^2

  var_iguales <- switch(criterio_var,
                        welch = FALSE, student = TRUE,
                        levene = lev_p >= alpha)

  prueba <- t.test(y ~ g, alternative = alternativa, conf.level = conf,
                   var.equal = var_iguales)
  t_val <- unname(prueba$statistic)
  gl <- unname(prueba$parameter)
  p <- prueba$p.value
  ic <- prueba$conf.int
  ee_dif <- prueba$stderr
  rechaza <- p < alpha
  ic_bil <- t.test(y ~ g, conf.level = conf, var.equal = var_iguales)$conf.int
  dif <- unname(dA["media"] - dB["media"])

  ## DE combinada (para la t de Student y para la d de Cohen)
  sp <- sqrt(((nA - 1) * dA["de"]^2 + (nB - 1) * dB["de"]^2) / (nA + nB - 2))
  sp <- unname(sp)
  d <- dif / sp
  t_crit <- switch(alternativa,
                   two.sided = qt(1 - alpha / 2, gl),
                   less = qt(alpha, gl),
                   greater = qt(1 - alpha, gl))

  atipA <- boxplot.stats(yA)$out
  atipB <- boxplot.stats(yB)$out

  ## Formato
  f3 <- function(x) .fmt_num(x, 3)
  fgl <- function(x) if (abs(x - round(x)) < 1e-8) format(round(x))
                     else formatC(x, format = "f", digits = 2)
  linea <- paste(rep("-", 70), collapse = "")
  seccion <- function(titulo) cat("\n", linea, "\n", titulo, "\n\n", sep = "")
  nombre_prueba <- if (var_iguales) "t de Student (varianzas iguales)"
                   else "t de Welch (varianzas distintas)"

  ## 8. Informe -------------------------------------------------------------
  cat("\n", linea, "\n", sep = "")
  cat("Prueba t para dos muestras independientes\n")
  cat("Variable: ", var_y, "   Grupos: ", var_g, " (", gA, " / ", gB,
      ")   Base de datos: ", nombre_df, "\n", sep = "")
  cat("Prueba aplicada: ", nombre_prueba, "   Nivel de significaci\u00f3n: alfa = ",
      alpha, "\n", sep = "")
  if (n_excl > 0)
    cat("Se excluye", if (n_excl == 1) " 1 observaci\u00f3n" else
          paste0("n ", n_excl, " observaciones"), " con '", var_y, "' o '",
        var_g, "' perdido.\n", sep = "")

  ## 8.1 Descriptivos
  seccion("1. Descripci\u00f3n de los grupos")
  fila <- function(dd) c(dd["n"], f3(dd["media"]), f3(dd["de"]), f3(dd["ee"]),
                         f3(dd["mediana"]),
                         paste0(f3(dd["p25"]), " - ", f3(dd["p75"])),
                         paste0(f3(dd["min"]), " - ", f3(dd["max"])))
  desc <- data.frame(fila(dA), fila(dB),
    row.names = c("n (valores v\u00e1lidos)", "Media", "Desviaci\u00f3n t\u00edpica (DE)",
                  "Error est\u00e1ndar de la media (EE)", "Mediana",
                  "Rango intercuart\u00edlico (P25 - P75)", "M\u00ednimo - M\u00e1ximo"))
  names(desc) <- c(gA, gB)
  print(desc)
  cat("\n")
  .imprimir(paste0("Diferencia de medias observada (", gA, " - ", gB, "): ",
                   f3(dA["media"]), " - ", f3(dB["media"]), " = ", f3(dif)))
  .imprimir(paste0("Las diferencias se calculan como '", gA,
                   "' menos '", gB, "', seg\u00fan el orden elegido. Una ",
                   "diferencia positiva significa que la media de '", gA,
                   "' es mayor."))

  ## 8.2 Supuestos
  seccion("2. Supuestos de la prueba")
  .imprimir(paste0(
    "a) Independencia: los dos grupos deben estar formados por individuos ",
    "distintos y no emparejados (si son las mismas personas medidas dos ",
    "veces, o pares emparejados, corresponde la prueba t para datos ",
    "relacionados). No se puede comprobar con los datos; depende del dise\u00f1o."))
  cat("\n")

  cat("  b) Normalidad en cada grupo (Shapiro-Wilk):\n")
  texto_sw <- function(nom, pv, n) {
    if (is.na(pv)) return(paste0("Grupo '", nom, "' (n = ", n, "): no se puede ",
                                 "aplicar (requiere entre 3 y 5000 observaciones)."))
    paste0("Grupo '", nom, "' (n = ", n, "): p", .comparar_p(pv),
           if (pv < alpha) paste0(" < ", alpha, ", se aleja de la normal.")
           else paste0(" >= ", alpha, ", sin evidencia de no normalidad."))
  }
  .imprimir(texto_sw(gA, swA, nA), sangria = 4)
  .imprimir(texto_sw(gB, swB, nB), sangria = 4)
  no_normal <- c(!is.na(swA) && swA < alpha, !is.na(swB) && swB < alpha)
  pequeno <- c(nA < 30, nB < 30)
  if (any(no_normal & pequeno)) {
    .imprimir(paste0(
      "ATENCI\u00d3N: hay alg\u00fan grupo con n < 30 en el que no se puede asumir ",
      "normalidad, as\u00ed que la prueba t no es fiable. Considera la prueba de ",
      "Mann-Whitney (en R Commander: Estad\u00edsticos > Test no param\u00e9tricos > ",
      "Test de Wilcoxon para dos muestras)."), sangria = 4)
  } else if (all(!pequeno)) {
    .imprimir(paste0(
      "Con n >= 30 en ambos grupos, la prueba t es robusta frente a ",
      "desviaciones moderadas de la normalidad (teorema central del l\u00edmite)."),
      sangria = 4)
  } else if (any(pequeno)) {
    .imprimir(paste0(
      "Alg\u00fan grupo tiene n < 30: la validez depende de la normalidad y ",
      "Shapiro-Wilk tiene poca potencia con muestras peque\u00f1as. Revisa tambi\u00e9n ",
      "los histogramas y gr\u00e1ficos Q-Q de cada grupo."), sangria = 4)
  }
  cat("\n")

  cat("  c) Homogeneidad de varianzas (Levene, centrado en la mediana):\n")
  .imprimir(paste0("Varianzas: ", gA, " = ", f3(dA["de"]^2), "; ", gB, " = ",
                   f3(dB["de"]^2), " (la mayor es ", formatC(razon_var,
                   format = "f", digits = 2), " veces la menor)."), sangria = 4)
  .imprimir(paste0("F(", lev_gl1, ", ", lev_gl2, ") = ", f3(lev_F), "; p",
                   .comparar_p(lev_p),
                   if (lev_p < alpha) paste0(" < ", alpha,
                     ": se rechaza la igualdad de varianzas.")
                   else paste0(" >= ", alpha,
                     ": no hay evidencia de varianzas distintas.")),
            sangria = 4)
  motivo <- switch(criterio_var,
    welch = paste0("Se aplica la t de Welch porque as\u00ed lo has elegido. No ",
                   "requiere varianzas iguales y apenas pierde potencia ",
                   "cuando lo son, por lo que es la opci\u00f3n segura."),
    student = paste0("Se aplica la t de Student porque has elegido suponer ",
                     "varianzas iguales.",
                     if (lev_p < alpha) paste0(" ATENCI\u00d3N: Levene rechaza la ",
                       "igualdad de varianzas; con estos datos ser\u00eda m\u00e1s ",
                       "adecuada la t de Welch.") else ""),
    levene = paste0("Se aplica la t de ",
                    if (var_iguales) "Student, porque Levene no rechaza "
                    else "Welch, porque Levene rechaza ",
                    "la igualdad de varianzas."))
  .imprimir(motivo, sangria = 4)
  cat("\n")

  texto_atip <- function(nom, at) {
    if (length(at) == 0) return(paste0("Grupo '", nom, "': ninguno."))
    paste0("Grupo '", nom, "': ", length(at), " (",
           paste(f3(utils::head(sort(at), 10)), collapse = ", "),
           if (length(at) > 10) ", ..." else "", ").")
  }
  cat("  d) Valores at\u00edpicos (criterio del diagrama de caja, 1.5 x RIC):\n")
  .imprimir(texto_atip(gA, atipA), sangria = 4)
  .imprimir(texto_atip(gB, atipB), sangria = 4)
  if (length(atipA) + length(atipB) > 0)
    .imprimir(paste0("Las medias y las DE son sensibles a los valores ",
                     "at\u00edpicos: comprueba que no son errores de registro."),
              sangria = 4)

  ## 8.3 Hipótesis
  seccion("3. Hip\u00f3tesis")
  .imprimir(paste0("H0: ", muA, " ", simbolo_H0, " ", muB,
                   "   (la media poblacional de '", var_y, "' en '", gA,
                   "' es ", sentido_H0, " la de '", gB, "')"))
  .imprimir(paste0("H1: ", muA, " ", simbolo, " ", muB,
                   "   (la media poblacional de '", var_y, "' en '", gA,
                   "' es ", sentido, " la de '", gB, "')"))
  cat("\n")
  .imprimir(paste0("Contraste ", if (alternativa == "two.sided") "bilateral"
                   else "unilateral", ". Expresado como diferencia de medias ",
                   "poblacionales: H0: ", muA, " - ", muB, " ", simbolo_H0,
                   " 0 frente a H1: ", muA, " - ", muB, " ", simbolo, " 0."))
  if (alternativa != "two.sided") {
    .imprimir(paste0(
      "H0 recoge todos los valores que no est\u00e1n en H1. El estad\u00edstico ",
      "y el valor p se calculan en el l\u00edmite de H0 (", muA, " = ", muB,
      "), que es el caso m\u00e1s desfavorable: si los datos permiten ",
      "rechazar la igualdad, tambi\u00e9n permiten rechazar cualquier otra ",
      "situaci\u00f3n incluida en H0."))
  }

  ## 8.4 Estadístico
  seccion("4. Estad\u00edstico de contraste")
  if (var_iguales) {
    cat("  DE combinada: sp = ", f3(sp), "\n", sep = "")
    cat("  EE de la diferencia = sp x \u221a(1/nA + 1/nB) = ", f3(sp), " x \u221a(1/",
        nA, " + 1/", nB, ") = ", f3(ee_dif), "\n", sep = "")
    cat("  Grados de libertad: gl = nA + nB - 2 = ", fgl(gl), "\n", sep = "")
  } else {
    cat("  EE de la diferencia = \u221a(DEA\u00b2/nA + DEB\u00b2/nB) = \u221a(",
        f3(dA["de"]), "\u00b2/", nA, " + ", f3(dB["de"]), "\u00b2/", nB, ") = ",
        f3(ee_dif), "\n", sep = "")
    cat("  Grados de libertad (aproximaci\u00f3n de Welch-Satterthwaite): gl = ",
        fgl(gl), "\n", sep = "")
  }
  cat("  t = (mediaA - mediaB) / EE = ", f3(dif), " / ", f3(ee_dif), " = ",
      f3(t_val), "\n", sep = "")
  cat("  Valor cr\u00edtico: t", switch(alternativa,
        two.sided = paste0("(", 1 - alpha / 2, "; ", fgl(gl), ") = \u00b1", f3(abs(t_crit))),
        less = paste0("(", alpha, "; ", fgl(gl), ") = ", f3(t_crit)),
        greater = paste0("(", 1 - alpha, "; ", fgl(gl), ") = ", f3(t_crit))),
      "\n", sep = "")
  cat("\n")
  .imprimir(paste0("El estad\u00edstico t indica a cu\u00e1ntos errores est\u00e1ndar est\u00e1 ",
                   "la diferencia observada de una diferencia nula."))

  ## 8.5 Valor p y decisión
  seccion("5. Valor p y decisi\u00f3n")
  cat("  Valor p: ", .fmt_p(p), "\n\n", sep = "")
  .imprimir(paste0(
    "Interpretaci\u00f3n del valor p: si las medias poblacionales de los dos ",
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
      "evidencia de que la media poblacional de '", var_y, "' en '", gA,
      "' es ", sentido, " la de '", gB, "'."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No ",
      "hay evidencia suficiente para afirmar que la media poblacional de '",
      var_y, "' en '", gA, "' sea ", sentido, " la de '", gB, "'. Esto no ",
      "demuestra que H0 sea cierta (que la media de '", gA, "' sea ",
      sentido_H0, " la de '", gB, "')."))
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
                 " para la diferencia de medias (", gA, " - ", gB, ")"))
  if (alternativa == "two.sided") {
    cat("  IC ", conf_txt, ": ", f3(ic[1]), " a ", f3(ic[2]), "\n\n", sep = "")
    .imprimir(paste0(
      "Con una confianza del ", conf_txt, ", la diferencia entre la media ",
      "poblacional de '", gA, "' y la de '", gB, "' est\u00e1 entre ", f3(ic[1]),
      " y ", f3(ic[2]), "."))
  } else {
    limite <- if (alternativa == "less") ic[2] else ic[1]
    cat("  IC unilateral ", conf_txt, ": ",
        if (alternativa == "less") paste0("-Inf a ", f3(limite))
        else paste0(f3(limite), " a +Inf"), "\n", sep = "")
    cat("  IC bilateral ", conf_txt, ":  ", f3(ic_bil[1]), " a ", f3(ic_bil[2]),
        "\n\n", sep = "")
    .imprimir(paste0(
      "Con una confianza del ", conf_txt, ", la diferencia de medias es ",
      if (alternativa == "less") "menor que " else "mayor que ", f3(limite),
      ". Es el intervalo que corresponde a un contraste unilateral; en las ",
      "publicaciones suele darse el bilateral."))
  }
  dentro <- 0 >= ic[1] && 0 <= ic[2]
  .imprimir(paste0(
    "El 0 (ninguna diferencia) ", if (dentro) "queda dentro" else "queda fuera",
    " del intervalo, coherente con que ", if (rechaza) "se rechace" else
    "no se rechace", " H0."))

  ## 8.7 Tamaño del efecto
  seccion("7. Tama\u00f1o del efecto")
  .imprimir(paste0("Diferencia de medias: ", f3(dif), " (IC ", conf_txt,
                   " bilateral: ", f3(ic_bil[1]), " a ", f3(ic_bil[2]),
                   "), en las unidades de '", var_y, "'"))
  mag <- if (abs(d) < 0.2) "despreciable" else if (abs(d) < 0.5) "peque\u00f1o"
         else if (abs(d) < 0.8) "mediano" else "grande"
  .imprimir(paste0("d de Cohen = (mediaA - mediaB) / DE combinada = ", f3(dif),
                   " / ", f3(sp), " = ", f3(d), "  (efecto ", mag, ")"))
  cat("\n")
  .imprimir(paste0(
    "El valor p indica si la diferencia es estad\u00edsticamente significativa, ",
    "pero no si es importante. Juzga la relevancia con la diferencia en ",
    "unidades reales y su intervalo. Los umbrales de Cohen (0.2 peque\u00f1o, ",
    "0.5 mediano, 0.8 grande) son solo orientativos."))
  if (rechaza && abs(d) < 0.2) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: la diferencia es significativa pero muy peque\u00f1a. Con ",
      "muestras grandes, diferencias sin relevancia pr\u00e1ctica pueden ",
      "resultar significativas."))
  }
  if (!rechaza && (nA < 30 || nB < 30) && abs(d) >= 0.5) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: el efecto observado no es peque\u00f1o, pero los grupos son ",
      "reducidos (n = ", nA, " y ", nB, "). La falta de significaci\u00f3n puede ",
      "deberse a poca potencia, no a la ausencia de diferencia."))
  }

  ## 8.8 Redacción
  seccion("8. C\u00f3mo redactar el resultado")
  desc_g <- function(nom, dd) paste0(nom, " (media = ", f3(dd["media"]),
                                     "; DE = ", f3(dd["de"]), "; n = ",
                                     dd["n"], ")")
  frase <- if (rechaza) {
    paste0("La media de ", var_y, " fue significativamente ",
           if (dif > 0) "mayor" else "menor", " en el grupo ",
           desc_g(gA, dA), " que en el grupo ", desc_g(gB, dB))
  } else {
    paste0("No se observaron diferencias significativas en la media de ",
           var_y, " entre el grupo ", desc_g(gA, dA), " y el grupo ",
           desc_g(gB, dB))
  }
  .imprimir(paste0(
    frase, " (t de ", if (var_iguales) "Student" else "Welch", ": t(", fgl(gl), ") = ", f3(t_val),
    "; p ", if (p < 1e-4) "< 0.0001" else paste0("= ", .fmt_p(p)),
    if (alternativa != "two.sided") ", contraste unilateral" else "",
    "). Diferencia de medias: ", f3(dif), " (IC ", conf_txt, ": ",
    f3(ic_bil[1]), " a ", f3(ic_bil[2]), "); d de Cohen = ", f3(d), "."))

  ## 8.9 Órdenes equivalentes
  cat("\n", linea, "\n", sep = "")
  cat("En R Commander: Estad\u00edsticos > Medias > Test t para muestras independientes...\n")
  cat("\u00d3rdenes equivalentes:\n")
  cat("  leveneTest(", var_y, " ~ ", var_g, ", data=", nombre_df,
      ", center=\"median\")\n", sep = "")
  termino_g <- if (invertido)
    paste0("relevel(", var_g, ", ref=\"", gA, "\")") else var_g
  cat("  t.test(", var_y, "~", termino_g, ", alternative='", alternativa,
      "', conf.level=", conf, ", var.equal=", var_iguales, ", data=",
      nombre_df, ")\n", sep = "")
  if (invertido) {
    cat("\n")
    .nota_orden_rcmdr(var_g, gA, gB)
  }
  cat(linea, "\n", sep = "")

  descriptivos <- rbind(dA, dB)
  rownames(descriptivos) <- c(gA, gB)
  shapiro_p <- c(swA, swB)
  names(shapiro_p) <- c(gA, gB)
  invisible(list(
    descriptivos = descriptivos,
    prueba = prueba,
    levene = lev,
    varianzas_iguales = var_iguales,
    diferencia = dif,
    d_cohen = d,
    shapiro_p = shapiro_p,
    rechaza_H0 = rechaza
  ))
}
