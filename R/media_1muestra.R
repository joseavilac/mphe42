#' Prueba t para una muestra interactiva
#'
#' Versión interactiva y comentada de la prueba t para una muestra que aplica
#' R Commander (Estadísticos > Medias > Test t para una muestra), basada en
#' \code{t.test()}. Pregunta por consola el data.frame, la variable, el valor
#' de referencia (mu0) y la hipótesis alternativa, y presenta un informe
#' completo: descriptivos, comprobación de supuestos, hipótesis, cálculo del
#' estadístico, decisión, intervalo de confianza, tamaño del efecto y una
#' frase de redacción de resultados.
#'
#' @param alpha Nivel de significación. El nivel de confianza de los
#'   intervalos es 1 - alpha. Por defecto 0.05.
#' @return De forma invisible, una lista con los descriptivos, el resultado
#'   de \code{t.test()} y los indicadores calculados.
#' @export
#' @importFrom stats t.test sd median quantile qt shapiro.test
#' @importFrom grDevices boxplot.stats
#' @importFrom utils menu
media_1muestra <- function(alpha = 0.05) {

  if (!interactive())
    stop("media_1muestra() es interactiva: ejec\u00fatala desde la consola de R.",
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

  ## 2. Variable ----------------------------------------------------------
  var_y <- .pedir_variable(datos, "cuantitativa",
    "\u00bfQu\u00e9 variable cuantitativa quieres analizar?")
  if (is.null(var_y)) return(.cancelar())

  ## 3. Valor de referencia (mu0) ----------------------------------------
  mu0 <- .pedir_numero(paste0(
    "\u00bfCon qu\u00e9 valor de referencia (mu0) quieres comparar la media de '",
    var_y, "'?"), defecto = 0)
  if (is.null(mu0)) return(.cancelar())
  mu0_txt <- format(mu0)

  ## 4. Hipótesis alternativa --------------------------------------------
  i <- menu(c(paste0("Bilateral:   la media es distinta de ", mu0_txt,
                     "  (\u03bc \u2260 ", mu0_txt, ")"),
              paste0("Unilateral:  la media es menor que ", mu0_txt,
                     "  (\u03bc < ", mu0_txt, ")"),
              paste0("Unilateral:  la media es mayor que ", mu0_txt,
                     "  (\u03bc > ", mu0_txt, ")")),
            title = "\n\u00bfCu\u00e1l es la hip\u00f3tesis alternativa (H1)?")
  if (i == 0) return(.cancelar())
  alternativa <- c("two.sided", "less", "greater")[i]
  simbolo <- c("\u2260", "<", ">")[i]
  sentido <- c("distinta de", "menor que", "mayor que")[i]
  ## H0 es el complementario de H1: en un contraste unilateral incluye la
  ## igualdad y el sentido opuesto a H1
  simbolo_H0 <- c("=", "\u2265", "\u2264")[i]
  sentido_H0 <- c("igual a", "mayor o igual que", "menor o igual que")[i]

  ## 5. Datos ---------------------------------------------------------------
  y_todo <- datos[[var_y]]
  n_na <- sum(is.na(y_todo))
  y <- y_todo[!is.na(y_todo)]
  n <- length(y)
  if (n < 2)
    stop("'", var_y, "' tiene menos de 2 valores v\u00e1lidos: no se puede ",
         "aplicar la prueba.", call. = FALSE)
  if (sd(y) == 0)
    stop("Todos los valores de '", var_y, "' son iguales (desviaci\u00f3n t\u00edpica ",
         "= 0): la prueba t no puede calcularse.", call. = FALSE)

  ## 6. Cálculos ------------------------------------------------------------
  media <- mean(y)
  de <- sd(y)
  ee <- de / sqrt(n)
  cuart <- quantile(y, c(0.25, 0.5, 0.75), names = FALSE)
  atipicos <- boxplot.stats(y)$out
  gl <- n - 1

  prueba <- t.test(y, alternative = alternativa, mu = mu0, conf.level = conf)
  t_val <- unname(prueba$statistic)
  p <- prueba$p.value
  ic <- prueba$conf.int
  rechaza <- p < alpha
  ## Intervalo bilateral, que es el que se suele comunicar aunque el
  ## contraste sea unilateral
  ic_bil <- t.test(y, mu = mu0, conf.level = conf)$conf.int
  dif <- media - mu0
  d <- dif / de
  t_crit <- switch(alternativa,
                   two.sided = qt(1 - alpha / 2, gl),
                   less = qt(alpha, gl),
                   greater = qt(1 - alpha, gl))

  sw_p <- if (n >= 3 && n <= 5000) shapiro.test(y)$p.value else NA

  ## Formato
  f3 <- function(x) .fmt_num(x, 3)
  linea <- paste(rep("-", 70), collapse = "")
  seccion <- function(titulo) cat("\n", linea, "\n", titulo, "\n\n", sep = "")

  ## 7. Informe ---------------------------------------------------------------
  cat("\n", linea, "\n", sep = "")
  cat("Prueba t para una muestra\n")
  cat("Variable: ", var_y, "   Base de datos: ", nombre_df, "\n", sep = "")
  cat("Valor de referencia: mu0 = ", mu0_txt, "   Nivel de significaci\u00f3n: alfa = ",
      alpha, "\n", sep = "")
  if (n_na > 0)
    cat("Se excluye", if (n_na == 1) " 1 valor perdido" else
          paste0("n ", n_na, " valores perdidos"), " de '", var_y, "'.\n",
        sep = "")

  ## 7.1 Descriptivos
  seccion("1. Descripci\u00f3n de la muestra")
  desc <- data.frame(
    Valor = c(n, f3(media), f3(de), f3(ee), f3(cuart[2]),
              paste0(f3(cuart[1]), " - ", f3(cuart[3])),
              paste0(f3(min(y)), " - ", f3(max(y)))),
    row.names = c("n (valores v\u00e1lidos)", "Media", "Desviaci\u00f3n t\u00edpica (DE)",
                  "Error est\u00e1ndar de la media (EE)", "Mediana",
                  "Rango intercuart\u00edlico (P25 - P75)", "M\u00ednimo - M\u00e1ximo"),
    check.names = FALSE
  )
  print(desc)
  cat("\n")
  .imprimir(paste0("El error est\u00e1ndar (EE = DE / \u221an = ", f3(de), " / \u221a",
                   n, " = ", f3(ee), ") mide cu\u00e1nto variar\u00eda la media de una ",
                   "muestra a otra; es la base del estad\u00edstico t."))

  ## 7.2 Supuestos
  seccion("2. Supuestos de la prueba")
  .imprimir(paste0(
    "a) Independencia: cada observaci\u00f3n debe corresponder a un individuo ",
    "distinto, sin medidas repetidas. No se puede comprobar con los datos; ",
    "depende del dise\u00f1o del estudio."))
  cat("\n")
  if (is.na(sw_p)) {
    .imprimir(paste0("b) Normalidad: con n = ", n, " no se puede aplicar ",
                     "Shapiro-Wilk (requiere entre 3 y 5000 observaciones)."))
  } else {
    .imprimir(paste0("b) Normalidad (Shapiro-Wilk): p", .comparar_p(sw_p),
                     if (sw_p < alpha) paste0(" < ", alpha,
                       ": la distribuci\u00f3n se aleja de la normal.")
                     else paste0(" >= ", alpha,
                       ": no hay evidencia de que se aleje de la normal.")))
  }
  if (n >= 30) {
    .imprimir(paste0(
      "Con n = ", n, " (>= 30), la prueba t es robusta frente a desviaciones ",
      "moderadas de la normalidad, porque la distribuci\u00f3n de la media muestral ",
      "se aproxima a la normal (teorema central del l\u00edmite)."), sangria = 4)
  } else if (!is.na(sw_p) && sw_p < alpha) {
    .imprimir(paste0(
      "ATENCI\u00d3N: con n = ", n, " (< 30) y sin normalidad, la prueba t no es ",
      "fiable. Considera la prueba de Wilcoxon para una muestra (en R ",
      "Commander: Estad\u00edsticos > Test no param\u00e9tricos > Test de Wilcoxon ",
      "para una muestra)."), sangria = 4)
  } else {
    .imprimir(paste0(
      "Con n = ", n, " (< 30) la validez de la prueba depende de la ",
      "normalidad, y Shapiro-Wilk tiene poca potencia con muestras peque\u00f1as: ",
      "revisa tambi\u00e9n el histograma y el gr\u00e1fico Q-Q."), sangria = 4)
  }
  cat("\n")
  if (length(atipicos) > 0) {
    .imprimir(paste0(
      "c) Valores at\u00edpicos (criterio del diagrama de caja, 1.5 x RIC): ",
      length(atipicos), " (", paste(f3(utils::head(sort(atipicos), 10)),
                                     collapse = ", "),
      if (length(atipicos) > 10) ", ..." else "", "). La media y la DE son ",
      "sensibles a ellos: comprueba que no son errores de registro."))
  } else {
    .imprimir("c) Valores at\u00edpicos (criterio del diagrama de caja, 1.5 x RIC): ninguno.")
  }

  ## 7.3 Hipótesis
  seccion("3. Hip\u00f3tesis")
  .imprimir(paste0("H0: \u03bc ", simbolo_H0, " ", mu0_txt,
                   "   (la media poblacional de '", var_y, "' es ", sentido_H0,
                   " ", mu0_txt, ")"))
  .imprimir(paste0("H1: \u03bc ", simbolo, " ", mu0_txt,
                   "   (la media poblacional de '", var_y, "' es ", sentido,
                   " ", mu0_txt, ")"))
  cat("\n")
  .imprimir(paste0("Contraste ", if (alternativa == "two.sided") "bilateral"
                   else "unilateral", ". \u03bc es la media de la poblaci\u00f3n, ",
                   "que se desconoce; la media muestral (", f3(media),
                   ") es su estimaci\u00f3n."))
  if (alternativa != "two.sided") {
    .imprimir(paste0(
      "H0 recoge todos los valores que no est\u00e1n en H1. El estad\u00edstico ",
      "y el valor p se calculan en el l\u00edmite de H0 (\u03bc = ", mu0_txt,
      "), que es el caso m\u00e1s desfavorable: si los datos permiten ",
      "rechazar \u03bc = ", mu0_txt, ", tambi\u00e9n permiten rechazar ",
      "cualquier otro valor de H0."))
  }

  ## 7.4 Estadístico
  seccion("4. Estad\u00edstico de contraste")
  cat("  t = (media - mu0) / EE = (", f3(media), " - ", mu0_txt, ") / ",
      f3(ee), " = ", f3(t_val), "\n", sep = "")
  cat("  Grados de libertad: gl = n - 1 = ", gl, "\n", sep = "")
  cat("  Diferencia observada: media - mu0 = ", f3(dif), "\n", sep = "")
  cat("  Valor cr\u00edtico: t", switch(alternativa,
        two.sided = paste0("(", 1 - alpha / 2, "; ", gl, ") = \u00b1", f3(abs(t_crit))),
        less = paste0("(", alpha, "; ", gl, ") = ", f3(t_crit)),
        greater = paste0("(", 1 - alpha, "; ", gl, ") = ", f3(t_crit))),
      "\n", sep = "")
  cat("\n")
  .imprimir(paste0("El estad\u00edstico t indica a cu\u00e1ntos errores est\u00e1ndar est\u00e1 ",
                   "la media muestral del valor de referencia."))

  ## 7.5 Valor p y decisión
  seccion("5. Valor p y decisi\u00f3n")
  cat("  Valor p: ", .fmt_p(p), "\n\n", sep = "")
  .imprimir(paste0(
    "Interpretaci\u00f3n del valor p: si la media poblacional fuera ",
    if (alternativa == "two.sided") "realmente " else "", mu0_txt,
    if (alternativa == "two.sided") "" else " (el l\u00edmite de H0)",
    ", la probabilidad de obtener una media muestral ",
    switch(alternativa,
           two.sided = paste0("tan alejada de ", mu0_txt,
                              " como la observada (o m\u00e1s), en cualquier ",
                              "direcci\u00f3n,"),
           less = paste0("tan baja como la observada (o m\u00e1s baja)"),
           greater = paste0("tan alta como la observada (o m\u00e1s alta)")),
    " ser\u00eda ", .fmt_p(p), "."))
  cat("\n")
  if (rechaza) {
    .imprimir(paste0(
      "Decisi\u00f3n: se rechaza H0 (p", .comparar_p(p), " < ", alpha, "). Hay ",
      "evidencia de que la media poblacional de '", var_y, "' es ", sentido,
      " ", mu0_txt, "."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No ",
      "hay evidencia suficiente para afirmar que la media poblacional de '",
      var_y, "' sea ", sentido, " ", mu0_txt, ". Esto no demuestra que H0 ",
      "sea cierta (que la media sea ", sentido_H0, " ", mu0_txt, ")."))
  }
  if ((alternativa == "less" && dif > 0) ||
      (alternativa == "greater" && dif < 0)) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: la media muestral (", f3(media), ") est\u00e1 en la direcci\u00f3n ",
      "contraria a la planteada en H1, por eso el valor p es tan alto. La ",
      "hip\u00f3tesis alternativa debe fijarse antes de ver los datos; no la ",
      "cambies a posteriori."))
  }

  ## 7.6 Intervalo de confianza
  seccion(paste0("6. Intervalo de confianza al ", conf_txt,
                 " para la media poblacional"))
  if (alternativa == "two.sided") {
    cat("  IC ", conf_txt, ": ", f3(ic[1]), " a ", f3(ic[2]), "\n\n", sep = "")
    .imprimir(paste0(
      "Con una confianza del ", conf_txt, ", la media poblacional de '", var_y,
      "' est\u00e1 entre ", f3(ic[1]), " y ", f3(ic[2]), "."))
  } else {
    limite <- if (alternativa == "less") ic[2] else ic[1]
    cat("  IC unilateral ", conf_txt, ": ",
        if (alternativa == "less") paste0("-Inf a ", f3(limite))
        else paste0(f3(limite), " a +Inf"), "\n", sep = "")
    cat("  IC bilateral ", conf_txt, ":  ", f3(ic_bil[1]), " a ", f3(ic_bil[2]),
        "\n\n", sep = "")
    .imprimir(paste0(
      "Con una confianza del ", conf_txt, ", la media poblacional es ",
      if (alternativa == "less") "menor que " else "mayor que ", f3(limite),
      ". Es el intervalo que corresponde a un contraste unilateral; en las ",
      "publicaciones suele darse el bilateral."))
  }
  dentro <- mu0 >= ic[1] && mu0 <= ic[2]
  .imprimir(paste0(
    "El valor de referencia (", mu0_txt, ") ",
    if (dentro) "queda dentro" else "queda fuera", " del intervalo, ",
    "coherente con que ", if (rechaza) "se rechace" else "no se rechace",
    " H0."))

  ## 7.7 Tamaño del efecto
  seccion("7. Tama\u00f1o del efecto")
  .imprimir(paste0("Diferencia de medias: ", f3(dif), " (IC ", conf_txt,
                   " bilateral: ", f3(ic_bil[1] - mu0), " a ",
                   f3(ic_bil[2] - mu0), "), en las unidades de '", var_y, "'"))
  mag <- if (abs(d) < 0.2) "despreciable" else if (abs(d) < 0.5) "peque\u00f1o"
         else if (abs(d) < 0.8) "mediano" else "grande"
  cat("  d de Cohen = (media - mu0) / DE = ", f3(d), "  (efecto ", mag, ")\n\n",
      sep = "")
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
  if (!rechaza && n < 30 && abs(d) >= 0.5) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: el efecto observado no es peque\u00f1o, pero la muestra es ",
      "reducida (n = ", n, "). La falta de significaci\u00f3n puede deberse a ",
      "poca potencia, no a la ausencia de diferencia."))
  }

  ## 7.8 Redacción
  seccion("8. C\u00f3mo redactar el resultado")
  frase_dir <- if (rechaza) {
    paste0("fue significativamente ",
           if (dif > 0) "superior" else "inferior", " al valor de referencia de ",
           mu0_txt)
  } else {
    paste0("no difiri\u00f3 significativamente del valor de referencia de ", mu0_txt)
  }
  .imprimir(paste0(
    "La media de ", var_y, " (media = ", f3(media), "; DE = ", f3(de),
    "; n = ", n, ") ", frase_dir, " (t(", gl, ") = ", f3(t_val), "; p ",
    if (p < 1e-4) "< 0.0001" else paste0("= ", .fmt_p(p)),
    if (alternativa != "two.sided") ", contraste unilateral" else "",
    "). Diferencia de medias: ", f3(dif), " (IC ", conf_txt, ": ",
    f3(ic_bil[1] - mu0), " a ", f3(ic_bil[2] - mu0), "); d de Cohen = ",
    f3(d), "."), sangria = 2)

  ## 7.9 Orden equivalente
  cat("\n", linea, "\n", sep = "")
  cat("En R Commander: Estad\u00edsticos > Medias > Test t para una muestra...\n")
  cat("Orden equivalente:\n  with(", nombre_df, ", (t.test(", var_y,
      ", alternative='", alternativa, "', mu=", mu0_txt, ", conf.level=",
      conf, ")))\n", sep = "")
  cat(linea, "\n", sep = "")

  invisible(list(
    descriptivos = c(n = n, media = media, de = de, ee = ee,
                     mediana = cuart[2], p25 = cuart[1], p75 = cuart[3],
                     min = min(y), max = max(y)),
    prueba = prueba,
    diferencia = dif,
    d_cohen = d,
    shapiro_p = sw_p,
    n_atipicos = length(atipicos),
    rechaza_H0 = rechaza
  ))
}
