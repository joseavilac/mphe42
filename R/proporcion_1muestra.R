#' Prueba de proporciones para una muestra interactiva
#'
#' Versión interactiva y comentada de la prueba de proporciones para una
#' muestra de R Commander (Estadísticos > Proporciones > Test de proporciones
#' para una muestra), basada en \code{prop.test()} (aproximación normal con
#' corrección de continuidad) y \code{binom.test()} (binomial exacta).
#' Pregunta por consola el data.frame, la variable cualitativa, la categoría
#' cuya proporción se analiza, la proporción de referencia (p0), la hipótesis
#' alternativa y el método, indicando cuándo se recomienda la binomial
#' exacta. Presenta un informe completo: descriptivos, condiciones de
#' aplicación, hipótesis, estadístico, decisión, intervalo de confianza,
#' tamaño del efecto y una frase de redacción de resultados.
#'
#' @param alpha Nivel de significación. El nivel de confianza de los
#'   intervalos es 1 - alpha. Por defecto 0.05.
#' @return De forma invisible, una lista con los recuentos, el resultado de la
#'   prueba y los indicadores calculados.
#' @export
#' @importFrom stats prop.test binom.test qnorm
#' @importFrom utils menu
proporcion_1muestra <- function(alpha = 0.05) {

  if (!interactive())
    stop("proporcion_1muestra() es interactiva: ejec\u00fatala desde la consola de R.",
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

  ## 2. Variable cualitativa y categoría ---------------------------------
  var_y <- .pedir_variable(datos, "cualitativa",
    "\u00bfQu\u00e9 variable cualitativa quieres analizar?")
  if (is.null(var_y)) return(.cancelar())
  categoria <- .elegir_categoria(datos, var_y,
    paste0("\u00bfPara qu\u00e9 categor\u00eda de '", var_y,
           "' quieres calcular la proporci\u00f3n?"))
  if (is.null(categoria)) return(.cancelar())

  ## 3. Proporción de referencia (p0) ------------------------------------
  p0 <- .pedir_numero(paste0(
    "\u00bfCon qu\u00e9 proporci\u00f3n de referencia (p0) quieres comparar la proporci\u00f3n de '",
    categoria, "'? Escr\u00edbela en tanto por uno (p. ej. 0.30 para un 30 %)."),
    defecto = 0.5, rango = c(0, 1),
    aviso_rango = paste0("La proporci\u00f3n debe estar entre 0 y 1, sin incluirlos ",
                         "(p. ej. 0.30 para un 30 %)."))
  if (is.null(p0)) return(.cancelar())
  p0_txt <- format(p0)

  ## 4. Hipótesis alternativa --------------------------------------------
  pi_s <- "\u03c0"
  i <- menu(c(paste0("Bilateral:   la proporci\u00f3n es distinta de ", p0_txt,
                     "  (", pi_s, " \u2260 ", p0_txt, ")"),
              paste0("Unilateral:  la proporci\u00f3n es menor que ", p0_txt,
                     "  (", pi_s, " < ", p0_txt, ")"),
              paste0("Unilateral:  la proporci\u00f3n es mayor que ", p0_txt,
                     "  (", pi_s, " > ", p0_txt, ")")),
            title = "\n\u00bfCu\u00e1l es la hip\u00f3tesis alternativa (H1)?")
  if (i == 0) return(.cancelar())
  alternativa <- c("two.sided", "less", "greater")[i]
  simbolo <- c("\u2260", "<", ">")[i]
  sentido <- c("distinta de", "menor que", "mayor que")[i]
  simbolo_H0 <- c("=", "\u2265", "\u2264")[i]
  sentido_H0 <- c("igual a", "mayor o igual que", "menor o igual que")[i]

  ## 5. Datos ---------------------------------------------------------------
  y_todo <- datos[[var_y]]
  n_na <- sum(is.na(y_todo))
  y <- y_todo[!is.na(y_todo)]
  n <- length(y)
  x <- sum(y == categoria)
  p_hat <- x / n
  niveles <- levels(droplevels(factor(y)))

  ## 6. Elección del método ----------------------------------------------
  esp_si <- n * p0
  esp_no <- n * (1 - p0)
  exacta_rec <- esp_si < 5 || esp_no < 5
  f2 <- function(v) formatC(v, format = "f", digits = 2)
  casos <- function(k) paste0(k, if (k == 1) " caso" else " casos")

  cat("\nComprobaci\u00f3n para elegir el m\u00e9todo (frecuencias esperadas si H0 fuera cierta):\n")
  cat("  n x p0 = ", n, " x ", p0_txt, " = ", f2(esp_si),
      "     n x (1 - p0) = ", n, " x ", format(1 - p0), " = ", f2(esp_no),
      "\n", sep = "")
  if (exacta_rec) {
    .imprimir(paste0("Alguna es < 5: la aproximaci\u00f3n normal no es fiable. ",
                     "Se recomienda la binomial exacta."))
  } else {
    .imprimir(paste0("Ambas son >= 5: la aproximaci\u00f3n normal es adecuada. ",
                     "La binomial exacta tambi\u00e9n es v\u00e1lida."))
  }
  i <- menu(c("Aproximaci\u00f3n normal con correcci\u00f3n de continuidad",
              paste0("Binomial exacta", if (exacta_rec) "   <-- RECOMENDADA" else "")),
            title = "\n\u00bfQu\u00e9 m\u00e9todo quieres utilizar?")
  if (i == 0) return(.cancelar())
  metodo <- c("normal", "exacta")[i]

  ## 7. Cálculos ------------------------------------------------------------
  ee <- sqrt(p_hat * (1 - p_hat) / n)
  if (metodo == "normal") {
    prueba <- prop.test(x, n, p = p0, alternative = alternativa,
                        conf.level = conf, correct = TRUE)
    ic_bil <- prop.test(x, n, p = p0, conf.level = conf,
                        correct = TRUE)$conf.int
    ee0 <- sqrt(p0 * (1 - p0) / n)
    cc <- 1 / (2 * n)
    z <- sign(p_hat - p0) * max(0, abs(p_hat - p0) - cc) / ee0
    chi2 <- unname(prueba$statistic)
    z_crit <- switch(alternativa,
                     two.sided = qnorm(1 - alpha / 2),
                     less = qnorm(alpha),
                     greater = qnorm(1 - alpha))
    nombre_metodo <- "aproximaci\u00f3n normal con correcci\u00f3n de continuidad"
    nombre_ic <- "Wilson con correcci\u00f3n de continuidad"
  } else {
    prueba <- binom.test(x, n, p = p0, alternative = alternativa,
                         conf.level = conf)
    ic_bil <- binom.test(x, n, p = p0, conf.level = conf)$conf.int
    nombre_metodo <- "prueba binomial exacta"
    nombre_ic <- "Clopper-Pearson (exacto)"
  }
  p <- prueba$p.value
  ic <- prueba$conf.int
  rechaza <- p < alpha
  dif <- p_hat - p0
  h <- 2 * asin(sqrt(p_hat)) - 2 * asin(sqrt(p0))

  ## Formato
  f3 <- function(v) .fmt_num(v, 3)
  fpct <- function(v) paste0(formatC(100 * v, format = "f", digits = 1), " %")
  fpp <- function(v) formatC(100 * v, format = "f", digits = 1)
  linea <- paste(rep("-", 70), collapse = "")
  seccion <- function(titulo) cat("\n", linea, "\n", titulo, "\n\n", sep = "")

  ## 8. Informe -------------------------------------------------------------
  cat("\n", linea, "\n", sep = "")
  cat("Prueba de proporciones para una muestra\n")
  cat("Variable: ", var_y, "   Categor\u00eda analizada: ", categoria,
      "   Base de datos: ", nombre_df, "\n", sep = "")
  cat("M\u00e9todo: ", nombre_metodo, "   Nivel de significaci\u00f3n: alfa = ", alpha,
      "\n", sep = "")
  if (n_na > 0)
    cat("Se excluye", if (n_na == 1) " 1 valor perdido" else
          paste0("n ", n_na, " valores perdidos"), " de '", var_y, "'.\n",
        sep = "")

  ## 8.1 Descripción
  seccion("1. Descripci\u00f3n de la muestra")
  frec <- table(factor(y, levels = niveles))
  tabla <- data.frame(n = as.vector(frec),
                      Porcentaje = fpct(as.vector(frec) / n),
                      row.names = niveles)
  tabla <- rbind(tabla, Total = data.frame(n = n, Porcentaje = "100.0 %"))
  print(tabla)
  cat("\n")
  cat("  Proporci\u00f3n muestral de '", categoria, "': p = ", x, " / ", n, " = ",
      f3(p_hat), " (", fpct(p_hat), ")\n", sep = "")
  cat("  Error est\u00e1ndar: EE = \u221a(p(1 - p) / n) = ", f3(ee), "\n", sep = "")
  if (length(niveles) > 2) {
    cat("\n")
    .imprimir(paste0("'", var_y, "' tiene ", length(niveles), " categor\u00edas: ",
                     "se compara '", categoria, "' frente al resto de ",
                     "categor\u00edas juntas."))
  }

  ## 8.2 Condiciones
  seccion("2. Condiciones de aplicaci\u00f3n")
  .imprimir(paste0(
    "a) Independencia: cada observaci\u00f3n debe corresponder a un individuo ",
    "distinto. No se puede comprobar con los datos; depende del dise\u00f1o."))
  cat("\n")
  .imprimir(paste0(
    "b) Variable dicot\u00f3mica: cada individuo pertenece o no a la categor\u00eda '",
    categoria, "'."))
  cat("\n")
  .imprimir(paste0(
    "c) Frecuencias esperadas bajo H0: n x p0 = ", f2(esp_si),
    " y n x (1 - p0) = ", f2(esp_no), ". ",
    if (exacta_rec) "Alguna es < 5, por lo que se recomienda la binomial exacta."
    else "Ambas son >= 5, por lo que la aproximaci\u00f3n normal es adecuada."))
  if (metodo == "normal" && exacta_rec) {
    .imprimir(paste0(
      "ATENCI\u00d3N: has elegido la aproximaci\u00f3n normal aunque no se cumplen sus ",
      "condiciones. El valor p puede ser poco fiable; repite el an\u00e1lisis con ",
      "la binomial exacta."), sangria = 4)
  } else if (metodo == "exacta") {
    .imprimir(paste0(
      "La binomial exacta no necesita esta condici\u00f3n: calcula las ",
      "probabilidades directamente con la distribuci\u00f3n binomial."), sangria = 4)
  }

  ## 8.3 Hipótesis
  seccion("3. Hip\u00f3tesis")
  .imprimir(paste0("H0: ", pi_s, " ", simbolo_H0, " ", p0_txt,
                   "   (la proporci\u00f3n poblacional de '", categoria, "' es ",
                   sentido_H0, " ", p0_txt, ")"))
  .imprimir(paste0("H1: ", pi_s, " ", simbolo, " ", p0_txt,
                   "   (la proporci\u00f3n poblacional de '", categoria, "' es ",
                   sentido, " ", p0_txt, ")"))
  cat("\n")
  .imprimir(paste0("Contraste ", if (alternativa == "two.sided") "bilateral"
                   else "unilateral", ". ", pi_s, " es la proporci\u00f3n en la ",
                   "poblaci\u00f3n, que se desconoce; la proporci\u00f3n muestral (",
                   f3(p_hat), ") es su estimaci\u00f3n."))
  if (alternativa != "two.sided") {
    .imprimir(paste0(
      "H0 recoge todos los valores que no est\u00e1n en H1. El valor p se calcula ",
      "en el l\u00edmite de H0 (", pi_s, " = ", p0_txt, "), que es el caso m\u00e1s ",
      "desfavorable: si los datos permiten rechazar ", pi_s, " = ", p0_txt,
      ", tambi\u00e9n permiten rechazar cualquier otro valor de H0."))
  }

  ## 8.4 Estadístico
  seccion("4. Estad\u00edstico de contraste")
  if (metodo == "normal") {
    cat("  EE bajo H0 = \u221a(p0(1 - p0) / n) = \u221a(", p0_txt, " x ",
        format(1 - p0), " / ", n, ") = ", f3(ee0), "\n", sep = "")
    cat("  Correcci\u00f3n de continuidad = 1 / (2n) = ", formatC(cc, format = "f",
        digits = 4), "\n", sep = "")
    cat("  z = (|p - p0| - 1/(2n)) / EE = (|", f3(p_hat), " - ", p0_txt,
        "| - ", formatC(cc, format = "f", digits = 4), ") / ", f3(ee0),
        " = ", f3(abs(z)), "\n", sep = "")
    if (z < 0)
      cat("  Con el signo de p - p0 (la proporci\u00f3n observada es menor): z = ",
          f3(z), "\n", sep = "")
    cat("  Valor cr\u00edtico: z", switch(alternativa,
          two.sided = paste0("(", 1 - alpha / 2, ") = \u00b1", f3(abs(z_crit))),
          less = paste0("(", alpha, ") = ", f3(z_crit)),
          greater = paste0("(", 1 - alpha, ") = ", f3(z_crit))), "\n", sep = "")
    cat("\n")
    .imprimir(paste0(
      "El estad\u00edstico z indica a cu\u00e1ntos errores est\u00e1ndar est\u00e1 la proporci\u00f3n ",
      "observada de la de referencia. R (y R Commander) lo presentan como ",
      "chi cuadrado con 1 grado de libertad: X\u00b2 = z\u00b2 = ", f3(chi2), "."))
  } else {
    cat("  N\u00famero de casos con '", categoria, "': x = ", x, " de n = ", n,
        "\n", sep = "")
    cat("  N\u00famero esperado si H0 fuera cierta: n x p0 = ", f2(esp_si), "\n",
        sep = "")
    cat("  Distribuci\u00f3n bajo H0: X ~ Binomial(n = ", n, ", p = ", p0_txt, ")\n",
        sep = "")
    cat("\n")
    .imprimir(paste0(
      "La prueba exacta no usa un estad\u00edstico aproximado: calcula con la ",
      "distribuci\u00f3n binomial la probabilidad de obtener ", casos(x), " o un ",
      "resultado m\u00e1s extremo si la proporci\u00f3n poblacional fuera ", p0_txt, "."))
  }

  ## 8.5 Valor p y decisión
  seccion("5. Valor p y decisi\u00f3n")
  cat("  Valor p: ", .fmt_p(p), "\n\n", sep = "")
  si_H0 <- paste0("si la proporci\u00f3n poblacional fuera ", p0_txt,
                  if (alternativa == "two.sided") "" else " (el l\u00edmite de H0)")
  .imprimir(paste0(
    "Interpretaci\u00f3n del valor p: ", si_H0, ", la probabilidad de obtener ",
    if (metodo == "exacta") {
      switch(alternativa,
        two.sided = paste0("un n\u00famero de casos tan improbable como ", x,
                           " (o m\u00e1s), en cualquier direcci\u00f3n,"),
        less = paste0(casos(x), " o menos, P(X \u2264 ", x, "),"),
        greater = paste0(casos(x), " o m\u00e1s, P(X \u2265 ", x, "),"))
    } else {
      switch(alternativa,
        two.sided = paste0("una proporci\u00f3n muestral tan alejada de ", p0_txt,
                           " como la observada (o m\u00e1s), en cualquier direcci\u00f3n,"),
        less = "una proporci\u00f3n muestral tan baja como la observada (o m\u00e1s baja)",
        greater = "una proporci\u00f3n muestral tan alta como la observada (o m\u00e1s alta)")
    },
    " ser\u00eda ", .fmt_p(p), "."))
  cat("\n")
  if (rechaza) {
    .imprimir(paste0(
      "Decisi\u00f3n: se rechaza H0 (p", .comparar_p(p), " < ", alpha, "). Hay ",
      "evidencia de que la proporci\u00f3n poblacional de '", categoria, "' es ",
      sentido, " ", p0_txt, "."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No ",
      "hay evidencia suficiente para afirmar que la proporci\u00f3n poblacional de '",
      categoria, "' sea ", sentido, " ", p0_txt, ". Esto no demuestra que H0 ",
      "sea cierta (que sea ", sentido_H0, " ", p0_txt, ")."))
  }
  if ((alternativa == "less" && dif > 0) ||
      (alternativa == "greater" && dif < 0)) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: la proporci\u00f3n muestral (", f3(p_hat), ") est\u00e1 en la ",
      "direcci\u00f3n contraria a la planteada en H1, por eso el valor p es tan ",
      "alto. La hip\u00f3tesis alternativa debe fijarse antes de ver los datos; ",
      "no la cambies a posteriori."))
  }

  ## 8.6 Intervalo de confianza
  seccion(paste0("6. Intervalo de confianza al ", conf_txt,
                 " para la proporci\u00f3n poblacional"))
  cat("  M\u00e9todo: ", nombre_ic, "\n", sep = "")
  if (alternativa == "two.sided") {
    cat("  IC ", conf_txt, ": ", f3(ic[1]), " a ", f3(ic[2]), "  (",
        fpct(ic[1]), " a ", fpct(ic[2]), ")\n\n", sep = "")
    .imprimir(paste0(
      "Con una confianza del ", conf_txt, ", la proporci\u00f3n poblacional de '",
      categoria, "' est\u00e1 entre el ", fpct(ic[1]), " y el ", fpct(ic[2]), "."))
  } else {
    limite <- if (alternativa == "less") ic[2] else ic[1]
    cat("  IC unilateral ", conf_txt, ": ",
        if (alternativa == "less") paste0("0 a ", f3(limite))
        else paste0(f3(limite), " a 1"), "\n", sep = "")
    cat("  IC bilateral ", conf_txt, ":  ", f3(ic_bil[1]), " a ", f3(ic_bil[2]),
        "  (", fpct(ic_bil[1]), " a ", fpct(ic_bil[2]), ")\n\n", sep = "")
    .imprimir(paste0(
      "Con una confianza del ", conf_txt, ", la proporci\u00f3n poblacional es ",
      if (alternativa == "less") "menor que " else "mayor que ", f3(limite),
      " (", fpct(limite), "). Es el intervalo que corresponde a un contraste ",
      "unilateral; en las publicaciones suele darse el bilateral."))
  }
  dentro <- p0 >= ic[1] && p0 <= ic[2]
  if (dentro != rechaza) {
    .imprimir(paste0(
      "El valor de referencia (", p0_txt, ") ",
      if (dentro) "queda dentro" else "queda fuera", " del intervalo, ",
      "coherente con que ", if (rechaza) "se rechace" else "no se rechace",
      " H0."))
  } else {
    .imprimir(paste0(
      "El valor de referencia (", p0_txt, ") ",
      if (dentro) "queda dentro" else "queda fuera", " del intervalo aunque ",
      if (rechaza) "se rechaza" else "no se rechaza", " H0. Ocurre en casos ",
      "l\u00edmite porque el intervalo y la prueba se calculan con m\u00e9todos ",
      "ligeramente distintos; la decisi\u00f3n se toma con el valor p."))
  }

  ## 8.7 Tamaño del efecto
  seccion("7. Tama\u00f1o del efecto")
  .imprimir(paste0("Diferencia de proporciones: p - p0 = ", f3(p_hat), " - ",
                   p0_txt, " = ", f3(dif), " (", fpp(dif),
                   " puntos porcentuales; IC ", conf_txt, " bilateral: ",
                   fpp(ic_bil[1] - p0), " a ", fpp(ic_bil[2] - p0), ")"))
  mag <- if (abs(h) < 0.2) "despreciable" else if (abs(h) < 0.5) "peque\u00f1o"
         else if (abs(h) < 0.8) "mediano" else "grande"
  .imprimir(paste0("h de Cohen = 2\u00b7arcsen(\u221ap) - 2\u00b7arcsen(\u221ap0) = ",
                   f3(h), "  (efecto ", mag, ")"))
  cat("\n")
  .imprimir(paste0(
    "El valor p indica si la diferencia es estad\u00edsticamente significativa, ",
    "pero no si es importante. Juzga la relevancia con la diferencia en ",
    "puntos porcentuales y su intervalo. Los umbrales de Cohen para h ",
    "(0.2 peque\u00f1o, 0.5 mediano, 0.8 grande) son solo orientativos."))
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
      "(n = ", n, "). Puede deberse a poca potencia, no a la ausencia de ",
      "diferencia."))
  }

  ## 8.8 Redacción
  seccion("8. C\u00f3mo redactar el resultado")
  frase_dir <- if (rechaza) {
    paste0("significativamente ", if (dif > 0) "superior" else "inferior",
           " al valor de referencia del ", fpct(p0))
  } else {
    paste0("sin diferencias significativas respecto al valor de referencia del ",
           fpct(p0))
  }
  .imprimir(paste0(
    "La proporci\u00f3n de '", categoria, "' en ", var_y, " fue del ", fpct(p_hat),
    " (", x, "/", n, "), ", frase_dir, " (",
    if (metodo == "normal") paste0("aproximaci\u00f3n normal con correcci\u00f3n de ",
                                   "continuidad: z = ", f3(z), "; ")
    else "prueba binomial exacta: ",
    .p_redaccion(p),
    if (alternativa != "two.sided") ", contraste unilateral" else "",
    "). Diferencia: ", fpp(dif), " puntos porcentuales (IC ", conf_txt, ": ",
    fpp(ic_bil[1] - p0), " a ", fpp(ic_bil[2] - p0), ")."))

  ## 8.9 R Commander
  cat("\n", linea, "\n", sep = "")
  cat("En R Commander: Estad\u00edsticos > Proporciones > Test de proporciones para una muestra...\n")
  cat("  (opci\u00f3n '", if (metodo == "normal")
        "Aproximaci\u00f3n normal con correcci\u00f3n de continuidad" else "Binomial exacto",
      "')\n", sep = "")
  cat("Orden equivalente:\n  ", if (metodo == "normal") "prop.test" else "binom.test",
      "(x=", x, ", n=", n, ", p=", p0_txt, ", alternative='", alternativa,
      "', conf.level=", conf, if (metodo == "normal") ", correct=TRUE" else "",
      ")\n", sep = "")
  if (length(niveles) > 2 || categoria != niveles[1]) {
    cat("\n")
    .imprimir(paste0(
      "R Commander solo admite variables con dos categor\u00edas y calcula la ",
      "proporci\u00f3n de la primera. ",
      if (length(niveles) > 2)
        paste0("Recodifica antes '", var_y, "' en dos categor\u00edas (Datos > ",
               "Modificar variables del conjunto de datos activo > Recodificar ",
               "variables), dejando '", categoria, "' como primera.")
      else
        paste0("Reordena antes '", var_y, "' (Datos > Modificar variables del ",
               "conjunto de datos activo > Reordenar niveles de factor) para ",
               "que '", categoria, "' sea la primera; si no, R Commander ",
               "analizar\u00e1 la proporci\u00f3n de '", niveles[1], "'.")),
      sangria = 0)
  }
  cat(linea, "\n", sep = "")

  invisible(list(
    recuentos = c(x = x, n = n),
    proporcion = p_hat,
    prueba = prueba,
    metodo = metodo,
    exacta_recomendada = exacta_rec,
    diferencia = dif,
    h_cohen = h,
    rechaza_H0 = rechaza
  ))
}
