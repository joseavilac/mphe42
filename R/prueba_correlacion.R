#' Prueba de correlación interactiva
#'
#' Versión interactiva y comentada del test de correlación de R Commander
#' (Estadísticos > Resúmenes > Test de correlación), basada en
#' \code{cor.test()}. Pregunta por consola el data.frame, las dos variables
#' cuantitativas y la hipótesis alternativa; comprueba la normalidad de las
#' dos variables y, según el resultado, recomienda el coeficiente de Pearson
#' o los de Spearman y Kendall antes de elegir el método. Presenta un informe
#' completo: descriptivos, supuestos, hipótesis, estadístico, decisión,
#' intervalo de confianza (Pearson), interpretación de la magnitud y una
#' frase de redacción de resultados.
#'
#' @param alpha Nivel de significación. El nivel de confianza del intervalo
#'   es 1 - alpha. Por defecto 0.05.
#' @return De forma invisible, una lista con los descriptivos, la
#'   comprobación de normalidad y el resultado de \code{cor.test()}.
#' @export
#' @importFrom stats cor.test cov sd median quantile qt shapiro.test complete.cases
#' @importFrom grDevices boxplot.stats
#' @importFrom utils menu
prueba_correlacion <- function(alpha = 0.05) {

  if (!interactive())
    stop("prueba_correlacion() es interactiva: ejec\u00fatala desde la consola de R.",
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

  ## 2. Variables ---------------------------------------------------------
  var_x <- .pedir_variable(datos, "cuantitativa",
    "\u00bfCu\u00e1l es la primera variable cuantitativa?")
  if (is.null(var_x)) return(.cancelar())
  repeat {
    var_y <- .pedir_variable(datos, "cuantitativa",
      "\u00bfCu\u00e1l es la segunda variable cuantitativa?")
    if (is.null(var_y)) return(.cancelar())
    if (!identical(var_y, var_x)) break
    cat("Elige una variable distinta de '", var_x, "'.\n", sep = "")
  }

  ## 3. Datos (pares completos) ------------------------------------------
  completos <- complete.cases(datos[[var_x]], datos[[var_y]])
  n_excl <- sum(!completos)
  x <- datos[[var_x]][completos]
  y <- datos[[var_y]][completos]
  n <- length(x)
  if (n < 3)
    stop("Hay menos de 3 pares de valores completos: no se puede calcular ",
         "la correlaci\u00f3n.", call. = FALSE)
  if (sd(x) == 0 || sd(y) == 0)
    stop("'", if (sd(x) == 0) var_x else var_y, "' tiene el mismo valor en ",
         "todos los casos: no se puede calcular la correlaci\u00f3n.", call. = FALSE)

  ## 4. Hipótesis alternativa --------------------------------------------
  i <- menu(c("Bilateral:   hay correlaci\u00f3n (positiva o negativa)",
              "Unilateral:  la correlaci\u00f3n es negativa  (< 0)",
              "Unilateral:  la correlaci\u00f3n es positiva  (> 0)"),
            title = "\n\u00bfCu\u00e1l es la hip\u00f3tesis alternativa (H1)?")
  if (i == 0) return(.cancelar())
  alternativa <- c("two.sided", "less", "greater")[i]
  simbolo <- c("\u2260", "<", ">")[i]
  simbolo_H0 <- c("=", "\u2265", "\u2264")[i]
  sentido <- c("distinta de 0", "negativa", "positiva")[i]
  sentido_H0 <- c("igual a 0", "mayor o igual que 0", "menor o igual que 0")[i]

  ## 5. Comprobación de normalidad y elección del método -----------------
  sw <- function(v) if (length(v) <= 5000) shapiro.test(v)$p.value else NA
  sw_x <- sw(x)
  sw_y <- sw(y)
  no_normal <- c(!is.na(sw_x) && sw_x < alpha, !is.na(sw_y) && sw_y < alpha)
  normalidad <- !any(no_normal)
  texto_sw <- function(nom, pv) {
    if (is.na(pv)) return(paste0("'", nom, "': n > 5000, no se aplica ",
                                 "Shapiro-Wilk."))
    paste0("'", nom, "': p", .comparar_p(pv),
           if (pv < alpha) paste0(" < ", alpha, ", se aleja de la normal.")
           else paste0(" >= ", alpha, ", sin evidencia de no normalidad."))
  }

  cat("\nComprobaci\u00f3n de la normalidad (Shapiro-Wilk, n = ", n, " pares):\n",
      sep = "")
  .imprimir(texto_sw(var_x, sw_x), sangria = 4)
  .imprimir(texto_sw(var_y, sw_y), sangria = 4)
  if (normalidad) {
    .imprimir(paste0("Se puede asumir normalidad en las dos variables: se ",
                     "recomienda el coeficiente de Pearson."))
  } else {
    .imprimir(paste0("No se puede asumir normalidad en ",
                     if (all(no_normal)) "ninguna de las dos variables"
                     else paste0("'", c(var_x, var_y)[no_normal], "'"),
                     ": no se recomienda Pearson. Elige Spearman o Kendall, ",
                     "que no requieren normalidad."))
  }
  i <- menu(c(paste0("Pearson (r)",
                     if (normalidad) "   <-- RECOMENDADO"
                     else "   (no recomendado: falta normalidad)"),
              paste0("Spearman (rho)",
                     if (!normalidad) "   <-- RECOMENDADO" else ""),
              paste0("Kendall (tau)",
                     if (!normalidad) paste0("   <-- RECOMENDADO (preferible ",
                                             "con n peque\u00f1o o muchos empates)")
                     else "")),
            title = "\n\u00bfQu\u00e9 coeficiente de correlaci\u00f3n quieres utilizar?")
  if (i == 0) return(.cancelar())
  metodo <- c("pearson", "spearman", "kendall")[i]
  nombre_coef <- c(pearson = "r de Pearson", spearman = "rho de Spearman",
                   kendall = "tau de Kendall")[[metodo]]
  simb_coef <- c(pearson = "r", spearman = "rho", kendall = "tau")[[metodo]]
  simb_pob <- c(pearson = "\u03c1", spearman = "\u03c1s",
                kendall = "\u03c4")[[metodo]]

  ## 6. Cálculos ------------------------------------------------------------
  hay_empates <- anyDuplicated(x) > 0 || anyDuplicated(y) > 0
  ## Con empates, R no puede dar el valor p exacto de Spearman o Kendall y
  ## avisa; el aviso se sustituye por una nota en el informe
  prueba <- suppressWarnings(cor.test(x, y, alternative = alternativa,
                                      method = metodo, conf.level = conf))
  coef <- unname(prueba$estimate)
  p <- prueba$p.value
  rechaza <- p < alpha
  est <- unname(prueba$statistic)
  nom_est <- names(prueba$statistic)
  gl <- if (!is.null(prueba$parameter)) unname(prueba$parameter) else NA
  if (metodo == "pearson") {
    ic <- prueba$conf.int
    ic_bil <- if (n > 3) cor.test(x, y, method = "pearson",
                                  conf.level = conf)$conf.int else NULL
    t_crit <- switch(alternativa,
                     two.sided = qt(1 - alpha / 2, gl),
                     less = qt(alpha, gl),
                     greater = qt(1 - alpha, gl))
  }
  p_exacto <- metodo == "pearson" ||
    (!hay_empates && ((metodo == "spearman" && n < 1290) ||
                      (metodo == "kendall" && n < 50)))

  resumen <- function(v) {
    q <- quantile(v, c(0.25, 0.5, 0.75), names = FALSE)
    c(media = mean(v), de = sd(v), mediana = q[2], p25 = q[1], p75 = q[3],
      min = min(v), max = max(v))
  }
  dX <- resumen(x)
  dY <- resumen(y)
  covar <- cov(x, y)
  r_pearson <- covar / (sd(x) * sd(y))
  atip_x <- boxplot.stats(x)$out
  atip_y <- boxplot.stats(y)$out

  ## Magnitud según Evans (1996), sobre el valor absoluto del coeficiente
  magnitud <- function(v) {
    a <- abs(v)
    if (a < 0.2) "muy d\u00e9bil" else if (a < 0.4) "d\u00e9bil"
    else if (a < 0.6) "moderada" else if (a < 0.8) "fuerte" else "muy fuerte"
  }
  direccion <- if (coef > 0) "positiva" else if (coef < 0) "negativa" else "nula"

  ## Formato
  f3 <- function(v) .fmt_num(v, 3)
  linea <- paste(rep("-", 70), collapse = "")
  seccion <- function(titulo) cat("\n", linea, "\n", titulo, "\n\n", sep = "")

  ## 7. Informe -------------------------------------------------------------
  cat("\n", linea, "\n", sep = "")
  cat("Prueba de correlaci\u00f3n: ", nombre_coef, "\n", sep = "")
  cat("Variables: ", var_x, " y ", var_y, "   Base de datos: ", nombre_df,
      "\n", sep = "")
  cat("Nivel de significaci\u00f3n: alfa = ", alpha, "\n", sep = "")
  if (n_excl > 0)
    cat("Se excluye", if (n_excl == 1) " 1 caso" else
          paste0("n ", n_excl, " casos"), " sin valor en alguna de las dos ",
        "variables.\n", sep = "")

  ## 7.1 Descripción
  seccion("1. Descripci\u00f3n de las variables (pares completos)")
  fila <- function(dd) c(n, f3(dd["media"]), f3(dd["de"]), f3(dd["mediana"]),
                         paste0(f3(dd["p25"]), " - ", f3(dd["p75"])),
                         paste0(f3(dd["min"]), " - ", f3(dd["max"])))
  desc <- data.frame(fila(dX), fila(dY),
    row.names = c("n (pares completos)", "Media", "Desviaci\u00f3n t\u00edpica (DE)",
                  "Mediana", "Rango intercuart\u00edlico (P25 - P75)",
                  "M\u00ednimo - M\u00e1ximo"))
  names(desc) <- c(var_x, var_y)
  print(desc)
  cat("\n")
  cat("  Covarianza: cov(", var_x, ", ", var_y, ") = ", f3(covar), "\n", sep = "")
  cat("  cov = \u03a3(xi - media x)(yi - media y) / (n - 1)\n\n")
  .imprimir(paste0(
    "Signo: ", if (covar > 0) paste0("positivo, los valores altos de '", var_x,
      "' tienden a ir con valores altos de '", var_y, "'.")
    else if (covar < 0) paste0("negativo, los valores altos de '", var_x,
      "' tienden a ir con valores bajos de '", var_y, "'.")
    else "nulo, no hay tendencia conjunta."))
  .imprimir(paste0(
    "Su valor depende de las unidades de las variables (se expresa en ",
    "unidades de '", var_x, "' \u00b7 unidades de '", var_y, "'), por lo que no ",
    "sirve para valorar la fuerza de la relaci\u00f3n. Para eso se usa el ",
    "coeficiente de correlaci\u00f3n de Pearson, que es la covarianza ",
    "estandarizada: r = cov / (DE de '", var_x, "' \u00b7 DE de '", var_y,
    "') = ", f3(covar), " / (", f3(dX["de"]), " \u00b7 ", f3(dY["de"]),
    ") = ", f3(r_pearson), ".",
    if (metodo != "pearson") paste0(" Este r es el de Pearson; el ",
      nombre_coef, " que has elegido se calcula sobre los rangos y no ",
      "procede directamente de esta covarianza.") else ""))

  ## 7.2 Supuestos
  seccion("2. Supuestos y elecci\u00f3n del coeficiente")
  .imprimir(paste0(
    "a) Independencia: cada par de valores debe corresponder a un individuo ",
    "distinto. No se puede comprobar con los datos; depende del dise\u00f1o."))
  cat("\n")
  .imprimir(paste0(
    "b) Forma de la relaci\u00f3n: ",
    if (metodo == "pearson")
      "Pearson solo mide relaci\u00f3n LINEAL (en l\u00ednea recta). "
    else "Spearman y Kendall miden relaci\u00f3n MON\u00d3TONA (cuando una aumenta, la otra tiende a aumentar, o a disminuir, aunque no en l\u00ednea recta). ",
    "No se puede comprobar con una prueba: mira el diagrama de dispersi\u00f3n ",
    "(en R Commander: Gr\u00e1ficas > Diagrama de dispersi\u00f3n...)."))
  cat("\n")
  cat("  c) Normalidad de cada variable (Shapiro-Wilk):\n")
  .imprimir(texto_sw(var_x, sw_x), sangria = 4)
  .imprimir(texto_sw(var_y, sw_y), sangria = 4)
  .imprimir(if (normalidad)
    "Se asume normalidad en las dos variables."
    else "No se puede asumir normalidad en las dos variables.", sangria = 4)
  cat("\n")
  texto_atip <- function(nom, at) {
    if (length(at) == 0) return(paste0("'", nom, "': ninguno."))
    paste0("'", nom, "': ", length(at), " (",
           paste(f3(utils::head(sort(at), 10)), collapse = ", "),
           if (length(at) > 10) ", ..." else "", ").")
  }
  cat("  d) Valores at\u00edpicos (criterio del diagrama de caja, 1.5 x RIC):\n")
  .imprimir(texto_atip(var_x, atip_x), sangria = 4)
  .imprimir(texto_atip(var_y, atip_y), sangria = 4)
  if (length(atip_x) + length(atip_y) > 0)
    .imprimir(paste0("Pearson es muy sensible a los valores at\u00edpicos: unos ",
                     "pocos pueden crear o esconder una correlaci\u00f3n. ",
                     "Spearman y Kendall, al trabajar con rangos, lo son ",
                     "mucho menos."), sangria = 4)
  cat("\n")
  motivo <- if (metodo == "pearson") {
    if (normalidad) paste0("Se usa Pearson porque se asume normalidad en las ",
                           "dos variables.")
    else paste0("ATENCI\u00d3N: has elegido Pearson aunque no se puede asumir ",
                "normalidad. El valor p y el intervalo de confianza pueden no ",
                "ser fiables; repite el an\u00e1lisis con Spearman o Kendall.")
  } else {
    paste0("Se usa ", nombre_coef, ", que trabaja con los rangos (\u00f3rdenes) ",
           "de los valores y no requiere normalidad.",
           if (normalidad) paste0(" Como se asume normalidad, Pearson tambi\u00e9n ",
                                  "ser\u00eda adecuado y tiene m\u00e1s potencia.") else "")
  }
  .imprimir(paste0("e) Coeficiente elegido: ", motivo))

  ## 7.3 Hipótesis
  seccion("3. Hip\u00f3tesis")
  .imprimir(paste0("H0: ", simb_pob, " ", simbolo_H0, " 0   (la correlaci\u00f3n ",
                   "poblacional entre '", var_x, "' y '", var_y, "' es ",
                   sentido_H0, ")"))
  .imprimir(paste0("H1: ", simb_pob, " ", simbolo, " 0   (la correlaci\u00f3n ",
                   "poblacional entre '", var_x, "' y '", var_y, "' es ",
                   sentido, ")"))
  cat("\n")
  .imprimir(paste0("Contraste ", if (alternativa == "two.sided") "bilateral"
                   else "unilateral", ". ", simb_pob, " es el coeficiente en ",
                   "la poblaci\u00f3n, que se desconoce; el coeficiente muestral (",
                   simb_coef, " = ", f3(coef), ") es su estimaci\u00f3n."))
  if (alternativa != "two.sided") {
    .imprimir(paste0(
      "H0 recoge todos los valores que no est\u00e1n en H1. El valor p se calcula ",
      "en el l\u00edmite de H0 (", simb_pob, " = 0), que es el caso m\u00e1s ",
      "desfavorable: si los datos permiten rechazar ", simb_pob, " = 0, ",
      "tambi\u00e9n permiten rechazar cualquier otro valor de H0."))
  }

  ## 7.4 Estadístico
  seccion("4. Coeficiente y estad\u00edstico de contraste")
  cat("  Coeficiente: ", simb_coef, " = ", f3(coef), "\n", sep = "")
  if (metodo == "pearson") {
    cat("  t = r \u00b7 \u221a(n - 2) / \u221a(1 - r\u00b2) = ", f3(coef),
        " \u00b7 \u221a", n - 2, " / \u221a(1 - ",
        if (coef < 0) paste0("(", f3(coef), ")") else f3(coef), "\u00b2) = ",
        f3(est), "\n", sep = "")
    cat("  Grados de libertad: gl = n - 2 = ", gl, "\n", sep = "")
    cat("  Valor cr\u00edtico: t", switch(alternativa,
          two.sided = paste0("(", 1 - alpha / 2, "; ", gl, ") = \u00b1", f3(abs(t_crit))),
          less = paste0("(", alpha, "; ", gl, ") = ", f3(t_crit)),
          greater = paste0("(", 1 - alpha, "; ", gl, ") = ", f3(t_crit))),
        "\n", sep = "")
    cat("\n")
    .imprimir(paste0("El estad\u00edstico t mide lo lejos que est\u00e1 r de 0 teniendo ",
                     "en cuenta el tama\u00f1o de la muestra."))
  } else {
    cat("  Estad\u00edstico que muestra R: ", nom_est, " = ", f3(est), "\n", sep = "")
    cat("\n")
    .imprimir(if (metodo == "spearman")
      paste0("Spearman calcula la correlaci\u00f3n de Pearson sobre los rangos de ",
             "los valores. S es la suma de los cuadrados de las diferencias ",
             "entre los rangos de cada par; cuanto menor es S, m\u00e1s se parecen ",
             "los dos \u00f3rdenes (correlaci\u00f3n positiva).")
      else paste0("Kendall compara todos los pares de individuos y cuenta ",
                  "cu\u00e1ntos est\u00e1n en el mismo orden en las dos variables ",
                  "(concordantes) y cu\u00e1ntos en orden inverso (discordantes). ",
                  if (nom_est == "T") "T es el n\u00famero de pares concordantes."
                  else "z es la aproximaci\u00f3n normal del estad\u00edstico."))
    if (!p_exacto) {
      cat("\n")
      .imprimir(paste0(
        "Nota: ", if (hay_empates) "hay valores repetidos (empates), por lo que "
        else "por el tama\u00f1o de la muestra, ", "R calcula un valor p ",
        "aproximado en lugar del exacto. Si lo ejecutas en R Commander ver\u00e1s ",
        "el aviso 'Cannot compute exact p-value with ties'; no es un error y ",
        "el resultado es v\u00e1lido."))
    }
  }

  ## 7.5 Valor p y decisión
  seccion("5. Valor p y decisi\u00f3n")
  cat("  Valor p: ", .fmt_p(p), "\n\n", sep = "")
  .imprimir(paste0(
    "Interpretaci\u00f3n del valor p: si la correlaci\u00f3n poblacional fuera 0",
    if (alternativa == "two.sided") "" else " (el l\u00edmite de H0)",
    ", la probabilidad de obtener un coeficiente ",
    switch(alternativa,
           two.sided = "tan alejado de 0 como el observado (o m\u00e1s), en cualquier direcci\u00f3n,",
           less = "tan negativo como el observado (o m\u00e1s)",
           greater = "tan positivo como el observado (o m\u00e1s)"),
    " ser\u00eda ", .fmt_p(p), "."))
  cat("\n")
  if (rechaza) {
    .imprimir(paste0(
      "Decisi\u00f3n: se rechaza H0 (p", .comparar_p(p), " < ", alpha, "). Hay ",
      "evidencia de que la correlaci\u00f3n poblacional entre '", var_x, "' y '",
      var_y, "' es ", sentido, "."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No ",
      "hay evidencia suficiente para afirmar que la correlaci\u00f3n poblacional ",
      "sea ", sentido, ". Esto no demuestra que H0 sea cierta (que la ",
      "correlaci\u00f3n sea ", sentido_H0, ")."))
  }
  if ((alternativa == "less" && coef > 0) ||
      (alternativa == "greater" && coef < 0)) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: el coeficiente observado (", f3(coef), ") tiene el signo ",
      "contrario al planteado en H1, por eso el valor p es tan alto. La ",
      "hip\u00f3tesis alternativa debe fijarse antes de ver los datos; no la ",
      "cambies a posteriori."))
  }

  ## 7.6 Intervalo de confianza
  seccion(paste0("6. Intervalo de confianza al ", conf_txt))
  if (metodo == "pearson" && !is.null(ic)) {
    cat("  M\u00e9todo: transformaci\u00f3n z de Fisher\n")
    if (alternativa == "two.sided") {
      cat("  IC ", conf_txt, ": ", f3(ic[1]), " a ", f3(ic[2]), "\n\n", sep = "")
      .imprimir(paste0(
        "Con una confianza del ", conf_txt, ", la correlaci\u00f3n poblacional ",
        "est\u00e1 entre ", f3(ic[1]), " y ", f3(ic[2]), "."))
    } else {
      limite <- if (alternativa == "less") ic[2] else ic[1]
      cat("  IC unilateral ", conf_txt, ": ",
          if (alternativa == "less") paste0("-1 a ", f3(limite))
          else paste0(f3(limite), " a 1"), "\n", sep = "")
      if (!is.null(ic_bil))
        cat("  IC bilateral ", conf_txt, ":  ", f3(ic_bil[1]), " a ",
            f3(ic_bil[2]), "\n", sep = "")
      cat("\n")
      .imprimir(paste0(
        "Con una confianza del ", conf_txt, ", la correlaci\u00f3n poblacional es ",
        if (alternativa == "less") "menor que " else "mayor que ", f3(limite),
        ". Es el intervalo que corresponde a un contraste unilateral; en las ",
        "publicaciones suele darse el bilateral."))
    }
    dentro <- 0 >= ic[1] && 0 <= ic[2]
    .imprimir(paste0(
      "El 0 (ausencia de correlaci\u00f3n) ", if (dentro) "queda dentro" else
      "queda fuera", " del intervalo, coherente con que ",
      if (rechaza) "se rechace" else "no se rechace", " H0."))
  } else if (metodo == "pearson") {
    .imprimir("Con n = 3 no se puede calcular el intervalo de confianza.")
  } else {
    .imprimir(paste0(
      "La funci\u00f3n cor.test() (y R Commander) no calcula intervalo de ",
      "confianza para ", nombre_coef, "; solo lo da para Pearson."))
  }

  ## 7.7 Magnitud
  seccion("7. Magnitud y sentido de la correlaci\u00f3n")
  cat("  ", simb_coef, " = ", f3(coef), ": correlaci\u00f3n ", direccion, " ",
      magnitud(coef), "\n", sep = "")
  if (metodo == "pearson")
    cat("  Coeficiente de determinaci\u00f3n: r\u00b2 = ", f3(coef^2), " (",
        formatC(100 * coef^2, format = "f", digits = 1), " %)\n", sep = "")
  cat("\n")
  .imprimir(paste0(
    "Sentido: ", switch(direccion,
      positiva = paste0("positiva, cuando '", var_x, "' aumenta, '", var_y,
                        "' tiende a aumentar."),
      negativa = paste0("negativa, cuando '", var_x, "' aumenta, '", var_y,
                        "' tiende a disminuir."),
      nula = "no hay tendencia en ning\u00fan sentido.")))
  .imprimir(paste0(
    "Magnitud (valor absoluto, criterios de Evans, 1996): < 0.2 muy ",
    "d\u00e9bil; 0.2 a 0.4 d\u00e9bil; 0.4 a 0.6 moderada; 0.6 a 0.8 fuerte; ",
    ">= 0.8 muy fuerte.",
    if (metodo == "kendall") paste0(" La tau de Kendall suele ser menor que ",
                                    "r o rho para la misma relaci\u00f3n, as\u00ed que ",
                                    "estos umbrales la infravaloran.") else ""))
  if (metodo == "pearson")
    .imprimir(paste0("r\u00b2 indica que el ",
                     formatC(100 * coef^2, format = "f", digits = 1),
                     " % de la variabilidad de una variable se asocia ",
                     "linealmente con la de la otra."))
  cat("\n")
  .imprimir(paste0(
    "El valor p indica si la correlaci\u00f3n es distinta de 0, no si es fuerte. ",
    "Correlaci\u00f3n no implica causalidad: la relaci\u00f3n puede deberse a una ",
    "tercera variable."))
  if (rechaza && abs(coef) < 0.4) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: la correlaci\u00f3n es significativa pero ", magnitud(coef),
      ". Con muestras grandes (n = ", n, "), correlaciones de poca relevancia ",
      "pr\u00e1ctica resultan significativas."))
  }
  if (!rechaza && abs(coef) >= 0.4) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: la correlaci\u00f3n observada no es d\u00e9bil, pero no es ",
      "significativa (n = ", n, "). Puede deberse a poca potencia, no a la ",
      "ausencia de relaci\u00f3n."))
  }

  ## 7.8 Redacción
  seccion("8. C\u00f3mo redactar el resultado")
  .imprimir(paste0(
    if (rechaza) paste0("Se observ\u00f3 una correlaci\u00f3n ", direccion, " ",
                        magnitud(coef), " y estad\u00edsticamente significativa ",
                        "entre ", var_x, " y ", var_y)
    else paste0("No se observ\u00f3 una correlaci\u00f3n estad\u00edsticamente ",
                "significativa entre ", var_x, " y ", var_y),
    " (", nombre_coef, " = ", f3(coef),
    if (metodo == "pearson" && !is.null(ic)) {
      ii <- if (alternativa == "two.sided") ic else ic_bil
      if (!is.null(ii)) paste0("; IC ", conf_txt, ": ", f3(ii[1]), " a ",
                               f3(ii[2])) else ""
    } else "",
    "; ", .p_redaccion(p),
    if (alternativa != "two.sided") ", contraste unilateral" else "",
    "; n = ", n, ")."))

  ## 7.9 R Commander
  cat("\n", linea, "\n", sep = "")
  cat("En R Commander: Estad\u00edsticos > Res\u00famenes > Test de correlaci\u00f3n...\n")
  cat("Orden equivalente:\n  with(", nombre_df, ", cor.test(", var_x, ", ",
      var_y, ", alternative='", alternativa, "', method='", metodo, "'))\n",
      sep = "")
  if (conf != 0.95 && metodo == "pearson")
    .imprimir(paste0("R Commander usa siempre un IC del 95 %; para otro nivel ",
                     "a\u00f1ade conf.level=", conf, " a la orden."), sangria = 0)
  cat(linea, "\n", sep = "")

  descriptivos <- rbind(dX, dY)
  rownames(descriptivos) <- c(var_x, var_y)
  invisible(list(
    n = n,
    descriptivos = descriptivos,
    shapiro_p = stats::setNames(c(sw_x, sw_y), c(var_x, var_y)),
    normalidad = normalidad,
    prueba = prueba,
    metodo = metodo,
    covarianza = covar,
    coeficiente = coef,
    rechaza_H0 = rechaza
  ))
}
