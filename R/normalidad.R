#' Pruebas de normalidad interactivas
#'
#' Versión interactiva de \code{RcmdrMisc::normalityTest()}. Pregunta por
#' consola el data.frame, la variable cuantitativa, la variable de agrupación
#' (opcional) y la prueba, y presenta para cada grupo las hipótesis, el
#' estadístico, el valor p y la decisión. Con grupos, los valores p se ajustan
#' por el método de Holm, igual que \code{normalityTest()}.
#'
#' @param alpha Nivel de significación para la decisión. Por defecto 0.05.
#' @return De forma invisible, un data.frame con los resultados por grupo.
#' @export
#' @importFrom stats shapiro.test p.adjust
#' @importFrom utils menu
#' @importFrom nortest sf.test ad.test cvm.test lillie.test pearson.test
normalidad <- function(alpha = 0.05) {

  if (!interactive())
    stop("normalidad() es interactiva: ejec\u00fatala desde la consola de R.",
         call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
    stop("'alpha' debe ser un n\u00famero entre 0 y 1.", call. = FALSE)
  if (!requireNamespace("nortest", quietly = TRUE))
    stop("Falta el paquete 'nortest'. Inst\u00e1lalo con install.packages(\"nortest\").",
         call. = FALSE)


  ## 1. Base de datos (por menú) ----------------------------------------
  sel <- .elegir_df()
  if (is.null(sel)) return(.cancelar())
  nombre_df <- sel$nombre
  datos <- sel$datos

  ## 2. Variable cuantitativa (se escribe su nombre) ----------------------
  var_y <- .pedir_variable(datos, "cuantitativa",
    "\u00bfDe qu\u00e9 variable quieres comprobar la normalidad?")
  if (is.null(var_y)) return(.cancelar())

  ## 3. Variable de agrupación (opcional) ---------------------------------
  var_g <- .pedir_variable(datos, "cualitativa",
    "\u00bfQuieres comprobar la normalidad por grupos? Escribe la variable de grupo.",
    opcional = TRUE)
  if (is.null(var_g)) return(.cancelar())
  if (is.na(var_g)) var_g <- NULL

  ## 4. Prueba --------------------------------------------------------------
  pruebas <- data.frame(
    nombre = c("Shapiro-Wilk", "Shapiro-Francia", "Anderson-Darling",
               "Cramer-von Mises", "Lilliefors (Kolmogorov-Smirnov)",
               "Chi cuadrado de Pearson"),
    funcion = c("shapiro.test", "sf.test", "ad.test",
                "cvm.test", "lillie.test", "pearson.test"),
    paquete = c("stats", rep("nortest", 5)),
    n_min = c(3, 5, 8, 8, 5, 5),
    n_max = c(5000, 5000, Inf, Inf, Inf, Inf),
    stringsAsFactors = FALSE
  )
  i <- menu(pruebas$nombre, title = "\n\u00bfQu\u00e9 prueba quieres aplicar?")
  if (i == 0) return(.cancelar())
  prueba <- pruebas[i, ]
  fun_prueba <- getExportedValue(prueba$paquete, prueba$funcion)

  ## Número de clases para chi cuadrado (opcional, como en R Commander)
  args_extra <- list()
  if (prueba$funcion == "pearson.test") {
    resp <- trimws(readline(paste0(
      "\nN\u00famero de clases para la prueba chi cuadrado ",
      "(pulsa Intro para usar el valor por defecto): ")))
    if (nzchar(resp)) {
      k <- suppressWarnings(as.integer(resp))
      if (is.na(k) || k < 4)
        stop("El n\u00famero de clases debe ser un entero >= 4 ",
             "(con menos no quedan grados de libertad).", call. = FALSE)
      args_extra$n.classes <- k
    }
  }

  ## 5. Preparación de los datos -----------------------------------------
  y <- datos[[var_y]]
  if (is.null(var_g)) {
    grupos <- list(Total = y[!is.na(y)])
  } else {
    g <- datos[[var_g]]
    completos <- !is.na(y) & !is.na(g)
    g <- droplevels(factor(g[completos]))
    grupos <- split(y[completos], g)
    n_excl <- sum(!is.na(y) & is.na(datos[[var_g]]))
    if (n_excl > 0)
      cat("\nSe excluyen ", n_excl, " observaciones con '", var_g,
          "' perdido.\n", sep = "")
  }
  n_na <- sum(is.na(y))
  if (n_na > 0)
    cat("Se excluyen ", n_na, " valores perdidos de '", var_y, "'.\n", sep = "")

  ## 6. Cálculo por grupo -------------------------------------------------
  res <- data.frame(grupo = names(grupos),
                    n = vapply(grupos, length, integer(1)),
                    estadistico = NA_real_, nombre_est = NA_character_,
                    p = NA_real_, aviso = NA_character_,
                    stringsAsFactors = FALSE, row.names = NULL)

  for (k in seq_along(grupos)) {
    x <- grupos[[k]]
    n <- length(x)
    if (n < prueba$n_min || n > prueba$n_max) {
      requisito <- if (is.finite(prueba$n_max))
        paste0("entre ", prueba$n_min, " y ", prueba$n_max, " observaciones")
      else paste0("al menos ", prueba$n_min, " observaciones")
      res$aviso[k] <- paste0("No se puede aplicar ", prueba$nombre,
                             ": requiere ", requisito, " (n = ", n, ").")
      next
    }
    r <- tryCatch(do.call(fun_prueba, c(list(x = x), args_extra)),
                  error = function(e) e)
    if (inherits(r, "error")) {
      res$aviso[k] <- paste("Error al aplicar la prueba:", conditionMessage(r))
      next
    }
    res$estadistico[k] <- unname(r$statistic)
    res$nombre_est[k] <- names(r$statistic)
    res$p[k] <- r$p.value
  }

  ## Ajuste por comparaciones múltiples (Holm), como normalityTest().
  ## p.adjust ignora los NA al contar el número de contrastes.
  hay_grupos <- !is.null(var_g)
  res$p_ajustado <- if (hay_grupos) p.adjust(res$p, method = "holm") else res$p
  p_decision <- res$p_ajustado
  res$rechaza_H0 <- p_decision < alpha

  ## 7. Impresión ---------------------------------------------------------
  linea <- paste(rep("-", 70), collapse = "")
  cat("\n", linea, "\n", sep = "")
  cat("Prueba de normalidad de ", prueba$nombre, "\n", sep = "")
  cat("Variable: ", var_y, "   Base de datos: ", nombre_df, "\n", sep = "")
  if (hay_grupos) {
    cat("Grupos: ", var_g, " (", nrow(res), " niveles)\n", sep = "")
    cat("Valores p ajustados por el m\u00e9todo de Holm (", nrow(res),
        " contrastes)\n", sep = "")
  }
  cat("Nivel de significaci\u00f3n: alfa = ", alpha, "\n", sep = "")

  for (k in seq_len(nrow(res))) {
    etiqueta <- if (hay_grupos) paste0("en el grupo ", var_g, " = ", res$grupo[k])
                else "en la muestra"
    cat("\n", linea, "\n", sep = "")
    if (hay_grupos) cat("Grupo: ", var_g, " = ", res$grupo[k], sep = "")
    else cat("Muestra completa")
    cat("   (n = ", res$n[k], ")\n\n", sep = "")

    .imprimir(paste0("H0: la variable '", var_y, "' ", etiqueta,
                    " sigue una distribuci\u00f3n normal."))
    .imprimir(paste0("H1: la variable '", var_y, "' ", etiqueta,
                    " no sigue una distribuci\u00f3n normal."))
    cat("\n")

    if (!is.na(res$aviso[k])) {
      .imprimir(paste("AVISO:", res$aviso[k]))
      next
    }

    cat("  Estad\u00edstico: ", res$nombre_est[k], " = ",
        formatC(res$estadistico[k], format = "f", digits = 4), "\n", sep = "")
    if (hay_grupos) {
      cat("  Valor p sin ajustar: ", .fmt_p(res$p[k]), "\n", sep = "")
      cat("  Valor p ajustado (Holm): ", .fmt_p(res$p_ajustado[k]), "\n", sep = "")
    } else {
      cat("  Valor p: ", .fmt_p(res$p[k]), "\n", sep = "")
    }
    etiqueta_p <- if (hay_grupos) "p ajustado" else "p"

    if (res$rechaza_H0[k]) {
      .imprimir(paste0(
        "Decisi\u00f3n: se rechaza H0 (", etiqueta_p,
        .comparar_p(p_decision[k]), " < ", alpha, "). Hay evidencia de que la ",
        "distribuci\u00f3n de '", var_y, "' ", etiqueta,
        " se aleja de la normal."))
    } else {
      .imprimir(paste0(
        "Decisi\u00f3n: no se rechaza H0 (", etiqueta_p,
        .comparar_p(p_decision[k]), " >= ", alpha, "). No hay evidencia suficiente ",
        "para afirmar que la distribuci\u00f3n de '", var_y, "' ", etiqueta,
        " se aleje de la normal."))
    }
  }

  ## Tabla resumen
  cat("\n", linea, "\nResumen\n", sep = "")
  tabla <- data.frame(
    n = res$n,
    Estadistico = ifelse(is.na(res$estadistico), "-",
                         formatC(res$estadistico, format = "f", digits = 4)),
    p = vapply(res$p, .fmt_p, character(1)),
    row.names = res$grupo, check.names = FALSE
  )
  if (hay_grupos) {
    names(tabla)[3] <- "p sin ajustar"
    tabla[["p ajustado (Holm)"]] <- vapply(res$p_ajustado, .fmt_p, character(1))
  }
  tabla$Decision <- ifelse(is.na(res$rechaza_H0), "No aplicable",
                    ifelse(res$rechaza_H0, "Se rechaza H0", "No se rechaza H0"))
  print(tabla)

  cat("\n")
  .imprimir(paste0(
    "Recuerda: no rechazar H0 no demuestra que la distribuci\u00f3n sea normal; ",
    "con muestras peque\u00f1as estas pruebas tienen poca potencia y con muestras ",
    "grandes detectan desviaciones m\u00ednimas sin relevancia pr\u00e1ctica. ",
    "Compl\u00e9talas siempre con un histograma y un gr\u00e1fico Q-Q."), sangria = 0)

  ## Orden equivalente en R Commander, para que el alumno la reconozca
  extra <- if (length(args_extra)) paste0(", n.classes=", args_extra$n.classes) else ""
  orden <- if (hay_grupos)
    paste0("normalityTest(", var_y, " ~ ", var_g, ", test=\"", prueba$funcion,
           "\", data=", nombre_df, extra, ")")
  else
    paste0("normalityTest(~", var_y, ", test=\"", prueba$funcion,
           "\", data=", nombre_df, extra, ")")
  cat("\nOrden equivalente en R Commander (paquete RcmdrMisc):\n  ", orden,
      "\n", linea, "\n", sep = "")

  res$aviso <- NULL
  invisible(res)
}
