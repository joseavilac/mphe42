#' Prueba de Levene interactiva
#'
#' Versión interactiva de \code{car::leveneTest()}, la función que usa
#' R Commander en Estadísticos > Varianzas > Test de Levene. Pregunta por
#' consola el data.frame, la variable cuantitativa, la variable de agrupación
#' y el centro (mediana o media), y presenta la varianza de cada grupo, las
#' hipótesis, el estadístico F con sus grados de libertad, el valor p y la
#' decisión.
#'
#' @param alpha Nivel de significación para la decisión. Por defecto 0.05.
#' @return De forma invisible, una lista con la tabla de varianzas por grupo
#'   (\code{descriptivos}) y el resultado de la prueba (\code{prueba}).
#' @export
#' @importFrom car leveneTest
#' @importFrom stats var sd median
#' @importFrom utils menu
varianza <- function(alpha = 0.05) {

  if (!interactive())
    stop("varianza() es interactiva: ejec\u00fatala desde la consola de R.",
         call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1 || alpha <= 0 || alpha >= 1)
    stop("'alpha' debe ser un n\u00famero entre 0 y 1.", call. = FALSE)

  ## 1. Base de datos (por menú) ----------------------------------------
  sel <- .elegir_df()
  if (is.null(sel)) return(.cancelar())
  nombre_df <- sel$nombre
  datos <- sel$datos

  ## 2. Variable cuantitativa (se escribe su nombre) ----------------------
  var_y <- .pedir_variable(datos, "cuantitativa",
    "\u00bfQu\u00e9 variable cuantitativa quieres comparar entre grupos?")
  if (is.null(var_y)) return(.cancelar())

  ## 3. Variable de agrupación (se escribe su nombre) ---------------------
  var_g <- .pedir_variable(datos, "cualitativa",
    "\u00bfQu\u00e9 variable define los grupos?")
  if (is.null(var_g)) return(.cancelar())

  ## 4. Centro ------------------------------------------------------------
  i <- menu(c("Mediana (opci\u00f3n por defecto en R Commander; recomendada)",
              "Media (prueba de Levene original)"),
            title = "\n\u00bfRespecto a qu\u00e9 centro se calculan las desviaciones?")
  if (i == 0) return(.cancelar())
  centro_txt <- c("median", "mean")[i]
  centro_fun <- if (i == 1) median else mean

  ## 5. Preparación de los datos -----------------------------------------
  y <- datos[[var_y]]
  g <- datos[[var_g]]
  completos <- !is.na(y) & !is.na(g)
  n_excl <- sum(!completos)
  y <- y[completos]
  g <- droplevels(factor(g[completos]))

  if (n_excl > 0)
    cat("\nSe excluye", if (n_excl == 1) " 1 observaci\u00f3n" else
          paste0("n ", n_excl, " observaciones"), " con '", var_y, "' o '",
        var_g, "' perdido.\n", sep = "")
  if (nlevels(g) < 2)
    stop("'", var_g, "' tiene menos de dos grupos con datos: ",
         "no hay varianzas que comparar.", call. = FALSE)

  ## 6. Descriptivos y prueba ---------------------------------------------
  descriptivos <- data.frame(
    grupo = levels(g),
    n = as.vector(tapply(y, g, length)),
    media = as.vector(tapply(y, g, mean)),
    varianza = as.vector(tapply(y, g, var)),
    desv_tipica = as.vector(tapply(y, g, sd)),
    stringsAsFactors = FALSE
  )

  lev <- tryCatch(leveneTest(y ~ g, center = centro_fun),
                  error = function(e) e)
  if (inherits(lev, "error"))
    stop("No se ha podido aplicar la prueba de Levene: ",
         conditionMessage(lev), call. = FALSE)

  F_val <- lev[1, "F value"]
  gl1 <- lev[1, "Df"]
  gl2 <- lev[2, "Df"]
  p <- lev[1, "Pr(>F)"]
  rechaza <- p < alpha

  ## 7. Impresión ---------------------------------------------------------
  linea <- paste(rep("-", 70), collapse = "")
  cat("\n", linea, "\n", sep = "")
  cat("Prueba de Levene de homogeneidad de varianzas\n")
  cat("Variable: ", var_y, "   Grupos: ", var_g, " (", nlevels(g),
      " niveles)   Base de datos: ", nombre_df, "\n", sep = "")
  cat("Centro: ", if (centro_txt == "median") "mediana (variante de Brown-Forsythe)" else "media",
      "\n", sep = "")
  cat("Nivel de significaci\u00f3n: alfa = ", alpha, "\n", sep = "")

  cat("\n", linea, "\nVarianza de '", var_y, "' en cada grupo de '", var_g,
      "'\n\n", sep = "")
  tabla_desc <- data.frame(
    n = descriptivos$n,
    Media = .fmt_num(descriptivos$media),
    Varianza = .fmt_num(descriptivos$varianza),
    "Desv. t\u00edpica" = .fmt_num(descriptivos$desv_tipica),
    row.names = descriptivos$grupo, check.names = FALSE
  )
  print(tabla_desc)
  if (any(descriptivos$n < 2)) {
    cat("\n")
    .imprimir(paste0("AVISO: alg\u00fan grupo tiene una sola observaci\u00f3n; su ",
                    "varianza no puede calcularse (NA)."))
  }

  cat("\n", linea, "\nContraste de hip\u00f3tesis\n\n", sep = "")
  .imprimir(paste0("H0: la varianza de '", var_y, "' es igual en todos los ",
                  "grupos de '", var_g, "'."))
  .imprimir(paste0("H1: la varianza de '", var_y, "' es distinta en al menos ",
                  "uno de los grupos de '", var_g, "'."))
  cat("\n")
  cat("  Estad\u00edstico: F(", gl1, ", ", gl2, ") = ", .fmt_num(F_val), "\n", sep = "")
  cat("  Valor p: ", .fmt_p(p), "\n", sep = "")

  if (rechaza) {
    .imprimir(paste0(
      "Decisi\u00f3n: se rechaza H0 (p", .comparar_p(p), " < ", alpha, "). Hay ",
      "evidencia de que la varianza de '", var_y, "' no es la misma en todos ",
      "los grupos de '", var_g, "': no se puede asumir homogeneidad de ",
      "varianzas."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No hay ",
      "evidencia suficiente para afirmar que la varianza de '", var_y,
      "' difiera entre los grupos de '", var_g, "': se asume homogeneidad ",
      "de varianzas."))
  }

  cat("\n")
  .imprimir(paste0(
    "Recuerda: no rechazar H0 no demuestra que las varianzas sean iguales; ",
    "con grupos peque\u00f1os la prueba tiene poca potencia. Compara tambi\u00e9n las ",
    "varianzas de la tabla y los diagramas de caja de cada grupo."),
    sangria = 0)

  ## Órdenes equivalentes en R Commander
  cat("\n\u00d3rdenes equivalentes en R Commander:\n")
  cat("  with(", nombre_df, ", tapply(", var_y, ", ", var_g,
      ", var, na.rm=TRUE))\n", sep = "")
  cat("  leveneTest(", var_y, " ~ ", var_g, ", data=", nombre_df,
      ", center=\"", centro_txt, "\")\n", sep = "")
  cat(linea, "\n", sep = "")

  invisible(list(
    descriptivos = descriptivos,
    prueba = data.frame(estadistico_F = F_val, gl1 = gl1, gl2 = gl2,
                        p = p, centro = centro_txt, rechaza_H0 = rechaza)
  ))
}
