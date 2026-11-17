#' ANOVA de un factor interactivo
#'
#' Versión interactiva y comentada del ANOVA de un factor de R Commander
#' (Estadísticos > Medias > ANOVA de un factor), basada en \code{aov()},
#' \code{oneway.test()} y \code{multcomp::glht()}. Pregunta por consola el
#' data.frame, la variable cuantitativa y la variable de grupo (con tres o
#' más categorías), comprueba la normalidad en cada grupo (Shapiro-Wilk, con
#' los valores p ajustados por el método de Holm, como
#' \code{normalityTest()}) y la homogeneidad
#' de varianzas (Levene), recomienda el ANOVA clásico o el F de Welch y
#' presenta un informe completo: descriptivos por grupo, supuestos,
#' hipótesis, tabla ANOVA con el cálculo del estadístico F, decisión,
#' intervalos de confianza de cada media, comparaciones dos a dos de Tukey,
#' tamaño del efecto (eta cuadrado) y una frase de redacción de resultados.
#'
#' @param alpha Nivel de significación. El nivel de confianza de los
#'   intervalos es 1 - alpha. Por defecto 0.05.
#' @return De forma invisible, una lista con los descriptivos por grupo, el
#'   modelo \code{aov()}, la tabla ANOVA, el resultado de
#'   \code{oneway.test()} (Welch), la prueba de Levene, las comparaciones de
#'   Tukey, el método aplicado, eta cuadrado y \code{rechaza_H0}.
#' @export
#' @importFrom stats aov oneway.test confint sd median quantile qt qf
#' @importFrom grDevices boxplot.stats
#' @importFrom car leveneTest
#' @importFrom multcomp glht mcp
#' @importFrom utils menu
anova_1factor <- function(alpha = 0.05) {

  if (!interactive())
    stop("anova_1factor() es interactiva: ejec\u00fatala desde la consola de R.",
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
    "\u00bfQu\u00e9 variable cuantitativa quieres comparar entre los grupos?")
  if (is.null(var_y)) return(.cancelar())

  ## 3. Variable de grupo (3 o más categorías) ---------------------------
  grp <- .pedir_grupos(datos, var_y,
    "\u00bfQu\u00e9 variable define los grupos?", min_grupos = 3,
    aviso_pocos = paste0(" Para comparar dos medias se usa la prueba t para ",
                         "dos muestras independientes (media_2muestra())."))
  if (is.null(grp)) return(.cancelar())
  var_g <- grp$var_g
  niveles <- grp$niveles
  k <- length(niveles)

  ## 4. Datos ---------------------------------------------------------------
  y_todo <- datos[[var_y]]
  g_todo <- datos[[var_g]]
  completos <- !is.na(y_todo) & !is.na(g_todo)
  n_excl <- sum(!completos)
  y <- y_todo[completos]
  g <- factor(g_todo[completos], levels = niveles)
  por_grupo <- split(y, g)
  n_i <- vapply(por_grupo, length, numeric(1))
  N <- sum(n_i)
  if (any(n_i < 2))
    stop("Cada grupo necesita al menos 2 observaciones (",
         paste0(niveles[n_i < 2], ": n = ", n_i[n_i < 2], collapse = "; "),
         ").", call. = FALSE)
  if (all(vapply(por_grupo, sd, numeric(1)) == 0))
    stop("Los valores de '", var_y, "' son constantes dentro de cada grupo: ",
         "el ANOVA no puede calcularse.", call. = FALSE)
  ## Categorías del factor sin ningún dato válido (no entran en el análisis)
  niveles_vacios <- if (is.factor(g_todo))
    setdiff(levels(g_todo), niveles) else character(0)

  ## 5. Comprobación de supuestos y elección del método ------------------
  ## Shapiro-Wilk por grupo con ajuste de Holm, como normalityTest() de
  ## Rcmdr y normalidad()
  norm <- .shapiro_grupos(por_grupo, alpha)
  sw_p <- norm$p
  sw_adj <- norm$p_holm
  n_sw <- sum(!is.na(sw_p))
  no_normal <- norm$no_normal
  pequeno <- n_i < 30
  falla_normal <- any(no_normal & pequeno)

  lev <- leveneTest(y ~ g, center = median)
  lev_F <- lev[1, "F value"]
  lev_gl1 <- lev[1, "Df"]
  lev_gl2 <- lev[2, "Df"]
  lev_p <- lev[1, "Pr(>F)"]
  var_distintas <- lev_p < alpha

  texto_sw <- function(nom, pv, pa, n) .texto_shapiro(nom, n, pv, pa, alpha)
  texto_lev <- paste0("F(", lev_gl1, ", ", lev_gl2, ") = ",
                      .fmt_num(lev_F, 3), "; p", .comparar_p(lev_p),
                      if (var_distintas) paste0(" < ", alpha,
                        ": se rechaza la igualdad de varianzas.")
                      else paste0(" >= ", alpha,
                        ": no hay evidencia de varianzas distintas."))

  cat("\nComprobaci\u00f3n de la normalidad en cada grupo (Shapiro-Wilk; p ",
      "ajustados por Holm):\n", sep = "")
  for (j in seq_len(k)) .imprimir(texto_sw(niveles[j], sw_p[j], sw_adj[j],
                                           n_i[j]), sangria = 4)
  cat("Comprobaci\u00f3n de la homogeneidad de varianzas (Levene, centrado en ",
      "la mediana):\n", sep = "")
  .imprimir(texto_lev, sangria = 4)
  if (falla_normal) {
    .imprimir(paste0(
      "ATENCI\u00d3N: hay alg\u00fan grupo con n < 30 en el que no se puede asumir ",
      "normalidad. El ANOVA no es fiable: se recomienda la prueba de ",
      "Kruskal-Wallis (en R Commander: Estad\u00edsticos > Test no param\u00e9tricos > ",
      "Test de Kruskal-Wallis). Si contin\u00faas, el informe lo advertir\u00e1."))
  }
  pre <- if (falla_normal) "Si aun as\u00ed contin\u00faas: " else ""
  if (var_distintas) {
    .imprimir(paste0(pre, "Levene rechaza la igualdad de varianzas, as\u00ed ",
                     "que se recomienda el F de Welch."))
  } else {
    .imprimir(paste0(pre, "Levene no rechaza la igualdad de varianzas, as\u00ed ",
                     "que se recomienda el ANOVA cl\u00e1sico."))
  }
  i <- menu(c(paste0("ANOVA cl\u00e1sico (F de Fisher; supone varianzas iguales)",
                     if (!var_distintas) "   <-- RECOMENDADO" else ""),
              paste0("F de Welch (no supone varianzas iguales)",
                     if (var_distintas) "   <-- RECOMENDADO" else "")),
            title = "\n\u00bfQu\u00e9 prueba quieres aplicar?")
  if (i == 0) return(.cancelar())
  metodo <- c("clasico", "welch")[i]
  recomendado <- if (var_distintas) "welch" else "clasico"

  ## 6. Cálculos ------------------------------------------------------------
  resumen <- function(x) {
    q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
    c(n = length(x), media = mean(x), de = sd(x), ee = sd(x) / sqrt(length(x)),
      mediana = q[2], p25 = q[1], p75 = q[3], min = min(x), max = max(x))
  }
  desc <- t(vapply(por_grupo, resumen, numeric(9)))
  desc_total <- resumen(y)
  media_global <- mean(y)

  ## ANOVA clásico (se calcula siempre: da la tabla y eta cuadrado)
  datos_mod <- data.frame(y = y, g = g)
  modelo <- aov(y ~ g, data = datos_mod)
  tabla <- summary(modelo)[[1]]
  gl1 <- tabla[1, "Df"]
  gl2 <- tabla[2, "Df"]
  sc_entre <- tabla[1, "Sum Sq"]
  sc_dentro <- tabla[2, "Sum Sq"]
  sc_total <- sc_entre + sc_dentro
  mc_entre <- tabla[1, "Mean Sq"]
  mc_dentro <- tabla[2, "Mean Sq"]
  F_clas <- tabla[1, "F value"]
  p_clas <- tabla[1, "Pr(>F)"]

  ## F de Welch
  welch <- oneway.test(y ~ g, data = datos_mod, var.equal = FALSE)
  F_welch <- unname(welch$statistic)
  gl1_w <- unname(welch$parameter[1])
  gl2_w <- unname(welch$parameter[2])
  p_welch <- welch$p.value

  if (metodo == "clasico") {
    F_val <- F_clas; g1 <- gl1; g2 <- gl2; p <- p_clas
  } else {
    F_val <- F_welch; g1 <- gl1_w; g2 <- gl2_w; p <- p_welch
  }
  F_crit <- qf(1 - alpha, g1, g2)
  rechaza <- p < alpha
  rechaza_clas <- p_clas < alpha

  ## Intervalo de confianza de la media de cada grupo (t, DE del grupo)
  ic_media <- t(vapply(por_grupo, function(x) {
    m <- mean(x); ee <- sd(x) / sqrt(length(x))
    m + c(-1, 1) * qt(1 - alpha / 2, length(x) - 1) * ee
  }, numeric(2)))

  ## Comparaciones dos a dos de Tukey (como R Commander: multcomp::glht).
  ## Los valores p ajustados y los intervalos simultáneos se calculan por
  ## integración numérica (mvtnorm), con una pequeña variación aleatoria.
  pares <- glht(modelo, linfct = mcp(g = "Tukey"))
  pares_res <- summary(pares)
  pares_ic <- confint(pares, level = conf)
  tk_est <- unname(pares_res$test$coefficients)
  tk_ee <- unname(pares_res$test$sigma)
  tk_t <- unname(pares_res$test$tstat)
  tk_p <- as.numeric(pares_res$test$pvalues)
  tk_li <- unname(pares_ic$confint[, "lwr"])
  tk_ls <- unname(pares_ic$confint[, "upr"])
  tk_nom <- names(pares_res$test$coefficients)
  tk_sig <- tk_p < alpha

  ## Tamaño del efecto
  eta2 <- sc_entre / sc_total

  atipicos <- lapply(por_grupo, function(x) boxplot.stats(x)$out)
  razon_var <- max(desc[, "de"])^2 / min(desc[, "de"])^2

  ## Formato
  f3 <- function(x) .fmt_num(x, 3)
  fgl <- function(x) if (abs(x - round(x)) < 1e-8) format(round(x))
                     else formatC(x, format = "f", digits = 2)
  linea <- paste(rep("-", 70), collapse = "")
  seccion <- function(titulo) cat("\n", linea, "\n", titulo, "\n\n", sep = "")
  nombre_prueba <- if (metodo == "clasico") "ANOVA cl\u00e1sico (F de Fisher)"
                   else "F de Welch (varianzas distintas)"
  mu <- paste0("\u03bc[", niveles, "]")
  i_max <- which.max(desc[, "media"])
  i_min <- which.min(desc[, "media"])

  ## 7. Informe -------------------------------------------------------------
  cat("\n", linea, "\n", sep = "")
  cat("ANOVA de un factor\n")
  cat("Variable: ", var_y, "   Base de datos: ", nombre_df, "\n", sep = "")
  .imprimir(paste0("Factor: ", var_g, " (", k, " grupos: ",
                   paste(niveles, collapse = ", "), ")"), sangria = 0)
  cat("Prueba aplicada: ", nombre_prueba, "   Nivel de significaci\u00f3n: alfa = ",
      alpha, "\n", sep = "")
  if (n_excl > 0)
    cat("Se excluye", if (n_excl == 1) " 1 observaci\u00f3n" else
          paste0("n ", n_excl, " observaciones"), " con '", var_y, "' o '",
        var_g, "' perdido.\n", sep = "")
  if (length(niveles_vacios) > 0)
    .imprimir(paste0(
      if (length(niveles_vacios) == 1) "La categor\u00eda " else "Las categor\u00edas ",
      paste0("'", niveles_vacios, "'", collapse = ", "), " de '", var_g,
      if (length(niveles_vacios) == 1) "' no tiene" else "' no tienen",
      " datos v\u00e1lidos y no entra", if (length(niveles_vacios) > 1) "n" else "",
      " en el an\u00e1lisis."), sangria = 0)

  ## 7.1 Descriptivos
  seccion("1. Descripci\u00f3n de los grupos")
  fila <- function(dd) c(dd["n"], f3(dd["media"]), f3(dd["de"]),
                         f3(dd["mediana"]),
                         paste0(f3(dd["p25"]), " - ", f3(dd["p75"])),
                         paste0(f3(dd["min"]), " - ", f3(dd["max"])))
  tabla_desc <- as.data.frame(rbind(t(apply(desc, 1, fila)), fila(desc_total)),
                              stringsAsFactors = FALSE)
  names(tabla_desc) <- c("n", "Media", "DE", "Mediana", "P25 - P75",
                         "M\u00edn - M\u00e1x")
  rownames(tabla_desc) <- c(niveles, "Total")
  print(tabla_desc)
  cat("\n")
  .imprimir(paste0("Media global (todas las observaciones): ",
                   f3(media_global), "."))
  .imprimir(paste0("La media m\u00e1s alta es la de '", niveles[i_max], "' (",
                   f3(desc[i_max, "media"]), ") y la m\u00e1s baja la de '",
                   niveles[i_min], "' (", f3(desc[i_min, "media"]),
                   "): diferencia de ", f3(desc[i_max, "media"] -
                   desc[i_min, "media"]), " unidades."))

  ## 7.2 Supuestos
  seccion("2. Supuestos de la prueba")
  .imprimir(paste0(
    "a) Independencia: cada grupo debe estar formado por individuos ",
    "distintos y sin emparejar (si son las mismas personas medidas varias ",
    "veces corresponde el ANOVA de medidas repetidas). No se puede ",
    "comprobar con los datos; depende del dise\u00f1o."))
  cat("\n")

  cat("  b) Normalidad en cada grupo (Shapiro-Wilk):\n")
  for (j in seq_len(k)) .imprimir(texto_sw(niveles[j], sw_p[j], sw_adj[j],
                                           n_i[j]), sangria = 4)
  .imprimir(.nota_holm(n_sw), sangria = 4)
  if (falla_normal) {
    .imprimir(paste0(
      "ATENCI\u00d3N: hay alg\u00fan grupo con n < 30 en el que no se puede asumir ",
      "normalidad (", paste0("'", niveles[no_normal & pequeno], "'",
                             collapse = ", "),
      "), as\u00ed que el ANOVA no es fiable. Utiliza la prueba de Kruskal-Wallis ",
      "(en R Commander: Estad\u00edsticos > Test no param\u00e9tricos > Test de ",
      "Kruskal-Wallis)."), sangria = 4)
  } else if (all(!pequeno)) {
    .imprimir(paste0(
      "Con n >= 30 en todos los grupos, el ANOVA es robusto frente a ",
      "desviaciones moderadas de la normalidad (teorema central del l\u00edmite)."),
      sangria = 4)
  } else {
    .imprimir(paste0(
      "Alg\u00fan grupo tiene n < 30: la validez depende de la normalidad y ",
      "Shapiro-Wilk tiene poca potencia con muestras peque\u00f1as. Revisa tambi\u00e9n ",
      "los histogramas y gr\u00e1ficos Q-Q de cada grupo."), sangria = 4)
  }
  cat("\n")

  cat("  c) Homogeneidad de varianzas (Levene, centrado en la mediana):\n")
  .imprimir(paste0("Varianzas: ", paste0(niveles, " = ", f3(desc[, "de"]^2),
                   collapse = "; "), " (la mayor es ",
                   formatC(razon_var, format = "f", digits = 2),
                   " veces la menor)."), sangria = 4)
  .imprimir(texto_lev, sangria = 4)
  motivo <- if (metodo == "clasico") {
    paste0("Se aplica el ANOVA cl\u00e1sico porque as\u00ed lo has elegido.",
           if (var_distintas) paste0(" ATENCI\u00d3N: Levene rechaza la igualdad ",
             "de varianzas; con estos datos ser\u00eda m\u00e1s adecuado el F de Welch ",
             "(en R Commander, marcando 'Test F de Welch que no asume ",
             "varianzas iguales'), y las comparaciones de Tukey, que suponen ",
             "varianzas iguales, deben interpretarse con cautela.")
           else " Levene no rechaza la igualdad de varianzas, que es su supuesto.")
  } else {
    paste0("Se aplica el F de Welch porque as\u00ed lo has elegido.",
           if (var_distintas) " Levene rechaza la igualdad de varianzas, as\u00ed que es la opci\u00f3n adecuada."
           else paste0(" Levene no rechaza la igualdad de varianzas, por lo ",
                       "que el criterio del curso recomienda el ANOVA ",
                       "cl\u00e1sico. El F de Welch sigue siendo v\u00e1lido (no ",
                       "requiere varianzas iguales), pero no es el ",
                       "recomendado."))
  }
  .imprimir(motivo, sangria = 4)
  cat("\n")

  texto_atip <- function(nom, at) {
    if (length(at) == 0) return(paste0("'", nom, "': ninguno."))
    paste0("'", nom, "': ", length(at), " (",
           paste(f3(utils::head(sort(at), 10)), collapse = ", "),
           if (length(at) > 10) ", ..." else "", ").")
  }
  cat("  d) Valores at\u00edpicos (criterio del diagrama de caja, 1.5 x RIC):\n")
  for (j in seq_len(k)) .imprimir(texto_atip(niveles[j], atipicos[[j]]),
                                  sangria = 4)
  if (sum(lengths(atipicos)) > 0)
    .imprimir(paste0("Las medias y las DE son sensibles a los valores ",
                     "at\u00edpicos: comprueba que no son errores de registro."),
              sangria = 4)

  ## 7.3 Hipótesis
  seccion("3. Hip\u00f3tesis")
  .imprimir(paste0("H0: ", paste(mu, collapse = " = "),
                   "   (las medias poblacionales de '", var_y,
                   "' son iguales en los ", k, " grupos)"))
  .imprimir(paste0("H1: al menos una ", "\u03bc", " es distinta",
                   "   (la media poblacional de '", var_y, "' de al menos ",
                   "un grupo es distinta de la de los dem\u00e1s)"))
  cat("\n")
  .imprimir(paste0(
    "El ANOVA es un contraste global (\u00f3mnibus): compara todos los grupos a ",
    "la vez y no tiene versi\u00f3n unilateral. Si se rechaza H0 solo sabremos ",
    "que no todas las medias son iguales, no cu\u00e1les difieren; para eso se ",
    "usan las comparaciones dos a dos (secci\u00f3n 7). H1 no significa que ",
    "todas las medias sean distintas entre s\u00ed."))

  ## 7.4 Estadístico
  seccion("4. Estad\u00edstico de contraste")
  cat("  Tabla ANOVA:\n\n")
  tab_imp <- data.frame(
    gl = c(fgl(gl1), fgl(gl2), fgl(gl1 + gl2)),
    SC = f3(c(sc_entre, sc_dentro, sc_total)),
    MC = c(f3(mc_entre), f3(mc_dentro), ""),
    F = c(f3(F_clas), "", ""),
    p = c(.fmt_p(p_clas), "", ""),
    stringsAsFactors = FALSE)
  names(tab_imp) <- c("gl", "SC", "MC", "F", "Valor p")
  rownames(tab_imp) <- c("Entre grupos", "Dentro de grupos", "Total")
  print(tab_imp)
  cat("\n")
  .imprimir(paste0("SC = suma de cuadrados; MC = media cuadr\u00e1tica. ",
                   "'Entre grupos' corresponde a '", var_g, "' y 'Dentro de ",
                   "grupos' a los residuos ('Residuals' en R)."))
  .imprimir(paste0("SC entre = \u03a3 n_i x (media_i - media global)\u00b2 = ",
                   paste0(n_i, " x (", f3(desc[, "media"]), " - ",
                          f3(media_global), ")\u00b2", collapse = " + "),
                   " = ", f3(sc_entre)))
  .imprimir(paste0("SC dentro = \u03a3 (n_i - 1) x DE_i\u00b2 = ",
                   paste0("(", n_i, " - 1) x ", f3(desc[, "de"]), "\u00b2",
                          collapse = " + "), " = ", f3(sc_dentro)))
  .imprimir(paste0("Grados de libertad: entre = k - 1 = ", k, " - 1 = ",
                   fgl(gl1), "; dentro = N - k = ", N, " - ", k, " = ",
                   fgl(gl2)))
  .imprimir(paste0("MC entre = SC entre / gl entre = ", f3(sc_entre), " / ",
                   fgl(gl1), " = ", f3(mc_entre), ";  MC dentro = ",
                   f3(sc_dentro), " / ", fgl(gl2), " = ", f3(mc_dentro)))
  .imprimir(paste0("F = MC entre / MC dentro = ", f3(mc_entre), " / ",
                   f3(mc_dentro), " = ", f3(F_clas)))
  if (metodo == "welch") {
    cat("\n")
    .imprimir(paste0(
      "F de Welch (prueba aplicada): F(", fgl(gl1_w), ", ", fgl(gl2_w),
      ") = ", f3(F_welch), ". Pondera cada grupo por n_i / DE_i\u00b2, de modo ",
      "que no necesita suponer varianzas iguales; los grados de libertad del ",
      "denominador se ajustan y por eso no son enteros. La tabla anterior ",
      "es la del ANOVA cl\u00e1sico: se muestra porque es la que da R Commander y ",
      "porque de ella sale eta cuadrado, pero la decisi\u00f3n se toma con el F ",
      "de Welch."))
  }
  cat("  Valor cr\u00edtico: F(", 1 - alpha, "; ", fgl(g1), ", ", fgl(g2), ") = ",
      f3(F_crit), "\n", sep = "")
  cat("\n")
  .imprimir(paste0(
    "El estad\u00edstico F compara la variabilidad entre las medias de los grupos ",
    "con la variabilidad de los individuos dentro de cada grupo. Si H0 es ",
    "cierta, F tiende a valer alrededor de 1; cuanto mayor es F, m\u00e1s ",
    "difieren las medias en relaci\u00f3n con la dispersi\u00f3n dentro de los grupos. ",
    "Se rechaza H0 si F supera el valor cr\u00edtico."))

  ## 7.5 Valor p y decisión
  seccion("5. Valor p y decisi\u00f3n")
  cat("  Valor p", if (metodo == "welch") " (Welch)" else "", ": ",
      .fmt_p(p), "\n\n", sep = "")
  .imprimir(paste0(
    "Interpretaci\u00f3n del valor p: si las medias poblacionales de los ", k,
    " grupos fueran iguales, la probabilidad de obtener un F tan grande como ",
    "el observado (o mayor) ser\u00eda ", .fmt_p(p), "."))
  cat("\n")
  if (rechaza) {
    .imprimir(paste0(
      "Decisi\u00f3n: se rechaza H0 (p", .comparar_p(p), " < ", alpha, "). Hay ",
      "evidencia de que la media poblacional de '", var_y, "' no es la misma ",
      "en todos los grupos de '", var_g, "'. Para saber qu\u00e9 grupos difieren, ",
      "mira las comparaciones dos a dos (secci\u00f3n 7)."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No ",
      "hay evidencia suficiente para afirmar que la media poblacional de '",
      var_y, "' difiera entre los grupos de '", var_g, "'. Esto no demuestra ",
      "que H0 sea cierta (que las ", k, " medias sean iguales)."))
  }
  if (metodo == "welch" && rechaza != rechaza_clas) {
    cat("\n")
    .imprimir(paste0(
      "Nota: el ANOVA cl\u00e1sico de la tabla (p", .comparar_p(p_clas), ") ",
      if (rechaza_clas) "s\u00ed" else "no", " rechazar\u00eda H0. Como las varianzas ",
      "se consideran distintas, la decisi\u00f3n v\u00e1lida es la del F de Welch; no ",
      "elijas la prueba seg\u00fan cu\u00e1l da el resultado que esperabas."))
  }

  ## 7.6 Intervalos de confianza de las medias
  seccion(paste0("6. Intervalo de confianza al ", conf_txt,
                 " de la media de cada grupo"))
  tab_ic <- data.frame(
    n = n_i, Media = f3(desc[, "media"]), EE = f3(desc[, "ee"]),
    IC = paste0(f3(ic_media[, 1]), " a ", f3(ic_media[, 2])),
    stringsAsFactors = FALSE)
  names(tab_ic)[4] <- paste0("IC ", conf_txt)
  rownames(tab_ic) <- niveles
  print(tab_ic)
  cat("\n")
  .imprimir(paste0(
    "Cada intervalo se calcula con la t de Student y la DE de su propio ",
    "grupo (como una prueba t para una muestra). Sirven para describir la ",
    "precisi\u00f3n de cada media, no para comparar grupos: que dos intervalos ",
    "se solapen no implica que la diferencia no sea significativa. Para ",
    "comparar, usa la secci\u00f3n 7."))

  ## 7.7 Comparaciones dos a dos (Tukey)
  seccion("7. Comparaciones dos a dos (prueba de Tukey)")
  tab_tk <- data.frame(
    Diferencia = f3(tk_est),
    IC = paste0(f3(tk_li), " a ", f3(tk_ls)),
    t = f3(tk_t),
    p = vapply(tk_p, .fmt_p, character(1)),
    sig = ifelse(tk_sig, "*", ""),
    stringsAsFactors = FALSE)
  names(tab_tk) <- c("Diferencia", paste0("IC ", conf_txt, " simult\u00e1neo"),
                     "t", "p ajustado", "")
  rownames(tab_tk) <- tk_nom
  print(tab_tk)
  cat("  * p ajustado < ", alpha, "\n\n", sep = "")
  .imprimir(paste0(
    "Se comparan los ", length(tk_nom), " pares posibles de grupos. Cada ",
    "diferencia es la media del primer grupo nombrado menos la del segundo ",
    "(p. ej. '", tk_nom[1], "'). Los valores p est\u00e1n ajustados por ",
    "comparaciones m\u00faltiples y el nivel de confianza (", conf_txt, ") se ",
    "aplica al conjunto de los intervalos a la vez: hacer pruebas t por ",
    "separado entre cada par aumentar\u00eda la probabilidad de encontrar alguna ",
    "diferencia por azar."))
  cat("\n")
  if (any(tk_sig)) {
    .imprimir(paste0("Pares con diferencia significativa (el intervalo no ",
                     "incluye el 0): ", paste(tk_nom[tk_sig], collapse = "; "),
                     "."))
  } else {
    .imprimir("Ning\u00fan par de grupos difiere significativamente.")
  }
  if (!rechaza) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: el contraste global no es significativo, as\u00ed que las ",
      "comparaciones dos a dos no deben interpretarse",
      if (any(tk_sig)) paste0(" (aunque alg\u00fan par salga significativo, ",
                              "puede ocurrir en casos l\u00edmite)") else "",
      "."))
  } else if (!any(tk_sig)) {
    cat("\n")
    .imprimir(paste0(
      "Nota: el contraste global es significativo pero ning\u00fan par lo es. ",
      "Puede ocurrir en casos l\u00edmite o cuando la diferencia se reparte entre ",
      "varios grupos (p. ej. un grupo frente a la media de los dem\u00e1s)."))
  }
  if (metodo == "welch" || var_distintas) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: la prueba de Tukey supone varianzas iguales",
      if (var_distintas) ", y Levene las rechaza" else "",
      ". R Commander no ofrece una prueba post hoc espec\u00edfica para ",
      "varianzas distintas: interpreta estas comparaciones con cautela, ",
      "sobre todo si los grupos tienen tama\u00f1os muy diferentes."))
  }

  ## 7.8 Tamaño del efecto
  seccion("8. Tama\u00f1o del efecto")
  mag <- if (eta2 < 0.01) "despreciable" else if (eta2 < 0.06) "peque\u00f1o"
         else if (eta2 < 0.14) "mediano" else "grande"
  .imprimir(paste0("Eta cuadrado (\u03b7\u00b2) = SC entre / SC total = ",
                   f3(sc_entre), " / ", f3(sc_total), " = ", f3(eta2),
                   "  (efecto ", mag, ")"))
  .imprimir(paste0("El ", formatC(100 * eta2, format = "f", digits = 1),
                   " % de la variabilidad de '", var_y, "' en la muestra se ",
                   "explica por las diferencias entre los grupos de '", var_g,
                   "'."))
  cat("\n")
  .imprimir(paste0(
    "El valor p indica si las diferencias son estad\u00edsticamente ",
    "significativas, pero no si son importantes. Juzga la relevancia con las ",
    "diferencias entre medias en unidades reales y sus intervalos (secci\u00f3n ",
    "7). Los umbrales de Cohen para \u03b7\u00b2 (0.01 peque\u00f1o, 0.06 mediano, 0.14 ",
    "grande) son solo orientativos. \u03b7\u00b2 tiende a sobrestimar el efecto en la ",
    "poblaci\u00f3n, sobre todo con muestras peque\u00f1as."))
  if (rechaza && eta2 < 0.01) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: las diferencias son significativas pero el efecto es muy ",
      "peque\u00f1o. Con muestras grandes, diferencias sin relevancia pr\u00e1ctica ",
      "pueden resultar significativas."))
  }
  if (!rechaza && any(pequeno) && eta2 >= 0.06) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: el efecto observado no es peque\u00f1o, pero hay grupos ",
      "reducidos (n = ", paste(n_i, collapse = ", "), "). La falta de ",
      "significaci\u00f3n puede deberse a poca potencia, no a la ausencia de ",
      "diferencias."))
  }

  ## 7.9 Redacción
  seccion("9. C\u00f3mo redactar el resultado")
  desc_g <- paste0(niveles, ": media = ", f3(desc[, "media"]), ", DE = ",
                   f3(desc[, "de"]), ", n = ", n_i, collapse = "; ")
  estad <- paste0(if (metodo == "clasico") "ANOVA de un factor" else
                  "ANOVA de Welch", ": F(", fgl(g1), ", ", fgl(g2), ") = ",
                  f3(F_val), "; ", .p_redaccion(p), "; \u03b7\u00b2 = ", f3(eta2))
  frase <- if (rechaza) {
    paste0("La media de ", var_y, " difiri\u00f3 significativamente entre los ",
           "grupos de ", var_g, " (", desc_g, ") (", estad, ").")
  } else {
    paste0("No se observaron diferencias significativas en la media de ",
           var_y, " entre los grupos de ", var_g, " (", desc_g, ") (",
           estad, ").")
  }
  if (rechaza) {
    frase <- paste0(frase, if (any(tk_sig)) {
      paste0(" En las comparaciones dos a dos (Tukey), ",
             if (sum(tk_sig) == 1) "la diferencia significativa fue "
             else "las diferencias significativas fueron ",
             paste0(tk_nom[tk_sig], " = ", f3(tk_est[tk_sig]), " (IC ",
                    conf_txt, ": ", f3(tk_li[tk_sig]), " a ",
                    f3(tk_ls[tk_sig]), "; p ajustado ",
                    ifelse(tk_p[tk_sig] < 1e-4, "< 0.0001",
                           paste0("= ", vapply(tk_p[tk_sig], .fmt_p,
                                               character(1)))), ")",
                    collapse = "; "), ".")
    } else {
      " Ninguna de las comparaciones dos a dos (Tukey) fue significativa."
    })
  }
  .imprimir(frase)

  ## 7.10 Órdenes equivalentes
  cat("\n", linea, "\n", sep = "")
  .imprimir(paste0("En R Commander: Estad\u00edsticos > Medias > ANOVA de un ",
                   "factor..., marcando 'Comparaciones dos a dos de las ",
                   "medias'", if (metodo == "welch") paste0(" y 'Test F de ",
                   "Welch que no asume varianzas iguales'") else "", "."),
            sangria = 0)
  cat("\u00d3rdenes equivalentes:\n")
  cat("  leveneTest(", var_y, " ~ ", var_g, ", data=", nombre_df,
      ", center=\"median\")\n", sep = "")
  cat("  AnovaModel.1 <- aov(", var_y, " ~ ", var_g, ", data=", nombre_df,
      ")\n", sep = "")
  cat("  summary(AnovaModel.1)\n")
  cat("  with(", nombre_df, ", numSummary(", var_y, ", groups=", var_g,
      ", statistics=c(\"mean\", \"sd\")))\n", sep = "")
  if (metodo == "welch")
    cat("  oneway.test(", var_y, " ~ ", var_g, ", data=", nombre_df,
        ") # Welch test\n", sep = "")
  cat("  local({\n")
  cat("    .Pairs <- glht(AnovaModel.1, linfct = mcp(", var_g,
      " = \"Tukey\"))\n", sep = "")
  cat("    print(summary(.Pairs)) # pairwise tests\n")
  cat("    print(confint(.Pairs", if (conf != 0.95) paste0(", level=", conf)
      else "", ")) # confidence intervals\n", sep = "")
  cat("  })\n")
  cat("\n")
  notas <- character(0)
  if (!is.factor(g_todo))
    notas <- c(notas, paste0(
      "'", var_g, "' no es un factor en '", nombre_df, "'. Para que ",
      "aparezca en 'Grupos' en R Commander, convi\u00e9rtela antes (Datos > ",
      "Modificar variables del conjunto de datos activo > Convertir variable ",
      "num\u00e9rica en factor...)."))
  if (conf != 0.95)
    notas <- c(notas, paste0(
      "El cuadro de di\u00e1logo de R Commander no permite cambiar el nivel de ",
      "confianza: los intervalos de Tukey que muestra son siempre al 95 %. ",
      "Los de este informe (", conf_txt, ") se obtienen con ",
      "confint(.Pairs, level=", conf, ")."))
  notas <- c(notas, paste0(
    "Los valores p ajustados y los intervalos de Tukey se calculan por ",
    "integraci\u00f3n num\u00e9rica: pueden variar ligeramente (3.er-4.o decimal) de ",
    "una ejecuci\u00f3n a otra y respecto a la salida de R Commander."))
  for (nt in notas) .imprimir(nt, sangria = 0)
  cat(linea, "\n", sep = "")

  names(sw_p) <- niveles
  names(sw_adj) <- niveles
  tukey <- data.frame(diferencia = tk_est, ee = tk_ee, t = tk_t,
                      li = tk_li, ls = tk_ls, p_ajustado = tk_p,
                      row.names = tk_nom)
  invisible(list(
    descriptivos = desc,
    modelo = modelo,
    tabla_anova = tabla,
    welch = welch,
    levene = lev,
    metodo = metodo,
    tukey = tukey,
    eta2 = eta2,
    shapiro_p = sw_p,
    shapiro_p_holm = sw_adj,
    rechaza_H0 = rechaza
  ))
}
