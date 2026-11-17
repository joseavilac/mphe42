#' Prueba de Kruskal-Wallis
#'
#' Versión interactiva y comentada del test de Kruskal-Wallis de R Commander
#' (Estadísticos > Test no paramétricos > Test de Kruskal-Wallis), basada en
#' \code{kruskal.test()}. Pregunta por consola el data.frame, la variable
#' cuantitativa y la variable de grupo (con tres o más categorías), y
#' presenta un informe completo: medianas, rangos intercuartílicos y rangos
#' medios por grupo, condiciones de aplicación (con la comprobación de la
#' normalidad por grupo, ajustada por Holm, para valorar si el ANOVA era
#' aplicable), hipótesis, cálculo del estadístico H, valor p, decisión,
#' comparaciones dos a dos con la prueba de Wilcoxon y corrección de
#' Bonferroni, y una frase de redacción de resultados.
#'
#' @param alpha Nivel de significación. Por defecto 0.05.
#' @return De forma invisible, una lista con los descriptivos por grupo, el
#'   resultado de \code{kruskal.test()}, la comprobación de la normalidad,
#'   las comparaciones dos a dos y \code{rechaza_H0}.
#' @export
#' @importFrom stats kruskal.test wilcox.test median quantile qchisq
#' @importFrom utils menu combn
np_kw <- function(alpha = 0.05) {

  if (!interactive())
    stop("np_kw() es interactiva: ejec\u00fatala desde la consola de R.",
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
    "\u00bfQu\u00e9 variable cuantitativa (u ordinal codificada con n\u00fameros) quieres comparar entre los grupos?")
  if (is.null(var_y)) return(.cancelar())

  ## 3. Variable de grupo (3 o más categorías) ---------------------------
  grp <- .pedir_grupos(datos, var_y,
    "\u00bfQu\u00e9 variable define los grupos?", min_grupos = 3,
    aviso_pocos = paste0(" Para comparar dos grupos se usa la prueba de ",
                         "Wilcoxon / Mann-Whitney (np_mw())."))
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
  if (length(unique(y)) == 1)
    stop("Todos los valores de '", var_y, "' son iguales: la prueba no ",
         "puede calcularse.", call. = FALSE)
  niveles_vacios <- if (is.factor(g_todo))
    setdiff(levels(g_todo), niveles) else character(0)

  ## 5. Normalidad (para valorar si el ANOVA era aplicable) ---------------
  norm <- .shapiro_grupos(por_grupo, alpha)
  n_sw <- sum(!is.na(norm$p))
  anova_aplicable <- all(norm$n >= 30 | (!is.na(norm$p) & !norm$no_normal))
  falla <- norm$no_normal & norm$n < 30

  cat("\nComprobaci\u00f3n de la normalidad en cada grupo (Shapiro-Wilk; p ",
      "ajustados por Holm):\n", sep = "")
  for (j in seq_len(k)) .imprimir(.texto_shapiro(niveles[j], norm$n[j],
                                                 norm$p[j], norm$p_holm[j],
                                                 alpha), sangria = 4)
  if (anova_aplicable) {
    .imprimir(paste0(
      "ATENCI\u00d3N: en todos los grupos se puede asumir normalidad o n >= 30, ",
      "as\u00ed que se cumplen las condiciones del ANOVA, que es preferible ",
      "(tiene m\u00e1s potencia). Considera anova_1factor() (en R Commander: ",
      "Estad\u00edsticos > Medias > ANOVA de un factor). Si contin\u00faas, el informe ",
      "lo advertir\u00e1. Excepci\u00f3n: si la variable es ordinal (p. ej. una ",
      "escala de 1 a 5), la prueba no param\u00e9trica es la adecuada."))
  } else {
    .imprimir(paste0(
      "Alg\u00fan grupo con n < 30 no sigue una distribuci\u00f3n normal: la prueba ",
      "de Kruskal-Wallis es la adecuada."))
  }
  i <- menu(c("S\u00ed, aplicar la prueba de Kruskal-Wallis", "No, cancelar"),
            title = "\n\u00bfQuieres continuar?")
  if (i != 1) return(.cancelar())

  ## 6. Cálculos ------------------------------------------------------------
  rangos <- rank(y)
  r_grupo <- split(rangos, g)
  R_i <- vapply(r_grupo, sum, numeric(1))
  empates <- table(y)
  hay_empates <- any(empates > 1)

  resumen <- function(x, r) {
    q <- quantile(x, c(0.25, 0.5, 0.75), names = FALSE)
    c(n = length(x), mediana = q[2], p25 = q[1], p75 = q[3], min = min(x),
      max = max(x), suma_rangos = sum(r), rango_medio = mean(r))
  }
  desc <- t(vapply(seq_len(k), function(j)
    resumen(por_grupo[[j]], r_grupo[[j]]), numeric(8)))
  rownames(desc) <- niveles

  prueba <- kruskal.test(y ~ g)
  H <- unname(prueba$statistic)
  gl <- unname(prueba$parameter)
  p <- prueba$p.value
  rechaza <- p < alpha
  H_sin <- 12 / (N * (N + 1)) * sum(R_i^2 / n_i) - 3 * (N + 1)
  C <- 1 - sum(empates^3 - empates) / (N^3 - N)
  chi_crit <- qchisq(1 - alpha, gl)

  ## Comparaciones dos a dos: Wilcoxon por pares + Bonferroni, como indica
  ## la guía de indicar() (equivale a pairwise.wilcox.test(..., "bonferroni"))
  pares <- combn(k, 2)
  m <- ncol(pares)
  comp <- data.frame(
    g1 = niveles[pares[1, ]], g2 = niveles[pares[2, ]],
    W = NA_real_, p = NA_real_, stringsAsFactors = FALSE)
  for (j in seq_len(m)) {
    a <- por_grupo[[pares[1, j]]]
    b <- por_grupo[[pares[2, j]]]
    w <- suppressWarnings(wilcox.test(a, b))
    comp$W[j] <- unname(w$statistic)
    comp$p[j] <- w$p.value
  }
  comp$p_bonf <- pmin(1, comp$p * m)
  comp$dif_med <- desc[comp$g1, "mediana"] - desc[comp$g2, "mediana"]
  comp$sig <- comp$p_bonf < alpha
  nom_par <- paste0(comp$g1, " - ", comp$g2)

  ## Formato
  f3 <- function(x) .fmt_num(x, 3)
  f1 <- function(x) .fmt_num(x, 1)
  linea <- paste(rep("-", 70), collapse = "")
  seccion <- function(titulo) cat("\n", linea, "\n", titulo, "\n\n", sep = "")
  me <- paste0("Me[", niveles, "]")

  ## 7. Informe -------------------------------------------------------------
  cat("\n", linea, "\n", sep = "")
  cat("Prueba de Kruskal-Wallis\n")
  cat("Variable: ", var_y, "   Base de datos: ", nombre_df, "\n", sep = "")
  .imprimir(paste0("Factor: ", var_g, " (", k, " grupos: ",
                   paste(niveles, collapse = ", "), ")"), sangria = 0)
  cat("Nivel de significaci\u00f3n: alfa = ", alpha, "\n", sep = "")
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
  tabla_desc <- data.frame(
    n = desc[, "n"], Mediana = f3(desc[, "mediana"]),
    RIC = paste0(f3(desc[, "p25"]), " - ", f3(desc[, "p75"])),
    MinMax = paste0(f3(desc[, "min"]), " - ", f3(desc[, "max"])),
    RangoMedio = f3(desc[, "rango_medio"]), stringsAsFactors = FALSE)
  names(tabla_desc) <- c("n", "Mediana", "P25 - P75", "M\u00edn - M\u00e1x",
                         "Rango medio")
  rownames(tabla_desc) <- niveles
  print(tabla_desc)
  cat("\n")
  i_max <- which.max(desc[, "rango_medio"])
  i_min <- which.min(desc[, "rango_medio"])
  .imprimir(paste0(
    "El rango medio m\u00e1s alto es el de '", niveles[i_max], "' (mediana = ",
    f3(desc[i_max, "mediana"]), ") y el m\u00e1s bajo el de '", niveles[i_min],
    "' (mediana = ", f3(desc[i_min, "mediana"]), ")."))
  .imprimir(paste0(
    "La prueba no usa los valores originales sino sus rangos: se ordenan ",
    "las ", N, " observaciones de menor a mayor (1 = la menor) y se compara ",
    "el rango medio de cada grupo. Si H0 es cierta, todos los rangos medios ",
    "deber\u00edan estar cerca de (N + 1) / 2 = ", f1((N + 1) / 2), ". A los ",
    "valores repetidos se les asigna el promedio de sus rangos. Describe los ",
    "grupos con mediana y rango intercuart\u00edlico, no con media y desviaci\u00f3n ",
    "t\u00edpica."))

  ## 7.2 Condiciones de aplicación
  seccion("2. Condiciones de aplicaci\u00f3n")
  .imprimir(paste0(
    "a) Independencia: cada grupo debe estar formado por individuos ",
    "distintos y sin emparejar (si son las mismas personas medidas varias ",
    "veces, corresponde la prueba de Friedman). No se puede comprobar con ",
    "los datos; depende del dise\u00f1o."))
  cat("\n")
  cat("  b) \u00bfEra aplicable el ANOVA? Normalidad en cada grupo (Shapiro-Wilk):\n")
  for (j in seq_len(k)) .imprimir(.texto_shapiro(niveles[j], norm$n[j],
                                                 norm$p[j], norm$p_holm[j],
                                                 alpha), sangria = 4)
  .imprimir(.nota_holm(n_sw), sangria = 4)
  if (anova_aplicable) {
    .imprimir(paste0(
      "ATENCI\u00d3N: se cumplen las condiciones del ANOVA (normalidad o n >= 30 ",
      "en todos los grupos). El ANOVA tiene m\u00e1s potencia y es el recomendado: ",
      "anova_1factor() o, en R Commander, Estad\u00edsticos > Medias > ANOVA de ",
      "un factor. Kruskal-Wallis sigue siendo v\u00e1lida, pero detecta peor las ",
      "diferencias reales. Si la variable es ordinal (p. ej. una escala de 1 ",
      "a 5), la no param\u00e9trica es la adecuada aunque se cumplan esas ",
      "condiciones."), sangria = 4)
  } else {
    .imprimir(paste0(
      "Hay alg\u00fan grupo con n < 30 en el que no se puede asumir normalidad (",
      paste0("'", niveles[falla], "'", collapse = ", "), "): el ANOVA no es ",
      "fiable y Kruskal-Wallis es la adecuada."), sangria = 4)
  }
  cat("\n")
  .imprimir(paste0(
    "c) Forma de las distribuciones: la prueba no exige normalidad. Si las ",
    "distribuciones de los grupos tienen forma parecida (solo desplazadas), ",
    "el resultado se puede interpretar como una comparaci\u00f3n de medianas; si ",
    "no, indica que los valores de alg\u00fan grupo tienden a ser mayores o ",
    "menores que los de otros. Comp\u00e1ralo con un diagrama de caja por grupos."))
  if (any(n_i < 5)) {
    cat("\n")
    .imprimir(paste0(
      "d) Tama\u00f1o de los grupos: el valor p se obtiene con una aproximaci\u00f3n ",
      "chi-cuadrado, poco precisa si alg\u00fan grupo tiene menos de 5 ",
      "observaciones (", paste0("'", niveles[n_i < 5], "': n = ", n_i[n_i < 5],
                                collapse = "; "),
      "). Interpreta el resultado con cautela."))
  }

  ## 7.3 Hipótesis
  seccion("3. Hip\u00f3tesis")
  .imprimir(paste0("H0: ", paste(me, collapse = " = "),
                   "   (los valores de '", var_y, "' tienen la misma ",
                   "distribuci\u00f3n en los ", k, " grupos)"))
  .imprimir(paste0("H1: al menos un grupo es distinto   (los valores de '",
                   var_y, "' de al menos un grupo tienden a ser mayores o ",
                   "menores que los de los dem\u00e1s)"))
  cat("\n")
  .imprimir(paste0(
    "Es un contraste global (\u00f3mnibus): compara todos los grupos a la vez y ",
    "no tiene versi\u00f3n unilateral. Si se rechaza H0 solo sabremos que alg\u00fan ",
    "grupo es distinto, no cu\u00e1l; para eso est\u00e1n las comparaciones dos a dos ",
    "(secci\u00f3n 6). Expresar H0 con medianas (Me) es correcto si las ",
    "distribuciones tienen forma parecida (apartado 2c)."))

  ## 7.4 Estadístico
  seccion("4. Estad\u00edstico de contraste")
  .imprimir(paste0("Suma de rangos de cada grupo: ",
                   paste0(niveles, " = ", f1(R_i), collapse = "; "), "."))
  .imprimir(paste0("H = 12 / (N x (N + 1)) x \u03a3 (R_i\u00b2 / n_i) - 3 x (N + 1) = ",
                   "12 / (", N, " x ", N + 1, ") x (",
                   paste0(f1(R_i), "\u00b2 / ", n_i, collapse = " + "),
                   ") - 3 x ", N + 1, " = ", f3(H_sin)))
  if (hay_empates) {
    .imprimir(paste0(
      "Correcci\u00f3n por empates: C = 1 - \u03a3 (t\u00b3 - t) / (N\u00b3 - N) = ", .fmt_num(C, 4),
      ", donde t es el n\u00famero de veces que se repite cada valor. H corregido ",
      "= H / C = ", f3(H_sin), " / ", .fmt_num(C, 4), " = ", f3(H),
      " (es el que da R)."))
  }
  cat("  Grados de libertad: gl = k - 1 = ", k, " - 1 = ", gl, "\n", sep = "")
  cat("  Valor cr\u00edtico: chi-cuadrado(", 1 - alpha, "; ", gl, ") = ",
      f3(chi_crit), "\n", sep = "")
  cat("\n")
  .imprimir(paste0(
    "H mide cu\u00e1nto se alejan los rangos medios de los grupos del rango medio ",
    "global. Si H0 es cierta, H sigue aproximadamente una distribuci\u00f3n ",
    "chi-cuadrado con k - 1 grados de libertad (en R Commander aparece como ",
    "'Kruskal-Wallis chi-squared'). Se rechaza H0 si H supera el valor ",
    "cr\u00edtico."))

  ## 7.5 Valor p y decisión
  seccion("5. Valor p y decisi\u00f3n")
  cat("  Valor p: ", .fmt_p(p), "\n\n", sep = "")
  .imprimir(paste0(
    "Interpretaci\u00f3n del valor p: si los valores de '", var_y, "' tuvieran ",
    "la misma distribuci\u00f3n en los ", k, " grupos, la probabilidad de ",
    "obtener un H tan grande como el observado (o mayor) ser\u00eda ", .fmt_p(p),
    "."))
  cat("\n")
  if (rechaza) {
    .imprimir(paste0(
      "Decisi\u00f3n: se rechaza H0 (p", .comparar_p(p), " < ", alpha, "). Hay ",
      "evidencia de que los valores de '", var_y, "' no se distribuyen igual ",
      "en todos los grupos de '", var_g, "'. Para saber qu\u00e9 grupos difieren, ",
      "mira las comparaciones dos a dos (secci\u00f3n 6)."))
  } else {
    .imprimir(paste0(
      "Decisi\u00f3n: no se rechaza H0 (p", .comparar_p(p), " >= ", alpha, "). No ",
      "hay evidencia suficiente para afirmar que los valores de '", var_y,
      "' difieran entre los grupos de '", var_g, "'. Esto no demuestra que ",
      "H0 sea cierta (que las ", k, " distribuciones sean iguales)."))
  }
  cat("\n")
  .imprimir(paste0(
    "El valor p indica si las diferencias son estad\u00edsticamente ",
    "significativas, pero no si son importantes: valora la relevancia con ",
    "las medianas y los rangos intercuart\u00edlicos de cada grupo (secci\u00f3n 1)."))

  ## 7.6 Comparaciones dos a dos
  seccion(paste0("6. Comparaciones dos a dos (Wilcoxon con correcci\u00f3n de ",
                 "Bonferroni)"))
  tab_comp <- data.frame(
    Dif = f3(comp$dif_med), W = f1(comp$W),
    p = vapply(comp$p, .fmt_p, character(1)),
    pb = vapply(comp$p_bonf, .fmt_p, character(1)),
    sig = ifelse(comp$sig, "*", ""), stringsAsFactors = FALSE)
  names(tab_comp) <- c("Dif. medianas", "W", "p sin ajustar",
                       "p Bonferroni", "")
  rownames(tab_comp) <- nom_par
  print(tab_comp)
  cat("  * p Bonferroni < ", alpha, "\n\n", sep = "")
  .imprimir(paste0(
    "Se compara cada par de grupos con la prueba de Wilcoxon (Mann-Whitney) ",
    "y se corrige el valor p por el n\u00famero de comparaciones (", m, "): p ",
    "Bonferroni = p x ", m, " (como m\u00e1ximo 1). La diferencia de medianas es ",
    "la del primer grupo nombrado menos la del segundo. Sin correcci\u00f3n, al ",
    "hacer muchas comparaciones aumentar\u00eda la probabilidad de encontrar ",
    "alguna diferencia por azar."))
  cat("\n")
  if (any(comp$sig)) {
    .imprimir(paste0("Pares con diferencia significativa: ",
                     paste(nom_par[comp$sig], collapse = "; "), "."))
  } else {
    .imprimir("Ning\u00fan par de grupos difiere significativamente.")
  }
  if (!rechaza) {
    cat("\n")
    .imprimir(paste0(
      "ATENCI\u00d3N: el contraste global no es significativo, as\u00ed que las ",
      "comparaciones dos a dos no deben interpretarse",
      if (any(comp$sig)) " (aunque alg\u00fan par salga significativo)" else "",
      "."))
  } else if (!any(comp$sig)) {
    cat("\n")
    .imprimir(paste0(
      "Nota: el contraste global es significativo pero ning\u00fan par lo es ",
      "tras la correcci\u00f3n. Puede ocurrir en casos l\u00edmite: Bonferroni es ",
      "conservadora y cada comparaci\u00f3n usa solo los datos de dos grupos."))
  }

  ## 7.7 Redacción
  seccion("7. C\u00f3mo redactar el resultado")
  desc_g <- paste0(niveles, ": mediana = ", f3(desc[, "mediana"]), ", RIC = ",
                   f3(desc[, "p25"]), " - ", f3(desc[, "p75"]), ", n = ", n_i,
                   collapse = "; ")
  estad <- paste0("prueba de Kruskal-Wallis: H(", gl, ") = ", f3(H), "; ",
                  .p_redaccion(p))
  frase <- if (rechaza) {
    paste0("Los valores de ", var_y, " difirieron significativamente entre ",
           "los grupos de ", var_g, " (", desc_g, ") (", estad, ").")
  } else {
    paste0("No se observaron diferencias significativas en ", var_y,
           " entre los grupos de ", var_g, " (", desc_g, ") (", estad, ").")
  }
  if (rechaza) {
    frase <- paste0(frase, if (any(comp$sig)) {
      paste0(" En las comparaciones dos a dos (Wilcoxon con correcci\u00f3n de ",
             "Bonferroni), ",
             if (sum(comp$sig) == 1) "difirieron significativamente "
             else "difirieron significativamente los pares ",
             paste0(nom_par[comp$sig], " (p ajustado ",
                    ifelse(comp$p_bonf[comp$sig] < 1e-4, "< 0.0001",
                           paste0("= ", vapply(comp$p_bonf[comp$sig], .fmt_p,
                                               character(1)))), ")",
                    collapse = "; "), ".")
    } else {
      paste0(" Ninguna de las comparaciones dos a dos (Wilcoxon con ",
             "correcci\u00f3n de Bonferroni) fue significativa.")
    })
  }
  .imprimir(frase)

  ## 7.8 Órdenes equivalentes
  cat("\n", linea, "\n", sep = "")
  cat("En R Commander: Estad\u00edsticos > Test no param\u00e9tricos > Test de Kruskal-Wallis...\n")
  cat("\u00d3rdenes equivalentes:\n")
  cat("  Tapply(", var_y, " ~ ", var_g, ", median, na.action=na.omit, data=",
      nombre_df, ") # medians by group\n", sep = "")
  cat("  kruskal.test(", var_y, " ~ ", var_g, ", data=", nombre_df, ")\n",
      sep = "")
  cat("  # Comparaciones dos a dos (no lo genera R Commander):\n")
  cat("  pairwise.wilcox.test(", nombre_df, "$", var_y, ", ", nombre_df, "$",
      var_g, ", p.adjust.method=\"bonferroni\")\n", sep = "")
  cat("\n")
  .imprimir(paste0(
    "R Commander no hace las comparaciones dos a dos. Para obtenerlas con ",
    "los men\u00fas: filtra la base para quedarte con cada par de grupos ",
    "(Datos > Conjunto de datos activo > Filtrar el conjunto de datos ",
    "activo...), aplica a cada par el test de Wilcoxon para dos muestras y ",
    "multiplica cada valor p por ", m, "."), sangria = 0)
  if (!is.factor(g_todo))
    .imprimir(paste0(
      "'", var_g, "' no es un factor en '", nombre_df, "'. Para que ",
      "aparezca en 'Grupos' en R Commander, convi\u00e9rtela antes (Datos > ",
      "Modificar variables del conjunto de datos activo > Convertir variable ",
      "num\u00e9rica en factor...)."), sangria = 0)
  cat(linea, "\n", sep = "")

  comparaciones <- data.frame(dif_medianas = comp$dif_med, W = comp$W,
                              p = comp$p, p_bonferroni = comp$p_bonf,
                              row.names = nom_par)
  invisible(list(
    descriptivos = desc,
    prueba = prueba,
    normalidad = norm,
    comparaciones = comparaciones,
    rechaza_H0 = rechaza
  ))
}
