# Funciones internas compartidas por las funciones interactivas de pphelp.
# No se exportan.

#' @noRd
.fmt_p <- function(p) {
  if (is.na(p)) return("NA")
  if (p < 1e-4) return("< 0.0001")
  formatC(p, format = "f", digits = 4)
}

#' @noRd
.comparar_p <- function(p) {
  if (p < 1e-4) " < 0.0001" else paste0(" = ", .fmt_p(p))
}

#' @noRd
.fmt_num <- function(x, d = 4) {
  ifelse(is.na(x), "NA", formatC(x, format = "f", digits = d))
}

#' @noRd
.imprimir <- function(texto, sangria = 2) {
  cat(strwrap(texto, width = 78, indent = sangria, exdent = sangria + 2),
      sep = "\n")
}

#' @noRd
.cancelar <- function() {
  cat("\nOperaci\u00f3n cancelada.\n")
  invisible(NULL)
}

#' Clasificación de las columnas de un data.frame
#' @noRd
.es_cuantitativa <- function(v) is.numeric(v)
.es_cualitativa <- function(v) is.factor(v) || is.character(v) || is.logical(v)

#' Selección del data.frame por menú
#'
#' Devuelve una lista con el nombre y el data.frame, o NULL si se cancela.
#' Se usa inherits() en lugar de class(x) == "data.frame" para incluir
#' también tibbles y data.tables.
#' @noRd
.elegir_df <- function() {
  entorno <- globalenv()
  objetos <- ls(envir = entorno)
  es_df <- vapply(objetos,
                  function(x) inherits(get(x, envir = entorno), "data.frame"),
                  logical(1))
  bases <- objetos[es_df]
  if (length(bases) == 0)
    stop("No hay ning\u00fan data.frame en el espacio de trabajo.", call. = FALSE)

  i <- utils::menu(bases,
                   title = "\n\u00bfEn qu\u00e9 base de datos est\u00e1n las variables?")
  if (i == 0) return(NULL)
  list(nombre = bases[i],
       datos = as.data.frame(get(bases[i], envir = entorno)))
}

#' Lista compacta de nombres de variables, en columnas
#' @noRd
.mostrar_variables <- function(nombres) {
  cat("\n")
  print(nombres, quote = FALSE)
  cat("\n")
}

#' Pide por consola el nombre de una variable y lo valida
#'
#' @param datos data.frame donde buscar.
#' @param tipo "cuantitativa" o "cualitativa".
#' @param pregunta Texto de la pregunta.
#' @param opcional Si es TRUE, pulsar Intro sin escribir nada significa
#'   "ninguna" y la función devuelve NA.
#' @return El nombre de la variable; NA si es opcional y se deja en blanco;
#'   NULL si se cancela.
#' @noRd
.pedir_variable <- function(datos, tipo = c("cuantitativa", "cualitativa"),
                            pregunta, opcional = FALSE) {
  tipo <- match.arg(tipo)
  es_tipo <- if (tipo == "cuantitativa") .es_cuantitativa else .es_cualitativa
  candidatas <- names(datos)[vapply(datos, es_tipo, logical(1))]

  if (length(candidatas) == 0) {
    aviso <- if (tipo == "cualitativa")
      paste("No hay variables cualitativas (factor). Si alguna est\u00e1",
            "codificada con n\u00fameros (p. ej. 0/1), convi\u00e9rtela antes a factor.")
    else "No hay variables num\u00e9ricas en esta base de datos."
    if (!opcional) stop(aviso, call. = FALSE)
    cat("\nNota:", aviso, "\n")
    return(NA_character_)
  }

  ayuda <- paste0("  (? = ver las variables ", tipo, "s, 0 = cancelar",
                  if (opcional) ", Intro = ninguna" else "", ")")
  cat("\n", pregunta, "\n", ayuda, "\n", sep = "")

  repeat {
    resp <- trimws(readline("Variable: "))
    # Se toleran comillas o acentos graves alrededor del nombre
    resp <- gsub("^[\"'`]+|[\"'`]+$", "", resp)

    if (resp == "0") return(NULL)
    if (resp == "") {
      if (opcional) return(NA_character_)
      cat("Escribe el nombre de una variable (? para ver la lista).\n")
      next
    }
    if (resp == "?") {
      .mostrar_variables(candidatas)
      next
    }

    # Coincidencia exacta con una variable del tipo correcto
    if (resp %in% candidatas) return(resp)

    # Existe, pero no es del tipo pedido
    if (resp %in% names(datos)) {
      clase <- class(datos[[resp]])[1]
      cat("'", resp, "' existe, pero no es ", tipo, " (es de clase '", clase,
          "').", sep = "")
      if (tipo == "cualitativa" && is.numeric(datos[[resp]]))
        cat(" Si es una variable de grupo codificada con n\u00fameros,",
            "convi\u00e9rtela antes a factor.")
      cat("\n")
      next
    }

    # R distingue mayúsculas y minúsculas: si solo difiere en eso, se acepta
    iguales <- candidatas[tolower(candidatas) == tolower(resp)]
    if (length(iguales) == 1) {
      cat("Se usa '", iguales, "' (R distingue may\u00fasculas y min\u00fasculas).\n",
          sep = "")
      return(iguales)
    }

    # No existe: sugerencias por parecido
    parecidas <- unique(c(
      candidatas[startsWith(tolower(candidatas), tolower(resp))],
      agrep(resp, candidatas, ignore.case = TRUE, value = TRUE,
            max.distance = 0.2)
    ))
    if (length(parecidas) > 0) {
      cat("No hay ninguna variable ", tipo, " llamada '", resp,
          "'. \u00bfQuiz\u00e1: ", paste(utils::head(parecidas, 5), collapse = ", "),
          "?\n", sep = "")
    } else {
      cat("No hay ninguna variable ", tipo, " llamada '", resp,
          "'. Escribe ? para ver la lista.\n", sep = "")
    }
  }
}

#' Pide por consola un número y lo valida
#'
#' Acepta coma o punto como separador decimal.
#' @param pregunta Texto de la pregunta.
#' @param defecto Valor si se pulsa Intro (NULL = obligatorio).
#' @param rango Límites exclusivos permitidos, c(mínimo, máximo).
#' @param aviso_rango Mensaje si el número cae fuera del rango.
#' @return El número, o NULL si se escribe "cancelar".
#' @noRd
.pedir_numero <- function(pregunta, defecto = NULL, rango = c(-Inf, Inf),
                          aviso_rango = NULL) {
  ayuda <- if (is.null(defecto)) "  (escribe cancelar para salir)"
           else paste0("  (Intro = ", defecto, "; escribe cancelar para salir)")
  cat("\n", pregunta, "\n", ayuda, "\n", sep = "")
  repeat {
    resp <- trimws(readline("Valor: "))
    if (tolower(resp) == "cancelar") return(NULL)
    if (resp == "" && !is.null(defecto)) return(defecto)
    num <- suppressWarnings(as.numeric(sub(",", ".", resp, fixed = TRUE)))
    if (length(num) != 1 || !is.finite(num)) {
      cat("'", resp, "' no es un n\u00famero v\u00e1lido. Usa por ejemplo 25 o 25.5.\n",
          sep = "")
      next
    }
    if (num <= rango[1] || num >= rango[2]) {
      cat(if (is.null(aviso_rango))
            paste0("El valor debe estar entre ", rango[1], " y ", rango[2], ".")
          else aviso_rango, "\n", sep = "")
      next
    }
    return(num)
  }
}

#' Elige por menú una categoría de una variable cualitativa
#'
#' @return La categoría elegida, o NULL si se cancela.
#' @noRd
.elegir_categoria <- function(datos, var, pregunta) {
  x <- datos[[var]]
  niveles <- levels(droplevels(factor(x[!is.na(x)])))
  if (length(niveles) < 2)
    stop("'", var, "' tiene menos de dos categor\u00edas con datos.", call. = FALSE)
  frec <- table(factor(x, levels = niveles))
  etiquetas <- paste0(niveles, "  (n = ", as.vector(frec), ")")
  i <- utils::menu(etiquetas, title = paste0("\n", pregunta))
  if (i == 0) return(NULL)
  niveles[i]
}

#' Pide una variable de grupo con exactamente dos categorías y su orden
#'
#' @param datos data.frame.
#' @param var_y Variable de respuesta: solo se cuentan categorías del grupo
#'   con datos válidos en ella.
#' @param pregunta Texto de la pregunta.
#' @param que_diferencia Texto que describe lo que se resta ("medias",
#'   "proporciones"), para el menú de orden.
#' @param extra_mas_de_dos Frase añadida si el grupo tiene más de dos
#'   categorías (p. ej. qué prueba usar en ese caso).
#' @return Lista con var_g, gA, gB (en el orden elegido) e invertido (TRUE si
#'   el orden elegido no es el del factor), o NULL si se cancela.
#' @noRd
.pedir_dos_grupos <- function(datos, var_y, pregunta, que_diferencia,
                              extra_mas_de_dos = "") {
  repeat {
    var_g <- .pedir_variable(datos, "cualitativa", pregunta)
    if (is.null(var_g)) return(NULL)
    if (identical(var_g, var_y)) {
      cat("La variable de grupo no puede ser la misma que la de respuesta.\n")
      next
    }
    g_tmp <- datos[[var_g]][!is.na(datos[[var_y]])]
    niveles <- levels(droplevels(factor(g_tmp[!is.na(g_tmp)])))
    if (length(niveles) == 2) break
    if (length(niveles) < 2) {
      cat("'", var_g, "' tiene menos de dos categor\u00edas con datos de '", var_y,
          "'. Elige otra variable.\n", sep = "")
    } else {
      .imprimir(paste0(
        "'", var_g, "' tiene ", length(niveles), " categor\u00edas (",
        paste(niveles, collapse = ", "), "). Esta prueba solo compara dos ",
        "grupos.", extra_mas_de_dos, " Elige otra variable o recodifica esta ",
        "en dos categor\u00edas."),
        sangria = 0)
    }
  }
  i <- utils::menu(
    c(paste0(niveles[1], "  (diferencia = ", niveles[1], " - ", niveles[2], ")"),
      paste0(niveles[2], "  (diferencia = ", niveles[2], " - ", niveles[1], ")")),
    title = paste0("\n\u00bfQu\u00e9 grupo quieres que sea el primero? La diferencia de ",
                   que_diferencia, " se calcular\u00e1 como primero menos segundo."))
  if (i == 0) return(NULL)
  list(var_g = var_g, gA = niveles[i], gB = niveles[3 - i], invertido = i == 2)
}

#' Nota sobre cómo reproducir en R Commander un orden de grupos invertido
#' @noRd
.nota_orden_rcmdr <- function(var_g, gA, gB) {
  .imprimir(paste0(
    "Has cambiado el orden de los grupos ('", gA, "' primero). En R ",
    "Commander el test usa el orden de las categor\u00edas del factor, ",
    "que tiene '", gB, "' primero. Para obtener el mismo resultado, ",
    "reordena antes el factor en Datos > Modificar variables del conjunto ",
    "de datos activo > Reordenar niveles de factor, poniendo '", gA,
    "' en primer lugar. Si no lo haces, R Commander calcular\u00e1 ",
    "la diferencia como '", gB, "' - '", gA, "': el signo del estad\u00edstico, ",
    "de la diferencia y del intervalo saldr\u00e1 cambiado, y en un contraste ",
    "unilateral la hip\u00f3tesis 'less' / 'greater' ser\u00eda la contraria."),
    sangria = 0)
}

#' Texto de valor p para frases de redacción
#' @noRd
.p_redaccion <- function(p) {
  if (p < 1e-4) "p < 0.0001" else paste0("p = ", .fmt_p(p))
}

#' Pide una variable de grupo con un número mínimo de categorías
#'
#' @param datos data.frame.
#' @param var_y Variable de respuesta: solo se cuentan categorías del grupo
#'   con datos válidos en ella.
#' @param pregunta Texto de la pregunta.
#' @param min_grupos Número mínimo de categorías con datos.
#' @param aviso_pocos Frase añadida si el grupo tiene menos categorías de las
#'   necesarias (p. ej. qué prueba usar en ese caso).
#' @return Lista con var_g y niveles (categorías con datos, en el orden del
#'   factor), o NULL si se cancela.
#' @noRd
.pedir_grupos <- function(datos, var_y, pregunta, min_grupos = 3,
                          aviso_pocos = "") {
  repeat {
    var_g <- .pedir_variable(datos, "cualitativa", pregunta)
    if (is.null(var_g)) return(NULL)
    if (identical(var_g, var_y)) {
      cat("La variable de grupo no puede ser la misma que la de respuesta.\n")
      next
    }
    g_tmp <- datos[[var_g]][!is.na(datos[[var_y]])]
    niveles <- levels(droplevels(factor(g_tmp[!is.na(g_tmp)])))
    if (length(niveles) >= min_grupos) break
    if (length(niveles) < 2) {
      cat("'", var_g, "' tiene menos de dos categor\u00edas con datos de '", var_y,
          "'. Elige otra variable.\n", sep = "")
    } else {
      .imprimir(paste0(
        "'", var_g, "' solo tiene ", length(niveles), " categor\u00edas con datos (",
        paste(niveles, collapse = ", "), "). Esta prueba compara ",
        min_grupos, " o m\u00e1s grupos.", aviso_pocos, " Elige otra variable."),
        sangria = 0)
    }
  }
  list(var_g = var_g, niveles = niveles)
}

#' Shapiro-Wilk en cada grupo con ajuste de Holm
#'
#' @param por_grupo Lista con los valores de cada grupo (p. ej. de split()).
#' @param alpha Nivel de significación.
#' @return data.frame con n, p (sin ajustar), p_holm y no_normal (TRUE si
#'   p_holm < alpha) por grupo. p es NA si n < 3 o n > 5000; p.adjust ignora
#'   los NA al contar los contrastes.
#' @noRd
.shapiro_grupos <- function(por_grupo, alpha) {
  n <- vapply(por_grupo, length, numeric(1))
  p <- vapply(por_grupo, function(x)
    if (length(x) >= 3 && length(x) <= 5000 && stats::sd(x) > 0)
      stats::shapiro.test(x)$p.value else NA_real_, numeric(1))
  p_holm <- stats::p.adjust(p, method = "holm")
  data.frame(n = n, p = p, p_holm = p_holm,
             no_normal = !is.na(p_holm) & p_holm < alpha,
             row.names = names(por_grupo))
}

#' Texto del resultado de Shapiro-Wilk de un grupo (p sin ajustar y Holm)
#' @noRd
.texto_shapiro <- function(nom, n, p, p_holm, alpha) {
  if (is.na(p)) return(paste0("'", nom, "' (n = ", n, "): no se puede ",
                              "aplicar (requiere entre 3 y 5000 ",
                              "observaciones no constantes)."))
  paste0("'", nom, "' (n = ", n, "): p sin ajustar", .comparar_p(p),
         "; p ajustado", .comparar_p(p_holm),
         if (p_holm < alpha) paste0(" < ", alpha, ", se aleja de la normal.")
         else paste0(" >= ", alpha, ", sin evidencia de no normalidad."))
}

#' Nota estándar sobre el ajuste de Holm en la normalidad por grupos
#' @noRd
.nota_holm <- function(n_contrastes) {
  paste0("Los valores p se ajustan por el m\u00e9todo de Holm (", n_contrastes,
         " contrastes), como hace R Commander al pedir el test de ",
         "normalidad por grupos: al hacer un contraste en cada grupo aumenta ",
         "la probabilidad de que alguno salga significativo por azar. La ",
         "decisi\u00f3n se toma con el p ajustado.")
}
