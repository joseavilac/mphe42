#' Asistente: ¿qué prueba quieres hacer?
#'
#' Muestra un menú con las pruebas estadísticas interactivas del paquete y
#' ejecuta la que se elija. A partir de ahí, la función de la prueba pregunta
#' por consola los datos, las variables y las opciones, y presenta su informe
#' comentado. Es la puerta de entrada a las pruebas del paquete, igual que
#' [indicar()] lo es a las guías de R Commander.
#'
#' Las pruebas disponibles y el orden del menú se definen en un registro
#' interno (`.pruebas_hacer()`, en este mismo archivo). Cada vez que se añade
#' una función de prueba nueva al paquete hay que añadirla también ahí.
#'
#' @param opcion Prueba que se quiere ejecutar, sin pasar por el menú: su
#'   número en el menú o el nombre de la función (por ejemplo,
#'   `"media_2muestra"`). Si es `NULL` (por defecto) se muestra el menú.
#' @param alpha Nivel de significación que se pasa a la prueba elegida. Por
#'   defecto 0.05.
#' @param ventana Lógico. Si es `FALSE` (por defecto) el menú se muestra en
#'   la consola y se responde escribiendo el número de la opción. Si es `TRUE`
#'   el menú se abre en una ventana emergente.
#'
#' @return De forma invisible, lo que devuelve la prueba ejecutada (ver la
#'   ayuda de cada función), o `NULL` si se cancela.
#'
#' @seealso [indicar()] para los pasos en R Commander; [normalidad()],
#'   [varianza()], [media_1muestra()], [media_2muestra()],
#'   [anova_1factor()], [np_mw()], [np_kw()], [proporcion_1muestra()],
#'   [proporcion_2muestra()] y [prueba_correlacion()].
#'
#' @examples
#' if (interactive()) {
#'   hacer()                    # menú con todas las pruebas
#'   hacer("media_2muestra")    # directamente la prueba t de dos muestras
#'   hacer(4, alpha = 0.01)     # opción 4 del menú, con alfa = 0.01
#' }
#'
#' @export
#' @importFrom utils menu
hacer <- function(opcion = NULL, alpha = 0.05, ventana = FALSE) {

  if (!interactive())
    stop("hacer() es interactiva: ejec\u00fatala desde la consola de R.",
         call. = FALSE)
  if (!is.numeric(alpha) || length(alpha) != 1 || is.na(alpha) ||
      alpha <= 0 || alpha >= 1)
    stop("'alpha' debe ser un n\u00famero entre 0 y 1.", call. = FALSE)

  pruebas <- .pruebas_hacer()
  titulos <- vapply(pruebas, `[[`, character(1), "titulo")
  nombres <- vapply(pruebas, `[[`, character(1), "nombre")
  lista <- paste0("  ", format(seq_along(titulos)), ". ", titulos, "  [",
                  nombres, "()]", collapse = "\n")

  ## Elegir la prueba ----------------------------------------------------
  if (is.null(opcion)) {
    cat("\nPruebas disponibles. Escribe el n\u00famero de la que quieras ",
        "hacer (0 = cancelar).\n",
        "Si no sabes qu\u00e9 prueba corresponde o quieres ver los pasos en ",
        "R Commander, usa indicar().\n", sep = "")
    opcion <- utils::menu(titulos, graphics = ventana,
                          title = "\u00bfQu\u00e9 prueba quieres hacer?")
    if (opcion == 0L) return(.cancelar())
  }

  if (is.character(opcion) && length(opcion) == 1L && !is.na(opcion)) {
    num <- match(sub("\\(\\)$", "", trimws(opcion)), nombres)
    if (is.na(num)) {
      cat("\nPruebas disponibles:\n", lista, "\n\n", sep = "")
      stop("No hay ninguna prueba llamada '", opcion, "'. Usa el nombre de ",
           "una de las funciones de la lista anterior.", call. = FALSE)
    }
    opcion <- num
  }
  if (!is.numeric(opcion) || length(opcion) != 1L || is.na(opcion) ||
      !opcion %in% seq_along(pruebas)) {
    cat("\nPruebas disponibles:\n", lista, "\n\n", sep = "")
    stop("'opcion' debe ser un n\u00famero entre 1 y ", length(pruebas),
         " o el nombre de una prueba de la lista anterior.", call. = FALSE)
  }

  ## Ejecutar la prueba --------------------------------------------------
  prueba <- pruebas[[opcion]]
  cat("\n>> ", prueba$titulo, "\n   (equivale a ejecutar ", prueba$nombre,
      "(", if (alpha != 0.05) paste0("alpha = ", alpha), "))\n\n", sep = "")
  invisible(prueba$funcion(alpha = alpha))
}


# Registro de las pruebas de hacer() ----------------------------------------
# Una entrada por función de prueba, en el orden en que aparecen en el menú.
# - titulo:  texto del menú, en lenguaje de "qué quiero hacer" y con el nombre
#            de la prueba entre paréntesis.
# - nombre:  nombre de la función exportada (permite hacer("nombre")).
# - funcion: la propia función; debe aceptar el argumento `alpha`.
# AL AÑADIR UNA FUNCIÓN DE PRUEBA NUEVA AL PAQUETE, AÑADIRLA AQUÍ.

#' @noRd
.pruebas_hacer <- function() {
  list(
    list(titulo = "Comprobar si una variable cuantitativa sigue una distribuci\u00f3n normal (Shapiro-Wilk y otras)",
         nombre = "normalidad", funcion = normalidad),
    list(titulo = "Comparar las varianzas de dos o m\u00e1s grupos (Levene)",
         nombre = "varianza", funcion = varianza),
    list(titulo = "Comparar una media con un valor de referencia (t para una muestra)",
         nombre = "media_1muestra", funcion = media_1muestra),
    list(titulo = "Comparar las medias de dos grupos independientes (t de Student / Welch)",
         nombre = "media_2muestra", funcion = media_2muestra),
    list(titulo = "Comparar las medias de tres o m\u00e1s grupos (ANOVA de un factor)",
         nombre = "anova_1factor", funcion = anova_1factor),
    list(titulo = "Comparar dos grupos independientes con una prueba no param\u00e9trica (Wilcoxon / Mann-Whitney)",
         nombre = "np_mw", funcion = np_mw),
    list(titulo = "Comparar tres o m\u00e1s grupos con una prueba no param\u00e9trica (Kruskal-Wallis)",
         nombre = "np_kw", funcion = np_kw),
    list(titulo = "Comparar una proporci\u00f3n con un valor de referencia",
         nombre = "proporcion_1muestra", funcion = proporcion_1muestra),
    list(titulo = "Comparar las proporciones de dos grupos independientes",
         nombre = "proporcion_2muestra", funcion = proporcion_2muestra),
    list(titulo = "Estudiar la correlaci\u00f3n entre dos variables cuantitativas (Pearson / Spearman / Kendall)",
         nombre = "prueba_correlacion", funcion = prueba_correlacion)
  )
}
