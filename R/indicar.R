#' Asistente: ¿qué deseas hacer?
#'
#' Muestra un menú con tareas habituales de manejo y análisis de datos y
#' devuelve, en la consola, los pasos que hay que seguir en R Commander para
#' realizarlas, el código equivalente que generará R Commander y los errores
#' más frecuentes.
#'
#' @param opcion Número de la opción que se quiere consultar. Si es `NULL`
#'   (por defecto) se muestra el menú para elegirla.
#' @param ventana Lógico. Si es `FALSE` (por defecto) el menú se muestra en
#'   la consola y se responde escribiendo el número de la opción. Si es `TRUE`
#'   el menú se abre en una ventana emergente.
#'
#' @return De forma invisible, una lista con la guía mostrada (`titulo`,
#'   `ruta`, `pasos`, `ejemplos`, `codigo` y `consejos`). Se usa por sus
#'   efectos: imprimir la guía en la consola.
#'
#' @examples
#' # Consultar directamente una opción sin pasar por el menú
#' indicar(2)
#'
#' # Uso habitual: abre el menú
#' if (interactive()) indicar()
#'
#' @export
indicar <- function(opcion = NULL, ventana = FALSE) {
  guias <- guias_indicar()
  titulos <- vapply(guias, `[[`, character(1), "titulo")

  if (is.null(opcion)) {
    if (!interactive()) {
      stop("indicar() necesita una sesión interactiva para mostrar el menú. ",
           "Indica la opción directamente, por ejemplo: indicar(1).",
           call. = FALSE)
    }
    opcion <- utils::menu(titulos, graphics = ventana,
                          title = "¿Qué deseas hacer?")
    if (opcion == 0L) {
      message("No se ha seleccionado ninguna opción.")
      return(invisible(NULL))
    }
  }

  if (!is.numeric(opcion) || length(opcion) != 1L || is.na(opcion) ||
      !opcion %in% seq_along(guias)) {
    stop("'opcion' debe ser un número entre 1 y ", length(guias), ":\n",
         paste0("  ", seq_along(titulos), ". ", titulos, collapse = "\n"),
         call. = FALSE)
  }

  guia <- guias[[opcion]]
  mostrar_guia(guia)
  invisible(guia)
}


# Imprime una guía en la consola ------------------------------------------
# `pasos` puede ser un vector de texto (una sola secuencia de pasos) o una
# lista con nombre de vectores de texto (varias fases, cada una con su
# título y su propia numeración).

mostrar_guia <- function(guia) {
  linea <- strrep("=", 70)
  sublinea <- function(txt) {
    cat("\n", txt, "\n", strrep("-", nchar(txt)), "\n", sep = "")
  }
  numerar <- function(x) cat(paste0(format(seq_along(x)), ". ", x), sep = "\n")

  cat("\n", linea, "\n", toupper(guia$titulo), "\n", linea, "\n", sep = "")

  sublinea("Ruta en R Commander")
  cat(guia$ruta, sep = "\n")

  sublinea("Pasos")
  if (is.list(guia$pasos)) {
    for (fase in names(guia$pasos)) {
      cat("\n>> ", fase, "\n", sep = "")
      numerar(guia$pasos[[fase]])
    }
  } else {
    numerar(guia$pasos)
  }

  if (length(guia$ejemplos)) {
    sublinea("Ejemplos")
    cat(paste0("  ", guia$ejemplos), sep = "\n")
  }

  sublinea("Código que generará R Commander (ejemplo)")
  cat(paste0("  ", guia$codigo), sep = "\n")

  sublinea("Cuidado con")
  cat(paste0("- ", guia$consejos), sep = "\n")
  cat("\n")
}
