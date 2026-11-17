# Mensaje de bienvenida ------------------------------------------------------
# Se muestra al cargar el paquete con library(pphelp). Se usa
# packageStartupMessage() (y no cat()) para que se pueda silenciar con
# suppressPackageStartupMessages(library(pphelp)).

.onAttach <- function(libname, pkgname) {
  version <- tryCatch(as.character(utils::packageVersion(pkgname)),
                      error = function(e) "")
  packageStartupMessage(
    "\npphelp ", version, ": asistente para R Commander en salud p\u00fablica.\n\n",
    "  indicar()  indica, paso a paso, qu\u00e9 men\u00fas y opciones elegir en\n",
    "             R Commander para lo que quieres hacer.\n",
    "  hacer()    te ayuda a realizar una prueba estad\u00edstica e interpretar\n",
    "             sus resultados.\n\n",
    "Escr\u00edbelas en la consola de R y responde a las preguntas\n",
    "(0 = cancelar).\n"
  )
}
