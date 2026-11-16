# pphelp

<!-- badges: start -->

<!-- badges: end -->

**pphelp** es un asistente para estudiantes que se inician en la estadística
con [R Commander](https://cran.r-project.org/package=Rcmdr) en salud pública e
investigación epidemiológica. Tiene dos partes:

- **Una guía interactiva** (`indicar()`) que indica, paso a paso, qué menús y
  opciones de R Commander utilizar según lo que se quiere hacer: filtrar,
  recodificar, resumir variables, comparar medias o proporciones, estudiar
  correlaciones...
- **Funciones interactivas que aplican e interpretan las pruebas más
  habituales.** Preguntan por consola los datos, las variables y las opciones,
  y devuelven un informe completo en español: supuestos, hipótesis, cálculo
  del estadístico, valor p, decisión, intervalo de confianza, tamaño del
  efecto, una frase para redactar el resultado y la orden equivalente en
  R Commander.

## Instalación

El paquete se instala desde GitHub. Solo hace falta R (versión 4.2 o
posterior); no requiere Rtools porque no contiene código compilado.

```r
# install.packages("remotes")   # solo la primera vez
remotes::install_github("joseavilac/pphelp")
```

Las dependencias (`car` y `nortest`) se instalan automáticamente desde CRAN.

Para instalar una versión concreta:

```r
remotes::install_github("joseavilac/pphelp@v0.0.1")
```

Si no hay acceso a GitHub, se puede instalar desde el archivo del paquete
(`pphelp_0.0.1.tar.gz`):

```r
install.packages(c("car", "nortest"))
install.packages("pphelp_0.0.1.tar.gz", repos = NULL, type = "source")
```

## Uso

```r
library(pphelp)
```

Las funciones son **interactivas**: ejecútalas en la consola de R (RGui o
RStudio) y responde a las preguntas escribiendo el número de la opción o el
nombre de la variable. Trabajan con los `data.frame` cargados en el espacio de
trabajo, incluido el conjunto de datos activo de R Commander. En cualquier
momento se puede cancelar con `0`.

### Funciones disponibles

| Función                  | Qué hace                                                                                                                                | Equivalente en R Commander                                            |
| ------------------------- | ---------------------------------------------------------------------------------------------------------------------------------------- | --------------------------------------------------------------------- |
| `indicar()`             | Guía paso a paso de las tareas habituales en R Commander                                                                                | —                                                                    |
| `normalidad()`          | Pruebas de normalidad (Shapiro-Wilk, Shapiro-Francia, Anderson-Darling, Cramer-von Mises, Lilliefors, chi cuadrado), global o por grupos | Estadísticos > Resúmenes > Test de normalidad                       |
| `varianza()`            | Prueba de Levene de homogeneidad de varianzas                                                                                            | Estadísticos > Varianzas > Test de Levene                            |
| `media_1muestra()`      | Prueba t para una muestra                                                                                                                | Estadísticos > Medias > Test t para una muestra                      |
| `media_2muestra()`      | Prueba t para dos muestras independientes (Welch o Student)                                                                              | Estadísticos > Medias > Test t para muestras independientes          |
| `proporcion_1muestra()` | Prueba de proporciones para una muestra (aproximación normal o binomial exacta)                                                         | Estadísticos > Proporciones > Test de proporciones para una muestra  |
| `proporcion_2muestra()` | Comparación de dos proporciones independientes (aproximación normal o prueba exacta de Fisher), con RR y OR                            | Estadísticos > Proporciones > Test de proporciones para dos muestras |
| `prueba_correlacion()`  | Correlación de Pearson, Spearman o Kendall, con covarianza                                                                              | Estadísticos > Resúmenes > Test de correlación                     |

Cada función tiene su página de ayuda: `?media_2muestra`, `?indicar`, etc.

### Ejemplo

```r
media_2muestra()
```

La función pregunta, en este orden:

```
¿En qué base de datos están las variables?
1: encuesta
Selection: 1

¿Qué variable cuantitativa quieres comparar entre los dos grupos?
  (? = ver las variables cuantitativas, 0 = cancelar)
Variable: PAS

¿Qué variable define los dos grupos?
Variable: sexo

¿Qué grupo quieres que sea el primero? ...
¿Cuál es la hipótesis alternativa (H1)? ...
¿Quieres suponer que las varianzas de los dos grupos son iguales? ...
```

Si se escribe mal el nombre de una variable, la función sugiere las más
parecidas; si la variable no es del tipo adecuado (por ejemplo, una variable
de grupo codificada como 0/1), lo indica y explica cómo corregirlo.

El informe resultante se organiza en secciones:

1. Descripción de los datos.
2. Supuestos de la prueba (independencia, normalidad, homogeneidad de
   varianzas, valores atípicos, frecuencias esperadas) y justificación del
   método elegido.
3. Hipótesis nula y alternativa, en símbolos y en palabras.
4. Cálculo del estadístico de contraste, con los números sustituidos.
5. Valor p, su interpretación y la decisión.
6. Intervalo de confianza.
7. Tamaño del efecto y su interpretación.
8. Frase para redactar el resultado.

Al final se muestra la ruta de menús y la orden equivalente en R Commander,
para que el estudiante pueda reproducir el análisis.

Todas las funciones aceptan el argumento `alpha` (por defecto 0.05) y
devuelven de forma invisible una lista con los resultados, por si se quieren
reutilizar:

```r
res <- media_2muestra(alpha = 0.01)
res$prueba        # resultado de t.test()
```

## Criterios estadísticos

Para que los informes sean coherentes entre sí y con la guía `indicar()`, el
paquete aplica estos criterios:

- **Normalidad:** Shapiro-Wilk. En las pruebas t, con n ≥ 30 por grupo se
  considera que la prueba es robusta por el teorema central del límite.
  Si n < 30 y no hay normalidad, se recomienda la alternativa no paramétrica
  (Wilcoxon, Mann-Whitney).
- **Comparaciones múltiples** (normalidad por grupos): ajuste de Holm, como
  `RcmdrMisc::normalityTest()`.
- **Levene:** centrado en la mediana (variante de Brown-Forsythe), como en
  R Commander.
- **Prueba t de dos muestras:** se ofrecen Welch (recomendada, opción por
  defecto de R Commander), Student o decidir según Levene.
- **Proporciones:** se recomienda la prueba exacta (binomial o de Fisher)
  cuando alguna frecuencia esperada bajo H0 es menor que 5.
- **Correlación:** se recomienda Pearson si se asume normalidad en las dos
  variables, y Spearman o Kendall si no. La magnitud se clasifica según
  Evans (1996): < 0.2 muy débil, 0.2–0.4 débil, 0.4–0.6 moderada, 0.6–0.8
  fuerte, ≥ 0.8 muy fuerte.
- **Tamaño del efecto:** d de Cohen (medias) y h de Cohen (proporciones),
  con los umbrales orientativos 0.2, 0.5 y 0.8. En la comparación de dos
  proporciones se añaden la diferencia en puntos porcentuales, el riesgo
  relativo y la odds ratio.
- **Contrastes unilaterales:** la hipótesis nula se expresa en forma
  compuesta (≤ o ≥) y se explica que el valor p se calcula en su límite.

## Aviso

pphelp es una herramienta docente. Sus recomendaciones siguen criterios
habituales en la enseñanza de la bioestadística, pero no sustituyen el
razonamiento sobre el diseño del estudio ni la revisión gráfica de los datos.

## Autor

José Alejandro Ávila (<jaachusc@gmail.com>)

## Licencia

MIT. Consulta el archivo [LICENSE.md](LICENSE.md).
