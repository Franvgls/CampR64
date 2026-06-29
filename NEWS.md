# CampR64 1.0.8

## Funciones nuevas

* `grafalk.camp64()`: representación gráfica de la clave talla-edad como
  barras apiladas de proporciones por edad. Distingue tallas con reparto
  imputado (flag `99` de los DBF originales) mediante tramado diagonal y
  asterisco. Complementaria a `grafedtal.camp64()`.

* `load_worldHires64()` / `unload_worldHires64()` (internas): cargan y
  descargan temporalmente `worldHiresMapEnv` en `globalenv()` para que
  `maps::map()` lo encuentre. Centraliza el patrón antes duplicado.

## Mejoras

* `armap64()`:
  - Defaults de `xlims`/`ylims` por zona (porc, arsa, cant) que respetan
    los valores que pase el usuario.
  - Posición de leyenda configurable vía `legpos`.
  - Normalización interna de `lon` → `long` para alinearse con la
    convención del paquete.
* `maparea64()`: carga segura de `worldHiresMapEnv` y eliminación del
  filtro frágil por nombres de regiones.
* `MapArsa64()`: ejes dinámicos derivados de `par("usr")`; `clip()` para
  evitar que la tierra se salga del recuadro con `xlims`/`ylims` amplios.
* `IBTSNeAtl_map64()`: refactorizado para usar `load_worldHires64()`.
* `CampsDNS.camp64()`: refactorizada para tomar `(zona, dns)` y resolver
  el directorio desde `CampR64_paths`. Antes estaba hardcoded a
  `C:/camp/...` y solo aceptaba `dns`.
* `grafhistbox64()`: corregido el bug que dejaba el plot vacío cuando
  `dumb$sector` venía como carácter en lugar de factor.
* `grafhistbox64()`: ahora respeta layouts multipanel externos
  (`mfrow`/`mfcol`/`layout`); permite usar `grafhistbox64.comp()` con
  los dos plots en la misma figura.

## Limpieza

* Eliminado `R/install_deps.R` (código top-level que se ejecutaba al
  cargar el paquete y disparaba `install.packages()` durante
  `R CMD check`). Las dependencias se gestionan ya desde `DESCRIPTION`.
* `.onLoad` migrado a `.onAttach` en `R/zzz.R` para que la carga de
  `configRoots_user.R` no se ejecute durante `R CMD check`.

## Documentación

* `grafedtal.camp64()` y `grafalk.camp64()` enlazadas mutuamente vía
  `@seealso` y `@family ALK`.
* Ejemplos protegidos con `\dontrun{}` en todas las funciones que
  requieren acceso a DBFs locales.

# CampR64 0.1.13

## Correcciones

* `AbAgStatRec.camp64()`: ahora incluye todos los rectángulos ICES muestreados
  (con ceros donde no hubo captura) al pasar `ceros=TRUE` a la llamada interna
  de `maphistage64()`. Antes se perdían rectángulos sin captura y las medias
  quedaban sesgadas al alza (denominador solo con lances positivos).
* `AbAgStatRec.camp64()`: eliminado el `merge` redundante con `CAMPtoHH64()`
  que duplicaba la columna `StatRec` (`.x`/`.y`) y rompía el `tapply()`. Se usa
  directamente la `StatRec` que ya devuelve `maphistage64()`.
* `AbAgStatRec.camp64()`: corregida la doble almohadilla en los nombres de
  rectángulo (`##12E0#` → `#12E0#`).
* `maphistage64()`: protegido el cálculo de escala ante edades sin datos
  (todo-NA), que devolvía `-Inf` y reventaba el gráfico/salida. Selección de
  columnas de edad por nombre en lugar de posición fija.