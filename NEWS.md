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