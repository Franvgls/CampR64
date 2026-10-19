# CampR64 0.2.0
Migración desde CampR de las funciones que quedaban pendientes, leyendo los DBF
con `readCampDBF()` en lugar de RODBC/ODBC.

## Nuevas funciones
* `dtall05.camp64()`: distribución de tallas estratificada por campañas con
  `cor.5` para pasar a 1 cm las campañas medidas a .5 cm (en mm), campaña a
  campaña, de modo que se pueden comparar series con unidades mezcladas
  (*Capros aper*, sardina, boquerón...). Antes `dtall.campa` en CampR; con
  `sex=TRUE` y `cor.5` ya no pierde machos y hembras, y con `sex=FALSE` ya no
  duplica los totales. `dtall.campa64()` se mantiene como alias obsoleto
  (`.Deprecated`).
* `CVlist.camp64()`: biomasa, número y nº de lances de todas las especies de un
  grupo (o `gr=9`) con familia, `percents` y `minim`. `minim` filtra siempre en
  kg/g y los porcentajes se calculan sobre todas las especies.
* `TabAbsEsp.camp64()`: tabla de medias y SE por sector x estrato, sectores,
  estratos y total (biomasa y número) para informes de campaña. Se construye a
  partir de los nombres de sectores y estratos, ya no sólo para el Cantábrico.
* `datTalaGeo.camp64()`: tallas por lance con posición, profundidad y sector.
  `number` va elevado al peso total de la muestra (en CampR se calculaba pero
  se devolvía sin elevar) y `cor.time`, `excl.sect` y `sex` ya tienen efecto.
* `datmuest.camp64()`: resumen del muestreo de una especie. Los pesos se suman
  por lance y categoría (antes la media por lance subestimaba el peso con
  varias categorías); coincide con el fichero de capturas.
* `dtallan.peso64()`: peso de la distribución de tallas por talla-peso. Toma
  la marca de clase en cm según `med`/`increm` (especies medidas en mm o .5 cm),
  `sex=TRUE` da el peso por sexos y `muestr` elige captura o muestra.

## Cambios
* `dtall.camp64()` y `dtall05.camp64()`: las marcas del eje x se calculan con
  el nuevo auxiliar interno `ticks.tal()` (~10 valores redondos), para que las
  etiquetas no se solapen con tallas en mm o .5 cm.
* Fix: `dtallan.camp64()` forzaba `sex=FALSE` (con aviso) también para una
  sola especie; ahora sólo con `esp="999"`.
* `hidrotodec` de CampR no se migra: lo cubre `dathidro.camp64()`.
* Documentación de los datasets `VedaCigala` y `VedasBesugo` retocada.

# CampR64 0.1.20
* `datlan.camp64()`: nuevo argumento `fill.dist` (TRUE por defecto). Los lances
  válidos con recorrido NA/-9/<=0 en el CAMP se rellenan con la distancia
  Haversine entre largada y virada y se avisa con un `warning()`. Los lances
  nulos no se rellenan.
* `CAMPtoHH64()` y `CAMPtoHHnw64()`: argumento `fill.dist`, pasado a
  `datlan.camp64()`, para que `Distance` no salga como -9 al probar lances en
  IMBUS. Con `fill.dist=FALSE` se mantiene el comportamiento anterior.
* `qcdistlan.camp64()`: usa `datlan.camp64(fill.dist=FALSE)` y rellena por su
  cuenta, avisando de que `error.dist` es 0 por construcción en esos lances.
* Nuevo auxiliar interno `.fill_recorrido64()`.

# CampR64 0.1.19
* `NepFU25/26/30/31.camp64()` y `NepFUs.camp64()`: nuevos argumentos `graf`,
  `xpng`, `ypng` y `ppng` para guardar directamente a PNG (como
  `ArteParComp64()`), con tamaños por defecto ajustados a cada FU. Se corrige
  el `dev.new()` que calculaba el aspecto con límites distintos a los del mapa.
  `NepFU25/26` añaden `ref` para fijar la escala de la leyenda.
* `NepFUs.camp64()`: usa `MapIberia64()` como mapa base; tamaño PNG por defecto
  según zona (cant 1200x600, arsa 600x600); `out.dat` ya funciona con
  `zona="arsa"` y con `out.dat=FALSE` (antes fallaba por `datFus` no definido).
* Fix: `MapArsa64(FU="FU30")` fallaba (`draw_FU` usaba `names()` sobre una
  matriz); ahora indexa `long`/`lat` como `MapNort64()`.
* `qcdistlan.camp64()`: los lances con recorrido NA/-9/<=0 se rellenan con la
  distancia Haversine y se avisa con un `warning()`, no solo si faltan todos.

# CampR64 0.1.18
* Fix: `maphist64()`, `maphistage64()`, `maphistal64()` y `MapEcol64.camp()`
  no pintaban el fondo del mar ni variaban el color de la tierra según `bw`
  en sus paneles lattice (a diferencia de `MapIberia64()` y el resto de mapas
  base). Ahora `bw=FALSE` pinta mar `lightblue1`, tierra `wheat` y puntos en
  negro; `bw=TRUE` pinta mar blanco, tierra `lightgray` y puntos en gris.
  En `MapEcol64.camp()` los puntos mantienen su color por categoría
  (índice ecológico) independientemente de `bw`.

# CampR64 0.1.17
* `qcdistlan.camp64()`: recupera los gráficos dist/speed/course perdidos en la
  migración desde CampR (argumento `plot`), portados de `qcHaulsDist()`/IMBUS.
* Nuevas viñetas: control de calidad (`control-calidad-campR64.Rmd`) y
  campañas/áreas implementadas (`campanas-areas-campR64.Rmd`).
* Fix: typo en `@family` de `AbrvEsp64.R` que rompía el `\seealso` cruzado de
  la familia `datos_especies`.
* `Imports`: se añaden `stats` y `graphics`.

# CampR64 0.1.16
# CampR64 0.1.15
# CampR64 0.1.14
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