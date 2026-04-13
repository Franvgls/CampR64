# Contexto de trabajo: Paquete CampR / CampR64 en R

## Entorno de trabajo

| Parámetro | Valor |
|---|---|
| **Usuario** | FVG |
| **Directorio usuario** | `D:\FVG\` |
| **SO** | Windows |
| **R (CampR)** | 3.6.3 (32 bits) |
| **R (CampR64)** | 4.0.4 (64 bits) |
| **Librería CampR** | `C:\GithubRs\CampR\` |
| **Librería CampR64** | `D:\FVG\GithubRs\CampR64\` |

---

## Historia y motivación de la migración

**CampR** es un paquete R para campañas de evaluación de recursos marinos (IBTS, MEDITS, etc.) desarrollado originalmente para Windows 32 bits bajo R 3.6.3. Su principal limitación estructural es el uso de **conexiones RODBC** para acceso a bases de datos Access (.mdb), que en Windows sólo funcionan con drivers ODBC de 32 bits. Esto bloqueó permanentemente el paquete en R 3.6.3 (última versión 32 bits de R).

**CampR64** es la migración a R 4.0.4 (64 bits), en la que se sustituyen las conexiones RODBC por alternativas compatibles con arquitecturas de 64 bits. Las funciones con sufijo `64` o ubicadas en CampR64 son las versiones migradas de sus equivalentes en CampR.

---

## Catálogo de funciones por categorías

### 📦 Infraestructura del paquete
| Función | Descripción |
|---|---|
| `CampR-package.r` | Documentación general del paquete |
| `config.R` | Configuración global de parámetros |
| `z_imports.R` | Importaciones de dependencias externas |
| `agradec.R` | **A**→**GRA**dos **DEC**imales: conversión grados decimales → formato GGG.MMmm (formato interno CAMP) |

---

### 🗺️ Cartografía y mapas

| Función | Descripción |
|---|---|
| `armap.camp.r` / `armap.tot.r` | Mapas de área de campaña (totales) |
| `arMapLansGPS.R` | Mapas de lances con coordenadas GPS |
| `GrafMarksGPS.R` / `GrafMarksGPS_2.R` | Gráficos de marcas GPS en mapa |
| `MapArsa.r` | Mapa zona ARSA |
| `MapCant.r` | Mapa Cantábrico |
| `MapComp.R` | Mapa comparativo |
| `MapEcol.camp.r` | Mapa ecológico de campaña |
| `MapGaltal.R` | Mapa Galicia (tallas) |
| `MapHidro.camp.R` / `MapHidro_b.camp.R` | Mapas hidrográficos de campaña |
| `MapIberia.R` | Mapa Península Ibérica |
| `MapLansGPS.r` / `MapLansHH.R` | Mapas de lances GPS / HH |
| `MapMedit.R` | Mapa Mediterráneo |
| `MapNort.r` | Mapa Norte |
| `MapPorc.R` / `mapporco.r` | Mapas marsopa / cetáceos |
| `maphist.r` / `maphistage.r` / `maphistal.r` / `maphistal.one.r` | Mapas históricos (tallas, edades) |
| `maparea.r` | Mapa de áreas |
| `mapscale.R` | Escala cartográfica |
| `mapsorteo.r` | Mapa de sorteo de estaciones |

---

### 🌊 Hidrografía / CTD

| Función | Descripción |
|---|---|
| `dathidro.camp.r` | Carga datos hidrográficos de campaña desde `hidroXXX.dbf` |
| `MapHidro.camp.R` / `MapHidro_b.camp.R` | Mapas hidrográficos de campaña |
| `hidrotodec.R` | Coordenadas CTD → grados decimales (ver sección Conversiones) |
| `HdectoH.R` / `HextoDec.R` | Conversiones hexadecimal → decimal (ver sección Conversiones) |
| `MatchHidro.ctd.R` | Emparejamiento datos CTD con datos de lance |

---

### 📊 Datos de campaña (lectura y carga)

| Función | Descripción |
|---|---|
| `datos.camp.r` | Carga datos generales de campaña |
| `datab.r` | **Abundancias estratificadas totales** por campaña. En CampR usaba DNS RODBC (`Cant`, `Porc`, `Arsa`); en CampR64 usa `readCampDBF()` |
| `databEstr.r` | **Abundancias por estrato** — desagregación de `datab` por estrato de profundidad/zona dentro de la campaña |
| `databICES.r` | **Abundancias por división ICES** — agrega los datos por las divisiones ICES que abarca cada campaña (ver tabla de zonas ICES por campaña abajo) |
| `databICESDiv.r` | Abundancias desglosadas por subdivisión ICES individual |
| `datCatches.camp.R` | Datos de capturas |
| `datgr.camp.r` / `datgr.camp64.r` | Datos de grupos taxonómicos (32/64 bits) |
| `datlan.camp.r` | Datos de lances |
| `datmuest.camp.r` | Datos de muestreo |
| `dattal.camp.r` / `dattal.camps.r` | Datos de tallas (campaña/campañas) |
| `datTalaGeo.camp.r` | Datos tallas con georreferencia |
| `dattalb.camp.r` | Datos tallas buck |
| `datTalCatch.camp.r` | Datos tallas de capturas |
| `dattalgr.camp.r` | Datos tallas por grupos |
| `dattalmean.camp.r` | Datos tallas medias |
| `datagegr.camp.r` | Datos geográficos por grupos |
| `camp_readers_dbf.r` | Lectores de archivos DBF para campañas |
| `camptoyear.r` | Conversión campaña a año |
| `CampsDNS.camp.R` | En CampR: nombre del DNS ODBC de cada zona. En CampR64: lista campañas disponibles por directorio |

#### Conexiones RODBC en CampR (obsoleto, referencia histórica)

En CampR, las funciones `datab`, `databEstr`, `databICES`, etc. recibían como argumento el nombre del **DNS ODBC** configurado en el Panel de Control de Windows (32 bits). Los DNS estándar eran:

| DNS RODBC | Zona |
|---|---|
| `Cant` | Demersales Norte — Cantábrico y Galicia |
| `Porc` | Banco de Porcupine |
| `Arsa` | Golfo de Cádiz (ARSA) |

En CampR64 estos argumentos se sustituyen por la **ruta del directorio** del área, tomada de `campRoots`.

---

## Campañas implementadas: detalle geográfico y temporal

### 🐟 Cant — Demersales Norte (Cantábrico y Galicia)

| Parámetro | Valor |
|---|---|
| **Zona geográfica** | Plataforma y talud del Cantábrico y Galicia (NW España) |
| **Directorio datos** | `c:/camp/cant/` |
| **Sufijo ficheros** | `*NXX.dbf` (p.ej. `lancN25.dbf` = campaña N25) |
| **Trimestre / meses** | 3º–4º trimestre — septiembre / octubre |
| **Nº lances aprox.** | ~120 lances |
| **Divisiones ICES** | **8c** (Cantábrico E y Galicia S) + **9a** (Galicia W) |
| **DNS RODBC (CampR)** | `Cant` |
| **Tipo campaña** | Demersal — arrastre de fondo, diseño estratificado |
| **Duración estándar lance** | **30 min** lances estándar · **40 min** lances especiales de profundidad (el arte tarda más en asentarse en el fondo) |

### 🌊 Porc — Banco de Porcupine

| Parámetro | Valor |
|---|---|
| **Zona geográfica** | Banco de Porcupine, al oeste de Irlanda (Atlántico NE) |
| **Directorio datos** | `c:/camp/Porc/` |
| **Sufijo ficheros** | `*PXX.dbf` |
| **Trimestre / meses** | ~3º trimestre — septiembre |
| **Nº lances aprox.** | ~80 lances |
| **Divisiones ICES** | **7c** + **7b** + **7k** (zona Porcupine: `7cbk`) |
| **DNS RODBC (CampR)** | `Porc` |
| **Tipo campaña** | Demersal profundo — arrastre de fondo, talud atlántico |
| **Duración estándar lance** | **30 min** |

### 🌅 Arsa — Golfo de Cádiz (ARSA)

| Parámetro | Valor |
|---|---|
| **Zona geográfica** | Golfo de Cádiz (SW España, Atlántico S) |
| **Directorio datos** | `c:/camp/arsa/` |
| **Sufijo ficheros** | `*1XX.dbf` (1er trim.) / `*2XX.dbf` (4º trim.) |
| **Trimestres / meses** | **1xx:** 1º trimestre (febrero–marzo) · **2xx:** 4º trimestre (octubre–noviembre) |
| **Nº lances aprox.** | ~40–44 lances por campaña |
| **Divisiones ICES** | **9a** (Golfo de Cádiz) |
| **DNS RODBC (CampR)** | `Arsa` |
| **Tipo campaña** | Demersal — dos campañas anuales (primavera e invierno) |
| **Duración estándar lance** | **60 min** |

### 🌿 Medi — Mediterráneo

| Parámetro | Valor |
|---|---|
| **Directorio datos** | `c:/camp/medi/` |
| **Sufijo ficheros** | `*MXX.dbf` |
| **Tipo campaña** | MEDITS — arrastre demersal Mediterráneo |

---

## Corrección por duración de lance: `cor.time` y `weight.time`

`weight.time` es el **ratio duración real / duración estándar** del lance, almacenado en `lanceXXX.dbf`. Permite estandarizar las capturas a la duración nominal independientemente de si el lance fue más corto o más largo de lo previsto.

La corrección (`cor.time = TRUE`) se aplica dividiendo capturas por `weight.time`:

```r
mm$peso   <- mm$peso   / mm$weight.time
mm$numero <- mm$numero / mm$weight.time
```

### Duraciones estándar por zona y tipo de lance

| Zona | Tipo lance | Duración estándar | Notas |
|---|---|---|---|
| Cant (Cantábrico/Galicia) | Estándar | 30 min | |
| Cant (Cantábrico/Galicia) | Especial profundidad | 40 min | Arte tarda más en asentarse |
| Porc (Porcupine) | Estándar | 30 min | |
| Arsa (Golfo de Cádiz) | Estándar | 60 min | |

### Comportamiento en `datos.camp64()`

| Parámetro | Efecto |
|---|---|
| `incl2 = FALSE` (por defecto) | Solo lances estándar — abundancia estratificada ponderada por área de estrato (IBTS) |
| `incl2 = TRUE` | Todos los lances incluidos especiales — suma directa sin ponderar por área (`arsect = 1`) |
| `cor.time = TRUE` (por defecto) | Aplica corrección por duración en **ambas** ramas (`incl2 = FALSE` e `incl2 = TRUE`) |
| `cor.time = FALSE` | Sin corrección — capturas brutas tal como están en el DBF |

> **Nota:** `cor.time` se aplica siempre en las campañas N83 y N84 independientemente del valor del parámetro, por particularidades históricas de esas campañas.

---

### 📐 Distribuciones de tallas y pesos

| Función | Descripción |
|---|---|
| `dtall.camp.r` / `dtall.campa.r` | Distribuciones de tallas campaña / agrupadas |
| `dtall.lan.r` | Distribución de tallas por lance |
| `dtallan.camp.r` / `dtallan.peso.r` | Tallas por lance (número / peso) |
| `dtallbarplot.r` | Barplot de distribución de tallas |
| `dtallboxplot.camps.R` | Boxplot de tallas por campañas |
| `dtallmean.camp.R` | Tallas medias de campaña |
| `denstal.camp.r` / `denstall.camp.r` | Densidad de tallas |

---

### 📈 Estadística y coeficientes de variación

| Función | Descripción |
|---|---|
| `CV.bt.camp.r` | CV bootstrap campaña |
| `CV.camp.r` | CV por campaña |
| `CV.camps.r` | CV múltiples campañas |
| `CVlist.camp.r` | Lista de CVs |
| `CVtal.bt.camp.R` | CV tallas bootstrap |
| `CVtal.camp.R` | CV tallas |
| `strmean.r` | Media estratificada |
| `strmean.camps.r` | Media estratificada campañas |
| `strmean.dt.r` / `strmean.dtt.r` | Media estratificada con detalles |
| `MeanMaxL.camp.r` / `MeanMaxL.camps.r` | Talla media máxima |
| `p95tal.camp.r` / `P95talesps.camp.r` | Percentil 95 tallas (campaña/especies) |

---

### 🐟 Fauna, especies y listas taxonómicas

| Función | Descripción |
|---|---|
| `ListFauna.camp.r` / `ListFauna.camps.r` | Lista de fauna (campaña/campañas) |
| `ListFauna.groups.R` | Lista de fauna por grupos |
| `ListFauna.lan.R` / `ListFauna.lans.R` | Lista de fauna por lance/lances |
| `ListFaunaTals.camp.r` / `ListFaunaTals.camps.r` | Fauna con tallas |
| `ListFaunaTals.groups.R` | Fauna con tallas por grupos |
| `Fauna.camp.r` | Datos generales de fauna |
| `BuscaAphia.R` | Búsqueda código Aphia (WoRMS) |
| `buscacod.r` | Búsqueda por código |
| `buscaesp.r` / `buscaesp.datras.R` | Búsqueda de especie / en DATRAS |
| `AbrvEsp.R` | Abreviaturas de especies |
| `AbAgStatRec.camp.R` | Estadísticas de registros por especie |
| `TabAbsEsp.camp.r` | Tabla de abundancias por especie |
| `PresenciaEsp.camp.R` | Presencia de especies |
| `SacaAphialD.r` | Extracción ID Aphia |
| `hmmSAC.R` | Modelos HMM SAC |

---

### 🦐 Nephrops (cigala) por unidades funcionales

| Función | Descripción |
|---|---|
| `NepFU25.camp.R` | Nephrops FU25 |
| `NepFU26.camp.R` | Nephrops FU26 |
| `NepFU30.camp.R` | Nephrops FU30 |
| `NepFU31.camp.R` | Nephrops FU31 |
| `NepFUs.camp.R` | Nephrops todas las FUs |
| `VedaCigala.R` / `VedaCigala.out.R` | Análisis veda cigala |

---

### 🐬 Cetáceos / Marsopa (Phocoena phocoena)

| Función | Descripción |
|---|---|
| `MapPorc.R` / `mapporco.r` | Mapas de distribución |
| `PorcNWSAC.R` | Marsopa NW SAC |
| `PorcShelfSAC.R` | Marsopa Shelf SAC |
| `PorcSWSAC.R` | Marsopa SW SAC |
| `NewPorcCanyon.R` | Marsopa Canyon (nueva) |

---

### 🐟 Besugo / Peces específicos

| Función | Descripción |
|---|---|
| `VedasBesugo.R` | Análisis vedas besugo |
| `CAMPtoDeepShark.R` | Conversión campaña a formato Deep Shark |

---

### 🗝️ Claves Talla-Edad (ALK)

| Función | Descripción |
|---|---|
| `ALKS.dns.camp.R` | ALK densidad campaña |
| `GetAlk.camp.r` | Obtener ALK |
| `ecolgr.camp.r` | Ecología por grupos |
| `ecolmatrix.camp.r` | Matriz ecológica |
| `edadsect.camp.r` / `edadstr.camp.r` | Edad por sector / estrato |

---

### 📉 Gráficos y visualización

| Función | Descripción |
|---|---|
| `grafedtal.camp.r` / `grafedtal.camps.R` | Gráficos edad-talla |
| `grafhistbox.r` / `grafhistbox.comp.r` | Histograma + boxplot (simple/compuesto) |
| `grafhisttalbox.r` | Histograma tallas + boxplot |
| `histboxplot.R` / `histboxplot.comp.R` | Histograma boxplot (simple/compuesto) |
| `talbox.camp.R` | Boxplot de tallas campaña |
| `talpes.camp.r` | Talla-peso gráfico campaña |
| `bubbage.camp.r` | Gráfico de burbujas por edad |
| `logabage.camp.r` / `logabage2.camp.r` | Log-abundancia por edad |

---

### 🔧 Artes de pesca

| Función | Descripción |
|---|---|
| `ArteParCompC.R` / `ArteParCompD.R` / `ArteParCompV.R` | Comparación parámetros de arte (C/D/V) |
| `ArtePars.R` | Parámetros de artes de pesca |
| `pasa.arte.r` | Pasos de arte |
| `pasa.lan.r` | Pasos de lance |
| `unid.camp.r` | Unidades de campaña |

---

### 🧭 Conversiones de coordenadas

El programa CAMP (Harbour/Clipper) almacena coordenadas en formato propio GGG.MMmm
(parte entera = grados, decimales = minutos decimales /100). Los CTDs registran posición
en otro formato que requiere conversión específica.

| Función | De → A | Notas |
|---|---|---|
| `agradec.R` | Grados decimales → GGG.MMmm | Nombre: **A**→**GRA**dos **DEC**imales. Formato de entrada para funciones CAMP. Inversa de `gradec()` |
| `gradec.r` | GGG.MMmm → grados decimales | Formato de salida estándar para SIG/mapas. Inversa de `agradec()` |
| `gradms.R` | Grados decimales → GG°MM'SS" | Grados, minutos y segundos |
| `decgrad.R` | Grados decimales → GG°MM' | Grados y minutos decimales |
| `hidrotodec.R` | Formato CTD/hidrográfico → grados decimales | Convierte las coordenadas almacenadas en `hidroXXX.dbf` (registradas por los CTDs durante la campaña) a grados decimales para análisis espacial y mapas |
| `HdectoH.R` | Hexadecimal → decimal | Uso interno CAMP |
| `HextoDec.R` | Hexadecimal → decimal | Variante de `HdectoH` |

---

### 🔍 Control de calidad (QC)

| Función | Descripción |
|---|---|
| `map.check.r` | Verificación de mapas |
| `mark.gaps.r` | Marcado de huecos/gaps |
| `MatchHidro.ctd.R` | Verificación CTD vs hidro |
| `qcLanDatr.camp.R` | QC lances DATRAS |
| `qcLW.camp.r` / `qcLWbucl.camp.r` | QC relación longitud-peso |
| `qcTalPez.camp.r` / `qcTalPez.lan.r` | QC tallas de peces |
| `qcdistlan.camp.r` | QC distancia de lances |
| `qcdtall.camp.r` / `qcdtallrev.camp.r` | QC distribución de tallas (normal/revisado) |

---

### 🗂️ Formatos de intercambio (DATRAS/ICES/HH/HL)

| Función | Descripción |
|---|---|
| `CAMPtoHH.R` / `CAMPtoHHnw.R` | Conversión campaña → formato HH (HH/HH-nuevo) |
| `CAMPtoHL.R` / `CAMPtoHLnw.R` | Conversión campaña → formato HL |
| `CAMPtoHL_crust.R` | Conversión HL crustáceos |
| `CAMPtoHL_molus.R` | Conversión HL moluscos |
| `getICESarea.R` | Obtener área ICES |
| `mapICESStatRec.R` / `mapICESStatRec_plotrix.R` | Mapa rectángulos estadísticos ICES |
| `Ian0.camp.r` / `Ian2.camp.r` | Índices Ian0 / Ian2 |

---

### 📌 Cuadrículas y sorteo de estaciones

| Función | Descripción |
|---|---|
| `sacagrid.r` / `sacagridNort.r` | Cuadrícula de sorteo (general/Norte) |
| `datagegr.camp.r` | Datos geográficos de cuadrícula |
| `sorteo.r` | Sorteo de estaciones |
| `captdia.camp.R` | Captura diaria |

---

### 🔢 Biología y modelos

| Función | Descripción |
|---|---|
| `As1.R` / `As2.R` | Análisis de supervivencia (modelos 1/2) |
| `BelgicaMound.R` | Análisis Bélgica Mound |
| `binom.r` | Distribución binomial |
| `propmat.camp.r` | Proporción de maduros |
| `sexr.camp.R` | Sex ratio campaña |
| `sefos.camp.R` | SEFOS campaña |
| `Msh.r` / `Nsh.r` | Modelos Msh / Nsh |
| `PrSh.R` / `Psh.r` | Modelos PrSh / Psh |
| `cefsps.R` | CEFS por especies |

---

### 🛠️ Utilidades generales

| Función | Descripción |
|---|---|
| `CephDataByH.R` | Datos cefalópodos por hora |
| `DpthPrfl.r` / `DpthPrflTals.r` | Perfil de profundidad / tallas |
| `logabage.camp.r` | Log-abundancia por edad campaña |
| `mark.gaps.r` | Marcado de gaps temporales |

---

## Repositorios GitHub

| Repositorio | URL | Contenido |
|---|---|---|
| **CampR** | https://github.com/Franvgls/CampR | Versión 32 bits, R ≤ 3.6.3, RODBC |
| **CampR64** | https://github.com/Franvgls/CampR64 | Versión 64 bits, R ≥ 4.0.4, `foreign` |
| **Organización** | https://github.com/franvgls | Todos los repositorios del autor |

---

## Base de datos: programa CAMP (Clipper → Harbour)

El programa **CAMP** (*Sánchez & Fernández, 1998, Manual del programa CAMP v2.10*) gestiona los datos de campaña. Fue desarrollado originalmente en **Clipper** (MS-DOS) y actualizado para funcionar en Windows moderno mediante **Harbour** (compilador open-source compatible con Clipper).

### Formato de datos: dBase III (DBF)

Los datos se almacenan en ficheros `.dbf` formato **dBase III**. Los ficheros generados por CAMP/Clipper llevan en la cabecera un **código de versión no estándar** que puede causar problemas de lectura.

#### Ficheros por campaña (en cada subdirectorio de zona)

| Fichero | Contenido |
|---|---|
| `campXXX.dbf` | Datos generales de campaña |
| `lanceXXX.dbf` | Datos de lances |
| `faunaXXX.dbf` | Datos de fauna/capturas |
| `ntallXXX.dbf` | Datos de distribución de tallas |
| `hidroXXX.dbf` | Datos hidrográficos |
| `edadXXX.dbf` | Datos de edades (claves talla-edad) |

#### Fichero maestro (directorio raíz)

| Fichero | Contenido |
|---|---|
| `especies.dbf` | Maestro de especies (directorio raíz `c:/camp/`) |

### Nomenclatura de ficheros por zona

| Zona | Directorio | Sufijo ficheros | Ejemplo |
|---|---|---|---|
| Demersales Norte (Cantábrico/Galicia) | `c:/camp/cant/` | `*NXX.dbf` | `lanN25.dbf` |
| Banco de Porcupine | `c:/camp/Porc/` | `*PXX.dbf` | `faunaP10.dbf` |
| Golfo de Cádiz (ARSA) | `c:/camp/arsa/` | `*2XX.dbf` / `*1XX.dbf` | `ntall220.dbf` |
| Mediterráneo | `c:/camp/medi/` | `*MXX.dbf` | `campM15.dbf` |

---

## Instalación y configuración

### Diferencias clave CampR vs CampR64

| Aspecto | CampR | CampR64 |
|---|---|---|
| Versión R | Máximo R 3.6.3 (32 bits) | R 4.x o posterior (64 bits) |
| Lectura DBF | Driver ODBC (`odbcad32.exe`) + DNS | Paquete `foreign` vía `readCampDBF()` |
| Configuración DNS | Requerida (Panel de Control Windows) | **No requerida** |
| Reparación cabecera DBF | Necesaria siempre | Solo si R lanza error "not a DBF file" |
| Zonas implementadas | Cant, Porc, Arsa, Medi | Cant, Porc, Arsa, Medi |

### Reparación de cabecera DBF: `fixupdyr.exe`

Cuando `foreign` rechaza un fichero con el error:
```
Error in read.dbf("edadn25.dbf") : edadn25.dbf is not a DBF file
```
hay que reparar la cabecera con `fixupdyr.exe` (actualiza el byte de versión a dBase III válido):

```cmd
cd c:\camp\cant
fixupdyr.exe edadn25.dbf
```

**Recomendado:** añadir `fixupdyr.exe` al PATH del sistema para usarlo desde cualquier directorio.  
La reparación es permanente; una vez reparado el fichero no necesita repetirse.

### Sistema de rutas: `campRoots`

CampR64 utiliza una estructura `campRoots` con rutas configurables por entorno:

```r
campRoots$local$cant     # c:/camp/cant/
campRoots$local$porc     # c:/camp/Porc/
campRoots$local$arsa     # c:/camp/arsa/
campRoots$local$medi     # c:/camp/medi/
campRoots$local$especies # c:/camp/especies.dbf
```

Las rutas por defecto del paquete pueden sobrescribirse mediante un **archivo de configuración del usuario**, lo que permite trabajar en distintos entornos (local, servidor) sin modificar el código del paquete.

### Listar campañas disponibles

```r
library(CampR64)
CampsDNS.camp(path = "c:/camp/cant/")   # Cantábrico/Galicia
CampsDNS.camp(path = "c:/camp/Porc/")   # Porcupine
CampsDNS.camp(path = "c:/camp/arsa/")   # Golfo de Cádiz
CampsDNS.camp(path = "c:/camp/medi/")   # Mediterráneo
```

---

## Notas técnicas importantes

- Las funciones con sufijo `.camps.r` (plural) operan sobre **múltiples campañas**; las que terminan en `.camp.r` (singular) operan sobre **una campaña**.
- Las versiones `64` (en CampR64) reemplazan `RODBC` por el paquete `foreign` + función interna `readCampDBF()`.
- El archivo `config.R` centraliza rutas, nombres de campaña y parámetros globales; es el primer archivo a revisar al iniciar trabajo.
- `z_imports.R` gestiona todas las dependencias del paquete.
- La función `datgr.camp64.r` es el equivalente 64 bits de `datgr.camp.r` y es clave en la migración.
- El sistema de rutas `campRoots` es el mecanismo central para localizar los DBF; personalizar antes de usar si las rutas difieren de `c:/camp/`.
- El programa CAMP (compilado con Harbour, sucesor de Clipper) sigue siendo el sistema de entrada de datos de campo; genera los `.dbf` que R lee.
- `fixupdyr.exe` debe estar accesible (PATH) cuando se trabaja con DBF de edades u otros ficheros con cabecera no estándar.

---

*Documento generado el 2026-03-17 como recordatorio de contexto para sesiones de trabajo con Claude AI.*
