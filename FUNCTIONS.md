# Catalogo de funciones — CampR64

Generado automaticamente por `tools/build_index.R`. No editar a mano.

Total funciones documentadas: **132**

## abunds (2)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `histboxplot64()` | histboxplot64.R | Gráficos de boxplot para la serie histórica incluyendo lances especiales o no y con rangos de profundidad | si | si |
| `histboxplot64.comp()` | histboxplot64.comp.R | Gráficos de boxplot combinados biomasa y número para la serie histórica | si | si |

## ALK (2)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `GrafAlk64.camp()` | grafALK64.camp.R | Clave talla-edad gráfica (Age-Length Key) | si | si |
| `grafedtal.camp64()` | grafedtal.camp64.R |  Histograma de distribución de tallas con edades  | si | si |

## Artes de pesca (2)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `ArteParComp64()` | ArteParComp64.R | Comparación de parámetros del arte entre CAMP IEO y DATRAS | si | si |
| `ArtePars64()` | ArtePars64.R | Gráficos de parámetros del arte con la profundidad | si | si |

## Calculos bootstraps (1)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `strmean.dt64()` | strmean.dt64.R | Medias, SE y CV intraestrato y estratificados por estrato geográfico y batimétrico | si | no |

## Calculos internos no mostrado (3)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `strmean.dtt64()` | strmean.dtt64.r | Medias intraestrato y estratificadas por estratos geográfico y batimétrico | si | no |
| `strmean64()` | strmean64.R | Media estratificada total del area de una especie | si | no |
| `strmean64.camps()` | strmean64.camps.r | Media estratificada total del área de la especie X en varias campañas | si | no |

## Cartografía (25)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `armap.camp64()` | armap.camp64.R | *(sin @title)* | si | no |
| `armap.tot64()` | armap.camp64.R | *(sin @title)* | si | no |
| `armap64()` | armap64.R | Mapa de desarrollo de campaña — función unificada para cant, porc y arsa | si | si |
| `arMapLansGPS64()` | arMapLansGPS64.R | Combinación en dos mapas de armap.camp con etiquetas de los lance y mapa con los lances en segmentos | si | si |
| `GrafMarksGPS()` | GrafMarksGPS64.R | Gráficos de dragas o ctds a partir de las marcas del pescawin | si | si |
| `IBTSNeAtl_map64()` | IBTSNeAtl_map64.R | Function IBTSNeAtl_map plots the map with all the NeAtl IBTS surveys | si | si |
| `maparea64()` | maparea64.r | Mapa general del banco de Porcupine | si | no |
| `MapArsa64()` | MapArsa64.R | Mapa base Golfo de Cádiz (ARSA) con selección por regiones (modelo MapNort64) | si | no |
| `MapEcol64.camp()` | MapEcol64.camp.R | Mapa de índices ecológicos para un grupo en una campaña | si | si |
| `maphist64()` | maphist64.R | Mapas de distribucion en varias campañas | si | si |
| `maphistage64()` | maphistage64.R | Mapa distribución por edad en la campaña | si | si |
| `maphistal64()` | maphistal64.r | Mapa distribución entre tallas tmin y tmax | si | si |
| `MapIberia64()` | MapIberia64.R | Mapa de la Península Ibérica completo | si | si |
| `MapIBTS64()` | MapIBTS64.R | Map species distribution from IBTS NeAtl surveys (R 4.4+) | si | si |
| `MapLansGPS64()` | MapLansGPS64.r | Gráfico con los lances en segmentos | si | si |
| `MapMedit64()` | MapMedit64.R | Mapa del Mediterráneo Ibérico completo | si | si |
| `MapNort64()` | MapNort64.R | Mapa base Mar Cantábrico y costa norte de España | si | no |
| `MapPorc64()` | MapPorc64.R | Mapa base Banco de Porcupine | si | no |
| `mapsorteo64()` | mapsorteo64.R | Mapa de las cuadrículas sorteadas para la campaña Porcupine | si | no |
| `MatchHidro.ctd64()` | MatchHidro.ctd64.R | Mapa de estaciones hidrológicas (CTDs) en una campaña | si | si |
| `NepFU25.camp64()` | NepFU25.camp64.R | Información anual sobre cigala para la FU 25 Galicia norte | si | si |
| `NepFU26.camp64()` | NepFU26.camp64.R | Información anual sobre cigala por FUs en el Cantábrico y Galicia | si | si |
| `NepFU30.camp64()` | NepFU30.camp64.R | Información anual sobre cigala para la FU 30 en el Golfo de Cádiz | si | si |
| `NepFU31.camp64()` | NepFU31.camp64.R | Información anual sobre cigala en la FU31 en el Cantábrico | si | si |
| `NepFUs.camp64()` | NepFUs.camp64.R | Información anual sobre cigala por FUs en el Cantábrico y Galicia | si | si |

## Control de calidad (8)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `qcdistlan.camp64()` | qcdistlan.camp64.r | Comprueba la distancia recorrida en los lances y la consistencia con recorrido y la velocidad en los datos del CAMP | si | si |
| `qcdtall64.camp()` | qcdtall64.camp.R | Revisión del histograma de distribución de tallas | si | si |
| `qcdtallrev64.camp()` | qcdtallrev64.camp.R | Revisión de los histogramas de tallas de todas las especies de una campaña | si | si |
| `qcDupFauna64.camp()` | qcDupFauna64.camp.R | Control de calidad: especies duplicadas en las faunisticas de una campaña | si | si |
| `qclandatr.camp64()` | qcLanDatr.camp64.R | Comprueba la distancia recorrida en los lances y la consistencia con recorrido y la velocidad en los datos del CAMP | si | si |
| `qcLW64.camp()` | qcLW64.camp.R | Outliers capturas según tallas medidas y relación talla-peso | si | si |
| `qcTalPez64.camp()` | qcTalPez64.camp.R | Comprueba que están medidas todas las especies en capturas y viceversa en la campaña | si | si |
| `qcTalPez64.lan()` | qcTalPez64.lan.R | Comprueba concordancia entre capturas y tallas en un lance | si | si |

## Conversión de unidades (4)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `agradec()` | agradec.R | Conversión de grados decimales a grados y minutos decimales | si | si |
| `gradec()` | gradec.R | Transforma grados y minutos decimales a grados decimales | si | si |
| `gradms()` | gradms.R | Transforma grados, minutos y segundos a grados decimales | si | si |
| `HdectoH()` | HdectoH.R | Transforma hora decimal en formato HH.NN a hora hexadecimal HH.MM | si | si |

## Cuadriculas/sorteo (3)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `sacagrid64()` | sacagrid64.R | Saca el grid de cuadrículas para Porcupine | si | no |
| `sacagridNort64()` | sacagridNort64.R | Saca el grid de cuadrículas para Demersales Norte | si | no |
| `sorteo64()` | sorteo64.R | Sorteo de distribución de lances en Porcupine | si | no |

## Datos de campana (13)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `datab64()` | datab64.R | Datos estratificados por sector para una especie para grupos de trabajo | si | no |
| `databEstr64()` | databEstr64.R | Salida con abundancias estratificadas por estrato de profundidad | si | si |
| `databICES64()` | databICES64.r | Datos de abundancia por división ICES para una especie resúmen para grupos de trabajo | si | no |
| `databICESdiv64()` | databICESDiv64.R | Datos de abundancia por división ICES para una especie resúmen para grupos de trabajo | si | no |
| `datCatches.camp64()` | datCatches.camp64.R | Crea datos de capturas en formato para evaluaciones tipo Capros o modelos bayesianos (completar con datTalCatch.camp) | si | si |
| `datgr.camp64()` | datgr.camp64.r | Datos de biomasa y abundancia para una especie y campaña | si | si |
| `dathidro.camp64()` | dathidro.camp64.R | Datos hidrográficos de una campaña | si | si |
| `datlan.camp64()` | datlan.camp64.R | Características del lance | si | si |
| `datos.camp64()` | datos.camp64.R | Datos de abundancia y biomasa de una especie  | si | si |
| `dattal.camp64()` | dattal.camp64.R | Abundancia estratificada por talla y sexo | si | si |
| `dattal.camps64()` | dattal.camps64.r | Abundancia estratificada para un rango de talla | si | si |
| `datTalCatch64.camp()` | datTalCatch64.camp.R | Crea datos de capturas en formato para evaluaciones tipo Capros o modelos bayesianos (completar con datCatches.camp) | si | si |
| `dattalgr.camp64()` | dattalgr.camp64.r | Biomasa y abundancia para un rango de talla | si | si |

## Datos especies (5)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `AbrvEsp64()` | AbrvEsp64.R | Crea abreviaturas de especies a partir del nombre científico, género+especie | si | si |
| `BuscaAphia64()` | BuscaAphia64.R | Recupera los AphiaId de las especies sin Aphiaid en el fichero especies.dbf | si | si |
| `buscacod64()` | buscacod64.R | Función de búsqueda del código del grupo y la familia | si | si |
| `buscaesp64()` | buscaesp64.R | Función de búsqueda del nombre de una especie | si | si |
| `camptoyear()` | camptoyear.R | Transforma series de nombres de campaña en años | si | si |

## Distribuciones batimétricas (2)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `CoGDpth.sp64.camp()` | CoGDpth.sp64.camp.R | Centro de gravedad batimétrico de una especie a lo largo de una serie de campañas | si | si |
| `CoGLng.sp64.camp()` | CoGLng.sp64.camp.R | Centro de gravedad batimétrico de una especie a lo largo de una serie de campañas | si | si |

## Distribuciones de tallas (9)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `denstall64.camp()` | denstall64.camp.r | Función de densidad distribución de tallas | si | si |
| `dtall.camp64()` | dtall.camp64.r | Crea un histograma distribución de tallas estratificada especie y campañas | si | si |
| `dtallan.camp64()` | dtallan.camp64.R | Valores absolutos en número por talla | si | si |
| `dtallbarplot64()` | dtallbarplot64.r | Histograma de la distribución de tallas media de una o varias campañas | si | si |
| `dtalldens64.camp()` | dtalldens64.camp.R | Histograma y densidad de tallas combinados | si | si |
| `dtallmean64.camp()` | dtallmean64.camp.R | **Talla media** de la distribución de tallas media estandarizada del conjunto de la campaña **Xxx** | si | si |
| `dtallmean64.serie()` | dtallmean64.serie.R | Distribución de tallas media de una serie de campañas | si | si |
| `talbox64.camps()` | talbox64.camp.R | Abundancia estratificada para un rango de talla | si | si |
| `talpes64.camp()` | talpes64.camp.R | Parámetros a y b de la relación talla-peso (CampR64) | si | no |

## Distribuciones geográficas (2)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `CoGLat.sp64.camp()` | CoGLat.sp64.camp.R | Centro de gravedad latitudinal de una especie a lo largo de una serie de campañas | si | si |
| `CoGmap.sp64.camp()` | CoGmap.sp64.camp.R | Centro de gravedad geográfico de una especie sobre el mapa, con deriva temporal | si | si |

## Ecología (2)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `ecolgr.camp64()` | ecolgr.camp64.R | Índices ecológicos para un grupo en una campaña | si | si |
| `ecolmatrix.camp64()` | ecolmatrix.camp64.r | Extrae los datos del FAUNA de una especie en concreto.  | si | si |

## edades (9)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `AbAgStatRec.camp64()` | AbAgStatRec.camp64.R | Calcula las abundancias estratificadas por edad y hora en cada rectángulo ICES de la zona considerada | si | si |
| `ALKs64.dns.camp()` | ALKs64.dns.camp.R | Datos de claves talla-edad en campañas (CampR64) | si | si |
| `bubbage64.camp()` | bubbage64.camp.r | Gráfico de burbujas de abundancia por edad y campaña | si | si |
| `datagegr.camp64()` | datagegr.camp64.R | Extrae los datos del FAUNA de una especie en concreto en una edad.  | si | no |
| `edadsect.camp64()` | edadsect.camp64.R | Calcula las abundancias estratificadas por edad | si | si |
| `edadstr.camp64()` | edadstr.camp64.r | Calcula las abundancias estratificadas por edad  | si | si |
| `GetAlk.camp64()` | GetAlk.camp64.R | Extrae ALK para una especie de los ficheros del camp  | si | si |
| `grafedtal.camps64()` | grafedtal.camps64.R |  Histograma de distribución de tallas con edades para varias campañas | si | si |
| `logabage64.camp()` | logabage64.camp.r | Descenso logarítmico de abundancia por cohorte | no | si |

## Estadistica/CV (4)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `CV.bt.camp64()` | CV.bt.camp64.r | Abundancias, SE y coeficientes de variación paramétricos calculados con bootstrap | si | no |
| `CV.camp64()` | CV.camp64.R | Abundancias, SE y coeficientes de variación paramétricos por subestrato, estrato y total | si | no |
| `CV.camps64()` | CV.camps64.R | Abundancias en número y peso totales de la especie para varias campañas | si | no |
| `CV.camps64_full()` | CV.camps64_full.R | Estadísticos estratificados completos (media, SE, CV) para varias campañas | si | no |

## Faunísticas (3)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `Fauna.camp()` | Fauna.camp64.R | Listado de especies por campaña sin  datos de las especies del grupo, sólo nombres permite añadir los lances especiales | si | si |
| `ListFauna.camp64()` | ListFauna.camp64.R | Capturas medias por lance de cada especie de gr (grupo) en una campaña  | si | si |
| `ListFauna.lan64()` | ListFauna.lan64.r | Capturas de un lance específico | si | si |

## Formatos DATRAS (5)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `CAMPtoHH64()` | CamptoHH64.R | Exporta datos de formato CAMP a formato DATRAs HH | si | si |
| `CAMPtoHHnw64()` | CAMPtoHHnw64.R | Exporta datos de formato CAMP a formato DATRAs HH | si | si |
| `CAMPtoHL64()` | CAMPtoHL64.R | Exporta datos de formato CAMP a formato DATRAs HL. Depende de que los códigos Aphia estén correctos en especies.dbf da error si son incompletos | si | si |
| `CAMPtoHLnw64()` | CAMPtoHLnw64.R | Exporta datos de formato CAMP a formato DATRAs HL. Depende de que los códigos Aphia estén correctos en especies.dbf da error si son incompletos | si | si |
| `getICESarea64()` | getICESarea64.R | StatRec y División ICES (Area) para una campaña — versión estricta sin helpers | si | no |

## Graficos (2)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `grafhistbox64()` | grafhistbox64.R | Evolución en biomasa o abundancia de la especie en la serie histórica | si | si |
| `grafhistbox64.comp()` | grafhistbox64.comp.r | Gráficas grafhistbox combinadas biomasa y peso | si | si |

## Series datos (1)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `CampsDNS.camp64()` | CampsDNS.camp64.R | Listado de campañas presentes en el directorio e información sobre ellas | si | si |

## Shapefiles (3)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `get_shape_path()` | shapes_utils.R | Ruta a un shape interno de CampR64 | si | no |
| `list_shapes()` | shapes_utils.R | Listar shapes disponibles en CampR64 | si | no |
| `read_shape()` | shapes_utils.R | Leer un shape interno de CampR64 como objeto sf | si | no |

## Sin clasificar (19)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `.ices_map_cant()` | utils-ceph.R | *(sin @title)* | no | no |
| `binom64()` | binom64.r | Distribucion binomial: exitos necesarios para ser significativo | si | si |
| `buscaFuncion64()` | buscaFuncion64.R | Busca funciones de CampR64 por palabra clave | si | si |
| `CephDataCallStr64.camp()` | CephDataCall64.camp.R | Crea la salida para el CPUE por lance del Data call de cefalópodos | si | si |
| `configurarCampR64()` | configurarCampR64.R | Configurar rutas de CampR64 | si | no |
| `edit_campR64_paths()` | edit_campR64_paths.R | Editar rutas de datos de CampR64 desde la consola | si | no |
| `fix_dbf_date()` | fix_dbf_date.R | Corregir fechas de DBF con efecto Y2K (años < 1980) | si | no |
| `HextoDec()` | HextoDec.R | Transforma hora hexadecimal en formato HH.MM a hora decimal H. | si | si |
| `lan0.camp()` | lan0.camp.r | Lances nulos en una campaña | si | si |
| `lan2.camp()` | lan2.camp.r | Lances especiales de una campaña | si | si |
| `ListALKs.camp64()` | ListALKs.camp64.R | Listar ALKs (EDAD<camp>.dbf) y especies por campaña, con nº de otolitos | si | no |
| `load_worldHires64()` | load_worldHires64.R | *(sin @title)* | no | no |
| `propmat64.camp()` | propmat64.camp.r | Proporcion en peso de peces mayores de L50 | si | si |
| `readCampDBF()` | readCampDBF.R | Leer un fichero DBF de CampR64 según zona y origen | si | no |
| `sefos64.camp()` | sefos64.camp.R | Crea datos en formato SEFOS a partir de Camp, dns | si | si |
| `set_campR64_root()` | set_campR64_root.R | Establecer la ruta raíz de los datos para CampR64 | si | si |
| `sexr64.camp()` | sexr64.camp.R | Sex ratio por talla de una especie | si | si |
| `unid64.camp()` | unid64.camp.R | Unidades en que se mide una especie | si | no |
| `unload_worldHires64()` | load_worldHires64.R | *(sin @title)* | no | no |

## Utilidades (3)

| Función | Fichero | Título | Export | @examples |
|---|---|---|---|---|
| `CephDataByH64()` | CephDataByH64.R | Crea la salida para el CPUE por lance del Data call de cefalópodos | si | si |
| `DpthPrfl64()` | DpthPrfl64.r | Perfil de distribución por profundidad | si | si |
| `DpthPrflTals64()` | DpthPrflTals64.r | Perfil de distribución por profundidad y tallas | si | si |

