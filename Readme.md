# CampR64

Paquete R para el acceso y análisis de datos de las campañas demersales españolas
(Demersales Norte / Cantábrico, Porcupine, ARSA) en R 64-bit (R 4.4+). Los datos deben estar guardados en la base de datos CAMP en ficheros dbf.

Sustituye al paquete `CampR` (32-bit, RODBC) usando `foreign::read.dbf` para
la lectura de ficheros DBF, ya sin dependencia de controladores ODBC.

---

## Instalación

```r
# Requiere devtools
install.packages("devtools")

devtools::install_github("Franvgls/CampR64")
```

---

## Configuración inicial (obligatoria, una sola vez)

Tras instalar el paquete es necesario indicar dónde están los datos en tu
máquina. Esta configuración se guarda en `%USERPROFILE%\.CampR64\configRoots_user.R` y **sobrevive a reinstalaciones** del paquete — solo hay que hacerlo una vez.

### Entorno local (datos en disco local)

Ajusta las rutas a las de tu máquina:

```r
configurarCampR64(
  env      = "local",
  base     = "c:/camp",
  especies = "c:/camp",
  cant     = "c:/camp/cant",    # Demersales Norte / Cantábrico-Galicia
  porc     = "c:/camp/porc",    # Porcupine
  arsa     = "c:/camp/arsa"     # ARSA
)
```

### Entorno servidor (datos en red, unidad Z:)

```r
configurarCampR64(
  env      = "serv",
  base     = "z:/camp",
  especies = "z:/camp",
  cant     = "z:/camp/datos/norte",
  porc     = "z:/camp/datos/porcupine",
  arsa     = "z:/camp/datos/arsa"
)
```

> **Importante:** las dos llamadas son independientes y se acumulan.
> El orden no importa. Si solo usas un entorno, basta con configurar ese, los demás puedes obviarlos o hacerlos NULL.

### Reiniciar R

Después de configurar, reinicia R para que las rutas se carguen:

```r
# Session → Restart R   (Ctrl+Shift+F10 en RStudio)
```

### Verificar

```r
CampR64_paths
```

Deberías ver las rutas que acabas de configurar en `$local` y/o `$serv`.

---

## Nota sobre ARSA (usuarios de Cádiz)

El centro de Cádiz mantiene un fichero `ESPECIES.DBF` propio que ha divergido
del fichero común. Si tus datos de ARSA usan ese fichero local, indica la ruta
correcta en el parámetro `especies` al configurar el entorno correspondiente,
o consulta con el administrador del paquete.

---

## Reinstalaciones

La configuración de rutas **no se pierde al reinstalar** el paquete, ya que se
guarda fuera del directorio de instalación:

```
C:\Users\<tu_usuario>\.CampR64\configRoots_user.R
```

Si necesitas cambiar alguna ruta, vuelve a llamar a `configurarCampR64()` con
los nuevos valores (solo para el entorno que quieras modificar).

Si quieres borrar toda la configuración y empezar desde cero:

```r
configurarCampR64(env = "local", overwrite = TRUE, ...)
```

---

## Campañas disponibles

| Parámetro `zona` | Campaña                               |
| ---------------- | ------------------------------------- |
| `"cant"`         | Demersales Norte / Cantábrico-Galicia |
| `"porc"`         | Porcupine                             |
| `"arsa"`         | ARSA (Golfo de Cádiz)                 |
| `"medi"`         | MEDITS                                |

---

## Requisitos

- R 4.4 o superior (64-bit)
- Windows (el paquete trabaja con ficheros DBF de bases de datos dBase/Clipper)
- Paquetes CRAN: `foreign`, `sf`, `sp`, `maps`, `mapdata`, `icesDatras`, `worrms`
