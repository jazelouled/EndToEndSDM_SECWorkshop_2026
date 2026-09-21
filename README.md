# Modelos de distribuciÃ³n de especies de principio a fin: CMEMS â†’ predicciones (MÃ¡laga, 2026)

Este repositorio contiene todo el material necesario para ejecutar un flujo de trabajo completo de Modelos de DistribuciÃ³n de Especies (SDM), desde datos brutos de seguimiento hasta predicciones de idoneidad de hÃ¡bitat.

<p align="center">
  <img src="logoWorkshop_Malaga2026_v2.png" width="50%">
</p>

El taller guÃ­a a las personas participantes a travÃ©s de un flujo de trabajo completo:

- procesamiento de datos de seguimiento
- preparaciÃ³n de datos ambientales
- construcciÃ³n de un conjunto de datos de presenciaâ€“ausencia
- ajuste de modelos mediante aprendizaje automÃ¡tico
- predicciÃ³n espacial y temporal

## Primeros pasos

### 1. Clonar el repositorio

```bash
git clone git@github.com:jazelouled/ModelosDistribucionEspecies_CMEMS_Malaga2026.git
cd ModelosDistribucionEspecies_CMEMS_Malaga2026
```

---

### 2. Preparar los datos de entrada

Algunos archivos de entrada de gran tamaÃ±o no estÃ¡n almacenados en el repositorio.

#### BatimetrÃ­a

Descarga el archivo desde:

https://www.dropbox.com/scl/fi/e90xlk0ousn83qkpuwgoh/bathymetry_wmed.tif?rlkey=6dlp2qgayjvbg4hipn22xuo1n&dl=0

ColÃ³calo en:

```
00inputOutput/00input/00rawData/00enviro/00StaticLayers/
```

#### Datos de seguimiento

Descarga el archivo desde:

https://www.dropbox.com/scl/fi/lgr1izxp7ls9jn6waqxen/simulated_tracking_final.csv?rlkey=1hc94drsmj7e8zf04nm4jd6r6&dl=0

ColÃ³calo en:

```
00inputOutput/00input/00rawData/01tracking/
```

#### Datos ambientales

Si la descarga directa desde Copernicus falla, puedes utilizar este enlace, que contiene los mismos datos ambientales:

https://www.dropbox.com/scl/fo/m6f6znub911rg6dixxnzm/AMOo3HL2sxs23Zm6g5YjNEA?rlkey=towzqx8s5o90amo2w2f3l9dev&dl=0

Coloca los archivos en:

```
00inputOutput/00input/00rawData/00enviro/01CMEMS
```

#### Datos del SSM

El script del modelo de espacio de estados (SSM) es computacionalmente exigente y puede que tu ordenador no consiga ejecutarlo. Para poder continuar con los pasos posteriores, descarga esta carpeta:

https://www.dropbox.com/scl/fo/yk7u4gmf0hf8unbao4v5b/AJRMJD9pZlprMgBT0YbDtQY?rlkey=maai7eo35mv8t2pl7cbjpgocb&dl=0

ColÃ³cala en:

```
00inputOutput/00input/01processedData/01tracking/04L2_ssm_behaviour
```

La estructura esperada es:

```
00inputOutput/
â””â”€â”€ 00input/
    â””â”€â”€ 00rawData/
        â”œâ”€â”€ 00enviro/
        â”‚   â””â”€â”€ 00StaticLayers/
        â”‚       â””â”€â”€ bathymetry_wmed.tif
        â”‚
        â””â”€â”€ 01tracking/
            â”œâ”€â”€ simulated_tracking_final.csv
            â””â”€â”€ 00auxiliaryFiles/
                â”œâ”€â”€ bbox_env.txt
                â””â”€â”€ tracking_dates.txt
```

---

## Estructura del proyecto

```
ModelosDistribucionEspecies_CMEMS_Malaga2026/
â”‚
â”œâ”€â”€ README.md
â”‚
â”œâ”€â”€ 00inputOutput/
â”‚   â”œâ”€â”€ 00input/
â”‚   â”‚   â”œâ”€â”€ 00rawData/
â”‚   â”‚   â”‚   â”œâ”€â”€ 00enviro/
â”‚   â”‚   â”‚   â”‚   â”œâ”€â”€ 00StaticLayers/
â”‚   â”‚   â”‚   â”‚   â”œâ”€â”€ 01CMEMS/
â”‚   â”‚   â”‚   â”‚   â””â”€â”€ oceanmask.tif
â”‚   â”‚   â”‚   â”‚
â”‚   â”‚   â”‚   â””â”€â”€ 01tracking/
â”‚   â”‚   â”‚       â””â”€â”€ 00auxiliaryFiles/
â”‚   â”‚   â”‚
â”‚   â”‚   â””â”€â”€ 01processedData/
â”‚   â”‚       â”œâ”€â”€ 00enviro/
â”‚   â”‚       â”‚   â”œâ”€â”€ 02presentStacks/
â”‚   â”‚       â”‚   â””â”€â”€ 03futureStacks/
â”‚   â”‚       â”‚
â”‚   â”‚       â””â”€â”€ 01tracking/
â”‚   â”‚           â”œâ”€â”€ 00L0_data/
â”‚   â”‚           â”œâ”€â”€ 02L1_douglas/
â”‚   â”‚           â”œâ”€â”€ 03L1_spaceTimeSplit/
â”‚   â”‚           â”œâ”€â”€ 04L2_ssm_behaviour/
â”‚   â”‚           â””â”€â”€ 06PresAbs_grid/
â”‚   â”‚
â”‚   â””â”€â”€ 01output/
â”‚       â”œâ”€â”€ 00figures/
â”‚       â”œâ”€â”€ 01rasters/
â”‚       â”œâ”€â”€ 02models/
â”‚       â””â”€â”€ 03tables/
â”‚
â”œâ”€â”€ 01scripts/
â”‚   â”œâ”€â”€ 00_main.R
â”‚   â”‚
â”‚   â”œâ”€â”€ 00enviro/
â”‚   â”‚   â”œâ”€â”€ 00_oceanMask.R
â”‚   â”‚   â”œâ”€â”€ 01_downloadCMEMS.R
â”‚   â”‚   â”œâ”€â”€ 01_downloadCMEMS.sh
â”‚   â”‚   â”œâ”€â”€ 03_prepareStaticLayers.R
â”‚   â”‚   â”œâ”€â”€ 04_prepareCMEMS.R
â”‚   â”‚   â””â”€â”€ 06_buildPresentStack.R
â”‚   â”‚
â”‚   â”œâ”€â”€ 01tracking/
â”‚   â”‚   â”œâ”€â”€ 00_L0_read_and_standardize_Balaenoptera_artificialis_tracking.R
â”‚   â”‚   â”œâ”€â”€ 02_L1_douglas_speed_filter_Balaenoptera_artificialis_from_L0.R
â”‚   â”‚   â”œâ”€â”€ 03_L1_spacetime_split_Balaenoptera_artificialis.R
â”‚   â”‚   â”œâ”€â”€ 04_L2_ssm_by_segment_Balaenoptera_artificialis_QC_routePath.R
â”‚   â”‚   â”œâ”€â”€ 05_simulations_tracks_Balaenoptera_artificialis.R
â”‚   â”‚   â””â”€â”€ 06_presAbs_grid_balancing_Balaenoptera_artificialis.R
â”‚   â”‚
â”‚   â””â”€â”€ 02habitatModel/
â”‚       â”œâ”€â”€ 00_exploratoryDataAnalysis_Balaenoptera_artificialis
â”‚       â”œâ”€â”€ 01_fitRF_Balaenoptera_artificialis
â”‚       â”œâ”€â”€ 02_predictDaily_and_MeanSD_Balaenoptera_artificialis
â”‚       â””â”€â”€ 99sessionInfo.R
```

---

## Resumen del flujo de trabajo

### Procesamiento de datos de seguimiento

```
00_L0_read_and_standardize_Balaenoptera_artificialis_tracking.R
02_L1_douglas_speed_filter_Balaenoptera_artificialis_from_L0.R
03_L1_spacetime_split_Balaenoptera_artificialis.R
04_L2_ssm_by_segment_Balaenoptera_artificialis_QC_routePath.R
05_simulations_tracks_Balaenoptera_artificialis.R
06_presAbs_grid_balancing_Balaenoptera_artificialis.R
```

Transforma los datos de seguimiento brutos en un conjunto de datos estructurado de presenciaâ€“ausencia.

---

### Procesamiento de datos ambientales

```
00_oceanMask.R
01_downloadCMEMS.R / 01_downloadCMEMS.sh
03_prepareStaticLayers.R
04_prepareCMEMS.R
06_buildPresentStack.R
```

Construye predictores ambientales alineados en el espacio y el tiempo.

---

### ModelizaciÃ³n de hÃ¡bitat

```
00_exploratoryDataAnalysis_Balaenoptera_artificialis.R
01_fitRF_Balaenoptera_artificialis.R
02_predictDaily_and_MeanSD_Balaenoptera_artificialis.R
99sessionInfo.R
```

Ajusta los modelos y genera predicciones espaciales.

---

## Ejecutar el flujo de trabajo completo

```r
source("01scripts/00_main.R")
```

---

## Requisitos

- R (â‰¥ 4.0)
- Paquetes: terra, sf, tidyverse, aniMotum, caret, randomForest, ranger
- Git
- Copernicus Marine Toolbox (`copernicusmarine`)

---

## Notas

- El flujo de trabajo es modular y reproducible.
- Los datos de entrada, el procesamiento y los resultados estÃ¡n claramente separados.
- Los resultados de los modelos dependen en gran medida de la calidad de los datos de entrada.
- El cÃ³digo estÃ¡ diseÃ±ado para la docencia: prima la claridad frente a la optimizaciÃ³n.