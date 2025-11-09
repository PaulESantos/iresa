#' IRESA General - Resumen de Índice y Pilares
#'
#' Dataset en formato largo que resume el índice general IRESA y sus dos pilares
#' (Calidad Ambiental y Gobernanza Ambiental y gestión de riesgos) para todas las
#' regiones del Perú durante el periodo 2019-2025.
#'
#' @format Un tibble con 75 filas y 8 columnas:
#' \describe{
#'   \item{indice_pilar}{Nombre del índice o pilar. Valores: "IRESA",
#'                       "Calidad ambiental", "Gobernanza Ambiental y gestión de riesgos"}
#'   \item{region}{Nombre de la región}
#'   \item{rank_2025, rank_2024, rank_2019}{Posición en el ranking (1 = mejor, 25 = peor)}
#'   \item{score_2025, score_2024, score_2019}{Puntaje en escala 0-10}
#' }
#'
#' @details
#' Este dataset facilita análisis comparativos entre el índice general y los pilares,
#' así como visualizaciones de la evolución temporal de cada región.
#'
#' ## Estructura de los datos
#' - 25 regiones × 3 índices/pilares = 75 filas
#' - Cada fila representa una combinación única de región e índice/pilar
#'
#' @source Instituto Peruano de Economía (IPE). IRESA 2025.
#'
#' @seealso [calidad_ambiental], [gobernanza_gestion]
#'
#' @examples
#' # Comparar puntajes de pilares para Tacna
#' iresa_general |>
#'   dplyr::filter(region == "Tacna", indice_pilar != "IRESA") |>
#'   dplyr::select(indice_pilar, score_2025)
#'
#' # Ranking del pilar Calidad Ambiental 2025
#' iresa_general |>
#'   dplyr::filter(indice_pilar == "Calidad ambiental") |>
#'   dplyr::arrange(rank_2025) |>
#'   dplyr::select(region, rank_2025, score_2025)
#'
#' # Evolución temporal del IRESA
#' iresa_general |>
#'   dplyr::filter(indice_pilar == "IRESA") |>
#'   dplyr::select(region, score_2019, score_2024, score_2025) |>
#'   dplyr::arrange(dplyr::desc(score_2025))
#'
"iresa_general"


#' Indicadores de Calidad Ambiental
#'
#' Dataset detallado de los 7 indicadores que componen el pilar Calidad Ambiental
#' del IRESA, para las 25 regiones del Perú durante el periodo 2019-2025.
#'
#' @format Un tibble con 175 filas y 8 columnas:
#' \describe{
#'   \item{indicador}{Nombre del indicador. Valores posibles:
#'     \itemize{
#'       \item "Tratamiento de aguas residuales"
#'       \item "Calidad del aire"
#'       \item "Acceso a agua segura"
#'       \item "Consumo de energías limpias"
#'       \item "Generación total de residuos sólidos municipales ajustados por PBI"
#'       \item "Degradación de ecosistemas"
#'       \item "Disposición adecuada de residuos"
#'     }}
#'   \item{region}{Nombre de la región}
#'   \item{rank_2025, rank_2024, rank_2019}{Posición en el ranking del indicador (1 = mejor)}
#'   \item{score_2025, score_2024, score_2019}{Valor del indicador en su unidad original:
#'     \itemize{
#'       \item Tratamiento: \% de volumen de agua tratado entre volumen de agua recogido
#'       \item Calidad del aire: µg/m³, Niveles promedio anuales de material particulado fino PM2.5, microgramos por metro cubico
#'       \item Acceso a agua segura: \% de la población con acceso a agua con el nivel adecuado de cloro residual mayor o igual a 0.5 mg/L
#'       \item Energías limpias: \% de hogares que usan gas natural, GLP o electricidad para cocinar sus alimentos
#'       \item Generación residuos: Kilos por s/1.000 de PBI de la región
#'       \item Degradación: \% del total de superficie de ecosistemas en la región
#'       \item Disposición: \% de la población que vive en una municipalidad (provincial o distrital) que dispone sus residuos en rellenos sanitarios o reciclaje
#'     }}
#' }
#'
#' @details
#' ## Estructura de los datos
#' - 25 regiones × 7 indicadores = 175 filas
#' - Cada fila representa una combinación única de región e indicador
#'
#' ## Criterios de evaluación
#' - **Mejor desempeño**: Valores altos excepto en calidad del aire, generación de residuos
#'   y degradación de ecosistemas (donde valores bajos son mejores)
#' - **Datos faltantes**: Algunos indicadores (ej: degradación de ecosistemas) pueden tener
#'   NA para Lima* y Lima Provincias
#'
#' @source Instituto Peruano de Economía (IPE). IRESA 2025.
#'   Fuentes originales: SUNASS, SINIA-MINAM, Universidad de Chicago Air Quality Index,
#'   INEI-RENAMU, MINAM-GeoBosques.
#'
#' @seealso [iresa_general], [gobernanza_gestion]
#'
#' @examples
#' # Ranking de tratamiento de aguas residuales 2025
#' calidad_ambiental |>
#'   dplyr::filter(indicador == "Tratamiento de aguas residuales") |>
#'   dplyr::arrange(rank_2025) |>
#'   dplyr::select(region, rank_2025, score_2025)
#'
#' # Regiones con mejor calidad del aire (menor PM2.5)
#' calidad_ambiental |>
#'   dplyr::filter(indicador == "Calidad del aire") |>
#'   dplyr::arrange(score_2025) |>
#'   dplyr::select(region, rank_2025, score_2025) |>
#'   dplyr::slice(1:5)
#'
#' # Evolución del acceso a agua segura en Loreto
#' calidad_ambiental |>
#'   dplyr::filter(
#'     region == "Loreto",
#'     indicador == "Acceso a agua segura"
#'   ) |>
#'   dplyr::select(region, score_2019, score_2024, score_2025)
#'
#' # Comparar todos los indicadores de Tacna en 2025
#' calidad_ambiental |>
#'   dplyr::filter(region == "Tacna") |>
#'   dplyr::select(indicador, rank_2025, score_2025) |>
#'   dplyr::arrange(rank_2025)
#'
"calidad_ambiental"


#' Indicadores de Gobernanza Ambiental y Gestión de Riesgos
#'
#' Dataset detallado de los 7 indicadores que componen el pilar Gobernanza Ambiental
#' y gestión de riesgos del IRESA, para las 25 regiones del Perú durante el periodo 2019-2025.
#'
#' @format Un tibble con 175 filas y 8 columnas:
#' \describe{
#'   \item{indicador}{Nombre del indicador. Valores posibles:
#'     \itemize{
#'       \item "Conflictos socioambientales activos"
#'       \item "Delitos denunciados por minería ilegal"
#'       \item "Áreas verdes en espacios públicos conservadas por la municipalidad"
#'       \item "Agua no facturada"
#'       \item "Viviendas informales"
#'       \item "Pérdidas por incendios forestales"
#'       \item "Ejecución del gasto destinado a prevención y reducción de desastres"
#'     }}
#'   \item{region}{Nombre de la región}
#'   \item{rank_2025, rank_2024, rank_2019}{Posición en el ranking del indicador (1 = mejor)}
#'   \item{score_2025, score_2024, score_2019}{Valor del indicador en su unidad original:
#'     \itemize{
#'       \item Conflictos: Conflictos activos por cada 100.000 habitantes
#'       \item Delitos minería: Denuncia por cada 100.000 habitantes
#'       \item Áreas verdes: m² Metros cuadrados por habitante
#'       \item Agua no facturada: \% de agua producida que no se convierte en ingresos
#'       \item Viviendas informales: \% de viviendas informales en ciudades de mas de 20 mil habitantes
#'       \item Incendios: \% de la superficie total de la región
#'       \item Ejecución gasto: \% de ejecución del PIM del programa presupuestal 68:reducción de vulnerabilidad y atención de emergencia por desastre
#'     }}
#' }
#'
#' @details
#' ## Estructura de los datos
#' - 25 regiones × 7 indicadores = 175 filas
#' - Cada fila representa una combinación única de región e indicador
#'
#' ## Criterios de evaluación
#' - **Mejor desempeño**: Valores altos en áreas verdes y ejecución de gasto;
#'   valores bajos en conflictos, delitos, agua no facturada, viviendas informales
#'   y pérdidas por incendios
#' - **Datos faltantes**: El indicador "Delitos denunciados por minería ilegal"
#'   no tiene datos para 2019 (NA)
#'
#' ## Notas importantes
#' - **Delitos minería ilegal**: Solo disponible desde 2024
#' - **Áreas verdes**: Datos corresponden al año anterior (2023 para edición 2025)
#' - **Ejecución gasto**: Puede variar significativamente entre años por eventos extraordinarios
#'
#' @source Instituto Peruano de Economía (IPE). IRESA 2025.
#'   Fuentes originales: Defensoría del Pueblo, Ministerio Público, INEI-RENAMU,
#'   SUNASS, CENEPRED, SERFOR, MEF-SIAF.
#'
#' @seealso [iresa], [iresa_general], [calidad_ambiental]
#'
#' @examples
#' # Regiones sin conflictos socioambientales en 2025
#' gobernanza_gestion |>
#'   dplyr::filter(
#'     indicador == "Conflictos socioambientales activos",
#'     score_2025 == 0
#'   ) |>
#'   dplyr::select(region, score_2025)
#'
#' # Ranking de delitos por minería ilegal 2025
#' gobernanza_gestion |>
#'   dplyr::filter(indicador == "Delitos denunciados por minería ilegal") |>
#'   dplyr::arrange(rank_2025) |>
#'   dplyr::select(region, rank_2025, score_2025)
#'
#' # Evolución de viviendas informales en Madre de Dios
#' gobernanza_gestion |>
#'   dplyr::filter(
#'     region == "Madre de Dios",
#'     indicador == "Viviendas informales"
#'   ) |>
#'   dplyr::select(region, score_2019, score_2024, score_2025)
#'
#' # Regiones con mayor ejecución del gasto en prevención 2025
#' gobernanza_gestion |>
#'   dplyr::filter(
#'     indicador == "Ejecución del gasto destinado a prevención y reducción de desastres"
#'   ) |>
#'   dplyr::arrange(dplyr::desc(score_2025)) |>
#'   dplyr::select(region, rank_2025, score_2025) |>
#'   dplyr::slice(1:5)
#'
#' # Comparar todos los indicadores de Tacna en 2025
#' gobernanza_gestion |>
#'   dplyr::filter(region == "Tacna") |>
#'   dplyr::select(indicador, rank_2025, score_2025) |>
#'   dplyr::arrange(rank_2025)
#'
"gobernanza_gestion"

#' Mapa de las Regiones del Perú
#'
#' Objeto espacial (simple feature) con las geometrías de las 25 regiones del Perú,
#' compatible con los datasets del IRESA para crear visualizaciones espaciales.
#' Incluye polígonos validados en proyección WGS 84 (EPSG:4326).
#'
#' @format Un objeto `sf` (simple feature collection) con 25 filas y 2 columnas:
#' \describe{
#'   \item{region}{Nombre de la región. Coincide exactamente con los nombres en
#'                 los datasets IRESA. Lima se divide en "Lima*" (Lima Metropolitana
#'                 y Callao) y "Lima Provincias"}
#'   \item{geometry}{Geometría tipo MULTIPOLYGON con las fronteras regionales}
#' }
#'
#' @details
#' ## Características espaciales
#' - **Tipo de geometría**: MULTIPOLYGON
#' - **Sistema de coordenadas**: WGS 84 (EPSG:4326)
#' - **Bounding box**:
#'   - xmin: -81.33756 (oeste)
#'   - xmax: -68.68425 (este)
#'   - ymin: -18.33775 (sur)
#'   - ymax: -0.0290927 (norte)
#'
#' ## Compatibilidad con IRESA
#' Los nombres de las regiones en `peru_mapa` coinciden exactamente con los
#' de los datasets `iresa_general`, `calidad_ambiental` y
#' `gobernanza_gestion`, facilitando la unión de datos mediante:
#'
#' ```r
#' peru_mapa |> dplyr::left_join(iresa_general, by = "region")
#' ```
#'
#' ## División de Lima
#' El departamento de Lima se presenta dividido en dos unidades:
#' - **Lima*** : Lima Metropolitana y Provincia Constitucional del Callao
#' - **Lima Provincias**: Resto del departamento de Lima
#'
#' Esta división permite análisis diferenciados entre la zona metropolitana
#' y las provincias del interior del departamento.
#'
#' ## Uso con paquetes espaciales
#' El objeto es compatible con:
#' - `\pkg{sf}`: Manipulación de datos espaciales
#' - `\pkg{ggplot2}` + `\pkg{sf}`: Visualización con `geom_sf()`
#' - `\pkg{tmap}`: Mapas temáticos interactivos
#' - `\pkg{leaflet}`: Mapas web interactivos
#' - `\pkg{mapview}`: Exploración rápida de datos espaciales
#'
#' @source Basado en cartografía oficial
#'   del Instituto Geográfico Nacional (IGN) del Perú.
#'
#' @seealso [iresa_general], [calidad_ambiental], [gobernanza_gestion]
#'
#' @examples
#' \dontrun{
#' library(sf)
#' library(ggplot2)
#' library(dplyr)
#'
#' # Estructura del objeto espacial
#' peru_mapa
#' st_crs(peru_mapa)
#'
#' # Mapa básico de las regiones
#' ggplot(peru_mapa) +
#'   geom_sf() +
#'   theme_minimal()
#'
#' # Mapa de un indicador específico
#' peru_mapa |>
#'   left_join(
#'     calidad_ambiental |>
#'       filter(indicador == "Acceso a agua segura") |>
#'       select(region, score_2025),
#'     by = "region"
#'   ) |>
#'   ggplot() +
#'   geom_sf(aes(fill = score_2025)) +
#'   scale_fill_viridis_c(
#'     name = "% Población",
#'     labels = scales::percent_format(scale = 1)
#'   ) +
#'   labs(
#'     title = "Acceso a Agua Segura",
#'     subtitle = "Población con niveles adecuados de cloro residual, 2024"
#'   ) +
#'   theme_minimal()
#'}
#'
"peru_mapa"
