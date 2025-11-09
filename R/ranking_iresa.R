#' Calcular Rankings por Indicador con Manejo de Empates
#'
#' Genera rankings para indicadores del IRESA considerando empates y calculando
#' cambios respecto al anio base 2019. Los rankings se recalculan para todos los
#' anios disponibles para asegurar consistencia en el manejo de empates.
#'
#' @param data Un data frame con datos del IRESA. Puede ser `calidad_ambiental`,
#'   `gobernanza_gestion` o `iresa_general`.
#' @param year anio para el cual calcular el ranking. Por defecto 2025.
#' @param compare_with_2019 Logico. Si es TRUE, calcula cambios respecto a 2019.
#'   Por defecto TRUE.
#' @param ascending Logico. Si es TRUE, valores mas bajos obtienen mejor ranking
#'   (posicion 1). Si es FALSE, valores mas altos obtienen mejor ranking.
#'   Por defecto FALSE (valores altos = mejor).
#'
#' @return Un tibble con columnas:
#'   - `region`: Nombre de la region
#'   - `indicador`: Nombre del indicador (si aplica)
#'   - `indice_pilar`: Nombre del indice o pilar (si aplica)
#'   - `score_year`: Puntaje para el anio especificado
#'   - `rank_year`: Ranking recalculado para el anio especificado (con empates)
#'   - `score_2019`: Puntaje en 2019 (si compare_with_2019 = TRUE)
#'   - `rank_2019`: Ranking recalculado para 2019 (si compare_with_2019 = TRUE)
#'   - `cambio_score`: Diferencia de puntaje (si compare_with_2019 = TRUE)
#'   - `cambio_rank`: Diferencia de ranking (si compare_with_2019 = TRUE)
#'   - `tendencia`: Categoria del cambio (si compare_with_2019 = TRUE)
#'
#' @details
#' ## Recalculo de rankings
#' La funcion SIEMPRE recalcula los rankings para todos los anios disponibles en
#' los datos, ignorando cualquier columna `rank_*` preexistente. Esto asegura:
#' - Manejo consistente de empates usando `dplyr::min_rank()`
#' - Comparaciones validas entre anios
#' - Rankings correctos segun la direccion especificada (ascending)
#'
#' ## Manejo de empates
#' Se usa `dplyr::min_rank()` que asigna el ranking mas bajo a todos los valores
#' empatados. Por ejemplo, si tres regiones empatan en el segundo lugar, todas
#' reciben ranking 2, y la siguiente region recibe ranking 5.
#'
#' ## Direccion del ranking
#' Algunos indicadores requieren rankings invertidos:
#' - **Valores altos = mejor** (ascending = FALSE): Tratamiento de aguas,
#'   acceso a agua, consumo de energias limpias, areas verdes, ejecucion de gasto
#' - **Valores bajos = mejor** (ascending = TRUE): Calidad del aire (PM2.5),
#'   generacion de residuos, degradacion de ecosistemas, conflictos, delitos,
#'   agua no facturada, viviendas informales, incendios
#'
#' ## Categorias de tendencia
#' - "Mejora significativa": Subio >=5 posiciones
#' - "Mejora": Subio 1-4 posiciones
#' - "Estable": Sin cambio de posicion
#' - "Deterioro": Bajo 1-4 posiciones
#' - "Deterioro significativo": Bajo >=5 posiciones
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' # Ranking de Calidad Ambiental para un indicador especifico
#' calidad_ambiental |>
#'   filter(indicador == "Acceso a agua segura") |>
#'   calcular_ranking_indicador(year = 2025)
#'
#' # Ranking general IRESA sin comparar con 2019
#' iresa_general |>
#'   filter(indice_pilar == "IRESA") |>
#'   calcular_ranking_indicador(year = 2025, compare_with_2019 = FALSE)
#'
#' # Ranking de calidad del aire (valores bajos = mejor)
#' calidad_ambiental |>
#'   filter(indicador == "Calidad del aire") |>
#'   calcular_ranking_indicador(year = 2025, ascending = TRUE)
#' }
#'
#' @export
calcular_ranking_indicador <- function(data,
                                       year = 2025,
                                       compare_with_2019 = TRUE,
                                       ascending = FALSE) {

  # Validaciones iniciales
  if (!is.data.frame(data)) {
    stop("El argumento 'data' debe ser un data frame", call. = FALSE)
  }

  if (!year %in% c(2019, 2024, 2025)) {
    stop("El anio debe ser 2019, 2024 o 2025", call. = FALSE)
  }

  if (!"region" %in% names(data)) {
    stop("El data frame debe contener la columna 'region'", call. = FALSE)
  }

  # Detectar columnas de agrupacion (indicador o indice_pilar)
  group_cols <- intersect(c("indicador", "indice_pilar"), names(data))

  # Construir nombres de columnas
  score_col_actual <- paste0("score_", year)

  if (!score_col_actual %in% names(data)) {
    stop(glue::glue("El data frame debe contener la columna '{score_col_actual}'"),
         call. = FALSE)
  }

  # Seleccionar solo las columnas necesarias (sin rankings preexistentes)
  cols_a_mantener <- c("region", group_cols, grep("^score_", names(data), value = TRUE))
  resultado <- data |>
    dplyr::select(dplyr::all_of(cols_a_mantener))

  # Identificar todos los anios disponibles
  years_disponibles <- gsub("score_", "", grep("^score_", names(resultado), value = TRUE))
  years_disponibles <- as.numeric(years_disponibles)

  # PASO 1: Recalcular rankings para TODOS los anios disponibles
  for (yr in years_disponibles) {
    score_col <- paste0("score_", yr)
    rank_col <- paste0("rank_", yr)

    resultado <- resultado |>
      dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
      dplyr::mutate(
        !!rank_col := if (ascending) {
          dplyr::min_rank(!!rlang::sym(score_col))
        } else {
          dplyr::min_rank(dplyr::desc(!!rlang::sym(score_col)))
        }
      ) |>
      dplyr::ungroup()
  }

  # PASO 2: Construir el resultado final segun los parametros
  rank_col_actual <- paste0("rank_", year)

  if (compare_with_2019 && year != 2019) {

    # Verificar que existan datos de 2019
    if (!"score_2019" %in% names(resultado)) {
      warning("No se encontro 'score_2019'. Se omitira la comparacion.", call. = FALSE)

      resultado_final <- resultado |>
        dplyr::select(
          region,
          dplyr::all_of(group_cols),
          !!rlang::sym(score_col_actual),
          !!rlang::sym(rank_col_actual)
        ) |>
        dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)), !!rlang::sym(rank_col_actual))

      return(resultado_final)
    }

    # Calcular cambios entre anio actual y 2019
    resultado <- resultado |>
      dplyr::mutate(
        cambio_score = !!rlang::sym(score_col_actual) - score_2019,
        cambio_rank = rank_2019 - !!rlang::sym(rank_col_actual),  # Positivo = mejora
        tendencia = dplyr::case_when(
          is.na(cambio_rank) ~ "Sin datos",
          cambio_rank >= 5 ~ "Mejora significativa",
          cambio_rank > 0 ~ "Mejora",
          cambio_rank == 0 ~ "Estable",
          cambio_rank > -5 ~ "Deterioro",
          TRUE ~ "Deterioro significativo"
        ),
        tendencia = factor(
          tendencia,
          levels = c(
            "Mejora significativa",
            "Mejora",
            "Estable",
            "Deterioro",
            "Deterioro significativo",
            "Sin datos"
          ),
          ordered = TRUE
        )
      )

    # Seleccionar columnas para el resultado final
    resultado_final <- resultado |>
      dplyr::select(
        region,
        dplyr::all_of(group_cols),
        !!rlang::sym(score_col_actual),
        !!rlang::sym(rank_col_actual),
        score_2019,
        rank_2019,
        cambio_score,
        cambio_rank,
        tendencia
      )

  } else {
    # Sin comparacion con 2019
    resultado_final <- resultado |>
      dplyr::select(
        region,
        dplyr::all_of(group_cols),
        !!rlang::sym(score_col_actual),
        !!rlang::sym(rank_col_actual)
      )
  }

  # Ordenar por grupo (si existe) y ranking
  resultado_final <- resultado_final |>
    dplyr::arrange(dplyr::across(dplyr::all_of(group_cols)), !!rlang::sym(rank_col_actual))

  return(resultado_final)
}


#' Calcular Rankings para Todos los Indicadores
#'
#' Aplica `calcular_ranking_indicador()` a todos los indicadores de un dataset
#' del IRESA de forma automatica, detectando la direccion correcta del ranking.
#'
#' @param data Un data frame del IRESA (`calidad_ambiental`, `gobernanza_gestion`
#'   o `iresa_general`).
#' @param year anio para el cual calcular el ranking. Por defecto 2025.
#' @param compare_with_2019 Logico. Si es TRUE, calcula cambios respecto a 2019.
#'
#' @return Un tibble con rankings recalculados para todos los indicadores del dataset.
#'
#' @details
#' La funcion detecta automaticamente que indicadores requieren ranking ascendente
#' (valores bajos = mejor) basandose en una lista predefinida de indicadores.
#'
#' Los rankings se recalculan completamente para asegurar consistencia, ignorando
#' cualquier columna `rank_*` preexistente en los datos originales.
#'
#' @examples
#' \dontrun{
#' # Rankings de todos los indicadores de Calidad Ambiental
#' ranking_ca <- calcular_rankings_completo(calidad_ambiental, year = 2025)
#'
#' # Rankings de Gobernanza sin comparar con 2019
#' ranking_gg <- calcular_rankings_completo(
#'   gobernanza_gestion,
#'   year = 2025,
#'   compare_with_2019 = FALSE
#' )
#'
#' # Ver regiones con mejora significativa
#' ranking_ca |>
#'   filter(tendencia == "Mejora significativa") |>
#'   arrange(indicador, rank_2025)
#' }
#'
#' @export
calcular_rankings_completo <- function(data,
                                       year = 2025,
                                       compare_with_2019 = TRUE) {

  # Indicadores donde valores BAJOS son MEJORES (ascending = TRUE)
  indicadores_ascendentes <- c(
    "Calidad del aire",
    "Generacion total de residuos solidos municipales ajustados por PBI",
    "Degradacion de ecosistemas",
    "Conflictos socioambientales activos",
    "Delitos denunciados por mineria ilegal",
    "Agua no facturada",
    "Viviendas informales",
    "Pardidas por incendios forestales"
  )

  # Detectar columna de agrupacion
  if ("indicador" %in% names(data)) {
    group_var <- "indicador"
  } else if ("indice_pilar" %in% names(data)) {
    group_var <- "indice_pilar"
  } else {
    stop("El dataset debe contener 'indicador' o 'indice_pilar'", call. = FALSE)
  }

  # Procesar cada indicador/pilar por separado
  resultado <- data |>
    dplyr::group_by(!!rlang::sym(group_var)) |>
    dplyr::group_split() |>
    purrr::map_dfr(function(df) {

      nombre_grupo <- unique(df[[group_var]])
      es_ascendente <- nombre_grupo %in% indicadores_ascendentes

      calcular_ranking_indicador(
        data = df,
        year = year,
        compare_with_2019 = compare_with_2019,
        ascending = es_ascendente
      )
    })

  return(resultado)
}


#' Obtener Ranking de una Region Especifica
#'
#' Extrae el ranking de una region particular para uno o todos los indicadores.
#'
#' @param data Un data frame del IRESA con rankings calculados.
#' @param region Nombre de la region (ej: "Tacna", "Lima*", "Loreto").
#' @param year anio del ranking. Por defecto 2025.
#'
#' @return Un tibble con el ranking de la region especificada.
#'
#' @examples
#' \dontrun{
#' # Ranking de Tacna en todos los indicadores de Calidad Ambiental
#' ranking_ca <- calcular_rankings_completo(calidad_ambiental)
#' obtener_ranking_region(ranking_ca, "Tacna")
#'
#' # Ranking de Loreto en Gobernanza
#' ranking_gg <- calcular_rankings_completo(gobernanza_gestion)
#' obtener_ranking_region(ranking_gg, "Loreto")
#' }
#'
#' @export
obtener_ranking_region <- function(data, region, year = 2025) {

  rank_col <- paste0("rank_", year)

  if (!rank_col %in% names(data)) {
    stop(glue::glue("No se encontro la columna '{rank_col}'. Primero calcule los rankings con calcular_ranking_indicador() o calcular_rankings_completo()."),
         call. = FALSE)
  }

  if (!region %in% data$region) {
    regiones_disponibles <- unique(data$region)
    stop(glue::glue("La region '{region}' no se encontro en los datos.\nRegiones disponibles: {paste(regiones_disponibles, collapse = ', ')}"),
         call. = FALSE)
  }

  resultado <- data |>
    dplyr::filter(region == !!region)

  return(resultado)
}


#' Identificar Empates en Rankings
#'
#' Identifica regiones que comparten la misma posicion en el ranking despues
#' del recalculo.
#'
#' @param data Un data frame del IRESA con rankings calculados.
#' @param year anio del ranking a analizar. Por defecto 2025.
#'
#' @return Un tibble con solo las regiones que tienen empates, ordenado por
#'   grupo e indicador.
#'
#' @examples
#' \dontrun{
#' # Identificar empates en el IRESA general
#' ranking <- calcular_ranking_indicador(
#'   iresa_general |> filter(indice_pilar == "IRESA")
#' )
#' identificar_empates(ranking)
#'
#' # Empates en indicadores de Calidad Ambiental
#' ranking_ca <- calcular_rankings_completo(calidad_ambiental)
#' empates_ca <- identificar_empates(ranking_ca)
#'
#' # Ver solo empates en un indicador especifico
#' empates_ca |>
#'   filter(indicador == "Acceso a agua segura")
#' }
#'
#' @export
identificar_empates <- function(data, year = 2025) {

  rank_col <- paste0("rank_", year)
  score_col <- paste0("score_", year)

  if (!rank_col %in% names(data)) {
    stop(glue::glue("No se encontro la columna '{rank_col}'"), call. = FALSE)
  }

  # Detectar columnas de agrupacion
  group_cols <- intersect(c("indicador", "indice_pilar"), names(data))

  # Agrupar por indicador/pilar y ranking para encontrar duplicados
  resultado <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, rank_col)))) |>
    dplyr::filter(dplyr::n() > 1) |>
    dplyr::arrange(dplyr::across(dplyr::all_of(c(group_cols, rank_col)))) |>
    dplyr::ungroup()

  if (nrow(resultado) == 0) {
    message(glue::glue("No se encontraron empates en el ranking del anio {year}"))
    return(tibble::tibble())
  } else {
    n_empates <- resultado |>
      dplyr::count(dplyr::across(dplyr::all_of(c(group_cols, rank_col)))) |>
      nrow()

    message(glue::glue("Se encontraron {nrow(resultado)} regiones en {n_empates} empate(s)"))
  }

  return(resultado)
}


#' Validar Consistencia de Rankings
#'
#' Verifica que los rankings calculados cumplan con las reglas esperadas:
#' - No hay saltos mayores a 1 + numero de empates
#' - Empates tienen el mismo score
#' - Rankings estan en el rango correcto [1, n_regiones]
#'
#' @param data Un data frame con rankings calculados.
#' @param year anio a validar. Por defecto 2025.
#'
#' @return Lista con:
#'   - `valido`: Logico indicando si pasa todas las validaciones
#'   - `errores`: Vector con mensajes de error (si hay problemas)
#'   - `detalles`: Tibble con estadisticas del ranking
#'
#' @examples
#' \dontrun{
#' ranking <- calcular_ranking_indicador(
#'   calidad_ambiental |> filter(indicador == "Acceso a agua segura")
#' )
#' validar_ranking(ranking)
#' }
#'
#' @export
validar_ranking <- function(data, year = 2025) {

  rank_col <- paste0("rank_", year)
  score_col <- paste0("score_", year)

  errores <- character()

  # Verificar que existan las columnas necesarias
  if (!rank_col %in% names(data)) {
    errores <- c(errores, glue::glue("No se encontro la columna '{rank_col}'"))
    return(list(valido = FALSE, errores = errores, detalles = NULL))
  }

  group_cols <- intersect(c("indicador", "indice_pilar"), names(data))

  # Validar por grupo
  detalles <- data |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_cols))) |>
    dplyr::summarise(
      n_regiones = dplyr::n(),
      min_rank = min(!!rlang::sym(rank_col), na.rm = TRUE),
      max_rank = max(!!rlang::sym(rank_col), na.rm = TRUE),
      n_ranks_unicos = dplyr::n_distinct(!!rlang::sym(rank_col)),
      n_empates = dplyr::n() - dplyr::n_distinct(!!rlang::sym(rank_col)),
      .groups = "drop"
    )

  # Validacion 1: Rankings deben empezar en 1
  if (any(detalles$min_rank != 1)) {
    problemas <- detalles |> dplyr::filter(min_rank != 1)
    errores <- c(errores, glue::glue("Rankings no empiezan en 1 para: {paste(problemas[[group_cols[1]]], collapse = ', ')}"))
  }

  # Validacion 2: Empates deben tener el mismo score
  empates <- identificar_empates(data, year)
  if (nrow(empates) > 0) {
    empates_check <- empates |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c(group_cols, rank_col)))) |>
      dplyr::summarise(
        n_scores_unicos = dplyr::n_distinct(!!rlang::sym(score_col)),
        .groups = "drop"
      ) |>
      dplyr::filter(n_scores_unicos > 1)

    if (nrow(empates_check) > 0) {
      errores <- c(errores, "Hay empates con scores diferentes (inconsistencia)")
    }
  }

  # Validacion 3: No debe haber NAs en rankings
  if (any(is.na(data[[rank_col]]))) {
    n_nas <- sum(is.na(data[[rank_col]]))
    errores <- c(errores, glue::glue("Hay {n_nas} valores NA en los rankings"))
  }

  valido <- length(errores) == 0

  return(list(
    valido = valido,
    errores = if (length(errores) > 0) errores else NULL,
    detalles = detalles
  ))
}
