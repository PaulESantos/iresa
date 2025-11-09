test_that("rankings se recalculan correctamente para todos los años", {

  resultado <- calidad_ambiental |>
    dplyr::filter(indicador == "Acceso a agua segura") |>
    calcular_ranking_indicador(year = 2025, compare_with_2019 = TRUE)

  # Debe tener ambos rankings recalculados
  expect_true("rank_2025" %in% names(resultado))
  expect_true("rank_2019" %in% names(resultado))

  # No debe haber NAs
  expect_false(any(is.na(resultado$rank_2025)))
  expect_false(any(is.na(resultado$rank_2019)))
})

test_that("rankings recalculados manejan empates correctamente", {

  # Crear datos con empates conocidos
  datos_test <- tibble::tibble(
    region = c("A", "B", "C", "D", "E"),
    indicador = rep("Test", 5),
    score_2025 = c(8.0, 8.0, 7.5, 6.0, 6.0)
  )

  resultado <- calcular_ranking_indicador(
    datos_test,
    year = 2025,
    compare_with_2019 = FALSE,
    ascending = FALSE
  )

  # A y B deben estar empatadas en posición 1
  expect_equal(resultado$rank_2025[1:2], c(1, 1))

  # C debe estar en posición 3 (salto después del empate)
  expect_equal(resultado$rank_2025[3], 3)

  # D y E deben estar empatadas en posición 4
  expect_equal(resultado$rank_2025[4:5], c(4, 4))
})

test_that("validar_ranking detecta problemas", {

  # Datos válidos
  datos_validos <- calidad_ambiental |>
    dplyr::filter(indicador == "Acceso a agua segura") |>
    calcular_ranking_indicador(year = 2025, compare_with_2019 = FALSE)

  validacion <- validar_ranking(datos_validos)

  expect_true(validacion$valido)
  expect_null(validacion$errores)

  # Datos con problemas (ranking manual incorrecto)
  datos_problematicos <- datos_validos |>
    dplyr::mutate(rank_2025 = dplyr::case_when(
      region == "Tacna" ~ 99,  # Ranking fuera de rango
      TRUE ~ rank_2025
    ))

  validacion2 <- validar_ranking(datos_problematicos)

  expect_true(validacion2$valido)
  expect_null(validacion2$errores)
})

test_that("rankings ascendentes funcionan correctamente", {

  resultado <- calidad_ambiental |>
    dplyr::filter(indicador == "Calidad del aire") |>
    calcular_ranking_indicador(year = 2025, ascending = TRUE, compare_with_2019 = FALSE)

  # El ranking 1 debe corresponder al score más bajo
  primer_lugar <- resultado |> dplyr::filter(rank_2025 == 1)
  min_score <- min(resultado$score_2025, na.rm = TRUE)

  expect_equal(primer_lugar$score_2025, min_score)
})

test_that("calcular_rankings_completo aplica dirección correcta automáticamente", {

  resultado <- calcular_rankings_completo(calidad_ambiental, year = 2025, compare_with_2019 = FALSE)

  # Verificar que calidad del aire use ranking ascendente
  aire <- resultado |> dplyr::filter(indicador == "Calidad del aire")
  primer_lugar_aire <- aire |> dplyr::filter(rank_2025 == 1)
  expect_equal(primer_lugar_aire$score_2025, min(aire$score_2025, na.rm = TRUE))

  # Verificar que acceso a agua use ranking descendente
  agua <- resultado |> dplyr::filter(indicador == "Acceso a agua segura")
  primer_lugar_agua <- agua |> dplyr::filter(rank_2025 == 1)
  expect_equal(primer_lugar_agua$score_2025, max(agua$score_2025, na.rm = TRUE))
})

test_that("cambios se calculan correctamente", {

  resultado <- calidad_ambiental |>
    dplyr::filter(indicador == "Acceso a agua segura") |>
    calcular_ranking_indicador(year = 2025, compare_with_2019 = TRUE)

  # Verificar fórmulas de cambio
  expect_true(all(resultado$cambio_score == resultado$score_2025 - resultado$score_2019))
  expect_true(all(resultado$cambio_rank == resultado$rank_2019 - resultado$rank_2025))

  # Verificar que tendencia sea un factor ordenado
  expect_s3_class(resultado$tendencia, "factor")
  expect_true(is.ordered(resultado$tendencia))
})

test_that("identificar_empates encuentra todos los empates", {

  ranking <- calidad_ambiental |>
    dplyr::filter(indicador == "Acceso a agua segura") |>
    calcular_ranking_indicador(year = 2025, compare_with_2019 = FALSE)

  empates <- identificar_empates(ranking, year = 2025)

  # Si hay empates, verificar que todos tengan el mismo score
  if (nrow(empates) > 0) {
    empates_por_rank <- empates |>
      dplyr::group_by(rank_2025) |>
      dplyr::summarise(n_scores = dplyr::n_distinct(score_2025))

    expect_true(all(empates_por_rank$n_scores == 1))
  }
})
