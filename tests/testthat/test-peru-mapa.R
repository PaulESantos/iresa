test_that("peru_mapa tiene la estructura correcta", {
  expect_s3_class(peru_mapa, "sf")
  expect_equal(nrow(peru_mapa), 25)
  expect_equal(ncol(peru_mapa), 2)
  expect_true("region" %in% names(peru_mapa))
  expect_true("geometry" %in% names(peru_mapa))
})

test_that("peru_mapa tiene geometrías válidas", {
  expect_true(all(sf::st_is_valid(peru_mapa)))
  expect_equal(as.character(sf::st_geometry_type(peru_mapa, by_geometry = FALSE)), "MULTIPOLYGON")
})

test_that("peru_mapa tiene el CRS correcto", {
  expect_equal(sf::st_crs(peru_mapa)$epsg, 4326)
})

test_that("nombres de regiones coinciden con iresa", {
  expect_setequal(peru_mapa$region, iresa_general$region)
})

test_that("puede unirse con datasets IRESA", {
  resultado <- peru_mapa |>
    dplyr::left_join(iresa_general |>
                       dplyr::filter(indice_pilar == "IRESA"),
                     by = "region")

  expect_s3_class(resultado, "sf")
  expect_equal(nrow(resultado), 25)
})
