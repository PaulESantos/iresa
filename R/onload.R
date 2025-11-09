
.onAttach <- function(lib, pkg) {
  packageStartupMessage("This is iresa ",
                        utils::packageDescription("iresa",
                                                  fields = "Version"
                        ),
                        appendLF = TRUE
  )
}


# -------------------------------------------------------------------------

show_progress <- function() {
  isTRUE(getOption("iresa.show_progress")) && # user disables progress bar
    interactive() # Not actively knitting a document
}



.onLoad <- function(libname, pkgname) {
  opt <- options()
  opt_iresa <- list(
    iresa.show_progress = TRUE
  )
  to_set <- !(names(opt_iresa) %in% names(opt))
  if (any(to_set)) options(opt_iresa[to_set])
  invisible()
}


utils::globalVariables(c(
  "cambio_rank",  "cambio_score", "min_rank", "n_scores_unicos", "rank_2019",
  "region", "score_2019", "tendencia",
  # ============================================================
  # Operadores
  # ============================================================
  "%>%", ":="
))
