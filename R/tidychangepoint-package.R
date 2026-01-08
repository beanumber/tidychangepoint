globalVariables(c("helper", "models", "penalties", "penalty", "pkg", 
                  "segmenter_class", "wraps"))

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

#' Algorithmic coverage through tidychangepoint
#' @returns A [tibble::tibble] or `character`
#' 
#' @export
#' @examples
#' # List packages supported by tidychangepoint
#' ls_pkgs()
#' 

ls_pkgs <- function() {
  tibble::tibble(
    pkg = c("tidychangepoint", "changepoint", "wbs", "GA")
  ) |>
    dplyr::mutate(
      version = purrr::map_chr(pkg, ~as.character(utils::packageVersion(.x))),
    )
}

#' @rdname ls_pkgs
#' @export
#' @seealso [segment()]
#' @examples
#' # List methods supported by segment()
#' ls_methods()
#' 

ls_methods <- function() {
  tibble::tribble(
    ~method, ~pkg, ~segmenter_class, ~helper, ~wraps,
    "pelt", "changepoint", "cpt", "segment_pelt()", "changepoint::cpt.mean() or changepoint::cpt.meanvar()",
    "binseg", "changepoint", "cpt", NA, "changepoint::cpt.meanvar()",
    "segneigh", "changepoint", "cpt", NA, "changepoint::cpt.meanvar()",
    "single-best", "changepoint", "cpt", NA, "changepoint::cpt.meanvar()",
    "wbs", "wbs", "wbs", NA, "wbs::wbs()",
    "ga", "GA", "tidyga", "segment_ga()", "GA::ga()",
    "ga-shi", "GA", "tidyga", "segment_ga_shi()", "segment_ga()",
    "ga-coen", "GA", "tidyga", "segment_ga_coen()", "segment_ga()",
    "coen", "tidychangepoint", "seg_basket", "segment_coen()", NA,
    "random", "GA", "tidyga", "segment_ga_random()", "segment_ga()",
    "manual", "tidychangepoint", "seg_cpt", "segment_manual()", NA,
    "null", "tidychangepoint", "seg_cpt", "segment_manual()", NA,
    "strucchange", "strucchange", "breakpointsfull", NA, "strucchange::breakpoints()",
    "segmented", "segmented", "segmented", NA, "segmented::segmented()",
    "cptga", "changepointGA", "tidycptga", "segment_cptga()", "changepointGA::cptga()"
  )
}

#' @rdname ls_pkgs
#' @export
#' @examples
#' # List penalty functions provided by tidychangepoint
#' ls_penalties()
#' 
ls_penalties <- function() {
  c("SIC", "AIC", "BIC", "HQC", "MBIC", "MDL", "BMDL")
}

#' @rdname ls_pkgs
#' @export
#' @examples
#' # List penalty functions supported by changepoint
#' ls_cpt_penalties()
#' 
ls_cpt_penalties <- function() {
  c("None", "SIC", "BIC", "MBIC", "AIC", "HQC", "Asymptotic", "Manual", "CROPS")
}

#' @rdname ls_pkgs
#' @export
#' @examples
#' # List combinations of method, model, and penalty supported by tidychangepoint
#' ls_coverage()
#' 
ls_coverage <- function() {
  dplyr::bind_rows(
    # PELT
    expand.grid(
      method = "pelt",
      model = c("fit_meanshift_norm", "fit_meanvar"),
      penalty = ls_cpt_penalties()
    ),
    # BinSeg, SegNeigh, single-best
    expand.grid(
      method = c("binseg", "segneigh", "single-best"),
      model = c("fit_meanvar"),
      penalty = ls_cpt_penalties()
    ),
    # GA
    expand.grid(
      method = c("ga", "random"),
      model = ls_models(),
      penalty = ls_penalties() |> 
        stringr::str_subset("BMDL", negate = TRUE)
    ),
    # special-cases
    tibble::tribble(
      ~method, ~model, ~penalty,
      "wbs", NA, NA,
      "cptga", NA, NA,
      "strucchange", NA, NA,
      "segmented", NA, NA,
      "ga-shi", "fit_meanshift_norm_ar1", "BIC",
      "ga-coen", "fit_nhpp", "BMDL",
      "coen", "fit_nhpp", "BMDL",
      "manual", "fit_meanshift_norm", "BIC",
      "null", "fit_meanshift_norm", "BIC"
    )
  )
}
