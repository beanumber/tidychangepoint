globalVariables(c("helper", "models", "penalties", "penalty", "pkg", 
                  "segmenter_class", "wraps"))

#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @importFrom lifecycle deprecated
## usethis namespace: end
NULL

#' Algorithmic coverage through tidychangepoint
#' @returns A [tibble::tibble] showing the coverage of `tidychangepoint`
#' 
#' @export
#' @examples
#' tidycpt_coverage()
#' 

tidycpt_coverage <- function() {
  pkgs <- tibble::tibble(
    pkg = c("tidychangepoint", "changepoint", "wbs", "GA")
  ) |>
    dplyr::mutate(
      version = purrr::map_chr(pkg, ~as.character(utils::packageVersion(.x))),
    )
  
  segment_methods <- tibble::tribble(
    ~method, ~pkg, ~segmenter_class, ~helper, ~wraps,
    "pelt", "changepoint", "cpt", "segment_pelt()", "changepoint::cpt.mean() or changepoint::cpt.meanvar()",
    "binseg", "changepoint", "cpt", NA, "changepoint::cpt.meanvar()",
    "segneigh", "changepoint", "cpt", NA, "changepoint::cpt.meanvar()",
    "single-best", "changepoint", "cpt", NA, "changepoint::cpt.meanvar()",
    "wbs", "wbs", "wbs", NA, "wbs::wbs().",
    "ga", "GA", "tidyga", "segment_ga()", "GA::ga()",
    "ga-shi", "GA", "tidyga", "segment_ga_shi()", "segment_ga()",
    "ga-coen", "GA", "tidyga", "segment_ga_coen()", "segment_ga()",
    "coen", "tidychangepoint", "seg_basket", "segment_coen()", "Note that this function is deprecated.",
    "random", "GA", "tidyga", "segment_ga_random()", "segment_ga()",
    "manual", "tidychangepoint", "seg_cpt", "segment_manual()", NA,
    "null", "tidychangepoint", "seg_cpt", "segment_manual()", NA
  )
  
  cpt_penalties <- c("None", "SIC", "BIC", "MBIC", "AIC", "Hannan-Quinn", "Asymptotic", "Manual", "CROPS")
  methods <- dplyr::bind_rows(
    # PELT
    expand.grid(
      method = "pelt",
      model = c("fit_meanshift_norm", "fit_meanvar"),
      penalty = cpt_penalties
    ),
    # BinSeg, SegNeigh, single-best
    expand.grid(
      method = c("binseg", "segneigh", "single-best"),
      model = c("fit_meanvar"),
      penalty = cpt_penalties
    ),
    # GA
    expand.grid(
      method = c("ga", "random"),
      model = ls_models(),
      penalty = c("AIC", "BIC", "MBIC", "MDL")
    ),
    # special-cases
    tibble::tribble(
      ~method, ~model, ~penalty,
      "wbs", NA, NA,
      "ga-shi", "fit_meanshift_norm_ar1", "BIC",
      "ga-coen", "fit_nhpp", "BMDL",
    )
  )
  methods |>
    dplyr::group_by(method) |>
    dplyr::summarize(
      models = paste(unique(model), collapse = ", "),
      penalties = paste(unique(penalty), collapse = ", ")
    ) |>
    dplyr::left_join(segment_methods, by = "method") |>
    dplyr::left_join(pkgs, by = "pkg") |>
    dplyr::select(method, pkg, version, segmenter_class, models, penalties, helper, wraps) |>
    dplyr::arrange(pkg, method)
}
