#' Title
#'
#' @param data input dataset
#' @param var1 variable 1
#' @param var2 bariable 2
#'
#' @return the tidy correlation
#' @export
#'
#' @examples
#' compute_corr(data = faithful, var1 = eruptions, var2 = waiting)
#'
#' @importFrom rlang .data
#'
#'
compute_corr <- function(data, var1, var2){

  # compute correlation ----
  stats::cor.test(
    x = data %>% dplyr::pull({{var1}}),
    y = data %>% dplyr::pull({{var2}})
  ) %>%
    # tidy up results ----
  broom::tidy() %>%
    # retain and rename relevant bits ----
  dplyr::select(
    correlation = .data$estimate,
    pval = .data$p.value
  )

}
