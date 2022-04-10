compute_corr <- function(data, var1, var2){

  # compute correlation ----
  cor.test(
    x = data %>% dplyr::pull({{var1}}),
    y = data %>% dplyr::pull({{var2}})
  ) %>%
    # tidy up results ----
  broom::tidy() %>%
    # retain and rename relevant bits ----
  dplyr::select(
    correlation = estimate,
    pval = p.value
  )

}
