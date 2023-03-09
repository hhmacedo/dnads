#' Implement a complex survey design
#'
#' @param df Data frame from PNAD
#'
#' @return An object of class survey.design.
#' @export
#'
pnad_design <- function(df) {
  df_year <- unique(as.numeric(df$V0101))
  if (df_year < 100) { df_year <- 1900 + df_year }

  design_vars <- data.frame(
    year = c(1998:1999, 2001:2009, 2011:2015),
    pre_id = c(rep("~PSU", 3), rep("~V4618", 13)),
    pre_strata = c(rep("~STRAT", 3), rep("~V4617", 13)),
    pre_weights = c(rep("~as.numeric(V4610)", 5),
                    rep("~(as.numeric(V4619) * as.numeric(V4610))", 6),
                    rep("~as.numeric(V4610)", 5)),
    post_strata = rep("~pos_estrato", 16)
  )

  design_vars <- design_vars[design_vars$year == df_year, ]

  options( survey.lonely.psu = "adjust" )

  # Create the PSU and STRAT for 1998 and 1999
  if (df_year %in% 1998:1999) {
    df <- df |>
      dplyr::mutate(
        STRAT = dplyr::case_when(V4107 %in% c("1", "2") ~
                                   as.numeric(UF)*1e8 + as.numeric(UPA),
                                 V4107 == "3" ~
                                   as.numeric(UF)*1e8 + 99*1e6 + as.numeric(V4602)*1e4),
        PSU = dplyr::case_when(V4107 %in% c("1", "2") ~
                                 as.numeric(V0102)*1e3,
                               V4107 == "3" ~
                                 as.numeric(UF)*1e6 + as.numeric(V4602)*1e4 + as.numeric(UPA))
      )
  }

  # Create a post stratification variable
  df <- df %>%
    dplyr::mutate(pos_estrato = ifelse(V4107 == "1",
                                       paste0(UF, "_", "RM"),
                                       paste0(UF, "_", "NaoRM")))

  # Verify the post stratification population
  pop_types <- df %>%
    dplyr::group_by(pos_estrato) %>%
    dplyr::summarise(minimo = min(V4609),
                     Freq = mean(V4609),
                     maximo = max(V4609))

  pop_types <- pop_types %>%
    dplyr::mutate(Freq = ifelse(minimo != maximo, minimo+maximo, Freq)) %>%
    dplyr::select(pos_estrato, Freq)

  pnad_pre <- survey::svydesign(id = stats::as.formula(design_vars$pre_id),
                                strata = stats::as.formula(design_vars$pre_strata),
                                data = df,
                                weights = stats::as.formula(design_vars$pre_weights),
                                nest = TRUE)

  rm(df);invisible(gc())

  pnad_post <- survey::postStratify(design = pnad_pre ,
                                    strata = stats::as.formula(design_vars$post_strata),
                                    population = pop_types)

  rm(pnad_pre, pop_types); invisible(gc())

  return(pnad_post)
}
