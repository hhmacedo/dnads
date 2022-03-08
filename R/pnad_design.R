#' Implement survey design.
#'
#' @param df Dataframe to be read
#'
#' @return A survey object.
#' @export
#'
pnad_design <- function(df) {
  df_year <- unique(as.numeric(df$V0101))

  design_vars <- data.frame(
    year = c(2001:2009, 2011:2015),
    pre_id = c("~PSU", rep("~V4618", 13)),
    pre_strata = c("~STRAT", rep("~V4617", 13)),
    pre_weights = c(rep("~as.numeric(V4610)", 3),
                    rep("~(as.numeric(V4619) * as.numeric(V4610))", 6),
                    rep("~as.numeric(V4610)", 5)),
    post_strata = rep("~V4609", 14)
  )

  design_vars <- design_vars[design_vars$year == df_year, ]

  options( survey.lonely.psu = "adjust" )

  pop_types <- data.frame(
    x = unique(as.numeric(dplyr::pull(df, gsub("~", "", design_vars$post_strata)))),
    y = unique(as.numeric(dplyr::pull(df, gsub("~", "", design_vars$post_strata))))
  )

  names(pop_types) <- c("Freq", gsub("~", "", design_vars$post_strata))

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
