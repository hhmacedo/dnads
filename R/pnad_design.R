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

  design_vars <- dplyr::case_when(
    df_year %in% 1992:1999 ~ list(c("psu", "strat", "V4610", "V4609")),
    df_year == 2001 ~ list(c("PSU", "STRAT", "V4610", "V4609")),
    df_year %in% 2004:2009 ~ list(c("V4618", "V4617", "V4619", "V4610", "V4609")),
    df_year %in% 2002:2015 ~ list(c("V4618", "V4617", "V4610", "V4609"))
  )
  design_vars <- unlist(design_vars)

  # Check if all design variables are ok and stop if not
  if (!all(design_vars %in% colnames(df))) {
    missing_vars <- design_vars[!(design_vars %in% colnames(df))]
    stop(paste0("The package couldn't find the following design vars:\n  ",
                paste(missing_vars, collapse = ", "), "."))
  }

  design_vars <- data.frame(
    year = c(1992:1993, 1995:1999, 2001:2009, 2011:2015),
    pre_id = c(rep("~psu", 7), "~PSU", rep("~V4618", 13)),
    pre_strata = c(rep("~strat", 7), "~STRAT", rep("~V4617", 13)),
    pre_weights = c(rep("~as.numeric(V4610)", 10),
                   rep("~(as.numeric(V4619) * as.numeric(V4610))", 6),
                   rep("~as.numeric(V4610)", 5)),
    post_strata = rep("~V4609", 21)
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
