#' Import the data from PNAD using SAS input
#'
#' The pnad_read() function is responsible for read the data using the input
#' file. The variables needed to use the pnad_design() function will always be
#' imported automatically, even if not selected.
#'
#' @param hh_data Household data file.
#' @param hh_input Household input SAS file.
#' @param prs_data Person data file.
#' @param prs_input Person input SAS file.
#' @param vars Selected variables.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Import household data
#' pnad2008_dom <- pnad_read(hh_data = "DOM2008.txt", hh_input = "input DOM2008.txt")
#'
#' # Import household and person data, but just mandatory vars and V8005.
#' pnad2008 <- pnad_read(hh_data = "DOM2008.txt", hh_input = "input DOM2008.txt", prs_data = "PES2008.TXT", prs_input = "input PES2008.txt", vars = c("V8005"))
#' }
#' @importFrom rlang .data
pnad_read <- function(hh_data, hh_input, prs_data, prs_input, vars) {

  # Get year from data
  hh_year <- as.numeric(substr(readLines(hh_data, n = 1), 1, 4))

  if (!missing(prs_data)) {
    prs_year <- as.numeric(substr(readLines(hh_data, n = 1), 1, 4))
    stopifnot("Household data and person data are not from the same year." =
                hh_year == prs_year)
  }

  year <- hh_year

  # Import household input
  hh_input_df <- sas_import(input = hh_input, year = year)

  if (!missing(prs_input)) {
    prs_input_df <- sas_import(input = prs_input, year = year)
  } else {
    prs_input_df <- NULL
  }

  # Check when vars are listed
  if (!missing("vars")) {
    if (!is.null(vars)) {

      # Check if all variables are ok and stop if not
      if (!all(vars %in% c(hh_input_df$name, prs_input_df$name))) {
        missing_vars <- vars[!(vars %in% c(hh_input_df$name, prs_input_df$name))]
        stop(paste0("The package couldn't find the following vars:\n  ",
                    paste(missing_vars, collapse = ", "), "."))
      } else {

        # Get mandatory vars
        mandatory_vars <-
          dplyr::case_when(year == 2001 ~ list(c("PSU", "STRAT")),
                           year %in% 2004:2009 ~ list(c("V4618", "V4617", "V4619")),
                           # The option below excludes 2004:2009
                           year %in% 2002:2015 ~ list(c("V4618", "V4617")))

        mandatory_vars <- c("V4610", "V4609", "V0101", "V0102", "V0103", "UF",
                            unlist(mandatory_vars))

        if (!is.null(prs_input_df)) {
          mandatory_vars <- c(mandatory_vars, "V0301")
        }

        # Bind with selected vars
        vars <- c(mandatory_vars, vars)

        hh_input_df <- dplyr::mutate(hh_input_df,
                                     type = ifelse(hh_input_df$name %in% vars,
                                                   hh_input_df$type,
                                                   "_"))
        if (!is.null(prs_input_df)) {
          prs_input_df <- dplyr::mutate(prs_input_df,
                                        type = ifelse(prs_input_df$name %in% vars,
                                                      prs_input_df$type,
                                                      "_"))
        }

      }

    }
  }

  hh_df <- readr::read_fwf(file = hh_data,
                           readr::fwf_positions(hh_input_df$start,
                                                hh_input_df$end,
                                                hh_input_df$name),
                           col_types = paste(hh_input_df$type, collapse = ""))


  attr(hh_df, "spec") <- NULL
  attr(hh_df, "problems") <- NULL
  rm(hh_input_df)

  if (!missing(prs_data)) {

    # Ignore the warning created by a special character at line 174609
    if (year == 2001) {
      suppressWarnings(
        prs_df <- readr::read_fwf(file = prs_data,
                                  readr::fwf_positions(prs_input_df$start,
                                                       prs_input_df$end,
                                                       prs_input_df$name),
                                  col_types = paste(prs_input_df$type,
                                                    collapse = ""))
      )
    } else {
      prs_df <- readr::read_fwf(file = prs_data,
                                readr::fwf_positions(prs_input_df$start,
                                                     prs_input_df$end,
                                                     prs_input_df$name),
                                col_types = paste(prs_input_df$type,
                                                  collapse = ""))
    }

    attr(prs_df, "spec") <- NULL
    attr(prs_df, "problems") <- NULL
    rm(prs_input_df)
    df <- dplyr::inner_join(hh_df, prs_df,
                            by = c("V0101", "UF", "V0102", "V0103"))
  } else {
    df <- hh_df
    rm(hh_df)
  }

  return(df)

}
