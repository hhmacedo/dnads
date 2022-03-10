#' Import the data from PNAD using SAS input
#'
#' The pnad_read() function is responsible for read the data using the input
#' file. The variables needed to use the pnad_design() function will always be
#' imported automatically, even if not selected.
#'
#' @param data Data file.
#' @param input Input SAS file.
#' @param vars Selected variables.
#'
#' @return A tibble
#' @export
#'
#' @examples
#' \dontrun{
#' # Import household data
#' pnad2008_dom <- pnad_read(data = "DOM2008.txt", input = "input DOM2008.txt")
#'
#' # Import person data
#' pnad2008_pes <- pnad_read(data = "PES2008.TXT", input = "input PES2008.txt", vars = c("V8005"))
#' }
#' @importFrom rlang .data
pnad_read <- function(data, input) {

  # Get year from data
  year <- as.numeric(substr(readLines(data, n = 1), 1, 4))

  # Import input
  input_df <- sas_import(input = input, year = year)




  }

  # Ignore the warning created by a special character at line 174609
  if (grepl("pes", input, ignore.case = TRUE) & year == 2001) {
    df <- suppressWarnings(
      readr::read_fwf(file = data,
                      readr::fwf_positions(input_df$start,
                                           input_df$end,
                                           input_df$name),
                      col_types = paste(input_df$type, collapse = ""))
    )
  } else {
    df <- readr::read_fwf(file = data,
                          readr::fwf_positions(input_df$start,
                                               input_df$end,
                                               input_df$name),
                          col_types = paste(input_df$type, collapse = ""))
  }

  attr(df, "spec") <- NULL
  attr(df, "problems") <- NULL
  rm(input_df)

  return(df)

}
