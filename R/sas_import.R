#' Read a SAS input file
#'
#' @param input File path to the input file
#' @param year Year, to deal properly with problems from PNAD files
#'
#' @return A data frame
#'
#' @noRd
sas_import <- function(input, year) {

  # Import input file
  input_df <- suppressWarnings(readr::read_table(input,
                                                 col_names = FALSE,
                                                 show_col_types = FALSE))

  # Check if there is no breakline between input and first variable and fix it
  if (year %in% c(2001:2007, 2011:2015)) {

    if (nrow(input_df[iconv(input_df$X1, "", "ASCII//TRANSLIT") == "INPUT" &
                      substr(iconv(input_df$X2, "", "ASCII//TRANSLIT"), 1, 1) == "@",]) > 0) {
      line1 <- input_df %>%
        dplyr::filter(iconv(X1, "", "ASCII//TRANSLIT") == "INPUT",
                      substr(iconv(X2, "", "ASCII//TRANSLIT"), 1, 1) == "@") %>%
        dplyr::select(X2, X3, X4) %>%
        dplyr::rename(X1 = X2, X2 = X3, X3 = X4)

      input_df <- dplyr::bind_rows(line1, input_df)
      rm(line1)
    }

  }

  # Extract variable name, positions and type
  input_df <- input_df %>%
    dplyr::filter(substr(iconv(X1, "", "ASCII//TRANSLIT"), 1, 1) == "@") %>%
    dplyr::select(X1, X2, X3) %>%
    dplyr::mutate(X3 = gsub("\\/\\*", "", X3)) %>%
    dplyr::transmute(start = as.integer(gsub("@", "", X1)),
                     end = start +
                       floor(as.numeric(gsub("\\$(CHAR)?", "", X3))) -
                       1,
                     type = ifelse(substr(X3, 1, 1) == "$", "c", "d"),
                     name = X2)

  # Fix the lower v between 1996 and 1999
  if (year %in% 1996:1999) {
    input_df <- input_df %>%
      dplyr::mutate(name = ifelse(name == "v0102",
                                  "V0102",
                                  name))
  }

  # Change age character to numeric
  if (year %in% c(2003, 2007)) {
    input_df <- input_df %>%
      dplyr::mutate(type = ifelse(name == "V8005" & type == "c",
                                  "d",
                                  type))
  }
  # Change population projection character to numeric
  if (year > 2007) {
    input_df <- input_df %>%
      dplyr::mutate(type = ifelse(name == "V4609" & type == "c",
                                  "d",
                                  type))
  }

  # Fix the duplicate column names in 2007
  if (year == 2007) {
    input_df <- input_df %>%
      dplyr::mutate(name = ifelse(start == 965 & name == "V9993",
                                  "V9993B",
                                  name))
  }

  return(input_df)

}
