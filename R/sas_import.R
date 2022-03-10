#' Read a SAS input file
#'
#' @param input File path to the input file
#' @param year Year, to deal propperly with problems from PNAD files
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
        dplyr::filter(iconv(.data$X1, "", "ASCII//TRANSLIT") == "INPUT",
                      substr(iconv(.data$X2, "", "ASCII//TRANSLIT"), 1, 1) == "@") %>%
        dplyr::select(.data$X2, .data$X3, .data$X4) %>%
        dplyr::rename(X1 = .data$X2, X2 = .data$X3, X3 = .data$X4)

      input_df <- dplyr::bind_rows(line1, input_df)
      rm(line1)
    }

  }

  # Extract variable name, positions and type
  input_df <- input_df %>%
    dplyr::filter(substr(iconv(.data$X1, "", "ASCII//TRANSLIT"), 1, 1) == "@") %>%
    dplyr::select(.data$X1, .data$X2, .data$X3) %>%
    dplyr::mutate(X3 = gsub("\\/\\*", "", .data$X3)) %>%
    dplyr::transmute(start = as.integer(gsub("@", "", .data$X1)),
                     end = .data$start +
                       floor(as.numeric(gsub("\\$(CHAR)?", "", .data$X3))) -
                       1,
                     type = ifelse(substr(.data$X3, 1, 1) == "$", "c", "d"),
                     name = .data$X2)

  # Change population projection character to numeric
  if (year > 2007) {
    input_df <- input_df %>%
      dplyr::mutate(type = ifelse(.data$name == "V4609" & .data$type == "c",
                                  "d",
                                  .data$type))
  }

  # Change age character to numeric
  if (year %in% c(2003, 2007)) {
    input_df <- input_df %>%
      dplyr::mutate(type = ifelse(.data$name == "V8005" & .data$type == "c",
                                  "d",
                                  .data$type))
  }

  # Fix the duplicate column names in 2007
  if (year == 2007) {
    input_df <- input_df %>%
      dplyr::mutate(name = ifelse(.data$start == 965 & .data$name == "V9993",
                                  "V9993B",
                                  .data$name))
  }

  return(input_df)

}
