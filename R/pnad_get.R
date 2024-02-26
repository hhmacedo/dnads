#' Download PNAD files and read them.
#'
#' @param year A year must be provided.
#' @param design TRUE if you want a survey object, FALSE if you want a dataframe.
#' @param vars Selected variables
#' @param hh_only If TRUE, will skip person data, using just household data.
#'
#' @return A survey object or a tibble.
#' @export
#'
#' @examples
#' \dontrun{
#' # Download PNAD 2015
#' pnad2015 <- pnad_get(year = 2015)
#'
#' # Select just the variable V8005 for PNAD 2008
#' pnad2008 <- pnad_get(year = 2008, vars = c("V8005"))
#' }
#'
#' @importFrom utils download.file unzip
pnad_get <- function(year, design = TRUE, vars = NULL, hh_only = FALSE) {

  # A year must be provided
  stopifnot("You must select an year" = !missing(year))

  # List required files
  download_links <- pnad_list(files = TRUE, year = year)

  # Create a temporary directory to keep files
  temp_dir <- file.path(tempdir(), "pnad")
  if(!dir.exists(temp_dir)) { dir.create(temp_dir) }

  if (length(download_links) %in% 1:2) {

    for (download_link in download_links) {
      # Download file
      temp_file <- file.path(tempdir(), basename(download_link))
      temp_zip <- file.path(tempdir(), "unzip")

      download_link |>
        httr2::request() |>
        httr2::req_progress() |>
        httr2::req_perform(path = temp_file)

      # Extract files and remove .zip
      zip::unzip(temp_file, junkpaths = TRUE, exdir = temp_zip)
      file.remove(temp_file)

    }

    temp_regex <- "^(input.)?(pes|dom)\\d{4}\\.txt$"

    keep_files <- grep(temp_regex,
                       list.files(path = temp_zip),
                       ignore.case = TRUE,
                       value = TRUE)

    file.rename(from = file.path(temp_zip, keep_files),
                to = file.path(temp_dir, keep_files))

    unlink(temp_zip, recursive = TRUE)

  } else {

    for (download_link in download_links) {

      # Identify the regex
      temp_regex <- dplyr::case_when(
        grepl("Dados\\.zip$", download_link) ~
          paste0("(d(o(m(icilio(s)?)?)?)?|p(e(s(soa(s)?)?)?)?)", # person and hh variations
                 "(\\d{2})?(\\d{2})?(br)?", # 0, 2 or 4 digits with or without a br
                 "(\\.txt|\\.dat)?$"), # .txt, .dat or without an extension
        grepl("Layout\\.zip$", download_link) ~ "(sas|input ).?(do(m)?|pe(s)?)(\\d{2})?\\.txt",
        grepl("amostra\\d{2}\\.zip$", download_link) ~ "amostra\\d{2}\\.csv"
      )

      # Download file
      temp_file <- file.path(tempdir(), basename(download_link))
      temp_zip <- file.path(tempdir(), "unzip")

      download_link |>
        httr2::request() |>
        httr2::req_progress() |>
        httr2::req_perform(path = temp_file)

      # Extract files and remove .zip
      zip::unzip(temp_file, junkpaths = TRUE, exdir = temp_zip)
      file.remove(temp_file)

      keep_files <- grep(temp_regex,
                         list.files(path = temp_zip),
                         ignore.case = TRUE,
                         value = TRUE)

      # Move files to the PNAD temporary directory
      file.rename(from = file.path(temp_zip, keep_files),
                  to = file.path(temp_dir, keep_files))

      unlink(temp_zip, recursive = TRUE)
    }

  }
  keep_files <- list.files(temp_dir)

  hh_file_df <- file.path(temp_dir,
                          grep("^d(om(icilio(s)?)?)?\\d{2}(\\d{2})?(br)?(\\.txt|\\.dat)?$",
                               list.files(temp_dir),
                               ignore.case = TRUE,
                               value = TRUE))

  hh_file_input <- file.path(temp_dir,
                             grep("^(input.|sas.?)do(m)?(\\d{2})?(\\d{2})?\\.txt$",
                                  list.files(temp_dir),
                                  ignore.case = TRUE,
                                  value = TRUE))

  if (year %in% 1992:1999) {
    psu_strat_df <- file.path(temp_dir,
                              grep("^amostra\\d{2}\\.csv$",
                                   list.files(temp_dir),
                                   ignore.case = TRUE,
                                   value = TRUE))
  }

  # Check if person data will be skipped
  if (hh_only == FALSE) {
    prs_file_df <- file.path(temp_dir,
                             grep("^p(es(soa(s)?)?)?\\d{2}(\\d{2})?(br)?(\\.txt|\\.dat)?$",
                                  list.files(temp_dir),
                                  ignore.case = TRUE,
                                  value = TRUE))

    prs_file_input <- file.path(temp_dir,
                                grep("^(input.|sas.?)pe(s)?(\\d{2})?(\\d{2})?\\.txt$",
                                     list.files(temp_dir),
                                     ignore.case = TRUE,
                                     value = TRUE))

    if (year %in% 1992:1999) {
      pnad <- pnad_read(hh_data = hh_file_df, hh_input = hh_file_input,
                        prs_data = prs_file_df, prs_input = prs_file_input,
                        psu_strat = psu_strat_df, vars = vars)
    } else {
      pnad <- pnad_read(hh_data = hh_file_df, hh_input = hh_file_input,
                        prs_data = prs_file_df, prs_input = prs_file_input,
                        vars = vars)
    }
  } else {

    if (year %in% 1992:1999) {
      pnad <- pnad_read(hh_data = hh_file_df, hh_file_input,
                        psu_strat = psu_strat_df, vars = vars)
    } else {
      pnad <- pnad_read(hh_data = hh_file_df, hh_file_input, vars = vars)
    }

  }

  # Remove used files
  file.remove(list.files(temp_dir, full.names = TRUE))

  invisible(gc())

  # Check if must return a survey object
  if (design == TRUE) {
    pnad <- pnad_design(pnad)
  }

  return(pnad)
}
