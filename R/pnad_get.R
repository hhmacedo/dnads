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

  for (download_link in download_links) {
    temp_file <- file.path(tempdir(), basename(download_link))
    temp_dir <- file.path(tempdir(), "unzip")

    utils::download.file(url = download_link, destfile = temp_file)

    # Extract files
    if (.Platform$OS.type == "windows") {

      utils::unzip(temp_file, junkpaths = TRUE, exdir = temp_dir)

    } else {

      system(paste("unzip -n -j",
                   "-d", temp_dir,
                   temp_file),
             ignore.stdout = TRUE)

    }
  }

  # Remove files that won't be necessary
  file.remove(grep("(input )?(pes|dom)\\d{4}\\.txt",
                   list.files(path = temp_dir, full.names = TRUE),
                   ignore.case = TRUE,
                   value = TRUE,
                   invert = TRUE))

  hh_file_df <- file.path(temp_dir,
                          grep(paste0("^dom\\d{4}"),
                               list.files(temp_dir),
                               ignore.case = TRUE,
                               value = TRUE))

  hh_file_input <- file.path(temp_dir,
                             grep(paste0("^input.{1}dom\\d{4}.txt"),
                                  list.files(temp_dir),
                                  ignore.case = TRUE,
                                  value = TRUE))

  # Check if person data will be skiped
  if (hh_only == FALSE) {
    prs_file_df <- file.path(temp_dir,
                            grep(paste0("^pes\\d{4}"),
                                 list.files(temp_dir),
                                 ignore.case = TRUE,
                                 value = TRUE))

    prs_file_input <- file.path(temp_dir,
                               grep(paste0("^input.{1}pes\\d{4}.txt"),
                                    list.files(temp_dir),
                                    ignore.case = TRUE,
                                    value = TRUE))

    pnad <- pnad_read(hh_data = hh_file_df, hh_input = hh_file_input,
                      prs_data = prs_file_df, prs_input = prs_file_input,
                      vars = vars)
  } else {
    pnad <- pnad_read(hh_data = hh_file_df, hh_file_input, vars = vars)
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
