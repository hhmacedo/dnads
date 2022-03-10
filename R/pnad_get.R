#' Download PNAD files and read them.
#'
#' @param year A year must be provided.
#' @param design TRUE if you want a survey object, FALSE if you want a dataframe.
#' @param vars Selected variables
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
pnad_get <- function(year, design = TRUE, vars = NULL) {

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

  # Import list of all variables
  if (!is.null(vars)) {
    input_pes_file <- file.path(temp_dir,
                                grep(paste0("^input.{1}pes\\d{4}.txt"),
                                     list.files(temp_dir),
                                     ignore.case = TRUE,
                                     value = TRUE))
    input_dom_file <- file.path(temp_dir,
                                grep(paste0("^input.{1}dom\\d{4}.txt"),
                                     list.files(temp_dir),
                                     ignore.case = TRUE,
                                     value = TRUE))

    input_pes_vars <- dplyr::pull(sas_import(input_pes_file, year = year), "name")
    input_dom_vars <- dplyr::pull(sas_import(input_dom_file, year = year), "name")
    all_variables <- c(input_dom_vars, input_pes_vars)

    rm(input_pes_file, input_dom_file, input_dom_vars, input_pes_vars)

    # Check if all variables are ok and stop if not
    if (!all(vars %in% all_variables)) {
      missing_vars <- vars[!(vars %in% all_variables)]
      stop(paste0("The package couldn't find the following vars:\n  ",
                 paste(missing_vars, collapse = ", "), "."))
    }
  }

  # Read data
  dom <- NULL
  pes <- NULL
  for (data in c("pes", "dom")) {
    file_df <- file.path(temp_dir,
                         grep(paste0("^", data, "\\d{4}"),
                              list.files(temp_dir),
                              ignore.case = TRUE,
                              value = TRUE))

    file_input <- file.path(temp_dir,
                            grep(paste0("^input.{1}", data, "\\d{4}.txt"),
                                 list.files(temp_dir),
                                 ignore.case = TRUE,
                                 value = TRUE))

    assign(data,
           pnad_read(data = file_df, input = file_input, vars = vars))
  }
  rm(data, file_df, file_input)

  # Remove used files
  file.remove(list.files(temp_dir, full.names = TRUE))

  # Merge households with persons
  pnad <- dplyr::inner_join(dom, pes, by = c("V0101", "UF", "V0102", "V0103"))
  rm(dom, pes)

  invisible(gc())

  # Check if must return a survey object
  if (design == TRUE) {
    pnad <- pnad_design(pnad)
  }

  return(pnad)
}
