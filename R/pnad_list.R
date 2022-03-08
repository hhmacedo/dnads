#' Lists available years or required files.
#'
#' @param year Selected year to return files.
#' @param files If TRUE, returns required files for a given year.
#'
#' @return A vector of years or files urls.
#' @export
#'
#' @examples
#' # Print years available
#' pnad_list()
#'
#' # List files for 2015
#' pnad_list(files = TRUE, year = 2015)
pnad_list <- function(year = NULL, files = FALSE) {

  # Check if files is a logical value
  stopifnot("files must be a logical value" = is.logical(files))

  # FTP link
  ibge_ftp <- paste0("ftp://ftp.ibge.gov.br/Trabalho_e_Rendimento/",
                     "Pesquisa_Nacional_por_Amostra_de_Domicilios_anual/",
                     "microdados/")

  # List available PNADs
  ibge_folders <- readLines( textConnection( RCurl::getURL( ibge_ftp ) ) )

  # Keep just the folder name
  ibge_folders <- stringr::str_extract(ibge_folders,"\\S+$")

  # Remove NA
  ibge_folders <- ibge_folders[!is.na(ibge_folders)]

  # Remove duplicates
  ibge_folders <- ibge_folders[!(ibge_folders %in% c("2011", "2012", "reponderacao_2001_2009"))]

  # Organize available PNADs as dataframe
  ibge_pnads <- data.frame(
    year = c(1976:1979, 1981:1990, 1992:1993, 1995:1999, 2013:2015, 2001:2009, 2011:2012),
    folder = paste0(rep(ibge_folders, c(rep(1, 24), 11)), "/")
  )
  rm(ibge_folders)
  ibge_pnads <- dplyr::arrange(ibge_pnads, year)

  # Make some PNADs unavailable
  ibge_pnads[ibge_pnads$year %in% c(1976:1979, 1981:1990, 1992:1993, 1995:1999), ]$folder <- NA

  # Check if a list of PNADs is the required result
  if (files == FALSE) {
    # Check if the year is empty for listing available PNADs
    stopifnot("files must be TRUE or the year must be empty" = is.null(year))

    warning("The survey was carried out for other years that are not available in this package.")

    return(ibge_pnads[!is.na(ibge_pnads$folder), ]$year)

  } else {
    # Check if the year is NULL
    stopifnot("to list required files, year shouldn't be empty" = !is.null(year))

    # Check if year is numeric
    stopifnot("year must be a number" = !is.na(suppressWarnings(as.numeric(year))))

    # In this case, check if the required result is a list of PNAD files

    stopifnot("this year is not available" = year %in% ibge_pnads$year)

    if (grepl("reponderacao", ibge_pnads[ibge_pnads$year == year,]$folder)) {
      ibge_year <- paste0(ibge_ftp,
                          ibge_pnads[ibge_pnads$year == year,]$folder)

      ibge_requiredfiles <-
        paste0(ibge_year,
               grep(paste0(year, ".*zip"),
                    stringr::str_extract(
                      readLines(textConnection(RCurl::getURL(ibge_year))),
                      "\\S+$"),
                    value = TRUE,
                    ignore.case = TRUE))

      rm(ibge_ftp, ibge_year)

      return(ibge_requiredfiles)

    } else {
      ibge_year <- paste0(ibge_ftp,
                          ibge_pnads[ibge_pnads$year == year,]$folder)

      ibge_requiredfiles <-
        paste0(ibge_year,
               grep("(dados|input).*zip",
                    stringr::str_extract(
                      readLines(textConnection(RCurl::getURL(ibge_year))),
                      "\\S+$"),
                    value = TRUE,
                    ignore.case = TRUE))

      rm(ibge_ftp, ibge_year)

      return(ibge_requiredfiles)
    }
  }
}
