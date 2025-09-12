#' Identifies the arquive type and read
#'
#' @param file the name of the file which the data are to be read from.
#' @returns estatelm return an object of class "estate.data.frame".
#' @export
#' @examples
#' data_csv <- readAvaliation(file = "~/pesquisa.csv")

readAvaliation <- function(file) {
  xlsx <- stringr::str_detect(file, pattern = "xlsx$")
  xls <- stringr::str_detect(file, pattern = "xls$")
  csv <- stringr::str_detect(file, pattern = "csv$")
  txt <- stringr::str_detect(file, pattern = "txt$")

  if (xlsx) {
    out_df <- readxl::read_xlsx(file)
  } else if (xls) {
    out_df <- readxl::read_xls(file)
  } else if (csv) {
    out_df <- read.csv(file)
  } else if (txt) {
    out_df <- read.table(file)
  } else {
    warning("Arquivo não pode ser lido use (.xlsx .xls .csv .txt)")
  }

  out_df <- as.data.frame(out_df)
  class(out_df) <- c("estate.data.frame", "data.frame")
  return(out_df)
}
