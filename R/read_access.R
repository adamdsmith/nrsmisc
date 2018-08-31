#' Read table from a Microsoft Access database
#'
#' This has been successfully tested on \code{mdb} and \code{accdb} files
#' on Windows machines
#'
#' @param accdb character scalar of file path to Access database
#' @param table character scalar of table name in \code{accdb} to read
#' @export

read_access <- function(accdb, table) {
  if (!requireNamespace("DBI", quietly = TRUE))
    install.packages("DBI")
  if (!requireNamespace("odbc", quietly = TRUE))
    install.packages("odbc")

  full_acc <- normalizePath(accdb, winslash = "/")
  con_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
                       full_acc)
  con <- DBI::dbConnect(odbc::odbc(),
                        .connection_string = con_string)
  df <- DBI::dbReadTable(con, table)
  DBI::dbDisconnect(con)
  return(df)
}
