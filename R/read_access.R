#' Read table from a Microsoft Access database
#'
#' This has been successfully tested on \code{mdb} and \code{accdb} files
#' on Windows machines
#'
#' @param accdb character scalar of file path to Access database
#' @param table character scalar of table name in \code{accdb} to read
#' @param check.names logical scalar; if \code{TRUE} variable names are checked
#'  to ensure that they are syntactically valid and, if necessary, made so by
#'  \code{make.names}. See \code{read.table}
#' @export

read_access <- function(accdb, table, check.names = FALSE) {
  if (!requireNamespace("DBI", quietly = TRUE))
    install.packages("DBI")
  if (!requireNamespace("odbc", quietly = TRUE))
    install.packages("odbc")

  full_acc <- normalizePath(accdb, winslash = "/")
  con_string <- paste0("Driver={Microsoft Access Driver (*.mdb, *.accdb)};Dbq=",
                       full_acc)
  con <- DBI::dbConnect(odbc::odbc(),
                        .connection_string = con_string)
  df <- DBI::dbReadTable(con, table, check.names = check.names)
  DBI::dbDisconnect(con)
  return(df)
}
