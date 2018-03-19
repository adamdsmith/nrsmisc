#' Read table from a Microsoft Access database
#'
#' This has been successfully tested on \code{mdb} and \code{accdb} files
#' on Windows machines
#'
#' @param accdb character scalar of file path to Access database
#' @param table character scalar of table name in \code{accdb} to read
#' @export

read_access <- function(accdb, table) {
  if (!requireNamespace("RODBC", quietly = TRUE))
    install.packages("RODBC")

  full_acc <- normalizePath(accdb, winslash = "/")
  tmp <- tempfile(fileext = ".R")
  tmp_rds <- suppressWarnings(
    normalizePath(tempfile(fileext = ".rds"), winslash = "/")
  )
  sink(file = tmp)
  cat(
    'options(stringsAsFactors = FALSE)\n',
    'chan <- RODBC::odbcConnectAccess2007("', full_acc, '")\n',
    'df <- RODBC::sqlFetch(chan, "', table, '")\n',
    'RODBC::odbcClose(chan)\n',
    'saveRDS(df, file = "', tmp_rds, '")\n', sep = '')
  sink()
  R32_path <- normalizePath(file.path(R.home(), "bin/i386/Rscript.exe"))
  system(paste(shQuote(R32_path),
               shQuote(tmp)),
         invisible = TRUE)
  df <- readRDS(tmp_rds)
  return(df)
}

