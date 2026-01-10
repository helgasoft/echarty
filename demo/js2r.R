#' Learn from ECharts examples - https://echarts.apache.org/examples/en/. 
#' This Shiny application helps translate JSON-like JavaScript data structures to R lists.
#' Additional information is available by clicking the "Info" button inside.
#' demo(js2r, package ='echarty')

devAskNewPage(ask = FALSE)
if (interactive()) {
  library(shiny)
  #shiny::runGist('https://gist.github.com/helgasoft/819035e853d9889ba02cb69ecc587f34', quiet=TRUE)  # problem Oct.2025
#  print(source('https://gist.githubusercontent.com/helgasoft/819035e853d9889ba02cb69ecc587f34/raw/1a017d9e99f3cfdafdaf266dce67c55f76eddffa/app.R'))
  
  url <- 'https://gist.github.com/helgasoft/819035e853d9889ba02cb69ecc587f34/download'
  filePath <- tempfile("shinyapp", fileext= ".zip")
  fileDir <- tempfile("shinyapp")
  download.file(url, filePath, mode='wb')
      on.exit(unlink(filePath))
  first <- as.character(utils::unzip(filePath, list = TRUE)$Name)[1]
  utils::unzip(filePath, exdir = fileDir)
  appdir <- file.path(fileDir, first)
  if (!utils::file_test("-d", appdir)) 
    appdir <- dirname(appdir)
  runApp(appdir)
}