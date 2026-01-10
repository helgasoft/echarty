#' Learn from echarty examples
#' Additional information is available by clicking the "Info" button inside.
#' From RStudio run 'demo(coder)'  after loading package 'echarty'

devAskNewPage(ask = FALSE)
if (interactive()) {
  # this stopped working - "cannot open URL"
  library(shiny)
  #shiny::runGist('https://gist.github.com/helgasoft/02b257429e78e138f87aefce14f7aebc')
  
  # this loads, but gist cannot be edited
  #print(source('https://gist.githubusercontent.com/helgasoft/02b257429e78e138f87aefce14f7aebc/raw/878dc34a7dd2ec239ea3aa96efecb75247db9e0a/app.R'))
  
  # this is from runGist code, download.file replacing shiny:::download
  url <- 'https://gist.github.com/helgasoft/02b257429e78e138f87aefce14f7aebc/download'
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