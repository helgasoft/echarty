#' Learn from ECharts examples - https://echarts.apache.org/examples/en/. 
#' This Shiny application helps translate JSON-like JavaScript data structures to R lists.
#' Additional information is available by clicking the "Info" button inside.
#' demo(js2r, package ='echarty')

devAskNewPage(ask = FALSE)
if (interactive()) {
  #library(shiny)
  #shiny::runGist('https://gist.github.com/helgasoft/819035e853d9889ba02cb69ecc587f34', quiet=TRUE)  # problem Oct.2025
  print(source('https://gist.githubusercontent.com/helgasoft/02b257429e78e138f87aefce14f7aebc/raw/878dc34a7dd2ec239ea3aa96efecb75247db9e0a/app.R'))
}