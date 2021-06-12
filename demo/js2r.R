#' Learn from ECharts examples - https://echarts.apache.org/examples/en/. 
#' This Shiny application helps translate JSON-like JavaScript data structures to R lists.
#' Additional information is available by clicking the "Info" button inside.
#' demo(js2r, package ='echarty')

devAskNewPage(ask = FALSE)
library(shiny)
shiny::runGist('https://gist.github.com/helgasoft/819035e853d9889ba02cb69ecc587f34', quiet=TRUE)
