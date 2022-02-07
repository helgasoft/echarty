#' @section Package Conventions:
#' echarty is "pipe-friendly", supports both %>% and |> \cr
#' echarty functions have three prefixes to help with auto-completion: \itemize{
#' \item \emph{ec.} for general functions
#' \item \emph{ecr.} for rendering functions
#' \item \emph{ecs.} for Shiny functions
#' }
#' 
#' @section Global Options: 
#' Options are set with R command \href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/options}{options}.\cr
#' echarty uses the following options: \cr \itemize{
#'  \item echarty.theme = name of theme file (without extension), from folder /inst/themes
#'  \item echarty.font = font family name 
#'  \item echarty.urltiles = tiles URL template for leaflet maps 
#'  \item echarty.short = a boolean flag, see [ec.snip]
#' }
#' 
#' @examples
#' # basic scatter chart from a data.frame, using presets
#' cars |> ec.init()
#' 
#' # set/get global options
#' options('echarty.short'=TRUE)  # set
#' getOption('echarty.short')     # get
#' options('echarty.short'=NULL)  # remove
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
