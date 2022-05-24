#' 
#' @section Package Conventions:
#' 1. pipe-friendly - supports both %>% and |> \cr
#' 2. functions have three prefixes to help with auto-completion: \cr
#' * \emph{ec.} for general functions, like [ec.data]
#' * \emph{ecs.} for Shiny functions, like [ecs.output]
#' * \emph{ecr.} for rendering functions, like [ecr.band]
#' 
#' 
#' @section Global Options: 
#' Options are set with R command \href{https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/options}{options}.\cr
#' echarty uses the following options: \cr
#'  * echarty.theme = name of theme file (without extension), from folder /inst/themes
#'  * echarty.font = font family name 
#'  * echarty.urltiles = tiles URL template for leaflet maps 
#'  * echarty.short = a boolean flag, see [ec.snip]
#' 
#' 
#' @section Events:
#' Event handling is usually necessary only in Shiny. See code in [ec.examples] and `eshiny.R`, run as `demo(eshiny)`.\cr
#' echarty has two built-in event callbacks - `click` and `mouseover`. 
#' All other ECharts \href{https://echarts.apache.org/en/api.html#events}{events} should be initialized through `p$x$capture`.\cr
#' Another option is to use `p$x$on` with JavaScript handlers, see code in [ec.examples].
#' 
#' 
#' @section Widget parameters:
#' These are `htmlwidget` and `ECharts` initialization parameters supported by echarty.\cr
#' There are code samples for most in [ec.examples].
#' * capture = event name(s), to monitor events usually in Shiny
#' * on = define JavaScript code for event handling, see in \href{https://echarts.apache.org/en/api.html#echartsInstance.on}{ECharts}
#' * registerMap = define a map from a geoJSON file, see in \href{https://echarts.apache.org/en/api.html#echarts.registerMap}{ECharts}
#' * group = group-name of a chart, see in \href{https://echarts.apache.org/en/api.html#echartsInstance.group}{ECharts}
#' * connect = command to connect charts with same group-name, see in \href{https://echarts.apache.org/en/api.html#echarts.connect}{ECharts}
#' * locale = 'EN'(default) or 'ZH', set from `locale` parameter of [ec.init], see also in \href{https://echarts.apache.org/en/api.html#echarts.init}{ECharts}.
#' * renderer = 'canvas'(default) or `svg`, set from `renderer` in [ec.init], see also in \href{https://echarts.apache.org/en/api.html#echarts.init}{ECharts}.
#' * jcode = custom JavaScript code to execute, set from `js` parameter of [ec.init]
#' 
#' 
#' @section Code examples:
#' Here is the complete list of sample code locations \cr
#' * [ec.examples]
#' * command examples, like in [ec.init]
#' * Shiny code is in `eshiny.R`, run with `demo(eshiny)`
#' * searchable \href{https://helgasoft.github.io/echarty/gallery.html}{gallery} and \href{https://helgasoft.github.io/echarty/}{tutorials} on website
#' * searchable \href{https://gist.github.com/helgasoft}{gists}
#' * demos on \href{https://rpubs.com/echarty}{RPubs}
#' * answers to \href{https://github.com/helgasoft/echarty/issues}{Github issues}
#' \cr
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
#' 
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
## usethis namespace: end
NULL
