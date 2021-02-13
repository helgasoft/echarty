# ----------- General --------------

#' Initialize
#'
#' Initialize a chart.
#'
#' @param df A data.frame to be used as dataset in the chart, default NULL
#' @param load Name(s) of plugin(s) to load. Currently supported are 
#'    \emph{3D, GL, leaflet, custom}. Could be a vector or comma-delimited string. default NULL.
#' @param preset Enable (TRUE, default) or disable(FALSE) presets like xAxis,yAxis,dataset,series, but not \code{group1}.
#' @param group1 Preset parameters if df is grouped, default TRUE \cr
#'      If the grouping is on multiple columns, only the first one is used.
#' @param width,height A valid CSS unit (like \code{'100\%'},
#'   \code{'500px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId Id of the widget, default NULL
#' @param renderer \code{'canvas'} (default) or \code{'svg'}.
#' @param js A Javascript expression to evaluate, default NULL.
#' @param ... Any other arguments to pass to the widget.
#' @return A widget to plot, or to save and expand with more features.
#'
#' @details Widgets are defined in \href{https://www.htmlwidgets.org/develop_intro.html}{htmlwidgets}. 
#'  This command creates one with \code{\link[htmlwidgets]{createWidget}}, then adds some EchartsJS features to it.\cr
#'  It also presets values for xAxis,yAxis,dataset and series. The user can overwrite them if needed.
#' 
#' @examples
#'  # basic scatter chart from a data.frame, using presets
#'  cars %>% ec.init()
#'  
#' @import htmlwidgets
#'
#' @export
ec.init <- function( df = NULL, load = NULL,
  width = NULL, height = NULL, elementId = NULL, 
  renderer = 'canvas', preset = TRUE, group1 = TRUE, js = NULL, ...) {
  
  opts <- list(...)
  if (preset)
    opts <- append(opts, list(
      xAxis = list(list()),
      yAxis = list(list()),
      series = list(list())
    ))
  
  # forward widget options using x
  x <- list(
    theme = '',
    draw = TRUE,
    renderer = tolower(renderer),
    mapping = list(),
    events = list(),
    buttons = list(),
    eval = js,
    opts = opts
  )
  
  # user will most probably replace this preset
  # we use it as default for examples and testing
  if (preset)
    x$opts$series[[1]] <- list(type='scatter')
  
  if (!is.null(df)) {
    # data.frame given, assign to dataset
    if (!'data.frame' %in% class(df)) 
      stop('df must be a data.frame', call. = FALSE)
    
    # grouping uses transform - a v.5 feature
    if (group1 && dplyr::is.grouped_df(df)) {
      grnm <- dplyr::group_vars(df)[[1]]   # group1 means just 1st one
      df <- df %>% dplyr::relocate(grnm, .after = dplyr::last_col())
      x$opts$dataset <- list(list(source = ec.data(df)))
      grvals <- unname(unlist(dplyr::group_data(df)[grnm]))
      txfm <- list(); k <- 0
      x$opts$legend = list(data=list())
      for(i in grvals) { 
        k <- k+1
        srch4 <- i
        if ('factor' %in% class(grvals)) srch4 <- k
        txfm <- append(txfm, list(list(transform = list(
          type='filter', config=list(dimension=grnm, '='=srch4)))))
        x$opts$series[[k]] <- list(
          type='scatter', datasetIndex=k, name=i)
        x$opts$legend$data <- append(x$opts$legend$data, list(list(name=i)))
      }
      x$opts$dataset <- append(x$opts$dataset, txfm)
    } 
    else if (preset)
      x$opts$dataset <- list(list(source = ec.data(df)))
  }

  # create widget
  wt <- htmlwidgets::createWidget(
    name = 'echarty',
    x,
    width = width,
    height = height,
    package = 'echarty',
    elementId = elementId,
    preRenderHook = .preRender,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = '100%',
      knitr.figure = FALSE,
      browser.fill = TRUE, padding = 0
    )
  )

  # check for theme
  theme <- getOption('ECHARTS_THEME')   # default
  if (!is.null(theme)) {
    wt <- ec.theme(wt, theme)
  }
  
  if (is.null(load)) return(wt)

  if (length(load)==1 && grepl(',', load, fixed=TRUE))
      load <- unlist(strsplit(load, ','))
      
  if ('3D' %in% load && preset) {
      wt$x$opts$xAxis <- NULL
      wt$x$opts$yAxis <- NULL
      wt$x$opts$series[[1]] <- NULL
      wt$x$opts$grid3D <- list(list())
      wt$x$opts$xAxis3D <- list(list())
      wt$x$opts$yAxis3D <- list(list())
      wt$x$opts$zAxis3D <- list(list())
  }
  
  path <- system.file('js', package = 'echarty')
  dep <- NULL
  if ('leaflet' %in% load) {
    if (preset) {
      # customize for leaflet
      wt$x$opts$dataset <- NULL  # dataset not suitable, data must be in series
      wt$x$opts$xAxis <- NULL
      wt$x$opts$yAxis <- NULL
      urltls <- getOption('ECHARTS_TILES')
      if (is.null(urltls))
        urltls <- 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
      wt$x$opts$leaflet = list(
        roam = TRUE, 
        tiles = list( list(urlTemplate = urltls))
      )
  
      # note: leaflet user data should be ordered (lon,lat)
      if (!is.null(df))
        wt$x$opts$series[[1]]$data <- ec.data(df, TRUE)
      wt$x$opts$series[[1]]$coordinateSystem <- 'leaflet'
    }
    
    dep <- htmltools::htmlDependency(
      name = 'echarts-leaflet', 
      version = '1.0.0', src = c(file = path), 
      script='echarts-leaflet.js')
    wt$dependencies <- append(wt$dependencies, htmlwidgets::getDependency('leaflet'))
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  ## will be implemented later as manual or dynamic load on-demand, world.js is too big to include in echarty
  # if ('geo' %in% load) {
  #   dep <- htmltools::htmlDependency(
  #     name = 'world',
  #     version = '1.0.0', src = c(file = path),
  #     script = 'world.js'
  #   )
  #   wt$dependencies <- append(wt$dependencies, list(dep))
  # }
  if ('GL' %in% load) {
    dep <- htmltools::htmlDependency(
      name = 'gl',
      version = '2.0.1', src = c(file = path),
      script = 'echarts-gl.min.js'
    )
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  if ('custom' %in% load) {
    dep <- htmltools::htmlDependency(
      name = 'renderers', version = '1.0.2', 
      src = c(file = path), script = 'renderers.js')
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  
  return(wt)
}


#' Get an EchartsJS dataset from a data.frame
#' 
#' @param df Chart data in data.frame format, required
#' @param series If FALSE, data is prepared for \href{https://echarts.apache.org/en/option.html#dataset.source}{dataset} (default),\cr
#'  if TRUE, data is for \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series} 
#' @return A list for \emph{dataset.source} or \emph{series.data}. The latter does not include column names.
#'
#' @export
ec.data <- function(df, series=FALSE) {
  if (missing(df))
    stop('expecting df as data.frame', call. = FALSE)
  if (!'data.frame' %in% class(df))
    stop('df has to be data.frame', call. = FALSE)
  
  # TODO: replace purrr with something simpler
  tmp <- lapply(lapply(purrr::transpose(df), unname), 
                function(x) unlist(purrr::flatten(x)) )
  datset <- c(list(colnames(df)), tmp)
  
  if (isTRUE(series)) {   # change format
    datset <- lapply(tmp, function(x) list(value=x))
  }
  return(datset)
}



#' Band
#' 
#' Add a new 'custom' serie with coordinates of a polygon.
#' 
#' @param df A data.frame with lower and upper numerical columns.
#' @param lower The column name of band's lower boundary, a string.
#' @param upper The column name of band's upper boundary, a string.
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-line.type}{serie}
#' @return One serie list
#'
#' @details The coordinates of the two boundaries are merged into a polygon and displayed as one.
#' @export
ec.sband <- function(df=NULL, lower=NULL, upper=NULL, ...) {
  if (is.null(df) || is.null(lower) || is.null(upper)) 
    stop('df, lower and upper are all required', call. = FALSE)
  if (!'data.frame' %in% class(df)) 
    stop('df must be a data.frame', call. = FALSE)
  
  ld <- nrow(df[upper])
  l2 <- unname(unlist(df[upper])[order(ld:1)])  		# reverse
  tmp <- data.frame(x=c(df[1:ld,1],df[ld:1,1]), y=c(df[lower][[1]], l2))
  
  serie <- list(
    type = 'custom', 
    renderItem = htmlwidgets::JS('riPolygon'),
    data = ec.data(tmp, TRUE),
    ...
  )
  if (is.null(serie$itemStyle))
    serie$itemStyle = list(borderWidth=0.5)
  serie
}


# ----------- Shiny --------------
#'
#' Shiny: UI chart
#' 
#' Placeholder for a chart in Shiny UI
#' 
#' @param outputId Name of output UI element.
#' @param width,height Must be a valid CSS unit (like \code{'100\%'},
#'   \code{'400px'}, \emph{'auto'}) or a number, which will be coerced to a
#'   string and have \emph{'px'} appended.
#' @return An output or render function that enables the use of the widget within Shiny applications. 
#'
#' @seealso [ecs.exec] for example, \code{\link[htmlwidgets]{shinyWidgetOutput}} for return value.
#' 
#' @export
ecs.output <- function(outputId, width = '100%', height = '400px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'echarty', width, height, package = 'echarty')
}


#' Shiny: Plot command to render chart 
#' 
#' This is the initial rendering of a chart in the UI.
#' 
#' @param expr An \code{echarty} widget to generate the chart.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression? default FALSE.
#' @return An output or render function that enables the use of the widget within Shiny applications.
#'   
#' @seealso [ecs.exec] for example, \code{\link[htmlwidgets]{shinyWidgetOutput}} for return value.
#' 
#' @export
ecs.render <- function(expr, env=parent.frame(), quoted=FALSE) {
  if (!quoted) {
    expr <- substitute(expr)  # do not add ',env'
  } # force quoted
  htmlwidgets::shinyRenderWidget(expr, ecs.output, env, quoted = TRUE)
}


#' Shiny: Create a proxy
#' 
#' Create a proxy for an existing chart in Shiny UI. It allows to
#' add, merge, delete elements to a chart without reloading it.
#' 
#' @param id Target chart id from the Shiny UI.
#' @return A proxy object to update the chart.
#' 
#' @seealso [ecs.exec] for example.
#' 
#' @export
ecs.proxy <- function(id) {
  proxy <- list(id = id, session = shiny::getDefaultReactiveDomain())
  class(proxy) <- 'ecsProxy'
  return(proxy)
}


#' Shiny: Execute a proxy command
#' 
#' Once chart changes had been made, they need to be sent back to the widget for display
#'
#' @param proxy A [ecs.proxy] object
#' @param cmd Name of command, default is \emph{p_merge}\cr
#'   Other proxy commands:\cr
#' \emph{p_update} - add new series and axes\cr
#' \emph{p_merge} - add serie features like marks\cr
#' \emph{p_del_serie} - delete a serie by index or name\cr
#' \emph{p_del_marks} - delete marks of a serie\cr
#' \emph{p_append_data} - add data to existing series\cr
#' \emph{p_dispatch} - send action commands, see \href{https://echarts.apache.org/en/api.html#echartsInstance.dispatchAction}{documentation}
#' @return A proxy object to update the chart.
#' 
#' @seealso [ecs.proxy], [ecs.render], [ecs.output]
#' 
#' @examples
#' if (interactive()) {
#' 
#' library(shiny)
#' runApp( list(
#'   ui = fluidPage(
#'     ecs.output('plot'),
#'     actionButton('addm', 'Add marks'),
#'     actionButton('delm', 'Del area+line marks'), HTML('&nbsp; &nbsp; &nbsp; &nbsp;'),
#'     actionButton('adds', 'Add serie'),
#'     actionButton('dels', 'Del serie'), HTML('&nbsp; &nbsp; &nbsp; &nbsp;'),
#'     actionButton('hilit', 'Highlight'),
#'     actionButton('dnplay', 'Downplay')
#'   ),
#'   server = function(input, output, session){
#' 
#'     output$plot <- ecs.render({
#'       e <- mtcars %>% group_by(cyl) %>% ec.init()
#'       e$x$opts$tooltip <- list(list(show=TRUE))
#'       e$x$opts$series[[1]]$emphasis <- list(focus='series', blurScope='coordinateSystem')
#'       e
#'     })
#' 
#'     observeEvent(input$addm, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts$series[[1]] = list(
#'         markPoint = list(data = list(
#'           list(coord = c(22.5, 140.8)),
#'           list(coord = c(30.5, 95.1))
#'         ), itemStyle = list(color='lightblue')
#'         )
#'         ,markArea = list(data = list(list(
#'           list(xAxis = 15),
#'           list(xAxis = 25)
#'         ))
#'         ,silent=TRUE
#'         ,itemStyle = list(color='pink', opacity=0.2)
#'         ,label = list(formatter='X-area', position='insideTop')
#'         )
#'         ,markLine = list(data = list(list(type='average')))
#'       )
#'       e %>% ecs.exec() #' ='p_merge'
#'     })
#'     observeEvent(input$adds, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts$series[[1]] <- list(
#'         type = 'line', name = 'newLine',
#'         encode = list(x='mpg', y='disp')
#'       )
#'       e %>% ecs.exec('p_update')
#'     })
#'     observeEvent(input$dels, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts$seriesName <- 'newLine'
#'       #'e$x$opts$seriesIndex <- 4  #' alternative ok
#'       e %>% ecs.exec('p_del_serie')
#'     })
#'     observeEvent(input$delm, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts$seriesIndex <- 1
#'       e$x$opts$delMarks <- c('markArea','markLine')
#'       e %>% ecs.exec('p_del_marks')
#'     })
#'     observeEvent(input$hilit, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts <- list(type='highlight', seriesName='4')
#'       e %>% ecs.exec('p_dispatch')
#'     })
#'     observeEvent(input$dnplay, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts <- list(type='downplay', seriesName='4')
#'       e %>% ecs.exec('p_dispatch')
#'     })
#'   }
#' ))
#' }
#' 
#' @export
ecs.exec <- function(proxy, cmd='p_merge') {

  if (missing(proxy))
    stop('missing proxy', call. = FALSE)
  if (!'ecsProxy' %in% class(proxy)) 
    stop('must pass ecsProxy object', call. = FALSE)
  if (is.null(proxy$x) || is.null(proxy$x$opts))
    stop('proxy is empty', call. = FALSE)
  
  plist <- list(id = proxy$id, 
                opts = proxy$x$opts,
                action = cmd)
  
  # create web dependencies for JS, if present
  if (!is.null(proxy$dependencies)) {
    deps <- list(shiny::createWebDependency(
      htmltools::resolveDependencies( proxy$dependencies )[[1]]
    ))
    plist$deps <- deps
  }
  
  proxy$session$sendCustomMessage('kahuna', plist)
  return(proxy)
}


# ----------- Utilities ----------------------

#' Global options
#' 
#' Set a global theme, font and/or tile URL
#'
#' @param options A list of options: \cr
#'     theme = name of theme file (without extension), from folder /inst/themes\cr
#'     font = font family name \cr
#'     urltiles = tiles URL template for leaflet maps
#' @return none, setting values only
#' 
#' @details To get these values in code use \code{\link[base]{getOption}}. 
#' Revert back to default by setting them to NULL.
#' More list components could be added in the future.
#'
#' @examples
#' ec.global(list(theme = 'dark'))
#' cars %>% ec.init()
#' ec.global(list(theme = NULL))
#' cars %>% ec.init()
#' 
#' @export
ec.global <- function(options = NULL) {
  if (is.null(options)) return()
  options(
    'ECHARTS_THEME' = options$theme,
    'ECHARTS_FONT' = options$font,
    'ECHARTS_TILES' = options$urltiles
  )
}


#' Themes
#'
#' Apply a pre-built theme to a chart
#'
#' @param e An \code{echarty} widget as returned by [ec.init]
#' @param name Name of existing theme file (without extension), or name of custom theme defined in \code{code}.
#' @param code Custom theme code in JSON format, default NULL.
#' @return An \code{echarty} widget.
#'
#' @details Just a few themes are included in folder 'inst/themes'. The entire collection could be found \href{https://github.com/apache/echarts/tree/master/theme}{here} and copied if needed.
#'
#' @examples
#' mtcars %>% ec.init() %>% ec.theme('dark-mushroom')
#' cars %>% ec.init() %>% ec.theme('mine', code=
#'   '{"color": ["green","#eeaa33"], 
#'     "backgroundColor": "lemonchiffon"}')
#' 
#' @export
ec.theme <- function (e, name, code = NULL) 
{
  if (missing(name))
    stop('must define theme name', call. = FALSE)

  e$x$theme <- name
  if (!is.null(code))
    e$x$themeCode <- code
  else {
    e$x$themeCode <- NULL
    path <- system.file('themes', package = 'echarty')
    dep <- htmltools::htmlDependency(
      name = name,
      version = '1.0.0', src = c(file = path),
      script = paste0(name, '.js'))
    e$dependencies <- append(e$dependencies, list(dep))
  }
  e
}


#' Convert chart to JSON
#' 
#' @param e An \code{echarty} widget as returned by [ec.init]
#' @param json Whether to return a JSON, or a \code{list}, default TRUE
#' @param ... Additional options to pass to \code{\link[jsonlite]{toJSON}}
#' @return A JSON string if \code{json} is \code{TRUE} and
#'  a \code{list} otherwise.
#'
#' @note Must be passed as last option.
#'
#' @examples
#' # extract JSON
#' json <- cars %>% ec.init() %>% ec.inspect()
#' json
#'
#' # get from JSON and modify plot
#' ec.fromJson(json) %>% ec.theme('macarons')
#'
#' @export
ec.inspect <- function(e, json=TRUE, ...) {
  opts <- e$x$opts
  
  if (!isTRUE(json)) return(opts)
  params <- list(...)
  if ('pretty' %in% names(params)) 
    opts <- jsonlite::toJSON(opts, force=TRUE, auto_unbox=TRUE, 
            null='null', ...)
  else
    opts <- jsonlite::toJSON(opts, force=TRUE, auto_unbox=TRUE, 
            null='null', pretty=TRUE, ...)

  return(opts)
}


#' Convert JSON string to chart
#' 
#' @param txt JSON character string, url, or file, see \code{\link[jsonlite]{fromJSON}}
#' @return An \code{echarty} widget.
#' 
#' @details \code{txt} should contain the full list of options required to build a chart.
#' It is subsequently passed to EchartsJS function \href{https://echarts.apache.org/en/api.html#echartsInstance.setOption}{setOption}.
#' 
#' @export
ec.fromJson <- function(txt) {
  json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  
  e <- ec.init()
  e$x$opts <- json
  e
}



# ----------- Internal --------------

# needed by widget init
.preRender <- function(e) {

  ff <- getOption('ECHARTS_FONT')
  
  if (!is.null(ff))
    e$x$opts$textStyle <- list(fontFamily = ff)
  e
}

# for Shiny actions
.onAttach <- function(libname, pkgname) {
  shiny::registerInputHandler('echartyParse', function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)
  
  options(
    'ECHARTS_THEME' = NULL,
    'ECHARTS_FONT' = NULL,
    'ECHARTS_TILES' = NULL
  )
}

.onLoad <- function(libname, pkgname) {
  shiny::registerInputHandler('echartyParse', function(data, ...) {
    jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
  }, force = TRUE)
  
  options(
    'ECHARTS_THEME' = NULL,
    'ECHARTS_FONT' = NULL,
    'ECHARTS_TILES' = NULL
  )
}

#' Pipe operator
#'
#' Imported from magrittr
#'
#' @name %>%
#' @return A value of the lhs (left-hand-side) object prepared for rhs (right-hand-side) function/call.
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
NULL


#  ------------- Licence -----------------
#'
#' Original work Copyright 2018 John Coene
#' 
#' Modified work Copyright 2021 Larry Helgason
#' 
#' Licensed under the Apache License, Version 2.0 (the "License");
#' you may not use this file except in compliance with the License.
#' You may obtain a copy of the License at
#' 
#' http://www.apache.org/licenses/LICENSE-2.0
#' 
#' Unless required by applicable law or agreed to in writing, software
#' distributed under the License is distributed on an "AS IS" BASIS,
#' WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
#' See the License for the specific language governing permissions and
#' limitations under the License.
#' ---------------------------------------

