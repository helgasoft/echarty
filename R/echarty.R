# ----------- General --------------

#' Initialize command
#'
#' Required to build a chart. In most cases this will be the only command necessary.
#'
#' @param df A data.frame to be preset as \href{https://echarts.apache.org/en/option.html#dataset}{dataset}, default NULL \cr
#'     For crosstalk df should be of type \code{\link[crosstalk]{SharedData}}.
#' @param group1 Type of grouped series, or type of first ungrouped serie. Default is 'scatter'. Set to NULL to disable. \cr
#'     If the grouping is on multiple columns, only the first one is used.
#' @param preset Disable(FALSE) or enable (TRUE, default) presets xAxis,yAxis,serie for 2D, or grid3D,xAxis3D,yAxis3D,zAxis3D for 3D.
#' @param load Name(s) of plugin(s) to load. Could be a character vector or comma-delimited string. default NULL.\cr
#'   Built-in plugins: \cr \itemize{
#'   \item leaflet - Leaflet maps with customizable tiles, see \href{https://github.com/gnijuohz/echarts-leaflet#readme}{source}\cr
#'   \item custom - renderers for [ecr.band] and [ecr.ebars] \cr 
#'  } Plugins with one-time installation (popup prompt): \cr \itemize{
#'   \item 3D - 3D charts and WebGL acceleration, see \href{https://github.com/ecomfe/echarts-gl}{source} and \href{https://echarts.apache.org/en/option-gl.html#series}{docs} \cr
#'   \item world - world map with country boundaries, see \href{https://github.com/apache/echarts/tree/master/test/data/map/js}{source} \cr
#'   \item liquid - liquid fill, see \href{https://github.com/ecomfe/echarts-liquidfill}{source}  \cr
#'   \item gmodular - graph modularity, see \href{https://github.com/ecomfe/echarts-graph-modularity}{source}  \cr
#'   \item wordcloud - cloud of words, see \href{https://github.com/ecomfe/echarts-wordcloud}{source} \cr
#'  } Install you own plugins with [ec.plugjs].
#' @param width,height A valid CSS unit (like \code{'100\%'},
#'   \code{'500px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param elementId Id of the widget, default NULL
#' @param renderer \code{'canvas'} (default) or \code{'svg'}.
#' @param js A Javascript expression to evaluate, default NULL.
#' @param ... Any other arguments to pass to the widget.
#' @return A widget to plot, or to save and expand with more features.
#'
#' @details  Command \emph{ec.init} creates a widget with \code{\link[htmlwidgets]{createWidget}}, then adds some ECharts features to it.\cr
#'  When \emph{ec.init} is chained after a data.frame, a \href{https://echarts.apache.org/en/option.html#dataset}{dataset} is preset. \cr
#'  When the data.frame is grouped and \emph{group1} is not null, more datasets with legend and series are also preset. Grouped series are preset as type \emph{scatter}. \cr
#'  Plugin '3D' presets will not work for 'scatterGL'. Instead, use \emph{preset=FALSE} and set explicitly \emph{xAxis,yAxis}. \cr
#'  Users can delete or overwrite any presets as needed. \cr
#' 
#' @examples
#'  # basic scatter chart from a data.frame, using presets
#'  cars %>% ec.init()
#'  
#' @importFrom htmlwidgets createWidget sizingPolicy getDependency JS shinyWidgetOutput shinyRenderWidget
#' @importFrom crosstalk is.SharedData crosstalkLibs
#' 
#' @export
ec.init <- function( df = NULL, group1 = 'scatter', preset = TRUE, load = NULL,
                     js = NULL, width = NULL, height = NULL, elementId = NULL, 
                     renderer = 'canvas', ...) {
  
  opts <- list(...)
  
  # presets are used as default for examples and testing
  # user can also ignore or replace them
  if (preset) {
    if (!('xAxis' %in% names(opts))) opts$xAxis <- list(ey='')
    if (!('yAxis' %in% names(opts))) opts$yAxis <- list(ey='')
    if (!('series' %in% names(opts))) opts$series <- list(
    	list(type=if (is.null(group1)) 'scatter' else group1) )
    #opts$series[[1]] <- list(type='scatter')
  }

  
  if (crosstalk::is.SharedData(df)) {
    # Using Crosstalk
    key <- df$key()
    group <- df$groupName()
    df <- df$origData()
    deps <- crosstalk::crosstalkLibs()
  } else {
    # Not using Crosstalk
    key <- group <- deps <- NULL
  }
  
  # forward widget options using x
  x <- list(
    theme = '',
    draw = TRUE,
    renderer = tolower(renderer),
    mapping = list(),
    events = list(),
    buttons = list(),
    eval = js,
    opts = opts,
    settings = list(
      crosstalk_key = key,
      crosstalk_group = group
    )
  )
  
  if (!is.null(df)) {
    # if data.frame given, assign to dataset regardless of parameter 'preset'
    if (!'data.frame' %in% class(df)) 
      stop('df must be a data.frame', call. = FALSE)
    
    # grouping uses transform - a v.5 feature
    if (!is.null(group1) && dplyr::is.grouped_df(df)) {
      grnm <- dplyr::group_vars(df)[[1]]   # group1 means just 1st one
      x$opts$dataset <- list(list(source = ec.data(df)))
      grvals <- unname(unlist(dplyr::group_data(df)[grnm]))
      txfm <- list(); k <- 0
      x$opts$legend = list(data=list())
      for(i in grvals) { 
        k <- k+1
        #srch4 <- i
        #if ('factor' %in% class(grvals)) srch4 <- k
        txfm <- append(txfm, list(list(transform = list(
          type='filter', config=list(dimension=grnm, '='=i)))))
        x$opts$series[[k]] <- list(
          type=group1, datasetIndex=k, name=as.character(i))
        x$opts$legend$data <- append(x$opts$legend$data, list(list(name=as.character(i))))
      }
      x$opts$dataset <- append(x$opts$dataset, txfm)
    } 
    else 
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
    ),
    dependencies = deps
  )

  # check for theme
  theme <- getOption('ECHARTS_THEME')   # default
  if (!is.null(theme)) {
    wt <- ec.theme(wt, theme)
  }
  
  if (is.null(load)) return(wt)

  if (length(load)==1 && grepl(',', load, fixed=TRUE))
      load <- unlist(strsplit(load, ','))
      
  path <- system.file('js', package = 'echarty')
  dep <- NULL
  if ('leaflet' %in% load) {
    if (preset) {
      # customizations for leaflet
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
  
      # leaflet user data should be ordered (lon,lat)!
      if (!is.null(df))
        wt$x$opts$series[[1]]$data <- ec.data(df, 'values')
      wt$x$opts$series[[1]]$coordinateSystem <- 'leaflet'
    }
    
    dep <- htmltools::htmlDependency(
      name = 'echarts-leaflet', 
      version = '1.0.0', src = c(file = path), 
      script='echarts-leaflet.js')
    wt$dependencies <- append(wt$dependencies, htmlwidgets::getDependency('leaflet'))
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  if ('custom' %in% load) {
    dep <- htmltools::htmlDependency(
      name = 'renderers', version = '1.0.2', 
      src = c(file = path), script = 'renderers.js')
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  
  # Plugins implemented as dynamic load on-demand
  if ('3D' %in% load) {
    if (preset) {
      wt$x$opts$xAxis <- NULL   # replace 2D presets with 3D
      wt$x$opts$yAxis <- NULL
      wt$x$opts$series[[1]] <- NULL
      wt$x$opts$grid3D  <- list(list())  # todo list(ey='')?
      wt$x$opts$xAxis3D <- list(list())
      wt$x$opts$yAxis3D <- list(list())
      wt$x$opts$zAxis3D <- list(list())
    }
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-gl@2.0.4/dist/echarts-gl.min.js')
  }
  if ('world' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts@4.9.0/map/js/world.js')
  
  if ('liquid' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-liquidfill@3.0.0/dist/echarts-liquidfill.min.js')
  
  if ('gmodular' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-graph-modularity@2.0.0/dist/echarts-graph-modularity.min.js')
  
  if ('wordcloud' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-wordcloud@2.0.0/dist/echarts-wordcloud.min.js')
  
  # load unknown installed plugins
  unk <- load[! load %in% c('leaflet','custom','3D','world','liquid','gmodular','wordcloud')]
  for(p in unk) wt <- ec.plugjs(wt, p)
  
  return(wt)
}


#' Install Javascript plugin from URL source
#' 
#' @param wt A widget to add dependency to, see \code{\link[htmlwidgets]{createWidget}}
#' @param source URL of the uninstalled Javascript plugin, \cr
#'   or name of an installed plugin file, suffix .js included. Default is NULL.
#' @return A widget with JS dependency added if successful, otherwise input wt
#'
#' @details When \emph{source} is URL, the plugin file is installed with a popup prompt to the user.\cr
#'   When \emph{source} is just a file name (xxx.js), it is assumed installed and only a dependency is added. The latter option is for internal usage by echarty.
#'   
#' @examples
#' # import map plugin and display two (lon,lat) locations
#' p <- ec.init() %>% ec.plugjs(
#'   'https://raw.githubusercontent.com/apache/echarts/master/test/data/map/js/china-contour.js')
#' p$x$opts <- list(
#'   geo = list(map='china-contour', roam=TRUE),
#'   legend = list(data = list(list(name = 'Geo'))),
#'   series = list(list( name = 'Geo',
#'     type = 'scatter', coordinateSystem = 'geo',
#'     symbolSize = 9, itemStyle = list(color='red'),
#'     data = list(list(value=c(113, 40)), list(value=c(118, 39))) ))
#' )
#' p
#'
#' @importFrom utils askYesNo download.file
#'
#' @export
ec.plugjs <- function(wt=NULL, source=NULL) {
  if (missing(wt))
    stop('ec.plugjs expecting widget', call. = FALSE)
  if (is.null(source)) return(wt)
  fname <- basename(source)
  if (!endsWith(fname, '.js'))
    stop('ec.plugjs expecting .js suffix', call. = FALSE)
  path <- system.file('js', package = 'echarty')
  ffull <- paste0(path,'/',fname)
  if (!file.exists(ffull)) {
    if (!startsWith(source, 'http'))
      stop('ec.plugjs expecting URL source', call. = FALSE)
    prompt <- paste0('One-time installation of plugin\n',fname,'\n Would you like to proceed ?')
    ans <- FALSE
    if (interactive())
      ans <- askYesNo(prompt)
    if (is.na(ans)) ans <- FALSE  # was cancelled
    if (ans) {
      try(withCallingHandlers(
        download.file(source, ffull), # method = "libcurl"),
        warning = function(w) {
          cat('ec.plugjs Error:', sub(".+HTTP status was ", "", w))
          ans <- FALSE
        }),
        silent = TRUE)
    } 
    if (!ans) return(wt)
  }
  dep <- htmltools::htmlDependency(
    name = fname, version = '1.0.0', src = c(file = path),
    script = fname
  )
  wt$dependencies <- append(wt$dependencies, list(dep))
  return(wt)
}


#' Data helper
#' 
#' Make ECharts data from a data.frame
#' 
#' @param df Chart data in data.frame format, required. 
#' @param format A key on how to format the output list \cr \itemize{
#'  \item 'dataset' = list to be used in \href{https://echarts.apache.org/en/option.html#dataset.source}{dataset} (default), or in \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} but without a header. \cr
#'  \item 'values' = list for customized \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} \cr
#'  \item 'names' = named lists useful for named data like \href{https://echarts.apache.org/en/option.html#series-sankey.links}{sankey links}.
#'  }
#' @param header Whether the data will have a header with column names or not, default TRUE. Set this to FALSE when used in \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data}.
#' @return A list for \emph{dataset.source}, \emph{series.data} or a list of named lists.
#'
#' @export
ec.data <- function(df, format='dataset', header=TRUE) {
  if (missing(df))
    stop('expecting df as data.frame', call. = FALSE)
  if (!'data.frame' %in% class(df))
    stop('df has to be data.frame', call. = FALSE)
  
  # TODO: replace purrr with something simpler
  #tmp <- purrr::transpose(df)       # named lists
  rownames(df) <- NULL
  tmp <- apply(df, 1, function(x) {
    out <- list()
    i <- 1
    for(n in colnames(df)) { out[n] <- x[i]; i <- i+1 }
    out 
  })
  if (format=='dataset') {
    datset <- lapply(tmp, unname)
    if (header)
      datset <- c(list(colnames(df)), datset)
  } else if (format=='values' || isTRUE(format)) {
    datset <- lapply(tmp, function(x) list(value=unlist(unname(x))))
  } else   # ='names'
    datset <- tmp

  return(datset)
}



#' Timeline by groups
#' 
#' Helper function to build timeline data for a grouped data.frame
#' 
#' @param wt An \code{echarty} widget as returned by [ec.init]
#' @param df A grouped data.frame, required
#' @param scol Vector of column names(strings) or indexes for series, required
#' @param xcol Column name or index for the X-axis. Default (NULL) will set X-axis to consecutive numbers.
#' @param type The series type, default is \emph{'line'}
#' @param ... Additional attributes to series
#' @return A widget with timeline and options added
#' 
#' @details Timeline axisType is set to 'category'. \cr
#'   Option titles are built and displayed, user could remove them later.
#' 
#' @examples
#'
#' p <- ec.init() %>% 
#'   ec.timegrp(iris %>% dplyr::group_by(Species), 
#'              c('Sepal.Width','Petal.Length'),
#'              markPoint = list(data=list(list(type='max'),
#'                                         list(type='min'))) ) 
#' p$x$opts$legend <- list(list())  # add legend
#' p
#'
#' @import dplyr
#' 
#' @export
ec.timegrp <- function(wt, df=NULL, scol=NULL, xcol=NULL, type='line', ...) {
  if (!is.grouped_df(df))
    stop('df must be a grouped data.frame', call. = FALSE)
  if (is.null(scol))
    stop('scol is required', call. = FALSE)
  # find group column(s)
  gvars <- df %>% group_vars()
  df[gvars] <- lapply(df[gvars], as.character)  # convert factors
  opt <- lapply(df %>% group_split(), function(y) {
    # nicer looking lines
    if (!is.null(xcol)) y <- y %>% arrange(across(all_of(xcol)))
    series <- lapply(y[,scol], function(sr) {
      dt <- list(); 
      for(i in 1:length(sr)) { 
        xx <- if (!is.null(xcol)) unname(y[i,xcol]) else i
        dt <- append(dt, list(c(xx, sr[i]))) 
      }
      list(type=type, data=dt, ...) 
    })
    # set series names for legend
    series <- lapply(1:length(series), function(i) {
      series[[i]]$name <- names(series[i]); series[[i]] })
    list(title = list(text = as.character(unique(y[,gvars])), left=40), 
         series = unname(series))
  })
  wt$x$opts$options <- opt
  
  steps <- lapply(opt, function(x) { paste(x$title$text,collapse=' ') })
  wt$x$opts$timeline <- list(data=steps, axisType='category')
  wt
}



#' Charts layout
#' 
#' Set multiple charts in rows/columns format
#' 
#' @param plots A list of charts
#' @param rows Number of rows
#' @param cols Number of columns
#' @param width Width of columns, one of xs, md, lg
#' @param title Title for the set
#' @return A container \code{\link[htmltools]{div}} in rmarkdown, otherwise \code{\link[htmltools]{browsable}}
#' @examples
#' 
#' p1 <- cars %>% ec.init()
#' p2 <- cars %>% ec.init() %>% ec.theme('dark')
#' ec.layout(list(p1,p2), cols=2 )
#' 
#' @export 
ec.layout <- function (plots, rows = NULL, cols = NULL, width = "xs", 
                       title = NULL) 
{
  if (!is.list(plots))
    stop('ec.layout charts must be a list', call. = FALSE)
  if (is.null(rows) & !is.null(cols)) rows <- ceiling(length(plots)/cols)
  if (!is.null(rows) & is.null(cols)) cols <- ceiling(length(plots)/rows)
  if (is.null(rows) & is.null(cols)) { rows <- length(plots); cols <- 1 }
  w <- "-xs"
  if (!isTRUE(getOption("knitr.in.progress"))) w <- ""
  x <- 0
  tg <- htmltools::tagList()
  for (i in 1:rows) {
    r <- htmltools::div(class = "row")
    for (j in 1:cols) {
      x <- x + 1
      cl <- paste0("col", w, "-", 12/cols)
      if (x <= length(plots))
        c <- htmltools::div(class = cl, plots[[x]])
      else 
        c <- htmltools::div(class = cl)
      r <- htmltools::tagAppendChild(r, c)
    }
    tg <- htmltools::tagAppendChild(tg, r)
  }
  if (isTRUE(getOption("knitr.in.progress"))) {
    if (!is.null(title))
      htmltools::div(title, tg)
    else
      tg
  }
  else
    htmltools::browsable(
      htmltools::div(
        class = "container-fluid", 
        htmltools::tags$head(
          htmltools::tags$link(
            rel = "stylesheet", 
            href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/css/bootstrap.min.css"
          )),
        htmltools::h3(title), tg))
}


#' Area band
#' 
#' A 'custom' serie with lower and upper boundaries
#' 
#' @param df A data.frame with lower and upper numerical columns.
#' @param lower The column name of band's lower boundary, a string.
#' @param upper The column name of band's upper boundary, a string.
#' @param two Type of rendering - by polygon (FALSE,default), or by two stacked lines (TRUE)
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-line.type}{serie}
#' @return One list serie when two=FALSE, or a list of two list series when two=TRUE
#'
#' @details When two=FALSE, the coordinates of the two boundaries are chained into a polygon and displayed as one. Uses absolute cartesian coordinates. \cr
#'      When two=TRUE, two smooth \emph{stacked} lines are drawn, one with customizable areaStyle. The upper boundary coordinates represent values on top of the lower boundary coordinates.
#'      
#' @examples 
#' myList <- list(x=LETTERS[1:7],
#'                d=c(140, 232, 101, 264, 90, 340, 250),
#'                u=c(120, 282, 111, 234, 220, 340, 310),
#'                l=c(200, 332, 151, 400, 190, 540, 450))
#' data <- as.data.frame(do.call(cbind, myList))
#' colnames(data) <- c('x','down','up','coord')
#' p <- ec.init(load='custom')
#' p$x$opts <- list(
#'   xAxis=list(list(type='category', boundaryGap=FALSE, data=data$x)),
#'   yAxis=list(list(scale=TRUE)),
#'   legend=list(ey=''),
#'   series = ecr.band(data, 'down', 'up', two=TRUE, name='band')   # two=TRUE
#'   #series = list(ecr.band(data, 'down', 'up', name='polyBand'))  # two=FALSE
#' )
#' p$x$opts$series <- append(p$x$opts$series, 
#'   list(list(name='line',type='line', lineStyle=list(width=2), data=data$coord)) )
#' p
#' 
#' @export
ecr.band <- function(df=NULL, lower=NULL, upper=NULL, two=FALSE, ...) {
  if (is.null(df) || is.null(lower) || is.null(upper)) 
    stop('df, lower and upper are all required', call. = FALSE)
  if (!'data.frame' %in% class(df)) 
    stop('df must be a data.frame', call. = FALSE)
  args <- list(...)
  
  if (two) {    # as two stacked areas
    colr <- paste("new echarts.graphic.LinearGradient(0, 0, 0, 1, [",
                  "{offset: 0, color: 'rgba(255, 0, 135)'},",
                  "{offset: 1, color: 'rgba(135, 0, 157)'}])")
    astyle <- list(opacity=0.8, color=htmlwidgets::JS(colr))  # default color
    if ('areaStyle' %in% names(args)) astyle <- args$areaStyle
    smooth <- if ('smooth' %in% names(args)) args$smooth else TRUE
    lineStyle <- if ('lineStyle' %in% names(args)) args$lineStyle else list(width=0)
    boundaryGap <- if ('boundaryGap' %in% names(args)) args$boundaryGap else FALSE
    serie <- list(
      list(type='line', stack='band',
           showSymbol=FALSE, lineStyle=lineStyle, smooth=smooth,
           data=unname(unlist(df[lower])), tooltip=list(show=FALSE), color='#fff0'), 
      list(type='line', stack='band',
           showSymbol=FALSE, lineStyle=lineStyle, smooth=smooth,
           data=unname(unlist(df[upper])), tooltip=list(show=FALSE), areaStyle=astyle, ...)
    )
  } else {    # as polygon
    ld <- nrow(df[upper])
    l2 <- unname(unlist(df[upper])[order(ld:1)])  		# reverse
    tmp <- data.frame(x=c(df[1:ld,1],df[ld:1,1]), y=c(df[lower][[1]], l2))
    
    serie <- list(
      type = 'custom', 
      renderItem = htmlwidgets::JS('riPolygon'),
      data = ec.data(tmp, 'values'),  # only this format works
      ...
    )
    if (is.null(serie$itemStyle))
      serie$itemStyle = list(borderWidth=0.5)
  }
  serie
}


#' Error bars
#' 
#' Custom series to display error bars for scatter,bar or line series
#' 
#' @param wt A widget to add error bars to, see \code{\link[htmlwidgets]{createWidget}}
#' @param df A data.frame with three or more columns in order x,low,high,etc.
#' @param hwidth Half-width of error bar in pixels, default is 6.
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-custom.type}{custom serie}
#' @return A widget with error bars added if successful, otherwise input wt
#'
#' @details Grouped bars are supported, but require the group column to be included in df. \cr
#'  Complete data frame df could be chained to ec.init to auto-populate the bar series.\cr
#'  ecr.ebars will add a legend if none is found.\cr
#'  ecr.ebars are custom series, so \emph{ec.init(load='custom')} is required.
#'  ecr.ebars should be set at the end, after all other series. \cr
#'
#' @examples
#' library(dplyr)
#' df <- mtcars %>% group_by(cyl,gear) %>% summarise(mmm=mean(mpg)) %>% 
#'   mutate(low=mmm*(1-0.2*runif(1)), high=mmm*(1+0.2*runif(1))) %>% 
#'   relocate(cyl, .after = last_col())   # move group column away from first three cols
#' p <- df %>% ec.init(group1='bar', load='custom')
#' # since this is grouped data, must include the group column 'cyl'
#' ecr.ebars(p, df[,c('gear','low','high','cyl')])
#' 
#' @export
ecr.ebars <- function(wt, df, hwidth=6, ...) {
  # alternating bar with custom series doesn't work, first bars then customs
  if (missing(wt)) stop('ecr.ebars expecting widget', call. = FALSE)
  if (missing(df)) stop('df is required', call. = FALSE)
  if (!inherits(df, "data.frame")) stop('df must be data.frame', call. = FALSE)
  ser <- wt$x$opts$series  # all series
  if (is.null(ser)) stop('series are missing', call. = FALSE)
  args <- list(...)
  
  # look for barGap(s), barCategoryGap(s)
  allBarGaps <-   lapply(ser, function(x) { x$barGap })
  allBarCgGaps <- lapply(ser, function(x) { x$barCategoryGap })
  lbg <- utils::tail(unlist(allBarGaps),1); lbg <- if (is.null(lbg)) '' else lbg
  lcg <- utils::tail(unlist(allBarCgGaps),1); lcg <- if (is.null(lcg)) '' else lcg
  
  cntr <- function(x, typ) { grep(typ, x) }
  name <- args$name; 
  tmp <- NULL   # count number of similar (grouped) series
  if (!is.null(name)) 
    tmp <- unlist(lapply(ser, function(x) { 
      if (length(grep(name,x))>0) x$type else NULL }))[1]
  if (!is.null(tmp))    # attached by name, count same-type series
    info <- length(unlist(lapply(ser, function(x) grep(tmp, x))))
  else {    # no name or not found - choose first of type bar/line/scatter, count how many
    info <- length(unlist(lapply(ser, cntr, typ='bar')))
    if (info==0) info <- length(unlist(lapply(ser, cntr, typ='line')))
    if (info==0) info <- length(unlist(lapply(ser, cntr, typ='scatter')))
  }
  
  if (info==0) return(wt)    # no bars/lines/scatter, nothing to attach to
  
  # set minimal info to be read by the renderer
  # renderers.js works in a very isolated environment, so we send data thru sessionStorage
  # info = last barGap, last barCategoryGap, number of bars, bar half-width in pixels
  info <- c(lbg, lcg, as.character(info), hwidth)
  
  info <- paste0("sessionStorage.setItem('ErrorBar.oss','"
                 ,jsonlite::toJSON(info),"'); riErrorBar;") #renderErrorBar2;")
  # no groups
  if (!dplyr::is.grouped_df(df)) {
    if (is.null(name)) name <- colnames(df)[1]
    c <- list(type='custom', name=name, renderItem = htmlwidgets::JS(info),
              data=ec.data(df, 'values'), ...)
    if (!("z" %in% names(args))) c$z <- 3
    if (!("itemStyle" %in% names(args))) c$itemStyle <- list()
    if (is.null(c$itemStyle$borderWidth)) c$itemStyle$borderWidth <- 1.5
    if (is.null(c$itemStyle$color)) c$itemStyle$color <- 'black'  # set, or it will blend with main bar
    cser <- list(c)
  }
  else {
    grnm <- dplyr::group_vars(df)[[1]]   # group1 means just 1st one
    tmp <- df %>% dplyr::group_split()
    cser <- lapply(tmp, function(s) {
      name <- unlist(unique(unname(s[,grnm])))
      c <- list(type='custom', name=name, renderItem = htmlwidgets::JS(info),
                data=ec.data(s, 'values'), ...)
      if (!("z" %in% names(args))) c$z <- 3
      if (!("color" %in% names(args))) c$color <- 'black'  # set, or it will blend with main bar
      if (!("itemStyle" %in% names(args))) c$itemStyle <- list(borderWidth = 1.5)
      c
    })
  }
  wt$x$opts$series <- append(wt$x$opts$series, cser)
  if (!("legend" %in% names(wt$x$opts))) wt$x$opts$legend <- list(ey='')
  wt$x$opts$xAxis$type <- 'category'
  wt
}


#' JS-to-R Translator Assistant 
#' 
#' Translate Javascript data objects to R
#' 
#' @return none
#'
#' @details Learn from Javascript examples of \href{https://echarts.apache.org/examples/en/}{ECharts}. This Shiny application helps translate JSON-like JavaScript data structures to R lists.\cr
#'  Additional information is available by clicking the \emph{Info} button inside.
#' @import shiny
#' @export
ec.js2r <- function() {
  if (interactive()) {
    prompt <- paste0('Ready to launch Translation Assistant\n Would you like to proceed ?')
    ans <- FALSE
    if (interactive())
      ans <- askYesNo(prompt)
    if (is.na(ans)) ans <- FALSE  # user canceled
    if (ans) {
      shiny::runGist('https://gist.github.com/helgasoft/819035e853d9889ba02cb69ecc587f34',quiet=TRUE)
    }
  }
  return(NULL)
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
#' @param wt An \code{echarty} widget to generate the chart.
#' @param env The environment in which to evaluate \code{expr}.
#' @param quoted Is \code{expr} a quoted expression? default FALSE.
#' @return An output or render function that enables the use of the widget within Shiny applications.
#'   
#' @seealso [ecs.exec] for example, \code{\link[htmlwidgets]{shinyWidgetOutput}} for return value.
#' 
#' @export
ecs.render <- function(wt, env=parent.frame(), quoted=FALSE) {
  if (!quoted) {
    wt <- substitute(wt)  # do not add ',env' in substitute command
  } # force quoted
  htmlwidgets::shinyRenderWidget(wt, ecs.output, env, quoted = TRUE)
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
#' @importFrom shiny getDefaultReactiveDomain
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
#'   The proxy commands are:\cr
#' \emph{p_update} - add new series and axes\cr
#' \emph{p_merge} - add serie features like marks\cr
#' \emph{p_replace} - replace entire chart \cr
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
#' ui = fluidPage(
#'   ecs.output('plot'),
#'   fluidRow(
#'     column(4, actionButton('addm', 'Add marks'),
#'            actionButton('delm', 'Delete marks'),
#'            br(),span('mark points stay, area/line deletable')
#'     ),
#'     column(3, actionButton('adds', 'Add serie'),
#'            actionButton('dels', 'Del serie')),
#'     column(5, actionButton('adata', 'Add data'),
#'            actionButton('hilit', 'Highlight'),
#'            actionButton('dnplay', 'Downplay') )
#'   )
#' ),
#' server = function(input, output, session) {
#' 
#'   output$plot <- ecs.render({
#'     p <- ec.init()
#'     p$x$opts$series <- lapply(mtcars %>% relocate(disp, .after=mpg)
#'       %>% group_by(cyl) %>% group_split(), function(s) {
#'           list(type='scatter', name=unique(s$cyl), data=ec.data(s, 'values'))
#'       })
#'     p$x$opts$legend <- list(ey='')
#'     p$x$opts$xAxis <- list(type="value"); p$x$opts$yAxis <- list(ec='')
#'     p$x$opts$tooltip <- list(list(show=TRUE))
#'     p$x$opts$series[[1]]$emphasis <- list(focus='series', blurScope='coordinateSystem')
#'     p
#'   })
#' 
#'   observeEvent(input$addm, {
#'     p <- ecs.proxy('plot')
#'     p$x$opts$series = list( list(
#'       markPoint = list(data = list(
#'         list(coord = c(22.5, 140.8)),
#'         list(coord = c(30.5, 95.1))
#'       ),
#'       itemStyle = list(color='lightblue')
#'       )
#'       ,markArea = list(data = list(list(
#'         list(xAxis = 15),
#'         list(xAxis = 25)
#'       ))
#'       ,silent=TRUE
#'       ,itemStyle = list(color='pink', opacity=0.2)
#'       ,label = list(formatter='X-area', position='insideTop')
#'       )
#'       ,markLine = list(data = list(list(type='average')))
#'     ), list(
#'       markPoint = list(data = list(
#'         list(coord = c(25.5, 143.8)),
#'         list(coord = c(33.5, 98.1))
#'       ),
#'       itemStyle = list(color='forestgreen')
#'       )
#'     ))
#'     p %>% ecs.exec() # ='p_merge'
#'   })
#'   observeEvent(input$adds, {
#'     p <- ecs.proxy('plot')
#'     p$x$opts$series <- list(list(
#'       type = 'line', name = 'newLine',
#'       #encode = list(x='mpg', y='disp')  # for dataset only
#'       data=list(list(10,100),list(5,200),list(10,400),list(10,200),list(15,150),list(5,300))
#'     ))
#'     p %>% ecs.exec('p_update')
#'   })
#' 
#'   observeEvent(input$adata, {
#'     tmp <- apply(unname(data.frame(rnorm(5, 10, 3), rnorm(5, 200, 33))),
#'                  1, function(x) { list(value=x) })
#'     p <- ecs.proxy('plot')
#'     p$x$opts$seriesIndex <- 1
#'     p$x$opts$data <- tmp
#'     p %>% ecs.exec('p_append_data')
#'   })
#' 
#'   observeEvent(input$dels, {
#'     p <- ecs.proxy('plot')
#'     p$x$opts$seriesName <- 'newLine'
#'     #'p$x$opts$seriesIndex <- 4  # ok too
#'     p %>% ecs.exec('p_del_serie')
#'   })
#'   observeEvent(input$delm, {
#'     p <- ecs.proxy('plot')
#'     p$x$opts$seriesIndex <- 1
#'     p$x$opts$delMarks <- c('markArea','markLine')
#'     p %>% ecs.exec('p_del_marks')
#'   })
#'   observeEvent(input$hilit, {
#'     p <- ecs.proxy('plot')
#'     p$x$opts <- list(type='highlight', seriesName='4')
#'     p %>% ecs.exec('p_dispatch')
#'   })
#'   observeEvent(input$dnplay, {
#'     p <- ecs.proxy('plot')
#'     p$x$opts <- list(type='downplay', seriesName='4')
#'     p %>% ecs.exec('p_dispatch')
#'   })
#' } ))
#'   
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
#' Apply a pre-built or custom coded theme to a chart
#'
#' @param wt An \code{echarty} widget as returned by [ec.init]
#' @param name Name of existing theme file (without extension), or name of custom theme defined in \code{code}.
#' @param code Custom theme as JSON formatted string, default NULL.
#' @return An \code{echarty} widget.
#'
#' @details Just a few built-in themes are included in folder \code{inst/themes}. The entire collection could be found \href{https://github.com/apache/echarts/tree/master/theme}{here} and copied if needed.\cr
#'   To create custom themes or view predefined ones, visit \href{https://echarts.apache.org/en/theme-builder.html}{this site}.
#'
#' @examples
#' mtcars %>% ec.init() %>% ec.theme('dark-mushroom')
#' cars %>% ec.init() %>% ec.theme('mine', code=
#'   '{"color": ["green","#eeaa33"], 
#'     "backgroundColor": "lemonchiffon"}')
#' 
#' @export
ec.theme <- function (wt, name, code = NULL) 
{
  if (missing(name))
    stop('must define theme name', call. = FALSE)

  wt$x$theme <- name
  if (!is.null(code))
    wt$x$themeCode <- code
  else {
    wt$x$themeCode <- NULL
    path <- system.file('themes', package = 'echarty')
    dep <- htmltools::htmlDependency(
      name = name,
      version = '1.0.0', src = c(file = path),
      script = paste0(name, '.js'))
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  wt
}

#' Chart to JSON
#' 
#' Convert chart to JSON string
#' 
#' @param wt An \code{echarty} widget as returned by [ec.init]
#' @param json Whether to return a JSON, or a \code{list}, default TRUE
#' @param ... Additional arguments to pass to \code{\link[jsonlite]{toJSON}}
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
ec.inspect <- function(wt, json=TRUE, ...) {
  opts <- wt$x$opts
  
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


#' JSON to chart
#' 
#' Convert JSON string to chart
#' 
#' @param txt JSON character string, url, or file, see \code{\link[jsonlite]{fromJSON}}
#' @param ... Any arguments to pass to internal ec.init
#' @return An \code{echarty} widget.
#' 
#' @details \code{txt} should contain the full list of options required to build a chart.
#' It is subsequently passed to ECharts function \href{https://echarts.apache.org/en/api.html#echartsInstance.setOption}{setOption}.
#' 
#' @examples
#' txt <- '{
#'   "xAxis": { "type": "category",
#'      "data": ["Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"]
#'    },
#'    "yAxis": { "type": "value" },
#'    "series": { "type": "line",
#'      "data": [150, 230, 224, 218, 135, 147, 260]
#'    } }'
#' ec.fromJson(txt)
#'
#' @export
ec.fromJson <- function(txt, ...) {
  json <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
  
  e <- ec.init(...)
  e$x$opts <- json
  e
}



# ----------- Internal --------------

# needed by widget init
.preRender <- function(wt) {

  ff <- getOption('ECHARTS_FONT')
  
  if (!is.null(ff))
    wt$x$opts$textStyle <- list(fontFamily = ff)
  wt
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

