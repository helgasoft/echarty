# ----------- General --------------

#' Initialize command
#'
#' Required to build a chart. In most cases this will be the only command necessary.
#'
#' @param df A data.frame to be preset as \href{https://echarts.apache.org/en/option.html#dataset}{dataset}, default NULL \cr
#'     For crosstalk df should be of type \code{\link[crosstalk]{SharedData}}.\cr
#'     Timeline requires a \emph{grouped data.frame} to build its \href{https://echarts.apache.org/en/option.html#options}{options}.
#' @param group1 Type of grouped series, or type of first ungrouped serie. Default is 'scatter'. Set to NULL to disable. \cr
#'     If the grouping is on multiple columns, only the first one is used.
#' @param preset Disable(FALSE) or enable (TRUE, default) presets xAxis,yAxis,serie for 2D, or grid3D,xAxis3D,yAxis3D,zAxis3D for 3D.
#' @param load Name(s) of plugin(s) to load. Could be a character vector or comma-delimited string. default NULL.\cr
#' @param width,height A valid CSS unit (like \code{'100\%'},
#'   \code{'500px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param tl.series A list to build a timeline or NULL(default). The list defines options \href{https://echarts.apache.org/en/option.html#series}{series} and their attributes. \cr
#'  The only required attribute is \href{https://echarts.apache.org/en/option.html#series-line.encode}{encode}. 
#'  \emph{encode} defines which data columns names(not indexes) to use for axes X and Y: \cr
#'   Set \emph{x} and \emph{y} when coordinateSystem is \emph{'cartesian2d'}, \cr
#'   Set \emph{lng} and \emph{lat} when coordinateSystem is \emph{'geo'}, \cr
#'   Set \emph{radius} and \emph{angle} when coordinateSystem is \emph{'polar'}.\cr
#'   Attribute \emph{coordinateSystem} is set to \emph{'cartesian2d'} by default. Auto-generated \emph{timeline} and \emph{options} will be preset for the chart as well.
#' @param ... other arguments to pass to the widget. \cr
#'   Custom widget arguments include: \cr \itemize{
#'   \item elementId - Id of the widget, default is NULL(auto-generated)
#'   \item ask - the \emph{plugjs} parameter when \emph{load} is present, FALSE by default
#'   \item js - single string or a two-strings vector with JavaScript expressions to evaluate. First is evaluated before and second after chart initialization. Chart object 'opts' is exposed for the second script.
#'   \item renderer - 'canvas'(default) or 'svg'
#'   \item locale - 'EN'(default) or 'ZH' or other, see \href{https://echarts.apache.org/en/api.html#echarts.init}{here}
#'   \item useDirtyRect - enable dirty rectangle rendering or not, FALSE by default, see \href{https://echarts.apache.org/en/api.html#echarts.init}{here}
#' }
#' @return A widget to plot, or to save and expand with more features.
#'
#' @details  Command \emph{ec.init} creates a widget with \code{\link[htmlwidgets]{createWidget}}, then adds some ECharts features to it.\cr
#'  When \emph{ec.init} is chained after a data.frame, a \href{https://echarts.apache.org/en/option.html#dataset}{dataset} is preset. \cr
#'  When the data.frame is grouped and \emph{group1} is not null, more datasets with legend and series are also preset. Grouped series are preset as type \emph{scatter}. \cr
#'  Plugin '3D' presets will not work for 'scatterGL'. Instead, use \emph{preset=FALSE} and set explicitly \emph{xAxis,yAxis}. \cr
#'  Plugins 'leaflet' and 'world' preset zoom=6 and center to the mean of all coordinates. \cr
#'  Users can delete or overwrite any presets as needed. \cr
#'  [ec.plugjs] will be called internally for each \emph{load} entry, popup prompts controlled by parameter \emph{ask}. \cr
#'   Built-in plugins: \cr \itemize{
#'   \item leaflet - Leaflet maps with customizable tiles, see \href{https://github.com/gnijuohz/echarts-leaflet#readme}{source}\cr
#'   \item custom - renderers for [ecr.band] and [ecr.ebars] \cr 
#'  } Plugins with one-time installation: \cr \itemize{
#'   \item 3D - 3D charts and WebGL acceleration, see \href{https://github.com/ecomfe/echarts-gl}{source} and \href{https://echarts.apache.org/en/option-gl.html#series}{docs} \cr
#'   \item world - world map with country boundaries, see \href{https://github.com/apache/echarts/tree/master/test/data/map/js}{source} \cr
#'   \item liquid - liquid fill, see \href{https://github.com/ecomfe/echarts-liquidfill}{source}  \cr
#'   \item gmodular - graph modularity, see \href{https://github.com/ecomfe/echarts-graph-modularity}{source}  \cr
#'   \item wordcloud - cloud of words, see \href{https://github.com/ecomfe/echarts-wordcloud}{source} \cr
#'  } or install you own third-party plugins.
#' 
#' @examples
#'  # basic scatter chart from a data.frame, using presets
#' cars %>% ec.init()
#'  
#'  # a timeline with two series and autoPlay
#' p <- iris %>% dplyr::group_by(Species) %>% ec.init(
#'   tl.series=list(
#'     encode=list(x=NULL, y=c('Sepal.Width', 'Petal.Length')),
#'     markPoint = list(data=list(list(type='max'), list(type='min')))
#'   )
#' )
#' p$x$opts$timeline <- append(p$x$opts$timeline, list(autoPlay=TRUE))
#' p$x$opts$legend <- list(list())  # add legend
#' p
#' 
#' @importFrom htmlwidgets createWidget sizingPolicy getDependency JS shinyWidgetOutput shinyRenderWidget
#' @import dplyr
#' 
#' @export
ec.init <- function( df=NULL, preset=TRUE, group1='scatter', load=NULL,
                     tl.series=NULL,
                     width=NULL, height=NULL, ...) {
  
  opts <- list(...)
  elementId <- if (is.null(opts$elementId)) NULL else opts$elementId
  js <- if (is.null(opts$js)) NULL else opts$js
  ask <- if (is.null(opts$ask)) FALSE else opts$ask
  renderer <- if (is.null(opts$renderer)) 'canvas' else tolower(opts$renderer)
  locale <- if (is.null(opts$locale)) 'EN' else toupper(opts$locale)
  useDirtyRect <- if (is.null(opts$useDirtyRect)) FALSE else opts$useDirtyRect
  # remove the above since they are not valid ECharts options
  opts$ask <- opts$js <- opts$renderer <- opts$locale <- opts$useDirtyRect <- opts$elementId <- NULL
  
  # presets are used as default for examples and testing
  # user can also ignore or replace them
  if (preset) {
    if (!('xAxis' %in% names(opts))) opts$xAxis <- list(ey='') #list(type='category')
    if (!('yAxis' %in% names(opts))) opts$yAxis <- list(ey='')
    if (!('series' %in% names(opts))) opts$series <- list(
    	list(type=if (is.null(group1)) 'scatter' else group1) )
  }

  if (requireNamespace("crosstalk", quietly = TRUE)) {  # replaced ' @importFrom crosstalk is.SharedData crosstalkLibs
    if (crosstalk::is.SharedData(data)) {
      crosstalkKey <- as.list(data$key())
      crosstalkGroup <- data$groupName()
      data <- data$origData()
      dependencies <- crosstalk::crosstalkLibs()
    }
  }

  key <- group <- deps <- NULL
  if (requireNamespace("crosstalk", quietly = TRUE)) {
    if (crosstalk::is.SharedData(df)) {
      # Using Crosstalk
      key <- as.list(df$key())
      group <- df$groupName()
      df <- df$origData()
      deps <- crosstalk::crosstalkLibs()
    }
  }
  
  # forward widget options using x
  x <- list(
    theme = '',
    draw = TRUE,
    renderer = renderer,
    locale = locale,
    useDirtyRect = useDirtyRect,
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
  
  #if (is.null(load)) return(wt)

  if (length(load)==1 && grepl(',', load, fixed=TRUE))
      load <- unlist(strsplit(load, ','))
      
  path <- system.file('js', package = 'echarty')
  dep <- NULL
  if ('leaflet' %in% load) {
    if (preset) {
      # customizations for leaflet
      wt$x$opts$xAxis <- NULL
      wt$x$opts$yAxis <- NULL
      urltls <- getOption('ECHARTS_TILES')
      if (is.null(urltls))
        urltls <- 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
      wt$x$opts$leaflet = list(
        roam = TRUE,
        tiles = list( list(urlTemplate = urltls))
      )
      wt$x$opts$series[[1]]$coordinateSystem <- 'leaflet'
      
      # user should order the leaflet data  (lon,lat)
      if (!is.null(df)) {
	      #rlo <- range(df[,1])
	      #rla <- range(df[,2])
	      #wt$x$opts$leaflet$center= c(sum(rlo)/2, sum(rla)/2)
	      wt$x$opts$leaflet$center= c(mean(unlist(df[,1])), mean(unlist(df[,2])))
	      wt$x$opts$leaflet$zoom <- 6
      }
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
      wt$x$opts$series[[1]] <- list(type='scatter3D')  #NULL
      wt$x$opts$grid3D  <- list(list())
      wt$x$opts$xAxis3D <- list(list())
      wt$x$opts$yAxis3D <- list(list())
      wt$x$opts$zAxis3D <- list(list())
    }
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-gl@2.0.8/dist/echarts-gl.min.js', ask)
  }
  if ('world' %in% load) {
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts@4.9.0/map/js/world.js', ask)
    if (preset) {
      wt$x$opts$xAxis <- NULL
      wt$x$opts$yAxis <- NULL
      wt$x$opts$geo = list(map='world', roam=TRUE, zoom=6)
      # if (!is.null(df))  # cancelled: dont know if first 2 cols are lng,lat
      #   wt$x$opts$geo$center= c(mean(unlist(df[,1])), mean(unlist(df[,2])))
    }
  }
  if ('liquid' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-liquidfill@3.0.0/dist/echarts-liquidfill.min.js', ask)
  
  if ('gmodular' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-graph-modularity@2.0.0/dist/echarts-graph-modularity.min.js', ask)
  
  if ('wordcloud' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-wordcloud@2.0.0/dist/echarts-wordcloud.min.js', ask)
  
  # load unknown plugins
  unk <- load[! load %in% c('leaflet','custom','3D','world','liquid','gmodular','wordcloud')]
  if (length(unk)>0) {
    for(p in unk) wt <- ec.plugjs(wt, p, ask)
  }
  
  # timeline is last to execute
  if (!is.null(tl.series)) {
    if (!is.grouped_df(df))
      stop('tl.series requires a grouped data.frame', call. = FALSE)

    if (is.null(tl.series$encode))
      stop('encode is required for tl.series', call. = FALSE)

    # add missing defaults
    if (is.null(tl.series$type)) tl.series$type <- 'line'
    #if (is.null(tl.series$coordinateSystem)) tl.series$coordinateSystem <- 'cartesian2d' # not for gauge
    if (is.null(tl.series$coordinateSystem) ||
        tl.series$coordinateSystem=='cartesian2d') { xtem <-'x'; ytem <- 'y' }
    else if (tl.series$coordinateSystem=='polar') { xtem <-'radius'; ytem <- 'angle' }
    else if (tl.series$coordinateSystem %in% c('geo','leaflet')) { 
      xtem <-'lng'; ytem <- 'lat'
      center <- c(mean(unlist(df[,tl.series$encode$lng])),
                  mean(unlist(df[,tl.series$encode$lat])))
      if (tl.series$coordinateSystem=='geo') wt$x$opts$geo$center <- center
      if (tl.series$coordinateSystem=='leaflet') wt$x$opts$leaflet$center <- center
    }
    if (is.null(unlist(tl.series$encode[xtem]))) {   
      # append col XcolX 1:max for each group 
      df <- df %>% group_modify(~ { .x %>% mutate(XcolX = 1:nrow(.)) })
      tl.series$encode[xtem] <- 'XcolX'    # instead of relocate(XcolX)
      # replace only source, transforms stay
      wt$x$opts$dataset[[1]] <- list(source=ec.data(df))
    }
    if (is.null(unlist(tl.series$encode[ytem])))
      stop("encode 'y' is required for tl.series", call.=FALSE)
    
    # dataset is already in, now set everything else
    #wt$x$opts$xAxis <- list(type='category')  # geo,leaf do not like
    wt$x$opts$series <- NULL
    wt$x$opts$legend <- NULL
    
    # loop group column(s)
    gvar <- df %>% group_vars() %>% first()
    gvar <- as.character(gvar)  # convert if factor
    di <- 0
    optl <- lapply(df %>% group_split(), function(gp) {
      di <<- di+1
      # nicer looking lines with sorted X 
      #if (!is.null(xcol)) gp <- gp %>% arrange(across(all_of(xcol)))
      
      # multiple series for each Y, like y=c('col1', 'col3')
      series <- lapply(unname(unlist(tl.series$encode[ytem])), function(sname) {
        append(list(datasetIndex=di, name=sname), tl.series)
      })
      series <- lapply(series, function(s) {
        s$encode[ytem] <- s$name   # replace multiple col.names with one
        s
      })

      list(title = list(text = as.character(unique(gp[gvar])), left=40),  
           series = unname(series))
    })
    
    wt$x$opts$options <- optl
    
    steps <- lapply(optl, function(x) { paste(x$title$text, collapse=' ') })
    wt$x$opts$timeline <- list(data=steps, axisType='category')
  }
  return(wt)
}


#' Install Javascript plugin from URL source
#' 
#' @param wt A widget to add dependency to, see \code{\link[htmlwidgets]{createWidget}}
#' @param source URL or file:// of an uninstalled Javascript plugin, \cr
#'   or name of an installed plugin file with suffix '.js'. Default is NULL.
#' @param ask Whether to ask the user to download source if missing. Default is FALSE
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
ec.plugjs <- function(wt=NULL, source=NULL, ask=FALSE) {
  if (missing(wt))
    stop('ec.plugjs expecting widget', call. = FALSE)
  if (is.null(source)) return(wt)
  if (!startsWith(source, 'http') && !startsWith(source, 'file://'))
    stop('ec.plugjs expecting source as URL or file://', call. = FALSE)
  fname <- basename(source)
  fname <- unlist(strsplit(fname, '?', fixed=TRUE))[1]  # when 'X.js?key=Y'
  # if (!endsWith(fname, '.js'))
  #   stop('ec.plugjs expecting .js suffix', call. = FALSE)
  path <- system.file('js', package = 'echarty')
  ffull <- paste0(path,'/',fname)
  if (!file.exists(ffull)) {
    if (ask) {
      prompt <- paste0('One-time installation of plugin\n',fname,'\n Would you like to proceed ?')
      ans <- FALSE
      if (interactive())
        ans <- askYesNo(prompt)
      if (is.na(ans)) ans <- FALSE  # was cancelled
    } else
      ans <- TRUE
    if (ans) {
      try(withCallingHandlers(
        download.file(source, ffull), # method = "libcurl"),
        error = function(w) { ans <- FALSE },
        warning = function(w) { ans <- FALSE }
            #cat('ec.plugjs Error:', sub(".+HTTP status was ", "", w, source))
        ))  #,silent=TRUE)
    } 
    if (!ans) return(wt)    # error
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
#' Make data lists from a data.frame
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
#' @seealso some live \href{https://rpubs.com/echarty/data-models}{code samples}
#' @export
ec.data <- function(df, format='dataset', header=TRUE) {
  if (missing(df))
    stop('expecting df as data.frame', call. = FALSE)
  if (!'data.frame' %in% class(df))
    stop('df has to be data.frame', call. = FALSE)
  
  #tmp <- purrr::transpose(df)          # named lists
  rownames(df) <- NULL
  # tmp <- apply(df, 1, function(x) {   # apply converts df to matrix with cols=char
  #   out <- list()
  #   i <- 1
  #   for(n in colnames(df)) { out[n] <- x[i]; i <- i+1 }
  #   out 
  # })
  n <- seq_along(df[[1]])       # assuming all lists in df have the same length
  tmp <- lapply(n, function(i) lapply(df, "[[", i))  # preserve column types
  
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


#' Data column
#' 
#' Helper function to address data column(s) by index or name
#' 
#' @param col A column index(number), column name(string) or a \code{\link[base]{sprintf}} format string. \cr
#' @param ... Comma separated list of column indexes or names, when \emph{col} is \emph{sprintf}. This allows formatting of multiple columns, as for a tooltip.
#' 
#' @details Column indexes are counted in R and start at 1.\cr
#' \emph{col} as sprintf supports only two placeholders - %d for column indexes and %s for column names.\cr
#' \emph{col} as sprintf can contain double quotes, but not single or backquotes.\cr
#' Useful to set formatter, color, symbolSize, etc.
#' 
#' @examples
#' tmp <- data.frame(Species = as.vector(unique(iris$Species)),
#'                   emoji = c('\U0001F33B','\U0001F335','\U0001F33A')) # 🌻,🌵,🌺
#' df <- iris %>% dplyr::inner_join(tmp)   # add 6th column 'emoji'
#' p <- df %>% dplyr::group_by(Species) %>% ec.init()
#' p$x$opts$series <- list(list(
#'   type='scatter', label=list(show=TRUE, formatter = ec.clmn(6))  # 6th column
#' ))
#' p$x$opts$tooltip <- list(formatter=
#'    ec.clmn('species <b>%d</b><br>s.len <b>%d</b><br>s.wid <b>%d</b>', 5,1,2))
#' p
#' 
#' @export
ec.clmn <- function(col=NULL, ...) {
  if (is.null(col)) stop('col is required', call.=FALSE)
  args <- list(...)
  if (is.na(suppressWarnings(as.numeric(col)))) {   # col is string
    if (length(args)==0)
      ret <- paste0('return x.data.',col,';')
    else {          # col a sprintf
      spf <- paste("var sprintf = (str, argv) => !argv.length ? str :",
                   "sprintf(str = str.replace('@', argv.shift()), argv); ")
      tmp <- suppressWarnings(as.numeric(args) -1)
      if (all(is.na(tmp))) {   # multiple non-numeric strings
        tmp <- sapply(args, function(s) toString(paste0('x.data.', s)) )
        tmp <- paste(tmp, collapse=',')
        ret <- paste0(sub('@','%s',spf),'let ss=[',tmp,']; ',
                      'let c = sprintf(`',col,'`, ss); return c;')
      }
      else {   #  multiple numeric, they could be in x, x.data, x.value
        tmp <- paste(tmp, collapse=',')
        ret <- paste0(sub('@','%d',spf), 
          "let ss=[",tmp,"]; ",
          "ss=ss.map(e => x.value!=null ? x.value[e] : x.data!=null ? x.data[e] : x[e]);",
          "let c = sprintf(`",col,"`, ss); return c; ")
      }
    }
  }
  else {  # col is numeric thus solitary parameter
    if (length(args) > 0) # { cat(length(args));
      warning('col is numeric, others are ignored', call.=FALSE)
    col <- as.numeric(col) - 1   # from R to JavaScript counting
    ret <- paste0('let c = (x.data) ? x.data[',col,'] : x[',col,']; return c;')
  }
  #cat(ret)
  htmlwidgets::JS(paste0('function(x) {', ret, '}'))
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
#' if (interactive()) {
#'   p1 <- cars %>% ec.init()
#'   p2 <- cars %>% ec.init() %>% ec.theme('dark')
#'   ec.layout(list(p1,p2), cols=2 )
#' }
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
#' @param df A data.frame with lower and upper numerical columns and first column with X coordinates.
#' @param lower The column name(string) of band's lower boundary.
#' @param upper The column name(string) of band's upper boundary.
#' @param type Type of rendering  \cr \itemize{
#'  \item 'stack' - by two \href{https://echarts.apache.org/en/option.html#series-line.stack}{stacked lines} 
#'  \item 'polygon' - by drawing a polygon as polyline (default)
#' }
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-line.type}{serie}
#' @return A list of one serie when type='polygon', or two series when type='stack'
#'
#' @details When type='polygon', coordinates of the two boundaries are chained into a polygon and displayed as one.\cr
#'      When type='stack', two smooth \emph{stacked} lines are drawn, one with customizable areaStyle. The upper boundary coordinates should be values added on top of the lower boundary coordinates.\cr
#'      Type 'stack' needs \emph{xAxis} to be of type 'category'.
#' 
#' @examples 
#' df <- data.frame( x = 1:10, y = runif(10, 5, 10)) %>%
#'   dplyr::mutate(lwr = y-runif(10, 1, 3), upr = y+runif(10, 2, 4))
#' 
#' p <- df %>% ec.init(load='custom')
#' p$x$opts$legend <- list(ey='') 
#' p$x$opts$xAxis <- list(type='category', boundaryGap=FALSE)
#' p$x$opts$series <- list(list(type='line', color='yellow', datasetIndex=0, name='line1'))
#' p$x$opts$series <- append( p$x$opts$series,
#'      ecr.band(df, 'lwr', 'upr', type='stack', name='stak')
#' )
#' p$x$opts$tooltip <- list(trigger = 'axis'
#'                          ,formatter = htmlwidgets::JS("function(x) {
#'   let str='high <b>'+x[2].value[2]+'</b><br>line <b>'+x[0].value[1]+
#'    '</b><br>low <b>'+x[1].value[1]+'</b>';
#'   return str;
#'   }"))
#' p
#' 
#' @export
ecr.band <- function(df=NULL, lower=NULL, upper=NULL, type='polygon', ...) {
  if (is.null(df) || is.null(lower) || is.null(upper)) 
    stop("df, lower and upper are all required", call. = FALSE)
  if (!"data.frame" %in% class(df)) 
    stop("df must be a data.frame", call. = FALSE)
  if (!is.numeric(df[lower][[1]]) || !is.numeric(df[upper][[1]]))
    stop("lower and upper must be numeric", call. = FALSE)
  
  fstc <- colnames(df)[1]   # first column name
  if (type=='stack') {
    colr <- paste("new echarts.graphic.LinearGradient(0, 0, 0, 1, [", 
                  "{offset: 0, color: 'rgba(255, 0, 135)'},", 
                  "{offset: 1, color: 'rgba(135, 0, 157)'}])")
    astyle <- list(opacity = 0.8, color = htmlwidgets::JS(colr))
    
    slow <- list(type='line', ...)
    if (is.null(slow$stack)) slow$stack <- 'band'
    if (is.null(slow$name)) slow$name <- 'band'
    if (is.null(slow$showSymbol)) slow$showSymbol <- FALSE
    if (is.null(slow$smooth)) slow$smooth <- TRUE
    if (is.null(slow$lineStyle)) slow$lineStyle <- list(width=0)
    supr <- slow
    if (!is.null(slow$areaStyle)) slow$areaStyle <- NULL
    else supr$areaStyle <- astyle
    # save realHI data for tooltip, 'hi' is just difference
    tmp <- data.frame(x = df[fstc][[1]], lo=df[lower][[1]], 
                      hi = df[upper][[1]] - df[lower][[1]], realHI = df[upper][[1]] )
    slow$data <- ec.data(tmp[,c('x','lo')], "values")
    supr$data <- ec.data(tmp[,c('x','hi','realHI')], "values")
    serios <- list(slow, supr)
  }
  else {   # polygon
    ld <- nrow(df[upper])
    l2 <- unname(unlist(df[upper])[order(ld:1)])
    tmp <- data.frame(x = unname(unlist(c(df[1:ld, fstc], df[ld:1, fstc]))), 
                      y = c(df[lower][[1]],  l2))
    serios <- list(type = "custom", renderItem = htmlwidgets::JS("riPolygon"), 
                   data = ec.data(tmp, "values"), ...)
    if (is.null(serios$itemStyle)) serios$itemStyle <- list(borderWidth = 0.5)
    if (is.null(serios$boundaryGap)) serios$boundaryGap <- FALSE
    serios <- list(serios)  # keep consistent with stack type
  }
  serios
}


#' Error bars
#' 
#' Custom series to display error-bars for scatter,bar or line series
#' 
#' @param wt A widget to add error bars to, see \code{\link[htmlwidgets]{createWidget}}
#' @param df NULL(default) or data.frame with four or more columns ordered exactly (x,y,low,high,others).
#' When NULL, data is taken from wt's dataset where order should be the same (x,y,low,high,etc)
#' @param hwidth Half-width of error bar in pixels, default is 6.
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-custom.type}{custom serie}
#' @return A widget with error bars added if successful, otherwise the input wt
#'
#' @details 
#'  Grouped series are supported, but require the group column to be included in df. \cr
#'  ecr.ebars are custom series, so \emph{ec.init(load='custom')} is required. \cr
#'  ecr.ebars will add a chart legend and its own tooltip if none is provided.\cr
#'  ecr.ebars should be set at the end, after all other series.
#'
#' @examples
#' tmp <- round(rnorm(24, sin(1:24/2)*10, .5))
#' df <- data.frame(x = 1:24, val = tmp, 
#'                  lower = round(rnorm(24, tmp -10, .5)),
#'                  upper = round(rnorm(24, tmp + 5, .8))
#' )
#' p <- df %>% ec.init(load='custom') %>% ecr.ebars()
#' p$x$opts$tooltip <- list(ey='')
#' p
#' 
#' @export
ecr.ebars <- function(wt, df=NULL, hwidth=6, ...) {
  # alternating bars with custom series doesn't work, first bars then customs
  if (missing(wt)) stop('ecr.ebars expecting widget', call.=FALSE)
  if (!is.null(df) && !inherits(df, "data.frame")) 
    stop('df must be a data.frame', call.=FALSE)
  if (!'renderers' %in% unlist(lapply(wt$dependencies, function(d) d$name)))
    stop("use ec.init(load='custom') for ecr.ebars", call.=FALSE)
  
  ser <- wt$x$opts$series  # all series
  if (is.null(ser)) stop('series are missing', call.=FALSE)
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
    if (info==0) info <- length(unlist(lapply(ser, cntr, typ='scatter')))
    if (info==0) info <- length(unlist(lapply(ser, cntr, typ='line')))
  }
  
  if (info==0) return(wt)    # no bars/lines/scatter, nothing to attach to
  
  # set minimal info to be read by the renderer
  # renderers.js works in a very isolated environment, so we send data thru sessionStorage
  # info = last barGap, last barCategoryGap, number of bars, bar half-width in pixels
  info <- c(lbg, lcg, as.character(info), hwidth)
  
  info <- paste0("sessionStorage.setItem('ErrorBar.oss','"
                 ,jsonlite::toJSON(info),"'); riErrorBar;") #renderErrorBar2;")
  
  oneSerie <- function(name, df=NULL) {
    if (is.null(df))
      c <- list(type='custom', name=name, renderItem = htmlwidgets::JS(info), ...)
    else
      c <- list(type='custom', name=name, renderItem = htmlwidgets::JS(info),
                data=ec.data(df, 'values'), ...)
    
    if (is.null(c$z)) c$z <- 3
    if (is.null(c$itemStyle$borderWidth)) c$itemStyle$borderWidth <- 1.5
    if (is.null(c$color) && is.null(c$itemStyle$color)) {
      # set own color, or it will blend with main bar
      # impression that c$itemStyle$color is better than c$color
      c$itemStyle$color <- 'black'
    }
    if (is.null(c$tooltip))  # shows up on non-grouped data
      c$tooltip <- list(formatter=htmlwidgets::JS("function(x) { let h=(typeof x.data[1] !== 'undefined') ? ",
                                                  "'high '+x.value[3]+'<br>value <b>'+x.value[1] + '</b><br>low '+x.value[2] : ''; return h; }"))
    c
  }
  
  # build the series
  if (is.null(df)) {
    if (length(wt$x$opts$dataset)==1) {
      if (is.null(name)) name <- wt$x$opts$dataset[[1]]$source[[1]][2]
      cser <- list(oneSerie(name))
    } else 
      stop('dataset is grouped, use df parameter', call. = FALSE)
  }
  else {
    if (dplyr::is.grouped_df(df)) {    # groups
      grnm <- dplyr::group_vars(df)[[1]]   # just 1st one matters
      tmp <- df %>% dplyr::group_split()
      cser <- lapply(tmp, function(gp) {
        name <- unlist(unique(unname(gp[,grnm])))
        oneSerie(name, gp)
      })
    }
    else
      cser <- list(oneSerie(names(df)[2], df))
  }
  wt$x$opts$series <- append(wt$x$opts$series, cser)
  if (!("legend" %in% names(wt$x$opts))) wt$x$opts$legend <- list(ey='')
  wt$x$opts$xAxis$type <- 'category'
  wt
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
#' @importFrom htmlwidgets shinyWidgetOutput
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
#' @seealso [ecs.exec] for example, \code{\link[htmlwidgets]{shinyRenderWidget}} for return value.
#' 
#' @importFrom htmlwidgets shinyRenderWidget
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
#' @export
ecs.proxy <- function(id) {
  if (requireNamespace("shiny", quietly = TRUE)) {
    sessi <- shiny::getDefaultReactiveDomain()
  } else
    return(invisible(NULL))
  proxy <- list(id = id, session = sessi)
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
#'    demo(eshiny, package='echarty')
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
    if (requireNamespace("shiny", quietly = TRUE)) {
      deps <- list(shiny::createWebDependency(
        htmltools::resolveDependencies( proxy$dependencies )[[1]]
      ))
      plist$deps <- deps
    }
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
#' @param target NULL(default) or 'data' to show info about chart's embedded data.
#' @param json Whether to return a JSON, or a \code{list}, default TRUE
#' @param ... Additional arguments to pass to \code{\link[jsonlite]{toJSON}}
#' @return A JSON string if \code{json} is \code{TRUE} and
#'  a \code{list} otherwise.
#'
#' @note Must be invoked or chained as last command.
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
ec.inspect <- function(wt, target=NULL, json=TRUE, ...) {

  opts <- wt$x$opts
  
  if (!is.null(target)) {
    if (target!='data') stop("only target='data' supported", call. = FALSE)
    out <- list()
    if (!is.null(opts$dataset))
      out <- sapply(opts$dataset, function(d) {
        if (!is.null(d$source[1])) paste('dataset:',paste(unlist(d$source[1]), collapse=', '))
        else if (!is.null(d$transform[1])) gsub('"', "'", paste(d$transform, collapse=', '))
      })
    #if (!is.null(opts$series)) {
    i <- 0
    out <- append(out, sapply(opts$series, function(s) {
      i <<- i+1 
      str <- paste0('serie',i,' name:',s$name)
      if (!is.null(s$dimensions)) str <- paste0(str, ' dim:',s$dimensions)
      if (!is.null(s$datasetIndex)) str <- paste0(str, ' dsi:',s$datasetIndex)
      if (!is.null(s$encode)) str <- paste0(str, ' enc:',paste(s$encode, collapse=', '))
      if (is.null(s$datasetIndex) && !is.null(s$data)) 
        str <- paste(str, gsub('"', "'", paste(s$data[1], collapse=', ')))
      str
    }))
    #}
    return(unlist(out))
  }
  
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

if (requireNamespace("shiny", quietly = TRUE)) {

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

