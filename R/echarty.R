# ----------- General --------------

#' Initialize command
#'
#' Required to build a chart. In most cases this will be the only command necessary.
#'
#' @param df A data.frame to be preset as \href{https://echarts.apache.org/en/option.html#dataset}{dataset}, default NULL \cr
#'     For crosstalk df should be of type \code{\link[crosstalk]{SharedData}}.\cr
#'     Timeline requires a \emph{grouped data.frame} to build its \href{https://echarts.apache.org/en/option.html#options}{options}.
#' @param ctype Chart type of series. Default is 'scatter'. Set to NULL to disable series preset. \cr
#'     If grouping is on multiple columns, only the first one is used for grouping.
#' @param preset Build preset xAxis,yAxis,serie for 2D, or grid3D,xAxis3D,yAxis3D,zAxis3D for 3D, default TRUE (enable).
#' @param load Name(s) of plugin(s) to load. Could be a character vector or comma-delimited string. default NULL.
#' @param width,height A valid CSS unit (like \code{'100\%'},
#'   \code{'500px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param tl.series A list to build a timeline or NULL(default). The list defines options \href{https://echarts.apache.org/en/option.html#series}{series} and their attributes. \cr
#'  Requires a grouped data.frame \emph{df}.
#'  The only indispensable attribute is \href{https://echarts.apache.org/en/option.html#series-line.encode}{encode}. 
#'  \emph{encode} defines which data columns names(not indexes) to use for the axes: \itemize{
#'   \item set \emph{x} and \emph{y} for coordinateSystem \emph{'cartesian2d'}
#'   \item set \emph{lng} and \emph{lat} for coordinateSystem \emph{'geo'}
#'   \item set \emph{radius} and \emph{angle} for coordinateSystem \emph{'polar'}
#'   \item set \emph{value} and \emph{itemName} for \emph{'pie'} chart.
#'   }
#'   Attribute \emph{coordinateSystem} is not set by default and depends on chart \emph{type}.\cr
#'   Auto-generated \emph{timeline} and \emph{options} will be preset for the chart as well.\cr
#'   \emph{tl.series} cannot be used for hierarchical charts like graph,tree,treemap,sankey. Chart options/timeline have to be built directly, see \href{https://helgasoft.github.io/echarty/uc4.html}{example}.
#' @param ... other arguments to pass to the widget. \cr
#'   Custom widget arguments include: \cr \itemize{
#'   \item elementId - Id of the widget, default is NULL(auto-generated)
#'   \item ask - the \emph{plugjs} parameter when \emph{load} is present, FALSE by default
#'   \item js - single string or a vector with JavaScript expressions to evaluate.\cr 
#'      First expression is evaluated before chart initialization. \cr
#'      Second is evaluated with an exposed object \emph{opts}. \cr
#'      Third is evaluated with an exposed object \emph{chart} after \emph{opts} have been set.
#'   \item renderer - 'canvas'(default) or 'svg'
#'   \item locale - 'EN'(default) or 'ZH' or other, see \href{https://echarts.apache.org/en/api.html#echarts.init}{here}
#'   \item useDirtyRect - enable dirty rectangle rendering or not, FALSE by default, see \href{https://echarts.apache.org/en/api.html#echarts.init}{here}
#' }
#' @return A widget to plot, or to save and expand with more features.
#'
#' @details  Command \emph{ec.init} creates a widget with \code{\link[htmlwidgets]{createWidget}}, then adds some ECharts features to it.\cr
#'  When \emph{ec.init} is chained after a data.frame, a \href{https://echarts.apache.org/en/option.html#dataset}{dataset} is preset. \cr
#'  When the data.frame is grouped and \emph{ctype} is not null, more datasets with legend and series are also preset. Grouped series are preset as type \emph{scatter}. \cr
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
#' cars |> ec.init()
#'  
#'  # a timeline with two series and autoPlay
#' p <- iris |> dplyr::group_by(Species) |> ec.init(
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
ec.init <- function( df=NULL, preset=TRUE, ctype='scatter', load=NULL,
                     tl.series=NULL,
                     width=NULL, height=NULL, ...) {
  
  opts <- list(...)
  elementId <- if (is.null(opts$elementId)) NULL else opts$elementId
  js <- if (is.null(opts$js)) NULL else opts$js
  ask <- if (is.null(opts$ask)) FALSE else opts$ask
  renderer <- if (is.null(opts$renderer)) 'canvas' else tolower(opts$renderer)
  locale <- if (is.null(opts$locale)) 'EN' else toupper(opts$locale)
  useDirtyRect <- if (is.null(opts$useDirtyRect)) FALSE else opts$useDirtyRect
  # remove the above arguments since they are not valid ECharts options
  opts$ask <- opts$js <- opts$renderer <- opts$locale <- opts$useDirtyRect <- opts$elementId <- NULL
  
  # presets are default settings
  # user can also ignore or replace them
  if (preset) {
    # list(show=TRUE) or list(list()) is to create an empty object{} in JS
    if (!('xAxis' %in% names(opts))) opts$xAxis <- list(show=TRUE)
    if (!('yAxis' %in% names(opts))) opts$yAxis <- list(show=TRUE)
    if (!('series' %in% names(opts))) opts$series <- list(
    	list(type=if (is.null(ctype)) 'scatter' else ctype) 
    )
    if (opts$series[[1]]$type == 'parallel') {
      opts$xAxis <- opts$yAxis <- NULL
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
    jcode = js,
    opts = opts,
    settings = list(
      crosstalk_key = key,
      crosstalk_group = group
    )
  )
  
  if (!is.null(df)) {
    # if data.frame given, assign to dataset regardless of parameter 'preset'
    if (!'data.frame' %in% class(df)) 
      stop('ec.init: df must be a data.frame', call. = FALSE)
    
    # grouping uses transform - a v.5 feature
    grnm <- NULL
    if (!is.null(ctype) && dplyr::is.grouped_df(df)) {
      grnm <- dplyr::group_vars(df)[[1]]   # just 1st grouping column name
      x$opts$dataset <- list(list(source = ec.data(df)))
      grvals <- unname(unlist(dplyr::group_data(df)[grnm]))
      txfm <- list(); k <- 0
      x$opts$legend = list(data=list())
      for(nm in grvals) { 
        k <- k+1
        txfm <- append(txfm, list(list(transform = list(
          type='filter', config=list(dimension=grnm, '='=nm)))))
        x$opts$series[[k]] <- list(
          type=ctype, datasetIndex=k, name=as.character(nm))
        if (colnames(df)[1]==grnm)  # grouping by 1st column
          x$opts$series[[k]]$encode <- list(x=1, y=2)  # JS count
        x$opts$legend$data <- append(x$opts$legend$data, list(list(name=as.character(nm))))
      }
      x$opts$dataset <- append(x$opts$dataset, txfm)
    } 
    else 
      x$opts$dataset <- list(list(source = ec.data(df)))

    if (preset) {
      if (!is.null(x$opts$xAxis)) { 
        # update xAxis,yAxis if X,Y columns are categorical
        colX <- 1     # by default 1st column is X, second is Y
        if (!is.null(grnm) && colnames(df)[1]==grnm)  
            colX <- 2  # grouping is by 1st column
        if (!is.numeric(unname(unlist(df[colX]))))
          x$opts$xAxis <- list(type='category')
        if (!is.numeric(unname(unlist(df[(colX+1)]))))
          x$opts$yAxis <- list(type='category')
      }
      if (!is.null(x$opts$series) && 
          x$opts$series[[1]]$type == 'parallel')
        x$opts$parallelAxis <- ec.paxis(df)
    }
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
  theme <- getOption('echarty.theme')   # default
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
      urltls <- getOption('echarty.tiles')
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
    if (preset) {       # replace 2D presets with 3D
      if (ctype != 'scatterGL') {  # scatterGL is 2D
        wt$x$opts$xAxis <- NULL   
        wt$x$opts$yAxis <- NULL
        wt$x$opts$grid3D  <- list(list())
        wt$x$opts$xAxis3D <- list(list())
        wt$x$opts$yAxis3D <- list(list())
        wt$x$opts$zAxis3D <- list(list())
      }
      # valid 3D types: scatter3D, bar3D, surface, etc.
      if ('series' %in% names(wt$x$opts)) {  # if default 2D, change it
        wt$x$opts$series <- lapply(wt$x$opts$series,
          function(s) {s$type= if (s$type=='scatter') 'scatter3D' else s$type; s })
      }
    }
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-gl@2.0.9/dist/echarts-gl.min.js', ask)
  }
  if ('world' %in% load) {
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts@4.9.0/map/js/world.js', ask)
    if (preset) {
      wt$x$opts$xAxis <- NULL
      wt$x$opts$yAxis <- NULL
      wt$x$opts$series <- list(list(type='map', geoIndex=0))
      wt$x$opts$geo = list(map='world', roam=TRUE)  # !duplicate map if series have map=
      # if (!is.null(df))  # cancelled: don't know if first 2 cols are 'lng','lat'
      #   wt$x$opts$geo$center= c(mean(unlist(df[,1])), mean(unlist(df[,2])))
    }
  }
  if ('liquid' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-liquidfill@latest/dist/echarts-liquidfill.min.js', ask)
  
  if ('gmodular' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-graph-modularity@latest/dist/echarts-graph-modularity.min.js', ask)
  
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
      stop('ec.init: tl.series requires a grouped data.frame', call. = FALSE)

    if (is.null(tl.series$encode))
      stop('ec.init: encode is required for tl.series', call. = FALSE)

    # add missing defaults
    if (is.null(tl.series$type)) tl.series$type <- 'line'
    #if (is.null(tl.series$coordinateSystem)) tl.series$coordinateSystem <- 'cartesian2d' # not for gauge
    # not in any coordinate system: 'pie','funnel','gauge','graph', tree/treemap/sankey
    if (is.null(tl.series$coordinateSystem))
      tl.series$coordinateSystem <- 'unknown'
    if (tl.series$type == 'pie') {
      xtem <- 'value'; ytem <- 'itemName' }
    if (tl.series$type %in% c('line','scatter','bar'))
      tl.series$coordinateSystem <- 'cartesian2d'
    if (tl.series$coordinateSystem=='cartesian2d') { 
      xtem <- 'x'; ytem <- 'y' }
    if (startsWith(tl.series$coordinateSystem, 'cartesian')) { 
      xtem <- 'x'; ytem <- 'y'; ztem <- 'z' }
    else if (tl.series$coordinateSystem=='polar') { 
      xtem <- 'radius'; ytem <- 'angle' }
    else if (tl.series$coordinateSystem %in% c('geo','leaflet')) { 
      xtem <- 'lng'; ytem <- 'lat'
      center <- c(mean(unlist(df[,tl.series$encode$lng])),
                  mean(unlist(df[,tl.series$encode$lat])))
      if (tl.series$coordinateSystem=='geo')     wt$x$opts$geo$center <- center
      if (tl.series$coordinateSystem=='leaflet') wt$x$opts$leaflet$center <- center
    }
    if (is.null(unlist(tl.series$encode[xtem]))) {
      # append col XcolX 1:max for each group
      df <- df |> group_modify(~ { .x |> mutate(XcolX = 1:nrow(.)) })
      tl.series$encode[xtem] <- 'XcolX'    # instead of relocate(XcolX)
      # replace only source, transforms stay
      wt$x$opts$dataset[[1]] <- list(source=ec.data(df))
    }
    if (is.null(unlist(tl.series$encode[ytem])))
      stop("ec.init: encode 'y' is required for tl.series", call.=FALSE)
    
    # dataset is already in, now set everything else
    #wt$x$opts$xAxis <- list(type='category')  # geo,leaf do not like
    wt$x$opts$series <- NULL
    wt$x$opts$legend <- NULL
    
    # loop group column(s)
    gvar <- df |> group_vars() |> first()
    gvar <- as.character(gvar)  # convert if factor
    di <- 0
    optl <- lapply(df |> group_split(), function(gp) {
      di <<- di+1
      # nicer looking lines with sorted X 
      #if (!is.null(xcol)) gp <- gp |> arrange(across(all_of(xcol)))
      
      # multiple series for each Y, like y=c('col1', 'col3')
      series <- lapply(unname(unlist(tl.series$encode[ytem])), 
        function(sname) {
          append(list(datasetIndex= di, name= sname), tl.series)
      })
      series <- lapply(series, function(s) {
        s$encode[ytem] <- s$name   # replace multiple col.names with one
        s
      })

      list(title= list(text= as.character(unique(gp[gvar]))),  
           series= unname(series))
    })
    
    wt$x$opts$options <- optl
    
    steps <- lapply(optl, function(x) { paste(x$title$text, collapse=' ') })
    wt$x$opts$timeline <- list(data=steps, axisType='category')
  }
  
  snip <- getOption('echarty.short')
  if (!is.null(snip) && snip) {
    snip <- wt$x$opts
    snip$saved <- wt
    snip$saved$opts <- NULL
    wt <- snip
  }
  return(wt)
}


#' Data helper
#' 
#' Make data lists from a data.frame
#' 
#' @param df Chart data in data.frame format, required. \cr
#' Except when format is 'dendrogram', then df is a list, result of \code{\link[stats]{hclust}} function.
#' @param format A key on how to format the output list \cr \itemize{
#'  \item 'dataset' = list to be used in \href{https://echarts.apache.org/en/option.html#dataset.source}{dataset} (default), or in \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} (without header). \cr
#'  \item 'values' = list for customized \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} \cr
#'  \item 'names' = named lists useful for named data like \href{https://echarts.apache.org/en/option.html#series-sankey.links}{sankey links}.
#'  \item 'boxplot' = build dataset and source lists, see Details
#'  \item 'dendrogram' = build series data for Hierarchical Clustering dendrogram
#'  }
#' @param header Boolean to include the column names in dataset, default TRUE.\cr
#'    Set this to FALSE when used in \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data}.
#' @return A list for \emph{dataset.source}, \emph{series.data} or a list of named lists.\cr
#'   For boxplot - a named list, see Details and Examples \cr
#'   For dendrogram - a tree structure, see format in \href{https://echarts.apache.org/en/option.html#series-tree.data}{tree data}
#' @details `format='boxplot'` requires the first two \emph{df} columns as: \itemize{
#'   \item column for the non-computational categorical axis
#'   \item column with (numeric) data to compute the five boxplot values
#'  }
#'  Grouped \emph{df} is supported. Groups will show in the legend, if enabled.\cr
#'  Returns a `list(dataset, series)` to set the chart dataset and series.\cr
#'  Make sure there is enough data for computation, like >4 values per boxplot. Otherwise ECharts may exit with a \emph{Object.transform} error.
#' @seealso some live \href{https://rpubs.com/echarty/data-models}{code samples}
#' 
#' @examples
#' library(dplyr)
#' #ds <- Orange |> relocate(age,circumference) |> ec.data(format='boxplot')
#' #ds <- Orange |> relocate(age,circumference) |> group_by(Tree) |> ec.data(format='boxplot')
#' #ds <- mtcars |> relocate(am,mpg) |> ec.data(format='boxplot')
#'  ds <- mtcars |> relocate(am,mpg) |> group_by(cyl) |> ec.data(format='boxplot')
#' p <- ec.init()
#' p$x$opts <- list(
#'   dataset= ds$dataset, 
#'   series=  ds$series, 
#'   yAxis= list(type= 'category'), # categorical yAxis = boxplots horizontal
#'   xAxis= list(show= TRUE),       # categorical xAxis = boxplots vertical
#'   legend= list(show= TRUE)
#' )
#' p 
#' 
#' hc <- hclust(dist(USArrests), "complete")
#' p <- ec.init(preset= FALSE)
#' p$x$opts$series <- list(list(
#'   type= 'tree', orient= 'TB', roam= TRUE, initialTreeDepth= -1,
#'   data= ec.data(hc, format='dendrogram'),
#'   # layout= 'radial', symbolSize= ec.clmn(-1, scale= 0.33),
#'   #    exclude added labels like 'pXX', leaving only the originals
#'   label= list(formatter= htmlwidgets::JS(
#'     "function(n) { out= /p\\d+/.test(n.name) ? '' : n.name; return out;}"))
#' ))
#' p
#' 
#' @importFrom utils tail
#' @importFrom grDevices boxplot.stats
#' @export
ec.data <- function(df, format='dataset', header=TRUE) {
  if (missing(df))
    stop('ec.data: expecting parameter df', call. = FALSE)
  if (format=='dendrogram') { 
    if (class(df) != 'hclust')
      stop('ec.data: df should be hclust for dendrogram', call. = FALSE)
    hc <- df
    # decode hc$merge with hc$labels
    inew <- list()
    i <- 0
    tmp <- apply(hc$merge, 1, function(x) {
      fst <- if (x[1]<0) { if (is.null(hc$labels)) -x[1] else hc$labels[-x[1]] } else inew[[x[1]]]$p[1]
      snd <- if (x[2]<0) { if (is.null(hc$labels)) -x[2] else hc$labels[-x[2]] } else inew[[x[2]]]$p[1]
      i <<- i+1
      inew <<- append(inew, list(
        list(p= rep(paste0('p',i),2), c= c(fst, snd), 
             h= rep(round(hc$height[i], 2), 2))))
    })
    tmp <- unlist(inew)
    parents <- children <- vals <- c()
    for(i in 1:length(tmp)) {
      fst <- substr(names(tmp[i]), 1, 1)
      switch(fst,
             'p'= parents <- c(parents, tmp[i]),
             'c'= children <- c(children, tmp[i]),
             'h'= vals <- c(vals, as.numeric(tmp[i]))
      )
    }
    # add top element, required for tree chart
    vals <- c(unname(vals), 1)
    children <- c(unname(children), unname(tail(parents,1)))
    parents <- c(unname(parents), '')
    
    # convert from data.frame to JSON
    dafr <- data.frame(parents=parents, children=children, value=vals)
    tmp <- data.tree::FromDataFrameNetwork(dafr)
    json <- data.tree::ToListExplicit(tmp, unname=TRUE)
    return(json$children)
    
  } 
  else if (!'data.frame' %in% class(df))
    stop('ec.data: df has to be data.frame', call. = FALSE)
  
  rownames(df) <- NULL
  n <- seq_along(df[[1]])       # assuming all lists in df have the same length
  tmp <- lapply(n, function(i) lapply(df, "[[", i))  # preserve column types
  
  if (format=='dataset') {
    datset <- lapply(tmp, unname)
    if (header)
      datset <- c(list(colnames(df)), datset)
    
  } else if (format=='values' || isTRUE(format)) {
    datset <- lapply(tmp, function(x) list(value=unlist(unname(x))))
    
  } else if (format=='boxplot') {
    cn <- colnames(df)
    if (length(cn)<2) stop('boxplot: df should have 2+ columns', call.=FALSE)
    colas <- cn[1]; colb5 <- cn[2]
    if (!is.numeric(df[[colb5]])) stop('boxplot: 2nd column must be numeric', call.=FALSE)
    # will group by colas column anyway
    colgrp <- if (is.grouped_df(df) && group_vars(df)[1]!=colas) 
      group_vars(df)[1] else NULL
    
    if (!is.null(colgrp)) {   # grouped
      series <- list()
      tmp <- df |> group_split()
      dataset <- lapply(tmp, function(dd) { 
        dv <- dd |> dplyr::group_by(.data[[colas]]) |> group_split()
        src <- lapply(dv, function(vv) vv[[colb5]])
        list(source= if (length(src[[1]])==1) list(src) else src)
      })
      for (i in 1:length(tmp)) { 
        dataset <- append(dataset, list(list(
          fromDatasetIndex= i-1, transform= list(type= 'boxplot')))) 
        series <- append(series, list(list(
          name= tmp[[i]][[colgrp]][1], 
          type= 'boxplot', datasetIndex= i+length(tmp)-1)) )
      }
      
    } else {  # non-grouped
      bdf <- ungroup(df) |> dplyr::group_by(across({colas})) |> group_split()
      dats <- lapply(bdf, function(x) {
        c(unique(pull(x,colas)), round(boxplot.stats( pull(x,colb5) )$stats, 4))
      })
      dataset <- list(source= ec.data( as.data.frame(do.call(rbind, dats)) ))
      # default is horizontal, for vertical switch xAxis/yAxis category type
      series <- list(list(type='boxplot', encode= list(y='V1', x=c('V2','V3','V4','V5','V6'))))
    }
    return(list(dataset= dataset, series= series)) 
    
  } else    # ='names'
    datset <- tmp

  return(datset)
}


#' Data column
#' 
#' Helper function to display/format data column(s) by index or name
#' 
#' @param col A single column index(number) or column name(quoted string), \cr
#'    or a \code{\link[base]{sprintf}} format string. 
#' @param ... A comma separated column indexes or names, only when \emph{col} is \emph{sprintf}. This allows formatting of multiple columns, as for a tooltip.\cr
#' @param scale A positive number, multiplier for numeric columns. When scale is 0, all numeric values are rounded.
#' @return A JavaScript code string (usually a function) marked as executable, see \code{\link[htmlwidgets]{JS}}.
#'  
#' @details Column indexes are counted in R and start at 1.\cr
#' Set column index to -1 for single values, like \emph{tree} chart, \emph{axisLabel.formatter} or \emph{valueFormatter}.\cr
#' Column indexes are decimals for combo charts with multiple series, see [ecr.band] example. The whole number part is the serie index, the decimal part is the column index inside.
#' \emph{col} as sprintf has the same placeholder \emph{%@} for both column indexes or column names.\cr
#' \emph{col} as sprintf can contain double quotes, but not single or backquotes.\cr
#' Placeholder \emph{%L@} will display a number in locale format, like '12,345.09'.\cr
#' Placeholder \emph{%LR@} will display a rounded number in locale format, like '12,345'.\cr
#' Placeholder \emph{%R@} will display a rounded number, like '12345'.\cr
#' Useful for attributes like formatter, color, symbolSize. 
#' 
#' @examples
#' tmp <- data.frame(Species = as.vector(unique(iris$Species)),
#'                   emoji = c('\U0001F33B','\U0001F335','\U0001F33A'))
#' df <- iris |> dplyr::inner_join(tmp)   # add 6th column 'emoji'
#' p <- df |> dplyr::group_by(Species) |> ec.init()
#' p$x$opts$series <- list(list(
#'   type='scatter', label=list(show=TRUE, formatter= ec.clmn(6))  # ref 6th column
#' ))
#' p$x$opts$tooltip <- list(formatter=     # sprintf + multiple column indexes
#'    ec.clmn('species <b>%@</b><br>s.len <b>%@</b><br>s.wid <b>%@</b>', 5,1,2))
#' p
#' 
#' @export
ec.clmn <- function(col=NULL, ..., scale=1) {
  if (is.null(col)) stop('ec.clmn: col is required', call.=FALSE)
  if (is.null(scale)) scale=1
  if (scale==1) scl <- 'return c;' 
  else { if (scale==0) scl <- 'return Math.round(c);'
         else scl <- paste0('return (parseFloat(c)*',scale,');') }
  args <- list(...)
  if (is.na(suppressWarnings(as.numeric(col)))) {   # col is string
    if (length(args)==0) {  # col is solitary name
      ret <- paste0('let c=(!x.data) ? `no data` : x.data.',col,'; ',scl)
      
    } else {                # col is sprintf
      spf <- "var sprintf= (template, values) => { let j=0;
return template.replace(/%@|%L@|%LR@|%R@/g, (m) => {
  if (m=='%@') return values[j++];
  if (m=='%L@') return Number(values[j++]).toLocaleString();
  if (m=='%LR@') return Math.round(Number(values[j++])).toLocaleString();
  if (m=='%R@') return Math.round(Number(values[j++]));
}); };"
      
      tmp <- suppressWarnings(as.numeric(args) -1)
      if (all(is.na(tmp))) {   
        # multiple non-numeric strings = column names
        t0 <- sapply(args, function(s) toString(paste0('x.data.', s)) )
        t0 <- paste(t0, collapse=',')
        t1 <- paste(args, collapse='`,`')
        ret <- paste0( spf, " if (!x.data) return `no data`; 
let args=[`",t1,"`], vv=[]; pos=[];
if (x.dimensionNames && x.dimensionNames.length>0) 
  pos= args.map(z => x.dimensionNames.indexOf(z));
if (x.data.length)
  vv= pos.map(p => x.data[p]);
else
  vv= (x.data.value && x.data.value.length>0) 
     ? pos.map(p => x.data.value[p]) : [",t0,"];"
        )
      }
      else {   
        #  multiple numeric, they could be in x, x.data, x.value OR x[].value[]
        #  in combo-charts (ec.band), get decimal portion as .value index
        tmp <- paste(tmp, collapse=',')
        ret <- paste0( spf, "let ss=[",tmp,"];
let vv= ss.map((e) => { 
    if (e<0) return x.value ? x.value : x;
    let i= Math.floor(e);
    return x.value!=null ? x.value[i] : 
           x.data!=null  ? x.data[i] : 
           x[i]!=null    ? x[i] : `no data` });
if (typeof vv[0] === 'object' && vv[0].value) {
    vv = ss.map((e,idx) => { 
      f= Math.round(e % 1 *10) -1;
      return vv[idx].value[f];  }) };")
      }
      if (scale >0) ret <- paste(ret,"vv= vv.map(e => isNaN(e) ? e : e*",scale,");")
      if (scale==0) ret <- paste(ret,"vv= vv.map(e => isNaN(e) ? e : Math.round(e));")
      ret <- paste(ret, "let c = sprintf(`",col,"`, vv); return c; ")
    }
  }
  else {      # col is solitary numeric
    if (length(args) > 0)
      warning('col is numeric, others are ignored', call.=FALSE)
    col <- as.numeric(col) - 1   # from R to JS counting
    if (col < 0)  # just a value is expected
      ret <- paste('let c=x;',scl)
    else
      ret <- paste0('let c = x.value!=null ? x.value[',col,'] : x.data!=null ? x.data[',col,'] : x[',col,']; ',scl)
  }
  htmlwidgets::JS(paste0('function(x) {', ret, '}'))
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
#'  \item 'polygon' - by drawing a polygon as polyline (default). Warning: cannot be zoomed!
#' }
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-line.type}{serie}
#' @return A list of one serie when type='polygon', or two series when type='stack'
#'
#' @details When type='polygon', coordinates of the two boundaries are chained into a polygon and displayed as one.\cr
#'      When type='stack', two \emph{stacked} lines are drawn, one with customizable areaStyle. The upper boundary coordinates should be values added on top of the lower boundary coordinates.\cr
#'      Type 'stack' needs \emph{xAxis} to be of type 'category'.
#' 
#' @examples 
#' if (interactive()) {
#' df <- airquality |> dplyr::mutate(lwr= round(Temp-Wind*2), 
#'                                   upr= round(Temp+Wind*2), 
#'                                   x=paste0(Month,'-',Day) ) |>
#'                     dplyr::relocate(x,Temp)
#' bands <- ecr.band(df, 'lwr', 'upr', type='stack', 
#'                   name='stak', areaStyle= list(opacity=0.4))
#' p <- df |> ec.init(load='custom')
#' p$x$opts$xAxis <- list(type='category', boundaryGap=FALSE)
#' p$x$opts$series <- list(list(type='line', color='blue', name='line'), 
#'                         bands[[1]], bands[[2]] )
#' p$x$opts$tooltip <- list(trigger= 'axis',
#'    formatter= ec.clmn('high <b>%@</b><br>line <b>%@</b><br>low <b>%@</b>', 
#'         3.3, 1.2, 2.2))  # 3.3= upper-serie index .index of column inside
#' p$x$opts$legend <- list(show= TRUE)
#' p
#' }
#' @export
ecr.band <- function(df=NULL, lower=NULL, upper=NULL, type='polygon', ...) {
  if (is.null(df) || is.null(lower) || is.null(upper)) 
    stop("ecr.band: df, lower and upper are all required", call. = FALSE)
  if (!"data.frame" %in% class(df)) 
    stop("ecr.band: df must be a data.frame", call. = FALSE)
  if (!is.numeric(df[lower][[1]]) || !is.numeric(df[upper][[1]]))
    stop("ecr.band: lower and upper must be numeric", call. = FALSE)
  
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
    #if (is.null(slow$smooth)) slow$smooth <- TRUE
    if (is.null(slow$lineStyle)) slow$lineStyle <- list(width=0)
    supr <- slow
    if (!is.null(slow$areaStyle)) slow$areaStyle <- NULL
    else supr$areaStyle <- astyle
    # save upper data for tooltip, 'hi' is just difference
    tmp <- data.frame(x = df[fstc][[1]], lo=df[lower][[1]], 
                      hi = df[upper][[1]] - df[lower][[1]], 
                      ttip = df[upper][[1]] )
    slow$data <- ec.data(tmp[,c('x','lo')], header=FALSE)
    supr$data <- ec.data(tmp[,c('x','hi','ttip')], header=FALSE)
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
#'  \emph{ecr.ebars} are custom series, so \emph{ec.init(load='custom')} is required. \cr
#'  Grouped series are supported, but require the group column to be included in \emph{df}. \cr
#'  Will add a chart legend and its own tooltip if none is provided.\cr
#'  ecr.ebars with name attribute will show separate in the legend. \cr
#'  Command should be called last, after all other series.
#'
#' @examples
#' if (interactive()) {
#' tmp <- round(rnorm(24, sin(1:24/2)*10, .5))
#' df <- data.frame(x = 1:24, val = tmp, 
#'                  lower = round(rnorm(24, tmp -10, .5)),
#'                  upper = round(rnorm(24, tmp + 5, .8))
#' )
#' p <- df |> ec.init(load='custom') |> ecr.ebars()
#' p$x$opts$tooltip <- list(show=TRUE)
#' p
#' }
#' @export
ecr.ebars <- function(wt, df=NULL, hwidth=6, ...) {
  # alternating bars with custom series doesn't work, first bars then customs
  if (missing(wt)) stop('ecr.ebars: expecting widget', call.=FALSE)
  if (!is.null(df) && !inherits(df, "data.frame")) 
    stop('ecr.ebars: df must be a data.frame', call.=FALSE)
  if (!'renderers' %in% unlist(lapply(wt$dependencies, function(d) d$name)))
    stop("ecr.ebars: use ec.init(load='custom') for ecr.ebars", call.=FALSE)
  if (!is.null(wt$saved))
    stop('ecr.ebars: did you place ec.snip at end-of-pipe?', call. = FALSE)
  
  ser <- wt$x$opts$series  # all series
  if (is.null(ser)) stop('ecr.ebars: series are missing', call.=FALSE)
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
      stop('ecr.ebars: dataset is grouped, use df parameter', call. = FALSE)
  }
  else {
    if (dplyr::is.grouped_df(df)) {    # groups
      grnm <- dplyr::group_vars(df)[[1]]   # just 1st one matters
      tmp <- df |> dplyr::group_split()
      cser <- lapply(tmp, function(gp) {
        name <- unlist(unique(unname(gp[,grnm])))
        oneSerie(name, gp)
      })
      wt$x$opts$xAxis$type <- 'category'
    }
    else
      cser <- list(oneSerie(names(df)[2], df))
  }
  wt$x$opts$series <- append(wt$x$opts$series, cser)
  if (!("legend" %in% names(wt$x$opts))) wt$x$opts$legend <- list(show=TRUE)
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
#' \emph{p_merge} - modify or add series features like style,marks,etc.\cr
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
    stop('ecs.exec: missing proxy', call. = FALSE)
  if (!'ecsProxy' %in% class(proxy)) 
    stop('ecs.exec: must pass ecsProxy object', call. = FALSE)
  if (is.null(proxy$x) || is.null(proxy$x$opts))
    stop('ecs.exec: proxy is empty', call. = FALSE)
  
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
#' @details  
#' For 3-4 charts one would use multiple series with a \href{https://echarts.apache.org/en/option.html#grid}{grid}. \cr
#' For greater number of charts _ec.layout_ come in handy.
#' @examples
#' options(browser = 'firefox')
#' tmp <- lapply(list('dark','macarons','gray','jazz','dark-mushroom'),
#'               function(x) cars |> ec.init() |> ec.theme(x) )
#' ec.layout(tmp, cols=2 )
#' 
#' @export 
ec.layout <- function (plots, rows = NULL, cols = NULL, width = "xs", 
                       title = NULL) 
{
  if (!is.list(plots))
    stop('ec.layout: charts must be a list', call. = FALSE)
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


#' Parallel Axis
#' 
#' Create 'parallelAxis' for a parallel chart
#' 
#' @param df A data.frame, regular or grouped
#' @param minmax Boolean to add max/min limits or not, default TRUE
#' @param cols A string vector with columns names in desired order
#' @param ... Additional arguments for \href{https://echarts.apache.org/en/option.html#parallelAxis}{parallelAxis}.
#' @return A list, see format in \href{https://echarts.apache.org/en/option.html#parallelAxis}{parallelAxis}.
#' @examples
#' iris |> dplyr::group_by(Species) |> ec.init(ctype='parallel')
#' 
#' p <- ec.init(preset=FALSE) 
#' p$x$opts$parallelAxis <- ec.paxis(mtcars, 
#'      cols= c('gear','cyl','hp','carb'), nameRotate= 45)
#' p$x$opts$series <- list(list( type= 'parallel',
#'      smooth= TRUE, data= ec.data(mtcars,'dataset',FALSE) ))
#' p
#' 
#' @export 
ec.paxis <- function (df=NULL, minmax=TRUE, cols=NULL, ...) {
  if (!'data.frame' %in% class(df))
    stop('ec.paxis: df has to be data.frame', call.= FALSE)
  pax <- list(); grnm <- ''
  cfilter <- 1:length(colnames(df))
  if (dplyr::is.grouped_df(df)) {
    # dont include grouping column (grnm)
    grnm <- dplyr::group_vars(df)[[1]]
    cfilter <- cfilter[!cfilter==match(grnm,colnames(df))]
  }
  if (!is.null(cols)) {
    if (!all(cols %in% colnames(df))) stop('ec.paxis: some cols not found', call.= FALSE)
    cfilter <- match(cols, colnames(df))  # indexes
  }
  for(i in cfilter) {
    cn <- colnames(df)[i]
    tmp <- list(dim=i-1, name=cn, ...)  # JS count is -1
    if (!is.numeric(df[cn][[1]]))
      tmp$type <- 'category'
    else {
      if (minmax) {
        tmp$max <- max(df[cn])
        tmp$min <- min(df[cn])
      }
    }
    pax <- append(pax, list(tmp)); 
  }
  pax
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
#' mtcars |> ec.init() |> ec.theme('dark-mushroom')
#' cars |> ec.init() |> ec.theme('mine', code=
#'   '{"color": ["green","#eeaa33"], 
#'     "backgroundColor": "lemonchiffon"}')
#' 
#' @export
ec.theme <- function (wt, name, code= NULL) 
{
  if (missing(name))
    stop('ec.theme: must define a name', call. = FALSE)
  if (!is.null(wt$saved))
    stop('ec.theme: did you place ec.snip at end-of-pipe?', call. = FALSE)
  
  #if (length(wt$x[[1]])==0) wt$x[[1]] <- NULL  # parasite list
  wt$x$theme <- name
  if (!is.null(code))
    wt$x$themeCode <- code
  else {
    wt$x$themeCode <- NULL
    path <- system.file('themes', package= 'echarty')
    dep <- htmltools::htmlDependency(
      name= name,
      version= '1.0.0', src= c(file= path),
      script= paste0(name, '.js'))
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
#' @param json Boolean whether to return a JSON, or a \code{list}, default TRUE
#' @param ... Additional arguments to pass to \code{\link[jsonlite]{toJSON}}
#' @return A JSON string if \code{json} is \code{TRUE} and
#'  a \code{list} otherwise.
#'
#' @note Must be invoked or chained as last command.\cr
#' ec.inspect is incompatible with [ec.snip]
#'
#' @examples
#' # extract JSON
#' json <- cars |> ec.init() |> ec.inspect()
#' json
#'
#' # get from JSON and modify plot
#' ec.fromJson(json) |> ec.theme('macarons')
#'
#' @export
ec.inspect <- function(wt, target=NULL, json=TRUE, ...) {
  if (!is.null(wt$saved))
    stop('ec.inspect: incompatible with ec.snip', call. = FALSE)
  
  opts <- wt$x$opts
  
  if (!is.null(target)) {
    if (target!='data') stop("ec.inspect: only target='data' supported", call. = FALSE)
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
#' @param ... Any arguments to pass to internal [ec.init]
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


#' Options list shortcut
#' 
#' Utility to improve readability and typing speed
#' 
#' @param wt A widget to be converted to option list \cr
#'    OR an option list to a plot
#' @details 
#' On initialization, add _ec.snip_ at the end of the [ec.init] pipe, \cr
#'   or set for the entire R session with \code{options('echarty.short'=TRUE)}.\cr
#' Note: ec.theme, ecr.ebars, ec.inspect will not work with the session setting. 
#'
#' @examples 
#' p <- cars |> ec.init() |> ec.snip()
#' p$dataZoom <- list(start=70)   # instead of p$x$opts$dataZoom
#' p$legend  <- list(show=TRUE)   # instead of p$x$opts$tooltip
#' ec.snip(p)                     # instead of just p
#' 
#' @export
ec.snip <- function(wt) {
  if (missing(wt))
    stop('ec.snip: expecting wt as htmlwidget or list', call.=FALSE)

  if ('echarty' %in% class(wt)) {
    # prepare for shorthand writing
    op <- wt$x$opts
    op$saved <- wt
    op$saved$opts <- NULL
    op
  } 
  else {
    # return the chart widget to plot
    # wt$x holds theme, registerMap, jcode, locale, renderer, etc.
    p <- wt$saved
    wt$saved <- NULL
    p$x$opts <- wt
    p
  }
}


# ----------- Internal --------------


#' Install Javascript plugin from URL source
#' 
#' @param wt A widget to add dependency to, see \code{\link[htmlwidgets]{createWidget}}
#' @param source URL or file:// of a Javascript plugin, \cr
#'   file name suffix is '.js'. Default is NULL.
#' @param ask Boolean, to ask the user to download source if missing. Default is FALSE.
#' @return A widget with JS dependency added if successful, otherwise input wt
#'
#' @details When \emph{source} is URL, the plugin file is installed with an optional popup prompt.\cr
#'   When \emph{source} is a file name (file://xxx.js), it is assumed installed and only a dependency is added.\cr
#'   Called internally by [ec.init]. It is recommended to use \emph{ec.init(load=...)} instead of \emph{ec.plugjs}.
#'   
#' @examples
#' # import map plugin and display two (lon,lat) locations
#' if (interactive()) {
#' p <- ec.init() |> ec.plugjs(
#'   'https://raw.githubusercontent.com/apache/echarts/master/test/data/map/js/china-contour.js')
#' p$x$opts <- list(
#'   geo = list(map= 'china-contour', roam= TRUE),
#'   series = list(list( name= 'Geo',
#'     type= 'scatter', coordinateSystem= 'geo',
#'     symbolSize= 9, itemStyle= list(color= 'red'),
#'     data= list(list(value= c(113, 40)), list(value= c(118, 39))) ))
#' )
#' p
#' }
#' @importFrom utils askYesNo download.file
#'
#' @export
ec.plugjs <- function(wt=NULL, source=NULL, ask=FALSE) {
  if (missing(wt))
    stop('ec.plugjs: expecting widget', call. = FALSE)
  if (is.null(source)) return(wt)
  if (!startsWith(source, 'http') && !startsWith(source, 'file://'))
    stop('ec.plugjs: expecting source as URL or file://', call. = FALSE)
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

# needed by widget init
.preRender <- function(wt) {

  ff <- getOption('echarty.font')
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
  }
  
  .onLoad <- function(libname, pkgname) {
      shiny::registerInputHandler('echartyParse', function(data, ...) {
        jsonlite::fromJSON(jsonlite::toJSON(data, auto_unbox = TRUE))
      }, force = TRUE)
  }
  
}

#  ------------- Global Options -----------------
#' 
#' For info on options and prefixes, type \emph{?echarty} in the R console.



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

