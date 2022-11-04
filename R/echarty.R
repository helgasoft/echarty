# ----------- General --------------

#' Introduction
#' 
#' echarty provides a lean interface between R and Javascript library ECharts.\cr
#' With only two major commands (_ec.init_ and _ec.upd_), it can trigger multiple native ECharts options to build a chart. \cr
#' The benefits - learn a very limited set of commands, and enjoy the **full functionality** of ECharts.
#' 
#' @includeRmd vignettes/info.Rmd
#'
#' @name -- Introduction --
NULL

#' Initialize command
#'
#' Required to build a chart. In most cases this will be the only command necessary.
#'
#' @param df A data.frame to be preset as \href{https://echarts.apache.org/en/option.html#dataset}{dataset}, default NULL \cr
#'   By default the first column is for X values, second column is for Y, and third is for Z when in 3D.\cr
#'   Best practice is to have the grouping column placed last. Grouping column cannot be used as axis.\cr
#'   Timeline requires a \emph{grouped data.frame} to build its \href{https://echarts.apache.org/en/option.html#options}{options}.\cr
#'   If grouping is on multiple columns, only the first one is used to determine settings.
#' @param ctype Chart type of series. Default is 'scatter'. Set to NULL to disable series preset.
#' @param preset Build preset xAxis,yAxis,serie for 2D, or grid3D,xAxis3D,yAxis3D,zAxis3D for 3D, default TRUE (enable).
#' @param width,height A valid CSS unit (like \code{'100\%'},
#'   \code{'500px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param tl.series A list to build a timeline or NULL(default). The list defines options \href{https://echarts.apache.org/en/option.html#series}{series} and their attributes. \cr
#'   The only required attribute is \href{https://echarts.apache.org/en/option.html#series-line.encode}{encode}.\cr
#'  \emph{encode} defines which data columns names to use for the axes: \cr
#'  * set \emph{x} and \emph{y} for coordinateSystem \emph{cartesian2d}
#'  * set \emph{lng} and \emph{lat} for coordinateSystem \emph{geo}
#'  * set \emph{radius} and \emph{angle} for coordinateSystem \emph{polar}
#'  * set \emph{value} and \emph{itemName} for \emph{pie} chart
#'  * set \emph{value} and \emph{name} for \emph{map} chart
#'  
#'   Attribute \emph{coordinateSystem} is not set by default and depends on chart \emph{type}.\cr
#'   Custom attribute \emph{groupBy}, a \emph{df} column name, can create series groups inside each timeline step.
#'   A grouped \emph{df} must be present, with group column providing the \href{https://echarts.apache.org/en/option.html#timeline.data}{timeline data}.
#'   Auto-generated \emph{timeline} and \emph{options} will be preset for the chart.\cr
#'   \emph{tl.series} cannot be used for hierarchical charts like graph,tree,treemap,sankey. Chart options/timeline have to be built directly, see \href{https://helgasoft.github.io/echarty/uc4.html}{example}.
#' @param ... other arguments to pass to the widget. \cr
#'   Custom echarty widget arguments include: \cr
#'  * elementId - Id of the widget, default is NULL(auto-generated)
#'  * load - name(s) of plugin(s) to load. A character vector or comma-delimited string. default NULL.
#'  * ask - prompt user before downloading plugins when \emph{load} is present, FALSE by default
#'  * js - single string or a vector with JavaScript expressions to evaluate.\cr 
#'      First expression is evaluated before chart initialization. \cr
#'      Second is evaluated with exposed object \emph{opts}. \cr
#'      Third is evaluated with exposed \emph{chart} object after \emph{opts} have been set.
#'  * renderer - 'canvas'(default) or 'svg'
#'  * locale - 'EN'(default) or 'ZH'. Use predefined or custom \href{https://gist.github.com/helgasoft/0618c6537c45bfd9e86d3f9e1da497b8}{like so}.
#'  * useDirtyRect - enable dirty rectangle rendering or not, FALSE by default, see \href{https://echarts.apache.org/en/api.html#echarts.init}{here}
#'
#' @details  Command \emph{ec.init} creates a widget with \link[htmlwidgets]{createWidget}, then adds some ECharts features to it.\cr
#'  When \emph{ec.init} is chained after a data.frame, a \href{https://echarts.apache.org/en/option.html#dataset}{dataset} is preset. \cr
#'  When data.frame is grouped and \emph{ctype} is not null, more datasets with legend and series are also preset. Grouped series are preset as type \emph{scatter}. \cr
#'  Plugin '3D' presets will not work for 'scatterGL'. Instead, use \emph{preset=FALSE} and set explicitly \emph{xAxis,yAxis}. \cr
#'  Plugins 'leaflet' and 'world' preset zoom=6 and center to the mean of all coordinates. \cr
#'  Users can delete or overwrite any presets as needed. \cr
#'  
#'  Built-in plugins: \cr 
#'  * leaflet - Leaflet maps with customizable tiles, see \href{https://github.com/gnijuohz/echarts-leaflet#readme}{source}\cr
#'  * world - world map with country boundaries, see \href{https://github.com/apache/echarts/tree/master/test/data/map/js}{source} \cr
#'  * lottie - support for \href{https://lottiefiles.com}{lotties} \cr
#'  * custom - renderers for [ecr.band] and [ecr.ebars] \cr 
#'  Plugins with one-time installation: \cr
#'  * 3D - 3D charts and WebGL acceleration, see \href{https://github.com/ecomfe/echarts-gl}{source} and \href{https://echarts.apache.org/en/option-gl.html#series}{docs} \cr
#'  * liquid - liquid fill, see \href{https://github.com/ecomfe/echarts-liquidfill}{source}  \cr
#'  * gmodular - graph modularity, see \href{https://github.com/ecomfe/echarts-graph-modularity}{source}  \cr
#'  * wordcloud - cloud of words, see \href{https://github.com/ecomfe/echarts-wordcloud}{source} \cr
#'  or install your own third-party plugins.\cr
#'  
#'  Crosstalk:\cr
#'  Parameter \emph{df} should be of type \link[crosstalk]{SharedData}, see \href{https://helgasoft.github.io/echarty/xtalk.html}{more info}.\cr
#'  It should NOT have string row names. Use \link[tibble]{rownames} to remove or convert to column.\cr
#'  Enabling \emph{crosstalk} will generate an additional dataset called _Xtalk_ and bind the first serie to it if \emph{datasetId} not set.\cr
#'  
#' 
#' @return A widget to plot, or to save and expand with more features.
#' 
#' @examples
#'  # basic scatter chart from a data.frame, using presets
#' cars |> ec.init()
#'  
#'  # a timeline with two series and autoPlay
#' p <- iris |> dplyr::group_by(Species) |> ec.init(
#'   legend= list(show=TRUE),
#'   tl.series= list(
#'     encode=list(x=NULL, y=c('Sepal.Width', 'Petal.Length')),
#'     markPoint = list(data=list(list(type='max'), list(type='min')))
#'   )
#' ) # |> ec.upd({...})
#' p$x$opts$timeline <- append(p$x$opts$timeline, list(autoPlay=TRUE))
#' p
#' 
#' @importFrom htmlwidgets createWidget sizingPolicy getDependency JS shinyWidgetOutput shinyRenderWidget
#' @import dplyr
#' 
#' @export
ec.init <- function( df= NULL, preset= TRUE, ctype= 'scatter',
                     tl.series= NULL,
                     width= NULL, height= NULL, ...) {
  
  opts <- list(...)
  elementId <- if (is.null(opts$elementId)) NULL else opts$elementId
  js <- if (is.null(opts$js)) NULL else opts$js
  ask <- if (is.null(opts$ask)) FALSE else opts$ask
  renderer <- if (is.null(opts$renderer)) 'canvas' else tolower(opts$renderer)
  locale <- if (is.null(opts$locale)) 'EN' else toupper(opts$locale)
  useDirtyRect <- if (is.null(opts$useDirtyRect)) FALSE else opts$useDirtyRect
  # remove the above arguments since they are not valid ECharts options
  opts$ask <- opts$js <- opts$renderer <- opts$locale <- opts$useDirtyRect <- opts$elementId <- NULL
  
  doType <- function(how) {
    .ty <- 'category'
    switch(how,
           'Date'= .ty <- 'time',
           'character' = .ty <- 'category',
           'factor' = .ty <- 'category',
           'numeric' = .ty <- 'value',
           'integer' = .ty <- 'value'
    )
    return(.ty)
  }
  xyNamesCS <- function(ser) {
    # no coordinateSystem = pie,funnel,gauge,graph, sunburst/tree/treemap/sankey
    xtem <- 'x'; ytem <- 'y'
    if (is.null(ser$coordinateSystem))
      ser$coordinateSystem <- 'unknown'
    if (ser$type %in% c('line','scatter','bar','pictorialBar','candlestick','boxplot'))
      ser$coordinateSystem <- 'cartesian2d'
    #if (startsWith(ser$coordinateSystem, 'cartesian')) { 
    #  xtem <- 'x'; ytem <- 'y' } #,ztem <- 'z' }
    if (ser$type == 'pie') {
      xtem <- 'value'; ytem <- 'itemName' }
    if (ser$coordinateSystem=='polar') { 
      xtem <- 'radius'; ytem <- 'angle' }
    if (ser$coordinateSystem %in% c('geo','leaflet')) {
      xtem <- 'lng'; ytem <- 'lat' }
    if (ser$type == 'map') {
      xtem <- 'name'; ytem <- 'value' }
    return(c(xtem, ytem, ser$coordinateSystem))
  }
  
  # presets are default settings
  # user can also ignore or replace them
  if (preset) {
    # list(show=TRUE) or list(list()) is to create an empty object{} in JS
    if (!'xAxis' %in% names(opts)) 
      opts$xAxis <- list(show=TRUE)
    if (is.null(opts$xAxis$type) && !is.null(opts$xAxis$data))
      opts$xAxis$type <- doType(class(opts$xAxis$data))
    if (!'yAxis' %in% names(opts)) 
      opts$yAxis <- list(show=TRUE)
    if (is.null(opts$yAxis$type) && !is.null(opts$yAxis$data))
      opts$yAxis$type <- doType(class(opts$yAxis$data))
    if (!'series' %in% names(opts)) {
    	if (!'world' %in% opts$load)   # world will add its own default serie
    	  opts$series <- list(list(type=if (is.null(ctype)) 'scatter' else ctype) )
    } else if (is.null(opts$series[[1]]$type))  # set default to user serie if omitted
      opts$series[[1]]$type <- if (is.null(ctype)) 'scatter' else ctype
    if (('series' %in% names(opts)) && 
        opts$series[[1]]$type %in% c('parallel','map','gauge','pie','funnel','graph', 'sunburst','tree','treemap','sankey')) {
      opts$xAxis <- opts$yAxis <- NULL
    }
  }

  key <- group <- deps <- NULL; isCrosstalk <- FALSE
  if (requireNamespace("crosstalk", quietly= TRUE)) {
    if (crosstalk::is.SharedData(df)) {
      isCrosstalk <- TRUE
      key <- as.list(df$key())
      group <- df$groupName()
      deps <- crosstalk::crosstalkLibs()
      tmp <- df$key()
      if (suppressWarnings( all(is.na(as.numeric(tmp)))))
        stop('ec.init crosstalk: df has non-numeric row names', call. = FALSE)
      df <- df$origData()
      df$XkeyX <- as.numeric(tmp)   # add for Xtalk filtering
    }
  }
  
  # forward widget options using x
  x <- list(
    theme = '',
    draw = TRUE,
    renderer = renderer,
    locale = locale,
    useDirtyRect = useDirtyRect,
    jcode = js,
    opts = opts,
    settings = list(
      crosstalk_key = key,
      crosstalk_group = group
    )
  )
  
  # ------------- data.frame -------------------
  if (!is.null(df)) {
    # if data.frame given, assign to dataset regardless of parameter 'preset'
    if (!'data.frame' %in% class(df)) 
      stop('ec.init: df must be a data.frame', call. = FALSE)
    
    # skip default group settings on map timeline
    if (!is.null(tl.series) && paste0(tl.series$type,'')=='map') ctype <- NULL
    
    # grouping uses transform
    grnm <- NULL
    if (!is.null(ctype) && dplyr::is.grouped_df(df)) {
      grnm <- dplyr::group_vars(df)[[1]]   # name of 1st grouping column 
      x$opts$dataset <- list(list(source = ec.data(df, header=TRUE)))
      grvals <- unname(unlist(dplyr::group_data(df)[grnm]))
      txfm <- list(); k <- 0
      legd = list(data= list())
      for(nm in grvals) { 
        k <- k+1
        txfm <- append(txfm, list(list(transform = list(
          type='filter', config=list(dimension=grnm, '='=nm)))))
        x$opts$series[[k]] <- list(
          type=ctype, datasetIndex=k, name=as.character(nm))
        # if (colnames(df)[1]==grnm)  # grouping by 1st column - breaks prll,map,etc.
        legd$data <- append(legd$data, list(list(name=as.character(nm))))
      }
      if (is.null(x$opts$legend)) x$opts$legend <- legd
      x$opts$dataset <- append(x$opts$dataset, txfm)
    } 
    else 
      x$opts$dataset <- list(list(source = ec.data(df, header=TRUE)))
    
    if (preset) {
      colX <- 1     # by default 1st column is X, 2nd is Y, 3rd is Z
      colY <- 2
      # grouping by any column, group columns do not become X or Y
      if (!is.null(grnm)) {  # find pos of grp column
        pos <- which(colnames(df)==grnm)
        if (!is.null(tl.series) && !is.null(tl.series$groupBy))
          pos <- c(pos, which(colnames(df)==tl.series$groupBy))
        allp <- rep(TRUE, length(colnames(df)))
        allp <- replace(allp, pos, FALSE)
        colX <- which(allp==TRUE)[1]   # first two=TRUE are X,Y
        colY <- which(allp==TRUE)[2]
        if (is.na(colY))
          stop('ec.init: df must have at least 3 columns when grouping by one', call.= FALSE)
      }
      # add encode(if missing) to series when grouping
      if (!(colX==1 && colY==2)) {
        x$opts$series <- lapply(x$opts$series, function(ss) {
          tmp <- xyNamesCS(ss)
          if (tmp[3] != 'unknown') {
            if (is.null(ss$coordinateSystem)) ss$coordinateSystem <- tmp[3]
          }
          if (is.null(ss$encode)) {
              xtem <- tmp[1]; ytem <- tmp[2]
              ss$encode <- list()
              ss$encode[xtem] <- colX   # R count
              ss$encode[ytem] <- colY 
          }
          # else dont overwrite user's encode
          ss
        })
      }
      
      # update xAxis/yAxis type depending on column type , TODO: 'log' type
      # cannot depend on default type of being NEITHER category, NOR value
      # axis source is data, dataset (or series.data = not applicable under df)
      clss <- unname(sapply(df, class))
      xx <- x$opts$xAxis    
      if (!is.null(xx) && is.null(xx$type)) {
        x$opts$xAxis$type <- doType(clss[colX])
      }
      yy <- x$opts$yAxis
      if (!is.null(yy) && is.null(yy$type)) {
        x$opts$yAxis$type <- doType(clss[colY])
      }
      if (!is.null(x$opts$series) && !is.null(x$opts$series[[1]]$type))
        if (x$opts$series[[1]]$type == 'parallel')
          if (is.null(x$opts$parallelAxis))
            x$opts$parallelAxis <- ec.paxis(df)
    }  # end preset
  }
  
  # ------------- convert from R to JS numbering --------------
  renumber <- function(opa) {
    r2jsEncode <- function(ss) {
      if (is.null(ss$encode)) return(ss)
      if (!is.null(ss$encode$x) && is.numeric(ss$encode$x))
        ss$encode$x <- ss$encode$x -1
      if (!is.null(ss$encode$y) && is.numeric(ss$encode$y))
        ss$encode$y <- ss$encode$y -1
      if (!is.null(ss$encode$tooltip) && is.numeric(unlist(ss$encode$tooltip)))
        ss$encode$tooltip <- unlist(ss$encode$tooltip) -1
      ss
    }
    opa$series <- lapply(opa$series, r2jsEncode)
    
    tmp <- opa$visualMap
    if (!is.null(tmp)) {
      doVmap <- function(x) {
        if (is.null(x$type) || x$type=='continuous') {
          if (!is.null(x$dimension))
            x$dimension <- x$dimension -1
        }
        x
      }
      if (all(sapply(tmp, is.list)))
        opa$visualMap <- lapply(tmp, doVmap)
      else
        opa$visualMap <- doVmap(tmp)
    }
    opa
  }
  x$opts <- renumber(x$opts)
  
  # ------------- create widget ----------------
  wt <- htmlwidgets::createWidget(
    name = 'echarty',
    x,
    width = width,
    height = height,
    package = 'echarty',
    elementId = elementId,
    # preRenderHook = .preRender,
    sizingPolicy = htmlwidgets::sizingPolicy(
      defaultWidth = '100%',
      knitr.figure = FALSE,
      browser.fill = TRUE, padding = 0
    ),
    dependencies = deps
  )
  
  tmp <- getOption('echarty.font')
  if (!is.null(tmp))
    wt$x$opts$textStyle <- list(fontFamily= tmp)
  
  tmp <- getOption('echarty.theme')   # default
  if (!is.null(tmp))
    wt <- ec.theme(wt, tmp)
  
  # ------------- plugins loading -----------------------------
  load <- wt$x$opts$load;
  wt$x$opts$load <- NULL
  if (length(load)==1 && grepl(',', load, fixed=TRUE))
      load <- unlist(strsplit(load, ','))
      
  path <- system.file('js', package= 'echarty')
  dep <- NULL
  if ('leaflet' %in% load) {
    if (preset) {
      # customizations for leaflet
      wt$x$opts$xAxis <- NULL
      wt$x$opts$yAxis <- NULL
      urltls <- getOption('echarty.urlTiles')
      if (is.null(urltls))
        urltls <- 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
      if (!'leaflet' %in% names(wt$x$opts)) {
        wt$x$opts$leaflet = list(
          roam = TRUE,
          tiles = list( list(urlTemplate = urltls))
        )
      } 
      else if (is.null(wt$x$opts$leaflet$tiles))
        wt$x$opts$leaflet$tiles <- list( list(urlTemplate = urltls))
      
      if ('series' %in% names(wt$x$opts)) {
        wt$x$opts$series <- lapply(wt$x$opts$series,
          function(ss) {
            if (is.null(ss$coordinateSystem)) 
              ss$coordinateSystem <- 'leaflet'
            ss })
      }
      #wt$x$opts$series[[1]]$coordinateSystem <- 'leaflet'
      
      if (!is.null(df)) {
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
      name = 'renderers', version= '1.0.2', 
      src = c(file = path), script= 'renderers.js')
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  if ('lottie' %in% load) {
    dep <- htmltools::htmlDependency(
      name = 'lottieParser', version = '1.0.0', 
      src = c(file = path), script= 'lottie-parser.js')
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  if ('world' %in% load) {
    #wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts@4.9.0/map/js/world.js', ask)
    dep <- htmltools::htmlDependency(
      name = 'world', version = '1.0.0', 
      src = c(file = path), script= 'world.js')
    wt$dependencies <- append(wt$dependencies, list(dep))
    
    if (preset) {
      wt$x$opts$xAxis <- NULL
      wt$x$opts$yAxis <- NULL
      #if (is.null(opts$series) || is.null(opts$series$type) || opts$series$type!='map')
      #  wt$x$opts$series <- list(list(type='map', geoIndex=0))
      if (!is.null(df)) {  # add map serie if missing
        tmp <- sapply(wt$x$opts$series, function(x) {x$type} )
        if (!'map' %in% tmp)
          wt$x$opts$series <- append(wt$x$opts$series,
                                     list(list(type='map', geoIndex=0)))
      }
      # WARN: map will duplicate if series have map='world' too
      if (!'geo' %in% names(wt$x$opts))
        wt$x$opts$geo = list(map='world', roam=TRUE)
      # if (!is.null(df))  # cancelled: don't know if df first 2 cols are 'lng','lat'
      #   wt$x$opts$geo$center= c(mean(unlist(df[,1])), mean(unlist(df[,2])))
    }
  }
  
  # Plugins implemented as dynamic load on-demand
  if ('3D' %in% load) {
    if (preset) {       # replace 2D presets with 3D
      if (ctype != 'scatterGL') {  # scatterGL is 2D
        wt$x$opts$xAxis <- NULL   
        wt$x$opts$yAxis <- NULL
        nops <- names(wt$x$opts)
        if (!('xAxis3D' %in% nops)) wt$x$opts$xAxis3D <- list(list())
        if (!('yAxis3D' %in% nops)) wt$x$opts$yAxis3D <- list(list())
        if (!('zAxis3D' %in% nops)) wt$x$opts$zAxis3D <- list(list())
        if (!('grid3D'  %in% nops)) wt$x$opts$grid3D <- list(list())
      }
      # valid 3D types: scatter3D, bar3D, surface, etc.
      if ('series' %in% names(wt$x$opts)) {  # if default 2D, change it
        wt$x$opts$series <- lapply(wt$x$opts$series,
          function(s) {s$type= if (s$type=='scatter') 'scatter3D' else s$type; s })
      }
    }
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-gl@2.0.9/dist/echarts-gl.min.js', ask)
  }
  if ('liquid' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-liquidfill@latest/dist/echarts-liquidfill.min.js', ask)
  
  if ('gmodular' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-graph-modularity@latest/dist/echarts-graph-modularity.min.js', ask)
  
  if ('wordcloud' %in% load) 
    wt <- ec.plugjs(wt, 'https://cdn.jsdelivr.net/npm/echarts-wordcloud@2.0.0/dist/echarts-wordcloud.min.js', ask)
  
  # load unknown plugins
  unk <- load[! load %in% c('leaflet','custom','world','lottie','3D','liquid','gmodular','wordcloud')]
  if (length(unk)>0) {
    for(pg in unk)
      wt <- ec.plugjs(wt, pg, ask)
  }
  
  if (isCrosstalk) {  # add transformation filter
    tmp <- list(list( 
      id= 'Xtalk',
      transform = list(type= 'filter', 
                       config= list(dimension= 'XkeyX', reg='^')
                       #"^(50|56|62|68|74|152|158)$")
    )))
    wt$x$opts$dataset <- append(wt$x$opts$dataset, tmp)
    if (!is.null(wt$x$opts$series) && is.null(wt$x$opts$series[[1]]$datasetId))
      wt$x$opts$series[[1]]$datasetId= 'Xtalk'
  }
  
  
  # ------------- timeline  -----------------
  if (is.null(tl.series)) return(wt)
  # timeline is evaluated last
  
  if (is.null(df) || !is.grouped_df(df))
    stop('ec.init: tl.series requires a grouped data.frame df', call. = FALSE)

  if (is.null(tl.series$encode))
    stop('ec.init: encode is required for tl.series', call. = FALSE)

  # add missing defaults
  if (is.null(tl.series$type)) tl.series$type <- 'scatter'
  
  tmp <- xyNamesCS(tl.series)
  xtem <- tmp[1]; ytem <- tmp[2]
  if (is.null(tl.series$coordinateSystem)) tl.series$coordinateSystem <- tmp[3]
  
  if (tl.series$coordinateSystem %in% c('geo','leaflet')) {
      xtem <- 'lng'; ytem <- 'lat'
      center <- c(mean(unlist(df[,tl.series$encode$lng])),
                  mean(unlist(df[,tl.series$encode$lat])))
      if (tl.series$coordinateSystem=='geo')
        wt$x$opts$geo$center <- center
      if (tl.series$coordinateSystem=='leaflet') 
        wt$x$opts$leaflet$center <- center
  } 
  
  if (tl.series$type == 'map') {
    # tl.series type='map' has no encode/dataset API, needs 'data'
    wt$x$opts$dataset <- NULL
    if (is.null(unlist(tl.series$encode[xtem])))
      stop(paste0("tl.series: encode '",xtem,"' is required "), call.=FALSE)
    if (is.null(unlist(tl.series$encode[ytem])))
      stop(paste0("tl.series: encode '",ytem,"' is required "), call.=FALSE)
    
    gvar <- df |> group_vars() |> first() |> as.character()  # convert if factor
    optl <- lapply(df |> group_split(), function(gp) {
      tmp <- gp
      names(tmp)[names(tmp)==tl.series$encode[xtem]] <- 'name'
      names(tmp)[names(tmp)==tl.series$encode[ytem]] <- 'value'
      series <- list(list(type= "map", geoIndex=0,
                          data= ec.data(tmp, 'names')))
      tmp <- list(title= list(text= as.character(unique(gp[gvar]))),  
                  series= series)
      tmp <- renumber(tmp)
    })
  }
  else {
    if (is.null(unlist(tl.series$encode[xtem]))) {
      # append col XcolX 1:max for each group
      df <- df |> group_modify(~ { .x |> mutate(XcolX = 1:nrow(.)) })
      tl.series$encode[xtem] <- 'XcolX'    # instead of relocate(XcolX)
      # replace only source, transforms stay
      wt$x$opts$dataset[[1]] <- list(source=ec.data(df, header=TRUE))
    }
    if (is.null(unlist(tl.series$encode[ytem])))
      stop(paste0("tl.series: encode '",ytem,"' is required for ",
                  tl.series$coordinateSystem), call.=FALSE)
    
    # dataset is already in, now loop group column(s)
    gvar <- df |> group_vars() |> first() |> as.character()  # convert if factor
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
      
      tmp <- list(title= list(text= as.character(unique(gp[gvar]))),  
                  series= unname(series))
      tmp <- renumber(tmp)
    })
  }
  
  #wt$x$opts$xAxis <- list(type='category')  # geo,leaf do not like
  wt$x$opts$series <- NULL
  wt$x$opts$legend <- NULL
  wt$x$opts$options <- optl
  
  if (preset && !is.null(tl.series$groupBy)) {
    if (!(tl.series$groupBy %in% colnames(df)))
      stop('ec.init: tl.series groupBy column missing in df', call.= FALSE)
    gvar <- df |> group_vars() |> first() |> as.character()  # convert if factor
    tgrp <- tl.series$groupBy
    # define additional filter transformations and option series based on preset ones
    dsf <- list()  # new filters
    opts <- list()  # new options
    filterIdx <- 0
    for (i in 1:length(unlist(unname(lapply(unique(df[gvar]), as.list))))) {
      snames <- c()
      for (x2 in unlist(unname(lapply(unique(df[tgrp]), as.list)))) {
        dst <- wt$x$opts$dataset[[i+1]]  # skip source-dataset 1st
        dst$transform$config <- list(and= list(
          dst$transform$config,
          list(dimension= tgrp, `=`= x2)
        ))
        dsf <- append(dsf, list(dst))
        snames <- c(snames, x2)
      }
      ooo <- wt$x$opts$options[[i]]
      sss <- lapply(snames, function(s) {
        filterIdx <<- filterIdx + 1
        tmp <- ooo$series[[1]]
        tmp$name <- s
        tmp$datasetIndex <- filterIdx
        tmp$groupBy <- NULL
        tmp
      })
      opts <- append(opts, list(
        list(title= ooo$title, series= sss)))
    }
    wt$x$opts$dataset <- append(wt$x$opts$dataset[1], dsf)   # keep source-dataset [1]
    wt$x$opts$options <- opts
    wt$x$opts$legend <- list(show=TRUE)  # needed for sub-group
  }
  
  steps <- lapply(optl, function(x) { paste(x$title$text, collapse=' ') })
  wt$x$opts$timeline <- list(data=steps, axisType='category')
  
  return(wt)
}


#' Update option lists
#' 
#' And improve readability by chaining commands after ec.init
#' 
#' @param wt An echarty widget
#' @param ... R commands to update chart option lists
#'
#' @details ec.upd makes changes to chart elements already set by ec.init.\cr
#' It should be always piped after ec.init.
#' Replaces syntax\cr
#'   \verb{   }p <- ec.init(...)\cr
#'   \verb{   }p$x$opts$series <- ...\cr
#' with\cr
#'   \verb{   }ec.init(...) |> \verb{   } # set or preset chart params\cr
#'   \verb{   }ec.upd(\{series <- ...\}) # update params thru R commands
#' @examples
#' Orange |> dplyr::group_by(Tree) |> ec.init() |>
#' ec.upd({ 
#'   series <- lapply(series, function(x) {
#'     x$symbolSize= 10; x$encode= list(x='age', y='circumference'); x } )
#' })
#' @export
ec.upd <- function(wt, ...) {
  if (!'echarty' %in% class(wt))
    stop('ec.upd: expecting wt as echarty widget', call.=FALSE)
  
  wt$x$opts <- within(wt$x$opts, ...)
  wt
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
#' df <- airquality |> mutate(lwr= round(Temp-Wind*2),
#'                            upr= round(Temp+Wind*2),
#'                            x= paste0(Month,'-',Day) ) |>
#'                     relocate(x,Temp)
#' bands <- ecr.band(df, 'lwr', 'upr', type='stack',
#'                   name='stak', areaStyle= list(opacity=0.4))
#' df |> ec.init(load='custom',
#'    legend= list(show= TRUE),
#'    xAxis= list(type='category', boundaryGap=FALSE),
#'    series= list(
#'      list(type='line', color='blue', name='line'),
#'      bands[[1]], bands[[2]]
#'    ),
#'    tooltip= list( trigger= 'axis',
#'      formatter= ec.clmn(
#'         'high <b>%@</b><br>line <b>%@</b><br>low <b>%@</b>',
#'                   3.3, 1.2, 2.2)
#'      )  # 3.3= upper_serie_index +.+ index_of_column_inside
#' )
#' }
#' 
#' @importFrom stats na.omit
#' @export
ecr.band <- function(df=NULL, lower=NULL, upper=NULL, type='polygon', ...) {
  if (is.null(df) || is.null(lower) || is.null(upper)) 
    stop("ecr.band: df, lower and upper are all required", call. = FALSE)
  if (!"data.frame" %in% class(df)) 
    stop("ecr.band: df must be a data.frame", call. = FALSE)
  if (!is.numeric(df[lower][[1]]) || !is.numeric(df[upper][[1]]))
    stop("ecr.band: lower and upper must be numeric", call. = FALSE)
  df <- na.omit(df)
  
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
    slow$data <- ec.data(tmp[,c('x','lo')])
    supr$data <- ec.data(tmp[,c('x','hi','ttip')])
    serios <- list(slow, supr)
  }
  else {   # polygon
    ld <- nrow(df[upper])
    l2 <- unname(unlist(df[upper])[order(ld:1)])
    tmp <- data.frame(x = unname(unlist(c(df[1:ld, fstc], df[ld:1, fstc]))), 
                      y = c(df[lower][[1]],  l2))
    serios <- list(type = "custom", 
                   renderItem = htmlwidgets::JS("riPolygon"), 
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
#' @param wt A widget to add error bars to, see \link[htmlwidgets]{createWidget}
#' @param df NULL(default) or data.frame with four or more columns ordered exactly x,y,low,high,(category),...\cr
#' When NULL, data is taken from wt's dataset where order should be the same x,y,low,high,(category),...
#' @param hwidth Half-width of error bar in pixels, default is 6.
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-custom.type}{custom serie}
#' @return A widget with error bars added if successful, otherwise the input wt
#'
#' @details 
#'  \emph{ecr.ebars} are custom series, so \emph{ec.init(load='custom')} is required. \cr
#'  Grouped series are supported, but \emph{df} is required with group column included. \cr
#'  Will add its own tooltip if none is provided.\cr
#'  Adding a name parameter will show error bars separate in the legend. \cr
#'  Command should be called after all other series are already set.\cr
#'  Simple non-grouped series could be displayed with formatter \emph{riErrBarSimple} instead of \emph{ecr.ebars}. See example below.
#'
#' @examples
#' library(dplyr)
#' df <- iris |> distinct(Sepal.Length, .keep_all= TRUE) |> 
#'   mutate(lo= Sepal.Width-Petal.Length/2, hi= Sepal.Width+Petal.Width) |>
#'   select(Sepal.Length, Sepal.Width, lo, hi, Species)
#'   
#' df |> ec.init(load='custom', legend=list(show=TRUE), xAxis=list(scale=TRUE)) |> 
#'   ecr.ebars(name= 'err')
#' 
#' # ----- grouped -------
#' dfg <- df |> group_by(Species)
#' dfg |> 
#'   ec.init(load= 'custom', legend= list(show=TRUE), xAxis= list(scale=TRUE)) |>
#'   ecr.ebars(dfg)
#'   
#' # ----- riErrBarSimple ------
#' df |> ec.init(load= 'custom',
#'               title= list(text= "riErrBarSimple"),
#'               legend= list(show=TRUE),
#'               xAxis= list(scale= TRUE)
#' ) |> ec.upd({
#'   series <- append(series, list(
#'     list(type= "custom", name= "error",
#'          itemStyle= list(color= 'brown'),
#'          data= ec.data(df |> select(Sepal.Length,lo,hi)),
#'          renderItem= htmlwidgets::JS("riErrBarSimple")) ))
#' })
#' 
#' @export
ecr.ebars <- function(wt, df=NULL, hwidth=6, ...) {
  # alternating bars with custom series doesn't work, first bars then customs
  if (missing(wt)) stop('ecr.ebars: expecting widget', call.=FALSE)
  if (!is.null(df) && !inherits(df, "data.frame")) 
    stop('ecr.ebars: df must be a data.frame', call.=FALSE)
  if (!'renderers' %in% unlist(lapply(wt$dependencies, function(d) d$name)))
    stop("ecr.ebars: use ec.init(load='custom') for ecr.ebars", call.=FALSE)

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
                data= ec.data(df), ...)
    
    if (is.null(c$z)) c$z <- 3
    if (is.null(c$itemStyle$borderWidth)) c$itemStyle$borderWidth <- 1.5
    if (is.null(c$color) && is.null(c$itemStyle$color)) {
      # set own color, or it will blend with main bar
      # impression that c$itemStyle$color is better than c$color
      c$itemStyle$color <- 'black'
    }
    if (is.null(c$tooltip))  # shows up on non-grouped data
      c$tooltip <- list(formatter= ec.clmn('<br>value <b>%@</b> <br>range <b>%@ to %@</b>', 2,3,4))
    c
  }
  
  # build the series
  if (is.null(df)) {
    if (length(wt$x$opts$dataset)==1) {
      if (is.null(name)) name <- wt$x$opts$dataset[[1]]$source[[1]][2]
      cser <- list(oneSerie(name))
    } else 
      stop('ecr.ebars: dataset is grouped, set df parameter', call. = FALSE)
  }
  else {
    if (dplyr::is.grouped_df(df)) {    # groups
      grnm <- dplyr::group_vars(df)[[1]]   # just 1st one matters
      tmp <- df |> dplyr::group_split()
      cser <- lapply(tmp, function(gp) {
        name <- unlist(unique(unname(gp[,grnm])))
        oneSerie(name, gp)
      })
    }
    else
      cser <- list(oneSerie(names(df)[2], df))
  }
  wt$x$opts$series <- append(wt$x$opts$series, cser)
  #if (!"legend" %in% names(wt$x$opts)) wt$x$opts$legend <- list(show=TRUE)
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
#' @seealso [ecs.exec] for example, \link[htmlwidgets]{shinyWidgetOutput} for return value.
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
#' @seealso [ecs.exec] for example, \link[htmlwidgets]{shinyRenderWidget} for return value.
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
#' @seealso [ecs.proxy], [ecs.render], [ecs.output] \cr
#' Read about event handling in [-- Introduction --], code in [ec.examples].
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


#' Data helper
#' 
#' Make data lists from a data.frame
#' 
#' @param df Chart data in data.frame format, required. \cr
#' Except when format is 'dendrogram', then df is a list, result of \link[stats]{hclust} function.
#' @param format A key on how to format the output list \cr \itemize{
#'  \item 'dataset' = list to be used in \href{https://echarts.apache.org/en/option.html#dataset.source}{dataset} (default), or in \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} (without header). \cr
#'  \item 'values' = list for customized \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} \cr
#'  \item 'names' = named lists useful for named data like \href{https://echarts.apache.org/en/option.html#series-sankey.links}{sankey links}.
#'  \item 'boxplot' = build dataset and source lists, see Details
#'  \item 'dendrogram' = build series data for Hierarchical Clustering dendrogram
#'  \item 'treePC' = build series data for sunburst,tree,treemap from parent/children data.frame
#'  \item 'treeTK' = build series data for sunburst,tree,treemap from data.frame like Titanic. Supports column \emph{itemStyle}.
#'  }
#' @param header Boolean to include the column names in dataset, default TRUE.\cr
#'    Set this to FALSE when used in \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data}.
#' @return A list for \emph{dataset.source}, \emph{series.data} or a list of named lists.\cr
#'   For boxplot - a named list, see Details and Examples \cr
#'   For dendrogram & treePC - a tree structure, see format in \href{https://echarts.apache.org/en/option.html#series-tree.data}{tree data}
#' @details `format='boxplot'` requires the first two \emph{df} columns as: \itemize{
#'   \item column for the non-computational categorical axis
#'   \item column with (numeric) data to compute the five boxplot values
#'  }
#'  Grouped \emph{df} is supported. Groups will show in the legend, if enabled.\cr
#'  Returns a `list(dataset, series, axlbl)` to set the chart. \emph{axlbl} is the category axis label list when data grouped.\cr
#'  Make sure there is enough data for computation, like >4 values per boxplot. Otherwise ECharts may exit with a \emph{Object.transform} error.
#' @seealso some live \href{https://rpubs.com/echarty/data-models}{code samples}
#' 
#' @examples
#' library(dplyr)
#' variety <- rep(LETTERS[1:7], each=40)
#' treatment <- rep(c("high","low"), each=20)
#' note <- seq(1:280)+sample(1:150, 280, replace=TRUE)
#' ds <- data.frame(variety, note, treatment) |> group_by(treatment) |> 
#'         ec.data(format='boxplot')
#' ec.init(
#'   dataset= ds$dataset,
#'   series=  ds$series,
#'   yAxis= list(type= 'category',  # categorical yAxis = horizontal boxplots
#'               axisLabel= ds$axlbl),
#'   xAxis= list(show= TRUE),       # categorical xAxis = vertical boxplots
#'   legend= list(show= TRUE)
#' )
#' 
#' ds <- airquality |> mutate(Day=round(Day/10)) |> relocate(Day,Wind) |> ec.data(format='boxplot')
#' ec.init(
#'   dataset= ds$dataset, 
#'   series= ds$series, 
#'   yAxis= list(type= 'category'), 
#'   xAxis= list(show= TRUE),
#'   legend= list(show= TRUE) #, tooltip= list(show=TRUE)
#' )  
#' 
#' hc <- hclust(dist(USArrests), "complete")
#' ec.init(preset= FALSE,
#'         series= list(list(
#'           type= 'tree', orient= 'TB', roam= TRUE, initialTreeDepth= -1,
#'           data= ec.data(hc, format='dendrogram'),
#'           # layout= 'radial', symbolSize= ec.clmn(scale= 0.33),
#'           ## exclude added labels like 'pXX', leaving only the originals
#'           label= list(formatter= htmlwidgets::JS(
#'             "function(n) { out= /p\\d+/.test(n.name) ? '' : n.name; return out;}"))
#'         ))
#' )
#' 
#' @importFrom utils tail
#' @importFrom grDevices boxplot.stats
#' @importFrom data.tree Aggregate
#' @export
ec.data <- function(df, format='dataset', header=FALSE) {
  if (missing(df))
    stop('ec.data: expecting parameter df', call. = FALSE)
  if (format=='dendrogram') { 
    if (!inherits(df, 'hclust'))
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
  if (!'data.frame' %in% class(df))
    stop('ec.data: df has to be data.frame', call. = FALSE)
  
  if (format=='treePC') {
    # for sunburst,tree,treemap
    if (!all(colnames(df) == c('parents', 'children', 'value')) ||
        !all(unlist(unname(lapply(as.list(df[,1:3]), class))) == c('character','character','numeric')) )
      stop('ec.data: tree df columns need to be (parents, children, value) with value as numeric', call. = FALSE)
    
    tryCatch({
      tmp <- data.tree::FromDataFrameNetwork(df)
    },
    error=function(e) { stop(e) })
    json <- data.tree::ToListExplicit(tmp, unname=TRUE)
    return(json$children)
  }
  if (format=='treeTK') {
    # for sunburst,tree,treemap from Titanic-like data
    chNames <- function(lest) {  
      # recursive, build pct and itemStyle
      cldrn <- lest$children
      nm <- names(cldrn)
      tot <- unlist(sapply(cldrn, '[[', 'value'))
      if (!is.null(tot)) {
        tot <- sum(sapply(cldrn, '[[', 'value'))
        lest$value <- tot
      }
      #cat('\nnames:',nm,' ',tot)
      cldrn <- unname(cldrn)
      cnt <- 0
      lest$children <- lapply(cldrn, function(x) {
        cnt <<- cnt+1; x$name <- nm[cnt]
        if (!is.null(tot)) x$pct <- round(x$value / tot * 100, 2)
        if (!is.null(x$itemStyle)) x$itemStyle <- eval(parse(text=paste0('list(',x$itemStyle,')')))
        if (!is.null(x$children)) x <- chNames(x)
        x })
      if (!is.null(lest$children[[1]]$itemStyle))
        lest$itemStyle <- lest$children[[1]]$itemStyle
      lest
    }
    
    tryCatch({
      nod <- data.tree::FromDataFrameTable(df)
    },
    error= function(e) { stop(e) })
    nod$Do(function(x) x$value <- data.tree::Aggregate(x, "value", sum))
    json <- data.tree::ToListExplicit(nod)
    tmp <- chNames(json)
    return(list(tmp))
  }
  
  rownames(df) <- NULL
  n <- seq_along(df[[1]])       # assuming all lists in df have the same length
  tmp <- lapply(n, function(i) lapply(df, "[[", i))  # preserve column types
  
  if (format=='dataset') {
    datset <- lapply(tmp, unname)
    if (header)
      datset <- c(list(colnames(df)), datset)
    
  } 
  else if (format=='values' || isTRUE(format)) {
    datset <- lapply(tmp, function(x) list(value=unlist(unname(x))))
    
  } 
  else if (format=='boxplot') {
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
      # default is horizontal, for vertical switch xAxis/yAxis category type
      
    } else {  # non-grouped
      bdf <- ungroup(df) |> dplyr::group_by(across({colas})) |> group_split()
      dats <- lapply(bdf, function(x) {
        c(unique(pull(x,colas)), round(boxplot.stats( pull(x,colb5) )$stats, 4))
      })
      dataset <- list(source= ec.data( as.data.frame(do.call(rbind, dats)), header=TRUE ))
      series <- list(list(type='boxplot', encode= list(y='V1', x=c('V2','V3','V4','V5','V6'))))
    }
    # category axis labels
    axe <- paste(sort(unique(df[[colas]])), collapse="','")
    axe <- paste0("function(v) { return ['",axe,"'][v]; }")			
    
    return(list(dataset= dataset, series= series, 
                axlbl= list(formatter= htmlwidgets::JS(axe))
    ))
  } 
  else {
    datset <- tmp
  }  # format=='names'
  
  return(datset)
}


#' Data column format
#' 
#' Helper function to display/format data column(s) by index or name
#' 
#' @param col A single column index(number) or column name(quoted string), \cr
#'    or a \link[base]{sprintf} format string. Or 'log' for debugging.
#'    Default is NULL, for charts with single values like tree, pie.\cr
#'    'json' - display tooltip with all available values to choose from\cr 
#'    'log' will write all values in the JS console (F12)
#' @param ... A comma separated column indexes or names, only when \emph{col} is \emph{sprintf}. This allows formatting of multiple columns, as for a tooltip.\cr
#' @param scale A positive number, multiplier for numeric columns. When scale is 0, all numeric values are rounded.
#' @return A JavaScript code string (usually a function) marked as executable, see \link[htmlwidgets]{JS}.
#'  
#' @details This function is useful for attributes like formatter, color, symbolSize.\cr
#' Column indexes are counted in R and start at 1.\cr
#' Omit _col_ or use index -1 for single values in tree/pie charts, \emph{axisLabel.formatter} or \emph{valueFormatter}. See [ec.data] dendrogram example.\cr
#' Use only column indexes when setting \emph{symbolSize}.\cr
#' Column indexes are decimals for combo charts with multiple series, see [ecr.band] example. The whole number part is the serie index, the decimal part is the column index inside.\cr
#' \emph{col} as sprintf has the same placeholder \emph{%@} for both column indexes or column names.\cr
#' \emph{col} as sprintf can contain double quotes, but not single or backquotes.\cr
#' Placeholders:\cr
#' * \emph{%L@} will display a number in locale format, like '12,345.09'.\cr
#' * \emph{%LR@} rounded number in locale format, like '12,345'.\cr
#' * \emph{%R@} rounded number, like '12345'.\cr
#' * \emph{%M@} marker in serie's color.\cr
#' 
#' @examples
#' tmp <- data.frame(Species = as.vector(unique(iris$Species)),
#'                   emoji = c('\U0001F33B','\U0001F335','\U0001F33A'))
#' df <- iris |> dplyr::inner_join(tmp)      # add 6th column emoji
#' df |> dplyr::group_by(Species) |> ec.init() |> ec.upd({
#'   series <- lapply(series,
#'     function(s) append(s,
#'       list(label= list(show= TRUE, formatter= ec.clmn('emoji')))) )
#'   tooltip <- list(formatter=
#'     # ec.clmn with sprintf + multiple column indexes
#'     ec.clmn('%M@ species <b>%@</b><br>s.len <b>%@</b><br>s.wid <b>%@</b>', 5,1,2))
#' })
#' 
#' @export
ec.clmn <- function(col=NULL, ..., scale=1) {
  if (is.null(scale)) scale=1
  if (scale==1) scl <- 'return c;'
  else {
    if (scale==0) scl <- 'return Math.round(c);'
    else scl <- paste0('return (parseFloat(c)*',scale,');') 
  }
  args <- list(...)
  ret <- paste("let c=String(typeof x=='object' ? x.value : x);", scl)
  
  if (is.null(col)) {}   # for pie,sunburst
  else if (col=='log')
    ret <- "console.log(x); return 'logged';"
  else if (col=='json')
    ret <- 'return JSON.stringify(x, null, " ").replace(/{/g,"<br>{").replace(/"value":/g,"<br> value:").replace(/"data":/g,"<br> data:").replace(/"seriesIndex":/g,"<br> seriesIndex:");'
  else if (is.na(suppressWarnings(as.numeric(col)))) {   
    if (length(args)==0) {  # col is solitary name
      args <- c(col); col <- '%@'   # replace
    }
    # col is sprintf
    spf <- "var sprintf= (template, values) => { let j=0;
return template.replace(/%@|%L@|%LR@|%R@|%M@/g, (m) => {
  if (m=='%@') return values[j++];
  if (m=='%L@') return Number(values[j++]).toLocaleString();
  if (m=='%LR@') return Math.round(Number(values[j++])).toLocaleString();
  if (m=='%R@') return Math.round(Number(values[j++]));
  if (m=='%M@') return x.marker;
}); };"
    
    tmp <- suppressWarnings(as.numeric(args) -1)
    if (all(is.na(tmp))) {   
      # multiple non-numeric strings = column names
      t0 <- sapply(args, function(s) toString(paste0("x.data['", s,"']")) )
      t0 <- paste(t0, collapse=',')
      t1 <- paste(args, collapse='`,`')
      # x.data = 1) object 2) array with x.dimensionNames 3) array only when dataset
      ret <- paste0( spf, " if (!x.data) return `no data`; 
let args=[`",t1,"`], vv=[",t0,"]; pos=[];
if (x.dimensionNames && x.dimensionNames.length>0) 
  pos= args.map(z => x.dimensionNames.indexOf(z));
if (x.data.length)
  vv= pos.map(p => x.data[p]);")
    }   # col.names
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
if (vv.length > 0) // multi-series 1.2, 3.1
  vv = ss.map((e,idx) => {
    if (typeof vv[idx] != 'object') return vv[idx];
    f= Math.round(e % 1 *10) -1;
    return vv[idx].value[f];
  }); ")
    }  # col.indexes
    
    if (scale >0) ret <- paste(ret,"vv= vv.map(e => isNaN(e) ? e : e*",scale,");")
    if (scale==0) ret <- paste(ret,"vv= vv.map(e => isNaN(e) ? e : Math.round(e));")
    ret <- paste(ret, "let c = sprintf(`",col,"`, vv); return c; ")
  } # col is string
  else {      # col is solitary numeric
    if (length(args) > 0)
      warning('col is numeric, others are ignored', call.=FALSE)
    col <- as.numeric(col) - 1   # from R to JS counting
    if (col >= 0)
      ret <- paste0('let c = String(x.value!=null ? x.value[',col,'] : x.data!=null ? x.data[',col,'] : x[',col,'] ); ',scl)
  }
  htmlwidgets::JS(paste0('function(x) {', ret, '}'))
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
#' ec.init(preset= FALSE,
#'         parallelAxis= ec.paxis(mtcars, 
#'                                cols= c('gear','cyl','hp','carb'), nameRotate= 45),
#'         series= list(list(type= 'parallel', smooth= TRUE, 
#'                           data= ec.data(mtcars, 'dataset') ))
#' )
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
#' @details Just a few built-in themes are included in folder \code{inst/themes}.\cr
#' Their names are dark, gray, jazz, dark-mushroom and macarons.\cr
#' The entire ECharts theme collection could be found \href{https://github.com/apache/echarts/tree/master/theme}{here} and files copied if needed.\cr
#' To create custom themes or view predefined ones, visit \href{https://echarts.apache.org/en/theme-builder.html}{this site}.
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
#' @param ... Additional arguments to pass to \link[jsonlite]{toJSON}
#' @return A JSON string if \code{json} is \code{TRUE} and
#'  a \code{list} otherwise.
#'
#' @details Must be invoked or chained as last command.\cr
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

  opts <- wt$x$opts
  
  if (!is.null(target)) {
    if (target!='data') stop("ec.inspect: only target='data' supported", call. = FALSE)
    out <- list()
    if (!is.null(opts$dataset))
      out <- sapply(opts$dataset, function(d) {
        if (!is.null(d$source[1])) paste('dataset:',paste(unlist(d$source[1]), collapse=', '))
        else if (!is.null(d$transform[1])) gsub('"', "'", paste(d$transform, collapse=', '))
      })
    
    i <- 0
    out <- append(out, sapply(opts$series, function(s) {
      i <<- i+1 
      str <- paste0('serie',i,' name:',s$name)
      if (!is.null(s$type)) str <- paste0(str, ' type:',s$type)
      if (!is.null(s$dimensions)) str <- paste0(str, ' dim:',s$dimensions)
      if (!is.null(s$datasetIndex)) str <- paste0(str, ' dsi:',s$datasetIndex)
      if (!is.null(s$encode)) str <- paste0(str, ' enc:',paste(s$encode, collapse=', '))
      if (is.null(s$datasetIndex) && !is.null(s$data)) 
        str <- paste(str, gsub('"', "'", paste(s$data[1], collapse=', ')))
      str
    }))
    
    return(unlist(out))
  }
  
  if (!isTRUE(json)) return(opts)
  params <- list(...)
  if ('pretty' %in% names(params)) 
    opts <- jsonlite::toJSON(opts, force=TRUE, auto_unbox=TRUE, 
                             null='null', ...)
  else  # pretty by default
    opts <- jsonlite::toJSON(opts, force=TRUE, auto_unbox=TRUE, 
                             null='null', pretty=TRUE, ...)
  
  return(opts)
}


#' JSON to chart
#' 
#' Convert JSON string to chart
#' 
#' @param txt JSON character string, url, or file, see \link[jsonlite]{fromJSON}
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


#' Utility functions
#' 
#' tabset, table layout, support for GIS shapefiles thru library sf
#'  
#' @param cmd Utility command\cr
#' * \emph{sf.series} returns a list of chart series\cr
#'      required parameter \emph{df} - value from \link[sf]{st_read}\cr
#'      optional \emph{nid} - column name for name-id used in tooltips\cr
#'      optional \emph{verbose} - print shapefile item names in console\cr
#' * \emph{sf.bbox} returns JavaScript code to position a map inside a bounding box from \link[sf]{st_bbox}, for leaflet only.\cr
#' * \emph{sf.unzip} unzips a remote file and returns local file name of the unzipped .shp file\cr
#'      required parameter \emph{url} - URL of remote zipped shapefile\cr
#'      optional \emph{shp} - name of .shp file inside ZIP file if multiple exist. Do not add file extension. \cr
#' * \emph{tabset} returns a \link[htmltools]{tagList} of tabs, each tab may contain a chart.\cr
#' * \emph{layout} returns a container \link[htmltools]{div} in rmarkdown, otherwise \link[htmltools]{browsable}.\cr
#' * \emph{morph} returns a chart with ability to morph into other charts\cr
#' @param js optional JavaScript function, default is NULL.\cr
#' @param ... Optional parameters for the command \cr
#'      for \emph{sf.series} - see \href{https://echarts.apache.org/en/option.html#series-scatter.type}{points}, \href{https://echarts.apache.org/en/option.html#series-lines.type}{polylines}, polygons(itemStyle).\cr
#'      for \emph{tabset} parameters should be in format \emph{name1=chart1, name2=chart2}, see example\cr
#' @details 
#' **cmd = 'sf.series'**\cr
#' \verb{   }Goal is to build \emph{leaflet} or \href{https://echarts.apache.org/en/option.html#geo.map}{geo} map series from shapefiles.\cr
#' \verb{   }Supported types: POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON \cr
#' \verb{   }Coordinate system is \emph{leaflet}(default) or \emph{geo}\cr
#' \verb{   }Limitations:\cr 
#' \verb{     }polygons can have only their name in tooltip,  \cr
#' \verb{     }assumes Geodetic CRS is WGS 84, use \link[sf]{st_transform} with \emph{crs=4326} to convert.\cr
#' **cmd = 'layout'** \cr
#' \verb{   }multiple charts in table-like rows/columns format\cr
#' \verb{   }...= List of charts\cr
#' \verb{   }optional parameters are: \cr
#' \verb{     }title= Title for the set, rows= Number of rows, cols= Number of columns,\cr
#' \verb{     }width= Width of columns (one of xs, md, lg)\cr
#' \verb{   }For 3-4 charts one would use multiple series within a \href{https://echarts.apache.org/en/option.html#grid}{grid}. \cr
#' \verb{   }For greater number of charts _ec.util(cmd='layout')_ comes in handy\cr
#' **cmd = 'tabset'** \cr
#' \verb{   }...= a list tab-name/chart pairs like \emph{n1=chart1, n2=chart2}\cr
#' \verb{   }optional parameters are: \cr
#' \verb{     }width= Width of tabs in pixels, height= Height of tabs in pixels\cr
#' \verb{     }tabStyle= tab style string, see default \emph{tabStyle} variable in the code\cr
#' **cmd = 'morph'** \cr
#' \verb{   }...= a list of charts or chart options\cr
#' \verb{   }optional parameter: \cr
#' \verb{     }js= JS function for switching charts. Default function is on \emph{mouseover}.\cr
#' **cmd = 'fullscreen'** \cr
#' \verb{   }a toolbox feature to toggle fullscreen on/off. Works in a browser, not in RStudio.\cr
#' **cmd = 'rescale'** \cr
#' \verb{   }t = target range c(min,max), numeric vector of two\cr
#' \verb{   }v = vector of numeric values to rescale\cr

#' @examples 
#' if (interactive()) {  # comm.out: Fedora errors about some 'browser'
#'   library(sf)
#'   fname <- system.file("shape/nc.shp", package="sf")
#'   nc <- as.data.frame(st_read(fname))
#'   ec.init(load= c('leaflet', 'custom'),  # load custom for polygons
#'      js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)),
#'      series= ec.util(df= nc, nid= 'NAME', itemStyle= list(opacity= 0.3)),
#'      tooltip= list(formatter= '{a}')
#'   )
#' 
#'   htmltools::browsable(
#'     lapply(iris |> dplyr::group_by(Species) |> dplyr::group_split(), 
#'            function(x) {
#'      x |> ec.init(ctype= 'scatter', title= list(text= unique(x$Species)))
#'            }) |> 
#'     ec.util(cmd= 'tabset')
#'   )
#' 
#'   p1 <- cars |> ec.init(grid= list(top= 20))
#'   p2 <- mtcars |> ec.init()
#'   htmltools::browsable(
#'     ec.util(cmd= 'tabset', cars= p1, mtcars= p2, width= 200, height= 200)
#'   )
#' 
#'   lapply(list('dark','macarons','gray','jazz','dark-mushroom'),
#'                 function(x) cars |> ec.init() |> ec.theme(x) ) |>
#'   ec.util(cmd='layout', cols= 2, title= 'my layout')
#'   
#'   setd <- function(type) {
#'     mtcars |> group_by(cyl) |> ec.init(ctype=type) |> ec.upd({
#'     title <- list(subtext='mouseover points to morph')
#'     xAxis <- list(scale=TRUE)
#'     series <- lapply(series, function(ss) {
#'       ss$groupId <- ss$name
#'       ss$universalTransition <- list(enabled=TRUE)
#'       ss })
#'     })
#'   }
#'   oscatter <- setd('scatter')
#'   obar <- setd('bar')
#'   ec.util(cmd='morph', oscatter, obar)
#' }
#' @importFrom utils unzip
#' @export
ec.util <- function( ..., cmd='sf.series', js=NULL) {
  
  opts <- list(...)
  
  do.opties <- function(names, dflts=NULL) {
    # set default optional parameters
    j <- 0
    for(n in names) {
      j <- j + 1
      val <- NULL
      if (!is.null(dflts)) val <- dflts[[j]]
      tmp <- unname(unlist(opts[n]))
      if (!is.null(tmp)) {
        val <- tmp
        opts[n] <<- NULL
      }
      assign(n, val, envir= parent.frame())
    }
  }
  
  switch( cmd,
    'sf.series'= {
      do.series <- function() {
        polig <- function(geom) {
          for(k in 1:length(geom)) {
            if ('matrix' %in% class(geom[[k]])) {
              gm <- as.data.frame(geom[[k]])
              coords <- list()
              for(j in 1:nrow(gm))
                coords <- append(coords, list(c(gm[j,1], gm[j,2])))
              s1 <- list(
                type= 'custom',
                renderItem= htmlwidgets::JS('riPolygon'),
                name= dname, 
                data= coords, ...
              )
              if (is.null(s1$coordinateSystem)) s1$coordinateSystem <- 'leaflet'
              s1$df <- NULL
              sers <<- append(sers, list(s1))
            } else polig(geom[[k]])  # recursive
          }
        }
        #geometry <- cmd <- 
        L1 <- NULL     # avoid code checking NOTES
        sers <- list()
        dff <- opts$df
        opts$df <<- NULL
        if (is.null(opts$coordinateSystem)) opts$coordinateSystem <<- 'leaflet'
        
        switch( class(dff$geometry)[1],
                'sfc_POINT' =,
                'sfc_MULTIPOINT'= {
                  #            dff <- dff |> rename(value= geometry)
                  #dff$value <- lapply(dff$value, function(x) x[1:2])  # XY, remove Z if any
                  tt <- NULL
                  flds <- colnames(dff)[! colnames(dff) %in% c("geometry")]
                  if (length(flds)>0) {
                    if (length(flds)>10) flds <- flds[1:10]
                    tt <- c(paste(rep('%@', length(flds)), collapse='<br>'), flds)
                  }
                  #            pnts <- ec.data(dff, 'names')
                  pnts <- ec.data(do.call(rbind, dff$geometry) |> as.data.frame())
                  s1 <- list(data= pnts, ...)
                  if (is.null(s1$type)) s1$type <- 'scatter'
                  if (!is.null(tt)) 
                    s1$tooltip= list(formatter= do.call("ec.clmn", as.list(tt)))
                  s1$df <- NULL
                  sers <- list(s1)
                  # opts$type <<- NULL   # cleanup
                  # opts$coordinateSystem <<- NULL
                  # sers <- list(append(list(sers), opts))
                  # sers[[1]]$df <- NULL  #ok
                  #opts$df <<- NULL
                  # sers <- list(
                  #   # list(type= 'scatter', coordinateSystem= cs,
                  #   list(type= ?, data= pnts, ...))
                  # if (!is.null(tt)) 
                  #   sers[[1]]$tooltip= list(formatter= do.call("ec.clmn", as.list(tt)))
                },
                'sfc_POLYGON' =,
                'sfc_MULTIPOLYGON' = {
                  for(i in 1:nrow(dff)) {
                    dname <- i
                    if (!is.null(nid) && (nid %in% colnames(dff))) 
                      dname <- dff[i, nid][[1]]
                    if (verbose) cat(dname,',', sep='')
                    geom <- dff$geometry[[i]]
                    polig(geom)
                  }
                },
                'sfc_LINESTRING' = {
                  tmp <- dff$geometry
                  tmp <- as.data.frame(cbind(do.call(rbind, tmp), 
                                             L1= rep(seq_along(tmp), times= vapply(tmp, nrow, 0L))))
                  for(i in 1:nrow(dff)) {
                    dname <- ifelse(is.null(nid), i, dff[i, nid][[1]])
                    if (verbose) cat(dname,',', sep='')
                    coords <- list()
                    geom <- tmp |> filter(L1==i)
                    for(k in 1:nrow(geom))
                      coords <- append(coords, list(c(geom[k,1], geom[k,2])))
                    s1 <- list(
                      type='lines', polyline= TRUE,
                      name= dname, 
                      tooltip= list(formatter= '{a}'),
                      data= list(coords), ... )
                    
                    if (is.null(s1$coordinateSystem)) s1$coordinateSystem <- 'leaflet'
                    s1$df <- NULL
                    sers <- append(sers, list(s1))
                  }
                },
                'sfc_MULTILINESTRING' = {
                  for(i in 1:nrow(dff)) {
                    dname <- ifelse(is.null(nid), i, dff[i,nid][[1]])
                    if (verbose) cat(dname,',', sep='')
                    corda <- list()
                    geom <- dff$geometry[[i]]
                    for(k in 1:length(geom)) {
                      gm <- as.data.frame(geom[[k]])
                      coords <- list()
                      for(j in 1:nrow(gm))
                        coords <- append(coords, list(c(gm[j,1], gm[j,2])))
                      corda <- append(corda, list(coords))
                    }
                    s1 <- list(
                      type='lines', polyline= TRUE, 
                      name= dname, 
                      tooltip= list(formatter= '{a}'),
                      data= corda, ... )
                    if (is.null(s1$coordinateSystem)) s1$coordinateSystem <- 'leaflet'
                    s1$df <- NULL
                    sers <- append(sers, list(s1))
                  }
                },
                stop(paste('ec.util:',class(dff$geometry)[1],'geometry not supported'), call.= FALSE)
        )
        cnt <- length(sers)
        recs <- sum(unlist(lapply(sers, function(x) {
          len <- length(x$data)
          if (len==1) len <- length(x$data[[1]])  #multiline
          len
        })))
        cat('\n series:',cnt,'coords:',recs,'\n')
        sers
      }
      
      if (is.null(opts$df))
        stop('ec.util: expecting parameter df', call. = FALSE)
      if (is.null(opts$df$geometry))
        stop('ec.util: expecting df$geometry', call. = FALSE)
      nid <- verbose <- NULL   # fix for CRAN check
      do.opties(c('nid','verbose'), list(NULL, FALSE))
      out <- do.series()
    },
    
    'sf.bbox'= {
      if (is.null(opts$bbox))
        stop('ec.util: expecting parameter bbox', call. = FALSE)
      if (is.null(opts$bbox$ymin))
        stop('ec.util: expecting bbox in sf format', call. = FALSE)
      tmp <- opts$bbox
      rng <- paste0('[[',tmp$ymin,',',tmp$xmin,'],[',tmp$ymax,',',tmp$xmax,']]')
      out <- c('','', 
               paste("var map= chart.getModel().getComponent('leaflet').__map;", 
                     "map.fitBounds(",rng,");"))
    },
    'sf.unzip'= {
      if (is.null(opts$url))
        stop('ec.util: expecting url of zipped shapefile', call. = FALSE)
      destfile <- tempfile('shapefile')
      download.file(opts$url, destfile, mode='wb') #, method='curl')
      # get name only, use as folder name to unzip to
      fldr <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(opts$url))
      unzip(destfile, exdir=fldr)  # new folder under getwd()
      # find name
      pat <- ifelse (is.null(opts$shp), '*.shp', paste0(opts$shp,'.shp'))
      tmp <- list.files(path= fldr, pattern= pat)
      if (length(tmp)==0) stop(paste('ec.util:',pat,'file not found in folder',fldr), call. = FALSE)
      out <- paste0(getwd(),'/',fldr,'/',tmp[1])
    },
    
    'tabset'= {
      width <- height <- tabStyle <- NULL   # CRAN check fix
      do.opties(c('width','height','tabStyle'), list(300, 300, 
                                                     "<style>
/*	CSS for the main interaction */
.tabset > input[type='radio'] {
position: absolute;
left: -200vw;
}
.tabset .tab-panel {	display: none; }
.tabset > input:first-child:checked ~ .tab-panels > .tab-panel:first-child,
.tabset > input:nth-child(3):checked ~ .tab-panels > .tab-panel:nth-child(2),
.tabset > input:nth-child(5):checked ~ .tab-panels > .tab-panel:nth-child(3),
.tabset > input:nth-child(7):checked ~ .tab-panels > .tab-panel:nth-child(4),
.tabset > input:nth-child(9):checked ~ .tab-panels > .tab-panel:nth-child(5),
.tabset > input:nth-child(11):checked ~ .tab-panels > .tab-panel:nth-child(6) {
display: block;
}
/*	Styling */
body {
font: 16px/1.5em 'Overpass', 'Open Sans', Helvetica, sans-serif;
color: #333; font-weight: 300;
}
.tabset > label {
position: relative;
display: inline-block;
padding: 15px 15px 25px;
border: 1px solid transparent;
border-bottom: 0;
cursor: pointer;
font-weight: 600;
}
.tabset > label::after {
content: '';
position: absolute;
left: 15px;
bottom: 10px;
width: 22px;
height: 4px;
background: #8d8d8d;
}
.tabset > label:hover,
.tabset > input:focus + label { color: #06c; }
.tabset > label:hover::after,
.tabset > input:focus + label::after,
.tabset > input:checked + label::after { background: #06c;}
.tabset > input:checked + label {
border-color: #ccc;
border-bottom: 1px solid #fff;
margin-bottom: -1px;
}
.tab-panel {
padding: 10px 0;
border-top: 1px solid #ccc;
}
body { padding: 10px; }
.tabset { max-width: 65em; }
</style>" ))
      
      tnames <- names(opts)
      if ((is.null(tnames) || length(tnames)==1) && 
          ('echarty' %in% class(opts[[1]][[1]]))) {  # pipe
        opts <- opts[[1]]
        tnames <- names(opts) <- paste0('chart', 1:length(opts))
      }
      
      tpans <- htmltools::tags$div(class='tab-panels')
      tset <- htmltools::tags$div(class='tabset')
      cnt <- 1
      for(n in tnames) {
        tid <- paste0('tab', cnt)
        tinp <- htmltools::tags$input(type='radio', name='tabso', id=tid, `aria-controls`=n)
        if (cnt==1) tinp <- htmltools::tagAppendAttributes(tinp, checked=1)
        tset <- htmltools::tagAppendChildren(tset, 
                    tinp, htmltools::tags$label(`for`=tid, n))
        cont <- unname(opts[n]) 
        cont[[1]]$width <- width
        cont[[1]]$height <- height
        tpans <- htmltools::tagAppendChild(tpans, 
                    htmltools::tags$section(id=n, class='tab-panel', cont))
        tout <- htmltools::tagAppendChild(tset, tpans)
        cnt <- cnt + 1
      }
      out <- htmltools::tagList(htmltools::HTML(tabStyle), tout)
    },

    'layout'= {
      
      title <- NULL   # CRAN check fix
      do.opties(c('rows','cols','width','title'))
      lplots <- length(opts[[1]])
      if (is.null(rows) & !is.null(cols)) rows <- ceiling(lplots/cols)
      if (!is.null(rows) & is.null(cols)) cols <- ceiling(lplots/rows)
      if (is.null(rows) & is.null(cols)) { rows <- lplots; cols <- 1 }
      w <- "-xs"
      if (!is.null(width)) w <- paste0('-',width)
      if (!isTRUE(getOption("knitr.in.progress"))) w <- ""
      x <- 0
      tg <- htmltools::tagList()
      for (i in 1:rows) {
        r <- htmltools::div(class = "row")
        for (j in 1:cols) {
          x <- x + 1
          cl <- paste0("col", w, "-", 12/cols)
          if (x <= lplots)
            c <- htmltools::div(class = cl, opts[[1]][[x]])
          else 
            c <- htmltools::div(class = cl)
          r <- htmltools::tagAppendChild(r, c)
        }
        tg <- htmltools::tagAppendChild(tg, r)
      }
      if (isTRUE(getOption("knitr.in.progress"))) {
        if (!is.null(title))
          out <- htmltools::div(title, tg)
        else
          out <- tg
      }
      else
        out <- htmltools::browsable(
          htmltools::div(
            class = "container-fluid", 
            htmltools::tags$head(
              htmltools::tags$link(
                rel = "stylesheet", 
                href = "https://cdn.jsdelivr.net/npm/bootstrap@5.0.1/dist/css/bootstrap.min.css"
              )),
            htmltools::div(class= 'row justify-content-center text-center', 
                           htmltools::h3(title) ),
            tg
          ))
    },
    
    'morph'= {

      opts <- lapply(opts, function(oo) {
        if ('echarty' %in% class(oo)) oo$x$opts
        else oo
      })
      # series types should be different for morph options
      defaultHandler <- htmlwidgets::JS("
function(event) {
    opt= this.getOption();
    keep= opt.morph;
    for(i=0; i<keep.length; i++) {
	    if (opt.series[0].type==keep[i].series[0].type) {
	      next= (i+1) % keep.length;
   		optcurr= Object.assign({}, keep[next]);
   		break;
	    }
	 };
	 if (!optcurr) return;
	 optcurr.morph= keep;
	 this.setOption(optcurr, true);
}")
      out <- ec.init(preset=FALSE, js=js)
      out$x$opts <- opts[[1]]
      out$x$opts$morph <- opts
      if (is.null(js))
        out$x$on <- list(list(
          event= 'mouseover', handler= defaultHandler
        ))
      out    
    },
    
    'rescale'= {
      scale <- opts$t
      if (!is.numeric(scale)) scale <- c(0,10)
      if (length(scale)!=2)
        stop("ec.util: rescale 't' vector too long/short")
      if (scale[1]==scale[2])
        stop("ec.util: rescale 't' vector elements equal")
      smin <- min(scale);  smax <- max(scale)-smin; 
      vect <- opts$v
      if (is.null(vect))
        stop("ec.util: rescale 'v' paramater missing")
      if (!is.numeric(vect))
        stop("ec.util: rescale 'v' is not a numeric vector")
    #  out <- drop(scale(vect, center=min(vect)-min(vect)*0.05, scale=diff(range(vect)))) * smax
      out <- drop(scale(vect, center=min(vect), scale=diff(range(vect)))) * smax
      out <- sapply(out, as.vector)
      out <- out + smin
    },
    
    'fullscreen'= {
      out <- list(myecfs= list(show=TRUE,  title= 'fullscreen', 
        icon= 'path://M5 5h5V3H3v7h2zm5 14H5v-5H3v7h7zm11-5h-2v5h-5v2h7zm-2-4h2V3h-7v2h5z',
        onclick= htmlwidgets::JS('function(){ ecfun.fscreen(); }') #debugger; this.fscreen()')
      ))
    },
    
    stop(paste("ec.util: invalid 'cmd' parameter",cmd), call. = FALSE)
  )
  out
  
}


# ----------- Internal --------------


#' Install Javascript plugin from URL source
#' 
#' @param wt A widget to add dependency to, see \link[htmlwidgets]{createWidget}
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
#'   ec.init(preset= FALSE,
#'           geo = list(map= 'china-contour', roam= TRUE),
#'           series = list(list(
#'             type= 'scatter', coordinateSystem= 'geo',
#'             symbolSize= 9, itemStyle= list(color= 'red'),
#'             data= list(list(value= c(113, 40)), list(value= c(118, 39))) ))
#'   ) |> 
#'   ec.plugjs( paste0('https://raw.githubusercontent.com/apache/echarts/',
#'                     'master/test/data/map/js/china-contour.js') )
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

# called by widget init
# .preRender <- function(wt) {
#   wt
# }

if (requireNamespace("shiny", quietly= TRUE)) {
  
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
#' For info on options and prefixes, see [-- Introduction --].



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