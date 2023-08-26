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
#'   Timeline requires a _grouped data.frame_ to build its \href{https://echarts.apache.org/en/option.html#options}{options}.\cr
#'   If grouping is on multiple columns, only the first one is used to determine settings.
#' @param ctype Chart type of series. Default is 'scatter'. Set to NULL to disable series preset.
#' @param preset Build preset xAxis,yAxis,serie for 2D, or grid3D,xAxis3D,yAxis3D,zAxis3D for 3D, default TRUE (enable).
#' @param width,height A valid CSS unit (like \code{'100\%'},
#'   \code{'500px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#' @param tl.series A list to build a timeline or NULL(default). The list defines options \href{https://echarts.apache.org/en/option.html#series}{series} and their attributes. \cr
#'   The only required attribute is \href{https://echarts.apache.org/en/option.html#series-line.encode}{encode}.\cr
#'  _encode_ defines which data columns names to use for the axes: \cr
#'  * set _x_ and _y_ for coordinateSystem _cartesian2d_
#'  * set _lng_ and _lat_ for coordinateSystem _geo_
#'  * set _radius_ and _angle_ for coordinateSystem _polar_
#'  * set _value_ and _itemName_ for _pie_ chart
#'  * set _value_ and _name_ for _map_ chart
#'  
#'   Attribute _coordinateSystem_ is not set by default and depends on chart _type_.\cr
#'   Custom attribute _groupBy_, a _df_ column name, can create series groups inside each timeline step.\cr
#'   A grouped _df_ must be present, with group column providing the \href{https://echarts.apache.org/en/option.html#timeline.data}{timeline data}.
#'   Auto-generated _timeline_ and _options_ will be preset for the chart.\cr
#'   _tl.series_ cannot be used for hierarchical charts like graph,tree,treemap,sankey. Chart options/timeline have to be built directly, see \href{https://helgasoft.github.io/echarty/uc4.html}{example}.
#' @param series.param  Additional parameters for preset series, or NULL(default).\cr
#'   One can also define the complete series with _series= list(list(...),...)_.
#' @param ...  List contains other attributes to pass to the widget. \cr
#'   Custom echarty widget attributes include: \cr
#'  * elementId - Id of the widget, default is NULL(auto-generated)
#'  * load - name(s) of plugin(s) to load. A character vector or comma-delimited string. default NULL.
#'  * ask - prompt user before downloading plugins when _load_ is present, FALSE by default
#'  * js - single string or a vector with JavaScript expressions to evaluate.\cr 
#'      First expression is evaluated before chart initialization. \cr
#'      Second is evaluated with exposed object _opts_. \cr
#'      Third is evaluated with exposed _chart_ object after _opts_ have been set.
#'  * renderer - 'canvas'(default) or 'svg'
#'  * locale - 'EN'(default) or 'ZH'. Use predefined or custom \href{https://gist.github.com/helgasoft/0618c6537c45bfd9e86d3f9e1da497b8}{like so}.
#'  * useDirtyRect - enable dirty rectangle rendering or not, FALSE by default, see \href{https://echarts.apache.org/en/api.html#echarts.init}{here}
#'
#' @details  Command _ec.init_ creates a widget with \link[htmlwidgets]{createWidget}, then adds some ECharts features to it.\cr
#'  When _ec.init_ is chained after a data.frame, a \href{https://echarts.apache.org/en/option.html#dataset}{dataset} is preset. \cr
#'  When data.frame is grouped and _ctype_ is not null, more datasets with legend and series are also preset. \cr
#'  Plugin '3D' presets will not work for 'scatterGL'. Instead, use _preset=FALSE_ and set explicitly _xAxis,yAxis_. \cr
#'  Plugins 'leaflet' and 'world' preset zoom=6 and center to the mean of all coordinates. \cr
#'  Users can delete or overwrite any presets as needed. \cr
#'  Numerical indexes for series,visualMap,etc. are R-counted (1,2...)\cr
#'  
#'  **Built-in plugins**: \cr 
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
#'  **Crosstalk**:\cr
#'  Parameter _df_ should be of type \link[crosstalk]{SharedData}, see \href{https://helgasoft.github.io/echarty/gallery.html#crosstalk-2d}{more info}.\cr
#'  Optional parameter _xtKey_: unique ID column name of data frame _df_. Must be same as _key_ parameter used in _SharedData$new()_. If missing, a new column _XkeyX_ will be appended to df.\cr
#'  Enabling _crosstalk_ will also generate an additional dataset called _Xtalk_ and bind the **first series** to it.\cr
#'  
#' 
#' @return A widget to plot, or to save and expand with more features.
#' 
#' @examples
#'  # basic scatter chart from a data.frame, using presets
#' cars |> ec.init()
#'  
#'  # grouping, tooltips, formatting
#' iris |> dplyr::group_by(Species) |> 
#' ec.init(        # init with presets
#'   tooltip= list(show= TRUE),
#'   series.param= list( 
#'     symbolSize= ec.clmn(4, scale=7),
#'     tooltip= list(formatter= ec.clmn('Petal.Width: %@', 4))
#'   )
#' )
#' 
#' @importFrom htmlwidgets createWidget sizingPolicy getDependency JS shinyWidgetOutput shinyRenderWidget
#' @import dplyr
#' 
#' @export
ec.init <- function( df= NULL, preset= TRUE, ctype= 'scatter', ...,
                     series.param= NULL, tl.series= NULL, 
                     width= NULL, height= NULL) {
  
  opts <- list(...)  
  # treacherous R does "partial matching of argument names" (bug to me): 
  #   if 'series.param' is before ... and 'series' is added, the latter is ignored!
  elementId <- if (is.null(opts$elementId)) NULL else opts$elementId
  js <- if (is.null(opts$js)) NULL else opts$js
  ask <- if (is.null(opts$ask)) FALSE else opts$ask
  renderer <- if (is.null(opts$renderer)) 'canvas' else tolower(opts$renderer)
  locale <- if (is.null(opts$locale)) 'EN' else toupper(opts$locale)
  useDirtyRect <- if (is.null(opts$useDirtyRect)) FALSE else opts$useDirtyRect
  xtKey <- if (is.null(opts$xtKey)) 'XkeyX' else opts$xtKey
  # remove the above attributes since they are not valid ECharts options
  opts$ask <- opts$js <- opts$renderer <- opts$locale <- NULL
  opts$useDirtyRect <- opts$elementId <- opts$xtKey <- NULL
  noAxis <- c('radar','parallel','map','gauge','pie','funnel','polar', #'graph', 
              'sunburst','tree','treemap','sankey')
  
  doType <- function(idx, axx) {
    # get one axis type & name
    # idx= column index, axx= axis
    .ty <- .nm <- NULL
    if (!is.null(names(lengths(axx)))) {  # otherwise multiple axes exist
      if (!is.null(axx) && !is.null(attributes(axx))) {
        .ty <- axx$type
        .nm <- axx$name
        if (is.null(.ty)) {
          if (!is.null(axx$data)) 
            .ty <- 'category'  # always when data
          else {
            clss <- unname(sapply(df, class))
            if (length(clss)>0) {
              how <- unlist(clss[idx])   # type from dataset.source
              if (any(c('POSIXt', 'Date') %in% how))
               .ty <- 'time'
              else if (any(c('numeric', 'integer') %in% how))
               .ty <- 'value'
              else 
               .ty <- 'category'
              #  'character' = 'category',
              #  'factor' = 'category',
              #  'list' = 'category',
            }          }
        }
        if (is.null(axx$name)) {
          .nm <- colnames(df)[idx]
        }
      }
    }
    if (!is.null(.ty)) axx$type <- .ty
    if (!is.null(.nm)) axx$name <- .nm
    return(axx)
  }
  axNamesEnc <- function(series) {
    # set axes names from encode
    tmp <- list(x=NULL, y=NULL)
    lapply(series, \(ss) {
      if (!is.null(ss$encode)) {
        if (is.character(ss$encode$x)) tmp$x <<- c(tmp$x, ss$encode$x[1])
        if (is.character(ss$encode$y)) tmp$y <<- c(tmp$y, ss$encode$y[1])
      }
    })
    if (!is.null(tmp$x)) x$opts$xAxis$name <<- trimws(paste(unique(tmp$x), collapse=','))
    if (!is.null(tmp$y)) x$opts$yAxis$name <<- trimws(paste(unique(tmp$y), collapse=','))
  }
  xyNamesCS <- function(ser) {
    # no coordinateSystem = pie,funnel,gauge, sunburst/tree/treemap/sankey (graph)
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
    return(list(x=xtem, y=ytem, c=ser$coordinateSystem))
  }
  
  # presets are default settings, user can ignore or replace them
  if (preset) {
    # list(show=TRUE) or list(list()) is to create an empty object{} in JS
    if (!'xAxis' %in% names(opts)) 
      opts$xAxis <- list(show=TRUE)
    if (!'yAxis' %in% names(opts)) 
      opts$yAxis <- list(show=TRUE)
    if (!any(c('series','options') %in% names(opts))) {
    	if (!'world' %in% opts$load)   # world will add its own default serie
    	  opts$series <- list(list(type=if (is.null(ctype)) 'scatter' else ctype) )
    }
    if ('series' %in% names(opts)) {
      if (is.null(opts$series[[1]]$type))  # set default to user serie if omitted
        opts$series[[1]]$type <- if (is.null(ctype)) 'scatter' else ctype
      if (opts$series[[1]]$type %in% noAxis)
        opts$xAxis <- opts$yAxis <- NULL
    }
    else if (!is.null(ctype) && ctype %in% noAxis)
        opts$xAxis <- opts$yAxis <- NULL
    if ('polar' %in% names(opts)) {
      opts$xAxis <- opts$yAxis <- NULL
      if (is.null(opts$polar$radius)) opts$polar$radius = 111
      if (is.null(opts$radiusAxis)) opts$radiusAxis= list(type= 'category')
      if (is.null(opts$angleAxis)) opts$angleAxis= list(doit=T)
      if (!is.null(series.param)) 
        series.param = .merlis(series.param, list(coordinateSystem= "polar"))
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
      df <- df$origData()
      if (xtKey=='XkeyX')
	      df$XkeyX <- tmp   # add new column for Xtalk filtering, if needed
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
  colX <- 1     # by default 1st column is X, 2nd is Y, 3rd is Z
  colY <- 2
  
  # ------------- data.frame -------------------
  if (!is.null(df)) {
    # if data.frame given, assign to dataset regardless of parameter 'preset'
    stopifnot('ec.init: df must be a data.frame'= inherits(df, 'data.frame'))
    
    # skip default group settings on map timeline
    if (!is.null(tl.series) && paste0(tl.series$type,'')=='map') ctype <- NULL
    
    # grouping uses transform
    grnm <- NULL
    if (!is.null(ctype) && dplyr::is.grouped_df(df)) {
      grnm <- dplyr::group_vars(df)[[1]]   # name of 1st grouping column 
      x$opts$dataset <- list(list(source = ec.data(df, header=TRUE)))
      grvals <- unname(unlist(dplyr::group_data(df)[grnm]))
      txfm <- sers <- list()
      legd = list(data= list())
      k <- 0
      for(nm in grvals) { 
        k <- k+1
        txfm <- append(txfm, list(list(transform = list(
          type='filter', config=list(dimension=grnm, '='=nm)))))
        sers <- append(sers, list(list(
          type= ctype, datasetIndex= k, name= as.character(nm))))
        # if (colnames(df)[1]==grnm)  # grouping by 1st column - breaks prll,map,etc.
        legd$data <- append(legd$data, list(list(name=as.character(nm))))
      }
      if (preset) {
        if (is.null(tl.series) && is.null(x$opts$options))
          x$opts$series <- sers
        #if (is.null(x$opts$legend))  overwrite simple legend=(show=T)
        x$opts$legend <- legd
      }
      x$opts$dataset <- append(x$opts$dataset, txfm)
    } 
    else 
      x$opts$dataset <- list(list(source = ec.data(df, header=TRUE)))
    
    if (preset) {
      # grouping by any column, group columns do not become X or Y axis
      if (!is.null(grnm)) {  # find pos of grp column
        pos <- which(colnames(df)==grnm)
        if (!is.null(tl.series) && !is.null(tl.series$groupBy))
          pos <- c(pos, which(colnames(df)==tl.series$groupBy))
        allp <- rep(TRUE, length(colnames(df)))
        allp <- replace(allp, pos, FALSE)
        colX <- which(allp==TRUE)[1]   # first two==TRUE are X,Y
        colY <- which(allp==TRUE)[2]
        stopifnot('ec.init: df must have at least 3 columns when grouping by one'= !is.na(colY))
      }
      # add encode if missing to series when grouping
      if (!(colX==1 && colY==2)) {
        x$opts$series <- lapply(x$opts$series, function(ss) {
          tmp <- xyNamesCS(ss)
          if (tmp$c != 'unknown') {
            if (is.null(ss$coordinateSystem)) ss$coordinateSystem <- tmp$c
          }
          if (is.null(ss$encode)) {
              xtem <- tmp$x; ytem <- tmp$y
              ss$encode <- list()
              ss$encode[xtem] <- colX   # R count
              ss$encode[ytem] <- colY 
          }
          # else don't overwrite user's encode
          ss
        })
      }
    }  # end preset
  }
  
  if (preset) {
      
    # set X,Y axes type & name
    x$opts$xAxis <- doType(colX, x$opts$xAxis)
    x$opts$yAxis <- doType(colY, x$opts$yAxis)
    axNamesEnc(x$opts$series)
    axNamesEnc(list(tl.series))
      
    if (!is.null(x$opts$series)) {
      if (!is.null(x$opts$series[[1]]$type)) {
        if (x$opts$series[[1]]$type == 'parallel') {
          if (is.null(x$opts$parallelAxis))
            x$opts$parallelAxis <- ec.paxis(df)
        }
      }
    }
  }
  #else { TODO: set axis type from series.data }

  # visualMap assist (min/max not needed with series[[x]].data)
  vm <- x$opts$visualMap
  if (!is.null(vm) && (is.null(vm$min) || is.null(vm$max))) {
    if (is.null(vm$type) || (vm$type == 'continuous')) {
      xx <- length(colnames(df))   # last column by default in ECharts
      if (!is.null(vm$dimension)) xx <- vm$dimension
      x$opts$visualMap$min <- min(df[,xx])
      x$opts$visualMap$max <- max(df[,xx])
    }
  }
  x$opts <- .renumber(x$opts)
  
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
  
  if ('world' %in% load) {
    dep <- htmltools::htmlDependency(
      name = 'world', version = '1.0.0', 
      src = c(file = path), script= 'world.js')
    wt$dependencies <- append(wt$dependencies, list(dep))
    
    if (preset) {
      wt$x$opts$xAxis <- NULL
      wt$x$opts$yAxis <- NULL
      if (!is.null(df)) {  # add map serie if missing
        tmp <- sapply(wt$x$opts$series, \(x) {x$type} )
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
  if (preset) {
    if (!is.null(series.param)) {
      # merge custom params to auto-generated series
      # this is a shortcut to avoid using ec.upd later
      wt$x$opts$series <- .merlis(wt$x$opts$series, series.param)
    }
  }
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
      if (!'tiles' %in% names(wt$x$opts$leaflet))
        wt$x$opts$leaflet$tiles <- list( list(urlTemplate = urltls))
      if (!'zoom' %in% names(wt$x$opts$leaflet))
        wt$x$opts$leaflet$zoom <- 6
      if (!'center' %in% names(wt$x$opts$leaflet)) {
        if (!is.null(df)) 
          wt$x$opts$leaflet$center= c(mean(unlist(df[,1])), mean(unlist(df[,2])))
      }
      
      if ('series' %in% names(wt$x$opts)) {
        wt$x$opts$series <- lapply(wt$x$opts$series,
          function(ss) {
            if (is.null(ss$coordinateSystem)) 
              ss$coordinateSystem <- 'leaflet'
            ss })
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
  
  # Plugins implemented as dynamic load on-demand
  cdn <- 'https://cdn.jsdelivr.net/npm/'
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
          \(s) { s$type= if (s$type=='scatter') 'scatter3D' else s$type; s })
      }
    }
    wt <- ec.plugjs(wt, 
      paste0(cdn,'echarts-gl@2.0.9/dist/echarts-gl.min.js'), ask)
  }
  if ('liquid' %in% load) 
    wt <- ec.plugjs(wt, 
      paste0(cdn,'echarts-liquidfill@latest/dist/echarts-liquidfill.min.js'), ask)
  
  if ('gmodular' %in% load) 
    wt <- ec.plugjs(wt, 
      paste0(cdn,'echarts-graph-modularity@latest/dist/echarts-graph-modularity.min.js'), ask)
  
  if ('wordcloud' %in% load) 
    wt <- ec.plugjs(wt, 
      paste0(cdn,'echarts-wordcloud@2.0.0/dist/echarts-wordcloud.min.js'), ask)
  
  # load unknown plugins
  unk <- load[! load %in% c('leaflet','custom','world','lottie',
                            '3D','liquid','gmodular','wordcloud')]
  if (length(unk)>0) {
    for(pg in unk)
      wt <- ec.plugjs(wt, pg, ask)
  }
  
  if (isCrosstalk) {  # add transformation filter
    tmp <- list(list( 
      id= 'Xtalk',
      transform = list(type= 'filter', 
                       config= list(dimension= xtKey, reg='^')
                       #"^(50|56|62|68|74|152|158)$")
    )))
    wt$x$opts$dataset <- append(wt$x$opts$dataset, tmp)
    if (!is.null(wt$x$opts$series))
      wt$x$opts$series[[1]]$datasetId= 'Xtalk'
  }
  
  
  # ------------- timeline  -----------------
  if (is.null(tl.series)) return(wt)
  # timeline is evaluated last
  
  if (is.null(df) || !is.grouped_df(df))
    stopifnot('ec.init: tl.series requires a grouped data.frame df'= 1==1)

  stopifnot('ec.init: encode is required for tl.series'= !is.null(tl.series$encode))

  # add missing defaults
  if (is.null(tl.series$type)) tl.series$type <- 'scatter'
  
  tmp <- xyNamesCS(tl.series)
  xtem <- tmp$x; ytem <- tmp$y
  if (is.null(tl.series$coordinateSystem)) tl.series$coordinateSystem <- tmp$c
  
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
    stopifnot("tl.series: encode 'name' is required"= !is.null(unlist(tl.series$encode[xtem])))
    stopifnot("tl.series: encode 'value' is required"= !is.null(unlist(tl.series$encode[ytem])))
    
    gvar <- df |> group_vars() |> first() |> as.character()  # convert if factor
    optl <- lapply(df |> group_split(), \(gp) {
      tmp <- gp
      names(tmp)[names(tmp)==tl.series$encode[xtem]] <- 'name'
      names(tmp)[names(tmp)==tl.series$encode[ytem]] <- 'value'
      series <- list(list(type= "map", geoIndex=0,
                          data= ec.data(tmp, 'names')))
      tmp <- list(title= list(text= as.character(unique(gp[gvar]))),  
                  series= series)
      tmp <- .renumber(tmp)
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
    # paste0("tl.series: encode '",ytem,"' is required for ",tl.series$coordinateSystem)
    stopifnot("tl.series: bad second parameter name for encode"= !is.null(unlist(tl.series$encode[ytem])))
    
    # dataset is already in, now loop group column(s)
    gvar <- df |> group_vars() |> first() |> as.character()  # convert if factor
    di <- 0
    optl <- lapply(df |> group_split(), \(gp) {
      di <<- di+1
      # nicer looking lines with sorted X 
      #if (!is.null(xcol)) gp <- gp |> arrange(across(all_of(xcol)))
      
      # multiple series for each Y, like y=c('col1', 'col3')
      series <- lapply(unname(unlist(tl.series$encode[ytem])), 
        \(sname) {
          append(list(datasetIndex= di), tl.series)  # , name= sname
      })
      # series <- lapply(series, \(s) {
      #   s$encode[ytem] <- s$name   # replace multiple col.names with one
      #   s
      # })
      
      #tmp <- list(title= list(text= as.character(unique(gp[gvar]))),  
      tmp <- list(title= list(text= unique(unlist(lapply(gp[gvar], as.character)))),
                  series= unname(series))
      tmp <- .renumber(tmp)
    })
  }
  
  #wt$x$opts$xAxis <- list(type='category')  # geo,leaf do not like
  wt$x$opts$series <- NULL
  wt$x$opts$legend <- NULL
  wt$x$opts$options <- .merlis(optl, wt$x$opts$options)
  
  if (preset && !is.null(tl.series$groupBy)) {
    stopifnot('ec.init: tl.series groupBy column missing in df'= tl.series$groupBy %in% colnames(df))
    gvar <- df |> group_vars() |> first() |> as.character()  # convert if factor
    tgrp <- tl.series$groupBy
    # define additional filter transformations and option series based on preset ones
    dsf <- list()  # new filters
    opts <- list() 
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
      sss <- lapply(snames, \(s) {
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
    wt$x$opts$legend <- .merlis(wt$x$opts$legend, list(show=TRUE))  # needed for sub-group
  }
  
  steps <- lapply(optl, \(x) { paste(x$title$text, collapse=' ') })
  wt$x$opts$timeline <- .merlis(wt$x$opts$timeline, list(data=steps, axisType='category'))
  
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
#' It should be always piped after ec.init.\cr
#' Numerical indexes for series,visualMap,etc. are JS-counted (0,1...)\cr
#' Replaces syntax\cr
#'   \verb{   }p <- ec.init(...)\cr
#'   \verb{   }p$x$opts$series <- ...\cr
#' with\cr
#'   \verb{   }ec.init(...) |> \verb{   } # set or preset chart params\cr
#'   \verb{   }ec.upd(\{series <- ...\}) # update params thru R commands
#' @examples
#' Orange |> dplyr::group_by(Tree) |> ec.init() |>
#' ec.upd({ series <- lapply(series, function(x) {
#'     x$symbolSize= 15;
#'     x$markPoint= list(data= list(list(type='max')))
#'     x })
#' })
#' @export
ec.upd <- function(wt, ...) {
  stopifnot('ec.upd: expecting wt as echarty widget'= inherits(wt, 'echarty'))
  
  wt$x$opts <- within(wt$x$opts, ...)
  wt
}


#' Area band
#' 
#' A 'custom' serie with lower and upper boundaries
#' 
#' @param df A data.frame with lower and upper numerical columns and first column with X coordinates.
#' @param lower The column name of band's lower boundary (string).
#' @param upper The column name of band's upper boundary (string).
#' @param type Type of rendering
#' \itemize{
#'  \item 'stack' - by two \href{https://echarts.apache.org/en/option.html#series-line.stack}{stacked lines} (default)
#'  \item 'polygon' - by drawing a polygon as polyline from upper/lower points. 
#' }
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-line.type}{serie}
#' @return A list of one serie when type='polygon', or two series when type='stack'
#'
#' @details
#' \itemize{
#'  \item type='stack': two _stacked_ lines are drawn, one with customizable areaStyle. The upper boundary coordinates are values added on top of the lower boundary coordinates.\cr
#'      _xAxis_ is required to be of type 'category'.
#'  \item type='polygon': coordinates of the two boundaries are chained into a polygon and displayed as one. Tooltips do not show upper band values.
#' }
#' Optional parameter _name_, if given, will show up in legend. Legend will merge all series with the same name into one item.
#' 
#' @examples 
#' df <- airquality |> dplyr::mutate(
#'     lwr= round(Temp-Wind*2),
#'     upr= round(Temp+Wind*2),
#'       x= paste0(Month,'-',Day) ) |>
#'   dplyr::relocate(x, Temp)
#' bands <- ecr.band(df, 'lwr', 'upr', # type='stack',
#'                   name= 'stak', areaStyle= list(opacity=0.4))
#' df |> ec.init( load= 'custom',
#'    legend= list(show= TRUE),
#'    dataZoom= list(type= 'slider'),
#'    toolbox= list(feature= list(dataZoom= list(show= TRUE))),
#'    xAxis= list(type= 'category', boundaryGap= FALSE),
#'    series= list(
#'      list(type='line', color='blue', name='line'),
#'      bands[[1]], bands[[2]]
#'    ),
#'    tooltip= list( trigger= 'axis',
#'      formatter= ec.clmn(
#'         'high <b>%@</b><br>line <b>%@</b><br>low <b>%@</b>',
#'                   3.3, 1.2, 2.2)
#'    )  # 3.3= upper_serie_index +.+ index_of_column_inside
#' )
#' 
#' @importFrom stats na.omit
#' @export
ecr.band <- function(df=NULL, lower=NULL, upper=NULL, type='stack', ...) {
  if (is.null(df) || is.null(lower) || is.null(upper)) 
    stop("ecr.band: df,lower,upper are required args", call. = FALSE)
  stopifnot("ecr.band: df must be a data.frame"= inherits(df, 'data.frame'))
  fstc <- colnames(df)[1]   # first column name
  stopifnot("ecr.band: df first column is lower or upper"= !fstc %in% c('lower','upper'))
  if (!is.numeric(df[lower][[1]]) || !is.numeric(df[upper][[1]]))
    stop("ecr.band: lower and upper must be numeric", call. = FALSE)
  df <- na.omit(df)
  
  if (type=='stack') {
    colr <- paste("new echarts.graphic.LinearGradient(0, 0, 0, 1, [", 
                  "{offset: 0, color: 'rgba(255, 0, 135)'},", 
                  "{offset: 1, color: 'rgba(135, 0, 157)'}]);")
    defStyle <- list(opacity = 0.8, color = htmlwidgets::JS(colr))
    
    slow <- list(type='line', ...)
    if (is.null(slow$name)) slow$name <- 'band'
    if (is.null(slow$stack))
      slow$stack <- ifelse(is.null(slow$name), 'band', slow$name)
    if (is.null(slow$showSymbol)) slow$showSymbol <- FALSE
    if (is.null(slow$lineStyle)) slow$lineStyle <- list(width= 0)
    supr <- slow
    if (!is.null(slow$areaStyle)) slow$areaStyle <- NULL
    if (is.null(supr$areaStyle))  supr$areaStyle <- defStyle
    # save upper data for tooltip, 'hi' values are just differences
    tmp <- data.frame(x = df[fstc][[1]], lo=df[lower][[1]], 
                      hi = df[upper][[1]] - df[lower][[1]], 
                      ttip = df[upper][[1]] )
    slow$data <- ec.data(tmp[,c('x','lo')])
    supr$data <- ec.data(tmp[,c('x','hi','ttip')])
    serios <- list(slow, supr)
  }
  else {   # polygon
    ld <- nrow(df[upper])
    nc <- c('c1','c2')
    t1 <- df[1:ld, c(1, which(colnames(df)==lower))]; colnames(t1)<- nc
    t2 <- df[ld:1, c(1, which(colnames(df)==upper))]; colnames(t2)<- nc
    tmp <- rbind(t2, t1)
    serios <- list(type = "custom", 
                   renderItem = htmlwidgets::JS("riPolygon"), 
                   data = ec.data(tmp), ...) 
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
#' @param df NULL(default) or data.frame with four or more columns 
#' ordered exactly x,y,low,high,(category),etc. When NULL, data is taken from 
#' the wt dataset where order should be the same.
#' @param hwidth Half-width of error bar in pixels, default is 6.
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-custom.type}{custom serie}
#' @return A widget with error bars added if successful, otherwise the input wt
#'
#' @details
#' _ecr.ebars_ are custom series, so _ec.init(load='custom')_ is required. \cr
#' Command should be called after _ec.init_ where all other series are set.\cr
#' Have their own default tooltip format showing _value & high/low_.\cr
#' Non-grouped series:\cr
#' \verb{     }Adding a name parameter will show error bars separate in the legend.\cr
#' \verb{     }Could be displayed with formatter _riErrBarSimple_ instead of _ecr.ebars_. See example below.\cr
#' Grouped series are supported:\cr
#' \verb{     }param _df_ is required with group column included. \cr
#' \verb{     }chart's _xAxis_ needs to be type 'category'\cr
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
#'               title= list(text= 'riErrBarSimple'),
#'               legend= list(show= TRUE),
#'               xAxis= list(scale= TRUE)
#' ) |> ec.upd({
#'   series <- append(series, list(
#'     list(type= 'custom', name= 'error',
#'          itemStyle= list(color= 'brown'),
#'          data= ec.data(df |> select(Sepal.Length,lo,hi)),
#'          renderItem= htmlwidgets::JS('riErrBarSimple')) ))
#' })
#' 
#' @export
ecr.ebars <- function(wt, df=NULL, hwidth=6, ...) {
  # alternating bars with custom series doesn't work, first bars then customs
  stopifnot('ecr.ebars: expecting widget'= !missing(wt))
  stopifnot('ecr.ebars: expecting echarty widget'= inherits(wt, "echarty"))
  if (!is.null(df) && !inherits(df, "data.frame")) 
    stop('ecr.ebars: df must be a data.frame', call.=FALSE)
#  if (!'renderers' %in% unlist(lapply(wt$dependencies, \(d) d$name)))
  if (!'renderers' %in% unlist(sapply(wt$dependencies, `[`, "name")))
    stop("use ec.init(load='custom') before ecr.ebars", call.=FALSE)

  ser <- wt$x$opts$series  # all series
  stopifnot('ecr.ebars: series are missing'= !is.null(ser))
  args <- list(...)
  
  cntr <- function(x, typ) { grep(typ, x) }
  name <- args$name; 
  tmp <- NULL   # count number of similar (grouped) series
  if (!is.null(name)) 
    tmp <- unlist(lapply(ser, function(x) { 
      if (length(grep(name,x))>0) x$type else NULL }))[1]
  if (!is.null(tmp))    # attached by name, count same-type series
    info <- length(unlist(lapply(ser, \(x) grep(tmp, x))))
  else {    # no name or not found - choose first of type bar/line/scatter, count how many
    info <- length(unlist(lapply(ser, cntr, typ='bar')))
    if (info==0) info <- length(unlist(lapply(ser, cntr, typ='scatter')))
    if (info==0) info <- length(unlist(lapply(ser, cntr, typ='line')))
  }
  
  if (info==0) return(wt)    # no bars/lines/scatter, nothing to attach to
  info <- "riErrorBar"
  if (is.null(wt$elementId)) wt$elementId <- 'id---99';

  oneSerie <- function(name, df=NULL) {
    c <- list(type='custom', name=name, 
    			 renderItem= htmlwidgets::JS(info), ...)
	 if (!is.null(df))
      c$data= ec.data(df)
    
    if (is.null(c$z)) c$z <- 3
    if (is.null(c$itemStyle$borderWidth)) c$itemStyle$borderWidth <- 1.5
    if (is.null(c$color) && is.null(c$itemStyle$color)) {
      # set own color, or it will blend with main bar
      # impression that c$itemStyle$color is better than c$color
      c$itemStyle$color <- 'black'
    }
    c$itemStyle$borderDashOffset <- hwidth  # => lineDashOffset
    #c$itemStyle$borderCap <- wt$elementId   # => lineCap
    
    if (is.null(c$tooltip))  # shows up on non-grouped data
      c$tooltip <- list(formatter= ec.clmn(
        '<br>value <b>%@</b> <br>range <b>%@ to %@</b>', 2,3,4))
    c
  }
  
  # build the series
  if (is.null(df)) {
    stopifnot('ecr.ebars: no dataset'= !is.null(wt$x$opts$dataset))
    stopifnot('ecr.ebars: dataset is grouped, set df parameter'= length(wt$x$opts$dataset)==1)
    if (is.null(name)) name <- wt$x$opts$dataset[[1]]$source[[1]][2]
      cser <- list(oneSerie(name))
  }
  else {
    if (dplyr::is.grouped_df(df)) {    # groups
      grnm <- dplyr::group_vars(df)[[1]]   # just 1st g.column matters
      tmp <- df |> dplyr::group_split()
      cser <- lapply(tmp, \(gp) {
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

  stopifnot('ecs.exec: missing proxy'= !missing(proxy))
  stopifnot('ecs.exec: must pass ecsProxy object'= inherits(proxy, 'ecsProxy'))
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
  
  if (interactive())
    proxy$session$sendCustomMessage('kahuna', plist)
  return(proxy)
}


# ----------- Utilities ----------------------
#  more utilities in util.R


#' Parallel Axis
#' 
#' Create 'parallelAxis' for a parallel chart
#' 
#' @param df A data.frame, regular or grouped
#' @param minmax Boolean to add max/min limits or not, default TRUE
#' @param cols A string vector with columns names in desired order
#' @param ... Additional attributes for \href{https://echarts.apache.org/en/option.html#parallelAxis}{parallelAxis}.
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
  stopifnot('ec.paxis: df has to be data.frame'= inherits(df, 'data.frame'))
  pax <- list(); grnm <- ''
  cfilter <- 1:length(colnames(df))
  if (dplyr::is.grouped_df(df)) {
    # dont include grouping column (grnm)
    grnm <- dplyr::group_vars(df)[[1]]
    cfilter <- cfilter[!cfilter==match(grnm,colnames(df))]
  }
  if (!is.null(cols)) {
    stopifnot('ec.paxis: some cols not found'= all(cols %in% colnames(df)))
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
  stopifnot('ec.theme: name required'= !missing(name))

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
#' @param target type of resulting string value: \cr
#'    'opts' - the htmlwidget _options_ as JSON (default)\cr
#'    'full' - the _entire_ htmlwidget as JSON\cr
#'    'data' - info about chart's embedded data (char vector)
#' @param ... Additional attributes to pass to \link[jsonlite]{toJSON}
#' @return A JSON string, except when \code{target} is 'data' then
#'  a character vector.
#'
#' @details Must be invoked or chained as last command.\cr
#' target='full' will export all JavaScript custom code, ready to be used on import.
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
ec.inspect <- function(wt, target='opts', ...) {

  stopifnot("ec.inspect: target only 'opts', 'data' or 'full'"= target %in% c('opts','data','full'))
	if (target=='full')
		return(jsonlite::serializeJSON(wt))
  opts <- wt$x$opts
  
  if (target=='data') {
    out <- list()
    if (!is.null(opts$dataset))
      out <- sapply(opts$dataset, \(d) {
        if (!is.null(d$source[1])) 
          paste('dataset:',paste(unlist(d$source[1]), collapse=', '),
                'rows=',length(d$source))
        else if (!is.null(d$transform[1])) 
          gsub('"', "'", paste(d$transform, collapse=', '))
      })
    
    i <- 0
    out <- append(out, sapply(opts$series, \(s) {
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
  
  params <- list(...)
  if ('pretty' %in% names(params) && !params$pretty) 
    tmp <- jsonlite::toJSON(opts, force=TRUE, auto_unbox=TRUE, 
                             null='null', ...)
  else  # pretty by default
    tmp <- jsonlite::toJSON(opts, force=TRUE, auto_unbox=TRUE, 
                             null='null', pretty=TRUE, ...)
  
  return(tmp)
}


#' JSON to chart
#' 
#' Convert JSON string to chart
#' 
#' @param txt JSON character string or object, url, or file, see \link[jsonlite]{fromJSON}
#' @param ... Any attributes to pass to internal [ec.init]
#' @return An _echarty_ widget.
#' 
#' @details _txt_ could be either a list of options (x$opts) only for \href{https://echarts.apache.org/en/api.html#echartsInstance.setOption}{setOption},\cr
#'  or an entire _htmlwidget_ generated thru \code{ec.inspect(target='full')}.\cr
#'  The latter imports also all JavaScript functions defined by the user.
#' 
#' @examples
#' txt <- '{
#'    "xAxis": { "data": ["Mon", "Tue", "Wed"]},
#'    "yAxis": { },
#'    "series": { "type": "line", "data": [150, 230, 224] } }'
#' ec.fromJson(txt)
#'
#' @export
ec.fromJson <- function(txt, ...) {
  recur <- \(opts) {
    names <- names(opts)
    for(k in seq_along(opts)) {
      nn <- opts[[k]]
      if (inherits(nn, 'list'))
        opts[[k]] <- recur(opts[[k]])  # recursive
      else if (
        (!is.null(names) && names[[k]]=='renderItem') || 
        (inherits(nn,'character') && length(nn)==1 && 
           (grepl(') =>', nn) || 
            startsWith(nn, 'function(') ||
            startsWith(nn, 'new echarts.')))) {
        opts[[k]] <- htmlwidgets::JS(opts[[k]])
      }
    }
    opts
  }
  
  if (inherits(txt, 'json')) {
  	tmp <- jsonlite::parse_json(txt, FALSE)
  	if (!is.null(tmp$attributes$package)) {
  	  badge <- tmp$attributes$package$value[[1]]
  		if (badge=='echarty') {
  			obj <- jsonlite::unserializeJSON(txt)
     		return(obj)
  		}
      stop(paste('ec.fromJson: unknown input',badge))
  	}
  }
  tmp <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
	# options only
	obj <- ec.init(...)
	obj$x$opts <- recur(tmp)
	obj
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
  stopifnot('ec.plugjs: expecting echarty widget'= inherits(wt, 'echarty'))
  if (is.null(source)) return(wt)
  stopifnot('ec.plugjs: expecting source as URL or file://'= 
              startsWith(source, 'http') || startsWith(source, 'file://'))
  fname <- basename(source)
  fname <- unlist(strsplit(fname, '?', fixed=TRUE))[1]  # when 'X.js?key=Y'
  # if (!endsWith(fname, '.js'))
  #   stop('ec.plugjs expecting .js suffix', call. = FALSE)
  path <- system.file('js', package = 'echarty')
  ffull <- paste0(path,'/',fname)
  if (!file.exists(ffull)) {
    if (ask) {
      prompt <- paste0('One-time installation of plugin\n',fname,
                       '\n Would you like to proceed ?')
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

.r2jsEncode <- function(ss) {
  if (!is.null(ss$encode)) {
    for(i in 1:length(ss$encode)) {
      if (!is.numeric(ss$encode[[i]])) next
      # if (names(ss$encode[i])=='tooltip')
      #   ss$encode$tooltip <- unlist(ss$encode$tooltip) -1
      # else   # could be x,y,lng,lat,value,etc
        ss$encode[[i]] <- ss$encode[[i]] -1
    }
  }
  if (!is.null(ss$xAxisIndex)) ss$xAxisIndex <- ss$xAxisIndex -1
  if (!is.null(ss$yAxisIndex)) ss$yAxisIndex <- ss$yAxisIndex -1
  #if (!is.null(ss$datasetIndex)) ss$datasetIndex <- ss$datasetIndex -1
  ss
}

# convert from R to JS numbering
.renumber <- function(opa) {
  
  if (!is.null(opa$series))
    opa$series <- lapply(opa$series, .r2jsEncode)
  
  tmp <- opa$visualMap
  if (!is.null(tmp)) {
    doVmap <- function(x) {
      if (!is.null(x$dimension) && is.numeric(x$dimension)) x$dimension <- x$dimension -1
      if (!is.null(x$seriesIndex)) x$seriesIndex <- x$seriesIndex -1
      x
    }
    if (all(sapply(tmp, is.list)))
      opa$visualMap <- lapply(tmp, doVmap)
    else
      opa$visualMap <- doVmap(tmp)
  }
  opa
}

# merge named lists
.merlis <- function(l1, l2) {
	if (!inherits(l1, 'list')) l2
  else if (length(l1)==0) l2
	else if (!inherits(l2, 'list')) l1
  else if (length(l2)==0) l1
	else if (inherits(l1[[1]], 'list'))     # list of lists
		lapply(l1, \(x) {
			c(x, l2)[!duplicated(c(names(x), names(l2)), fromLast= TRUE)]
		})
	else
		c(l1, l2)[!duplicated(c(names(l1), names(l2)), fromLast= TRUE)]
}

#if (requireNamespace("shiny", quietly= TRUE)) {
  
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
  
#}

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
