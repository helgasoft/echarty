# ----------- Core --------------

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
#'    single: exposed _chart_ object (most common)\cr
#'    vector:\cr
#'  \verb{     }First expression is evaluated before chart initialization. \cr
#'  \verb{     }Second is evaluated with exposed object _opts_. \cr
#'  \verb{     }Third is evaluated with exposed _chart_ object after _opts_ set.
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
  # treacherous R does "partial matching of argument names" (like a bug): 
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
    namop <- names(opts)
    # list(show=TRUE) or list(list()) is to create an empty object{} in JS
    if (!'xAxis' %in% namop) 
      opts$xAxis <- list(show=TRUE)
    if (!'yAxis' %in% namop) 
      opts$yAxis <- list(show=TRUE)
    if (!any(c('series','options') %in% namop)) {
    	#if (!'world' %in% opts$load)   # world will add its own default serie
    	  opts$series <- list(list(type=if (is.null(ctype)) 'scatter' else ctype) )
    }
    
    if ('series' %in% names(opts)) {
      if (is.null(opts$series[[1]]$type))  # set default to user serie if omitted
        opts$series[[1]]$type <- if (is.null(ctype)) 'scatter' else ctype
      if (opts$series[[1]]$type %in% noAxis)
        opts$xAxis <- opts$yAxis <- NULL
    }
    else if (!is.null(ctype) && (ctype %in% noAxis))
        opts$xAxis <- opts$yAxis <- NULL
    if ('polar' %in% namop) {
      opts$xAxis <- opts$yAxis <- NULL
      if (is.null(opts$polar$radius)) opts$polar$radius = 111
      if (is.null(opts$radiusAxis)) opts$radiusAxis= list(type= 'category')
      if (is.null(opts$angleAxis)) opts$angleAxis= list(doit=TRUE)
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
      # add encode to series if missing after grouping
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
    }  # colX,colY, visualMap
  }
  
  if (!is.null(x$opts$series) && !is.null(series.param)) {
    x$opts$series <- .merlis(x$opts$series, series.param)
    # TODO: not 'x','y'
    tmp <- series.param$encode
    if (!is.null(tmp)) {
      if (is.numeric(tmp$x)) colX <- tmp$x
      else if (!is.null(df)) colX <- which(colnames(df) %in% tmp$x)
      if (is.numeric(tmp$y)) colY <- tmp$y
      else if (!is.null(df)) colY <- which(colnames(df) %in% tmp$y)
    }
  }
  
  if (preset) {
    # TODO: set axis type from series.data
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
  }  # axes

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
      if (!is.null(df)) {   # coordinateSystem='geo' needed for all series
        wt$x$opts$series <- .merlis(wt$x$opts$series, list(coordinateSystem='geo'))
        # tmp <- sapply(wt$x$opts$series, \(x) {x$type} )
        # if (!'map' %in% tmp)  # add map serie if missing
        #   wt$x$opts$series <- append(wt$x$opts$series,
        #                               list(list(type='map', geoIndex=0)))
      }
      # WARN: map will duplicate if series have map='world' too
      if (!'geo' %in% names(wt$x$opts))
        wt$x$opts$geo = list(map='world', roam=TRUE)
      # if (!is.null(df))  # cancelled: don't know if df first 2 cols are 'lng','lat'
      #   wt$x$opts$geo$center= c(mean(unlist(df[,1])), mean(unlist(df[,2])))
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
        # wt$x$opts$series <- lapply(wt$x$opts$series,
        #   function(ss) {
        #     if (is.null(ss$coordinateSystem)) ss$coordinateSystem <- 'leaflet'
        #     ss })
        wt$x$opts$series <- .merlis(wt$x$opts$series, list(coordinateSystem='leaflet'))
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
#' Orange |> dplyr::group_by(Tree) |> 
#' ec.init(series.param= list(universalTransition= list(enabled=TRUE))) |>
#' ec.upd({ 
#'  series <- lapply(series, function(ss) { ss$groupId <- ss$name; ss })
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
#' Custom series to display error-bars for scatter, bar or line series
#' 
#' @param wt An echarty widget to add error bars to, see [ec.init].
#' @param encode Column selection for both axes (x & y) as vectors, see \href{https://echarts.apache.org/en/option.html#series-bar.encode}{encode}
#' @param hwidth Half-width of error bar in pixels, default is 6.
#' @param ... More parameters for \href{https://echarts.apache.org/en/option.html#series-custom.type}{custom serie}
#' @return A widget with error bars added if successful, otherwise the input widget
#'
#' @details
#' Command should be called after _ec.init_ where main series are set.\cr
#' _ecr.ebars_ are custom series, so _ec.init(load='custom')_ is required. \cr
#' Horizontal and vertical layouts supported, only switch _encode_ values 'x' and 'y', both for series and ecr.ebars.\cr
#' Grouped bar series are supported.\cr
#' Have own default tooltip format showing _value, high & low_.\cr
#' Non-grouped series could be shown with formatter _riErrBarSimple_ instead of _ecr.ebars_. See example below.\cr
#' Limitations:\cr
#' \verb{     }manually add axis type='category' if needed\cr
#' \verb{     }error bars cannot have own name when data is grouped\cr
#' \verb{     }legend select/deselect will not re-position grouped error bars\cr
#' 
#' @examples
#' library(dplyr)
#' df <- mtcars |> group_by(cyl,gear) |> summarise(yy= round(mean(mpg),2)) |>
#'   mutate(low= round(yy-cyl*runif(1),2), high= round(yy+cyl*runif(1),2))
#' ec.init(df, load= 'custom', ctype= 'bar', tooltip= list(show=TRUE), 
#'       xAxis= list(type='category')) |> 
#' ecr.ebars(encode= list(y=c(3,4,5), x=2))
#'      
#' # ----- riErrBarSimple ------
#' df <- mtcars |> mutate(x=1:nrow(mtcars),hi=hp-drat*3, lo=hp+wt*3) |> select(x,hp,hi,lo)
#' ec.init(df, load= 'custom', legend= list(show= TRUE)) |> 
#' ec.upd({ series <- append(series, list(
#'     list(type= 'custom', name= 'error',
#'          data= ec.data(df |> select(x,hi,lo)),
#'          renderItem= htmlwidgets::JS('riErrBarSimple')
#'     )))
#' })
#' 
#' @export
ecr.ebars <- function(wt, encode=list(x=1, y=c(2,3,4)), hwidth=6, ...) {
  # alternating bars with custom series doesn't work, first bars then customs
  stopifnot('ecr.ebars: expecting widget'= !missing(wt))
  stopifnot('ecr.ebars: expecting echarty widget'= inherits(wt, "echarty"))
  if (!'renderers' %in% unlist(sapply(wt$dependencies, `[`, "name")))
    stop("use ec.init(load='custom') before ecr.ebars", call.=FALSE)
  #stopifnot('ecr.ebars: encode is required'= !is.null(encode))
  stopifnot('ecr.ebars: encode is invalid'= !is.null(encode$x) && !is.null(encode$y))
  stopifnot('ecr.ebars: encode x/y invalid'= abs(length(encode$x)-length(encode$y))==2)

  sers <- wt$x$opts$series  # all series
  stopifnot('ecr.ebars: series are missing'= !is.null(sers))
  args <- list(...)
  
  # find eligible series and extract names, etc.
  cntr <- function(x, typ) { 
    if (length(grep(typ, x))>0) {
      nme <- if (!is.null(args$name)) args$name else if (is.null(x$name)) wt$x$opts$yAxis$name else x$name
      ds <- if (is.null(x$datasetIndex)) 1 else x$datasetIndex
      dm <- if (is.null(x$dimensions)) NULL else x$dimensions
      dd <- if (is.null(x['data']$data)) NULL else x['data']$data
      list(nm=nme, ds=ds, dd=dd, dm=dm)
    } else NULL
  }
  tmp <-                           lapply(sers, cntr, typ='bar')
  if (is.null(unlist(tmp))) tmp <- lapply(sers, cntr, typ='scatter')
  if (is.null(unlist(tmp))) tmp <- lapply(sers, cntr, typ='line')
  if (is.null(unlist(tmp))) 
    return(wt)    # no bar/line/scatter, nothing to attach to
  
  enc2num <- function(out, liss) {
    # convert encode to numerical if character
    if (is.character(out)) {
      if (is.null(liss$dm)) {   # get dimensions from dataset
        ds <- wt$x$opts$dataset[[liss$ds]]
        if (!is.null(ds)) {
          if (!is.null(ds$dimensions))
            out <- which(ds$dimensions %in% out)
          else {
            if (!is.null(ds$sourceHeader) && ds$sourceHeader)
              out <- which(ds$source[[1]] %in% out)
            else {
              if (!class(ds$source[[1]]) %in% class(ds$source[[2]]))
                out <- which(ds$source[[1]] %in% out)
              else
                stop('could not find names from encode', call.=FALSE)
            }
          }
        }
      } else
        out <- which(liss$dm %in% out)  # from data
    }
    out
  }
  encode$y <- enc2num(encode$y, tmp[[1]])   # assumes all series from same dataset
  encode$x <- enc2num(encode$x, tmp[[1]])
  # set correct axis to type 'category' (char or factor)
  enc <- wt$x$opts$dataset[[1]]$source[[2]]$encode
  if (!is.null(enc)) {
    if (length(encode$y)==1) {
      if (is.character(enc$y) || is.factor(enc$y))
        wt$x$opts$yAxis <- .merlis(wt$x$opts$yAxis, list(type='category'))
    } else {
      if (is.character(enc$x) || is.factor(enc$x))
        wt$x$opts$xAxis <- .merlis(wt$x$opts$xAxis, list(type='category'))
    }
  }
  
  rim <- if (!is.null(args$renderItem)) args$renderItem else 'riErrBars'
  oneSerie <- function(liss, ...) {
    cc <- list(type='custom', datasetIndex= liss$ds, encode= encode,
    			 renderItem= htmlwidgets::JS(rim), ...)
	  if (is.null(cc$name)) cc$name <- liss$nm
	  if (!is.null(liss$dd)) cc$data <- liss$dd
	  #if (is.null(c$data) && is.null(c$datasetIndex)) c$datasetIndex <- 0
    if (is.null(cc$z)) cc$z <- 3   # over bar
    if (is.null(cc$itemStyle$borderWidth)) cc$itemStyle$borderWidth <- 1.5
    if (is.null(cc$color) && is.null(cc$itemStyle$color)) {
      # set own color, or it will blend with main bar
      # impression that cc$itemStyle$color is better than cc$color
      cc$itemStyle$color <- 'brown'  # 'darkslategray'
    }
    cc$itemStyle$borderDashOffset <- hwidth  # => lineDashOffset
    
    if (is.null(cc$tooltip) && is.null(encode$tooltip)) {
      tt <- if (length(cc$encode$x) > length(cc$encode$y)) cc$encode$x else cc$encode$y
      cc$tooltip <- list(formatter= ec.clmn(
        '<br>value <b>%@</b> <br>range <b>%@</b> to <b>%@</b>', tt[1],tt[2],tt[3]))
    }
    if (is.numeric(cc$encode$x)) cc$encode$x <- cc$encode$x -1  # R to JS
    if (is.numeric(cc$encode$y)) cc$encode$y <- cc$encode$y -1  # R to JS
    cc
  }
  # build err.bar series
  cser <- lapply(tmp, \(x) oneSerie(x, ...))

  wt$x$opts$series <- append(wt$x$opts$series, cser)
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
ecs.output <- function(outputId, width= '100%', height= '400px') {
  htmlwidgets::shinyWidgetOutput(outputId, 'echarty', width, height, package= 'echarty')
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
ecs.render <- function(wt, env=parent.frame(), quoted= FALSE) {
  if (!quoted) {
    wt <- substitute(wt)  # do not add ',env' in substitute command
  } # force quoted
  htmlwidgets::shinyRenderWidget(wt, ecs.output, env, quoted= TRUE)
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
  sessi <- globalenv()
  if (interactive()) {
    if (requireNamespace("shiny", quietly = TRUE)) {
      sessi <- shiny::getDefaultReactiveDomain()
    } else 
      return(invisible(NULL))
  }
  proxy <- list(id= id, session= sessi)
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
ecs.exec <- function(proxy, cmd= 'p_merge') {

  stopifnot('ecs.exec: missing proxy'= !missing(proxy))
  stopifnot('ecs.exec: must pass ecsProxy object'= inherits(proxy, 'ecsProxy'))
  if (is.null(proxy$x) || is.null(proxy$x$opts))
    stop('ecs.exec: proxy is empty', call. = FALSE)
  
  plist <- list(id = proxy$id, 
                opts = proxy$x$opts,
                action = cmd)
  
  # create web dependencies for JS, if present
  if (!is.null(proxy$dependencies)) {
    if (interactive()) {
      if (requireNamespace("shiny", quietly = TRUE)) {
        plist$deps <- list(shiny::createWebDependency(
          htmltools::resolveDependencies( proxy$dependencies )[[1]]
        ))
      }
    }
  }
  if (!is.null(proxy$session))
    proxy$session$sendCustomMessage('kahuna', plist)
  return(proxy)
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

if (interactive()) {
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
