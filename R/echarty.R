# ----------- Core --------------

#' echarty
#'
#' @includeRmd vignettes/echarty.Rmd
#' 
#' @name -- Introduction --
NULL

the <- new.env(parent = emptyenv())
the$.ecv.colnames <- NULL
noAxis <- c('radar','parallel','themeRiver','map','gauge','pie','funnel','polar',  
        'sunburst','tree','treemap','sankey','lines', 'liquidFill','wordCloud')
noCoord <- c('polar','radar','singleAxis','parallelAxis','calendar')
# using list(show=TRUE) or list(list()) is to create empty object{} in JS

#' Initialize command
#'
#' Required to build a chart. In most cases this will be the only command necessary.
#'
#' @param df Optional data.frame to be preset as \href{https://echarts.apache.org/en/option.html#dataset}{dataset}, default NULL \cr
#'   By default the first column is for X values, second column is for Y, and third is for Z when in 3D.\cr
#'   Best practice is to have the grouping column placed last. Grouping column cannot be used as axis.\cr
#'   Timeline requires a _grouped data.frame_ to build its \href{https://echarts.apache.org/en/option.html#options}{options}.\cr
#'   If grouping is on multiple columns, only the first one is used to determine settings.
#' @param ctype Chart type, default is 'scatter'. Could be set in _series.param_ instead.
#' @param preset Boolean (default TRUE). Build preset attributes like dataset, series, xAxis, yAxis, etc.\cr
#'   When preset is FALSE, these attributes need to be set explicitly.\cr
#' @param series.param  Additional attributes for single preset series, default is NULL.\cr
#'  Defines a **single** series for both non-timeline and timeline charts. \cr
#'  **Multiple** series should be defined directly with _series=list(list(type=...),list(type=...))_ or added with [ec.upd].
#' @param tl.series Deprecated, use _timeline_ and _series.param_ instead.\cr
#' @param ...  Optional widget attributes. See Details. \cr
#' @param width,height Optional valid CSS unit (like \code{'100\%'},
#'   \code{'500px'}, \code{'auto'}) or a number, which will be coerced to a
#'   string and have \code{'px'} appended.
#'
#' @details  Command _ec.init_ creates a widget with \link[htmlwidgets]{createWidget}, then adds some ECharts features to it.\cr
#'  Numerical indexes for series,visualMap,etc. are R-counted (1,2...)\cr
#' 
#'  **Presets**: \cr 
#'  When data.frame **df** is present, a \href{https://echarts.apache.org/en/option.html#dataset}{dataset} is preset. \cr
#'  When **df** is grouped and _ctype_ is not NULL, more datasets with legend and series are also preset. \cr
#'  Plugin '3D' (load='3D') is required for GL series like _scatterGL, linesGL_, etc. \cr
#'  Plugins 'leaflet' and 'world' preset _center_ to the mean of all coordinates from **df**. \cr
#'  Users can delete or overwrite any presets as needed. \cr
#'  
#'  **Widget attributes**: \cr
#'  Optional echarty widget attributes include: \cr
#'  * elementId - Id of the widget, default is NULL(auto-generated)
#'  * load - name(s) of plugin(s) to load. A character vector or comma-delimited string. default NULL.
#'  * ask - prompt user before downloading plugins when _load_ is present, FALSE by default
#'  * js - single string or a vector with JavaScript expressions to evaluate.\cr 
#'    single: exposed _chart_ object (most common)\cr
#'    vector: \verb{     }see demo code in [ec.examples]\cr
#'  \verb{     }First expression evaluated before initialization, exposed object _window_ \cr
#'  \verb{     }Second is evaluated with exposed object _opts_. \cr
#'  \verb{     }Third is evaluated with exposed _chart_ object after _opts_ set.
#'  * renderer - 'canvas'(default) or 'svg'
#'  * locale - 'EN'(default) or 'ZH'. Use predefined or custom \href{https://gist.github.com/helgasoft/0618c6537c45bfd9e86d3f9e1da497b8}{like so}.
#'  * useDirtyRect - enable dirty rectangle rendering or not, FALSE by default, see \href{https://echarts.apache.org/en/api.html#echarts.init}{here}\cr
#'  
#'  **Built-in plugins**: \cr 
#'  * leaflet - Leaflet maps with customizable tiles, see \href{https://github.com/gnijuohz/echarts-leaflet#readme}{source}\cr
#'  * world - world map with country boundaries, see \href{https://github.com/apache/echarts/tree/master/test/data/map/js}{source} \cr
#'  * lottie - support for \href{https://lottiefiles.com}{lotties} \cr
#'  * ecStat - statistical tools, see\href{https://github.com/ecomfe/echarts-stat}{echarts-stat}\cr
#'  * custom - renderers for [ecr.band] and [ecr.ebars] \cr 
#'  Plugins with one-time installation: \cr
#'  * 3D - support for 3D charts and WebGL acceleration, see \href{https://github.com/ecomfe/echarts-gl}{source} and \href{https://echarts.apache.org/en/option-gl.html#series}{docs} \cr
#'  \verb{     }This plugin is auto-loaded when 3D/GL axes/series are detected.\cr
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
#'  **Timeline**:\cr
#'  Defined by _series.param_ for the \href{https://echarts.apache.org/en/option.html#series}{options series} and a _timeline_ list for the \href{https://echarts.apache.org/en/option.html#timeline}{actual control}.
#'  A grouped _df_ is required, each group providing data for one option serie.
#'  Timeline \href{https://echarts.apache.org/en/option.html#timeline.data}{data} and \href{https://echarts.apache.org/en/option.html#options}{options} will be preset for the chart.\cr
#'  Each option title can include the current timeline item by adding a placeholder '%@' in title$text. See example below.\cr
#'  Another preset is _encode(x=1,y=2,z=3)_, which are the first 3 columns of _df_. Parameter _z_ is ignored in 2D. See Details below.\cr
#'  Optional attribute _groupBy_, a _df_ column name, can create series groups inside each timeline option.\cr
#'  Options/timeline for hierarchical charts like graph,tree,treemap,sankey have to be built directly, see \href{https://helgasoft.github.io/echarty/uc4.html}{example}.
#'  
#'  **\href{https://echarts.apache.org/en/option.html#series-line.encode}{Encode}** \cr
#'  A series attribute to define which columns to use for the axes, depending on chart type and coordinate system: \cr
#'  * set _x_ and _y_ for coordinateSystem _cartesian2d_
#'  * set _lng_ and _lat_ for coordinateSystem _geo_ and _scatter_ series
#'  * set _value_ and _name_ for coordinateSystem _geo_ and _map_ series
#'  * set _radius_ and _angle_ for coordinateSystem _polar_
#'  * set _value_ and _itemName_ for _pie_ chart
#'  Example: \code{encode(x='col3', y='col1')} binds xAxis to _df_ column 'col3'.
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
#'     symbolSize= ec.clmn('Petal.Width', scale=7),
#'     tooltip= list(formatter= ec.clmn('Petal.Width: %@', 'Petal.Width'))
#'   )
#' )
#' 
#' data.frame(n=1:5) |> dplyr::group_by(n) |> ec.init(
#'   title= list(text= "gauge #%@"),
#'   timeline= list(show=TRUE, autoPlay=TRUE),
#'   series.param= list(type='gauge', max=5)
#' )
#' 
#' @importFrom htmlwidgets createWidget sizingPolicy getDependency JS shinyWidgetOutput shinyRenderWidget
#' @importFrom utils read.csv
#' @import dplyr
#' 
#' @export
ec.init <- function( df= NULL, preset= TRUE, ctype= 'scatter', ...,
                     series.param= NULL, tl.series= NULL, 
                     width= NULL, height= NULL) {
  
  opt1 <- list(...)  
  # treacherous R does "partial matching of argument names" (like a bug): 
  #   if 'series.param' is before ... and 'series' is added, the latter is ignored!
  elementId <- if (is.null(opt1$elementId)) NULL else opt1$elementId
  js <- if (is.null(opt1$js)) NULL else opt1$js
  ask <- if (is.null(opt1$ask)) FALSE else opt1$ask
  renderer <- if (is.null(opt1$renderer)) 'canvas' else tolower(opt1$renderer)
  locale <- if (is.null(opt1$locale)) 'EN' else toupper(opt1$locale)
  useDirtyRect <- if (is.null(opt1$useDirtyRect)) FALSE else opt1$useDirtyRect
  xtKey <- if (is.null(opt1$xtKey)) 'XkeyX' else opt1$xtKey
  # allow debug feedback thru cat() in JS and R code:
  dbg <- if (is.null(opt1$dbg)) FALSE else opt1$dbg   
  # remove the above attributes since they are not valid ECharts options
  opt1$ask <- opt1$js <- opt1$renderer <- opt1$locale <- NULL
  opt1$useDirtyRect <- opt1$elementId <- opt1$xtKey <- opt1$dbg <- NULL
  axis2d <- c('pictorialBar','candlestick','boxplot','scatterGL') #'custom',
  isCrosstalk <- FALSE; deps <- NULL
  
  # forward widget options using x
  x <- list(
    theme = '',
    draw = TRUE,
    renderer = renderer,
    locale = locale,
    useDirtyRect = useDirtyRect,
    jcode = js, dbg = dbg,
    opts = opt1
    # settings= list( crosstalk_key= key, crosstalk_group= group )
  )

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
            .ty <- 'category'  # default when data without type
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
    # set axes names from encode or df-columns
    tmp <- list(x=NULL, y=NULL)
    lapply(series, \(ss) {
      if (any(names(ss)=='encode')) {
        if (is.character(ss$encode$x)) tmp$x <<- c(tmp$x, ss$encode$x[1])
        if (is.character(ss$encode$y)) tmp$y <<- c(tmp$y, ss$encode$y[1])
      }
    })

    if (!is.null(tmp$x)) {   # dont name multiple xAxis
      if (is.null(x$opts$xAxis$name) && !is.null(names(lengths(x$opts$xAxis))))
        x$opts$xAxis$name <<- trimws(paste(unique(tmp$x), collapse=','))
      tt <- tmp$x[1]
      colX <<- if (is.numeric(tt)) tt else which(colnames(df)==tt)[1]
    }
    if (!is.null(tmp$y)) {
      if (is.null(x$opts$yAxis$name) && !is.null(names(lengths(x$opts$yAxis))))
        x$opts$yAxis$name <<- trimws(paste(unique(tmp$y), collapse=','))
      tt <- tmp$y[1]
      colY <<- if (is.numeric(tt)) tt else which(colnames(df)==tt)[1]
    }
  }
  xyNamesCS <- function(ser) {
    # set x,y names + cs, called from single or tl.series
    # careful: setting x$opts$ but could be wt$x$opts coming from tl.series
    xtem <- 'x'; ytem <- 'y'
    if (is.null(ser$coordinateSystem))
      ser$coordinateSystem <- 'unknown'
    if (ser$type %in% axis2d)
      ser$coordinateSystem <- 'cartesian2d'   # default 'scatter' comes here
    if (any(noCoord %in% names(opt1)) || 
        ser$type %in% c('map','themeRiver'))
      ser$coordinateSystem <- 'unknown'       # keep to compare below
    if (ser$type %in% c('scatter3D','bar3D','line3D','surface'))
      ser$coordinateSystem <- 'cartesian3D'
    if (ser$type %in% c('scatter3D','bar3D','lines3D')) {
      if (!is.null(opt1$geo3D)) ser$coordinateSystem <- 'geo3D'
      if (!is.null(opt1$globe)) ser$coordinateSystem <- 'globe'
    }
    if (ser$type %in% c('scatter','scatterGL','lines')) {
      if (!is.null(opt1$geo)) ser$coordinateSystem <- 'geo'
      if ('world' %in% opt1$load) ser$coordinateSystem <- 'geo'
      if ('leaflet' %in% opt1$load) ser$coordinateSystem <- 'leaflet'
    }
    #if (!is.null(opt1$leaflet)) ser$coordinateSystem <- 'leaflet'
      
    if (!is.null(opt1$calendar) && ser$type %in% c('heatmap','scatter','effectScatter'))
      ser$coordinateSystem <- 'calendar'
    #if (!is.null(opt1$radar)) series?$type <- 'radar'
    if (ser$type == 'parallel') {
      if (is.null(opt1$parallelAxis) && !is.null(df))
        x$opts$parallelAxis <<- ec.paxis(df)
      if (!is.null(grnm) && tail(colnames(df),1) != grnm)
        stop(paste0("ec.init: df group column '",grnm,"' should be last for parallel chart"))
    }
    if (ser$type == 'themeRiver')
      x$opts$singleAxis <<- .merlis(x$opts$singleAxis, list(min='dataMin', max='dataMax'))
    if (ser$type %in% noAxis || any(c('roam') %in% names(ser))) # +graph
      x$opts$xAxis <<- x$opts$yAxis <<- NULL
    
    if (ser$type == 'pie') {
      xtem <- 'value'; ytem <- 'itemName' }
    if (ser$coordinateSystem=='polar') { 
      xtem <- 'radius'; ytem <- 'angle' }
    if (ser$coordinateSystem %in% c('geo','leaflet')) {
      xtem <- 'lng'; ytem <- 'lat' }
    if (ser$type == 'map') {
      xtem <- 'name'; ytem <- 'value' }
    if (ser$coordinateSystem=='unknown')
      ser$coordinateSystem <- NULL
    return(list(x=xtem, y=ytem, z='z', c=ser$coordinateSystem))
  }
  doVMap <- function(wid) {
    # visualMap assist: auto add min/max/calculable   (categories==piecewise)
    vm <- wid$opts$visualMap
    out <- NULL
    if (!is.null(df) && !is.null(vm) &&
        is.null(vm$min) && is.null(vm$max) && is.null(vm$categories) &&
        (is.null(vm$type) || (vm$type == 'continuous')) ) {
      
        xx <- length(colnames(df))   # last numeric column by default
        for(xx in xx:1) if (is.numeric(df[,xx])) break
        if (any(names(df) == 'value') && (
          (!is.null(tl.series) && tl.series$type=='map') ||
          (!is.null(series.param) && series.param$type=='map'))
        ) xx <- 'value'
        if (!is.null(vm$dimension)) xx <- vm$dimension
        out <- list(
          min= min(na.omit(df[,xx])),
          max= max(na.omit(df[,xx])),
          calculable= TRUE
        )
    }
    out
  }
 
  # ------------- data.frame -------------------
  colX <- 1     # by default 1st column is X, 2nd is Y, 3rd is Z
  colY <- 2
  grnm <- NULL
  if (!is.null(df)) {
    stopifnot('ec.init: df should be data.frame or SharedData'= 
                any(class(df) %in% c("SharedData", "data.frame")))
    .setColnm(colnames(df))
    if (dbg) cat('\n coln=', .getColnm())
    
    ct.key <- ct.group <- ct.dfKey <- NULL
    if (requireNamespace("crosstalk", quietly= TRUE)) {
      if (crosstalk::is.SharedData(df)) {
        isCrosstalk <- TRUE
        ct.key <- as.list(df$key())
        ct.group <- df$groupName()
        deps <- crosstalk::crosstalkLibs()
        ct.dfKey <- df$key()
        df <- df$origData()
      }
    }
    if (xtKey=='XkeyX') df$XkeyX <- ct.dfKey   # add new column for Xtalk filtering, if needed
    x$settings = list(
      crosstalk_key = ct.key,
      crosstalk_group = ct.group
    )
    
    # if data.frame given, build dataset regardless of 'preset' or 'dataset'
    
    # grouping uses transform
    if (!is.null(ctype) && dplyr::is.grouped_df(df)) {
      #grnm <- dplyr::group_vars(df)[[1]]   # name of 1st grouping column 
      grnm <- df |> group_vars() |> first() |> as.character()  # convert if factor
      x$opts$dataset <- list(list(dimensions= colnames(df), source= ec.data(df)))
      grvals <- unlist(dplyr::group_data(df)[grnm], use.names=FALSE)
      txfm <- sers <- list()
      legd = list(data= list())
      k <- 0
      for(nm in grvals) { 
        k <- k+1
        txfm <- append(txfm, list(list(transform= list(
          type= 'filter', config= list(dimension= grnm, '='=nm)), id= nm)))
        sers <- append(sers, list(list(  # datasetIndex will be decremented later
          type= ctype, datasetIndex= k+1, name= as.character(nm))))
        # if (colnames(df)[1]==grnm)  # grouping by 1st column - breaks prll,map,etc.
        legd$data <- append(legd$data, list(list(name=as.character(nm))))
      }
      if (preset) {
        if (is.null(opt1$series)) x$opts$series <- sers
        if (is.null(opt1$legend)) x$opts$legend <- legd
      }
      x$opts$dataset <- append(x$opts$dataset, txfm)
    } 
    else 
      x$opts$dataset <- list(list(dimensions= colnames(df), source= ec.data(df)))
    
    if (preset) {
      # group by any column, prevent group columns from becoming X/Y axis
      if (!is.null(grnm)) {  # find position of group column
        pos <- which(colnames(df)==grnm)
        if (!is.null(tl.series) && !is.null(tl.series$groupBy))
          pos <- c(pos, which(colnames(df)==tl.series$groupBy))
        if (!is.null(series.param) && !is.null(series.param$groupBy))
          pos <- c(pos, which(colnames(df)==series.param$groupBy))
        allp <- rep(TRUE, length(colnames(df)))
        allp <- replace(allp, pos, FALSE)
        colX <- which(allp==TRUE)[1]   # first two==TRUE are X,Y
        colY <- which(allp==TRUE)[2]
        if (is.na(colY)) colY <- length(colnames(df))
        # if (is.na(colY))
        #   # map data can have only 2 columns and group by one of them
        #   if (ctype=='map' ||
        #       (!is.null(tl.series) && tl.series$type=='map') ||
        #       (!is.null(series.param) && series.param$type=='map'))
        #     colY <- 2
        # 
        # stopifnot('ec.init: df must have at least 3 columns when grouping by one'= !is.na(colY))
      }

      tmp <- doVMap(x)
      x$opts$visualMap <- .merlis(x$opts$visualMap, tmp)
      #if (is.null(opt1$visualMap)) x$opts$visualMap <- tmp
    }  # colX,colY, visualMap
  }
  #else .setColnm()  # do not reset, ec.data could've done it

  # presets are default settings, user can ignore or replace them
  if (preset) {
    namop <- names(x$opts)
    if (!'xAxis' %in% namop) 
      x$opts$xAxis <- list(show=TRUE)
    if (!'yAxis' %in% namop) 
      x$opts$yAxis <- list(show=TRUE)
    if (!is.null(ctype)) {
      if (!any(c('series','options') %in% namop))
        x$opts$series <- list(list(type= ctype))
      if (!is.null(tl.series) && is.null(tl.series$type)) 
        tl.series$type <- ctype
    }
    if ('series' %in% names(x$opts)) { 
      # set default to user serie if omitted
      if (is.null(x$opts$series[[1]]$type) && !is.null(ctype))
        x$opts$series[[1]]$type <- ctype
        #series.param = .merlis(series.param, list(type= ctype)) # TODO?
      if (x$opts$series[[1]]$type %in% noAxis) { # || series.param$type %in% noAxis) {
        x$opts$xAxis <- x$opts$yAxis <- NULL
      }
    }
    #else if ((!is.null(ctype) && (ctype %in% noAxis)) ||
    if (any(c('geo',noCoord) %in% namop))
      x$opts$xAxis <- x$opts$yAxis <- NULL
    if ('polar' %in% namop) {
      if (is.null(x$opts$polar$radius)) x$opts$polar$radius = 111
      if (is.null(x$opts$radiusAxis)) x$opts$radiusAxis= list(type= 'category')
      if (is.null(x$opts$angleAxis)) x$opts$angleAxis= list(doit=TRUE)
      if (!is.null(series.param)) 
        series.param = .merlis(series.param, list(coordinateSystem= "polar"))
    }
    
    # series --------------------
    if (!is.null(series.param)) {
      x$opts$series <- .merlis(x$opts$series, series.param)
      
      # if (x$opts$series[[1]]$type=='map')
      #   x$opts$series[[1]] <- .merlis(x$opts$series[[1]], list(geoIndex=1))
    }
    x$opts$series <- lapply(x$opts$series, function(ss) {
      tmp <- xyNamesCS(ss)
      if (!is.null(tmp$c)) ss$coordinateSystem <- tmp$c

      if (ss$type=='map' && is.null(ss$geoIndex))
        ss <- .merlis(ss, list(geoIndex=1))
      
      # add encode to series after grouping, if missing 
      if (!(colX==1 && colY==2)) {
        xtem <- tmp$x; ytem <- tmp$y
        if (!any(names(ss)=='encode')) {
            ss$encode <- list()
            ss$encode[xtem] <- colX   # R count
            ss$encode[ytem] <- colY 
        }
        # else don't overwrite user's encode
      }
      ss
    })
      
      # reading from encode (set above)
        # TODO: when names not 'x','y' ?
      tmp <- series.param$encode
      if (!is.null(tmp) && !is.null(x$opts$xAxis)) {
        if (is.numeric(tmp$x)) colX <- tmp$x
        else if (!is.null(df)) colX <- which(colnames(df) %in% tmp$x)
        if (is.numeric(tmp$y)) colY <- tmp$y
        else if (!is.null(df)) colY <- which(colnames(df) %in% tmp$y)
      }
  
      # set X,Y axes type & name from df
      # TODO: get axis type also from series.data
      if (!is.null(df)) {
        axNamesEnc(x$opts$series)
        axNamesEnc(list(tl.series))
        if (!is.null(x$opts$xAxis)) x$opts$xAxis <- doType(colX, x$opts$xAxis)
        if (!is.null(x$opts$yAxis)) x$opts$yAxis <- doType(colY, x$opts$yAxis)
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
  #if (dbg) cat('\naxis2d=',axis2d)
  
  tmp <- getOption('echarty.font')
  if (!is.null(tmp))
    wt$x$opts$textStyle <- list(fontFamily= tmp)
  
  tmp <- getOption('echarty.theme')   # default
  if (!is.null(tmp))
    wt <- ec.theme(wt, tmp)
  
  # ------------- plugins loading -----------------------------
  opt1 <- wt$x$opts
  load <- opt1$load;  wt$x$opts$load <- NULL
  if (length(load)==1 && grepl(',', load, fixed=TRUE))
      load <- unlist(strsplit(load, ','))
  # autoload 3D 
  cnd1 <- any(c('xAxis3D','yAxis3D','zAxis3D','grid3D','globe','geo3D') %in% names(opt1))
  styp <- ctype
  if (!is.null(series.param) && !is.null(series.param$type))
    styp <- series.param$type
  cnd2 <- any(endsWith(styp, c('3D','GL')))
  if ((cnd1 || cnd2) && !'3D' %in% load) load <- c(load, '3D')
  
  path <- system.file('js', package= 'echarty')
  dep <- NULL
  
  if ('world' %in% load) {
    if (preset) {
      wt$x$opts$xAxis <- wt$x$opts$yAxis <- NULL
      if (!is.null(df)) {   # coordinateSystem='geo' needed for all series
        wt$x$opts$series <- .merlis(wt$x$opts$series, list(coordinateSystem='geo'))
      }
      # WARN: duplicate maps if series have map='world' too
      if (!'geo' %in% names(opt1) && !'3D' %in% load)
        wt$x$opts$geo = list(map='world', roam=TRUE)
      # else {
      #   wt$x$opts$geo = .merlis(wt$x$opts$geo, list(map='world'))
      #   if (is.null(wt$x$opts$geo$roam)) wt$x$opts$geo$roam <- TRUE
      # }
      # if (!is.null(df))  # cancelled: don't know if df first 2 cols are 'lng','lat'
      #   wt$x$opts$geo$center= c(mean(unlist(df[,1])), mean(unlist(df[,2])))
    }
    dep <- htmltools::htmlDependency(
      name = 'world', version = '1.0.0', 
      src = c(file = path), script= 'world.js')
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  if ('leaflet' %in% load) {
      # coveralls pops error, win/linux ok :
      #stopifnot("ec.init: library 'leaflet' not installed"= file.exists(file.path(.libPaths(), 'leaflet')[[1]]))
    if (!file.exists(file.path(.libPaths(), 'leaflet')[[1]])) warning("ec.init: library 'leaflet' not installed")
    if (preset) {
      # customizations for leaflet
      wt$x$opts$xAxis <- wt$x$opts$yAxis <- NULL
      urltls <- getOption('echarty.urlTiles')
      if (is.null(urltls))
        urltls <- 'https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png'
      if (!'leaflet' %in% names(opt1)) {
        wt$x$opts$leaflet = list(
          roam = TRUE,
          tiles = list( list(urlTemplate = urltls))
        )
      } 
      if (!'tiles' %in% names(opt1$leaflet))
        wt$x$opts$leaflet$tiles <- list( list(urlTemplate = urltls))
      if (!'zoom' %in% names(opt1$leaflet))
        wt$x$opts$leaflet$zoom <- 6
      if (!'center' %in% names(opt1$leaflet)) {
        if (!is.null(df)) 
          wt$x$opts$leaflet$center= c(mean(unlist(df[,1])), mean(unlist(df[,2])))
      }
      if ('series' %in% names(opt1))
        wt$x$opts$series <- .merlis(wt$x$opts$series, list(coordinateSystem='leaflet'))
      
    }
    
    #wt$dependencies <- append(wt$dependencies, htmlwidgets::getDependency('leaflet'))  # working with leaflet <=v.2.1.0
    wt$dependencies <- append(wt$dependencies, list( htmltools::htmlDependency(
      name= "leaflet", version= "1.3.1", package= "leaflet", src= "htmlwidgets/lib/leaflet",
      script= "leaflet.js", stylesheet= "leaflet.css") )
    )
    dep <- htmltools::htmlDependency(
      name= 'echarts-leaflet', version= '1.0.0', src= c(file= path), 
      script= 'echarts-leaflet.js')
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
  if ('ecStat' %in% load) {
    dep <- htmltools::htmlDependency(
      name = 'ecStat', version = '1.0.0', 
      src = c(file = path), script= 'ecStat.min.js')
    wt$dependencies <- append(wt$dependencies, list(dep))
  }
  
  # Plugins implemented as dynamic load on-demand
  if (any(load %in% c('3D','liquid','gmodular','wordcloud'))) {
    plf <- read.csv(system.file('plugins.csv', package='echarty'), header=TRUE, stringsAsFactors=FALSE)
    if ('3D' %in% load) {
      if (preset) {       # replace 2D presets with 3D
      isGL <- any(unlist(lapply(opt1$series, \(k){ endsWith(k$type, 'GL') })))  # all GL are 2D
      if (!isGL) isGL <- endsWith(styp, 'GL')
      isMap3d <- !is.null(opt1$globe) || !is.null(opt1$geo3D)
      if (isMap3d) isGL <- FALSE
      if (!isGL) {  
        # check for series types ending in 3D or GL
        stypes <- ifelse(!is.null(series.param), styp, 
                         unlist(lapply(opt1$series, \(k){k$type})) )
        stypes <- stypes[stypes!='surface']
        if (!is.null(stypes)) stopifnot("Non-3D series type detected"= all(endsWith(stypes, '3D')) )
        if (!isMap3d) {
          nops <- names(opt1)   # add defaults 3D
          for(x in c('xAxis3D','yAxis3D','zAxis3D','grid3D')) {
            a2d <- sub('3D','',x)
            if (!(x %in% nops)) 
              wt$x$opts[[x]] <- if (!is.null(wt$x$opts[[a2d]])) wt$x$opts[[a2d]]
                                else list(show=TRUE)
          }
        }
        wt$x$opts$xAxis <- wt$x$opts$yAxis <- NULL
      }
    }
      wt <- ec.plugjs(wt, plf[plf$name=='3D',]$url, ask)
    }
    if ('liquid' %in% load) wt <- ec.plugjs(wt, plf[plf$name=='liquid',]$url, ask)
    if ('gmodular' %in% load) wt <- ec.plugjs(wt, plf[plf$name=='gmodular',]$url, ask)
    if ('wordcloud' %in% load) wt <- ec.plugjs(wt, plf[plf$name=='wordcloud',]$url, ask)
  }  
  # load unknown plugins
  unk <- load[! load %in% c('leaflet','custom','world','lottie','ecStat',
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
    if ('series' %in% names(opt1))
      wt$x$opts$series[[1]]$datasetId= 'Xtalk'
  }
  
  
  # ------------- timeline is last -----------------
  if (is.null(tl.series) && is.null(opt1$timeline)) return(wt)
  if (!preset) return(wt)
  if (!is.null(opt1$options) && !is.null(opt1$timeline))
    return(wt)    # both set manually

  if (is.null(tl.series) && 
      !is.null(opt1$timeline) && 
      !is.null(series.param))
    tl.series <- series.param
  
  if (is.null(df) || !is.grouped_df(df))
    stop('ec.init: timeline requires a grouped data.frame df')

  if (is.null(tl.series$encode)) {
    tl.series$encode <- list(x=1, y=2)  # set default for non-map series
    if ('3D' %in% load) tl.series$encode$z <- 3
  }
  # add missing defaults
  if (is.null(tl.series$type)) tl.series$type <- 'scatter'
  
  steps <- c()
  tmp <- xyNamesCS(tl.series)
  xtem <- tmp$x; ytem <- tmp$y
  if (!is.null(tmp$c)) tl.series$coordinateSystem <- tmp$c
  #if (dbg) cat('\ntl=',tmp$x,' ',tmp$y,' ',tmp$c)
  
  if (any(c('geo','leaflet') %in% tl.series$coordinateSystem)) {
      klo <- 'lng'; kla <- 'lat'
      if (!is.null(tl.series$encode)) {
        klo <- unlist(tl.series$encode[klo]);
        kla <- unlist(tl.series$encode[kla]);
        if (is.numeric(klo)) klo <- colnames(df)[[klo]] 
        if (is.numeric(kla)) kla <- colnames(df)[[kla]] 
      }
      if (all(c(klo,kla) %in% colnames(df))) {
        center <- c(mean(unlist(df[,klo])),
                    mean(unlist(df[,kla])))
        if (tl.series$coordinateSystem=='geo')
          wt$x$opts$geo$center <- center
        if (tl.series$coordinateSystem=='leaflet') 
          wt$x$opts$leaflet$center <- center
      }
  }
  
  if (tl.series$type == 'map') {
    xtem <- 'name'; ytem <- 'value'
    di <- 0
    optl <- lapply(df |> group_split(), \(gp) {
      di <<- di+1
      steps <<- c(steps, unique(unlist(lapply(gp[grnm], as.character))))
      series <- list(list(type= 'map', geoIndex= 1, datasetIndex= di +1))
      tmp <- list(series= series)
      if (!is.null(opt1$title$text) && grepl('%@', opt1$title$text))
        tmp$title= list(text= sub('%@', as.character(unique(gp[grnm])), opt1$title$text) )
      tmp <- .renumber(tmp)
    })
  } 
  else {
    if (is.null(unlist(tl.series$encode[xtem]))) {
      # append col XcolX 1:max for each group
      df <- df |> group_modify(~ { .x |> mutate(XcolX = 1:nrow(.)) })
      tl.series$encode[xtem] <- 'XcolX'    # instead of relocate(XcolX)
      # replace only source, transforms stay
      wt$x$opts$dataset[[1]] <- list(source= ec.data(df, header=TRUE))
    }
    stopifnot("timeline: bad second parameter name for encode"= !is.null(unlist(tl.series$encode[ytem])))
    
    # dataset is already in, now loop group column(s)
    #gvar <- df |> group_vars() |> first() |> as.character()  # convert if factor
    di <- 0
    optl <- lapply(df |> group_split(), \(gp) {
      di <<- di+1
      steps <<- c(steps, unique(unlist(lapply(gp[grnm], as.character))))
      # nicer looking lines with sorted X 
      #if (!is.null(xcol)) gp <- gp |> arrange(across(all_of(xcol)))
      
      # multiple series for each Y, like y=c('col1', 'col3')
      series <- lapply(unlist(tl.series$encode[ytem], use.names=FALSE), 
        \(sname) {
          append(list(datasetIndex= di +1), tl.series)  # , name= sname
      })
      
      tmp <- list(series= unname(series))
      if (!is.null(opt1$title$text) && grepl('%@', opt1$title$text))
        tmp$title= list(text= sub('%@', as.character(unique(gp[grnm])), opt1$title$text) )
      tmp <- .renumber(tmp)
    })
  }
  
  #wt$x$opts$xAxis <- list(type='category')  # geo,leaf do not like
  wt$x$opts$series <- NULL   # otherwise legend + scatter series may stay behind
  wt$x$opts$options <- .merlis(optl, wt$x$opts$options)
  
  if (!is.null(tl.series$groupBy)) {
    stopifnot('ec.init: timeline `groupBy` column missing in df'= tl.series$groupBy %in% colnames(df))
    #gvar <- df |> group_vars() |> first() |> as.character()  # convert if factor
    tgrp <- tl.series$groupBy
    # define additional filter transforms and option series based on groupBy
    dsf <- list()  # new filters
    optm <- list() 
    filterIdx <- 0
    for (ii in 1:length(unlist(unique(df[grnm]))) ) {
      snames <- c()
      for (x2 in unlist(unique(df[tgrp]), use.names=FALSE) ) {
        dst <- opt1$dataset[[ii+1]]  # skip source-dataset 1st
        dst$transform$config <- list(and= list(
          dst$transform$config,
          list(dimension= tgrp, `=`= x2)
        ))
        dsf <- append(dsf, list(dst))
        snames <- c(snames, x2)
      }
      ooo <- wt$x$opts$options[[ii]]
      sss <- lapply(snames, \(s) {
        tmp <- ooo$series[[1]]
        tmp$name <- s
        filterIdx <<- filterIdx + 1
        tmp$datasetIndex <- filterIdx   # wont be decremented
        tmp$groupBy <- NULL
        tmp
      })
      tmp <- list(title= ooo$title, series= sss)
      optm <- append(optm, list(tmp))
    }
    wt$x$opts$dataset <- append(wt$x$opts$dataset[1], dsf)   # keep source-dataset [1]
    wt$x$opts$options <- optm
    wt$x$opts$legend <- .merlis(wt$x$opts$legend, list(show=TRUE))  # needed for sub-group
  }
  
  if ('timeline' %in% names(opt1)) {
    if (is.null(opt1$timeline$data))
       wt$x$opts$timeline <- .merlis(wt$x$opts$timeline, list(data= steps))
    if (is.null(opt1$timeline$axisType))
       wt$x$opts$timeline <- .merlis(wt$x$opts$timeline, list(axisType='category'))
  } else
    wt$x$opts$timeline <- .merlis(wt$x$opts$timeline, list(data=steps, axisType='category'))
  
  return(wt)
}


#' Update option lists
#' 
#' Chain commands after ec.init to add or update chart items
#' 
#' @param wt An echarty widget
#' @param ... R commands to add/update chart option lists
#'
#' @details \emph{ec.upd} makes changes to a chart already set by [ec.init].\cr
#' It should be always piped(chained) after [ec.init].\cr
#' All numerical indexes for series,visualMap,etc. are JS-counted starting at 0.\cr
#' @examples
#' library(dplyr)
#' df <- data.frame(x= 1:30, y= runif(30, 5, 10), cat= sample(LETTERS[1:3],size=30,replace=TRUE)) |>
#'   		mutate(lwr= y-runif(30, 1, 3), upr= y+runif(30, 2, 4))
#' band.df <- df  |> group_by(cat) |> group_split()
#' 
#' df |> group_by(cat) |> 
#' ec.init(load='custom', ctype='line', 
#'         xAxis=list(data=c(0,unique(df$x)), boundaryGap=FALSE) ) |> 
#' ec.upd({
#'   for(ii in 1:length(band.df))   # add bands to their respective groups
#'     series <- append(series,   
#'       ecr.band(band.df[[ii]], 'lwr', 'upr', type='stack', smooth=FALSE,
#'          name= unique(band.df[[ii]]$cat), areaStyle= list(color=c('blue','green','yellow')[ii])) )
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
#'  \item 'polygon' - by drawing a polygon as polyline from upper/lower points (default)
#'  \item 'stack' - by two \href{https://echarts.apache.org/en/option.html#series-line.stack}{stacked lines}
#' }
#' @param ... More attributes for \href{https://echarts.apache.org/en/option.html#series-line.type}{serie}
#' @return A list of **one serie** when type='polygon', or list of **two series** when type='stack'
#'
#' @details
#' \itemize{
#' \item type='polygon': coordinates of the two boundaries are chained into one polygon.\cr
#' \verb{     }_xAxis type_ could be 'category' or 'value'.\cr
#' \verb{     }Set fill color with attribute _color_.
#' \item type='stack': two _stacked_ lines are drawn, the lower with customizable areaStyle.\cr
#' \verb{     }_xAxis type_ should be 'category' ! \cr
#' \verb{     }Set fill color with attribute _areaStyle$color_.\cr
#' \verb{     }Optional tooltip formatter available in _band\[\[1\]\]$tipFmt_.
#' }
#' Optional parameter _name_, if given, will show up in legend. Legend merges all series with same name into one item.
#' 
#' @examples 
#' set.seed(222)
#' df <- data.frame( x = 1:10, y = round(runif(10, 5, 10),2)) |>
#'   dplyr::mutate(lwr= round(y-runif(10, 1, 3),2), upr= round(y+runif(10, 2, 4),2) )
#' banda <- ecr.band(df, 'lwr', 'upr', type='stack', name='stak', areaStyle= list(color='green'))
#' #banda <- ecr.band(df, 'lwr', 'upr', type='polygon', name='poly1')
#' 
#' df |> ec.init( load='custom', # polygon only
#'   legend= list(show= TRUE),
#'   xAxis= list(type='category', boundaryGap=FALSE), # stack
#'   #xAxis= list(scale=T, min='dataMin'),            # polygon 
#'   series= append(
#'     list(list(type='line', color='blue', name='line1')),
#'     banda
#'   ),
#'   tooltip= list(trigger='axis', formatter= banda[[1]]$tipFmt)
#' )
#' 
#' @importFrom stats na.omit
#' @export
ecr.band <- function(df=NULL, lower=NULL, upper=NULL, type='polygon', ...) {
  if (is.null(df) || is.null(lower) || is.null(upper)) 
    stop("ecr.band: df,lower,upper are required args", call. = FALSE)
  stopifnot("ecr.band: df must be a data.frame"= inherits(df, 'data.frame'))
  fstc <- colnames(df)[1]   # first column name
  stopifnot("ecr.band: df first column is lower or upper"= !fstc %in% c('lower','upper'))
  if (!is.numeric(df[lower][[1]]) || !is.numeric(df[upper][[1]]))
    stop("ecr.band: lower and upper must be numeric", call. = FALSE)
  df <- na.omit(df)
  
  if (type=='stack') {
    tipFmt <- "(ss) => { lo=''; hi=''; lin='';
ss.map(o => { nn = o.dimensionNames[1]; vv= o.value[1];
if (nn==='.s.lo') lo= vv; 
else if (nn==='.s.hi') hi= vv;
else lin= '<br>line <b>'+vv+'</b>'; });
str='high <b>'+(lo+hi)+'</b>'+lin+'<br>low <b>'+lo+'</b>'; return str;}"  # stack only
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
                      hi = df[upper][[1]] - df[lower][[1]], # for stacked line
                      ttip = df[upper][[1]] )
    slow$data <- ec.data(tmp[,c('x','lo')])
    supr$data <- ec.data(tmp[,c('x','hi','ttip')])
    supr$dimensions <- c('x','.s.hi','.s.tip')
    slow$tipFmt <- ec.clmn(tipFmt)    # simple optional tooltip 
    slow$dimensions <- c('x','.s.lo')
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
    serios$tipFmt <- NULL
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
#' Horizontal and vertical layouts supported, just switch _encode_ values _x_ and _y_ for both for series and ecr.ebars.\cr
#' Have own default tooltip format showing _value, high & low_.\cr
#' Grouped bar series are supported.\cr
#' Non-grouped series could be shown with formatter _riErrBarSimple_ instead of _ecr.ebars_. This is limited to vertical only, see example below.\cr
#' Other limitations:\cr
#' \verb{     }manually add axis type='category' when needed\cr
#' \verb{     }error bars cannot have own name when data is grouped\cr
#' \verb{     }legend select/deselect will not re-position grouped error bars\cr
#' 
#' @examples
#' library(dplyr)
#' df <- mtcars |> group_by(cyl,gear) |> summarise(avg.mpg= round(mean(mpg),2)) |>
#'   mutate(low = round(avg.mpg-cyl*runif(1),2), 
#'          high= round(avg.mpg+cyl*runif(1),2))
#' ec.init(df, load= 'custom', ctype= 'bar',
#'       xAxis= list(type='category'), tooltip= list(show=TRUE)) |>
#' ecr.ebars(encode= list(y=c('avg.mpg','low','high'), x='gear'))
#' #ecr.ebars(encode= list(y=c(3,4,5), x=2))  # ok: data indexes
#'
#' # same but horizontal
#' ec.init(df, load= 'custom',
#'   yAxis= list(type='category'), tooltip= list(show=TRUE),
#'   series.param= list(type='bar', encode= list(x='avg.mpg', y='gear') )) |>
#' ecr.ebars(encode= list(x=c('avg.mpg','low','high'), y='gear'))
#' 
#' # ----- riErrBarSimple ------
#' df <- mtcars |> mutate(name= row.names(mtcars), hi= hp-drat*3, lo= hp+wt*3) |> 
#'   filter(cyl==4) |> select(name,hp,hi,lo)
#' ec.init(df, load= 'custom', legend= list(show=TRUE)) |>
#' ec.upd({ series <- append(series, list(
#'   list(type= 'custom', name= 'error',
#'     data= ec.data(df |> select(name,hi,lo)),
#'     renderItem= htmlwidgets::JS('riErrBarSimple')
#'   )))
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
              # TODO: series.seriesLayoutBy 
              out <- which(ds$source[[1]] %in% out)
            else {
              if (!class(ds$source[[1]]) %in% class(ds$source[[2]]))
                out <- which(ds$source[[1]] %in% out)
              else
                stop('could not find names from encode', call.=FALSE)
            }
          }
        }
      } 
      else
        out <- which(liss$dm %in% out)  # from data
    }
    out
  }
  # assuming all attached series from same dataset
  encode$y <- enc2num(encode$y, tmp[[1]])
  encode$x <- enc2num(encode$x, tmp[[1]])
  
  rim <- if (!is.null(args$renderItem)) args$renderItem else 'riErrBars'
  decds <- ifelse(length(tmp)>1, 0, 1)   # single or grouped
  oneSerie <- function(liss, ...) {
    cc <- list(type='custom', datasetIndex= liss$ds-decds, encode= encode,
    			 renderItem= htmlwidgets::JS(rim), ...)
	  if (is.null(cc$name)) cc$name <- liss$nm
	  if (!is.null(liss$dd)) cc$data <- liss$dd
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
  htmlwidgets::shinyWidgetOutput(outputId, 'echarty', width, height) #, package= 'echarty')
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
  # if (!endsWith(fname, '.js')) stop('ec.plugjs expecting .js suffix', call. = FALSE)
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
    if (ans && !exists('ec.webR')) {  # WebR dislikes download.file
      #try(withCallingHandlers(    # function(w) { ans <- FALSE }
      errw <- function(w) { ans <- FALSE
        cat('ec.plugjs:', sub(".+HTTP status was ", "", w, source)) }
      tryCatch({
        download.file(source, ffull, quiet=TRUE) }, # method = "libcurl"),
        error = errw, warning = errw
      )
    } 
    if (!ans) return(wt)    # error
  }
  dep <- htmltools::htmlDependency(
    name= fname, version= '1.1.1', src= c(file = path),
    script= fname
  )
  wt$dependencies <- append(wt$dependencies, list(dep))
  return(wt)
}

# called by widget init
# .preRender <- function(wt) {
#   wt
# }

# convert from R to JS numbering
.renumber <- function(opa) {

  r2jsEncode <- function(ss) {
  
    if (any(names(ss)=='encode')) {
      for(i in 1:length(ss$encode)) {
        if (!is.numeric(ss$encode[[i]])) next
          ss$encode[[i]] <- ss$encode[[i]] -1
      }
    }
    if (!ss$type %in% noAxis) {
      if (!is.null(ss$xAxisIndex)) ss$xAxisIndex <- ss$xAxisIndex -1
      if (!is.null(ss$yAxisIndex)) ss$yAxisIndex <- ss$yAxisIndex -1
    }
    if (!is.null(ss$datasetIndex)) ss$datasetIndex <- ss$datasetIndex -1
    if (!is.null(ss$geoIndex)) ss$geoIndex <- ss$geoIndex -1
    if (!is.null(ss$polarIndex)) ss$polarIndex <- ss$polarIndex -1
    if (!is.null(ss$calendarIndex)) ss$calendarIndex <- ss$calendarIndex -1
    if (!is.null(ss$radarIndex)) ss$radarIndex <- ss$radarIndex -1
    ss
  }
  
  if (!is.null(opa$series))
    opa$series <- lapply(opa$series, r2jsEncode)
  
  decro <- function(x) {
    if (!is.null(x$dimension) && is.numeric(x$dimension)) x$dimension <- x$dimension -1
    if (!is.null(x$seriesIndex)) x$seriesIndex <- x$seriesIndex -1   # vMap
    if (!is.null(x$gridIndex)) x$gridIndex <- x$gridIndex -1  # x/y Axis
    x
  }
  decType <- \(typ) {   # handle single or multiple items
    item <- opa[[typ]]
    if (!is.null(item)) {
      if (all(sapply(item, is.list)))
        opa[[typ]] <<- lapply(item, decro)
      else
        opa[[typ]] <<- decro(item)
    }
  }
  decType('xAxis')
  decType('yAxis')
  decType('visualMap')
  opa
}

# merge named lists: list OR list.of.lists like series
.merlis <- function(l1, l2) {
	if (!inherits(l1, 'list')) l2
  else if (length(l1)==0) l2
	else if (!inherits(l2, 'list')) l1
  else if (length(l2)==0) l1
	#else if (inherits(l1[[1]], 'list'))     # list of lists
	else if (inherits(l1[1],'list') && is.null(names(l1[1])))     # list of lists
		lapply(l1, \(x) {
			c(x, l2)[!duplicated(c(names(x), names(l2)), fromLast= TRUE)]
		})
	else
		c(l1, l2)[!duplicated(c(names(l1), names(l2)), fromLast= TRUE)]
}

# manage colnames for ec.clmn (not used)
.getColnm <- function() { the$.ecv.colnames }
.setColnm <- function(vv=NULL) {
  old <- the$.ecv.colnames
  the$.ecv.colnames <- vv
  invisible(old)
}

#  ------------- Global Options -----------------
#' 
#' For info on options and prefixes, see [-- Introduction --].


#  ------------- Licence -----------------
#'
#' Original work Copyright 2018 John Coene
#' 
#' Modified work Copyright 2021-2024 Larry Helgason
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
