
#' Utility functions
#' 
#' tabset, table layout, support for GIS shapefiles through library sf
#'  
#' @param cmd utility command, see Details\cr
#' @param js optional JavaScript function, default is NULL.\cr
#' @param ... Optional parameters for the command \cr
#'      for \emph{sf.series} - see \href{https://echarts.apache.org/en/option.html#series-scatter.type}{points}, \href{https://echarts.apache.org/en/option.html#series-lines.type}{polylines}, polygons(itemStyle).\cr
#'      for \emph{tabset} parameters should be in format \emph{name1=chart1, name2=chart2}, see example\cr
#' @details 
#' **cmd = 'sf.series'**\cr
#' \verb{   }Build _leaflet_ or \href{https://echarts.apache.org/en/option.html#geo.map}{geo} map series from shapefiles.\cr
#' \verb{   }Supported types: POINT, MULTIPOINT, LINESTRING, MULTILINESTRING, POLYGON, MULTIPOLYGON \cr
#' \verb{   }Coordinate system is _leaflet_(default), _geo_ or _cartesian3D_ (for POINT(xyz))\cr
#' \verb{   }Limitations:\cr 
#' \verb{     }polygons can have only their name in tooltip,  \cr
#' \verb{     }assumes Geodetic CRS is WGS 84, use \link[sf]{st_transform} with _crs=4326_ to convert.\cr
#' \verb{   }parameter _df_ - value from \link[sf]{st_read}\cr
#' \verb{   }optional parameters: \cr
#' \verb{     }cs - _coordinateSystem_ value\cr
#' \verb{     }nid - column name for name-id used in tooltips\cr
#' \verb{     }verbose - print shapefile item names in console\cr
#' \verb{   }returns a list of chart series\cr
#' **cmd = 'sf.bbox'**\cr
#' \verb{   }returns JavaScript code to position a map inside a bounding box from \link[sf]{st_bbox}, for leaflet only.\cr
#' **cmd = 'sf.unzip'**\cr
#' \verb{   }unzips a remote file and returns local file name of the unzipped .shp file\cr
#' \verb{   }url - URL of remote zipped shapefile\cr
#' \verb{   }optional \emph{shp} - name of .shp file inside ZIP file if multiple exist. Do not add file extension. \cr
#' **cmd = 'geojson'** \cr
#' \verb{   }custom series list with geoJson objects\cr
#' \verb{   }geojson - object from \link[jsonlite]{fromJSON}\cr
#' \verb{   }... - optional custom series attributes like _itemStyle_\cr
#' \verb{   }optional parameters: \cr
#' \verb{     }ppfill - fill color like '#F00', OR NULL for no-fill, for all Points and Polygons\cr
#' \verb{     }nid - property name for name-id used in tooltips\cr
#' \verb{   }optional geoJSON _feature properties_ are: color, ppfill, lwidth(for lines), radius(for points)
#' **cmd = 'layout'** \cr
#' \verb{   }multiple charts in table-like rows/columns format\cr
#' \verb{   }... - List of charts\cr
#' \verb{   }optional parameters: \cr
#' \verb{     }title - Title for the set, rows= Number of rows, cols= Number of columns,\cr
#' \verb{     }width - Width of columns (one of xs, md, lg)\cr
#' \verb{   }returns a container \link[htmltools]{div} in rmarkdown, otherwise \link[htmltools]{browsable}.\cr
#' \verb{   }For 3-4 charts one would use multiple series within a \href{https://echarts.apache.org/en/option.html#grid}{grid}. \cr
#' \verb{   }For greater number of charts _ec.util(cmd='layout')_ comes in handy\cr
#' **cmd = 'tabset'** \cr
#' \verb{   }... - a list name/chart pairs like \emph{n1=chart1, n2=chart2}, each tab may contain a chart.\cr
#' \verb{   }optional parameters are: \cr
#' \verb{     }width - Width of tabs in pixels, height= Height of tabs in pixels\cr
#' \verb{     }tabStyle - tab style string, see default \emph{tabStyle} variable in the code\cr
#' \verb{   }returns A) \link[htmltools]{tagList} of tabs when in a pipe without '...' params, see example\cr
#' \verb{   }returns B) \link[htmltools]{browsable} when '...' params are provided by user\cr
#' **cmd = 'morph'** \cr
#' \verb{   }... - a list of charts or chart options\cr
#' \verb{   }optional parameter: \cr
#' \verb{     }js - JS function for switching charts. Default function is on \emph{mouseover}.\cr
#' \verb{   }returns a chart with ability to morph into other charts\cr
#' **cmd = 'fullscreen'** \cr
#' \verb{   }a toolbox feature to toggle fullscreen on/off. Works in a browser, not in RStudio.\cr
#' **cmd = 'rescale'** \cr
#' \verb{   }t - target range c(min,max), numeric vector of two\cr
#' \verb{   }v - vector of numeric values to rescale\cr
#' **cmd = 'level'** \cr
#' \verb{   }calculate vertical levels for timeline \emph{line} charts, returns a numeric vector\cr
#' \verb{   }df - data.frame with from & to columns\cr
#' \verb{   }from - name of 'from' column\cr
#' \verb{   }to - name of 'to' column\cr
#' **cmd = 'button'** \cr
#' \verb{   }UI button to execute a JS function, returns a \href{https://echarts.apache.org/en/option.html#graphic.elements-text}{graphic.elements-text}\cr
#' \verb{   }text - the button label, use _js_ for the JS function string \cr
#'
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
#'   p1 <- cars |> ec.init(grid= list(top= 20))  # move chart up
#'   p2 <- mtcars |> ec.init()
#'   ec.util(cmd= 'tabset', cars= p1, mtcars= p2, width= 333, height= 333)
#' 
#'   lapply(list('dark','macarons','gray','jazz','dark-mushroom'),
#'                 \(x) cars |> ec.init() |> ec.theme(x) ) |>
#'   ec.util(cmd='layout', cols= 2, title= 'my layout')
#'   
#'   setd <- \(type) {
#'     mtcars |> group_by(cyl) |> ec.init(ctype=type) |> ec.upd({
#'     title <- list(subtext='mouseover points to morph')
#'     xAxis <- list(scale=TRUE)
#'     series <- lapply(series, \(ss) {
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
  
  do.opties <- \(names, dflts=NULL) {
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
      do.series <- \(dff) {
        polig <- \(geom) {
          for(k in 1:length(geom)) {
            if ('matrix' %in% class(geom[[k]])) {
              gm <- as.data.frame(geom[[k]])
              coords <- list()
              for(j in 1:nrow(gm))
                coords <- append(coords, list(c(gm[j,1], gm[j,2])))
              sers <<- append(sers, list( c(
                list(
                  type= 'custom', coordinateSystem= cs, 
                  renderItem= htmlwidgets::JS('riPolygon'),
                  name= dname, 
                  data= coords), 
                opts) #...
              ))
            } else polig(geom[[k]])  # recursive
          }
        }
        geometry <- L1 <- cmd <- NULL  # trick to avoid code checking NOTES
        sers <- list()
        switch( class(dff$geometry)[1],
          'sfc_MULTIPOINT' =,
          'sfc_POINT'= {
            dff <- dff |> rename(value= geometry)
            tt <- NULL
            flds <- colnames(dff)[! colnames(dff) %in% c("value")]
            if (length(flds)>0) {
              if (length(flds)>10) flds <- flds[1:10]
              tt <- c(paste(rep('%@', length(flds)), collapse='<br>'), flds)
            }
            pnts <- ec.data(dff, 'names')
            sers <- list( c(
              list(type= 'scatter', coordinateSystem= cs, data= pnts), opts))
            if (!is.null(tt)) 
              sers[[1]]$tooltip= list(formatter= do.call("ec.clmn", as.list(tt)))
          },
          'sfc_POLYGON' =,
          'sfc_MULTIPOLYGON' = {
            for(i in 1:nrow(dff)) {
              dname <- i
              if (!is.null(opts$nid) && opts$nid %in% colnames(dff)) 
                dname <- dff[i, opts$nid][[1]]
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
              dname <- ifelse(is.null(opts$nid), i, dff[i, opts$nid][[1]])
              if (verbose) cat(dname,',', sep='')
              coords <- list()
              geom <- tmp |> filter(L1==i)
              for(k in 1:nrow(geom))
                coords <- append(coords, list(c(geom[k,1], geom[k,2])))
              
              sers <- append(sers, list( c(
                list( type='lines', coordinateSystem= cs, polyline= TRUE,
                      name= dname, tooltip= list(formatter= '{a}'), 
                      data= list(coords)),
                opts) ))
            }
          },
          'sfc_MULTILINESTRING' = {
            for(i in 1:nrow(dff)) {
              dname <- ifelse(is.null(opts$nid), i, dff[i,opts$nid][[1]])
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
              sers <- append(sers, list( c(
                list( type='lines', coordinateSystem= cs, polyline= TRUE,
                      name= dname, tooltip= list(formatter= '{a}'),
                      data= corda),
                opts) ))
            }
          },
          stop(paste('ec.util:',class(dff$geometry)[1],'geometry not supported'), call.= FALSE)
        )
        cnt <- length(sers)
        recs <- sum(unlist(lapply(sers, \(x) {
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
      cs <- verbose <- NULL   # CRAN check fix
      do.opties(c('cs','verbose'), list('leaflet', FALSE))
      tmp <- opts$df
      opts$df <- opts$cs <- NULL
      out <- do.series(tmp)
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
    
    'geojson'= {
      geojson <- opts$geojson
      opts$geojson <- NULL
      cat('\ngeoJSON has',nrow(geojson$features),'features')
      myGeojson <- toString(jsonlite::toJSON(geojson))
      dfill <- ''
      if ('ppfill' %in% names(opts)) {
        dfill <- ifelse(is.null(opts$ppfill), 'null', paste0('"',opts$ppfill,'"'))
        dfill <- paste0('ecfun.geofill=',dfill,';')
      }
      out <- c( list(type= 'custom',
        coordinateSystem= 'leaflet',
        # set JS variables for riGeoJson() to work with
        renderItem= htmlwidgets::JS(paste("(params, api) => {",dfill,
          " ecfun.geojson=",myGeojson,"; return riGeoJson(params, api); }")),
        data= if (is.null(opts$nid)) lapply(1:nrow(geojson$features), list)
              else lapply(unlist(tmp$features$properties[opts$nid],
                                 use.names=FALSE), \(n) { list(name=n)})
      ), opts)
    },
    
    'tabset'= {
      width <- height <- tabStyle <- NULL   # CRAN check fix
      do.opties(c('width','height','tabStyle'), 
                list('100%', 400, "<style>
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
</style>")
      )
      
      tnames <- names(opts)
      isPipe <- FALSE
      if ((is.null(tnames) || length(tnames)==1) && 
          ('echarty' %in% class(opts[[1]][[1]]))) {  # pipe
        opts <- opts[[1]]
        cnt <- 1
        tnames <- names(opts) <- lapply(opts, \(oo) {
      		tit <- oo$x$opts$title$text
      		if (is.null(tit) || grepl(' ',tit)) {
      			tit <- paste('chart', cnt); cnt <<- cnt + 1 }
      		tit
        })
        #tnames <- names(opts) <- paste0('chart', 1:length(opts))
        isPipe <- TRUE
      }
      
      tpans <- htmltools::tags$div(class='tab-panels')
      tset <- htmltools::tags$div(class='tabset', id='ec_tabset')
      cnt <- 1
      for(n in tnames) {
        tid <- paste0('tab', cnt)
        tinp <- htmltools::tags$input(type='radio', id=tid,
            name='tabso', `aria-controls`=n, onclick=paste0('trsz(',cnt-1,')') )
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
      
      # resize on tab click
      tmp <- "function trsz(i) { var ecs= document.getElementsByClassName('echarty'); 
         ecs[i].htmlwidget_data_init_result.resize(); }"
      out <- htmltools::tagAppendChild(out,
        htmltools::tags$script(tmp))
      if (!isPipe)
        out <- htmltools::browsable(out)
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
      
      opts <- lapply(opts, \(oo) {
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
                               onclick= htmlwidgets::JS('function(){
                tmp = this.ecModel.getOption();
                ecfun.fscreen(tmp.hwid); 
            }')
      ))
    },
    
    'level'= {
      from <- to <- NULL
      do.opties(c('from','to'), list('from','to'))
      if (is.null(opts$df) || !is.data.frame(opts$df))
        stop("ec.util-level: 'df' paramater missing or invalid", call. = FALSE)
      if (!all(c(from,to) %in% colnames(opts$df)))
        stop(paste('ec.util-level: df has no "',from,' ',to,'" columns'), call. = FALSE)
      # build levels
      df <- opts$df
      level <- rep(1, nrow(df))  # bottom start
      rr <- rep(0, nrow(df))
      for(i in 1:nrow(df)){
        ifrom <- unlist(df[,from])[i]
        ito <- unlist(df[,to])[i]
        for(j in 1:min(which(rr==0))) {
          if (ifrom > rr[j]) {
            rr[j] <- ito
            level[i] <- j
            break
          }
        }
      }
      out <- level
    },
    
    'button' = {
      text <- NULL   # CRAN check fix
      do.opties(c('text'), list('back')) 
      # h & w for default font
      tmp <- sum(charToRaw(text) == charToRaw('\n'))
      h <- 20 * if (tmp==0) 1 else tmp
      w <- if (h>20) max(sapply(unlist(strsplit(text, '\n')),nchar)) else nchar(text)
      w <- w * 10
      tmp <- list( 
          type= 'rect', right= 40, top= 20,
      		shape= list(height=h, width=w, r=5),
          style= list(fill= 'lightgray'),
          textContent= list(style= list(text= text, fill= 'black')),
          textConfig= list(position= 'inside'),
      		onclick= htmlwidgets::JS(js)
      )
      tt <- list(...)   # user values overwrite defaults
      #out <- c(tmp, tt)[!duplicated(c(names(tmp), names(tt)), fromLast=T)]
      out <- .merlis(tmp, tt)
    },

    stop(paste("ec.util: invalid 'cmd' parameter",cmd), call. = FALSE)
  )
  out
}

#' Data helper
#' 
#' Make data lists from a data.frame
#' 
#' @param df Chart data in data.frame format, required. \cr
#'     Except when format is 'dendrogram', then df is a list, result of \link[stats]{hclust} function.\cr
#' @param format A key on how to format the output list \cr \itemize{
#'  \item 'dataset' = list to be used in \href{https://echarts.apache.org/en/option.html#dataset.source}{dataset} (default), or in \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} (without header). \cr
#'  \item 'values' = list for customized \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} \cr
#'  \item 'names' = named lists useful for named data like \href{https://echarts.apache.org/en/option.html#series-sankey.links}{sankey links}.
#'  \item 'dendrogram' = build series data for Hierarchical Clustering dendrogram
#'  \item 'treePC' = build series data for sunburst,tree,treemap from parent/children data.frame
#'  \item 'treeTK' = build series data for sunburst,tree,treemap from data.frame like Titanic. Supports column \emph{itemStyle}.
#'  \item 'boxplot' = build dataset and source lists, see Details
#' }
#' @param header for dataset, to include the column names or not, default TRUE. Set it to FALSE for \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data}.\cr
#' @param layout for boxplot, 'h' for horizontal(default) or 'v' for vertical layout\cr
#' @param jitter for boxplot, value for \link[base]{jitter} of numerical values in second column, default 0 (no scatter). Adds scatter series on top of boxplot.\cr
#' @param ... for boxplot scatter, additional parameters for the _scatter_ (jitter) serie\cr
#' @return A list for _dataset.source_, _series.data_ or other lists:\cr
#'   For boxplot - a named list, see Details and Examples \cr
#'   For dendrogram & treePC - a tree structure, see format in \href{https://echarts.apache.org/en/option.html#series-tree.data}{tree data}
#' @details `format='boxplot'` requires the first two _df_ columns as: \cr
#'   * column for the non-computational categorical axis\cr
#'   * column with (numeric) data to compute the five boxplot values\cr
#'   
#'  Additional grouping is supported on a column after the second. Groups will show in the legend, if enabled.\cr
#'  Returns a `list(dataset, series, xAxis, yAxis)` to set params in [ec.init].\cr
#'  Make sure there is enough data for computation, 4+ values per boxplot.
#' @seealso some live \href{https://rpubs.com/echarty/data-models}{code samples}
#' 
#' @examples
#' library(dplyr)
#' ds <- iris |> relocate(Species) |>
#' 	 ec.data(format= 'boxplot', jitter= 0.1, layout= 'v')
#' ec.init(
#'   dataset= ds$dataset, series= ds$series, xAxis= ds$xAxis, yAxis= ds$yAxis,
#'   legend= list(show= TRUE), tooltip= list(show= TRUE)
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
ec.data <- function(df, format='dataset', header=FALSE, jitter=0, layout='h', ...) {
  if (missing(df))
    stop('ec.data: expecting parameter df', call. = FALSE)
  if (format=='dendrogram') { 
    if (!inherits(df, 'hclust'))
      stop('ec.data: df should be hclust for dendrogram', call. = FALSE)
    hc <- df
    # decode hc$merge with hc$labels
    inew <- list()
    i <- 0
    tmp <- apply(hc$merge, 1, \(x) {
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
    error= function(e) { stop(e) })
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
      lest$children <- lapply(cldrn, \(x) {
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
  tmp <- lapply(n, \(i) lapply(df, "[[", i))  # preserve column types
  
  if (format=='dataset') {
    datset <- lapply(tmp, unname)
    if (header)
      datset <- c(list(colnames(df)), datset)
    
  } 
  else if (format=='values' || isTRUE(format)) {
    datset <- lapply(tmp, \(x) list(value=unlist(unname(x))))
    
  } 
  else if (format=='boxplot') {
    cn <- colnames(df)
    if (length(cn)<2) stop('boxplot: df should have 2+ columns', call.=FALSE)
    colas <- cn[1]
    colb5 <- cn[2]
    if (!is.numeric(df[[colb5]])) stop('boxplot: 2nd column must be numeric', call.=FALSE)
    # is there another group beside colas ?
    grpcol <- if (is.grouped_df(df) && group_vars(df)[1]!=colas) 
      group_vars(df)[1] else NULL
    yaxis <- list(list(type= 'category', name=colas))   # default horiz layout
    xaxis <- list(list(scale= TRUE, name=colb5))
    ttcol <- 1
    # category axis labels
    if (is.factor(df[[colas]]))
        axe <- paste(levels(df[[colas]]), collapse="','")
    else
		    axe <- paste(sort(as.character(unique(df[[colas]]))), collapse="','")
    tbox <- ec.clmn('min %@<br>Q1 %@<br>median %@<br>Q3 %@<br>max %@',2,3,4,5,6) 
    
    if (!is.null(grpcol)) {   # grouped
      tmp <- df |> group_split()
      dataset <- lapply(tmp, \(dd) { 
        dv <- dd |> arrange(.data[[colas]]) |> dplyr::group_by(.data[[colas]]) |> group_split()
        src <- lapply(dv, \(vv) {
      	 if (nrow(vv)<5) stop(paste0('ec.data: insufficient data in group "',grpcol,'"'), call.= FALSE)
      	 vv[[colb5]]
        })
        list(source= if (length(src[[1]])==1) list(src) else src)
      })
      series <- list()
      for (i in 1:length(tmp)) { 
        dataset <- append(dataset, 
        	 list(list( fromDatasetIndex= i-1, transform= list(type= 'boxplot')))) 
        series <- append(series, list(list(
          name= tmp[[i]][[grpcol]][1], tooltip= list(formatter=tbox), 
          type= 'boxplot', datasetIndex= i+length(tmp)-1) ))
      }
	    axe <- paste0("function(v) { return ['",axe,"'][v]; }")
    } 
    else {  # non-grouped
    	
      bdf <- ungroup(df) |> arrange(.data[[colas]]) |> dplyr::group_by(across({colas})) |> group_split()
      dats <- lapply(bdf, \(x) {
        c(unique(pull(x,colas)), round(boxplot.stats( pull(x,colb5) )$stats, 4))
      })
      dataset <- list(source= ec.data( as.data.frame(do.call(rbind, dats)) ))
      series <- list(list(type='boxplot', 
                    name= 'boxplot', tooltip= list(formatter=tbox),
      						  encode= list(y=1, x=c(2,3,4,5,6))))
      # default is horizontal, for vertical swap xAxis/yAxis category type
      if (layout=='v') {
        e <- series[[1]]$encode
        series[[1]]$encode <- list(x=e$y, y=e$x) # swap x & y
      }
		  axe <- paste0("function(v) { return ['",axe,"'][v-1]; }")
   }
	 if (is.factor(df[[colas]]) || is.character(df[[colas]]))
	    yaxis[[1]] <- c(yaxis[[1]], list(axisLabel= list(formatter= htmlwidgets::JS(axe))))
 	 if (layout=='v') {
      swap <- xaxis; xaxis <- yaxis; yaxis <- swap; ttcol <- 2
 	 }
    
    if (jitter>0) {
  		tmp <- df |> arrange(.data[[colas]]) |> dplyr::group_by(.data[[colas]]) |> group_split()
  		mcyl <- lapply(tmp, \(x) unname(unlist(x[[colb5]])))
  		names(mcyl) <- sort(unique(df[[colas]]))
  		i <- 0.5
  		serj <- lapply(names(mcyl), \(nn) {  
  			yy <- unname(unlist(mcyl[nn]))
  			xx <- jitter(rep(i, length(yy)), amount= jitter)
  			out <- list(type= 'scatter', ...)
  			args <- list(...)
  			if (!'name' %in% names(args)) out$name <- nn
  			if (!'large' %in% names(args)) out$large <- TRUE
  			if (!'tooltip' %in% names(args)) out$tooltip <- list(formatter= ec.clmn(ttcol))
  			if (!'z' %in% names(args)) out$z <- 10
  			if (layout=='v') {
  				out$data <- do.call(Map, c(f = c, list(xx, yy)))
  				out$xAxisIndex <- 2
  				if (i==0.5) xaxis <<- append(xaxis, 
  									list(list(max= length(mcyl), show=F)))
  			} else {
  				out$data <- do.call(Map, c(f = c, list(yy, xx)))
  				out$yAxisIndex <- 2
  				if (i==0.5) yaxis <<- append(yaxis, 
  									list(list(max= length(mcyl), show=F)))
  			}
  			i <<- i + 1
  			out
  		})
  		series <- append(series, serj)
    }
    return(list(dataset= dataset, series= series, xAxis=xaxis, yAxis=yaxis))
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
#'    'json' displays tooltip with all available values to choose from\cr 
#'    'log' will write all values in the JS console (F12)\cr
#'    starts with _'function('_ or has _') =>'_ - gets result of JS function\cr
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
#'   series <- lapply(series, \(s) append(s,
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
  remnl <- isTRUE(args$remnl)
  
  if (is.null(col)) {}   # for pie,sunburst
  else if (col=='log')
    ret <- "console.log(x); return 'logged';"
  else if (col=='json')
    ret <- 'return JSON.stringify(x, null, " ").replace(/{/g,"<br>{").replace(/"value":/g,"<br> value:").replace(/"data":/g,"<br> data:").replace(/"seriesIndex":/g,"<br> seriesIndex:");'
  else if (is.character(col) && (grepl(') =>', col) || 
                                 startsWith(col, 'function(')))
    return(htmlwidgets::JS(col))
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
      t0 <- sapply(args, \(s) toString(paste0("x.data['", s,"']")) )
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
if (vv.length > 0)
  vv = ss.map((e,idx) => {
    if (typeof vv[idx] != 'object') return vv[idx];
    f= Math.round(e % 1 *10) -1;
    return vv[idx].value[f];
  }); ")  # multi-series 1.2, 3.1
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
  if (remnl)
    ret <- gsub('\n', ' ', ret, fixed=T)
  htmlwidgets::JS(paste0('function(x) {', ret, '}'))
}

