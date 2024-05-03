# ----------- Primary utilities ----------------------

#' Utility functions
#' 
#' tabset, table layout, support for GIS shapefiles through library 'sf'
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
#' \verb{     }assumes Geodetic CRS is WGS 84, for conversion use \link[sf]{st_transform} with _crs=4326_.\cr
#' \verb{   }Parameters:\cr 
#' \verb{   }df - value from \link[sf]{st_read}\cr
#' \verb{   }cs - optional _coordinateSystem_ value\cr
#' \verb{   }nid - optional column name for name-id used in tooltips\cr
#' \verb{   }verbose - optional, print shapefile item names in console\cr
#' \verb{   }Returns a list of chart series\cr
#' **cmd = 'sf.bbox'**\cr
#' \verb{   }Returns JavaScript code to position a map inside a bounding box from \link[sf]{st_bbox}, for leaflet only.\cr
#' **cmd = 'sf.unzip'**\cr
#' \verb{   }unzips a remote file and returns local file name of the unzipped .shp file\cr
#' \verb{   }url - URL of remote zipped shapefile\cr
#' \verb{   }shp - optional name of .shp file inside ZIP file if multiple exist. Do not add file extension. \cr
#' **cmd = 'geojson'** \cr
#' \verb{   }custom series list with geoJson objects\cr
#' \verb{   }geojson - object from \link[jsonlite]{fromJSON}\cr
#' \verb{   }... - optional custom series attributes like _itemStyle_\cr
#' \verb{   }ppfill - optional fill color like '#F00', OR NULL for no-fill, for all Points and Polygons\cr
#' \verb{   }nid - optional feature property for item name used in tooltips\cr
#' \verb{   }optional geoJson _feature properties_: color, ppfill, lwidth, ldash, radius(for points)\cr
#' **cmd = 'layout'** \cr
#' \verb{   }multiple charts in table-like rows/columns format\cr
#' \verb{   }... - List of charts\cr
#' \verb{   }title - optional title for the set, rows= Number of rows, cols= Number of columns\cr
#' \verb{   }Returns a container \link[htmltools]{div} in rmarkdown, otherwise \link[htmltools]{browsable}.\cr
#' \verb{   }For 3-4 charts one would use multiple series within a \href{https://echarts.apache.org/en/option.html#grid}{grid}. \cr
#' \verb{   }For greater number of charts _ec.util(cmd='layout')_ comes in handy\cr
#' **cmd = 'tabset'** \cr
#' \verb{   }... - a list name/chart pairs like \emph{n1=chart1, n2=chart2}, each tab may contain a chart.\cr
#' \verb{   }tabStyle - tab style string, see default \emph{tabStyle} variable in the code\cr
#' \verb{   }Returns A) \link[htmltools]{tagList} of tabs when in a pipe without '...' params, see example\cr
#' \verb{   }Returns B) \link[htmltools]{browsable} when '...' params are provided by user\cr
#' **cmd = 'button'** \cr
#' \verb{   }UI button to execute a JS function,\cr
#' \verb{   }text - the button label\cr
#' \verb{   }js - the JS function string\cr
#' \verb{   }... - optional parameters for the \href{https://echarts.apache.org/en/option.html#graphic.elements-rect.type}{rect} element\cr
#' \verb{   }Returns a graphic.elements-\href{https://echarts.apache.org/en/option.html#graphic.elements-rect.type}{rect} element.\cr
#' **cmd = 'morph'** \cr
#' \verb{   }... - a list of charts or chart options\cr
#' \verb{   }js - optional JS function for switching charts. Default function is on \emph{mouseover}. Disable with FALSE.\cr
#' \verb{   }Returns a chart with ability to morph into other charts\cr
#' **cmd = 'fullscreen'** \cr
#' \verb{   }a toolbox feature to toggle fullscreen on/off. Works in a browser, not in RStudio.\cr
#' **cmd = 'rescale'** \cr
#' \verb{   }v - input vector of numeric values to rescale\cr
#' \verb{   }t - target range c(min,max), numeric vector of two\cr
#' **cmd = 'level'** \cr
#' \verb{   }calculate vertical levels for timeline \emph{line} charts, returns a numeric vector\cr
#' \verb{   }df - data.frame with _from_ and _to_ columns\cr
#' \verb{   }from - name of 'from' column\cr
#' \verb{   }to - name of 'to' column\cr
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
#' 	   mtcars |> group_by(cyl) |> 
#' 	 ec.init(ctype= type,
#' 		  title= list(subtext= 'mouseover points to morph'),
#' 		  xAxis= list(scale= TRUE))
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
            if (inherits(geom[[k]],'matrix')) {
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
                list( type='lines', polyline= TRUE, coordinateSystem= cs,
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
                list( type='lines', polyline= TRUE, coordinateSystem= cs,
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
      
      stopifnot('ec.util: expecting parameter df'= !is.null(opts$df))
      stopifnot('ec.util: expecting df$geometry'= !is.null(opts$df$geometry))
      cs <- verbose <- NULL   # CRAN check fix
      do.opties(c('cs','verbose'), list('leaflet', FALSE))
      tmp <- opts$df
      opts$df <- opts$cs <- NULL
      out <- do.series(tmp)
    },
    
    'sf.bbox'= {
      stopifnot('ec.util: expecting parameter bbox'= !is.null(opts$bbox))
      stopifnot('ec.util: expecting bbox in sf format'= !is.null(opts$bbox$ymin))
      tmp <- opts$bbox
      rng <- paste0('[[',tmp$ymin,',',tmp$xmin,'],[',tmp$ymax,',',tmp$xmax,']]')
      out <- c('','', 
               paste("var map= chart.getModel().getComponent('leaflet').__map;", 
                     "map.fitBounds(",rng,");"))
    },
    'sf.unzip'= {
      stopifnot('ec.util: expecting url of zipped shapefile'= !is.null(opts$url))
      destfile <- tempfile('shapefile')
      download.file(opts$url, destfile, mode='wb') #, method='curl')
      # get name only, use as folder name to unzip to
      fldr <- sub(pattern = "(.*)\\..*$", replacement = "\\1", basename(opts$url))
      unzip(destfile, exdir=fldr)  # new folder under getwd()
      # find name
      pat <- ifelse (is.null(opts$shp), '*.shp', paste0(opts$shp,'.shp'))
      tmp <- list.files(path= fldr, pattern= pat)
      if (length(tmp)==0) 
        stop(paste('ec.util:',pat,'file not found in folder',fldr), call. = FALSE)
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
        dfill <- paste0('ecf.geofill=',dfill,';')
      }
      out <- c( list(type= 'custom',
        coordinateSystem= 'leaflet',
        # set JS variables for riGeoJson() to work with
        renderItem= htmlwidgets::JS(paste("(params, api) => {",dfill,
          " ecf.geojson=",myGeojson,"; return riGeoJson(params, api); }")),
        data= if (is.null(opts$nid)) 
                lapply(1:nrow(geojson$features), list)
              else 
                lapply(unlist(tmp$features$properties[opts$nid],
                              use.names=FALSE), \(n) { list(name=n)})
      ), opts)
    },
    
    'tabset'= {
      tabStyle <- NULL   # CRAN check fix
      do.opties(c('tabStyle'), 
                list("<style>
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
          inherits(opts[[1]][[1]],'echarty')) {  # pipe
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
        #cont[[1]]$width <- width
        #cont[[1]]$height <- height
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
      do.opties(c('rows','cols','title'))
      lplots <- length(opts[[1]])
      if (is.null(rows) & !is.null(cols)) rows <- ceiling(lplots/cols)
      if (!is.null(rows) & is.null(cols)) cols <- ceiling(lplots/rows)
      if (is.null(rows) & is.null(cols)) { rows <- lplots; cols <- 1 }
      x <- 0
      tg <- htmltools::tagList()
      for (i in 1:rows) {
        r <- htmltools::div(style='display:flex;')
        for (j in 1:cols) {
          x <- x + 1
          sty <- paste0('width:', round(100/cols),'vw')
          c <- htmltools::div(style= sty)
          if (x <= lplots)
            c <- htmltools::div(style= sty, opts[[1]][[x]])
          r <- htmltools::tagAppendChild(r, c)
        }
        tg <- htmltools::tagAppendChildren(tg, htmltools::br(), r)
      }
  		out <- htmltools::browsable(
  		  htmltools::div(
  		 	  style= "width:100%",
  		    htmltools::div(style= 'justify-content:center!important; text-align:center!important',
    		                 htmltools::h3(title) ),
  		  tg )
  		)
    },
    
    'morph'= {
      
      # TODO: individual oo$x$theme support
      opts <- lapply(opts, \(oo) {
        if (inherits(oo, 'echarty')) {
          oo$x$opts$series <- .merlis(oo$x$opts$series,
                          list(universalTransition= list(enabled= TRUE)) )
          oo$x$opts
        }
        else oo
      })
      # series types should be different for morph options
      clickHandler <- htmlwidgets::JS("
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
          event= 'click', handler= clickHandler
        ))
      out    
    },
    
    'rescale'= {
      scale <- opts$t
      if (!is.numeric(scale)) scale <- c(0,10)
      stopifnot("ec.util: rescale 't' vector too long/short"= length(scale)==2)
      stopifnot("ec.util: rescale 't' vector elements equal"= scale[1]!=scale[2])
      smin <- min(scale);  smax <- max(scale)-smin; 
      vect <- opts$v
      stopifnot("ec.util: rescale 'v' paramater missing"= !is.null(vect))
      stopifnot("ec.util: rescale 'v' is not a numeric vector"= is.numeric(vect))
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
                ecf.fscreen(tmp.echwid); 
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
      # calc H & W of rect, for default font
      tmp <- sum(charToRaw(text) == charToRaw('\n'))
      h <- 20 * if (tmp==0) 1 else tmp
      w <- if (h>20) max(sapply(unlist(strsplit(text, '\n')),nchar)) else nchar(text)
      w <- w * 10
      tmp <- list( 
          type= 'rect', right= 40, top= 20, zlevel=4,
      		shape= list(height=h, width=w, r=5),
          style= list(fill= 'lightgray'),
          textContent= list(zlevel= 4, style= list(text= text, fill= 'black')),
          textConfig= list(position= 'inside'),
      		onclick= htmlwidgets::JS(js)
      )
      tt <- list(...)   # user values overwrite defaults
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
#' @param df Required chart data as **data.frame**. \cr
#'     Except when format is _dendrogram_, then df is a **list**, result of \link[stats]{hclust} function.\cr
#' @param format Output list format\cr \itemize{
#'  \item **dataset** = list to be used in \href{https://echarts.apache.org/en/option.html#dataset.source}{dataset} (default), or in \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} (without header). \cr
#'  \item **values** = list for customized \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data} \cr
#'  \item **names** = named lists useful for named data like \href{https://echarts.apache.org/en/option.html#series-sankey.links}{sankey links}.
#'  \item **dendrogram** = build series data for Hierarchical Clustering dendrogram
#'  \item **treePC** = build series data for tree charts from parent/children data.frame
#'  \item **treeTT** = build series data for tree charts from data.frame like Titanic.\cr
#'  \item **boxplot** = build dataset and source lists, see Details
#' }
#' @param header for dataset, to include the column names or not, default TRUE. Set it to FALSE for \href{https://echarts.apache.org/en/option.html#series-scatter.data}{series.data}.\cr
#' @param ... optional parameters\cr
#' Optional parameters for **boxplot** are:\cr
#' * _layout_ = 'h' for horizontal(default) or 'v' for vertical layout\cr
#' * _outliers_ boolean to add outlier points (default FALSE)\cr
#' * _jitter_ value for \link[base]{jitter} of numerical values in second column, default 0 (no scatter). Adds scatter series on top of boxplot.\cr
#' 
#' Optional parameter for **names**:\cr
#' * _nasep_ = single character name separator for nested lists, see Examples. \cr
#' Purpose is to facilitate conversion from _data.frame_ to nested named lists.\cr
#' 
#' @return A list for _dataset.source_, _series.data_ or other lists:\cr
#'   For boxplot - a named list, see Details and Examples \cr
#'   For dendrogram & treePC - a tree structure, see format in \href{https://echarts.apache.org/en/option.html#series-tree.data}{tree data}
#'   
#' @details 
#' `format='boxplot'` requires the first two _df_ columns as: \cr
#' \verb{   }column for the non-computational categorical axis\cr
#' \verb{   }column with (numeric) data to compute the five boxplot values\cr
#'  Additional grouping is supported on a column after the second. Groups will show in the legend, if enabled.\cr
#'  Returns a `list(dataset, series, xAxis, yAxis)` to set params in [ec.init]. 
#'  Make sure there is enough data for computation, 4+ values per boxplot.\cr
#' `format='treeTT'` expects data.frame _df_ columns _pathString,value,(optional itemStyle)_ for \link[data.tree]{FromDataFrameTable}.\cr
#'  It will add column 'pct' with value percentage for each node. See Details.
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
#'   series= list(list(
#'     type= 'tree', orient= 'TB', roam= TRUE, initialTreeDepth= -1,
#'     data= ec.data(hc, format='dendrogram'),
#'     # layout= 'radial', symbolSize= ec.clmn(scale= 0.33),
#'     ## exclude added labels like 'pXX', leaving only the originals
#'     label= list(formatter= htmlwidgets::JS(
#'       "function(n) { out= /p\\d+/.test(n.name) ? '' : n.name; return out;}"))
#'   ))
#' )
#' 
#' # build required pathString,value and optional itemStyle columns
#' df <- as.data.frame(Titanic) |> rename(value= Freq) |> mutate(
#'   pathString= paste('Titanic\nSurvival', Survived, Age, Sex, Class, sep='/'),
#' 	 itemStyle= case_when(Survived=='Yes' ~"color='green'", TRUE ~"color='LightSalmon'")) |>
#' 	 select(pathString, value, itemStyle)
#' ec.init(
#' 	  series= list(list(
#' 		  data= ec.data(df, format='treeTT'),
#' 		  type= 'tree', symbolSize= ec.clmn("(x) => {return Math.log(x)*10}")
#' 	  )),
#' 	  tooltip= list(formatter= ec.clmn('%@<br>%@%','value','pct'))
#' )
#' 
#' # column itemStyle_color will become itemStyle= list(color=...) in data list
#' # attribute names separator (nasep) is "_"
#' df <- data.frame(name= c('A','B','C'), value= c(1,2,3), 
#'      itemStyle_color= c('chartreuse','lightblue','pink'),
#'      emphasis_itemStyle_color= c('darkgreen','blue','red')
#' )
#' ec.init(series.param= list(type='pie', data= ec.data(df, 'names', nasep='_')))
#'
#' @importFrom utils tail
#' @importFrom grDevices boxplot.stats
#' @importFrom data.tree Aggregate
#' @export
ec.data <- function(df, format='dataset', header=FALSE, ...) {
  stopifnot('ec.data: expecting parameter df'= !missing(df))
  if (format=='dendrogram') { 
    stopifnot('ec.data: df should be hclust for dendrogram'= inherits(df, 'hclust'))
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
  stopifnot('ec.data: df has to be data.frame'= inherits(df, 'data.frame'))
  # save var for ec.clmn
  .setColnm(colnames(df))
  
  if (format=='treePC') {
    # for sunburst,tree,treemap
    if (!all(unlist(unname(lapply(as.list(df[,1:3]), class))) == 
             c('character','character','numeric')) )
      stop('ec.data: df columns need to be in order (parents, children, value), only value is numeric', call. = FALSE)
    
    tryCatch({
      tmp <- data.tree::FromDataFrameNetwork(df)
    },
    error= function(e) { stop(e) })
    json <- data.tree::ToListExplicit(tmp, unname=TRUE)
    return(json$children)
  }
  if (format %in% c('treeTT','treeTK')) {
    # data for tree,sunburst,treemap from Titanic-like data
    chNames <- function(lest) {
      stopifnot('chNames: expecting a list'= inherits(lest, 'list'))
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
        if (!is.null(x$itemStyle)) 
          x$itemStyle <- eval(parse(text=paste0('list(',x$itemStyle,')')))
        if (!is.null(x$children)) x <- chNames(x)
        x })
      if (!is.null(lest$children[[1]]$itemStyle))
        lest$itemStyle <- lest$children[[1]]$itemStyle
      lest
    }
    # verify columns pathString, value, itemStyle
    stopifnot('ec.data: df first columns not pathString,value'= c('pathString','value') %in% colnames(df))
    stopifnot('ec.data: column value not numeric'= is.numeric(df$value))
    
    tryCatch({
      nod <- data.tree::FromDataFrameTable(df, ...)
    },
      error= function(e) { stop(e) })
    nod$Do(function(x) x$value <- data.tree::Aggregate(x, "value", sum))
    json <- data.tree::ToListExplicit(nod)
    tmp <- chNames(json)
    return(list(tmp))
  }
  
  rownames(df) <- NULL
  n <- seq_along(df[[1]])       #  all df columns have the same length
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
    args <- list(...)
    rady <- if ('ol.radius' %in% names(args)) args$ol.radius else NULL
    jitter <- if ('jitter' %in% names(args)) args$jitter else 0
    layout <- if ('layout' %in% names(args)) args$layout else 'h'
    outliers <- if ('outliers' %in% names(args)) args$outliers else FALSE
    cn <- colnames(df)
    stopifnot('boxplot: df should have 2+ columns'= length(cn)>1)
    colas <- cn[1]
    colb5 <- cn[2]
    stopifnot('boxplot: 2nd column must be numeric'= is.numeric(df[[colb5]]))
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
#     tbox <- "(p) => { d=p.data; out= '<b>'+p.seriesName+'</b>'+
# '<br>min '+d[1].toFixed(3)+ '<br>Q1 '+d[2].toFixed(3)+
# '<br>median '+d[3].toFixed(3)+ '<br>Q3 '+d[4].toFixed(3)+
# '<br>max '+d[5].toFixed(3); return out; }"
    ttip <- c('Low', 'Q1', 'Q2', 'Q3', 'High')

    if (!is.null(grpcol)) {   # grouped
      tmp <- df |> group_split()
      dataset <- lapply(tmp, \(dd) { 
        dv <- dd |> arrange(.data[[colas]]) |> 
              group_by(.data[[colas]]) |> group_split()
        src <- lapply(dv, \(vv) {
      	 if (nrow(vv)<5) stop(paste0('ec.data: insufficient data in group "'
      	                             ,grpcol,'"'), call.= FALSE)
      	 vv[[colb5]]
        })
        list(source= if (length(src[[1]])==1) list(src) else src)
      })
      series <- list()
      for (i in 1:length(tmp)) { 
        dataset <- append(dataset, 
        	 list(list( fromDatasetIndex= i-1, transform= list(type= 'boxplot')))) 
        series <- append(series, list(list(
          name= tmp[[i]][[grpcol]][1], 
          #tooltip= list(formatter= tbox), 
          encode= list(tooltip= ttip),
          type= 'boxplot', datasetIndex= i+length(tmp)) ))  # will be decremented
      }
	    axe <- paste0("function(v) { return ['",axe,"'][v]; }")
	   
  		if (outliers) {
  		  gcnt <- length(tmp)   # group count
        c2 <- gcnt+gcnt-1
        # new datasets with outliers
        dsotl <- lapply(gcnt:c2, \(x) list( fromDatasetIndex=x, fromTransformResult=1))
        serol <- lapply((gcnt*2):(gcnt+c2), \(x) 
      		list(type='custom', 
      			  datasetIndex=x, renderItem= htmlwidgets::JS('riOutliers'),
      			  encode= list(x=2, y=1),
      			  name=series[[x-c2]]$name, z=4, 
      			  itemStyle= list(borderDashOffset= rady)
      	))
			  if (layout=='v') {
			    serol <- lapply(serol, \(ss) { 
					  e <- ss$encode; ss$encode <- list(x=e$y, y=e$x); ss })
			  }
        dataset <- append(dataset, dsotl)
        series <- append(series, serol)  		}
    } 
    else {  # non-grouped
      bdf <- ungroup(df) |> arrange(.data[[colas]]) |>
                 group_by(across({colas})) |> group_split()
      src <- lapply(bdf, \(x) {x[[colb5]]})
      nms <- paste(unlist(lapply(bdf, \(x) {unique(x[[colas]])})), collapse="','")
      nms <- paste0("(p) => ['",nms,"'][p.value]")
      dataset <- list(
        list(source= src),
        list(transform= list(type= 'boxplot', 
              config= list(itemNameFormatter= htmlwidgets::JS(nms)
        )))
      )
      series <- list(list(type='boxplot', name= 'boxplot', 
        datasetIndex= 1,
			  encode= list(y= 1, x= c(2,3,4,5,6), tooltip=ttip)
		  ))
      # default is horizontal, for vertical swap xAxis/yAxis category type
      if (layout=='v') {
        e <- series[[1]]$encode
        series[[1]]$encode <- list(x=e$y, y=e$x, tooltip=ttip) # swap x & y
      }
		  axe <- "function(v) { return v;}"
		  
  		if (outliers) {
        # new dataset with outliers and serie for it
        dsotl <- list(list( fromDatasetIndex=1, fromTransformResult=1 ))
        serol <- list(list(type='scatter', 
        		datasetIndex=2, name=series[[1]]$name, z=4, 
        		itemStyle= list(borderDashOffset= rady) ))
        if (jitter==0) {
          serol[[1]] <-  .merlis(serol[[1]], args)
          series[[1]] <- .merlis(series[[1]], args)
        }
        if (layout=='h')
          serol[[1]]$encode <- list(x=2, y=1) # swap x & y
        dataset <- append(dataset, dsotl)
        series <- append(series, serol)
  		}
    }
    if (is.factor(df[[colas]]) || is.character(df[[colas]]))
      yaxis[[1]] <- c(yaxis[[1]], list(axisLabel= list(formatter= htmlwidgets::JS(axe))))
    if (layout=='v') {
      swap <- xaxis; xaxis <- yaxis; yaxis <- swap; ttcol <- 2
    }
    
    if (jitter>0) {
  		tmp <- df |> arrange(.data[[colas]]) |> 
  		        group_by(.data[[colas]]) |> group_split()
  		mcyl <- lapply(tmp, \(x) unname(unlist(x[[colb5]])))
  		names(mcyl) <- sort(unique(df[[colas]]))
  		i <- 0.5
  		serj <- lapply(names(mcyl), \(nn) {  
  			yy <- unname(unlist(mcyl[nn]))
  			xx <- jitter(rep(i, length(yy)), amount= jitter)
  			out <- list(type= 'scatter', ...)
  			if (!'name' %in% names(args)) out$name <- nn
  			if (!'large' %in% names(args)) out$large <- TRUE
  			#if (!'tooltip' %in% names(args)) out$tooltip <- list(formatter= ec.clmn(ttcol))
  			if (!'z' %in% names(args)) out$z <- 3
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
  else { # format=='names'
    args <- list(...)
    if ('nasep' %in% names(args)) {
      stopifnot("data('names'): nasep should be 1 char"= nchar(args$nasep)==1)
      # names separator is present, replace compound names with nested lists
      tmp <- lapply(tmp, \(rr) {
        for(cc in names(rr)) {
          if (grepl(args$nasep, cc, fixed=T)) {
            nlis <- strsplit(cc, args$nasep, fixed=T)
            out <- rr[[cc]]; 
            for(nn in rev(nlis[[1]][-1])) {
              cur <- list(); cur[[nn]] <- out;
              out <- cur
            }
            rr[[cc]] <- NULL
            rr[[ nlis[[1]][1] ]] <- out
          }
        }
        rr
      })
    }
    datset <- tmp;
  }
  
  return(datset)
} 


#' Data column format
#' 
#' Helper function to display/format data column(s) by index or name
#' 
#' @param col A single column index(number) or column name(quoted string), \cr
#'    or a \link[base]{sprintf} string template for multiple indexes.\cr
#'    NULL(default) for charts with single values like tree, pie.\cr
#'    'json' to display tooltip with all available values to choose from.\cr 
#'    'log' to write all values in the JS console (F12) for debugging.\cr
#'    Can contain JS function starting with _'function('_ (or format _'(x) => {}'_).\cr
#' @param ... Comma separated column indexes or names, only when \emph{col} is \emph{sprintf}. This allows formatting of multiple columns, as for a tooltip.\cr
#' @param scale A positive number, multiplier for numeric columns. When scale is 0, all numeric values are rounded.
#' @return A JavaScript code string (usually a function) marked as executable, see \link[htmlwidgets]{JS}.
#'  
#' @details This function is useful for attributes like formatter, color, symbolSize, label.\cr
#' Column indexes are counted in R and start with 1.\cr
#' Omit _col_ or use index -1 for single values in tree/pie charts, \emph{axisLabel.formatter} or \emph{valueFormatter}. See [ec.data] dendrogram example.\cr
#' Column indexes are decimals for combo charts with multiple series, see [ecr.band] example. The whole number part is the serie index, the decimal part is the column index inside.\cr
#' \emph{col} as sprintf has the same placeholder \emph{%@} for both column indexes or column names.\cr
#' \emph{col} as sprintf can contain double quotes, but not single or backquotes.\cr
#' Template placeholders with formatting:\cr
#' * \emph{%@} will display column value as-is.\cr
#' * \emph{%L@} will display a number in locale format, like '12,345.09'.\cr
#' * \emph{%LR@} rounded number in locale format, like '12,345'.\cr
#' * \emph{%R@} rounded number, like '12345'.\cr
#' * \emph{%R2@} rounded number, two digits after decimal point.\cr
#' * \emph{%M@} marker in serie's color.\cr
#' Notice that tooltip _formatter_ will work for _trigger='item'_, but not for _trigger='axis'_ when there are multiple value sets.
#' 
#' @examples
#' tmp <- data.frame(Species = as.vector(unique(iris$Species)),
#'                   emoji = c('A','B','C'))
#' df <- iris |> dplyr::inner_join(tmp)      # add 6th column emoji
#' df |> dplyr::group_by(Species) |> ec.init(
#'   series.param= list(label= list(show= TRUE, formatter= ec.clmn('emoji'))),
#'   tooltip= list(formatter=
#'     # with sprintf template + multiple column indexes
#'     ec.clmn('%M@ species <b>%@</b><br>s.len <b>%@</b><br>s.wid <b>%@</b>', 5,1,2))
#' )
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
  ret <- paste("pos=[]; c= String(typeof x=='object' ? x.value : x);", scl)
  
  if (is.null(col)) {}   # for pie,sunburst
  else if (col=='log')
    ret <- "console.log(x); return 'logged';"
  else if (col=='json')
    ret <- 'return JSON.stringify(x, null, " ").replace(/{/g,"<br>{").replace(/"value":/g,"<br> value:").replace(/"data":/g,"<br> data:").replace(/"seriesIndex":/g,"<br> seriesIndex:");'
  else if (is.character(col) && (grepl(') =>', col) || 
                                 startsWith(col, 'function(')))
    return(htmlwidgets::JS(col))
  else if (is.na(suppressWarnings(as.numeric(col)))) {
  	
	spf <- "var sprintf= (template, vals) => {
j=0; if (template=='%@') return vals[j++];
return template.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => {
  if (m=='%@') return vals[j++];
  if (m=='%L@') return Number(vals[j++]).toLocaleString();
  if (m=='%LR@') return Math.round(Number(vals[j++])).toLocaleString();
  if (m=='%R@') return Math.round(Number(vals[j++]));
  if (m=='%R2@') return Number(vals[j++]).toFixed(2);
  if (m=='%M@') return x.marker;
}); };"
	if (length(args)==0) {  # col is solitary name
		args <- col; col <- '%@'   # replace
	}
	# col is sprintf

	tmp <- suppressWarnings(as.numeric(args) -1)
	if (all(is.na(tmp))) {   
		# multiple column names (non-numeric strings)
		# to find position in colnames
	  tmp <- .getColnm()
		stopifnot("ec.clmn: colnames missing.
    Use ec.clmn after ec.data and/or inside ec.init(df).
    Otherwise use column indexes instead of names."= !is.null(tmp))
		spf <- paste0(spf, " pos=['", paste(tmp, collapse="','"), "'];")
		t0 <- sapply(args, \(s) toString(paste0("x.data['", s,"']")) )
		t0 <- paste(t0, collapse=',')
		t1 <- paste(args, collapse='`,`')
		# ec.data(df,'values'): x is array for size, x.data.value for tooltip
		# x.data = 
		# 	1) object when ec.data('names')
		# 	2) array only when ec.data('dataset') or df
		#		3) x.data.value array with x.dimensionNames when ec.data('values')
		# if series.dimensions=colnames(df) not set then x.dimensionNames may be wrong,
		#   like ['lng', 'lat', 'value', 'value0',...]   console.log(x.dimensionNames);
		# was if (x.dimensionNames && x.dimensionNames.length>0) pos= args.map(z => x.dimensionNames.indexOf(z)); else..
		ret <- paste0( spf, " 
aa= Array.isArray(x) ? x : x.data; tmp= null;
if (aa && aa instanceof Object && !Array.isArray(aa)) {
	tmp= Object.keys(aa); if (tmp.length==1 && tmp[0]=='value') aa= x.data.value;}
if (tmp && tmp.length>1)
	vv=[",t0,"];
else {
 if (!aa || !aa.length) return `no data`;
 args= [`",t1,"`]; 
 pos= args.map(z => pos.indexOf(z));
 vv= pos.map(p => aa[p]); }")
	}   # col.names
	else {   
		#  multiple numeric, they could be in x, x.data, x.value OR x[].value[]
		#  in combo-charts (ec.band), get decimal portion as .value[] index
		tmp <- paste(tmp, collapse=',')
		ret <- paste0( spf, "ss=[",tmp,"];
vv= ss.map((e) => { 
  if (e<0) return x.value ? x.value : x;
  i= Math.floor(e);
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
    
	if (scale >0) ret <- paste(ret,"vv= vv.map(e => isNaN(e) | !e ? e : e*",scale,");")
	if (scale==0) ret <- paste(ret,"vv= vv.map(e => isNaN(e) | !e ? e : Math.round(e));")
	# keep backwards-quotes for handling '\n'
	ret <- paste0(ret, "c= sprintf(`",col,"`, vv); return c; ")
  } # col is string
  else {      # col is solitary numeric
    if (length(args) > 0)
      warning('col is numeric, others are ignored', call.=FALSE)
    col <- as.numeric(col) - 1   # from R to JS counting
    if (col >= 0)
      ret <- paste0('c= String(x.value!=null ? x.value[',col,
                    '] : x.data!=null ? x.data[',col,'] : x[',col,'] ); ',scl)
  }  # col is solitary numeric
  ret <- gsub('\t',' ', gsub('\n',' ', ret, fixed=T), fixed=T)
  htmlwidgets::JS(paste0('function(x) {', ret, '}'))
}

# ----------- Other utilities ----------------------


#' Parallel Axis
#' 
#' Build 'parallelAxis' for a parallel chart
#' 
#' @param dfwt An echarty widget OR a data.frame(regular or grouped)
#' @param cols A string vector with columns names in desired order
#' @param minmax Boolean to add max/min limits or not, default TRUE
#' @param ... Additional attributes for \href{https://echarts.apache.org/en/option.html#parallelAxis}{parallelAxis}.
#' @return A list, see format in \href{https://echarts.apache.org/en/option.html#parallelAxis}{parallelAxis}.
#' @details This function could be chained to _ec.init_ or used with a _data.frame_\cr
#' @examples
#' iris |> dplyr::group_by(Species) |>    # chained
#' ec.init(ctype= 'parallel', series.param= list(lineStyle= list(width=3))) |> 
#' ec.paxis(cols= c('Petal.Length','Petal.Width','Sepal.Width'))
#' 
#' mtcars |> ec.init(ctype= 'parallel', 
#'    parallelAxis= ec.paxis(mtcars, cols= c('gear','cyl','hp','carb'), nameRotate= 45),
#'    series.param= list(smooth= TRUE)
#' )
#' 
#' @export 
ec.paxis <- function(dfwt=NULL, cols=NULL, minmax=TRUE, ...) {
  pax <- list(); grnm <- ''
  if (inherits(dfwt, 'data.frame')) {
    coln <- colnames(dfwt)
    cfilter <- 1:length(coln)
    if (is.grouped_df(dfwt)) {
      # dont include grouping column (grnm)
      grnm <- group_vars(dfwt)[[1]]
      cfilter <- cfilter[!cfilter==match(grnm, colnames(dfwt))]
    }
    idf <- dfwt
  } 
  else {
    stopifnot('ec.paxis: dfwt has to be class echarty'= inherits(dfwt, 'echarty'))
    coln <- unlist(dfwt$x$opts$dataset[[1]]$source[1])
    cfilter <- 1:length(coln)
    if (length(dfwt$x$opts$dataset) > 1) {
      grnm <- dfwt$x$opts$dataset[[2]]$transform$config$dimension
      cfilter <- cfilter[!cfilter==match(grnm, coln)]
    }
    idf <- as.data.frame(t(do.call(cbind, dfwt$x$opts$dataset[[1]]$source[(-1)])))
    colnames(idf) <- coln
    idf <- as.data.frame(apply(idf, 2, unlist, simplify=FALSE))
  }
  if (!is.null(cols)) {
    stopifnot('ec.paxis: some cols not found'= all(cols %in% coln))
    cfilter <- match(cols, coln)
  }
  for(i in cfilter) {
    cn <- coln[i]
    tmp <- list(dim= i-1, name= cn, ...)  # JS count is -1
    if (!is.numeric(idf[cn][[1]]))
      tmp$type <- 'category'
    else {
      if (minmax) {
        tmp$max <- max(idf[cn])
        tmp$min <- min(idf[cn])
      }
    }
    pax <- append(pax, list(tmp)); 
  }
  if (inherits(dfwt, 'data.frame')) 
    pax
  else {
    dfwt$x$opts$parallelAxis= pax
    dfwt
  }
}
               


#' Themes
#'
#' Apply a pre-built or custom coded theme to a chart
#'
#' @param wt Required \code{echarty} widget as returned by [ec.init]
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
ec.theme <- function (wt, name='custom', code= NULL) 
{
  #stopifnot('ec.theme: name required'= !missing(name))
  stopifnot('ec.theme: wt should be echarty object'= inherits(wt, 'echarty'))

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
#' @param target type of resulting value: \cr
#' \verb{   }'opts' - the htmlwidget _options_ as JSON (default)\cr
#' \verb{   }'full' - the _entire_ htmlwidget as JSON\cr
#' \verb{   }'data' - info about chart's embedded data (char vector)
#' @param ... Additional attributes to pass to \link[jsonlite]{toJSON}\cr
#' 'file' - optional file name to save to when target='full'\cr
#' @return A JSON string, except when \code{target} is 'data' - then
#'  a character vector.
#'
#' @details Must be invoked or chained as last command.\cr
#' target='full' will export all JavaScript custom code, ready to be used on import.\cr
#' See also [ec.fromJson].
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

  stopifnot("ec.inspect: target to be 'opts', 'data' or 'full'"= target %in% c('opts','data','full'))
	if (target=='full') {
	  jjwt <- jsonlite::serializeJSON(wt)
	  opts <- list(...)
	  if ('file' %in% names(opts)) {
	    fn <- opts$file
      con <- file(fn,'wb'); write(jjwt, con); close(con)
      return(paste('saved in',fn))
	  } else
	    return(jjwt)
	}
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
      if (!is.null(s$data)) 
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
#' Convert JSON string or file to chart
#' 
#' @param txt Could be one of the following:\cr
#' \verb{   }class _url_, like \code{url('https://serv.us/cars.txt')}\cr
#' \verb{   }class _file_, like \code{file('c:/temp/cars.txt','rb')}\cr
#' \verb{   }class _json_, like \code{ec.inspect(p)}, for options or full\cr
#' \verb{   }class _character_, JSON string with options only, see example below\cr
#' @param ... Any attributes to pass to internal [ec.init] when _txt_ is options only
#' @return An _echarty_ widget.
#' 
#' @details _txt_ could be either a list of options (x$opts) to be set by \href{https://echarts.apache.org/en/api.html#echartsInstance.setOption}{setOption},\cr
#'  OR an entire _htmlwidget_ generated thru [ec.inspect] with _target='full'_.\cr
#'  The latter imports all JavaScript functions defined by the user.
#' 
#' @examples
#' txt <- '{
#'    "xAxis": { "data": ["Mon", "Tue", "Wed"]}, "yAxis": { },
#'    "series": { "type": "line", "data": [150, 230, 224] } }'
#' ec.fromJson(txt)
#' 
#' # ec.fromJson('https://helgasoft.github.io/echarty/test/pfull.json')
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
  
	if (inherits(txt, c('url','file')))
  		return(jsonlite::unserializeJSON(txt))
	if (inherits(txt, 'character')) {
		if (any(startsWith(txt, c('http://','https://'))))
			return(jsonlite::unserializeJSON(url(txt)))
	} else if (inherits(txt, 'json')) {
		if (grepl('preRenderHook',txt))
  			return(jsonlite::unserializeJSON(txt))
	} else
		stop(paste0("ec.fromJson: unknown class '",class(txt),"'"))
	
	tmp <- jsonlite::fromJSON(txt, simplifyVector = FALSE)
	# options only
	obj <- ec.init(...)
	obj$x$opts <- recur(tmp)
	obj
}



#' ------------- Licence -----------------
#'
#' Original work Copyright 2021-2024 Larry Helgason
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
