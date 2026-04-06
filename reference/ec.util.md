# Utility functions

tabset, table layout, support for GIS shapefiles through library 'sf'

## Usage

``` r
ec.util(cmd = "sf.series", ..., js = NULL, event = "click")
```

## Arguments

- cmd:

  Utility command name, see Details.

- ...:

  Optional parameters for each command.

- js:

  Optional JavaScript function, default is NULL.

- event:

  Optional event name for cmd='morph', default is 'click'.

## Details

**cmd = 'sf.series'**  
  Build *leaflet* or
[geo](https://echarts.apache.org/en/option.html#geo.map) map series from
shapefiles.  
  Supported types: POINT, MULTIPOINT, LINESTRING, MULTILINESTRING,
POLYGON, MULTIPOLYGON  
  Coordinate system could be *leaflet*(default), *geo*, *cartesian2D* or
*cartesian3D*(for POINT(xyz))  
  Limitations:  
   polygons can have only their name in tooltip, need *load='custom'*
for rendering  
   assumes Geodetic CRS is WGS 84, for conversion use
[st_transform](https://r-spatial.github.io/sf/reference/st_transform.html)
with *crs=4326*.  
  Parameters:  
   df - value from
[st_read](https://r-spatial.github.io/sf/reference/st_read.html)  
   nid - optional column name used in tooltip formatter  
   verbose - optional, print shapefile item names in console  
  Returns a list of chart series  
  
**cmd = 'sf.bbox'**  
  Returns JavaScript code to position a map inside a bounding box from
[st_bbox](https://r-spatial.github.io/sf/reference/st_bbox.html), for
leaflet only.  
  
**cmd = 'sf.unzip'**  
  Unzips a remote file and returns local file name of the unzipped .shp
file  
   url - URL of remote zipped shapefile  
   shp - optional name of .shp file inside ZIP file if multiple exist.
Do not add file extension.  
  Returns full name of unzipped .shp file, or error string starting with
'ERROR'  
  
**cmd = 'geojson'**  
  Custom series list from geoJson objects  
   geojson - object from
[fromJSON](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)  
   cs - optional *coordinateSystem* value, default 'leaflet'  
   ppfill - optional fill color like '#F00', OR NULL for no-fill, for
all Points and Polygons  
   nid - optional feature property for item name used in tooltips  
   ... - optional custom series attributes like *itemStyle*  
  Can display also geoJson *feature properties*: color; lwidth, ldash
(lines); ppfill, radius (points)  
  
**cmd = 'layout'**  
  Multiple charts in table-like rows/columns format  
   ... - List of charts  
   title - optional title for the entire set  
   rows - optional number of rows  
   cols - optional number of columns  
  Returns a container
[div](https://rstudio.github.io/htmltools/reference/builder.html) in
rmarkdown, otherwise
[browsable](https://rstudio.github.io/htmltools/reference/browsable.html).  
  For 3-4 charts one would use multiple series within a
[grid](https://echarts.apache.org/en/option.html#grid).  
  For greater number of charts *ec.util(cmd='layout')* comes in handy  
  
**cmd = 'tabset'**  
   ... - a list of name/chart pairs like *n1=chart1, n2=chart2*, each
tab may contain a chart, see example  
   tabStyle - tab style string, see default *strTabStyle* variable in
the code  
   width - optional width size for the tabset, in CSS format, default is
100%  
  Returns A)
[browsable](https://rstudio.github.io/htmltools/reference/browsable.html)
when '...' params are provided  
  Returns B)
[tagList](https://rstudio.github.io/htmltools/reference/tagList.html) of
tabs when in a pipe (no '...' params)  
  Please note that a maximum of five(5) tabs are supported by current
*tabStyle*.  
  
**cmd = 'button'**  
  UI button to execute a JS function,  
   text - the button label  
   js - the JS function string  
   ... - optional parameters for the
[rect](https://echarts.apache.org/en/option.html#graphic.elements-rect.type)
element  
  Returns a
graphic.elements-[rect](https://echarts.apache.org/en/option.html#graphic.elements-rect.type)
element.  
  
**cmd = 'morph'**  
   ... - a list of charts or chart option lists  
   event - name of event for switching charts. Default is *click*.  
  Returns a chart with ability to morph into other charts  
  
**cmd = 'fullscreen'**  
  A toolbox feature to toggle fullscreen on/off. Works in a browser, not
in RStudio.  
  
**cmd = 'rescale'**  
   v - input vector of numeric values to rescale  
   t - target range c(min,max), numeric vector of two  
  
**cmd = 'level'**  
  Calculate vertical levels for timeline *line* charts, returns a
numeric vector  
   df - data.frame with *from* and *to* columns  
   from - name of 'from' column  
   to - name of 'to' column  

## Examples

``` r
library(dplyr)
if (interactive()) {  # comm.out: Cran Fedora errors about some 'browser'
  library(sf)
  fname <- system.file("shape/nc.shp", package="sf")
  nc <- as.data.frame(st_read(fname))
  ec.init(load= c('leaflet', 'custom'),  # load custom for polygons
     js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)),
     series= ec.util(cmd= 'sf.series', df= nc, nid= 'NAME', itemStyle= list(opacity=0.3)),
     tooltip= list(formatter= '{a}')
  )
}

if (interactive()) {
 p1 <- cars |> ec.init(grid= list(top=26), height=333)  # move chart up
 p2 <- mtcars |> arrange(mpg) |> ec.init(height=333, ctype='line')
 ec.util(cmd= 'tabset', cars= p1, mtcars= p2)

 lapply(list('dark','macarons','gray','dark-mushroom'),
   function(x) cars |> ec.init(grid= list(bottom=5, top=10)) |> ec.theme(x) ) |>
 ec.util(cmd='layout', cols= 2, title= 'Layout')
}

cars |> ec.init(
  graphic = list(
    ec.util(cmd='button', text='see type', right='center', top=20,
      js="function(a) {op=ec_option(echwid); alert(op.series[0].type);}")
  )
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"graphic":[{"type":"rect","zlevel":4,"shape":{"height":20,"width":80,"r":5},"style":{"fill":"lightgray"},"textContent":{"zlevel":4,"style":{"text":"see type","fill":"black"}},"textConfig":{"position":"inside"},"onclick":"function(a) {op=ec_option(echwid); alert(op.series[0].type);}","text":"see type","right":"center","top":20}],"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"series":[{"type":"scatter","id":"ec.auto"}],"xAxis":{"type":"value","name":"speed","show":true},"yAxis":{"type":"value","name":"dist","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.graphic.0.onclick"],"jsHooks":[]}
colors <- c("blue","red","green")
cyls <- as.character(sort(unique(mtcars$cyl)))
sers <- lapply(mtcars |> group_by(cyl) |> group_split(), \(x) {
  cyl <- as.character(unique(x$cyl))
  list(type='scatter', id=cyl, dataGroupId=cyl, 
       data= ec.data(x |> select(mpg,hp)),
       universalTransition= TRUE)
})
oscatter <- list(
  title= list(text='Morph', left='center', subtext='click points to morph'), 
  color= colors, tooltip= list(show=TRUE),
  xAxis= list(scale=TRUE, name='mpg'), yAxis= list(scale=TRUE, name='hp'),
  series= sers
)
opie <- list(
  title= list(text= 'Average hp'), 
  color= colors, tooltip= list(show=TRUE),
  series= list(list(
    type= 'pie', label= list(show=TRUE), colorBy= 'data',
    data= ec.data(mtcars |> group_by(cyl) |> summarize(value= mean(hp)) |>
       mutate(groupId= as.character(cyl), name= as.character(cyl)),'names'),
    universalTransition= list(enabled=TRUE, seriesKey= cyls)
  ))
)
ec.util(cmd='morph', oscatter, opie) 

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"title":{"text":"Morph","left":"center","subtext":"click points to morph"},"color":["blue","red","green"],"tooltip":{"show":true},"xAxis":{"scale":true,"name":"mpg"},"yAxis":{"scale":true,"name":"hp"},"series":[{"type":"scatter","id":"4","dataGroupId":"4","data":[[22.8,93],[24.4,62],[22.8,95],[32.4,66],[30.4,52],[33.9,65],[21.5,97],[27.3,66],[26,91],[30.4,113],[21.4,109]],"universalTransition":true},{"type":"scatter","id":"6","dataGroupId":"6","data":[[21,110],[21,110],[21.4,110],[18.1,105],[19.2,123],[17.8,123],[19.7,175]],"universalTransition":true},{"type":"scatter","id":"8","dataGroupId":"8","data":[[18.7,175],[14.3,245],[16.4,180],[17.3,180],[15.2,180],[10.4,205],[10.4,215],[14.7,230],[15.5,150],[15.2,150],[13.3,245],[19.2,175],[15.8,264],[15,335]],"universalTransition":true}],"morph":[{"title":{"text":"Morph","left":"center","subtext":"click points to morph"},"color":["blue","red","green"],"tooltip":{"show":true},"xAxis":{"scale":true,"name":"mpg"},"yAxis":{"scale":true,"name":"hp"},"series":[{"type":"scatter","id":"4","dataGroupId":"4","data":[[22.8,93],[24.4,62],[22.8,95],[32.4,66],[30.4,52],[33.9,65],[21.5,97],[27.3,66],[26,91],[30.4,113],[21.4,109]],"universalTransition":true},{"type":"scatter","id":"6","dataGroupId":"6","data":[[21,110],[21,110],[21.4,110],[18.1,105],[19.2,123],[17.8,123],[19.7,175]],"universalTransition":true},{"type":"scatter","id":"8","dataGroupId":"8","data":[[18.7,175],[14.3,245],[16.4,180],[17.3,180],[15.2,180],[10.4,205],[10.4,215],[14.7,230],[15.5,150],[15.2,150],[13.3,245],[19.2,175],[15.8,264],[15,335]],"universalTransition":true}]},{"title":{"text":"Average hp"},"color":["blue","red","green"],"tooltip":{"show":true},"series":[{"type":"pie","label":{"show":true},"colorBy":"data","data":[{"cyl":4,"value":82.63636363636364,"groupId":"4","name":"4"},{"cyl":6,"value":122.2857142857143,"groupId":"6","name":"6"},{"cyl":8,"value":209.2142857142857,"groupId":"8","name":"8"}],"universalTransition":{"enabled":true,"seriesKey":["4","6","8"]}}]}]},"on":[{"event":"click","handler":"function(event) {\n        opt= this.getOption();\n        keep= opt.morph;\n        for(i=0; i<keep.length; i++) {\n    \t    if (opt.series[0].type==keep[i].series[0].type) {\n    \t      next= (i+1) % keep.length;\n       \t\t  optcurr= Object.assign({}, keep[next]);\n       \t\t  break;\n    \t    }\n    \t  };\n    \t  if (!optcurr) return;\n    \t  optcurr.morph= keep;\n    \t  this.setOption(optcurr, true);\n      }"}]},"evals":["on.0.handler"],"jsHooks":[]}  
```
