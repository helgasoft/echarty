#' tests for ec.util()
library(dplyr)

test_that("serie from ec.util with cartesian3D", {
  expect_error(ec.util(cmd= 'dummy'))

  # usage for LIDAR data
  library(sf)
  tmp <- st_as_sf(data.frame(x=c(-70,-70,-70), y=c(45, 46, 47), z=c(1,2,3)), 
                  coords= c('x','y','z'), crs= st_crs(4326))
  p <- ec.init(load= c('3D'),
               series= ec.util(df= tmp, cs= 'cartesian3D')
               ,tooltip= list(formatter= '{b}')
  )
  expect_s3_class(p$x$opts$series[[1]]$data[[2]]$value, 'sfg')
  expect_equal(as.numeric(p$x$opts$series[[1]]$data[[2]]$value), c(-70,46,2))
  expect_type( p$x$opts$xAxis3D[[1]],'list')
})

test_that("shapefiles with multi-POLYGONS", {
  library(sf)
  fname <- system.file("shape/nc.shp", package="sf")
  nc <- as.data.frame(st_read(fname, quiet=TRUE))
  p <- ec.init(load= c('leaflet', 'custom'),  # load custom for polygons
               js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)),
               series= ec.util(cmd= 'sf.series', df= nc, nid= 'NAME', itemStyle= list(opacity= 0.3)),
               tooltip= list(formatter= '{a}')
  )
  expect_true(p$x$opts$leaflet$roam)
  expect_equal(p$x$opts$series[[108]]$name, 'Brunswick')
  expect_equal(p$x$opts$series[[108]]$itemStyle$opacity, 0.3)
})

test_that("shapefile LINES from ZIP", {
  if (interactive()) {  # creates a subfolder 'railways'
    library(sf)
    fname <- ec.util(cmd= 'sf.unzip', 
                     url= 'https://mapcruzin.com/sierra-leone-shapefiles/railways.zip')
    nc <- as.data.frame(st_read(fname, quiet=TRUE))
    p <- ec.init(load= 'leaflet',
       js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)), 
       series= ec.util(df= nc, nid= 'osm_id', verbose=TRUE,
                       lineStyle= list(width= 3, color= 'red')),
       tooltip= list(formatter= '{a}'), animation= FALSE,
       leaflet= list( roam= TRUE,
         tiles= list(list(
           urlTemplate= 'https://stamen-tiles-{s}.a.ssl.fastly.net/terrain/{z}/{x}/{y}{r}.{ext}',
           options= list(attribution= 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>',
                         subdomains= 'abcd', maxZoom= 18, ext= 'png')))) 
    )
    expect_equal(p$x$opts$leaflet$tiles[[1]]$options$subdomains, 'abcd')
    expect_equal(p$x$opts$series[[6]]$name, '207557821')
    expect_equal(p$x$opts$series[[6]]$lineStyle$color, 'red')
    
  }
  else expect_equal(1,1) # bypass
})

test_that("shapefile LINESTRING and MULTILINESTRING", {
  p <- ec.init(load= 'leaflet')  #js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)),
  ls <- st_linestring(rbind(c(0,0),c(1,1),c(2,1)))
  nc <- ls %>% st_sfc %>% st_sf %>% st_cast(to='LINESTRING')
  p$x$opts$series= ec.util(cmd= 'sf.series', df= nc, lineStyle= list(width=5))
  expect_equal(p$x$opts$series[[1]]$name, 1)
  
  mls <- st_multilinestring(list(rbind(c(2,2),c(1,3)), rbind(c(0,0),c(1,1),c(2,1))))
  nc <- mls %>% st_sfc %>% st_sf %>% st_cast(to='MULTILINESTRING')
  p$x$opts$series= ec.util(cmd= 'sf.series', df= nc, lineStyle= list(width=5))
  expect_equal(length(p$x$opts$series[[1]]$data[[2]]), 3)
})

test_that("shapefile POINTS from ZIP", {
  fn <- ec.util(cmd= 'sf.unzip', 
          url= 'https://mapcruzin.com/sierra-leone-shapefiles/points.zip')
  expect_true(endsWith(fn, 'points.shp'))

  if (interactive()) {  # creates a subfolder 'points'
    library(sf)
    fn <- ec.util(cmd= 'sf.unzip', 
                  url= 'https://mapcruzin.com/sierra-leone-shapefiles/points.zip')
    nc <- as.data.frame(st_read(fn, quiet=TRUE)) |> head(10)
    p <- ec.init(load= c('leaflet'),
       js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)), 
       series= ec.util(df= nc, name= 'spots', itemStyle= list(color= 'red'), verbose=TRUE),
       tooltip= list(valueFormatter= ec.clmn('json')), legend= list(show= TRUE)
    )
    expect_s3_class(p$x$opts$series[[1]]$data[[2]]$value, 'sfg')
    expect_equal(round(as.numeric(p$x$opts$series[[1]]$data[[2]]$value),1), c(-13.3,8.5))
    expect_true( p$x$opts$leaflet$roam)
  }
  else expect_equal(1,1)
})

test_that("layout", {
  p <- lapply(list('dark','macarons','gray','jazz','dark-mushroom'),
              function(x) cars |> ec.init() |> ec.theme(x) ) |>
    ec.util(cmd='layout', cols= 4, title= 'my layout')
  expect_equal(p$children[[2]]$children[[2]]$children[[2]]$children[[1]]$x$theme, 'macarons')
  # test for 2nd row
  expect_equal(p$children[[2]]$children[[4]]$children[[1]]$children[[1]]$x$theme, 'dark-mushroom')
})

test_that("tabset with pairs", {
  p1 <- cars |> ec.init(grid= list(top= 20))
  p2 <- mtcars |> ec.init()
  r <- ec.util(cmd='tabset', cars=p1, mtcars=p2)
  expect_equal(r[[2]]$children[[5]]$children[[1]]$children[[1]][[1]]$x$opts$dataset[[1]]$source[[1]], c("speed", "dist"))
  expect_equal(r[[2]]$children[[5]]$children[[1]]$name, "section")
  expect_equal(r[[2]]$children[[2]]$children[[1]], "cars")
  expect_s3_class(r[[2]]$children[[5]]$children[[2]]$children[[1]][[1]], 'echarty')
})

test_that("tabset with pipe", {
  r <- htmltools::browsable(
    lapply(iris |> group_by(Species) |> group_split(), function(x) { 
      x |> ec.init(ctype= 'scatter', title= list(text= unique(x$Species)))
    }) |> ec.util(cmd='tabset')
  )
  expect_equal(r[[2]]$children[[7]]$children[[2]]$children[[1]][[1]]$width, NULL)
  expect_equal(as.character(r[[2]]$children[[6]]$children[[1]]), "virginica")
})

test_that("morph 1", {
  mc <- mtcars |> filter(cyl<8)
  datt <- function(idx) { return(mc[mc$cyl==idx,]$hp) }
  colors <- c("blue","red","green","yellow")
  
  oscatter <- list(
    xAxis= list(scale=TRUE),
    yAxis= list(scale=TRUE), color= colors,
    series=list(
      list(type='scatter', id=4, dataGroupId=4, data= datt(4),
           universalTransition= list(enabled= TRUE)),
      list(type='scatter', id=6, dataGroupId=6, data= datt(6),
           universalTransition= list(enabled=TRUE)) 
    )
  )
  obar <- list(
    title= list(text= 'Average'),
    xAxis= list(type= 'category', data= list('cyl4', 'cyl6')),
    yAxis= list(show= TRUE), color= colors,
    series= list(list(
      type= 'bar', id= 'average', colorBy= 'data',
      data= list(
        list(value= mean(datt(4)), groupId=4),
        list(value= mean(datt(6)), groupId=6)),
      universalTransition=list(enabled= TRUE, 
                               seriesKey=c(4, 6))
    ))
  )
  
  auto <- " cnt = 0;
  setInterval(() => {
    cnt++;
    opts= chart.getOption();
    optcurr= Object.assign({}, opts.morph[cnt % 2]);
    optcurr.morph= opts.morph;
    chart.setOption(optcurr, true);
  }, 2000);
  "
  p <- ec.util(cmd='morph', oscatter, obar, js=auto)
  expect_equal(p$x$opts$morph[[1]]$series[[1]]$type, 'scatter')
  expect_equal(p$x$opts$morph[[2]]$series[[1]]$type, 'bar')
  expect_true(grepl('setInterval', p$x$jcode, fixed=TRUE))
  p <- ec.util(cmd='morph', oscatter, obar)
  expect_equal(p$x$on[[1]]$event, 'click')
})

test_that("morph 2", {
  setd <- function(type) {
    mtcars |> group_by(cyl) |> ec.init(ctype= type) |> ec.upd({
      title <- list(subtext='mouseover points to morph')
      xAxis <- list(scale=TRUE)
      series <- lapply(series, function(ss) {
        ss$groupId <- ss$name
        ss$universalTransition <- list(enabled=TRUE)
        ss })
    })
  }
  oscatter <- setd('scatter')
  obar <- setd('bar')
  p <- ec.util(cmd='morph', oscatter, obar)
  expect_equal(p$x$opts$morph[[2]]$series[[3]]$type, 'bar')
  expect_true (p$x$opts$morph[[2]]$series[[3]]$universalTransition$enabled)
  expect_equal(p$x$opts$yAxis, list(show=T, type= "value", name= "disp"))
})

test_that("fullscreen", {
  tbox <- list(right='20%', feature= ec.util(cmd='fullscreen'))
  #p <- cars |> ec.init(toolbox= tbox)
  #expect_match(p$x$opts$toolbox$feature$myecfs$onclick, 'ecf.fscreen', fixed=TRUE)
  p <- crosstalk::bscols(
    cars |> ec.init(toolbox= tbox),
    mtcars |> ec.init(toolbox= tbox) |>
      htmlwidgets::prependContent(
        htmltools::tags$style(".echarty:fullscreen { background-color: beige; }")
      )
  )
  expect_match(p$children[[1]]$children[[1]][[1]]$children[[1]]$x$opts$toolbox$feature$myecfs$onclick, 'ecf.fscreen(tmp.echwid)', fixed=TRUE)
  expect_match(p$children[[1]]$children[[1]][[2]]$children[[1]]$prepend[[1]]$children[[1]], '.echarty:fullscreen', fixed=TRUE)
})

test_that("rescale", {
  p <- ec.util(cmd='rescale', t=c(5,25), v=44:64)
  expect_equal(p[5], 9)
})

test_that("level", {
  tmp <- "id,from,to
1,2020-03-03,2020-05-03
2,2020-01-03,2020-03-13
3,2020-06-03,2020-07-03
"
  df <- read.table(text=tmp, header= TRUE, sep=',')
  p <- ec.util(cmd='level', df=df)
  expect_equal(p, c(1,2,1))
})

test_that("labelsInside and doType(xAxis)", {
  p <- ec.init(
    xAxis= list(data= list(1,2,3,4,5,6,7)),
    series= list(
      list(name= 'long text, 20 chars', type='line',
           data= c(110, 132, 101, 134, 90, 230, 210),
           endLabel= list( show=TRUE, formatter='{a}'),
           labelLayout= htmlwidgets::JS("(params) => ecf.labelsInside(params)")),
      list(name='longer text, this is 35 characters',type='line', 
           data= c(210, 232, 201,234, 290, 240, 230),
           endLabel=list(show=TRUE, formatter='{a}'),
           labelLayout= htmlwidgets::JS("(params) => ecf.labelsInside(params)"))
          # labelLayout= ec.util(cmd='labelsInside'))
    )
  )	
  expect_match(   p$x$opts$series[[2]]$labelLayout, "ecf.labelsInside", fixed=TRUE)
  expect_s3_class(p$x$opts$series[[2]]$labelLayout, 'JS_EVAL')
  #expect_equal(p$x$opts$xAxis$type, 'category')  # default for xAxis.data ?!
})

test_that("lottie", {
  json <- 'https://helgasoft.github.io/echarty/js/spooky-ghost.json'
  cont <- jsonlite::fromJSON(json, simplifyDataFrame=FALSE)
  
  p <- iris |> group_by(Species) |> 
    ec.init(
      load= 'lottie',
      graphic= list(elements= list(
        list( type= "group", 
    		# lottie params: info + optional scale and loop 
    		info= cont, scale= 250, # loop= FALSE,
            left= 'center', top= 'middle' # ,rotation= -20
        )
      ))
    )
  expect_match(p$x$opts$graphic$elements[[1]]$info$nm, "Spookey", fixed=TRUE)
  expect_equal(length(p$x$opts$graphic$elements[[1]]$info$layers), 13)
})

test_that("button as graphic element", {
  p <- ec.util(cmd='button', text='btn', js="(a) => {return a.txt;}")
  expect_equal(p$style$fill, 'lightgray')
  expect_equal(p$textContent$style$text, 'btn')
  expect_s3_class(p$onclick, 'JS_EVAL')
})

test_that("ec.data dendrogram", {
  hc <- hclust(dist(USArrests), "average")
  p <- ec.init(preset= FALSE,
               series= list(list(
                 type= 'tree', roam= TRUE, initialTreeDepth= -1,
                 data= ec.data(hc, format='dendrogram') ))
  )
  expect_equal(p$x$opts$series[[1]]$data[[1]]$name, 'p49')
  expect_equal(p$x$opts$series[[1]]$data[[1]]$children[[1]]$children[[1]]$children[[2]]$name, 'North Carolina')
  expect_equal(length(p$x$opts$series[[1]]$data[[1]]$children[[1]]$children), 2)
})

test_that("ec.data boxlpot", {
  
  # without grouping -------------------
  p <- mtcars |> relocate(cyl,mpg) |> ec.data(format='boxplot', outliers=TRUE)
  expect_equal(p$series[[1]]$type, 'boxplot')
  expect_equal(p$series[[1]]$datasetIndex, 2)
  expect_equal(p$dataset[[1]]$source[[1]][[3]], 22.8)
  expect_equal(p$xAxis[[1]]$name, 'mpg')
  #expect_equal(p$series[[2]]$z, 4)
  expect_equal(p$series[[2]]$encode$x, 2)
  expect_equal(p$series[[2]]$type, 'scatter')
  
  ds <- mtcars |> select(cyl, drat) |>
	ec.data(format='boxplot', jitter=0.1, layout= 'v',
  	symbolSize=5, itemStyle=list(opacity=0.9), 
  	emphasis= list(itemStyle= list(color= 'chartreuse', borderWidth=4, opacity=1))
	)
  p <- ec.init(
    #colors= heat.colors(length(mcyl)),
    legend= list(show= TRUE), tooltip= list(show=TRUE),
    dataset= ds$dataset, series= ds$series, xAxis= ds$xAxis, yAxis= ds$yAxis
  ) |> 
  ec.upd({ 
  	series[[1]] <- c(series[[1]], 
  	  list(color= 'LightGrey', itemStyle= list(color='DimGray')))
  }) |> ec.theme('dark-mushroom')
  expect_equal(p$x$opts$series[[1]]$name, 'boxplot')
  expect_equal(p$x$opts$series[[4]]$name, '8')
  #expect_match(p$x$opts$series[[4]]$tooltip$formatter, "x[1] ); return c;}", fixed=TRUE)
  expect_equal(p$x$opts$yAxis[[1]]$name, 'drat')
  expect_equal(p$x$opts$xAxis[[2]]$max, 3)

  # with grouping -------------------
  ds <- airquality |> mutate(Day=round(Day/10)) |> 
    relocate(Day,Wind,Month) |> group_by(Month) |> 
  	ec.data(format='boxplot', jitter=0.1, outliers=TRUE)
  p <- ec.init(load='custom',  # for outliers
    dataset= ds$dataset, series= ds$series,xAxis= ds$xAxis, yAxis= ds$yAxis,
    legend= list(show= TRUE), tooltip= list(show=TRUE)
  )
  expect_equal(length(p$x$opts$dataset), 15)
  expect_equal(p$x$opts$yAxis[[1]]$type, 'category')
  expect_equal(p$x$opts$series[[5]]$type, 'boxplot')
  expect_equal(p$x$opts$series[[5]]$datasetIndex, 9)
  expect_equal(p$x$opts$series[[10]]$type, 'custom')
  expect_equal(as.character(p$x$opts$series[[10]]$renderItem), 'riOutliers')
  expect_equal(p$x$opts$series[[10]]$encode$x, 1)
  expect_equal(p$x$opts$series[[14]]$type, 'scatter')
  expect_equal(p$x$opts$series[[14]]$name, '3')
})

test_that("ec.data treePC", {
  df <- as.data.frame(Titanic) |> group_by(Survived,Class) |> 
    summarise(value=sum(Freq), .groups='drop') |>
    mutate(parents= as.character(Survived), 
           children= as.character(Class)) |>
    select(parents, children, value)
  # add root to form a tree
  df[nrow(df) + 1,] <- list('survived','Yes',711)
  df[nrow(df) + 1,] <- list('survived','No', 1490)
  df[nrow(df) + 1,] <- list('root2','survived',2201)
  
  p <- ec.init(preset= FALSE,
          series= list(list(
            type= 'sunburst', 
            data= ec.data(df, format='treePC')[[1]]$children, 
            radius=c('11%', '90%')
            #,label=list(rotate='radial'), emphasis=list(focus='none')
          ))
  )
  expect_equal(p$x$opts$series[[1]]$data[[1]]$value, 711)
  expect_equal(length(p$x$opts$series[[1]]$data[[1]]$children), 4)
})

test_that("ec.data treeTK", {
  # see example https://helgasoft.github.io/echarty/uc3.html
  df <- as.data.frame(Titanic) |> rename(value= Freq) |>
    mutate(pathString= paste('Survive', Survived, Age, Sex, Class, sep='/'),
		  itemStyle= case_when(Survived=='Yes' ~ "color='green'", TRUE ~ "color='pink'")) |>
	  select(pathString, value, itemStyle)

  p <- ec.init(preset= FALSE,
  	title= list(text= 'Titanic: Survival by Class'), tooltip= list(s=TRUE),
  	series= list(list(
  	  type= 'tree', symbolSize= htmlwidgets::JS("x => {return Math.log(x)*10}"),
  	  data= ec.data(df, format='treeTK'),
    	tooltip= list(formatter= ec.clmn('%@ (%@%)', 'value','pct'))
  	))
  )
  expect_equal(p$x$opts$series[[1]]$data[[1]]$value, 2201)
})

test_that("ec.data 'names' + nasep", {
  df <- data.frame(name= c('A','B','C'), value= c(1,2,3),
        itemStyle_color= c('chartreuse','lightblue','pink'),
        itemStyle_decal_symbol= c('rect','diamond','none'),                   
        emphasis_itemStyle_color= c('green','blue','red')
  )
  p <- ec.init(series.param= list(
    type='pie', data= ec.data(df, 'names', nasep='_')))
  expect_equal(p$x$opts$series[[1]]$data[[1]]$emphasis$itemStyle$color, 'green')
  expect_equal(p$x$opts$series[[1]]$data[[2]]$itemStyle$decal$symbol, 'diamond')
  expect_equal(p$x$opts$series[[1]]$data[[3]]$itemStyle$color, 'pink')
})

test_that("ec.inspect and ec.fromJson", {
  p <- mtcars |> group_by(gear) |> 
    # param to increase coverage, no sense otherwise
    ec.init(series.param= list(dimensions= c('m','c','d'), encode= list(y='qsec'))) |> 
    ec.inspect('data')
  expect_match(p[1], "rows= 33", fixed=TRUE)
  expect_match(p[2], "filter", fixed=TRUE)

  txt <- '{
     "xAxis": { "data": ["Mon", "Tue", "Wed"]},
     "yAxis": { },
     "series": { "type": "line", "data": [150, 230, 224] } }'
  p <- ec.fromJson(txt)
  expect_equal(p$x$opts$xAxis$data[[2]], "Tue")
  
  # test renderItem and functions JS_EVAL setting
  set.seed(222)
  df <- data.frame( x = 1:10, y = round(runif(10, 5, 10),2)) |>
    mutate(lwr = y-round(runif(10, 1, 3),2), 
            upr = y+round(runif(10, 2, 4),2))
  p <- df |> ec.init(load='custom',
    legend= list(show= TRUE),
    xAxis= list(type='category', boundaryGap=FALSE),
    series= append(
      list(list(type='line', color='red', datasetIndex=1, name='line1')),
      ecr.band(df, 'lwr', 'upr', type='stack', name='stak')
    ),
    tooltip= list(trigger='axis',
                formatter=htmlwidgets::JS("(x) => 
    x.length==1 ? 'line '+x[0].value[1] :
    x.length==2 ? 'high <b>'+x[1].value[2]+'</b><br>low <b>'+x[0].value[1] :
      'high <b>'+x[2].value[2]+'</b><br>line <b>'+
      x[0].value[1]+'</b>'+'</b><br>low <b>'+x[1].value[1]"))
  )
  tmp <- p |> ec.inspect(target='full')
  expect_true(inherits(tmp, 'json'))
  #expect_true(regexpr('^\\{\\n  "x": \\{', tmp)==1)
  expect_true(regexpr('^\\{"type":"list","attributes":\\{"names":', tmp)==1)
  expect_true(grepl('dependencies', tmp))
    
  v <- ec.fromJson(tmp)   # full
  expect_true(inherits(v, 'echarty'))
  expect_equal(v$dependencies[[1]]$name, 'renderers')
  
  tmp <- p |> ec.inspect()  # opts only
  expect_true(regexpr('^\\{\\n  "legend": \\{', as.character(tmp))==1)
  tmp <- p |> ec.inspect(pretty=FALSE)
  expect_true(startsWith(tmp,'{"legend')==1)
  
  v <- ec.fromJson(tmp)
  expect_equal(v$x$opts$xAxis$type, 'category')
  p <- ec.fromJson('https://helgasoft.github.io/echarty/test/pfull.json')
  expect_true(inherits(p, 'echarty'))
})

