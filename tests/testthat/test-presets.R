
library(dplyr)
set.seed(2021)
df <- data.frame(
  name = sample(LETTERS, 10),
  size = rnorm(10, 10, 2),
  symbol = sample(c("circle", "rect", "triangle"), 10, replace= TRUE)
)

test_that("options preset", {
  options(echarty.theme='jazz')
  p <- ec.init()
  expect_equal(p$x$theme, 'jazz')
  expect_equal(p$dependencies[[1]]$name, 'jazz')
  
  p <- cars |> ec.init() |> ec.theme(name='mine', code='{ "backgroundColor": "pink" }')
  expect_equal(p$x$theme, 'mine')
  
  options(echarty.theme=NULL)
  p <- cars |> ec.init(dbg=T)
  expect_equal(p$x$theme, '')
  
  options(echarty.font='monospace')
  p <- cars |> ec.init()
  expect_equal(p$x$opts$textStyle$fontFamily, 'monospace')
  options(echarty.font=NULL)
  
  p <- cars |> ec.init(series.param= list(yAxisIndex=1), 
          yAxis= list(gridIndex=1), visualMap= list(dimension=2))
  expect_equal(p$x$opts$series[[1]]$yAxisIndex, 0)  # is decremented?
  expect_equal(p$x$opts$yAxis$gridIndex, 0)
  expect_equal(p$x$opts$visualMap$dimension, 1)
  expect_equal(p$x$opts$dataset[[1]]$dimensions, c('speed','dist'))
})

test_that("webR works with plugins", {
  lif <- paste0(system.file('js', package='echarty'), '/echarts-liquidfill.min.js')
  ec.webR <<- TRUE
  tmp <- ec.init(load= 'liquid')
  expect_false(file.exists(lif))
  rm(ec.webR, envir=globalenv())
})

test_that("ec.init presets for non-grouped data.frame", {
  p <- df |> ec.init(xAxis= list(scale=TRUE))
  expect_equal(p$x$opts$xAxis$type, 'category')
  #expect_true(is.null(p$x$opts$xAxis$type))  # assume default='category' = WRONG
  expect_true(!is.null(p$x$opts$yAxis))
  expect_equal(length(p$x$opts$dataset[[1]]$source), 10)
  expect_equal(p$x$opts$series[[1]]$type, 'scatter')
})

test_that("ec.init presets for grouped data.frame", {
  p <- df |> dplyr::group_by(symbol) |> ec.init(yAxis= list(scale=TRUE, name='yaxe'), 
                  series.param= list(symbol=ec.clmn('symbol')))
  po <- p$x$opts
  expect_equal(po$xAxis$type, 'category')
  expect_equal(po$yAxis$name, 'yaxe')
  expect_equal(length(po$dataset[[1]]$source), 10)
  expect_equal(po$dataset[[4]]$transform$id, "triangle")
  expect_equal(length(po$legend$data), 3)
  expect_equal(po$series[[1]]$type, 'scatter')
  expect_equal(po$series[[1]]$name, 'circle')
  expect_s3_class(po$series[[2]]$symbol, 'JS_EVAL')
  expect_equal(po$series[[2]]$datasetIndex, 2)
  # group_by does not overwrite series ?
  p <- df |> dplyr::group_by(symbol) |> ec.init(series= list(list(type='bar' )))
  expect_equal(p$x$opts$series[[1]]$type, 'bar')
})

test_that("ec.init presets for timeline", {
  # TODO 'timeline= list(data=,axisType=)'...
  dftl <- data.frame(
    value = runif(16),
    quarter = as.factor(rep(1:4, 4)),
    year = unlist(lapply(2018:2021, function(x) {rep(x, 4)}))
  )
  barTL <- function(data, timeline_var) {
    bt <- data |> dplyr::group_by(!!dplyr::sym(timeline_var)) |> 
      ec.init(series.param = list(no_type='bar'), 
              xAxis= list(name='xval'),
              timeline= list(show=T) # data= c(1,2,3,4), axisType='value') #ok
      )
    bt
  }
  p <- barTL(dftl, timeline_var= "year")
  o <- p$x$opts
  expect_equal(length(o$dataset[[1]]$source), 16)
  expect_equal(length(o$dataset), 5)
  expect_equal(length(o$options), 4)
  expect_equal(o$timeline$axisType, 'category')
  expect_equal(o$yAxis$name, 'quarter')
  expect_equal(o$xAxis$name, 'xval')
  expect_equal(o$options[[1]]$series[[1]]$encode, list(x=0, y=1))
  expect_equal(o$options[[1]]$series[[1]]$type, 'scatter')
})

test_that("ec.init presets for timeline groupBy", {
  set.seed(2022)
  dat <- data.frame(
    x1 = rep(2020:2023, each = 4),
    x2 = rep(c("A", "A", "B", "B"), 4),
    x = runif(16),
    x4 = runif(16),
    y = abs(runif(16)), z= runif(16)
  ) 
  p <- dat |> group_by(x1) |> ec.init(
    legend= list(show=TRUE), timeline= list(show=T),
    series.param= list(encode= list(x= 'x', y= 'y'), 
        symbolSize= ec.clmn('x4', scale=30), groupBy= 'x2') 
  )
  expect_equal(p$x$opts$options[[4]]$series[[1]]$type, 'scatter')
  expect_equal(p$x$opts$options[[4]]$series[[1]]$encode$y, 'y')
  expect_equal(p$x$opts$yAxis$name, 'y')

  p <- dat |> group_by(x1) |> ec.init( #load='3D',
    xAxis3D=list(s=T),yAxis3D=list(s=T),zAxis3D=list(s=T),grid3D=list(s=T),
    timeline=list(s=T), legend= list(show=TRUE), 
    series.param= list(type='scatter3D', groupBy= 'x2',
      encode= list(x='x', y='y', z='z'), 
      symbolSize= ec.clmn('x4', scale=30) )
  )
  expect_equal(p$x$opts$options[[1]]$series[[1]]$coordinateSystem, 'cartesian3D')
  expect_equal(length(p$x$opts$options[[1]]$series), 2)
  expect_equal(p$x$opts$options[[4]]$series[[2]]$datasetIndex, 8)
  expect_equal(p$x$opts$options[[4]]$series[[2]]$name, 'B')
  
  cns <- data.frame(
    value = c(22, 99, 33),
    name = c('Brazil','China','India'),
    dim = c(11, 88, 44)  # last clmn is value if there is no column 'value'
  )
  p <- cns |> group_by(name) |> 
  ec.init(load= 'world', tooltip= list(show=T),   # geo
    timeline= list(show=T),
    series.param= list(type='map'), #encode= list(name='nam', value='val')), 
    visualMap= list()
  )

  # name & value are required column names for tl.series
  expect_equal(p$x$opts$options[[3]]$series[[1]]$geoIndex,0)  # decremented
  #expect_equal(p$x$opts$options[[1]]$series[[1]]$data[[1]]$name, 'Brazil')
  expect_equal(p$x$opts$options[[1]]$series[[1]]$datasetIndex, 1)
  expect_equal(p$x$opts$geo$map, 'world')
  expect_equal(p$x$opts$visualMap$max, 99)
  
  # map defaults:
  # 1. map serie picks up first num clmn for value, first char clmn for name
  # 2. visualMap picks up last numeric clmn for max/min, but 'name' as last clmn doesn't work
  p <- cns |> rename(val=value) |>
    ec.init(load= 'world', series.param= list(type='map'), 
            visualMap= list(seriesIndex=1))
  #expect_equal(p$x$opts$dataset[[1]]$dimensions, c("val","name","dim"))
  expect_equal(p$x$opts$series[[1]]$geoIndex, 0)
  expect_equal(p$x$opts$visualMap$max, 88)
  
  p <- quakes |> head(11) |> group_by(stations) |> 
  ec.init(load='world', timeline= list(show=T),
	  series.param= list(type='scatter', itemStyle= list(color='brown'),
	    encode= list(lng=2, lat=1, value=3)
		  #encode= list(lng='long', lat='lat', value='mag')  #ok
	) )
  expect_equal(length(p$x$opts$options), 8)
  expect_equal(p$x$opts$options[[2]]$series[[1]]$datasetIndex, 2)
  expect_equal(p$x$opts$options[[2]]$series[[1]]$coordinateSystem, 'geo')
})

test_that("presets for parallel chart", {
  p <- mtcars |> relocate(cyl, .after=last_col()) |> group_by(cyl) |> ec.init(ctype='parallel')

  expect_equal(length(p$x$opts$dataset), 4)
  expect_equal(p$x$opts$series[[3]]$datasetIndex, 3)
  expect_equal(p$x$opts$parallelAxis[[2]]$name, 'disp')
})

test_that("presets for crosstalk", {
  library(crosstalk)
  df <- cars
  df <- SharedData$new(df)
  p <- df |> ec.init()
  expect_equal(p$x$opts$dataset[[2]]$id, 'Xtalk')
  expect_equal(p$x$opts$series[[1]]$datasetId, 'Xtalk')
  expect_equal(p$x$opts$dataset[[1]]$dimensions[3], 'XkeyX')
  
  tmp <- SharedData$new(longley, key=~Year)  # without XkeyX
  p <- tmp |> ec.init( xtKey='Year',
    parallelAxis= ec.paxis(longley, cols=c('Year','GNP')),
    series= list(list(type='parallel'))
  )
  expect_equal(p$x$opts$dataset[[2]]$transform$config$dimension, 'Year')
})

test_that("presets for leaflet/world", {
  tmp <- '
lng,lat,name,date,place
-118.808101,32.843715,"Seabed","2021-02-02","location A"
-117.332678,34.845565,"Lancaster","2021-04-02","location A"
-116.127504,32.846118,"fwy #8","2021-04-02","place B"
-117.316886,30.961700,"Baja","2021-07-02","place B"
'
  df <- read.csv(text=tmp, header=TRUE)
  p <- df |> ec.init(
    load='leaflet', tooltip= list(show=TRUE),
    series= list(list(
      encode= list(tooltip=c(3,4,5))
    ))
  )
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'leaflet')
  expect_equal(p$x$opts$series[[1]]$encode$tooltip, c(2,3,4))
  expect_equal(p$dependencies[[1]]$name, 'leaflet')
  
  p <- ec.init(quakes |> head(11), load='world',
	  series.param= list( encode= list(lng=2, lat=1, value=3),
		#encode= list(lng='long', lat='lat', value='mag'),
		itemStyle= list(color='brown')) )
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'geo')
  expect_equal(p$x$opts$geo$map, 'world')
})

test_that("presets with series.param", {
  p <- df |> ec.init(ctype='line', 
    series.param= list(symbol='pin', encode=list(x='size',y='name')))
  expect_equal(p$x$opts$series[[1]]$symbol, 'pin')
  expect_equal(p$x$opts$yAxis$type, 'category')
  p <- ec.init(ctype='line', 
     series.param= list(areaStyle= list(show= T), stack= 'stk', 
                        data= list(c(0,0), c(2,2)))
  )
  expect_equal(p$x$opts$series[[1]]$data[[2]], c(2,2))
  p <- df |> relocate(symbol) |> group_by(symbol) |> 
    ec.init(series.param= list(encode= list(x=3, y=2)))
  expect_equal(p$x$opts$yAxis$name, 'name')
  p <- data.frame(name=c('Brazil','Australia'), value=c(111,222)) |>
    ec.init(load='world', series.param= list(type='map'), visualMap=list())
  expect_equal(p$x$opts$visualMap$max, 222)
})

test_that("presets for visualMap", {
  p <- df |> ec.init(visualMap= list(dimension= 2, inRange= list(color= c("blue", "red"))) )
  expect_equal(p$x$opts$visualMap$dimension, 1)
  expect_equal(round(p$x$opts$visualMap$min,2), 8.66)
})

test_that('axis names from preset encode', {
  tmp <- cars |> mutate(group = sample( c(1,2), 50, replace = TRUE)) |> 
    relocate(group) |> group_by(group)    # group is 1st col
  p <- tmp |> ec.init(xAxis= list(gridIndex=1)) |> ec.plugjs()  # 4 coveralls
  expect_equal(p$x$opts$xAxis$name, 'speed')
  expect_equal(p$x$opts$yAxis$name, 'dist')
  p <- tmp |> ec.init(series.param= list(encode= list(x='dist', y='speed')))
  expect_equal(p$x$opts$yAxis$name, 'speed')
  p <- tmp |> ec.init(series.param= list(encode= list(x=3, y=2)))
  expect_equal(p$x$opts$xAxis$name, 'dist')
})

test_that('polar, pie, radar, themeRiver, parallel, etc.', {
  p <-  df |> dplyr::group_by(name) |> ec.init(ctype='pie')
  expect_equal(p$x$opts$series[[1]]$encode, list(value=1, itemName=2))
  p <- cars |> ec.init(polar= list(radius= 222), 
      series.param= list(type='line', smooth=T))
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'polar')
  dd <- data.frame(
    c1 = rep(1:3, each= 2),
    c2 = c(0,1,2,3,2,1),
    c3 = rep(c('d1', 'd2'), 3)
  )
  p <- ec.init(series.param= list(
    type='themeRiver', data= ec.data(dd), label= list(s=T) ) )
  expect_equal(p$x$opts$singleAxis, list(min='dataMin', max='dataMax'))
  
  p <- ec.init(
  	radar= list(indicator= lapply(LETTERS[1:5], \(x){list(name= x)}) ),
  	series.param= list(type='radar', data= list(list(name='r1', value= runif(5, 1, 5))) )
  )
  expect_equal(p$x$opts$series[[1]]$type, 'radar')
  
    # group column to be last
  p <- mtcars |> relocate(cyl, .after= last_col()) |> group_by(cyl) |> 
    ec.init(ctype='parallel')
  expect_equal(length(p$x$opts$series), 3)
  expect_equal(length(p$x$opts$parallelAxis), 10)
  expect_equal(p$x$opts$parallelAxis[[1]]$name, 'mpg')
  
  p <- ec.init(series.param= list(
    type='gauge', data= list(list(name='score',value=44))))
  expect_equal(names(p$x$opts), 'series')
  
  p <- mtcars |> ec.init(ctype='scatterGL')
  expect_equal(p$dependencies[[1]]$name, 'echarts-gl.min.js')
})

test_that('polar presets', {
  df <- data.frame(x = 1:10, y = seq(1, 20, by = 2))
  p <- df |> ec.init(ctype='line', polar= list(dummy= T), 
  		 series.param= list(smooth= T)
  )
  expect_equal(p$x$opts$polar$radius, 111)
  expect_equal(p$x$opts$radiusAxis$type, 'category')
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'polar')
})
