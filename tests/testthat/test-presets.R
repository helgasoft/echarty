isCovr <- Sys.getenv("R_COVR")!=''

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
  p <- cars |> ec.init(dbg=T, dataZoom=list(show=TRUE))
  expect_true(is.null(p$x$theme))
  # dataZoom should not produce error from r2jsEncode
  
  options(echarty.font='monospace')
  p <- cars |> ec.init()
  expect_equal(p$x$opts$textStyle$fontFamily, 'monospace')
  options(echarty.font=NULL)
  
  p <- cars |> ec.init(
    series.param= list(yAxisIndex=1, encode=list(x=1, y=2, tooltip=c(3,4))), 
    yAxis= list(gridIndex=1), visualMap= list(dimension=2),
    dataZoom= list(list(),list(yAxisIndex=1)) )
  expect_equal(p$x$opts$series[[1]]$yAxisIndex, 0)  # is decremented?
  expect_equal(p$x$opts$series[[1]]$encode$tooltip, c(2,3))
  expect_equal(p$x$opts$dataZoom[[2]]$yAxisIndex, 0)
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
  expect_equal(po$dataset[[4]]$id, "triangle")
  #expect_equal(length(po$legend$data), 3)
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
              timeline= list(show=TRUE) # data= c(1,2,3,4), axisType='value') #ok
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

test_that("ec.init presets for timeline + groupBy, geo", {
  set.seed(2022)
  dat <- data.frame(
    x1 = rep(2020:2023, each = 4),
    x2 = rep(c("A", "A", "B", "B"), 4),
    x = runif(16),
    x4 = runif(16),
    y = abs(runif(16)), z= runif(16)
  ) 
  if (isCovr) {
    p <- dat |> group_by(x1) |> ec.init( #load='3D',
      xAxis3D=list(s=TRUE), yAxis3D=list(s=TRUE), zAxis3D=list(s=TRUE), grid3D=list(s=TRUE),
      timeline=list(s=TRUE), legend= list(show=TRUE), 
      series.param= list(type='scatter3D', groupBy= 'x2',
        encode= list(x='x', y='y', z='z'), 
        symbolSize= ec.clmn('x4', scale=30) )
    )
    expect_equal(p$x$opts$options[[1]]$series[[1]]$coordinateSystem, 'cartesian3D')
    expect_equal(length(p$x$opts$options[[1]]$series), 2)
    expect_equal(p$x$opts$options[[4]]$series[[2]]$datasetIndex, 8)
    expect_equal(p$x$opts$options[[4]]$series[[2]]$name, 'B')
  }
  
  cns <- data.frame(
    value = c(22, 99, 33),
    name = c('Brazil','China','India'),
    dim = c(11, 88, 44)  # last clmn is value if there is no column 'value'
  )
  p <- cns |> group_by(name) |> 
  ec.init(load= 'world', tooltip= list(show=TRUE),   # geo
    timeline= list(show=TRUE),
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
    ec.init(load= 'world', visualMap= list(seriesIndex=1), 
      geo= list(map='world', roam=TRUE), series= list(
        list(type='map'), list(type='scatter', data=list(c(-117,32)))) )
  #expect_equal(p$x$opts$dataset[[1]]$dimensions, c("val","name","dim"))
  expect_equal(p$x$opts$series[[1]]$geoIndex, 0)
  expect_equal(p$x$opts$visualMap$max, 88)
  
  p <- quakes |> head(11) |> group_by(stations) |> 
  ec.init(load='world', timeline= list(show=TRUE),
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
  
  p <- iris |> dplyr::group_by(Species) |>    # chained
  ec.init(ctype= 'parallel', series.param= list(lineStyle= list(width=3))) |>
  ec.paxis(cols= c('Petal.Length','Petal.Width','Sepal.Width'))
  expect_equal(p$x$opts$parallelAxis[[3]]$max, 4.4)
})

test_that("presets for crosstalk", {
  library(crosstalk)
  sdf <- SharedData$new(cars)
  p <- sdf |> ec.init()
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
  vdf <- read.csv(text=tmp, header=TRUE)
  p <- vdf |> ec.init(
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
  
  p <- ec.init(geo= list(map='world'))    # autoload 'world'
  expect_equal(p$dependencies[[1]]$name, 'world')
  p <- ec.init(series.param= list(type='map', map='world')) 
  expect_equal(p$dependencies[[1]]$name, 'world')
  p <- ec.init(series= list(list(type='map', map='world')))
  expect_equal(p$dependencies[[1]]$name, 'world')
})

test_that("presets for lottieParser and ecStat", {
  url= 'https://helgasoft.github.io/echarty/test/pinJump.json'
  cont <- jsonlite::fromJSON(url, simplifyDataFrame=FALSE)
  # set lottie animation as graphic item in echarty
  p <- cars |> ec.init( load= 'lottie',
  	graphic= list(elements= list(
  		 list( type= "group",  # lottie params qre info + optional (scale, loop, etc.)
  		 	info= cont, scale= 250, left= 'center', bottom= '25%'  #,rotation= -20
  		 )
  	)) 
  )
  expect_equal(p$dependencies[[1]]$name, 'lottieParser')
  
  s1 <- list(type='scatter', symbolSize=15, datasetIndex=1)
  s2 <- list(type='line', symbolSize=0, smooth=TRUE, color='red', datasetIndex=2)
  p <- ec.init(load= 'ecStat',
    #js= c('echarts.registerTransform(ecStat.transform.regression)','',''),
    dataset= list(
      list(source= ec.data(matrix(runif(18)*5, ncol= 2))),
      list(transform= list(type='ecStat:regression', 
                           config= list(method= 'polynomial', order= 3)) )
    ),
    xAxis=list(show=TRUE), yAxis=list(show=TRUE), tooltip= list(position='top'),
    series= list(s1, s2)
  )
  expect_equal(p$dependencies[[1]]$name, 'ecStat')
  expect_equal(length(unlist(strsplit(p$x$jcode[[1]], 'registerTransform', fixed=TRUE))), 5)
  p <- ec.init(load= 'ecStat', js='a=1;')
  expect_equal(length(unlist(strsplit(p$x$jcode[[1]], 'registerTransform', fixed=TRUE))), 5)
  p <- ec.init(load= 'ecStat', js=c('a=1;','b=2',''))
  expect_equal(length(unlist(strsplit(p$x$jcode[[1]], 'registerTransform', fixed=TRUE))), 5)
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
      series.param= list(type='line', smooth=TRUE))
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'polar')
  dd <- data.frame(
    c1 = rep(1:3, each= 2),
    c2 = c(0,1,2,3,2,1),
    c3 = rep(c('d1', 'd2'), 3)
  )
  p <- ec.init(series.param= list(
    type='themeRiver', data= ec.data(dd), label= list(s=TRUE) ) )
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
  
})

test_that('polar presets', {
  p <- data.frame(x = 1:10, y = seq(1, 20, by = 2)) |>
  ec.init(ctype='line', polar= list(dummy= T), 
  		 series.param= list(smooth= T)
  )
  expect_equal(p$x$opts$polar$radius, 111)
  expect_equal(p$x$opts$radiusAxis$type, 'category')
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'polar')
})

test_that('style-to-column feature', {
  
  p <- cars |> mutate(    # non-grouped
      clr= sample(c('darkgreen','blue','red'), 50, TRUE),
      opa= sample(c(0.3, 0.6, 0.9), 50, TRUE), value= 1:50
    ) |>
  ec.init( dbg=TRUE,
    series.param= list(symbolSize=22, encode= list(data= list(
      value=c('speed', 'dist'), 333,   # 333 is illegal R, for covr only
      label= list(show=TRUE, formatter='speed'),
      itemStyle= list(color='clr', opacity='opa')) ))
  )
  expect_true(is.null(p$x$opts$dataset))
  #expect_true(is.null(p$x$opts$series[[1]]$encode))
  expect_equal(length(p$x$opts$series[[1]]$data), 50)
  expect_equal(p$x$opts$series[[1]]$data[[50]]$value, list(25,85))
  expect_equal(p$x$opts$series[[1]]$data[[50]]$label$formatter, 25)
  
  p <- iris |> group_by(Species) |>    # grouped
    ec.init(tooltip=list(s=TRUE), series.param= list(
    encode= list(data= list(value= c('Sepal.Length','Sepal.Width'), 
                            itemStyle= list(opacity='Petal.Width')) )
  ))
  expect_equal(p$x$opts$series[[1]]$data[[2]]$value[[1]], p$x$opts$series[[1]]$data[[2]]$Sepal.Length)
  expect_equal(p$x$opts$series[[1]]$data[[2]]$itemStyle$opacity, 0.2)
  expect_true(is.null(p$x$opts$series[[1]]$encode))
  expect_true(is.null(p$x$opts$dataset))

})

test_that('preset axis type', {
  p <- ec.init( dataset= list(list(source= c(list(as.list(colnames(df))), ec.data(df)) )))
  expect_equal(p$x$opts$xAxis$type, 'category')   # sourceHeader

  p <- data.frame(
    time = seq(from = as.POSIXct("2021-01-01 08:00:00"), to = as.POSIXct("2021-01-01 09:10:00"), by = "1 min"),
    y = rnorm(71, mean = 100)
  ) |> ec.init(yAxis= list(scale=TRUE))
  expect_equal(p$x$opts$xAxis$type, 'time')
  expect_equal(p$x$opts$yAxis$type, 'value')

  p <- ec.init(dbg=T,
    series.param = list( encode= list(x=2, y=1),
    data= list(list(value= list(1,'B')), list(value=list(3,'A')), list(value=list(2, 'C')) )
  ))
  expect_equal(p$x$opts$xAxis$type, 'category')
  
  dd <- lapply(1:nrow(df), \(ii) {
    list(value= list(df[ii,]$name, df[ii,]$size, df[ii,]$symbol))
  })
  p <- ec.init(series.param= list( data= dd) )
  expect_equal(p$x$opts$xAxis$type, 'category')
  p <- ec.init(series.param= list( encode= list(x=3, y=2), data=dd) )
  expect_equal(p$x$opts$xAxis$type, 'category')
  expect_equal(length(p$x$opts$series[[1]]$data[[1]]$value), 3)
  p <- ec.init(series.param= list( encode= list(x=2, y=1), data=dd))
  expect_equal(p$x$opts$yAxis$type, 'category')
  
  p <- ec.init(df, series.param= list( encode= list( data= list(value=c('name','size'), symbol='symbol')) ))
  expect_equal(length(p$x$opts$series[[1]]$data[[10]]$value), 2)
  expect_equal(p$x$opts$series[[1]]$data[[10]]$symbol, 'triangle')
  p <- df |> rename(value= size) |>  
    ec.init( tooltip= list(show=TRUE), series.param= list( encode= list(
      data= list(name='name', symbol='symbol')) ))
  expect_equal(p$x$opts$series[[1]]$data[[1]]$name, 'G')
  expect_true(is.null(p$x$opts$dataset))
  p <- ec.init(dataset= list(list(source= ec.data(df, 'names'))),
        series.param= list(encode= list(x=2, y=1) )) 
  expect_equal(p$x$opts$yAxis$type, 'category')
  expect_equal(p$x$opts$dataset[[1]]$source[[10]]$symbol, 'triangle')

  p <- Orange |> mutate(opa= circumference/200) |>   # Tree is factor
    ec.init(series.param= list(encode= list(data= 
        list(value= c('Tree','circumference'), itemStyle= list(opacity='opa'))))
    )
  expect_equal(p$x$opts$xAxis$type, 'category')
  expect_equal(p$x$opts$series[[1]]$data[[1]]$itemStyle$opacity, 0.15)
})

