#isCovr <- interactive()
#isCovr <- Sys.getenv("COVERALLS_TOKEN")!=''
isCovr <- Sys.getenv("R_COVR")!=''
#cat('\n isCovr=',Sys.getenv("R_COVR"),'\n')

test_that("registerAll + custom-series v.6", {
  theme1= '{"color": ["green","#eeaa33"], "backgroundColor": "lemonchiffon"}'
  p <- cars |> ec.init(
    theme='mine', registerTheme=list(name='mine', theme=jsonlite::fromJSON(theme1))
  )
  expect_equal(p$x$registerTheme$theme$backgroundColor, 'lemonchiffon')
  
  p <- cars |> ec.init(theme='dark')    # as string
  expect_equal(p$x$theme, 'dark')
  expect_equal(p$dependencies[[1]]$name, 'dark')

  p <- cars |> ec.init(theme= jsonlite::fromJSON(theme1))   # as object
  expect_equal(class(p$x$theme), 'list')
  
  p <- ec.init(
    load= 'https://cdn.jsdelivr.net/gh/apache/echarts-custom-series@main/custom-series/segmentedDoughnut/dist/index.auto.js',
    ask= 'loadRemote',
    series.param= list(
      type= 'custom', renderItem= 'segmentedDoughnut',
      coordinateSystem= 'none',
      itemPayload= list(
        radius= list('50%','65%'), segmentCount= 8,
        label= list(show=T, formatter= '{c}/{b}', fontSize=35, color= '#555')
      ),
      data= list(5) )
  )
  expect_equal(class(p$x$opts$series[[1]]$renderItem), 'character')
  expect_true(grepl('segmented', p$dependencies[[1]]$src$href))
})
  
test_that("registerMap", {
  # similar in ec.examples, with USA map
  gjson <- jsonlite::parse_json('{"type":"FeatureCollection", "properties":{"id":"all3"},
  "features":[
     {"type":"Feature", "geometry":{"type":"MultiPolygon", "coordinates":[[[[2.330466,48.862223],[2.330305,48.861636],[2.329572,48.861581],[2.329466,48.861531],[2.329413,48.861528],[2.328796,48.861475],[2.328545,48.861396],[2.328466,48.861404],[2.328361,48.86137]]]]},
    	"properties":{"lat":48.859475,"lon":2.329466,"name":"bic1","range":500, "id":"0.5 min", "ppfill": "#00FF0077"} },
     {"type":"Feature", "geometry":{"type":"MultiPolygon", "coordinates":[[[[2.333466,48.866204],[2.333061,48.86588],[2.332897,48.865906],[2.332466,48.865801],[2.332165,48.865776],[2.331811,48.865475],[2.331621,48.86532],[2.331466,48.865265],[2.331274,48.865283]]]]},
     	"properties":{"lat":48.859475,"lon":2.329466,"name":"bic2","range":1000, "id":"1 min"} },
     {"type":"Feature", "geometry":{"type":"MultiPolygon", "coordinates":[[[[2.335466,48.869736],[2.335037,48.870046],[2.334836,48.870105],[2.334466,48.870265],[2.334289,48.870298],[2.333577,48.870364],[2.333466,48.870381],[2.333364,48.870373],[2.332485,48.870456]]]]},
     	"properties": {"lat":48.859475,"lon":2.329466,"name":"bic3","range":1500, "id":"1.5 min"} }
  ]}')
  ext <- function(dd) { unlist(unname(sapply(gjson$features, \(f) {f$properties[dd]}))) }
  #vals <- ext('range')
  dparis <- data.frame(name= ext('name'), value= ext('range'))
  p <- ec.init(preset= FALSE,
    geo= list(map= 'paris', roam= TRUE),
    series =list(list(
      type= 'map', geoIndex=1,
      data= ec.data(dparis, 'names')
    )),
    visualMap= list(type='continuous', calculable=TRUE,
      inRange= list(color = rainbow(8)) )
      #,min= min(vals), max= max(vals))
  )
  p$x$registerMap <- list(list(mapName= 'paris', geoJSON= gjson))
  p
  expect_equal(length(p$x$registerMap[[1]]$geoJSON), 3)
  expect_equal(p$x$opts$geo$map, 'paris')
  expect_equal(p$x$opts$series[[1]]$data[[2]]$value, 1000)
})

test_that("tl.series, timeline options, groupBy", {  # also in test-presets
  p <- Orange |> dplyr::group_by(age) |> ec.init(
    timeline= list(autoPlay=TRUE), title= list(text='age %@ days'),
    series.param= list(type='bar', encode=list(x='Tree', y='circumference'))
  ) # |> ec.upd({
  #   options <- lapply(seq_along(options), \(i) { 
  #       options[[i]]$title$text <- paste('age',timeline$data[[i]],'days'); 
  #       options[[i]] })
  # })
  expect_equal(p$x$opts$options[[5]]$title$text, "age 1231 days")
  expect_equal(p$x$opts$options[[5]]$series[[1]]$datasetIndex, 5)
  expect_equal(p$x$opts$options[[7]]$series[[1]]$encode$x, "Tree")
  expect_equal(p$x$opts$timeline$data[[5]], "1231")
  expect_true(p$x$opts$dataset[[5]]$transform$config['='] == 1004)
  
  set.seed(2022)
  dat <- data.frame(
    x1 = rep(2020:2023, each = 4),
    x2 = rep(c("A", "A", "B", "B"), 4),
    x3 = runif(16), x4 = runif(16), x5 = abs(runif(16))
  )
  p <- dat |> group_by(x1) |> ec.init(    # it's tl.series OR series.param + timeline
    series.param= list(encode= list(x= 'x3', y= 'x5'), groupBy='x2',
                    symbolSize= ec.clmn('x4', scale=30)),
    timeline= list(show=TRUE), legend= list(show=TRUE)
  )
  expect_equal(p$x$opts$options[[4]]$series[[2]]$name, 'B')
  expect_true(p$x$opts$dataset[[9]]$transform$config$and[[2]]$dimension=='x2')
  expect_equal(p$x$opts$options[[4]]$series[[1]]$type, 'scatter')
  expect_equal(p$x$opts$options[[4]]$series[[1]]$encode$y, 'x5')
  expect_equal(p$x$opts$yAxis$name, 'x5')
  
  p <- data.frame(name=c('Brazil','Australia'), value=c(111,222)) |> group_by(name) |>
  ec.init(load= 'world', title= list(text='map %@'), visualMap=list(show=TRUE), 
        timeline= list(show=TRUE), series.param= list(type='map') )
  expect_equal(p$x$opts$options[[2]]$title$text, "map Brazil")
  expect_equal(p$x$opts$visualMap$min, 111)

})

test_that("leaflet with ec.clmn and timeline", {

  tmp <- quakes |> dplyr::relocate('long') |>  # set order to lon,lat
      dplyr::mutate(size= exp(mag)/20) |> head(100)  # add accented size
  p <- tmp |> ec.init(load= 'leaflet',
    tooltip = list(formatter= ec.clmn('magnitude %@', 'mag')),
    series.param= list(datasetIndex= 1, symbolSize= ec.clmn(6, scale=3) )
  )
  expect_equal(p$x$opts$leaflet$zoom, 6)
  expect_equal(p$x$opts$series[[1]]$datasetIndex, 0)  # decremented
  expect_s3_class(p$x$opts$tooltip$formatter, 'JS_EVAL')
  expect_true(is.null(p$x$opts$xAxis))   # no axes added

  p <- tmp |> group_by(stations) |> ec.init(load='leaflet', 
    legend= list(show=TRUE), tooltip = list(formatter=ec.clmn('magnitude %@', 'mag')),
  	leaflet= list(center= c(179.462,-20), zoom= 5, roam=TRUE),
  	timeline= list(autoPlay=F, axisType='value', 
  	               data= unique(sort(tmp$stations)),
  	               controlStyle= list(borderColor='brown')),
    series.param= list(
        type='scatter', name='quake',
        symbolSize = ec.clmn(6, scale=3),
        encode= list(lng='long', lat='lat')
    ),
    visualMap= list(
        show= FALSE, top= 'top', dimension=4,
        calculable= TRUE, inRange= list(color= c('blue','red'))
    )
  )
  expect_equal(p$x$opts$leaflet$zoom, 5)
  expect_s3_class(p$x$opts$tooltip$formatter, 'JS_EVAL')
  expect_equal(p$dependencies[[1]]$name, 'leaflet')
  #expect_equal(p$x$opts$options[[10]]$title$text, '19')
  expect_equal(p$x$opts$options[[10]]$series[[1]]$name, 'quake')
  expect_true (p$x$opts$legend$show)
  expect_equal(p$x$opts$options[[1]]$series[[1]]$coordinateSystem, 'leaflet')
  expect_equal(p$x$opts$timeline$data[[1]], 10)
  expect_equal(p$x$opts$dataset[[2]]$transform$config$dimension, 'stations')
  expect_equal(p$x$opts$dataset[[2]]$transform$config$`=`, 10)
})

test_that("ec.data treeTK", {

  df <- as.data.frame(Titanic) |> 
    group_by(Survived,Age,Sex,Class) |> 
    summarise(value= sum(Freq), .groups= 'drop') |> 
    rowwise() |>
    mutate(pathString= paste('Survive', Survived, Age, Sex, Class, sep='/'),
           itemStyle= case_when(Survived=='Yes' ~ "color='green'", TRUE ~ "color='pink'")) |>
    select(pathString, value, itemStyle)
  
  dat <- ec.data(df, format='treeTK')
  dat[[1]] <- within(dat[[1]], { itemStyle <- list(color= 'white'); pct <- 0 })
  
  p <- ec.init(preset= FALSE, 
      title= list(text= 'Titanic: Survival by Class'),
      tooltip= list(formatter= ec.clmn('%@%','pct')),
      series= list(list(
        type= 'sunburst', radius= c(0, '90%'), label= list(rotate=0),
        # type= 'tree', symbolSize= ec.clmn(scale=0.08),
        # type= 'treemap', upperLabel= list(show=TRUE, height=30), itemStyle= list(borderColor= '#999'), #leafDepth=4,
        data= dat,
        emphasis= list(focus='none') 
      ))
  )
  expect_equal(p$x$opts$series[[1]]$data[[1]]$value, 2201)
  expect_equal(length(p$x$opts$series[[1]]$data[[1]]$children), 2)
  expect_equal(p$x$opts$series[[1]]$data[[1]]$children[[2]]$pct, 32.3)
})

test_that("3D globe & autoload 3D", {
  
  if (isCovr) {
    # first time will load echarts-gl.js in source folder 'js'
    p <- ec.init( #load='3D',  # test autoload
      globe= list(viewControl= list(autoRotate= FALSE)),
      series.param= list(type= 'scatter3D',
          data= list(c(32,-117,11), c(2,44,22)) ,
          symbolSize= 40, itemStyle= list(color= 'red')
      )
    )
    lif <- paste0(system.file('js', package='echarty'), '/echarts-gl.min.js')
    expect_true(file.exists(lif))
    expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'globe')
    
    data <- list()   # volcano is a lot of data ==slow
    for(y in 1:dim(volcano)[2]) for(x in 1:dim(volcano)[1])
      data <- append(data, list(c(x, y, volcano[x,y])))
    p <- ec.init(load= '3D', series.param= list(type= 'surface', data= data) )
    expect_equal(length(p$x$opts$series[[1]]$data), 5307)
    cat('\n load 3D + volcano')
  
    p <- ec.init(load='world', geo3D= list(map= 'world', roam=TRUE),
      series.param= list(type= 'scatter3D', data=list(c(115, 22, 10), c(-116, 32, -11)))
    ) 
    expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'geo3D')
    
    p <- mtcars |> ec.init(series.param= list(type='scatterGL'))
    expect_equal(p$dependencies[[1]]$name, 'echarts-gl.min.js')
  } 
  else {
    lif <- paste0(system.file('js', package='echarty'), '/echarts-gl.min.js')
    expect_false(file.exists(lif))
  }
})

test_that("radar and polar", {
  p <- ec.init(
  	radar= list(indicator= lapply(LETTERS[1:5], \(x) { list(name=x)} ) ),
  	series= list(list(type='radar', radarIndex=1,
  	                  data= list(c(10,22,5,9,11), c(12,18,15,15,7))))
  )
  expect_equal(p$x$opts$series[[1]]$radarIndex, 0)

  args <- list( df= data.frame(x= 1:10, y= seq(1, 20, by=2)),
    polar= list(show=TRUE), series.param= list(type='line', polarIndex=1) )
  p <- do.call(ec.init, args)
  expect_equal(p$x$opts$series[[1]]$polarIndex, 0)
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, "polar")
  
  args$series.param$polarIndex <- NULL
  args$series.param$coordinateSystem <- "polar"
  p <- do.call(ec.init, args)
  expect_equal(p$x$opts$radiusAxis$type, 'category')
})
  
test_that("calendar", {

  df <- data.frame(d= seq(as.Date("2023-01-01"), by="day", length.out=360), v=runif(360,1,100))
  p <- df |> ec.init( 
    visualMap= list(show= FALSE, min= 0, max= 100), 
    calendar = list(range= c('2023-01','2023-04')),
    series.param= list(type= 'scatter', name='scat', symbolSize=11, calendarIndex=1)
  )
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, "calendar")
  expect_equal(p$x$opts$series[[1]]$calendarIndex, 0)
})

test_that("ec.plugjs", {
  # .valid.url exits gracefully
  p <- ec.init() |> ec.plugjs('http://does.not.exist.com')
  expect_true(startsWith(p$x$opts$title$text, 'ec.plugjs:'))
  
  if (isCovr) {
  p <- ec.init() |> ec.plugjs(
    'https://raw.githubusercontent.com/apache/echarts/master/test/data/map/js/china-contour.js')
  expect_equal(p$dependencies[[1]]$name, "china-contour.js")
  
  p <- ec.init( preset=FALSE,    # for covr
    load= c('https://maps.googleapis.com/maps/api/js',
      'https://cdn.jsdelivr.net/npm/echarts-extension-gmap@latest/dist/echarts-extension-gmap.min.js'),
    gmap= list( center= c(108.39, 39.9), zoom= 3),
    series= list(list(type='scatter', coordinateSystem='gmap', data= list(
      list(name='岳阳', value=c(113.09,29.37)),
      list(name='吉林', value=c(126.57,43.87))
    )))
  )
  expect_equal(p$x$opts$gmap$zoom, 3)
  
  df <- mtcars |> mutate(name=rownames(mtcars)) |> relocate(mpg,wt,hp) |> group_by(cyl) 
  p <- df |> ec.init(load='3D')
  expect_equal(p$x$opts$series[[1]]$type, 'scatter3D')
  }
})

test_that("Shiny commands", {
  # coveralls.io and codecov cannot run tests on Shiny code, here is a workaround
  
  # ui <- fluidPage(column(width= 12, ecs.output('sash')), actionButton('adds', 'Upd') )
  # tmp <- ui[[4]][[1]]$children[[1]]$children[[1]][[1]]$attribs
  # expect_equal(tmp$id, 'sash')
  # expect_match(tmp$class, '^echarty ')
  tmp <- attributes(ecs.output('sash'))
  p <- sapply(tmp$html_dependencies, c)
  expect_equal(unlist(p[1,]), c("htmlwidgets","echarty","echarty-binding"))
  
  tmp <- ecs.render({ p <- cars |> ec.init() })
  expect_match(as.character(attributes(tmp)$cacheHint$origUserFunc$body[2]), "p <- ec\\.init\\(cars\\)")
  
  p <- ecs.proxy('sash')
  expect_equal(p$id, 'sash')
  expect_equal(attributes(p)$class, 'ecsProxy')
  p$x <- NULL
  expect_error(ecs.exec(p))
  
  # works in interactive only (+Shiny session), else "attempt to apply non-function"
  # #sendCustomMessage <- \(name,plist) {a <- 1}
  # p$session <- NULL   # disable p$session$sendCustomMessage
  # p$x$opts$test <- 'sankey'
  # tmp <- ecs.exec(p)
  # expect_equal(tmp$x$opts$test, 'sankey')
  tmp <- attributes(ecs.output('sash'))
  p$session <- NULL   # disable p$session$sendCustomMessage
  p$x$opts$renderer <- 'canvas'
  p$x$opts$series <- list(list(type='gauge'))
  p$dependencies <- tmp$html_dependencies
  ecs.exec(p)
})

test_that(".merlis", {
  aa = list(list(type= "map", geoIndex= 0))
  p <- echarty:::.merlis(aa, list(val= 13))
  expect_equal(p[[1]]$val, 13)
  p <- echarty:::.merlis(aa[[1]], list(val= 13))
  expect_equal(p$val, 13)
  aa = list(x= list(type= "1st.is.named.list"), geoIndex= 0)
  p <- echarty:::.merlis(aa, list(val= 13))
  expect_equal(p$val, 13)
  expect_equal(echarty:::.valid.url('http://does.not.exist.com'), FALSE)
})

test_that('stops are working in echarty.R', {
  df <- data.frame(x = 1:10, y = seq(1, 20, by=2))
  expect_error(ec.init(0)) # df
  expect_error(ec.init(cars, tl.series= list(d=1))) # groups
  expect_silent(ec.init(mtcars |> group_by(gear), tl.series= list(type='map'))) # no name/value, can use encode; 'regions' err
  expect_silent(ec.init(df |> group_by(y), series.param= list(type='bar')))
  expect_silent(ec.init(df |> group_by(y), series.param= list(type='bar'), 
                        timeline= list(show=TRUE)))
  # expect_error(cars |> group_by(speed) |> ec.init()) # 3 cols min
  # expect_error(ec.init(data.frame(name='n',value=1) |> group_by(name), 
  #     tl.series= list(type='bar')))  # 3 cols min
  # expect_silent(ec.init(data.frame(name='n',value=1) |> group_by(name), 
  #     tl.series= list(type='map')))  # 2 cols exception for map
  expect_error(ec.init(mtcars |> group_by(gear), tl.series= list(encode= list(x=1, y=2),groupBy='zzz'))) # groupBy
  expect_error(ec.util(cmd='layout'))
  expect_error(ecr.band(cars))
  tmp <- cars; tmp <- tmp |> rename(lower=speed, upper=dist)
  expect_error(ecr.band(tmp, lower='lower', upper='upper')) # 1st column name
  tmp <- ToothGrowth
  expect_error(ecr.band(tmp, lower='len', upper='supp'))    # non-numeric

  expect_error(ecr.ebars())
  expect_error(ecr.ebars(1))
  expect_error(ecr.ebars(ec.init(), 1))
  expect_error(ecr.ebars(ec.init(), cars))
  expect_silent(ecr.ebars(ec.init(load='custom'), cars, encode=list(x=1,y=c(2,3,4))))
  expect_silent(ec.init(load='lottie'))
  expect_silent(ec.init(load='ecStat'))
  expect_silent(ec.init(load='lottie,ecStat'))
  #expect_silent(ec.init(load='liquid'))   # Debian throws warnings in CRAN check
  #expect_silent(ec.init(load='gmodular'))
  #expect_silent(ec.init(load='wordcloud'))
  expect_error(mtcars |> group_by(cyl) |> ec.init(series.param= list(type='parallel')))
  expect_error(data.frame(name= c('A','B','C','D'), value= c(1,2,3,1), cat=c(1,1,2,2)) |>
    group_by(cat) |> ec.init(timeline= list(s=TRUE), dbg=T, series.param= list(type='pie'))
  )
})

test_that('mostly for coverage', {
  args <- list( 
    df= data.frame(name=c('Brazil','Australia'), value=c(111,222)) |> group_by(name),
    load= 'world', visualMap=list(s=TRUE),
    timeline= list(data= list("Australia", "Brazil"), axisType= "category"), 
    geo= list(map='world'),
    options= list(
      list(series=list(type='map', datasetIndex=1, geoIndex=0)), 
      list(series=list(type='map', datasetIndex=2, geoIndex=0))) 
  )
  p <- do.call(ec.init, args)
  expect_false(is.null(p$x$opts$legend))
  args$preset <- FALSE
  p <- do.call(ec.init, args)
  expect_true(is.null(p$x$opts$legend))
  
  p <- cars |> ec.init(renderer='svg', locale='ZH', useDirtyRect=TRUE) # legacy params
  expect_equal(p$x$iniOpts$locale, 'ZH')
  
  #p <- ec.init() |> ec.plugjs(paste0('file:///',.libPaths()[1],'/echarty/news.md'), ask=TRUE)
  p <- ec.init() |> ec.plugjs('https://helgasoft.github.io/echarty/js/spooky-ghost.json', ask=TRUE)
  expect_equal(p$x$opts$series[[1]]$type, 'scatter')
  
  #p <- chickwts |> ec.init()  # bug #21282
  #expect_equal(p$x$opts$dataset[[1]]$dimensions[1], 'feed')
})
