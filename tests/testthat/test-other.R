test_that("registerMap", {
  # similar in ec.examples, with USA map
  gjson <- jsonlite::parse_json('{"type":"FeatureCollection", "properties":{"id":"all3"},
  "features":[
     {"type":"Feature", "geometry":{"type":"MultiPolygon", "coordinates":[[[[2.330466,48.862223],[2.330305,48.861636],[2.329572,48.861581],[2.329466,48.861531],[2.329413,48.861528],[2.328796,48.861475],[2.328545,48.861396],[2.328466,48.861404],[2.328361,48.86137]]]]},
    	"properties":{"lat":48.859475,"lon":2.329466,"name":"bic1","range":500, "id":"0.5 min", "ppfill": "#00FF0077"} },
     {"type":"Feature", "geometry":{"type":"MultiPolygon", "coordinates":[[[[2.333466,48.866204],[2.333061,48.86588],[2.332897,48.865906],[2.332466,48.865801],[2.332165,48.865776],[2.331811,48.865475],[2.331621,48.86532],[2.331466,48.865265],[2.331274,48.865283]]]]},
     	"properties":{"lat":48.859475,"lon":2.329466,"name":"bic2","range":1000, "id":"1 min"} },
     {"type":"Feature", "geometry":{"type":"MultiPolygon", "coordinates":[[[[2.335466,48.869736],[2.335037,48.870046],[2.334836,48.870105],[2.334466,48.870265],[2.334289,48.870298],[2.333577,48.870364],[2.333466,48.870381],[2.333364,48.870373],[2.332485,48.870456]]]]},
     	"properties": {"lat":48.859475, "lon":2.329466, "name":"bic3", "range":1500, "id":"1.5 min"} }
  ]}')
  ext <- function(dd) { unlist(unname(sapply(gjson$features, \(f) {f$properties[dd]}))) }
  vals <- ext('range')
  dparis <- data.frame(name= ext('name'), value= vals)
  p <- ec.init(preset= FALSE,
    geo= list(map= 'paris', roam= TRUE),
    series =list(list(
      type= 'map', geoIndex=0, coordinateSystem= 'geo',
      data= ec.data(dparis, 'names')
    )),
    visualMap= list(type='continuous', calculable=TRUE,
      inRange= list(color = rainbow(8)),
      min= min(vals), max= max(vals))
  )
  p$x$registerMap <- list(list(mapName= 'paris', geoJSON= gjson))
  p
  expect_equal(length(p$x$registerMap[[1]]$geoJSON), 3)
  expect_equal(p$x$opts$geo$map, 'paris')
  expect_equal(p$x$opts$series[[1]]$geoIndex, 0)
  expect_equal(p$x$opts$series[[1]]$data[[2]]$value, 1000)
})

test_that("tl.series and timeline options", {
  p <- Orange |> dplyr::group_by(age) |> ec.init(
    tl.series= list(type='bar', encode=list(x='Tree', y='circumference'))
  ) |> ec.upd({
    timeline <- append(timeline, list(autoPlay=TRUE))
    options <- lapply(options, 
      function(o) { o$title$text <- paste('age',o$title$text,'days'); o })
  })
  expect_equal(p$x$opts$options[[5]]$title$text, "age 1231 days")
  expect_equal(p$x$opts$options[[5]]$series[[1]]$datasetIndex, 5)
  expect_equal(p$x$opts$options[[7]]$series[[1]]$encode$x, "Tree")
  expect_equal(p$x$opts$timeline$data[[5]], "1231")
  expect_true(p$x$opts$dataset[[5]]$transform$config['='] == 1004)
})

test_that("tl.series type 'map'", {
#  if (interactive()) {
    cns <- data.frame(
      country = c('United States','China','Russia'),
      value = runif(3, 1, 100)
    )
    p <- cns |> group_by(country) |> ec.init(load='world',
        tl.series= list(type='map', encode=list(value='value', name='country')),
        visualMap= list(calculable=TRUE, max=100)
    )
    expect_equal(p$x$opts$options[[1]]$series[[1]]$data[[1]]$name, "China")
#  }
#  else expect_equal(1,1)
})
  
test_that("ec.upd(), echarts.registerTransform and ecStat", {
  dset <- data.frame(x=1:10, y=sample(1:100,10))
  p <- dset |> ec.init(js= 'echarts.registerTransform(ecStat.transform.regression)'
  ) |> ec.upd({
    dataset[[2]] <- list(transform = list(type='ecStat:regression'))
    series[[2]] <- list(
      type='line', itemStyle=list(color='red'), datasetIndex=1)
  })
  expect_equal(p$x$jcode, 'echarts.registerTransform(ecStat.transform.regression)')
  expect_equal(p$x$opts$dataset[[2]]$transform$type, "ecStat:regression")
})

test_that("leaflet with ec.clmn", {
#  if (interactive()) {
    tmp <- quakes |> dplyr::relocate('long') |>  # set order to lon,lat
      dplyr::mutate(size= exp(mag)/20) |> head(100)  # add accented size
    p <- tmp |> ec.init(load= 'leaflet',
                  tooltip = list(formatter=ec.clmn('magnitude %@', 'mag'))
    ) |> ec.upd({
      series[[1]]$symbolSize = ec.clmn(6, scale=2)   # size column
    })
    
    expect_equal(p$x$opts$leaflet$zoom, 6)
    expect_s3_class(p$x$opts$tooltip$formatter, 'JS_EVAL')
#  }
#  else expect_equal(1,1)
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
  p <- mtcars |> dplyr::relocate(cyl,mpg) |> ec.data(format='boxplot')
  expect_equal(p$series[[1]]$type, 'boxplot')
  expect_equal(p$dataset$source[[1]][[3]], 22.8)
  expect_equal(p$xAxis[[1]]$name, 'mpg')
  
  ds <- mtcars |> dplyr::select(cyl, drat) |>
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
  expect_match(p$x$opts$series[[4]]$tooltip$formatter, "x[1] ); return c;}", fixed=TRUE)
  expect_equal(p$x$opts$yAxis[[1]]$name, 'drat')
  expect_equal(p$x$opts$xAxis[[2]]$max, 3)

  # with grouping
  ds <- airquality |> dplyr::mutate(Day=round(Day/10)) |> 
    dplyr::relocate(Day,Wind,Month) |> dplyr::group_by(Month) |> 
  	ec.data(format='boxplot', jitter=0.1)
  p <- ec.init(
    dataset= ds$dataset, series= ds$series,xAxis= ds$xAxis, yAxis= ds$yAxis,
    legend= list(show= TRUE), tooltip= list(show=TRUE)
  )
  expect_equal(length(p$x$opts$dataset), 10)
  expect_equal(p$x$opts$series[[5]]$type, 'boxplot')
  expect_equal(p$x$opts$series[[5]]$datasetIndex, 9)
  expect_equal(p$x$opts$series[[6]]$type, 'scatter')
  expect_equal(p$x$opts$series[[6]]$name, '0')
  expect_equal(p$x$opts$yAxis[[1]]$type, 'category')
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

test_that("load 3D surface", {
  #if (interactive()) {  # first time will load echarts-gl.js in source folder 'js'
    data <- list()
    for(y in 1:dim(volcano)[2]) for(x in 1:dim(volcano)[1])
      data <- append(data, list(c(x, y, volcano[x,y])))
    p <- ec.init(load= '3D', series= list(list(type= 'surface',	data= data)) )
    
    expect_equal(length(p$x$opts$series[[1]]$data), 5307)
  #}
  #else expect_equal(1,1)
})

test_that("ec.inspect", {
  p <- mtcars |> dplyr::group_by(gear) |> ec.init() |> ec.inspect('data')
  expect_match(p[1], "rows= 33", fixed=TRUE)
  expect_match(p[2], "filter", fixed=TRUE)
})

test_that("ec.plugjs", {
  p <- ec.init() |> ec.plugjs(
    'https://raw.githubusercontent.com/apache/echarts/master/test/data/map/js/china-contour.js')
  expect_equal(p$dependencies[[1]]$name, "china-contour.js")
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
  
  tmp <- ecs.render({ p<-cars |> ec.init() })
  expect_match(as.character(attributes(tmp)$cacheHint$origUserFunc$body[2]), "p <- ec\\.init\\(cars\\)")
  
  p <- ecs.proxy('sash')
  expect_equal(p$id, 'sash')
  expect_equal(attributes(p)$class, 'ecsProxy')
  
  sendCustomMessage <- \(name,plist) {}
  p$session <- globalenv()
  p$x$opts$test <- 'sankey'
  tmp <- ecs.exec(p)
  expect_equal(tmp$x$opts$test, 'sankey')
})

test_that(".merlis", {
  aa = list(list("type"= "map", "geoIndex"= 0))
  p <- echarty:::.merlis(aa, list(val= 13))
  expect_equal(p[[1]]$val, 13)
  p <- echarty:::.merlis(aa[[1]], list(val= 13))
  expect_equal(p$val, 13)
})

test_that('autoset axis type', {
  df <- data.frame(
    time = seq(from = as.POSIXct("2021-01-01 08:00:00"), to = as.POSIXct("2021-01-01 09:10:00"), by = "1 min"),
    y = rnorm(71, mean = 100)
  )
  p <- df |> ec.init(yAxis= list(scale=T))
  expect_equal(p$x$opts$xAxis$type, 'time')
  expect_equal(p$x$opts$yAxis$type, 'value')
})