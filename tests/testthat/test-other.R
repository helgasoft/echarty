

test_that("registerMap", {
  json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
  dusa <- USArrests
  dusa$states <- row.names(dusa)
  p <- ec.init(preset= FALSE,
     series= list(list(type= 'map', map= 'USA', roam= TRUE, zoom= 3, left= -100, top= -30,
             data= lapply(ec.data(dusa, 'names'), 
                          function(x) list(name=x$states, value=x$UrbanPop))
     )),
     visualMap= list(type='continuous', calculable=TRUE, 
               inRange= list(color = rainbow(8)), seriesIndex= 0,
               min= min(dusa$UrbanPop), max= max(dusa$UrbanPop))
  )
  p$x$registerMap <- list(list(mapName= 'USA', geoJSON= json))
  
  expect_equal(round(p$x$registerMap[[1]]$geoJSON$features[[50]]$geometry$coordinates[[1]][[47]][[2]],4), 43.6164)
  expect_equal(p$x$opts$series[[1]]$data[[50]]$name, 'Wyoming')
  expect_equal(p$x$opts$series[[1]]$data[[50]]$value, 60)
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
        tl.series= list(type='map',  encode=list(value='value', name='country')),
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
  expect_equal(p$dataset$source[[1]], c("V1","V2","V3","V4","V5","V6"))
  expect_equal(p$xAxis[[1]]$name, 'mpg')
  
  ds <- mtcars |> dplyr::select(cyl, drat) |>
	ec.data(format='boxplot', jitter=0.1, #layout= 'h',
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
  expect_equal(p$x$opts$xAxis[[1]]$name, 'drat')
  expect_equal(p$x$opts$yAxis[[2]]$max, 3)

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


