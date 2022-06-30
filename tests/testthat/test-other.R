
test_that("registerMap", {
  json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
  dusa <- USArrests
  dusa$states <- row.names(dusa)
  p <- ec.init(preset=FALSE)
  p$x$registerMap <- list(list(mapName= 'USA', geoJSON= json))
  p$x$opts$series <- list(list(type= 'map', map= 'USA', 
                               roam= TRUE, zoom= 3, left= -100, top= -30,
       data= lapply(ec.data(dusa,'names'), function(x) list(name=x$states, value=x$UrbanPop))
  ))
  
  expect_equal(round(p$x$registerMap[[1]]$geoJSON$features[[50]]$geometry$coordinates[[1]][[47]][[2]],4), 43.6164)
  expect_equal(p$x$opts$series[[1]]$data[[50]]$name, 'Wyoming')
  expect_equal(p$x$opts$series[[1]]$data[[50]]$value, 60)
})

test_that("tl.series and timeline options", {
  p <- Orange |> dplyr::group_by(age) |> ec.init(
    tl.series=list(type='bar', encode=list(x='Tree', y='circumference'))
  )
  p$x$opts$timeline <- append(p$x$opts$timeline, list(autoPlay=TRUE))
  p$x$opts$options <- lapply(p$x$opts$options, 
    function(o) { o$title$text <- paste('age',o$title$text,'days'); o })
  
  expect_equal(p$x$opts$options[[5]]$title$text, "age 1231 days")
  expect_equal(p$x$opts$options[[5]]$series[[1]]$datasetIndex, 5)
  expect_equal(p$x$opts$options[[7]]$series[[1]]$encode$x, "Tree")
  expect_equal(p$x$opts$timeline$data[[5]], "1231")
  expect_true(p$x$opts$dataset[[5]]$transform$config['='] == 1004)
})
test_that("tl.series type 'map'", {
  if (interactive()) {
    cns <- data.frame(
      country = c('United States','China','Russia'),
      value = runif(3, 1, 100)
    )
    p <- cns |> group_by(country) |> ec.init(load='world',
        tl.series=list(type='map',  encode=list(value='value', name='country')) )
    p$x$opts$visualMap <- list(calculable=TRUE, max=100)
    expect_equal(p$x$opts$options[[1]]$series[[1]]$data[[1]]$name, "China")
  }
  else expect_equal(1,1)
})
  
test_that("echarts.registerTransform and ecStat", {
  dset <- data.frame(x=1:10, y=sample(1:100,10))
  p <- dset |> ec.init(js='echarts.registerTransform(ecStat.transform.regression)')
  p$x$opts$dataset[[2]] <- list(transform = list(type='ecStat:regression'))
  p$x$opts$series[[2]] <- list(
    type='line', itemStyle=list(color='red'), datasetIndex=1)
  
  expect_equal(p$x$jcode, 'echarts.registerTransform(ecStat.transform.regression)')
  expect_equal(p$x$opts$dataset[[2]]$transform$type, "ecStat:regression")
})

test_that("leaflet with ec.clmn", {
  if (interactive()) {
    tmp <- quakes |> dplyr::relocate('long') |>  # set order to lon,lat
      dplyr::mutate(size= exp(mag)/20) |> head(100)  # add accented size
    p <- tmp |> ec.init(load='leaflet')
    p$x$opts$series[[1]]$symbolSize = ec.clmn(6, scale=2)   # size column
    p$x$opts$tooltip = list(formatter=ec.clmn('magnitude %@', 'mag'))
    
    expect_equal(p$x$opts$leaflet$zoom, 6)
    expect_equal(class(p$x$opts$tooltip$formatter), 'JS_EVAL')
  }
  else expect_equal(1,1)
})

test_that("ec.snip", {
  op <- iris |> group_by(Species) |> ec.init() |> ec.snip()
  op$dataZoom <- list(type='inside', start=70)

  expect_equal(length(op$dataset), 4)
  expect_equal(length(op$series), 3)
})

test_that("ec.data format dendrogram", {
  hc <- hclust(dist(USArrests), "average")
  p <- ec.init(preset= FALSE)
  p$x$opts$series <- list(list(
    type= 'tree', roam= TRUE, initialTreeDepth= -1,
    data= ec.data(hc, format='dendrogram')
  ))
  
  expect_equal(p$x$opts$series[[1]]$data[[1]]$name, 'p49')
  expect_equal(p$x$opts$series[[1]]$data[[1]]$children[[1]]$children[[1]]$children[[2]]$name, 'North Carolina')
  expect_equal(length(p$x$opts$series[[1]]$data[[1]]$children[[1]]$children), 2)
})

test_that("ec.data format boxlpot", {
  ds <- mtcars |> dplyr::relocate(am,mpg) |> ec.data(format='boxplot')
  expect_equal(ds$series[[1]]$type, 'boxplot')
  expect_equal(ds$dataset$source[[1]], c("V1","V2","V3","V4","V5","V6"))
  expect_equal(class(ds$axlbl), 'JS_EVAL')
})

test_that("ec.data for treePC", {
  df <- as.data.frame(Titanic) |> group_by(Survived,Class) |> 
    summarise(value=sum(Freq), .groups='drop') |>
    mutate(parents= as.character(Survived), 
           children= as.character(Class)) |>
    select(parents, children, value)
  # add root to form a tree
  df[nrow(df) + 1,] <- list('survived','Yes',711)
  df[nrow(df) + 1,] <- list('survived','No', 1490)
  df[nrow(df) + 1,] <- list('root2','survived',2201)
  
  p <- ec.init(preset=FALSE)
  p$x$opts$series <- list(list(
    type='sunburst', 
    data= ec.data(df, format='treePC')[[1]]$children, 
    radius=c('11%', '90%')
    #,label=list(rotate='radial'), emphasis=list(focus='none')
  ))
  
  expect_equal(p$x$opts$series[[1]]$data[[1]]$value, 711)
  expect_equal(length(p$x$opts$series[[1]]$data[[1]]$children), 4)
})

test_that("ec.data format treeTK", {
  df <- as.data.frame(Titanic) |> 
    group_by(Survived,Age,Sex,Class) |>
    summarise(value=sum(Freq), .groups='drop') |>
    mutate(pathString= paste('Survive?', Survived, Age, Sex, Class, sep='/'),
           itemStyle= case_when(Survived=='Yes' ~ "color='green'", 
                                TRUE ~ "color='pink'"))  |> 
    select(pathString,value,itemStyle) |>
    rbind( as.data.frame(Titanic) |> 
             group_by(Survived,Age,Sex) |>
             summarise(value=sum(Freq), .groups='drop') |>
             mutate(pathString= paste('Survive?', Survived, Age, Sex, sep='/'),
                    itemStyle= case_when(Survived=='Yes' ~ "color='green'", 
                                         TRUE ~ "color='pink'"))  |> 
             select(pathString,value,itemStyle)) |>
    rbind( as.data.frame(Titanic) |> 
             group_by(Survived,Age) |> 
             summarise(value=sum(Freq), .groups='drop') |>
             mutate(pathString= paste('Survive?', Survived, Age, sep='/'),
                    itemStyle= case_when(Survived=='Yes' ~ "color='green'", 
                                         TRUE ~ "color='pink'"))  |> 
             select(pathString,value,itemStyle)) |>
    rbind( as.data.frame(Titanic) |> 
             group_by(Survived) |>
             summarise(value=sum(Freq), .groups='drop') |>
             mutate(pathString= paste('Survive?', Survived, sep='/'),
                    itemStyle= case_when(Survived=='Yes' ~ "color='green'", 
                                         TRUE ~ "color='pink'"))  |> 
             select(pathString,value,itemStyle))
  dat1 <- ec.data(df, format='treeTK')
  dat1[[1]]$itemStyle <- list(color= 'white')
  
  p <- ec.init(preset=FALSE, title=list(text= 'Titanic: Survival by Class'))
  p$x$opts$series <- list(list(
    type= 'sunburst', radius= c(0, '90%'), label=list(rotate=0),
    #type= 'tree', symbolSize= ec.clmn(scale=0.08),
    #type= 'treemap', upperLabel= list(show=TRUE, height=30), itemStyle= list(borderColor= '#999'), #leafDepth=4,
    data= dat1, 
    #	levels= list(list(itemStyle= list(color= 'blue')),
    #					 list(itemStyle= list(color= 'pink'))),
    emphasis= list(focus='none') 
  ))
  p$x$opts$tooltip <- list(show=TRUE, formatter=ec.clmn('%@%','pct')) # OK
  
  expect_equal(p$x$opts$series[[1]]$data[[1]]$value, 2201)
  expect_equal(length(p$x$opts$series[[1]]$data[[1]]$children), 2)
})

test_that("load 3D surface", {
  if (interactive()) {  # first time will load echarts-gl.js in source folder 'js'
    data <- list()
    for(y in 1:dim(volcano)[2]) for(x in 1:dim(volcano)[1])
      data <- append(data, list(c(x, y, volcano[x,y])))
    p <- ec.init(load = '3D')
    p$x$opts$series <- list(type = 'surface',	data = data)
  
    expect_equal(length(p$x$opts$series$data), 5307)
  }
  else expect_equal(1,1)
})

test_that("shapefiles with multi-polygons", {
  if (interactive()) {
    library(sf)
    fname <- system.file("shape/nc.shp", package="sf")
    nc <- st_read(fname)
    p <- ec.init(load= c('leaflet', 'custom'),  # load custom for polygons
           js= ec.util(type= 'sf.bbox', bbox= st_bbox(nc)),
           series= ec.util(df= nc, nid= 'NAME', itemStyle= list(opacity= 0.3)),
           tooltip= list(show= TRUE, formatter= '{a}')
    )
    expect_true(p$x$opts$leaflet$roam)
    expect_equal(p$x$opts$series[[108]]$name, 'Brunswick')
    expect_equal(p$x$opts$series[[108]]$itemStyle$opacity, 0.3)
  }
  else expect_equal(1,1)
})

