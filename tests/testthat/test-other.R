

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
    tl.series=list(type='bar', encode=list(x='Tree', y='circumference'))
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

test_that("ec.data format dendrogram", {
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

test_that("ec.data format boxlpot", {
  p <- mtcars |> dplyr::relocate(am,mpg) |> ec.data(format='boxplot')
  expect_equal(p$series[[1]]$type, 'boxplot')
  expect_equal(p$dataset$source[[1]], c("V1","V2","V3","V4","V5","V6"))
  expect_type(p$axlbl, 'list')  # was 'JS_EVAL'
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

test_that("ec.data format treeTK", {

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

test_that("serie from ec.util with cartesian3D", {
  # usage for LIDAR data
  library(sf)
  tmp <- st_as_sf(data.frame(x=c(-70,-70,-70), y=c(45, 46, 47), z=c(1,2,3)), 
                  coords= c('x','y','z'), crs= st_crs(4326))
  p <- ec.init(load= c('3D'),
        series= ec.util(df= tmp, 
                        coordinateSystem= 'cartesian3D', type= 'scatter3D')
        #,tooltip= list(formatter= '{b}')
  )
  expect_equal(p$x$opts$series[[1]]$data[[2]][[2]], 46)
  expect_type( p$x$opts$xAxis3D[[1]],'list')
})

test_that("shapefiles with multi-polygons", {
    library(sf)
    fname <- system.file("shape/nc.shp", package="sf")
    nc <- as.data.frame(st_read(fname))
    p <- ec.init(load= c('leaflet', 'custom'),  # load custom for polygons
       js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)),
       series= ec.util(cmd= 'sf.series', df= nc, nid= 'NAME', itemStyle= list(opacity= 0.3)),
       tooltip= list(show= TRUE, formatter= '{a}')
    )
    expect_true(p$x$opts$leaflet$roam)
    expect_equal(p$x$opts$series[[108]]$name, 'Brunswick')
    expect_equal(p$x$opts$series[[108]]$itemStyle$opacity, 0.3)
})

test_that("shapefile from ZIP", {
  if (interactive()) {  # creates a subfolder 'railways'
    library(sf)
    fname <- ec.util(cmd= 'sf.unzip', 
                     url= 'https://mapcruzin.com/sierra-leone-shapefiles/railways.zip')
    nc <- as.data.frame(st_read(fname))
    p <- ec.init(load= 'leaflet',
       js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)), 
       series= ec.util(df= nc, nid= 'osm_id', verbose=TRUE,
                       lineStyle= list(width= 3, color= 'red')),
       tooltip= list(formatter= '{a}'), animation= FALSE,
       leaflet= list(tiles= list(list(
         urlTemplate= 'https://stamen-tiles-{s}.a.ssl.fastly.net/terrain/{z}/{x}/{y}{r}.{ext}',
         options= list(attribution= 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a>',
                       subdomains= 'abcd',	maxZoom= 18,	ext= 'png')))) 
    )
    expect_equal(p$x$opts$leaflet$tiles[[1]]$options$subdomains, 'abcd')
    expect_equal(p$x$opts$series[[6]]$name, '207557821')
    expect_equal(p$x$opts$series[[6]]$lineStyle$color, 'red')
    
  }
  else expect_equal(1,1)
})

test_that("tabset", {
   p1 <- cars |> ec.init(width= 300, height= 300, grid= list(top= 20))
   p2 <- mtcars |> ec.init(width= 300, height= 300)
   r <- htmltools::browsable(
     ec.util(cmd='tabset', cars=p1, mtcars=p2)
   )
   expect_equal(r[[2]]$children[[5]]$children[[1]]$children[[1]][[1]]$x$opts$dataset[[1]]$source[[1]], c("speed", "dist"))
   expect_equal(r[[2]]$children[[5]]$children[[1]]$name, "section")
   expect_equal(r[[2]]$children[[2]]$children[[1]], "cars")
})

test_that("tabset with pipe", {
  library(dplyr)
  r <- htmltools::browsable(
    lapply(iris |> group_by(Species) |> group_split(), function(x) { 
      x |> ec.init(ctype= 'scatter', title= list(text= unique(x$Species)))
    }) |> ec.util(cmd='tabset')
  )
  expect_equal(r[[2]]$children[[7]]$children[[2]]$children[[1]][[1]]$width, 300)
  expect_equal(r[[2]]$children[[6]]$children[[1]], "chart3")
})

test_that("morph", {
  mc <- mtcars |> filter(cyl<8)
  datt <- function(idx) { return(mc[mc$cyl==idx,]$hp) }
  colors <- c("blue","red","green","yellow")
  
  oscatter <- list(
    xAxis= list(scale=TRUE),
    yAxis= list(scale=TRUE), color= colors,
    series=list(
      list(type='scatter', id='4', dataGroupId='4', data= datt(4),
           universalTransition= list(enabled= TRUE)),
      list(type='scatter', id='6', dataGroupId='6', data= datt(6),
           universalTransition= list(enabled=TRUE)) 
    )
  )
  obar <- list(
    title= list(text='Average'),
    xAxis=list(type='category', data=list('cyl4', 'cyl6')),
    yAxis=list(show= TRUE), color=colors,
    series=list(list(
      type='bar', id='average', colorBy='data',
      data=list(
        list(value= mean(datt(4)), groupId='4'),
        list(value= mean(datt(6)), groupId='6')),
      universalTransition=list(enabled= TRUE, 
                               seriesKey=list('4', '6'))
    ))
  )
  
  auto <- "
  cnt = 0;
  setInterval(() => {
    cnt++;
    opts= chart.getOption();
    keep= opts.morph;
    delete opts.morph;
    optcurr= Object.assign({}, keep[cnt % 2]);
    optcurr.morph= keep;
    chart.setOption(optcurr, true);
  }, 2000);
  "
  p <- ec.util(cmd='morph', oscatter, obar, js=auto)
  expect_equal(p$x$opts$morph[[1]]$series[[1]]$type, 'scatter')
  expect_equal(p$x$opts$morph[[2]]$series[[1]]$type, 'bar')
  expect_true(grepl('setInterval', p$x$jcode, fixed=TRUE))
  p <- ec.util(cmd='morph', oscatter, obar)
  expect_equal(p$x$on[[1]]$event, 'mouseover')
})