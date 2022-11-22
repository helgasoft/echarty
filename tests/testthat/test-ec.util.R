
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
  expect_equal(p$x$on[[1]]$event, 'mouseover')
})

test_that("fullscreen", {
  tbox <- list(right='20%', feature= ec.util(cmd='fullscreen'))
  #p <- cars |> ec.init(toolbox= tbox)
  #expect_match(p$x$opts$toolbox$feature$myecfs$onclick, 'ecfun.fscreen', fixed=TRUE)
  p <- crosstalk::bscols(
    cars |> ec.init(toolbox= tbox),
    mtcars |> ec.init(toolbox= tbox) |>
      htmlwidgets::prependContent(
        htmltools::tags$style(
          ".echarty:fullscreen { background-color: beige; }"
        )
      )
  )
  expect_match(p$children[[1]]$children[[1]][[1]]$children[[1]]$x$opts$toolbox$feature$myecfs$onclick, 'ecfun.fscreen(tmp.hwid)', fixed=TRUE)
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

test_that("labelsInside", {
  p <- ec.init(
    xAxis= list(data= list(1,2,3,4,5,6,7)),
    yAxis= list(type= 'value'),
    series= list(
      list(name= 'long text, 20 chars', type='line',
           data= c(110, 132, 101, 134, 90, 230, 210),
           endLabel= list( show=TRUE, formatter='{a}'),
           labelLayout= ec.util(cmd='labelsInside')),
      list(name='longer text, this is 35 characters',type='line', 
           data= c(210, 232, 201,234, 290, 240, 230),
           endLabel=list(show=TRUE, formatter='{a}'),
           labelLayout= ec.util(cmd='labelsInside'))
    )
  )
  expect_match(p$x$opts$series[[2]]$labelLayout, "get_e_charts(cid)", fixed=TRUE)
})