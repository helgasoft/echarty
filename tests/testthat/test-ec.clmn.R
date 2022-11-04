

test_that("ec.clmn works with sprintf, column indexes and names", {
  
  tmp <- data.frame(Species= as.vector(unique(iris$Species)), 
                    color= c("#387e78","#eeb422","#d9534f"))
  p <- iris |> dplyr::inner_join(tmp) |> ec.init(
    tooltip= list(formatter= ec.clmn('Petal Length %@, Width %@', 3,4))
  ) |> ec.upd({
    series[[1]]$itemStyle <- list(color= ec.clmn(6))
    series[[1]]$symbolSize <- ec.clmn(3, scale=3)
  })
  expect_true(p$x$opts$series[[1]]$itemStyle$color == "function(x) {let c = String(x.value!=null ? x.value[5] : x.data!=null ? x.data[5] : x[5] ); return c;}")
  expect_s3_class( p$x$opts$series[[1]]$itemStyle$color, 'JS_EVAL')
  expect_true(p$x$opts$series[[1]]$symbolSize == "function(x) {let c = String(x.value!=null ? x.value[2] : x.data!=null ? x.data[2] : x[2] ); return (parseFloat(c)*3);}")
  expect_s3_class( p$x$opts$series[[1]]$symbolSize, 'JS_EVAL')
  expect_true(startsWith(p$x$opts$tooltip$formatter, "function(x) {var sprintf= (template, values) => { "))
  expect_s3_class( p$x$opts$tooltip$formatter, 'JS_EVAL')
  
  
  tmp <- data.frame(name=names(islands), value=islands) |> 
    dplyr::filter(value>100) |> dplyr::arrange(value)
  p <- ec.init(
    tooltip = list(formatter= ec.clmn("%@ <br> %L@", 'name','value')),
    series = list(list(type= 'pie', data= ec.data(tmp, 'names'), 
                  label= list(formatter= ec.clmn('value', scale=2))) )
  )
  expect_equal(length(unlist(gregexpr('x.data', p$x$opts$tooltip$formatter ))), 5)
  expect_true(grepl('[x.data.name,x.data.value]', p$x$opts$tooltip$formatter ))
  expect_s3_class(p$x$opts$tooltip$formatter, 'JS_EVAL')
  expect_equal(length(unlist(gregexpr('x.data', p$x$opts$series[[1]]$label$formatter ))), 4)
  expect_is(p$x$opts$series[[1]]$label$formatter, 'JS_EVAL')
  
  
  tmp <- quakes |> dplyr::relocate('long') |>     # set order to lon,lat
    dplyr::mutate(size= exp(mag)/20) |> head(100)  # add accented size
  p <- tmp |> ec.init(load='leaflet',
    tooltip= list(formatter=ec.clmn('magnitude %@', 'mag')) # 'mag' or 4
  ) |> ec.upd({ 
    series[[1]]$symbolSize = ec.clmn(6, scale=2)   # size column is 6th
  })
  expect_true(grepl("[x.data['mag']]", p$x$opts$tooltip$formatter, fixed=TRUE ))
  expect_true(endsWith(p$x$opts$series[[1]]$symbolSize, '(parseFloat(c)*2);}'))
  
  
  tmp <- sort(islands)
  tmp <- tmp[tmp>60]
  tmp <- data.frame(name=names(tmp), value=as.character(unname(tmp)))
  data <- ec.data(tmp, 'names')
  p <- ec.init()
  p$x$opts <- list(
    title = list(text = "Landmasses over 60,000 mi\u00B2", left = 'center'),
    tooltip = list(trigger='item', formatter=ec.clmn()),
    series = list(type='pie', radius='50%', data= data, name='mi\u00B2')
  )
  expect_equal("function(x) {let c=String(typeof x=='object' ? x.value : x); return c;}",
               as.character(p$x$opts$tooltip$formatter) )
  expect_equal(p$x$opts$series$data[[1]]$name, 'Celebes')
})

test_that("ec.clmn layout", {
  p <- lapply(list('dark','macarons','gray','jazz','dark-mushroom'),
                function(x) cars |> ec.init() |> ec.theme(x) ) |>
    ec.util(cmd='layout', cols= 2, title= 'my layout')
  expect_equal(p$children[[3]]$children[[1]]$children[[2]]$children[[1]]$x$theme, 'macarons')
})

test_that("ec.clmn tabset", {
  p1 <- cars |> ec.init(grid= list(top= 20))
  p2 <- mtcars |> ec.init()
  p <- htmltools::browsable(
    ec.util(cmd= 'tabset', cars= p1, mtcars= p2, width= 200, height= 200)
  )
  expect_s3_class(p[[2]]$children[[5]]$children[[2]]$children[[1]][[1]], 'echarty')
})

test_that("ec.clmn morph", {
  setd <- function(type) {
    mtcars |> group_by(cyl) |> ec.init(ctype=type) |> ec.upd({
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
})

