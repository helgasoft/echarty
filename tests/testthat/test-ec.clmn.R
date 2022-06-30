

test_that("ec.clmn works with sprintf, column indexes and names", {
  tmp <- data.frame(Species=as.vector(unique(iris$Species)), 
                    color=c("#387e78","#eeb422","#d9534f"))
  p <- iris |> dplyr::inner_join(tmp) |> ec.init()
  p$x$opts$series[[1]]$itemStyle <- list(color= ec.clmn(6))
  p$x$opts$series[[1]]$symbolSize <- ec.clmn(3, scale=3)
  p$x$opts$tooltip <- list(formatter= ec.clmn('Petal Length %@, Width %@', 3,4))

  expect_true(p$x$opts$series[[1]]$itemStyle$color == "function(x) {let c = String(x.value!=null ? x.value[5] : x.data!=null ? x.data[5] : x[5] ); return c;}")
  expect_is(  p$x$opts$series[[1]]$itemStyle$color, 'JS_EVAL')
  expect_true(p$x$opts$series[[1]]$symbolSize == "function(x) {let c = String(x.value!=null ? x.value[2] : x.data!=null ? x.data[2] : x[2] ); return (parseFloat(c)*3);}")
  expect_is(  p$x$opts$series[[1]]$symbolSize, 'JS_EVAL')
  expect_true(startsWith(p$x$opts$tooltip$formatter, "function(x) {var sprintf= (template, values) => { "))
  expect_is(  p$x$opts$tooltip$formatter, 'JS_EVAL')
  
  
  isl <- data.frame(name=names(islands), value=islands) |> 
    dplyr::filter(value>100) |> dplyr::arrange(value)
  p <- ec.init()
  p$x$opts <- list(
    tooltip = list(trigger='item', 
                   formatter=ec.clmn("%@ <br> %L@", 'name','value')),
    series = list(type='pie', data=ec.data(isl, 'names'), 
                  label=list(formatter=ec.clmn('value', scale=2)) )
  )
  
  expect_equal(length(unlist(gregexpr('x.data', p$x$opts$tooltip$formatter ))), 5)
  expect_true(grepl('[x.data.name,x.data.value]', p$x$opts$tooltip$formatter ))
  expect_is(p$x$opts$tooltip$formatter, 'JS_EVAL')
  expect_equal(length(unlist(gregexpr('x.data', p$x$opts$series$label$formatter ))), 4)
  expect_is(p$x$opts$series$label$formatter, 'JS_EVAL')
  
  
  tmp <- quakes |> dplyr::relocate('long') |>     # set order to lon,lat
    dplyr::mutate(size= exp(mag)/20) |> head(100)  # add accented size
  p <- tmp |> ec.init(load='leaflet')
  p$x$opts$series[[1]]$symbolSize = ec.clmn(6, scale=2)   # size column is 6th
  p$x$opts$tooltip = list(formatter=ec.clmn('magnitude %@', 'mag')) # 'mag' or 4
  
  expect_true(grepl("[x.data['mag']]", p$x$opts$tooltip$formatter, fixed=TRUE ))
  expect_true(endsWith(p$x$opts$series[[1]]$symbolSize, '(parseFloat(c)*2);}'))
  
  
  is <- sort(islands); is <- is[is>60]
  is <- data.frame(name=names(is), value=as.character(unname(is)))
  data <- ec.data(is, 'names')
  p <- ec.init()
  p$x$opts <- list(
    title = list(text = "Landmasses over 60,000 mi\u00B2", left = 'center'),
    tooltip = list(trigger='item', formatter=ec.clmn()),
    series = list(type='pie', radius='50%', data=data, name='mi\u00B2')
  )
  expect_equal("function(x) {let c=String(typeof x=='object' ? x.value : x); return c;}",
               as.character(p$x$opts$tooltip$formatter) )
  expect_equal(p$x$opts$series$data[[1]]$name, 'Celebes')
  
})


