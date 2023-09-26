library(dplyr)
test_that("ec.clmn with sprintf, column indexes and names", {
  
  # dataset + column indexes
  p <- iris |> inner_join(data.frame(Species=unique(iris$Species), color= rainbow(3))) |>
  ec.init(
    tooltip= list(formatter= ec.clmn('Petal Length %@, Width %@', 3,4)),
    series.param= list(itemStyle= list(color= ec.clmn(6)),
                       symbolSize= ec.clmn(3, scale=3))
  )
  tmp <- p$x$opts$series[[1]]$itemStyle$color
  expect_true(tmp == "function(x) {c= String(x.value!=null ? x.value[5] : x.data!=null ? x.data[5] : x[5] ); return c;}")
  expect_s3_class(tmp, 'JS_EVAL')
  tmp <-  p$x$opts$series[[1]]$symbolSize
  expect_true(startsWith(tmp, "function(x) {c= String(x.value!=null ? x.value[2]"))
  expect_s3_class(tmp, 'JS_EVAL')
  tmp <- p$x$opts$tooltip$formatter
  expect_match(tmp, "sprintf(`Petal Length %@, Width %@`, vv)", fixed=TRUE)
  expect_s3_class(tmp, 'JS_EVAL')
  
  # dataset + column names
  p <- iris |> inner_join(data.frame(Species=unique(iris$Species), color= rainbow(3))) |>
  ec.init(
  	yAxis= list(axisLabel= list(formatter=ec.clmn('%R2@', -1, scale=0.5))),
    series.param= list(
      itemStyle= list(color= ec.clmn('color')),
      symbolSize= ec.clmn('Petal.Length', scale=3)
  	,tooltip= list(formatter= ec.clmn('Petal Length %@, Width %@', 'Petal.Length','Petal.Width'))
    )
    ,tooltip= list(s=T)  # enables series.tooltip
    # does not pass thru ec.init(df) - cant use colnames!
    # series= list(list( type='scatter',
    #   itemStyle= list(color= ec.clmn('color')), # use 6 instead of 'color'
    #   symbolSize= ec.clmn(3, scale=3)
    # ))
    #,tooltip= list(formatter= ec.clmn('Length %@, Width %@', 'Petal.Length','Petal.Width')) # use 3,4
  )
  tmp <- p$x$opts$series[[1]]
  expect_match(tmp$tooltip$formatter, "vv=[x.data['Petal.Length'],x.data['Petal.Width']];", fixed=TRUE)
  expect_match(tmp$itemStyle$color, "vv=[x.data['color']];", fixed=TRUE)
  expect_match(tmp$itemStyle$color, "pos=['Sepal.Length'", fixed=TRUE)
  expect_match(tmp$symbolSize, "vv=[x.data['Petal.Length']];", fixed=TRUE)
  expect_match(p$x$opts$yAxis[[1]]$formatter, "ss=[-2]", fixed=TRUE)
  
  # data + column names
  tmp <- data.frame(name=names(islands), value=islands) |> 
    filter(value>100) |> arrange(value)
  p <- ec.init( tooltip= list(s=TRUE),
    series = list(list(type= 'pie', data= ec.data(tmp, 'names'), 
            label= list(formatter= ec.clmn('value', scale=2)),
            tooltip= list(formatter= ec.clmn("%@ <br> %L@", 'name','value'))) )
  )
  tmp <- p$x$opts$series[[1]]$tooltip$formatter
  expect_equal(length(unlist(gregexpr('x.data', tmp ))), 4)
  expect_match(tmp, "[x.data['name'],x.data['value']]", fixed=TRUE)
  expect_s3_class(tmp, 'JS_EVAL')
  tmp <- p$x$opts$series[[1]]$label$formatter
  expect_equal(length(unlist(gregexpr('x.data', tmp ))), 3)
  expect_is(tmp, 'JS_EVAL')
  
  # leaflet + mixed
  tmp <- quakes |> relocate('long') |>     # set order to lon,lat
    mutate(size= exp(mag)/20) |> head(100)  # add accented size
  p <- tmp |> ec.init(load='leaflet',
    tooltip= list(formatter= ec.clmn('magnitude %@', 4)) # 'mag' is 4th
  ) |> ec.upd({ 
    series[[1]]$symbolSize <- ec.clmn('size', scale=2)
  })
  expect_true(grepl("ss=[3]", p$x$opts$tooltip$formatter, fixed=TRUE ))
  expect_true(grepl("[x.data['size']]", p$x$opts$series[[1]]$symbolSize, fixed=TRUE ))
  
  # data + mixed
  isl <- data.frame(name=names(islands), value=islands) |> arrange(desc(value)) |> 
      head(13) |> mutate(colr=rainbow(13))
  p <- ec.init(
    title = list(text = "Landmasses over 60,000 mi\u00B2", left = 'center'),
    tooltip = list(trigger='item', formatter=ec.clmn()),
    series = list(list(type='pie', radius='50%', data= ec.data(isl, 'names'),
                       itemStyle= list(color= ec.clmn('colr')),
                       label=list(position='inside', 
          formatter=ec.clmn("%@\n%@", 'name','value')) ))
  )
  expect_equal("function(x) {pos=[]; c= String(typeof x=='object' ? x.value : x); return c;}",
               as.character(p$x$opts$tooltip$formatter) )
  expect_equal(p$x$opts$series[[1]]$data[[1]]$name, 'Asia')
  
  
  p <- ec.clmn('json', scale=NULL)
  expect_match(p, 'stringify', fixed=TRUE)
  p <- ec.clmn('log', scale=0)
  expect_match(p, 'logged', fixed=TRUE)
})

