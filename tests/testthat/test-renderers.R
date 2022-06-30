library(dplyr)

test_that("custom renderers - ecr.ebars", {
  df <- mtcars |> group_by(cyl,gear) |> summarise(yy=round(mean(mpg),2)) |>
    mutate(low=round(yy-cyl*runif(1),2), high=round(yy+cyl*runif(1),2)) |>
    relocate(cyl, .after = last_col())   # move group column behind first four cols
  
  p <- df |> ec.init(ctype='bar', load='custom') |>
    ecr.ebars(df, name = 'eb')
  
  expect_equal(length(p$x$opts$series), 6)
  expect_equal(p$x$opts$series[[4]]$type, 'custom')
  expect_true( p$x$opts$series[[4]]$renderItem == "sessionStorage.setItem('ErrorBar.oss','[\"\",\"\",\"3\",\"6\"]'); riErrorBar;")
  expect_equal(class(p$x$opts$series[[4]]$renderItem), 'JS_EVAL')
})

test_that("custom renderers - riErrBarSimple", {
  df <- data.frame(category= paste0('category', seq(1,50,1)),
                 avg= round(runif(50) * 1000, 2))
  errorData <- list()
  for (i in 1:50) {
    errorData <- append(errorData, list(list(
      i-1,
      round(max(0, df$avg[i] - runif(1) * 100)),
      round(df$avg[i] + runif(1) * 80)
    )))
  }						
  p <- df |> ec.init(load='custom')
  p$x$opts$title <- list(text = "Avg/Error chart")
  p$x$opts$xAxis <- list(data= df$category)
  p$x$opts$tooltip <- list(trigger= "axis", axisPointer= list(type= "shadow"))
  p$x$opts$legend <- list(show=TRUE)
  p$x$opts$dataZoom <- list(list(type= "slider", start= 50, end= 70), 
                            list(type= "inside", start= 50, end= 70))
  p$x$opts$series <- append(p$x$opts$series, list(list(
    type= "custom", name= "error",
    itemStyle = list(borderWidth= 1.5, color= 'brown'), 
    encode = list(x= 0, y= list(1, 2)), 
    data = errorData,
    renderItem = htmlwidgets::JS("riErrBarSimple") )))
  
  expect_equal(p$x$opts$series[[2]]$type, 'custom')
  expect_true( p$x$opts$series[[2]]$name == "error")
  expect_equal(class(p$x$opts$series[[2]]$renderItem), 'JS_EVAL')
})

test_that("custom renderers - ecr.band", {
  df <- Orange |> mutate(Tree=as.numeric(Tree)) |> relocate(Tree, .after= last_col())
  p <- df |> group_by(Tree) |> ec.init(load='custom')
  p$x$opts$legend <- list(ii='')
  p$x$opts$series <- append(
    ecr.band(df |> filter(Tree==4) |> inner_join(df |> filter(Tree=='1'), by='age'),
             'circumference.y', 'circumference.x', name='poly1'),
    list(list(type='line', datasetIndex=3, color='orange', name='line1'))
  )
  p$x$opts$tooltip <- list(trigger='axis')
  
  expect_equal(length(p$x$opts$series), 2)
  expect_equal(p$x$opts$series[[1]]$type, 'custom')
  expect_true( p$x$opts$series[[1]]$renderItem == "riPolygon")
  expect_equal(class(p$x$opts$series[[1]]$renderItem), 'JS_EVAL')
})