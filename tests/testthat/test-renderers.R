library(dplyr)

test_that("custom renderers - ecr.ebars", {
  df <- mtcars |> group_by(cyl,gear) |> summarise(yy= round(mean(mpg),2)) |>
    mutate(low= round(yy-cyl*runif(1),2), high= round(yy+cyl*runif(1),2)) |>
    relocate(cyl, .after = last_col())   # move group column behind first four cols
  
  p <- df |> ec.init(ctype='bar', load='custom', xAxis=list(type='category')) |>
    ecr.ebars(df)
  
  expect_equal(length(p$x$opts$series), 6)
  expect_equal(p$x$opts$series[[4]]$type, 'custom')
  expect_true( p$x$opts$series[[4]]$renderItem == "sessionStorage.setItem('ErrorBar.oss','[\"\",\"\",\"3\",\"6\"]'); riErrorBar;")
  expect_equal(class(p$x$opts$series[[4]]$renderItem), 'JS_EVAL')
  expect_equal(p$x$opts$xAxis$type, 'category')
  expect_equal(p$x$opts$series[[6]]$name, 8)
})

test_that("custom renderers - riErrBarSimple", {
  set.seed(222)
  df <- data.frame(category= paste0('category', seq(1,50,1)),
                   avg= round(runif(50) * 1000, 2)) |>
    mutate(lo= round(avg - runif(50) * 200), hi= round(avg + runif(50) * 180))
  
  p <- df |> ec.init(load='custom',
    xAxis= list(data= df$category),
    tooltip= list(trigger= "axis", axisPointer= list(type= "shadow")),
    legend= list(show=TRUE),
    dataZoom= list(list(type= "slider", start= 50, end= 70), 
                   list(type= "inside", start= 50, end= 70))
  ) |> ec.upd({
    series <- append(series, list(
      list(
        type= "custom", name= "error", 
        itemStyle = list(borderWidth= 1.5, color= 'brown'),
        # encode() does not work ?!
        # get data from dataset$source in format 'x,lo,hi'
        data= ec.data(as.data.frame(do.call(rbind, dataset[[1]]$source[-1]))[,c(1,3,4)]),
        renderItem = htmlwidgets::JS("riErrBarSimple") 
      ) ))
  }) 
  expect_equal(p$x$opts$series[[2]]$type, 'custom')
  expect_true( p$x$opts$series[[2]]$name == "error")
  expect_equal(class(p$x$opts$series[[2]]$renderItem), 'JS_EVAL')
  expect_equal(length(p$x$opts$series[[2]]$data), 50)
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