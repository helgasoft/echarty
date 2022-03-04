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

test_that("custom renderers - ecr.band", {
  df <- Orange |> mutate(Tree=as.numeric(Tree)) |> relocate(Tree, .after = last_col())
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