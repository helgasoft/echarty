
library(dplyr)
set.seed(2021)
df <- data.frame(
  name = sample(LETTERS, 10),
  size = rnorm(10, 10, 2),
  symbol = sample(c("circle", "rect", "triangle"), 10, replace= TRUE)
)

test_that("options preset", {
  options(echarty.theme='jazz')
  p <- ec.init()
  expect_equal(p$x$theme, 'jazz')
  expect_equal(p$dependencies[[1]]$name, 'jazz')
  
  p <- cars |> ec.init() |> ec.theme(name='mine', code='{ "backgroundColor": "pink" }')
  expect_equal(p$x$theme, 'mine')
  
  options(echarty.theme=NULL)
  p <- cars |> ec.init()
  expect_equal(p$x$theme, '')
  
  options(echarty.font='monospace')
  p <- cars |> ec.init()
  expect_equal(p$x$opts$textStyle$fontFamily, 'monospace')
  options(echarty.font=NULL)
})

test_that("ec.init presets for non-grouped data.frame", {
  p <- df |> ec.init(xAxis= list(scale=TRUE))
  expect_equal(p$x$opts$xAxis$type, 'category')
  #expect_true(is.null(p$x$opts$xAxis$type))  # assume default='category' = WRONG
  expect_true(!is.null(p$x$opts$yAxis))
  expect_equal(length(p$x$opts$dataset[[1]]$source), 11)
  expect_equal(p$x$opts$series[[1]]$type, 'scatter')
})

test_that("ec.init presets for grouped data.frame", {
  p <- df |> dplyr::group_by(symbol) |> ec.init(yAxis= list(scale=TRUE))
  expect_equal(p$x$opts$xAxis$type, 'category')
  expect_true(!is.null(p$x$opts$yAxis))
  expect_equal(length(p$x$opts$dataset[[1]]$source), 11)
  expect_equal(length(p$x$opts$legend$data), 3)
  expect_equal(p$x$opts$series[[1]]$type, 'scatter')
  expect_equal(p$x$opts$series[[1]]$datasetIndex, 1)
  expect_equal(p$x$opts$series[[1]]$name, 'circle')
})

test_that("ec.init presets for timeline", {
  dftl <- data.frame(
    year = unlist(lapply(2018:2021, function(x) {rep(x, 4)})), 
    quarter = rep(1:4, 4), 
    value = runif(16)
  )
  barTL <- function(data, timeline_var, x_var, bar_var) {
    bt <- data |> dplyr::group_by(!!dplyr::sym(timeline_var)) |> 
      ec.init(tl.series = list(type='bar', encode=list(x=x_var, y=bar_var)))
    bt
  }
  p <- barTL(dftl, timeline_var= "year", x_var= "quarter", bar_var= "value")
  expect_equal(length(p$x$opts$dataset[[1]]$source), 17)
  expect_equal(length(p$x$opts$dataset), 5)
  expect_equal(length(p$x$opts$options), 4)
  expect_equal(p$x$opts$options[[4]]$title$text, '2021')
})

test_that("ec.init presets for timeline groupBy", {
  set.seed(2022)
  dat <- data.frame(
    x3 = runif(16),
    x4 = runif(16),
    x5 = abs(runif(16)),
    x1 = rep(2020:2023, each = 4),
    x2 = rep(c("A", "A", "B", "B"), 4)
  ) 
  p <- dat |> group_by(x1) |> ec.init(
    tl.series= list(encode= list(x= 'x3', y= 'x5'), 
                    symbolSize= ec.clmn(2, scale=30),
                    groupBy= 'x2') 
  )
  p$x$opts$legend <- list(show=TRUE)
  expect_equal(p$x$opts$options[[4]]$series[[1]]$type, 'scatter')
  expect_equal(p$x$opts$options[[4]]$series[[1]]$encode$y, 'x5')
})

test_that("presets for parallel chart", {
  p <- mtcars |> group_by(cyl) |> ec.init(ctype='parallel')

  expect_equal(length(p$x$opts$dataset), 4)
  expect_equal(p$x$opts$series[[3]]$datasetIndex, 3)
  expect_equal(p$x$opts$parallelAxis[[2]]$name, 'disp')
})

test_that("presets for parallelAxis", {
  df <- as.data.frame(state.x77) |> head(10)
  p <- df |> ec.init(ctype= 'parallel',
              parallelAxis= ec.paxis(df, cols=c('Illiteracy','Population','Income')) ) |>
    ec.upd({ series <- lapply(series,
        function(ss) { ss$lineStyle <- list(width=3); ss }) 
    })
  expect_equal(length(p$x$opts$dataset[[1]]$source[[1]]), 8)
  expect_equal(p$x$opts$parallelAxis[[3]]$name, 'Income')
})

test_that("presets for crosstalk", {
  library(crosstalk)
  df <- cars
  df <- SharedData$new(df)
  p <- df |> ec.init()
  expect_equal(p$x$opts$dataset[[2]]$id, 'Xtalk')
  expect_equal(p$x$opts$series[[1]]$datasetId, 'Xtalk')
  expect_equal(p$x$opts$dataset[[1]]$source[[1]][3], 'XkeyX')
})

test_that("presets for leaflet", {
  tmp <- '
lng,lat,name,date,place
-118.808101,32.843715,"Seabed","2021-02-02","location A"
-117.332678,34.845565,"Lancaster","2021-04-02","location A"
-116.127504,32.846118,"fwy #8","2021-04-02","place B"
-117.316886,30.961700,"Baja","2021-07-02","place B"
'
  df <- read.csv(text=tmp, header=TRUE)
  p <- df |> ec.init(
    load='leaflet', tooltip= list(ey=''),
    series= list(list(
      encode= list(tooltip=c(3,4,5))
    ))
  )
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'leaflet')
  expect_equal(p$x$opts$series[[1]]$encode$tooltip, c(2,3,4))
})

test_that("presets with series.param", {
  p <- df |> ec.init(ctype='line', 
          series.param= list(areaStyle= list(show= T), stack= 'stk'))
  expect_equal(p$x$opts$series[[1]]$stack, 'stk')
  p <- ec.init(ctype='line', 
     series.param= list(areaStyle= list(show= T), stack= 'stk', 
                        data=list(c(0,0), c(2,2)))
  )
  expect_equal(p$x$opts$series[[1]]$data[[2]], c(2,2))
})

test_that("presets for visualMap", {
  p <- df |> ec.init(visualMap= list(dimension= 2, inRange= list(color= c("blue", "red"))) )
  expect_equal(p$x$opts$visualMap$dimension, 1)
  expect_equal(round(p$x$opts$visualMap$min,2), 8.66)
})
