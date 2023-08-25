library(dplyr)
# TODO: df,scatter, dset/data

test_that("ecr.ebars scatter non-grouped", {
  df <- iris |> distinct(Sepal.Length, .keep_all= TRUE) |> 
    mutate(lo= Sepal.Width-Petal.Length/2, hi= Sepal.Width+Petal.Width) |>
    select(Sepal.Length, Sepal.Width, lo, hi, Species)
  p <- df |> ec.init(load='custom', legend=list(show=TRUE), xAxis=list(scale=TRUE)) |> 
    ecr.ebars(name= 'err')
  expect_equal(p$x$opts$series[[2]]$z, 3)
  expect_match(p$x$opts$series[[2]]$tooltip$formatter, 'range', fixed=TRUE)
  
  tmp <- round(rnorm(24, sin(1:24/2)*10, .5))
  df <- data.frame(x = 1:24, val = tmp,
         lower = round(rnorm(24, tmp -10, .5)),
         upper = round(rnorm(24, tmp + 5, .8)),
         cat= rep(c('A','B'),24) )
  df <- df |> group_by(cat)
  ec.init(df, load='custom') |> ecr.ebars(df)
})

test_that("ecr.ebars bars grouped", {
  df <- mtcars |> group_by(cyl,gear) |> summarise(yy= round(mean(mpg),2)) |>
    mutate(low= round(yy-cyl*runif(1),2), high= round(yy+cyl*runif(1),2)) |>
    relocate(cyl, .after = last_col())   # move group column behind first four cols
  
  p <- df |> ec.init(load='custom', ctype='bar',
                     xAxis=list(type='category')  # manual input
  ) |> ecr.ebars(df)
  
  expect_equal(length(p$x$opts$series), 6)
  expect_equal(p$x$opts$series[[4]]$type, 'custom')
  expect_true(p$x$opts$series[[4]]$renderItem == 'riErrorBar')
  expect_s3_class(p$x$opts$series[[4]]$renderItem, 'JS_EVAL')
  expect_equal(p$x$opts$xAxis$type, 'category')
  expect_equal(as.character(p$x$opts$series[[6]][['name']]), '8')
  
  # series.data
  sers = list(
    list(name= 's1',type= 'bar', data= list(5, 20, 36, 10, 10, 20, 4)),
    list(name= 's2',type= 'bar', data= list(4, 2, 40, 40, 30, 22, 1)),
    list(name= 's3',type= 'bar', data= list(15, 10, 36, 12, 9, 0, 14)),
    list(name= 's1',type= 'custom', z=11, renderItem= htmlwidgets::JS('riErrorBar'), 
         itemStyle= list(color= 'brown', borderDashOffset=8 ), # = halfWidth
         data= list(list('Mon',5,4,7), list('Tue',10, 9, 11))
    )
  )
  p <- ec.init(load='custom', ctype='bar', yAxis= list(show=T),
               xAxis= list(data= list('Mon','Tue','Wed','Thu','Fri','Sat','Sun')),
               series= sers
  )
  expect_equal(p$x$opts$xAxis$type, 'category')
  expect_s3_class(p$x$opts$series[[4]]$renderItem, 'JS_EVAL')
})

test_that("ecr.ebars riErrBarSimple", {
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
  expect_s3_class(p$x$opts$series[[2]]$renderItem, 'JS_EVAL')
  expect_equal(length(p$x$opts$series[[2]]$data), 50)
})

test_that("ecr.band", {
  df <- Orange |> mutate(Tree=as.numeric(Tree)) |> relocate(Tree, .after= last_col())
  band <- ecr.band(df |> filter(Tree==4) |> inner_join(df |> filter(Tree=='1'), by='age'),
        'circumference.y', 
        'circumference.x', 
        type= 'polygon', name= 'poly1')
  p <- df |> group_by(Tree) |> ec.init(load='custom',
    legend= list(s=T), tooltip= list(trigger='axis'), 
    dataZoom= list(type='inside', filterMode='none')
  )
  p$x$opts$series <- list(
    band[[1]],
    list(type='line', datasetIndex=3, color='orange', name='line1', clip=F)
  )
  
  expect_equal(length(p$x$opts$series), 2)
  expect_equal(p$x$opts$series[[1]]$type, 'custom')
  expect_equal(p$x$opts$series[[1]]$name, 'poly1')
  expect_true( p$x$opts$series[[1]]$renderItem == "riPolygon")
  expect_s3_class(p$x$opts$series[[1]]$renderItem, 'JS_EVAL')
})

test_that("ecr.band tooltips", {
  df <- airquality |> mutate(lwr= round(Temp-Wind*2),
                             upr= round(Temp+Wind*2),
                             x= paste0(Month,'-',Day) ) |>
                      relocate(x,Temp)
  bands <- ecr.band(df, 'lwr', 'upr', type='stack',
                    name='Band', areaStyle= list(opacity=0.4))
  p <- df |> ec.init(load='custom',
     legend= list(show= TRUE),
     xAxis= list(type='category', boundaryGap=FALSE),
     series= list(
       list(type='line', color='blue', name='line'),
       bands[[1]], bands[[2]]
     ),
     tooltip= list( trigger= 'axis',
       formatter= ec.clmn(
          'high <b>%@</b><br>line <b>%@</b><br>low <b>%@</b>',
                    3.3, 1.2, 2.2)
       )  # 3.3= upper_serie_index +.+ index_of_column_inside
  )
  expect_equal(p$x$opts$series[[2]]$stack, 'Band')
  expect_match(p$x$opts$tooltip$formatter, 'ss=[2.3,0.2,1.2]', fixed=TRUE)
})

test_that("leaflet with geoJson", {
  myGeojson= gsub('\n', '', '{
    "type": "FeatureCollection",
    "features": [
      {
         "type": "Feature",
         "properties": {
            "color": "purple"
         },
         "geometry": {
            "type": "MultiPolygon",
            "coordinates": [
            [
               [-110.81391,  31.931967 ],
               [-111.8391,   31.931931 ],
               [-111.838888, 32.931931 ],
               [-110.813849, 32.9319   ]
            ],
            [
               [-110.81391, 33.93196 ],
               [-111.8391,  33.93193 ],
               [-111.83888, 34.93193 ],
               [-110.81384, 34.9319  ]
            ]
            ]
         }
      }, 
      {
         "type": "Feature",
         "properties": {
            "color": "green"
         },
         "geometry": {
            "type": "LineString",
            "coordinates":
            [
               [-115.81391, 31.93196 ],
               [-116.8391,  31.93193 ],
               [-116.83888, 32.93193 ],
               [-115.81384, 32.9319   ]
            ]
         }
      }, 
      {
         "type": "Feature",
         "properties": {
            "color": "black"
         },
         "geometry": {
            "type": "MultiLineString",
            "coordinates": [
            [  [-115.8139, 34.93196 ],
               [-116.8391, 34.93193 ],
               [-116.8388, 35.93193 ],
               [-115.8138, 35.9319  ] ],
            [  [-117.8138, 34.9319  ],
               [-117.8139, 32.93186 ],
               [-118.8139, 32.93196 ] ]
            ]
         }
      },

      {
         "type": "Feature",
         "geometry": {
            "type": "Point",
            "coordinates": [-116, 33.66]
         },
         "properties": {
            "color": "green"
         }
      },
      {
         "type": "Feature",
         "geometry": {
            "type": "MultiPoint",
            "coordinates": [ [-119.0, 35.99], [-120.0, 36.66] ]
         },
         "properties": {
            "color": "blue"
         }
      }
  ]}')
  p <- ec.init(load= c('leaflet','custom'), 
    leaflet= list(center= c(-116.35, 35.5), zoom= 5, roam= T), 
    series= list(
      ec.util(cmd= 'geojson', 
              geojson= jsonlite::fromJSON(myGeojson),
              itemStyle= list(opacity= 0.5), 
              ppfill= NULL )  # =no polygon fill
    )
  )
  
  expect_equal(p$x$opts$leaflet$zoom, 5)
  expect_equal(p$x$opts$series[[1]]$type, 'custom')
  expect_s3_class(p$x$opts$series[[1]]$renderItem, 'JS_EVAL')
  expect_match(p$x$opts$series[[1]]$renderItem,"ecfun.geofill=null", fixed=T)
  expect_equal(length(p$x$opts$series[[1]]$data), 5)
  
# tmp <- jsonlite::fromJSON('https://echarts.apache.org/examples/data/asset/geo/USA.json')
# p <- ec.init(load= c('leaflet', 'custom'), 
#   leaflet= list(
#     center= c(-111, 35.5), zoom= 4, roam= T), 
#   tooltip= list(show=T),
#   series= list(
#     ec.util(cmd= 'geojson', geojson= tmp, colorBy= 'data', nid='name',
#             itemStyle= list(opacity= 0.5) )
#   )
# )
# p 
})

