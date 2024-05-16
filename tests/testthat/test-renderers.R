library(dplyr)

test_that("ecr.ebars", {
  # dset + groups
  tmp <- iris |> group_by(Species) |>
    ec.init(load= 'custom', ctype='bar', legend=list(show=T), yAxis=list(type='category'))
  p <- tmp |> ecr.ebars(encode= list(x=c('Sepal.Width', 'Petal.Length', 'Petal.Width'), y='Sepal.Length'))
  expect_equal(p$x$opts$series[[6]]$name, 'virginica')
  expect_equal(p$x$opts$xAxis$name, 'Sepal.Length')     # horiz.bars
  
  p <- tmp |> ecr.ebars(encode= list(x=c(2,3,4), y=1))  # numeric encode
  expect_equal(p$x$opts$series[[4]]$z, 3)
  expect_match(p$x$opts$series[[4]]$tooltip$formatter, 'range', fixed=TRUE)
  
  # dset + groups
  df <- mtcars |> group_by(cyl,gear) |> summarise(yy= round(mean(mpg),2)) |>
    mutate(low= round(yy-cyl*runif(1),2), high= round(yy+cyl*runif(1),2))
  p <- ec.init(df, load='custom', ctype='bar', tooltip= list(s=T), xAxis=list(type='category')) |> 
    ecr.ebars(encode= list(y=c(3,4,5), x=2))
  expect_equal(p$x$opts$series[[6]]$name, '8')
  expect_match(p$x$opts$series[[6]]$tooltip$formatter, 'range', fixed=TRUE)
  expect_equal(length(p$x$opts$series), 6)
  expect_equal(p$x$opts$series[[4]]$type, 'custom')
  expect_true(p$x$opts$series[[4]]$renderItem == 'riErrBars')
  expect_s3_class(p$x$opts$series[[4]]$renderItem, 'JS_EVAL')
  expect_equal(p$x$opts$xAxis$type, 'category')

  # test for auto xAxis= list(type='category')
  p <- Orange |> arrange(Tree) |> mutate(  #Tree= paste0('D',Tree), # char
          up= circumference+runif(5)*6,
          lo= circumference-runif(5)*6 ) |> group_by(age) |> 
    ec.init(load='custom', ctype='bar', legend= list(show=T),
      series.param= list(encode= list(x='Tree', y='circumference'))           
    ) |> ecr.ebars(encode= list(x=1, y=c(3,4,5)))
  expect_equal(p$x$opts$xAxis$type, 'category')

  # data + name + char.encode
  p <- ec.init(load= 'custom', legend= list(show=T), tooltip= list(show=T), 
               xAxis=list(type='category'),
    series= list(list(type='bar', name= 'data',
      encode= list(x='gear',y='yy'),
      dimensions= c('cyl','gear','yy','low','high'),
      data= ec.data(df |> filter(cyl==4))
  ))) |> 
    ecr.ebars(encode= list(x='gear', y=c('yy','low','high')), hwidth=12, name='err',
      itemStyle= list(borderWidth= 2.5, color= "red")
  )
  expect_equal(p$x$opts$series[[2]]$encode$y, c(2,3,4))
  expect_equal(p$x$opts$series[[2]]$itemStyle$borderDashOffset, 12)

  # grouped + non-categorical
  tmp <- round(rnorm(24, sin(1:24/2)*10, .5))
  df <- data.frame(x = 1:24, val = tmp,
         lower = round(rnorm(24, tmp -10, .5)),
         upper = round(rnorm(24, tmp + 5, .8)),
         cat= rep(c('A','B'),24) ) |> group_by(cat)
  p <- ec.init(df, load='custom') |> ecr.ebars(encode= list(x=1, y=c(2,3,4)))
  expect_equal(p$x$opts$series[[3]]$encode$y, c(1,2,3))
    # make horizontal
  p <- ec.init(df, load='custom', series.param= list(encode= list(y=1, x=2))) |> 
    ecr.ebars(encode= list(y=1, x=c(2,3,4)))     #|> ec.inspect()
  expect_equal(p$x$opts$series[[1]]$encode$y, 0)
  expect_equal(p$x$opts$series[[3]]$encode$x, c(1,2,3))

  # manual series + data
  sers = list(
    list(name= 's1',type= 'bar', data= list(5, 20, 36)),
    list(name= 's2',type= 'bar', data= list(4, 2, 40)),
    list(name= 's3',type= 'bar', data= list(15, 10, 36)),
    list(name= 's2',type= 'custom', z=11, renderItem= htmlwidgets::JS('riErrBars'), 
         itemStyle= list(color= 'brown', borderDashOffset=8 ), # 8= halfWidth
         encode= list(x=1,y=c(2,3,4)),
         data= list(list('Mon',5,4,7), list('Tue',10, 9, 11))
    )
  )
  p <- ec.init(load='custom', yAxis= list(show=T),
               xAxis= list(data= list('Mon','Tue','Wed')),
               series= sers
  )
  expect_equal(p$x$opts$series[[4]]$encode$y, c(1,2,3))
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
  p <- ec.init(load='custom',
  	legend= list(s=T), tooltip= list(trigger='axis'), 
  	dataZoom= list(type='inside', filterMode='none'),
  	series= list( band[[1]],
  		list(type='line', data=ec.data(df |> filter(Tree==5)), 
  			color='orange', name='line1', clip=F)
  	)
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
  expect_match(p$x$opts$series[[1]]$renderItem,"ecf.geofill=null", fixed=T)
  expect_equal(length(p$x$opts$series[[1]]$data), 5)
  expect_equal(p$x$opts$series[[1]]$data[[5]][[1]], 5)
  
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
  
  p <- ec.init(load= c('world','custom'), 
	  geo= list(type='map', map='world', center= c(-116.35, 35.5), zoom= 17, roam=TRUE), 
	  series= list( 
	    ec.util(cmd= 'geojson', geojson= jsonlite::fromJSON(myGeojson), cs='geo',
	            itemStyle= list(opacity= 0.5), ppfill= 'red' )
	  )
	)
  expect_equal(p$x$opts$series[[1]]$coordinateSystem, 'geo')
})

