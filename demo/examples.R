#' Code Examples
#' 
#' Learn by example - copy/paste code from examples below.
#' This code collection is to demonstrate various concepts of 
#'   data preparation, conversion, grouping, 
#'   parameter setting, visual fine-tuning, 
#'   custom rendering, plugins attachment, 
#'   Shiny plots & interactions through Shiny proxy.\cr
#' 
#' see also gallery https://helgasoft.github.io/echarty/articles/gallery.html for more examples

#'
library(dplyr); library(echarty)

#------ Basic scatter chart, instant display -----
cars |> ec.init()

#------ Same chart, change theme and save in variable for further processing -----
p <- cars |> ec.init() |> ec.theme('dark')
p
# or just set inside ec.init
cars |> ec.init(theme= 'dark')

# registerTheme custom
theme1 <- '{ "color":["#ff715e","#ffaf51"],
 "line": {
    "smooth": true,
    "symbol": "square",
    "symbolSize": 10
  },
 "backgroundColor": "lemonchiffon" }'
cars |> ec.init(
  registerTheme= list(name= 'myt', theme= jsonlite::fromJSON(theme1)),
  theme= 'myt')

#------ parallel chart -----
ToothGrowth |> ec.init(ctype= 'parallel')

#------ JSON back and forth -----
tmp <- cars |> ec.init()
tmp
json <- tmp |> ec.inspect()
ec.fromJson(json) |> ec.theme("dark")

#------ Time data -----
now <- Sys.time()
times <- now + (3600 * c(1, 5, 20, 100, 200)) # as.POSIXct()
df <- data.frame( x= times, y= 1:5 )

ec.init(df,
  xAxis= list(
    type= 'time', 
    axisLabel= list(
      formatter= '{dd} {HH}:{mm}', customValues= df$x,
      hideOverlap=TRUE, showMinLabel=TRUE, showMaxLabel=TRUE)
  ),
  series = list(list(type= "line", clip= FALSE,
    markPoint= list(data= list(list(coord= list(df$x[4], 1.8)), list(type= "max") ))
  )),
  tooltip= list(trigger= 'axis'),
  dataZoom= list(show=TRUE, filterMode='none')
)

#------ Data grouping -----
iris |> mutate(Species= as.character(Species)) |>
        group_by(Species) |> ec.init()      # group by non-factor column

Orange |> group_by(Tree) |> ec.init(
  series.param= list(symbolSize= 10, encode= list(x='age', y='circumference'))
)

#------ Polar bar chart -----
cnt <- 5; set.seed(222)
data.frame(
    x = seq(cnt),
    y = round(rnorm(cnt, 10, 3)),
    z = round(rnorm(cnt, 11, 2)),
    colr = rainbow(cnt)
) |>
ec.init(
   polar= list(radius= '90%'),
   radiusAxis= list(max= 'dataMax'),
   angleAxis= list(type= "category"),
   series= list(
   	list(type= "bar", coordinateSystem= "polar",
   		itemStyle= list(color= ec.clmn('colr')),
   		label= list(show= TRUE, position= "middle", formatter= "y={@[1]}")
   	),
	   list(type= 'scatter', coordinateSystem= "polar",
   		itemStyle= list(color= 'black'),
	   	  encode= list(angle='x', radius='z'))
   )
)

#------ Area chart -----
mtcars |> dplyr::relocate(wt,mpg) |> arrange(wt) |> group_by(cyl) |>
  ec.init(ctype= 'line', series.param= list(areaStyle= list(show=TRUE)) )


#------ style columns with ec.data -----
df <- data.frame(name= c('A','B','C'), value= c(1,2,3),
     itemStyle_color= c('chartreuse','lightblue','pink'),
     itemStyle_decal_symbol= c('rect','diamond','none'),
     emphasis_itemStyle_color= c('darkgreen','blue','red')
)
ec.init(series.param= list(type='pie', data= ec.data(df, 'names', nasep='_')))

df <- cars |> rowwise() |> mutate(value= list(list(speed, dist))) |> ungroup() |> 
  mutate(itemStyle_color=sample(c('darkgreen','blue','red'), 50, TRUE)) |> select(value,itemStyle_color)
ec.init(series.param= list(data= ec.data(df, 'names', nasep='_')))


#------ column-to-style with encode$data -----
cars |> mutate(opac= runif(50)) |>
  ec.init(series.param= list(encode= list(data= 
      list(value=c('dist','speed'), itemStyle= list(opacity='opac'))
  )))

cars |> rowwise() |> mutate(value= list(list(speed, dist))) |> ungroup() |> 
  mutate(
    speed= NULL, dist= NULL,   # value has been built so these are redundant
    clr= sample(c('darkgreen','blue','red'), 50, TRUE),
    opa= sample(c(0.3, 0.6, 0.9), 50, TRUE)
  ) |>
ec.init( title= list(subtext='X and Y are values'),
  dataZoom= list(type='inside'), tooltip= list(formatter='value={c}'),
  series.param= list(encode= list(data= list(itemStyle= list(color='clr', opacity='opa'))))
)

df <- chickwts |> group_by(feed) |> summarise(mn= mean(weight)) |> mutate(
    opa= sample(c(0.3, 0.6, NA), 6, TRUE),
    lbl= do.call(paste0, replicate(5, sample(LETTERS, 6, TRUE), FALSE)),
    dsy= sample(c('rect','diamond','triangle'), 6, TRUE)
)
ec.init(df, title= list(subtext='One axis is category, other is value'),
  dataZoom= list(type='inside'), tooltip=list(show=TRUE), 
  series.param= list(type='bar', colorBy='data', encode= list(data= list(
      value= c('feed','mn'),
      label= list(show=TRUE, formatter='lbl', color='black', fontWeight='bold'),
      itemStyle= list(opacity='opa', borderRadius=7,
            decal= list(symbol= 'dsy', symbolSize=0.9)) 
    ))
  )
)

iris |> mutate(name= sample(c('pot1','pot2','pot3'), 150, TRUE),
               opa= sample(c(0.4, 0.8, NA), 150, TRUE),
               dsy= sample(c('rect','diamond','triangle'), 150, TRUE)
) |> distinct(name, Species, .keep_all= TRUE) |> group_by(name) |>
ec.init(
  title= list(subtext='grouped data with styling'),
  series.param= list(type='bar', encode= list( 
    data= list(value= c('Species', 'Petal.Width'), 
               itemStyle= list(opacity='opa', borderRadius=7,
                               decal= list(symbol= 'dsy', symbolSize=1.5) ))
  ))
)

#------ Plugin leaflet -----
quakes |> dplyr::relocate('long') |>  # set order to long,lat
  mutate(size= exp(mag)/20) |> head(100) |>  # add accented size
ec.init(load= 'leaflet',
   tooltip= list(formatter= ec.clmn('magnitude %@', 'mag')),
   legend= list(show=TRUE),
	series.param= list(name= 'quakes', symbolSize= ec.clmn('size', scale=2))
)

#------ Plugin 'world' with visualMap, minimal code -----
data.frame(name=c('Brazil','Australia'), value=c(111,222)) |>
ec.init(load= 'world', ctype='map', visualMap=list(), color='lightgray')

#------ Plugin 'world' with timeline -----
set.seed(333)
cns <- data.frame(
  nam = c('Brazil','China','India'),
  dim = c(44,66, 100)
)
cns |> group_by(nam) |> ec.init(load= 'world', 
  timeline= list(show=TRUE), color=c('#eee','green'),
  series.param= list(type='map', 
      encode=list(value='dim', name='nam')
  ),
  toolbox= list(feature= list(restore= list())),
  visualMap= list(calculable=TRUE)
)

#------ Plugin 'world' with lines and color coding -----
flights <- NULL
flights <- try(read.csv(paste0('https://raw.githubusercontent.com/plotly/datasets/master/',
                               '2011_february_aa_flight_paths.csv')), silent= TRUE)
if (!is.null(flights)) {
  tmp <- data.frame(airport1 = unique(head(flights,10)$airport1),
                    color = c("#387e78","#eeb422","#d9534f",'magenta'))
  tmp <- head(flights,10) |> inner_join(tmp)    # add color by airport
  ec.init(load= 'world',
   geo= list(center= c(mean(flights$start_lon), mean(flights$start_lat)), zoom=7, map='world'),
   series.param= list( type= 'lines',
      data= lapply(ec.data(tmp, 'names'), function(x)
        list(coords = list(c(x$start_lon, x$start_lat),
                           c(x$end_lon, x$end_lat)),
             colr = x$color)
      ),
      lineStyle= list(curveness=0.3, width=3, color=ec.clmn('colr'))
    )
  )
}


#------ registerMap JSON -----
# registerMap supports also maps in SVG format, see website gallery

dusa <- USArrests |> mutate(name= row.names(USArrests)) |> rename(value=UrbanPop)
ec.init(
  series.param= list(type= 'map', map= 'USA', roam= TRUE, zoom= 3, left= -100, top= -30,
    data= ec.data(dusa, 'names')
  ),
  visualMap= list(type='continuous', calculable=TRUE, inRange= list(color= rainbow(8)),
    min= min(dusa$value), max= max(dusa$value) ),
  registerMap= list(mapName='USA', 
    opt= list(geoJson= jsonlite::read_json('https://echarts.apache.org/examples/data/asset/geo/USA.json')))
) #|> ec.registerMap('USA', 'https://echarts.apache.org/examples/data/asset/geo/USA.json')


#------ ec.data borders -----
data <- data.frame(   # triangles map
  long = c(-32, -31.5, -31, -31, -30.5, -30),
  lat = c(50, 52, 50, 50, 51, 50),
  region = c('A', 'A', 'A', 'A', 'A', 'A'),
  subregion = c('sr1','sr1','sr1', 'sr2','sr2','sr2')
)
ec.init(
  # geo= list(roam=TRUE, map='trgl', itemStyle= list(areaColor='pink')),
  series.param= list( type='map', map='trgl', roam=TRUE,
    data= list(
      list(name= 'sr1', value= 9),
      list(name= 'sr2', value= 1)
    )
  ),
  visualMap= list( max=11, inRange= list(color= rev(rainbow(10)))),
  tooltip=list(show=TRUE),
  registerMap= list(mapName='trgl', opt= list(geoJson= ec.data(data, 'borders')))
) #|> ec.registerMap('trgl', ec.data(data, 'borders'))

# library(ggplot2)   # CRAN complains
# df <- ggplot2::map_data("world", c("taiwan")) |>
#   mutate(subregion= ifelse(is.na(subregion), region, subregion))
# ec.init(
#   geo= list(roam=TRUE, map='tw', itemStyle= list(areaColor='pink')),
#   tooltip=list(show=TRUE)
# ) |> ec.registerMap('tw', ec.data(df, 'borders'))


#------ locale -----
mo <- seq.Date(Sys.Date() - 444, Sys.Date(), by= "month")
df <- data.frame(date= mo, val= runif(length(mo), 1, 10))
p <- df |> ec.init(title= list(text= 'ZH locale test'),
    toolbox= list(feature= list(saveAsImage= list(type='svg'))) )
p$x$locale <- 'ZH'
p$x$renderer <- 'svg'
p


#------ Pie -----
isl <- data.frame(name=names(islands), value=islands) |> filter(value>100) |> arrange(value)
ec.init( preset= FALSE,
   title= list(text = "Landmasses over 60,000 sq.mi", left = 'center'),
   tooltip= list(trigger='item'),   #, formatter= ec.clmn()),
   series= list(list(type= 'pie', radius= '50%',
                     data= ec.data(isl, 'names'), name='sq.mi'))
)


#------ Liquidfill plugin -----
if (interactive()) {
  ec.init(load= 'liquid', preset=FALSE,
    series= list(list(
    type='liquidFill', data=c(0.66, 0.5, 0.4, 0.3),
    waveAnimation= FALSE, animationDuration=0, animationDurationUpdate=0))
  )
}


#------ Heatmap -----
times <- c(5,1,0,0,0,0,0,0,0,0,0,2,4,1,1,3,4,6,4,4,3,3,2,5,7,0,0,0,0,0,
           0,0,0,0,5,2,2,6,9,11,6,7,8,12,5,5,7,2,1,1,0,0,0,0,0,0,0,0,3,2,
           1,9,8,10,6,5,5,5,7,4,2,4,7,3,0,0,0,0,0,0,1,0,5,4,7,14,13,12,9,5,
           5,10,6,4,4,1,1,3,0,0,0,1,0,0,0,2,4,4,2,4,4,14,12,1,8,5,3,7,3,0,
           2,1,0,3,0,0,0,0,2,0,4,1,5,10,5,7,11,6,0,5,3,4,2,0,1,0,0,0,0,0,
           0,0,0,0,1,0,2,1,3,4,0,0,0,0,1,2,2,6)
df <- NULL; n <- 1;
for(i in 0:6) { df <- rbind(df, data.frame(0:23, rep(i,24), times[n:(n+23)]));  n<-n+24  }
hours <- ec.data(df); hours <- hours[-1]    # remove columns row
times <- c('12a',paste0(1:11,'a'),'12p',paste0(1:11,'p'))
days <- c('Saturday','Friday','Thursday','Wednesday','Tuesday','Monday','Sunday')
ec.init(preset= FALSE,
  title= list(text='Punch Card Heatmap'),
  tooltip= list(position='top'),grid=list(height='50%',top='10%'),
  xAxis= list(type='category', data=times, splitArea=list(show=TRUE)),
  yAxis= list(type='category', data=days,  splitArea=list(show=TRUE)),
  visualMap= list(min=0,max=10,calculable=TRUE,orient='horizontal',left='center',bottom='15%'),
  series= list(list(name='Hours', type = 'heatmap', data= hours,label=list(show=TRUE),
                    emphasis=list(itemStyle=list(shadowBlur=10,shadowColor='rgba(0,0,0,0.5)'))))
)


if (interactive()) {
#------ Plugin 3D -----
  data <- list()
  for(y in 1:dim(volcano)[2]) for(x in 1:dim(volcano)[1])
    data <- append(data, list(c(x, y, volcano[x,y])))
  ec.init(load= '3D',
          series= list(list(type= 'surface',	data= data))
  )

#------ 3D chart with custom item size -----
  iris |> group_by(Species) |>
  mutate(size= log(Petal.Width*10)) |>  # add size as 6th column
  ec.init(
     xAxis3D= list(name= 'Petal.Length'),
     yAxis3D= list(name= 'Sepal.Width'),
     zAxis3D= list(name= 'Sepal.Length'),
     legend= list(show= TRUE),
     series.param= list(type='scatter3D', symbolSize= ec.clmn(6, scale=10))
  )

#------ Surface data equation with JS code
 ec.init(load= '3D',
   series= list(list(
     type= 'surface',
     equation= list(
       x = list(min= -3, max= 4, step= 0.05),
       y = list(min= -3, max= 3, step= 0.05),
       z = htmlwidgets::JS("function (x, y) {
                           return Math.sin(x * x + y * y) * x / Math.PI; }")
     )
   )))

#------ Surface with data from a data.frame -----
  data <- expand.grid(
    x = seq(0, 2, by = 0.1),
    y = seq(0, 1, by = 0.1)
  ) |> mutate(z = x * (y ^ 2)) |> select(x,y,z)
  ec.init(load= '3D',
          series= list(list(
            type= 'surface',
            data= ec.data(data, 'values'))) )
}  # end 3d


#------ Band series with customization -----
dats <- as.data.frame(EuStockMarkets) |> mutate(day= 1:n()) |>
  # first column ('day') becomes X-axis by default
  dplyr::relocate(day) |> slice_head(n= 100)

# 1. with unnamed data
bands <- ecr.band(dats, 'DAX','FTSE', name= 'Ftse-Dax',
						areaStyle= list(color='pink'))
ec.init(load= 'custom',
  tooltip= list(trigger= 'axis'),
  legend= list(show= TRUE), xAxis= list(type= 'category'),
  dataZoom= list(type= 'slider', end= 50),
  series = append( bands,
    list(list(type= 'line', name= 'CAC', color= 'red', symbolSize= 1,
              data= ec.data(dats |> select(day,CAC), 'values')
    ))
  )
)

# 2. with a dataset
# dats |> ec.init(load= 'custom', ...
#   + replace data=... with encode= list(x='day', y='CAC')


#------ Error Bars on grouped data -----
df <- mtcars |> group_by(cyl,gear) |> summarise(yy= round(mean(mpg),2)) |>
  mutate(low= round(yy-cyl*runif(1),2),
         high= round(yy+cyl*runif(1),2))
df |> ec.init(load='custom', ctype='bar',
              xAxis= list(type='category'), tooltip= list(show=TRUE)) |>
  ecr.ebars( # name = 'eb',  # cannot have own name in grouped series
    encode= list(x='gear', y=c('yy','low','high')),
    tooltip = list(formatter=ec.clmn('high <b>%@</b><br>low <b>%@</b>', 'high','low')))


#------ Timeline simple -----
Orange |> group_by(Tree) |>
ec.init(timeline= list(show=TRUE, autoPlay=TRUE), series.param= list() )

#------ Timeline animation and use of ec.upd for readability -----
Orange |> group_by(age) |> ec.init(
  xAxis= list(type= 'category', name= 'tree'),
  yAxis= list(max= max(Orange$circumference)),
  timeline= list(autoPlay= TRUE),
  series.param= list(type= 'bar', encode= list(x='Tree', y='circumference'))
) |> ec.upd({
  options <- lapply(options,
     function(o) {
       vv <- o$series[[1]]$datasetIndex +1;
       vv <- dataset[[vv]]$transform$config[["="]]
       o$title$text <- paste('age',vv,'days');
       o })
})


#------ Timeline with pies -----
df <- data.frame(
  group= c(1,1,1,1,2,2,2,2),
  type=  c("type1","type1","type2","type2","type1","type1","type2","type2"),
  value= c(5,2,2,1,4,3,1,4),
  label= c("name1","name2","name3","name4","name1","name2","name3","name4"),
  color= c("blue","purple","red","gold","blue","purple","red","gold")
)
df |> group_by(group) |> ec.init(
   legend= list(show=TRUE),
   timeline= list(show=TRUE),
   series.param= list(type= 'pie', roseType= 'radius',
     itemStyle= list(color=ec.clmn(5)),
     label= list(formatter=ec.clmn(4)),
     encode=list(value='value', itemName='type'))
)


#------ Boxplot without grouping -----
ds <- mtcars |> select(cyl, drat) |>
  ec.data(format='boxplot', jitter=0.1, symbolSize=6 ) #,layout='c')
ds$series[[1]]$color= 'LightGrey'
ds$series[[1]]$itemStyle= list(color='DimGray')
ec.init(
  legend= list(show= TRUE), tooltip= list(show=TRUE),
  dataset= ds$dataset, series= ds$series, xAxis= ds$xAxis, yAxis= ds$yAxis,
) |> ec.theme('dark-mushroom')


#------ Boxplot with grouping -----
ds = airquality |> mutate(Day=round(Day/10)) |>
  dplyr::relocate(Day,Wind,Month) |> group_by(Month) |>
	 ec.data(format='boxplot', jitter=0.1, layout= 'h')
ec.init(
  dataset= ds$dataset, series= ds$series,xAxis= ds$xAxis, yAxis= ds$yAxis,
  legend= list(show= TRUE), tooltip= list(show=TRUE)
)


#------ ecStat plugin: dataset transform to regression line -----
# presets for xAxis,yAxis,dataset and series are used
data.frame(x= 1:10, y= sample(1:100,10)) |>
ec.init(load= 'ecStat',
  js= c('echarts.registerTransform(ecStat.transform.regression)','',''),
  title= list(text= 'regression line')
) |> ec.upd({
  dataset[[2]] <- list(
  	 transform= list(type= 'ecStat:regression',
                     config= list(method= 'polynomial', order= 3)))
  series[[2]] <- list(
    type= 'line', itemStyle=list(color= 'red'), datasetIndex= 1)
})


#------ ecSimpleTransform -----

iris |> ec.init(
  load='https://cdn.jsdelivr.net/gh/100pah/echarts-simple-transform@refs/heads/main/dist/ecSimpleTransform.min.js',
  js= c('echarts.registerTransform(ecSimpleTransform.aggregate)','',''),
  title= list( text='ecSimpleTransform.aggregate'), legend= list(show=TRUE),
  series.param= list(name='scatter')
) |> ec.upd({
  dataset <- append(dataset, list(list(
    transform= list(
      type='ecSimpleTransform:aggregate',
      config= list(
        resultDimensions= list(
          list(from='Sepal.Width', method= 'average'), list(from='Species')
        )
        ,groupBy= 'Species'
     ))
  )) )
  xAxis <- list(xAxis, list(data= as.character(unique(iris$Species)), name='Avg'))
  series <- append(series, list(list(type='bar', name='Avg',
    encode=list(x='Species', y='Sepal.Width'), datasetIndex=1, xAxisIndex=1, colorBy='data')))
})

#------ ECharts dataset, transform and sort
datset <- list(
  list(source=list(
    list('name', 'age', 'profession', 'score', 'date'),
    list('Hannah Krause', 41, 'Engineer', 314, '2011-02-12'),
    list('Zhao Qian', 20, 'Teacher', 351, '2011-03-01'),
    list('Jasmin Krause', 52, 'Musician', 287, '2011-02-14'),
    list('Li Lei', 37, 'Teacher', 219, '2011-02-18'),
    list('Karle Neumann', 25, 'Engineer', 253, '2011-04-02'),
    list('Adrian Gro?', 19, 'Teacher', NULL, '2011-01-16'),
    list('Mia Neumann', 71, 'Engineer', 165, '2011-03-19'),
    list('B?hm Fuchs', 36, 'Musician', 318, '2011-02-24'),
    list('Han Meimei', 67, 'Engineer', 366, '2011-03-12'))),
  list(transform = list(type= 'sort', config=list(
    list(dimension='profession', order='desc'),
    list(dimension='score', order='desc'))
  )))
ec.init(
  title= list(
    text= 'Data transform, multiple-sort bar',
    subtext= 'JS source',
    sublink= paste0('https://echarts.apache.org/next/examples/en/editor.html',
                    '?c=doc-example/data-transform-multiple-sort-bar'),
    left= 'center'),
  tooltip= list(trigger= 'item', axisPointer= list(type= 'shadow')),
  dataset= datset,
  xAxis= list(type= 'category', axisLabel= list(interval=0, rotate=30)),
  yAxis= list(name= 'score'),
  series= list(list(
    type= 'bar',
    label= list(show= TRUE, rotate= 90, position= 'insideBottom',
                align= 'left', verticalAlign= 'middle'),
    itemStyle =list(color= htmlwidgets::JS("function (params) {
        return ({
          Engineer: '#5470c6',
          Teacher: '#91cc75',
          Musician: '#fac858'
        })[params.data[2]]
      }")),
    encode= list(x= 'name', y= 'score', label= list('profession') ),
    datasetIndex= 1
  ))
)


#------ Sunburst -----
# see website for different ways to set hierarchical data
# https://helgasoft.github.io/echarty/uc3.html
data = list(list(name='Grandpa',children=list(list(name='Uncle Leo',value=15,
     children=list(list(name='Cousin Jack',value=2), list(name='Cousin Mary',value=5,
     children=list(list(name='Jackson',value=2))), list(name='Cousin Ben',value=4))),
   list(name='Father',value=10,children=list(list(name='Me',value=5),
   list(name='Brother Peter',value=1))))), list(name='Nancy',children=list(
   list(name='Uncle Nike',children=list(list(name='Cousin Betty',value=1),
   list(name='Cousin Jenny',value=2))))))
ec.init( preset= FALSE,
         series= list(list(type= 'sunburst', data= data,
                           radius= list(0, '90%'),
                           label= list(rotate='radial') ))
)

#------ Gauge -----
ec.init(preset= FALSE,
  series= list(list(
    type = 'gauge', max = 160, min=40,
    detail = list(formatter='{value}'),
    data = list(list(value=85, name='IQ test')) )) )


#------ Custom gauge with animation -----
jcode <- "setInterval(function () {
    opts.series[0].data[0].value = (Math.random() * 100).toFixed(2) - 0;
    chart.setOption(opts, true);}, 2000);"
ec.init(preset= FALSE, js= jcode,
        series= list(list(
          type= 'gauge',
          axisLine= list(lineStyle=list(width=30,
            color= list(c(0.3, '#67e0e3'),c(0.7, '#37a2da'),c(1, '#fd666d')))),
            pointer= list(itemStyle=list(color='auto')),
            axisTick= list(distance=-30,length=8, lineStyle=list(color='#fff',width=2)),
            splitLine= list(distance=-30,length=30, lineStyle=list(color='#fff',width=4)),
            axisLabel= list(color='auto',distance=40,fontSize=20),
            detail= list(valueAnimation=TRUE, formatter='{value} km/h',color='auto'),
            data= list(list(value=70))
)))


#------ Sankey and graph plots
sankey <- data.frame(
  name   = c("a","b", "c", "d", "e"),
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value  = c(5, 6, 2, 8, 13)
)
data <- ec.data(sankey, 'names')
ec.init(
  series.param= list(type= 'sankey', data= data, edges= data )
)


#------ graph plot with same data ---------------
ec.init(
   title= list(text= 'Graph'),
   tooltip= list(show= TRUE),
   series= list(list(
      type= 'graph',
      layout= 'force',   # try 'circular' too
      data= data,
      edges= lapply(data,
         function(x) { x$lineStyle <- list(width=x$value); x }),
      emphasis= list(focus= 'adjacency',
                     label= list(position= 'right', show=TRUE)),
      label= list(show=TRUE), roam= TRUE, zoom= 4,
      tooltip= list(textStyle= list(color= 'blue')),
      lineStyle= list(curveness= 0.3) ))
)


#------ multiple series + common series.param + dataset -----
mtcars |> arrange(mpg) |> ec.init(
  legend= list(show=TRUE), tooltip= list(show=TRUE),
  preset=F,   # dont add axes names
  series.param= list(symbolSize=11),
  series= list(
    list(type='scatter',name='s1'),
    list(type='line',   name='s2'))
)


#------ flame chart -----
# data 0
treeData <- list( name= 'family', value=100, children= list(
    list(name='Grandpa', value=25,
        children= list(
            list(name='Uncle Leo', value=15,
                children= list(list(name='Cousin Jack',value=2),
                    list(name='Cousin Mary',value=5,
                        children=list(list(name='Jackson',value=2))),
                            list(name='Cousin Ben',value=4))),
            list(name='Father', value=10,
               children= list(list(name='Me',value=5),
                    list(name='Brother Peter',value=1))))),
    list(name='Granma Nancy', value=55,
        children= list(
            list(name='Uncle Nike',
               children=list(list(name='Cousin Betty',value=1),
                    list(name='Cousin Jenny',value=2)))))
))

# data 1
hc <- hclust(dist(USArrests), "ave")
cmax <- max(hc$height)
treeData <- ec.data(hc, format='dendrogram')[[1]]

# # data 2
# library(data.tree); data(acme)
# tmp <- acme
# cmax <- max(tmp$Get('cost'), na.rm=TRUE)
# tmp$Do(function(x) {   # works with or without values
#    cos <- as.numeric(x$cost); x$value <- ifelse(length(cos)==0, 0, cos) })  # add 'value'
# treeData <- tmp |> ToListExplicit(unname =TRUE)
# 
# # data 3
# library(data.tree)
# library(treemap); data(GNI2014)
# tmp <- GNI2014
# # Create a pathString column to define the hierarchy
# tmp$continent <- as.character(tmp$continent)
# tmp$pathString <- paste("world", tmp$continent, tmp$country, sep = "/")
# # Convert the data frame to a data.tree Node
# tmp <- as.Node(tmp[,])
# tmp$Do(function(x) {
#   #pop <- as.numeric(x$population); x$value <- ifelse(length(pop)==0, 0, pop) })  # add 'value'
#   gni <- as.numeric(x$GNI); x$value <- ifelse(length(gni)==0, 0, gni) })  # add 'value'
# cmax <- max(tmp$Get('GNI'), na.rm=TRUE) # add -1e9(-1B) for population
# treeData <- tmp |> ToListExplicit(unname =TRUE)

 # needed by JS for click event
fdat <- jsonlite::toJSON(treeData, force=TRUE, auto_unbox=TRUE, null='null')
vlvl <- 2  # min level for vertical labels (optional), set in jscode OR jsfun(with button)
jscode <- paste0('window.flameData=',fdat,'; //window.ec$vlevel=',vlvl,';')
jfun <- paste0("function(a) {
      if (typeof ec$vlevel == 'undefined') {window.ec$vlevel=",vlvl,";} else delete window.ec$vlevel;
      ch= ec_chart(echwid); ch.resize(); }")

ec.init(load= 'custom', title= list(text='flame tree', bottom='5%'),
  js= c(jscode, '',''),
  graphic= list(
    ec.util(cmd='button', text='\u00B1 level2', right=11, top= 20, js=jfun)
  ),
  xAxis= list(show=F), yAxis= list(show=F),
  tooltip= list(formatter= ec.clmn('%@ %R2@',4,6)),
  series.param= list(
    type= 'custom', renderItem= "riFlame",   # JS function in renderers.js
    encode= list(x= c(1, 2, 3), y= 1),
    data= ec.data(treeData, format='flame')  # name='p22' is optional
  ),
  visualMap = list(
    type= 'continuous', max= cmax,
    inRange= list(color= c('#2F93C8', '#AEC48F', '#FFDB5C', '#F98862'))
  )
  ,on= list( list(event='click', handler='flameClick'))  # in renderers.js
)


#------ segmented donut v.6 -----
ec.init(
  #load= 'https://cdn.jsdelivr.net/gh/apache/echarts-custom-series@main/custom-series/segmentedDoughnut/dist/segmented-doughnut.auto.js',
  #ask= 'loadRemote',
  series.param= list(
    renderItem= 'segmentedDoughnut',  
    # type= 'custom', coordinateSystem= 'none',
    itemPayload= list(
      radius= list('50%','65%'), segmentCount= 8,
      label= list(show=TRUE, formatter= '{c}/{b}', fontSize=35, color= '#555')
    ),
    data= list(5) )
)

#------ lineRange & barRange custom charts v.6 -----
temperatureData = list(
  list( time= 0, min= 26.7, max= 32.5, avg= 29.1 ),
  list( time= 100000000, min= 25.3, max= 32.4, avg= 28.4 ),
  list( time= 200000000, min= 24.6, max= 32.7, avg= 28.2 ),
  list( time= 300000000, min= 26.8, max= 35.8, avg= 30.5 ),
  list( time= 400000000, min= 26.2, max= 33.1, avg= 29.3 ),
  list( time= 500000000, min= 24.9, max= 31.4, avg= 27.8 )
)
url <- 'https://cdn.jsdelivr.net/gh/apache/echarts-custom-series@main/custom-series'
ec.init(
  load= c( paste0(url,'/lineRange/dist/line-range.auto.min.js'),
           paste0(url,'/barRange/dist/bar-range.auto.min.js') ),
  ask= 'loadRemote',
  xAxis= list(type= "category"),   # other types like time,value do not work (bug)
  dataset= list(source= temperatureData), tooltip= list(trigger= "axis"), legend= list(top= 15), 
  series= list(
  	list(type= "line", name= "Average", smooth= TRUE, encode= list(x= "time", y= "avg", tooltip= "avg" ) ), 
  	list(type= "custom", name= "lineRange", renderItem= "lineRange", itemPayload= list(areaStyle= list(color= "red")),
         encode= list(x= "time", y= list("min", "max"), tooltip= list("min", "max")) )
  	,list(type= "custom", name= "barRange", renderItem= "barRange",
         encode= list(x= "time", y= list("min", "max"), tooltip= list("min", "max")) )
  )
)

#------ matrix v.6 ------
mtx <- cor(swiss)
cols <- colnames(mtx)
mtx[upper.tri(mtx)] <- NA
datam <- as.data.frame(mtx)
#datam <- tibble::rownames_to_column(datam, 'x')
datam <- datam |> mutate(x= rownames(datam)); rownames(datam) <- NULL
# Convert to long format
long_data_base <- reshape( datam, direction= "long",
  idvar = "x",
  varying = list(colnames(mtx)),
  v.names = "value",
  timevar = "y",
  times = cols # Custom labels for timevar
)
datam <- na.omit(long_data_base)
row.names(datam) <- NULL
vals <- lapply(cols, \(x) { list(value=x) })

ec.init(
  title= list(text= 'demo: new matrix chart from ECharts v.6.0'),
  matrix= list(x= list(data=vals), y= list(data=vals)),
  visualMap= list(type='continuous', min=-1,max=1, dimension=3,
                 calculable=TRUE, orient='horizontal', bottom=0, left='center'),
  series= list(list(type= 'heatmap', coordinateSystem= 'matrix',
                    data= ec.data(datam),
    label= list(show=TRUE, formatter= ec.clmn('%R2@', 3))
  ))
)

#---- chord v.6 ----
ec.init(
  tooltip = list(show=TRUE), legend= list(show=TRUE), 
  series.param= list(type = "chord", name = "test",
    startAngle = 90, endAngle = -270, clockwise = FALSE, 
    lineStyle = list(color = "target"), 
    data = list(
      list(name= "A"), list(name= "B"), list(name= "C"), list(name= "D")), 
    links = list(list(source = "A", target = "B", value = 40), 
                 list(source = "A", target = "C", value = 20, lineStyle= list(color = "source")), 
                 list(source = "A", target = "D", value = 10) ))
)

if (interactive()) {
#------ tabsets ------
  p1 <- cars |> ec.init(grid= list(top=26), height=333)  # move chart up
  p2 <- mtcars |> arrange(mpg) |> ec.init(height=333, ctype='line')
  ec.util(cmd= 'tabset', cars= p1, mtcars= p2)

#------ group connect -----
  main <- mtcars |> ec.init(height= 200, legend= list(show=FALSE),
  		 tooltip= list(axisPointer= list(axis='x')),
      series.param= list(name= "this legend is shared"))
  main$x$group <- 'group1' # same group name for all charts
  main$x$connect <- 'group1'
  q1 <- main |> ec.upd({ series[[1]]$encode <- list(y='hp'); yAxis$name <- 'hp'
         legend <- list(show=TRUE)  # show first legend to share
  })
  q2 <- main |> ec.upd({ series[[1]]$encode <- list(y='wt'); yAxis$name <- 'wt' })
  ec.util(cmd='layout', list(q1,q2), cols=2, title='group connect')
}


#------  ec.init 'js' parameter demo -----
# in single item scenario (js=jcode), execution is same as j3 below
j1 <- "winvar= 'j1';" # set window variables
j2 <- "opts.title.text= 'Javascript execution';"   # opts exposed and changed
j3 <- "ww= chart.getWidth(); alert('width:'+ww);"  # chart exposed
ec.init(js= c(j1, j2, j3), title= list(text= 'Title'),
  series.param= list(name='sname'),
  legend= list(formatter= ec.clmn("function(name) {
    return name +' - '+ this.winvar; }"))
)


#------ Javascript built-in functions -----
jtgl <- "() => {
  ch1 = ec_chart(echwid);   // takes the auto-assigned id
//ch1 = ec_chart('myTree'); // manual id is OK too
  opts = ch1.getOption();
//opts = ec_option(echwid);  // for reading, without .setOption
  opts.series[0].orient= opts.series[0].orient=='TB' ? 'LR':'TB';
  ch1.setOption(opts); }"
dbut <- ec.util(cmd='button', text='toggle', js=jtgl)
data <- list(list(name='root', children=list(list(name='A',value=1),list(name='B',value=3))))
ec.init( # elementId='myTree',
  series.param= list(type='tree', data=data, symbolSize=33), graphic= list(dbut)
)


#----- Events in R (without Shiny) -----
ec.init(
  title= list(text= 'click node or edge'),
  series.param= list(
    type= 'graph', layout= 'force',
    nodes= list(list(name= 'a', value= 10), list(name= 'b', value= 20)),
    edges= list(list(source= 0, target= 1))
  ),
  on= list(    # Javascript handlers
    list(event='click', query=list(dataType='node'), handler= ec.clmn("(e) => alert('Node');")),
    list(event='click', query=list(dataType='edge'), handler= ec.clmn("(e) => alert('Edge');"))
  )
)

mtcars |> group_by(cyl) |> 
ec.init(title= list(text='Events with Javascript handler', subtext='zoom and legend events'), 
  dataZoom= list(type= 'inside'),
  on= list(
    list(event= 'legendselectchanged', handler= ec.clmn(
      "(e) => { ch1=ec_chart(echwid); opts=ch1.getOption(); opts.title[0].text= 'legend:'+e.name; ch1.setOption(opts); }")),
    list(event= 'datazoom', handler= ec.clmn(
      "(e) => { ch1=ec_chart(echwid); opts=ch1.getOption(); opts.title[0].text= 'Zoom.start: '+ e.batch[0].start.toFixed(); ch1.setOption(opts); }"))
))


url <- 'https://echarts.apache.org/examples/data/asset/geo/Veins_Medical_Diagram_clip_art.svg'
svg <- url |> readLines(encoding='UTF-8') |> paste0(collapse="")
ec.init(
  title= list(text= 'mouseover events'),
  grid = list(left= "60%", top= "10%", bottom= "10%"), tooltip= list(show=TRUE),
  xAxis = list(show=TRUE),
  yAxis = list(type='category', data = list("heart", "large-intestine", "small-intestine", "spleen", "kidney", "lung", "liver")),
  series = list(
     list(type= "bar", emphasis= list(focus= "self"), data= list(121, 321, 141, 52, 198, 289, 139))
     ,list(type='map', map= "organs", coordinateSystem= 'cartesian2d',
        left= 10, right= "50%", selectedMode= "multiple",
  		  emphasis= list(focus= "self",
  		      itemStyle=list(opacity=1, color='yellow', borderWidth=2, borderColor='red'),
  		      label= list(position="bottom", distance=0, color='brown'))
	   )
  ),
  registerMap= list(mapName='organs', opt= list(svg= svg)),
  on= list(
    list(event='mouseover',
      handler=ec.clmn("function(ev) { cmd={ type:'highlight', seriesIndex:0, name:ev.name};
      if(ev.seriesType=='bar') { cmd.seriesIndex=1; }
      this.dispatchAction(cmd); }") ),
    list(event='mouseout',
      handler=ec.clmn("function(ev) { cmd={ type:'downplay', seriesIndex:0, name:ev.name};
      if(ev.seriesType=='bar') { cmd.seriesIndex=1; }  // delete cmd.seriesIndex; cmd.geoIndex=0; }
      this.dispatchAction(cmd); }") )
  )
) |> ec.theme('dark-mushroom')


#------ Events in Shiny ----------
if (interactive()) {
  library(shiny); library(dplyr); library(echarty)

 runApp( list(
   ui= fluidPage(	fluidRow(
 		column(8, ecs.output("chart")),
 		column(4, actionButton("bzoom", "Brush"), tableOutput('dats'))
   )),
 server= function(input, output, session) {
 	output$chart <- ecs.render({
 		cars |> ec.init(  capture='brushselected',
 			toolbox= list( feature= list(brush= list(type=list("lineX", "clear")))),
 			brush= list(toolbox= c('lineX'),
 			            brushType= 'lineX', xAxisIndex= 0,
 		 		    brushStyle= list( borderWidth= 0, color= 'rgba(0,255,0,0.1)'))
 		)
 	})
 	observeEvent(input$bzoom, {
 		p <- ecs.proxy("chart")
 		p$x$opts <- list(type='brush',
 							  areas= list(list(xAxisIndex= 0, brushType= 'lineX', coordRange= c(10, 25) ))
 		)
 		p |> ecs.exec('p_dispatch')
 	})
 	observeEvent(input$chart_brushselected, {
 		bsel <- input$chart_brushselected
 		output$dats <- renderTable(
 		{
 			if (length(bsel$batch[[1]]$selected[[1]]$dataIndex)==0) return()
 			idcs <- unlist(bsel$batch[[1]]$selected[[1]]$dataIndex) +1  # js to R
 			cars[idcs,]
 		})
 	})
 }
 ))

 #----- types of events ------
  ui <- fluidPage(ecs.output('plot'), textOutput('out1') )
  server <- function(input, output, session) {
    output$plot <- ecs.render({
      mtcars |> group_by(cyl) |>
      ec.init(dataZoom= list(type= 'inside'), title=list(text='mouseover,legend,zoom events'),
        series.param= list(selectedMode=TRUE),
        on= list(             # event(s) with Javascript handler
          list(event= 'legendselectchanged',
               handler= ec.clmn("(e) => Shiny.setInputValue('lgnd', 'legend:'+e.name);"))
        ),
        capture= c('datazoom','selectchanged')
      )
    })
    observeEvent(input$plot_datazoom, {   # captured event
      output$out1 <- renderText({
        paste('Zoom.start:',input$plot_datazoom$batch[[1]]$start,'%') })
    })
    observeEvent(input$plot_selectchanged, {   # 2nd captured event
     output$out1 <- renderText({ input$plot_selectchanged })
    })
    observeEvent(input$plot_mouseover, {  # built-in event
      v <- input$plot_mouseover
      output$out1 <- renderText({ paste('s:',v$seriesName,'d:',v$data[v$dataIndex+1]) })
    })
    observeEvent(input$lgnd, {            # reactive response to on:legend event
      output$out1 <- renderText({ input$lgnd })
    })
  }
  shinyApp(ui, server)
}

#------ generate chart SVG image with SSR & Shiny ----------
# see https://echarts.apache.org/handbook/en/how-to/cross-platform/server/
if (interactive()) {
 runApp( list(
  ui= fluidPage( ecs.output("plot") ),
  server= function(input, output, session) {
    jco1 <- "svgStr= chart.renderToSVGString(); Shiny.setInputValue('svgic', svgStr); chart.dispose();"
    output$plot <- ecs.render({
      cars |> ec.init(js=jco1, iniOpts= list(renderer='svg', ssr=TRUE, height=200, width=200), animation=F) #,ctype='bar')
    })
    # write a local file is easier in R than JS
    observeEvent(input$svgic, { cat(input$svgic, file='c:/temp/plot.svg') })
  }
 ))
}

#------------- demo: Shiny interactive charts ---------------
#  run command: demo(eshiny)

