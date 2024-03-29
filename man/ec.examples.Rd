% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/examples.R
\name{ec.examples}
\alias{ec.examples}
\title{Code Examples}
\usage{
ec.examples()
}
\value{
No return value, used only for help
}
\description{
Learn by example - copy/paste code from Examples below.\cr
This code collection is to demonstrate various concepts of
data preparation, conversion, grouping,
parameter setting, visual fine-tuning,
custom rendering, plugins attachment,
Shiny plots & interactions through Shiny proxy.\cr
}
\examples{
\donttest{
library(dplyr); library(echarty)

#------ Basic scatter chart, instant display
cars |> ec.init()

#------ Same chart, change theme and save for further processing
p <- cars |> ec.init() |> ec.theme('dark')
p

#------ parallel chart
ToothGrowth |> ec.init(ctype= 'parallel')

#------ JSON back and forth
tmp <- cars |> ec.init()
tmp
json <- tmp |> ec.inspect()
ec.fromJson(json) |> ec.theme("dark")


#------ Data grouping
iris |> mutate(Species= as.character(Species)) |>
        group_by(Species) |> ec.init()      # by non-factor column

Orange |> group_by(Tree) |> ec.init(
  series.param= list(symbolSize= 10, encode= list(x='age', y='circumference'))
)

#------ Polar bar chart
cnt <- 5; set.seed(222)
data.frame(
    x = seq(cnt),
    y = round(rnorm(cnt, 10, 3)), 
    z = round(rnorm(cnt, 11, 2)),
    colr = rainbow(cnt) 
) |> 
ec.init( preset= FALSE,
   polar= list(radius= '90\%'),
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


#------ Area chart
mtcars |> dplyr::relocate(wt,mpg) |> arrange(wt) |> group_by(cyl) |>
  ec.init(ctype= 'line', series.param= list(areaStyle= list(show=TRUE)) )

#------ Plugin leaflet
quakes |> dplyr::relocate('long') |>  # set order to long,lat
  mutate(size= exp(mag)/20) |> head(100) |>  # add accented size
ec.init(load= 'leaflet',
   tooltip= list(formatter= ec.clmn('magnitude \%@', 'mag')),
   legend= list(show=TRUE),
	series.param= list(name= 'quakes', symbolSize= ec.clmn(6, scale=2))  # 6th column is size
)

#------ Plugin 'world' with visualMap
set.seed(333)
cns <- data.frame(   
  val = runif(3, 1, 100),
  dim = runif(3, 1, 100),
  nam = c('Brazil','China','India')
)
cns |> group_by(nam) |> ec.init(load= 'world', timeline= list(s=TRUE),
  series.param= list(type='map', 
      encode=list(value='val', name='nam')),
  toolbox= list(feature= list(restore= list())),
  visualMap= list(calculable=TRUE, dimension=2)
)

#------ Plugin 'world' with lines and color coding
if (interactive()) {
flights <- NULL
flights <- try(read.csv(paste0('https://raw.githubusercontent.com/plotly/datasets/master/',
                               '2011_february_aa_flight_paths.csv')), silent = TRUE)
if (!is.null(flights)) {
  tmp <- data.frame(airport1 = unique(head(flights,10)$airport1),
                    color = c("#387e78","#eeb422","#d9534f",'magenta'))
  tmp <- head(flights,10) |> inner_join(tmp)    # add color by airport
  ec.init(load= 'world',
    geo= list(center= c(mean(flights$start_lon), mean(flights$start_lat)), 
  	   		 zoom= 7, map='world' ),
    series= list(list(
      type= 'lines', coordinateSystem= 'geo',
      data= lapply(ec.data(tmp, 'names'), function(x)
        list(coords = list(c(x$start_lon,x$start_lat),
                           c(x$end_lon,x$end_lat)),
             colr = x$color)
      ),
      lineStyle= list(curveness=0.3, width=3, color=ec.clmn('colr'))
    ))
  )
} }

#------ registerMap JSON
# registerMap supports also maps in SVG format, see website gallery
if (interactive()) {
json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
dusa <- USArrests
dusa$states <- row.names(dusa)
p <- ec.init(preset= FALSE,
   series= list(list(type= 'map', map= 'USA', roam= TRUE, zoom= 3, left= -100, top= -30,
       data= lapply(ec.data(dusa, 'names'), 
           function(x) list(name=x$states, value=x$UrbanPop))
   )),
   visualMap= list(type='continuous', calculable=TRUE, 
       inRange= list(color = rainbow(8)),
       min= min(dusa$UrbanPop), max= max(dusa$UrbanPop))
)
p$x$registerMap <- list(list(mapName= 'USA', geoJSON= json))
p
}

#------ locale
mo <- seq.Date(Sys.Date() - 444, Sys.Date(), by= "month")
df <- data.frame(date= mo, val= runif(length(mo), 1, 10))
p <- df |> ec.init(title= list(text= 'locale test'))
p$x$locale <- 'ZH'
p$x$renderer <- 'svg'
p


#------ Pie
isl <- data.frame(name=names(islands), value=islands) |> filter(value>100) |> arrange(value)

ec.init( preset= FALSE,
   title= list(text = "Landmasses over 60,000 mi\u00B2", left = 'center'),
   tooltip= list(trigger='item'),   #, formatter= ec.clmn()),
   series= list(list(type= 'pie', radius= '50\%', 
                     data= ec.data(isl, 'names'), name='mi\u00B2'))
)


#------ Liquidfill plugin
if (interactive()) {
  ec.init(load= 'liquid', preset=FALSE,
    series= list(list(
    type='liquidFill', data=c(0.66, 0.5, 0.4, 0.3),
    waveAnimation= FALSE, animationDuration=0, animationDurationUpdate=0))
  )
}


#------ Heatmap
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
  tooltip= list(position='top'),grid=list(height='50\%',top='10\%'),
  xAxis= list(type='category', data=times, splitArea=list(show=TRUE)),
  yAxis= list(type='category', data=days,  splitArea=list(show=TRUE)),
  visualMap= list(min=0,max=10,calculable=TRUE,orient='horizontal',left='center',bottom='15\%'),
  series= list(list(name='Hours', type = 'heatmap', data= hours,label=list(show=TRUE),
                    emphasis=list(itemStyle=list(shadowBlur=10,shadowColor='rgba(0,0,0,0.5)'))))
)


#------ Plugin 3D
if (interactive()) {
  data <- list()
  for(y in 1:dim(volcano)[2]) for(x in 1:dim(volcano)[1]) 
    data <- append(data, list(c(x, y, volcano[x,y])))
  ec.init(load= '3D',
          series= list(list(type= 'surface',	data= data))
  )
}


#------ 3D chart with custom item size
if (interactive()) {
iris |> group_by(Species) |>
  mutate(size= log(Petal.Width*10)) |>  # add size as 6th column
  ec.init(load= '3D',
          xAxis3D= list(name= 'Petal.Length'),
          yAxis3D= list(name= 'Sepal.Width'),
          zAxis3D= list(name= 'Sepal.Length'),
          legend= list(show= TRUE),
    series.param= list(symbolSize= ec.clmn(6, scale=10))
  )
}


#------ Surface data equation with JS code
if (interactive()) {
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
}


#------ Surface with data from a data.frame
if (interactive()) {
  data <- expand.grid(
    x = seq(0, 2, by = 0.1),
    y = seq(0, 1, by = 0.1)
  ) |> mutate(z = x * (y ^ 2)) |> select(x,y,z)
  ec.init(load= '3D',
          series= list(list(
            type= 'surface',
            data= ec.data(data, 'values'))) )
}

 
#------ Band series with customization
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


#------ Error Bars on grouped data
df <- mtcars |> group_by(cyl,gear) |> summarise(yy= round(mean(mpg),2)) |>
  mutate(low= round(yy-cyl*runif(1),2), 
         high= round(yy+cyl*runif(1),2))
df |> ec.init(load='custom', ctype='bar',
              xAxis= list(type='category'), tooltip= list(show=TRUE)) |>
  ecr.ebars( # name = 'eb',  # cannot have own name in grouped series
    encode= list(x='gear', y=c('yy','low','high')),
    tooltip = list(formatter=ec.clmn('high <b>\%@</b><br>low <b>\%@</b>', 'high','low')))


#------ Timeline animation and use of ec.upd for readability
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


#------ Timeline with pies
df <- data.frame(
  group= c(1,1,1,1,2,2,2,2),
  type=  c("type1","type1","type2","type2","type1","type1","type2","type2"),
  value= c(5,2,2,1,4,3,1,4),
  label= c("name1","name2","name3","name4","name1","name2","name3","name4"),
  color= c("blue","purple","red","gold","blue","purple","red","gold")
)
df |> group_by(group) |> ec.init( 
     preset= FALSE, legend= list(selectedMode= "single"),
     timeline= list(show=TRUE),
     series.param= list(type= 'pie', roseType= 'radius',
       itemStyle= list(color=ec.clmn(5)), 
       label= list(formatter=ec.clmn(4)),
       encode=list(value='value', itemName='type'))
)


#------ Boxplot without grouping
ds <- mtcars |> select(cyl, drat) |>
	 ec.data(format='boxplot', jitter=0.1, #layout= 'h',
			symbolSize=5, itemStyle=list(opacity=0.9), 
  			emphasis= list(itemStyle= list(
  			   color= 'chartreuse', borderWidth=4, opacity=1))
	)
ec.init(
  #colors= heat.colors(length(mcyl)),
  legend= list(show= TRUE), tooltip= list(show=TRUE),
  dataset= ds$dataset, series= ds$series, xAxis= ds$xAxis, yAxis= ds$yAxis,
  series.param= list(color= 'LightGrey', itemStyle= list(color='DimGray'))
) |> ec.theme('dark-mushroom')


#------ Boxplot with grouping
ds = airquality |> mutate(Day=round(Day/10)) |> 
  dplyr::relocate(Day,Wind,Month) |> group_by(Month) |>
	 ec.data(format='boxplot', jitter=0.1, layout= 'h')
ec.init(
  dataset= ds$dataset, series= ds$series,xAxis= ds$xAxis, yAxis= ds$yAxis,
  legend= list(show= TRUE), tooltip= list(show=TRUE)
)


#------ ecStat plugin: dataset transform to regression line
# presets for xAxis,yAxis,dataset and series are used
data.frame(x= 1:10, y= sample(1:100,10)) |>
ec.init(load= 'ecStat',
  js= c('echarts.registerTransform(ecStat.transform.regression)','','')) |>
ec.upd({
  dataset[[2]] <- list(
  	 transform= list(type= 'ecStat:regression', 
                    config= list(method= 'polynomial', order= 3)))
  series[[2]] <- list(
    type= 'line', itemStyle=list(color= 'red'), datasetIndex= 1)
})


#------ ECharts: dataset, transform and sort
datset <- list(
  list(source=list(
    list('name', 'age', 'profession', 'score', 'date'),
    list('Hannah Krause', 41, 'Engineer', 314, '2011-02-12'),
    list('Zhao Qian', 20, 'Teacher', 351, '2011-03-01'),
    list('Jasmin Krause', 52, 'Musician', 287, '2011-02-14'),
    list('Li Lei', 37, 'Teacher', 219, '2011-02-18'),
    list('Karle Neumann', 25, 'Engineer', 253, '2011-04-02'),
    list('Adrian Groß', 19, 'Teacher', NULL, '2011-01-16'),
    list('Mia Neumann', 71, 'Engineer', 165, '2011-03-19'),
    list('Böhm Fuchs', 36, 'Musician', 318, '2011-02-24'),
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


#------ Sunburst
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
                           radius= list(0, '90\%'),
                           label= list(rotate='radial') ))
)

#------ Gauge
ec.init(preset= FALSE,
        series= list(list(
          type = 'gauge', max = 160, min=40,
          detail = list(formatter='\U1F9E0={value}'),
          data = list(list(value=85, name='IQ test')) )) )


#------ Custom gauge with animation
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
ec.init(preset= FALSE,
  series= list(list( type= 'sankey',
    data= data,
    edges= data ))
)


# graph plot with same data ---------------
ec.init(preset= FALSE,
        title= list(text= 'Graph'),
        tooltip= list(show= TRUE),
        series= list(list(
          type= 'graph',
          layout= 'force',   # try 'circular' too
          data= lapply(data,
             function(x) list(name= x$node, tooltip= list(show=FALSE))),
          edges= lapply(data,
             function(x) { x$lineStyle <- list(width=x$value); x }),
          emphasis= list(focus= 'adjacency',
                         label= list(position= 'right', show=TRUE)),
          label= list(show=TRUE), roam= TRUE, zoom= 4,
          tooltip= list(textStyle= list(color= 'blue')),
          lineStyle= list(curveness= 0.3) ))
)


#------ group connect 
main <- mtcars |> ec.init(height= 200, legend= list(show=FALSE),
		 tooltip= list(axisPointer= list(axis='x')),
    series.param= list(name= "this legend is shared"))
main$x$group <- 'group1' # same group name for all charts
main$x$connect <- 'group1'
q1 <- main |> ec.upd({ series[[1]]$encode <- list(y='hp'); yAxis$name <- 'hp'
       legend <- list(show=TRUE)  # show first legend to share
})
q2 <- main |> ec.upd({ series[[1]]$encode <- list(y='wt'); yAxis$name <- 'wt' })
#if (interactive()) {   # browsable
  ec.util(cmd='layout', list(q1,q2), cols=2, title='group connect')
#}


#------ Events in Shiny
if (interactive()) {
  library(shiny); library(dplyr); library(echarty)

ui <- fluidPage(ecs.output('plot'), textOutput('out1') )
server <- function(input, output, session) {
  output$plot <- ecs.render({
    p <- mtcars |> group_by(cyl) |> ec.init(dataZoom= list(type= 'inside'))
    p$x$on <- list(                     # event(s) with Javascript handler
      list(event= 'legendselectchanged', 
        handler= htmlwidgets::JS("(e) => Shiny.setInputValue('lgnd', 'legend:'+e.name);"))
    )
    p$x$capture <- 'datazoom'
    p
  })
  observeEvent(input$plot_datazoom, {   # captured event
    output$out1 <- renderText({ 
      paste('Zoom.start:',input$plot_datazoom$batch[[1]]$start,'\%') })
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

#------------- Shiny interactive charts demo ---------------
#  run command: demo(eshiny)

}  # donttest
}
\seealso{
\href{https://helgasoft.github.io/echarty/}{website} has many more examples
}
