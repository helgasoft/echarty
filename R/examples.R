# ----------- Examples --------------

#' Code Examples
#' 
#' Learn by example - copy/paste code from Examples below.\cr
#' This code collection is to demonstrate various concepts of 
#'   data preparation, conversion, grouping, 
#'   parameter setting, visual fine-tuning, 
#'   custom rendering, plugins attachment, 
#'   Shiny plots & interactions through Shiny proxy.\cr
#' 
#' @return No return value, used only for help
#' 
#' @seealso \href{https://helgasoft.github.io/echarty/}{website} has many more examples
#' 
#' @examples  
#' \donttest{
#' library(dplyr); library(echarty)
#' 
#' #------ Basic scatter chart, instant display
#' cars |> ec.init()
#' 
#' #------ Same chart, change theme and save for further processing
#' p <- cars |> ec.init() |> ec.theme('dark')
#' p
#' 
#' 
#' #------ JSON back and forth
#' tmp <- cars |> ec.init()
#' tmp
#' json <- tmp |> ec.inspect()
#' ec.fromJson(json) |> ec.theme("dark")
#'
#'
#' #------ Data grouping
#' library(dplyr)
#' iris |> mutate(Species=as.character(Species)) |>
#'          group_by(Species) |> ec.init()      # by non-factor column
#' 
#' p <- Orange |> group_by(Tree) |> ec.init()   # by factor column
#' p$x$opts$series <- lapply(p$x$opts$series, function(x) { 
#'   x$symbolSize=10; x$encode=list(x='age', y='circumference'); x } )
#' p
#' 
#' 
#' #------ Area chart
#' p <- mtcars |> relocate(wt,mpg) |> arrange(wt) |> 
#'                group_by(cyl) |> ec.init(ctype= 'line')
#' p$x$opts$series <- lapply(p$x$opts$series, append, list(areaStyle= list(show=TRUE)) )
#' p 
#' 
#' 
#' #------ Plugin leaflet
#' if (interactive()) {
#' tmp <- quakes |> dplyr::relocate('long') |>  # set order to lon,lat
#'   dplyr::mutate(size= exp(mag)/20) |> head(100)   # add accented size
#' p <- tmp |> ec.init(load='leaflet')
#' p$x$opts$series[[1]]$name = 'quakes'
#' p$x$opts$series[[1]]$symbolSize = ec.clmn(6, scale=2)   # size column
#' p$x$opts$tooltip = list(formatter=ec.clmn('magnitude %@', 'mag'))
#' p$x$opts$legend = list(show=TRUE)
#' p
#' }
#'
#' #------ Plugin 'world' with visualMap
#' if (interactive()) {
#' cns <- data.frame(   
#'   country = c('United States','China','Russia'),
#'   value = runif(3, 1, 100)
#' )
#' p <- cns |> group_by(country) |> ec.init(load='world',
#'     tl.series=list(type='map', encode=list(value='value', name='country')) )
#' p$x$opts$visualMap <- list(calculable=TRUE, max=100)
#' p$x$opts$toolbox <- list(feature= list(restore= list()))
#' p
#' }
#'
#' #------ Plugin 'world' with lines and color coding
#' if (interactive()) {
#' flights <- NULL
#' flights <- try(read.csv(paste0('https://raw.githubusercontent.com/plotly/datasets/master/',
#'                                '2011_february_aa_flight_paths.csv')), silent = TRUE)
#' if (!is.null(flights)) {
#'   tmp <- data.frame(airport1 = unique(head(flights,10)$airport1),
#'                     color = c("#387e78","#eeb422","#d9534f",'magenta'))
#'   tmp <- head(flights,10) |> inner_join(tmp)    # add color by airport
#'   p <- ec.init(load='world')
#'   p$x$opts$geo$center <- c(mean(flights$start_lon), mean(flights$start_lat))
#'   p$x$opts$geo$zoom <- 7
#'   p$x$opts$series <- list(list(
#'     type='lines', coordinateSystem='geo',
#'     data = lapply(ec.data(tmp, 'names'), function(x)
#'       list(coords = list(c(x$start_lon,x$start_lat),
#'                          c(x$end_lon,x$end_lat)),
#'            colr = x$color)
#'     )
#'     ,lineStyle = list(curveness=0.3, width=3, color=ec.clmn('colr'))
#'   ))
#'   p
#' } }
#' 
#' 
#' #------ registerMap JSON
#' json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
#' dusa <- USArrests
#' dusa$states <- row.names(dusa)
#' p <- ec.init(preset=FALSE)
#' p$x$registerMap <- list(list(mapName= 'USA', geoJSON= json))
#' #   registerMap supports also maps in SVG format, see website gallery
#' p$x$opts$visualMap <- list(type= 'continuous', calculable= TRUE, 
#'                            min= min(dusa$UrbanPop), max= max(dusa$UrbanPop))
#' p$x$opts$series <- list(list(type= 'map', map= 'USA', 
#'    roam= TRUE, zoom= 3, left= -100, top= -30,
#'    data= lapply(ec.data(dusa,'names'), function(x) list(name=x$states, value=x$UrbanPop))
#' ))
#' p
#'
#'
#' #------ locale
#' mo <- seq.Date(Sys.Date() - 444, Sys.Date(), by= "month")
#' df <- data.frame(date= mo, val= runif(length(mo), 1, 10))
#' p <- df |> ec.init(title=list(text='locale test'))
#' p$x$locale <- 'ZH'
#' p$x$renderer <- 'svg'
#' p
#' 
#' 
#' #------ Pie
#' is <- sort(islands); is <- is[is>60]
#' is <- data.frame(name=names(is), value=as.character(unname(is)))
#' data <- ec.data(is, 'names')
#' p <- ec.init()
#' p$x$opts <- list(
#'   title = list(text = "Landmasses over 60,000 mi\u00B2", left = 'center'),
#'   tooltip = list(trigger='item'),
#'   series = list(type='pie', radius='50%', data=data, name='mi\u00B2'))
#' p
#' 
#' 
#' #------ Liquidfill plugin
#' if (interactive()) {
#'  p <- ec.init(load=c('liquid'), preset=FALSE)
#'  p$x$opts$series[[1]] <- list(
#'    type='liquidFill', data=c(0.6, 0.5, 0.4, 0.3), # amplitude=0,
#'    waveAnimation=FALSE, animationDuration=0, animationDurationUpdate=0
#'  )
#'  p
#' }
#' 
#' 
#' #------ Heatmap
#' times <- c(5,1,0,0,0,0,0,0,0,0,0,2,4,1,1,3,4,6,4,4,3,3,2,5,7,0,0,0,0,0,
#'            0,0,0,0,5,2,2,6,9,11,6,7,8,12,5,5,7,2,1,1,0,0,0,0,0,0,0,0,3,2,
#'            1,9,8,10,6,5,5,5,7,4,2,4,7,3,0,0,0,0,0,0,1,0,5,4,7,14,13,12,9,5,
#'            5,10,6,4,4,1,1,3,0,0,0,1,0,0,0,2,4,4,2,4,4,14,12,1,8,5,3,7,3,0,
#'            2,1,0,3,0,0,0,0,2,0,4,1,5,10,5,7,11,6,0,5,3,4,2,0,1,0,0,0,0,0,
#'            0,0,0,0,1,0,2,1,3,4,0,0,0,0,1,2,2,6)
#' df <- NULL; n <- 1; 
#' for(i in 0:6) { df <- rbind(df, data.frame(0:23, rep(i,24), times[n:(n+23)]));  n<-n+24  }
#' hours <- ec.data(df); hours <- hours[-1]    # remove columns row
#' times <- c('12a',paste0(1:11,'a'),'12p',paste0(1:11,'p'))
#' days <- c('Saturday','Friday','Thursday','Wednesday','Tuesday','Monday','Sunday')
#' p <- ec.init(preset=FALSE)
#' p$x$opts <- list( title = list(text='Punch Card Heatmap'),
#'   tooltip = list(position='top'),grid=list(height='50%',top='10%'),
#'   xAxis = list(type='category', data=times, splitArea=list(show=TRUE)),
#'   yAxis = list(type='category', data=days,  splitArea=list(show=TRUE)),
#'   visualMap = list(min=0,max=10,calculable=TRUE,orient='horizontal',left='center',bottom='15%'),
#'   series = list(list(name='Hours', type = 'heatmap', data= hours,label=list(show=TRUE),
#'       emphasis=list(itemStyle=list(shadowBlur=10,shadowColor='rgba(0,0,0,0.5)'))))
#' )
#' p
#' 
#' 
#' #------ Plugin 3D
#' if (interactive()) {
#'  data <- list()
#'  for(y in 1:dim(volcano)[2]) for(x in 1:dim(volcano)[1]) 
#'    data <- append(data, list(c(x, y, volcano[x,y])))
#'  p <- ec.init(load= '3D')
#'  p$x$opts$series <- list(type= 'surface',	data= data)
#'  p
#' }
#'
#'
#' #------ 3D chart with custom item size, improved readability with ec.snip
#' if (interactive()) {
#' p <- iris |> group_by(Species) |>
#'      mutate(size= log(Petal.Width*10)) |>  # add 6th column accented size
#'      ec.init(load= '3D') |> ec.snip()
#' p$xAxis3D <- list(name= 'Petal.Length')
#' p$yAxis3D <- list(name= 'Sepal.Width')
#' p$zAxis3D <- list(name= 'Sepal.Length')
#' p$series <- lapply(p$series, function(s) {  # update preset series
#'                    s$symbolSize <- ec.clmn(6, scale=10); s })
#' p$legend <- list(show= TRUE)
#' ec.snip(p)
#' }
#'
#'
#' #------ Surface data equation with JS code
#' if (interactive()) {
#' p <- ec.init(load= '3D')
#' p$x$opts$series[[1]] <- list(
#'   type= 'surface',
#'   equation= list(
#'     x = list(min= -3, max= 4, step= 0.05), 
#'     y = list(min= -3, max= 3, step= 0.05), 
#'     z = htmlwidgets::JS("function (x, y) { 
#'           return Math.sin(x * x + y * y) * x / Math.PI; }")
#'   )
#' )
#' p
#' }
#'
#'
#' #------ Surface with data from a data.frame
#' if (interactive()) {
#' library(dplyr)
#' data <- expand.grid(
#'   x = seq(0, 2, by = 0.1),
#'   y = seq(0, 1, by = 0.1)
#' ) |> mutate(z = x * (y ^ 2)) |> select(x,y,z)
#' p <- ec.init(load= '3D')
#' p$x$opts$series[[1]] <- list(
#'   type= 'surface',
#'   data= ec.data(data, 'values'))
#' p
#' }
#'
#'  
#' #------ Band serie with customization
#' # first column ('day') usually goes to the X-axis
#' # try also alternative data setting - replace lines *1 with *2
#' if (interactive()) {
#' library(dplyr)
#' dats <- as.data.frame(EuStockMarkets) |> mutate(day= 1:n()) |>
#'           relocate(day) |> slice_head(n= 100)
#' p <- ec.init(load= 'custom')            # *1 = unnamed data
#' #p <- dats |> ec.init(load= 'custom')   # *2 = dataset
#' p$x$opts$series = append(
#'   ecr.band(dats, 'DAX','FTSE', name= 'Ftse-Dax', color= 'lemonchiffon'),
#'   list(list(type='line', name='CAC', color='red', symbolSize=1,
#'             data= ec.data(dats |> select(day,CAC), 'values')   # *1
#'             # encode=list(x='day', y='CAC')   # *2
#'   ))
#' )
#' p$x$opts$legend <- list(show=TRUE) 
#' p$x$opts$dataZoom <- list(type= 'slider', end= 50)
#' p
#' }
#' 
#' #------ Timeline animation and ec.snip
#' p <- Orange |> dplyr::group_by(age) |> ec.init(
#'   tl.series=list(type= 'bar', encode= list(x='Tree', y='circumference'))
#' ) |> ec.snip()
#' p$timeline <- append(p$timeline, list(autoPlay= TRUE))
#' p$options <- lapply(p$options, 
#'          function(o) { o$title$text <- paste('age',o$title$text,'days'); o })
#' p$xAxis <- list(type= 'category', name= 'tree')
#' p$yAxis <- list(max= max(Orange$circumference))
#' ec.snip(p)
#' 
#' 
#' #------ Timeline with pies
#' df <- data.frame(
#'   group= c(1,1,1,1,2,2,2,2),
#'   type=  c("type1","type1","type2","type2","type1","type1","type2","type2"),
#'   value= c(5,2,2,1,4,3,1,4),
#'   label= c("name1","name2","name3","name4","name1","name2","name3","name4"),
#'   color= c("blue","purple","red","gold","blue","purple","red","gold")
#' )
#' p <- df |> group_by(group) |> ec.init( preset= FALSE,
#'          tl.series= list(type= 'pie', roseType= 'radius',
#'                          encode=list(value='value', itemName='type')
#'          ))
#' p$x$opts$options <- lapply(p$x$opts$options, function(s) {
#'   s$series[[1]]$itemStyle <- list(color=ec.clmn(5))
#'   s$series[[1]]$label <- list(formatter=ec.clmn(4))
#'   s
#' })
#' p$x$opts$legend <- list(selectedMode= "single")
#' p
#'
#'
#' #------ Boxplot
#' ds <- mtcars |> relocate(am,mpg) |> group_by(cyl) |> 
#'       ec.data(format= 'boxplot')
#' p <- ec.init()
#' p$x$opts <- list(
#'   dataset= ds$dataset, 
#'   series=  ds$series, 
#'   yAxis= list(type= 'category'),
#'   xAxis= list(show= TRUE),
#'   legend= list(show= TRUE)
#' )
#' p 
#' 
#' 
#' #------ ECharts feature: custom transform - a regression line
#' # presets for xAxis,yAxis,dataset and series are used
#' dset <- data.frame(x= 1:10, y= sample(1:100,10))
#' p <- dset |> ec.init(js= 'echarts.registerTransform(ecStat.transform.regression)')
#' p$x$opts$dataset[[2]] <- list(transform = list(type= 'ecStat:regression'))
#' p$x$opts$series[[2]] <- list(
#'   type= 'line', itemStyle=list(color= 'red'), datasetIndex= 1)
#' p 
#' 
#' 
#' #------ ECharts: dataset, transform and sort
#' datset <- list(
#'   list(source=list(
#'     list('name', 'age', 'profession', 'score', 'date'),
#'     list('Hannah Krause', 41, 'Engineer', 314, '2011-02-12'),
#'     list('Zhao Qian', 20, 'Teacher', 351, '2011-03-01'),
#'     list('Jasmin Krause', 52, 'Musician', 287, '2011-02-14'),
#'     list('Li Lei', 37, 'Teacher', 219, '2011-02-18'),
#'     list('Karle Neumann', 25, 'Engineer', 253, '2011-04-02'),
#'     list('Adrian Groß', 19, 'Teacher', NULL, '2011-01-16'),
#'     list('Mia Neumann', 71, 'Engineer', 165, '2011-03-19'),
#'     list('Böhm Fuchs', 36, 'Musician', 318, '2011-02-24'),
#'     list('Han Meimei', 67, 'Engineer', 366, '2011-03-12'))),
#'   list(transform = list(type= 'sort', config=list(
#'     list(dimension='profession', order='desc'),
#'     list(dimension='score', order='desc'))
#'   )))
#' p <- ec.init(title= list(
#'         text= 'Data transform, multiple-sort bar', 
#'         subtext= 'JS source',
#'         sublink= paste0('https://echarts.apache.org/next/examples/en/editor.html',
#'                    '?c=doc-example/data-transform-multiple-sort-bar'), 
#'         left= 'center')) 
#' p$x$opts$dataset <- datset
#' p$x$opts$xAxis <- list(type= 'category', axisLabel= list(interval=0, rotate=30))
#' p$x$opts$yAxis <- list(name= 'score')
#' p$x$opts$series[[1]] <- list(
#'   type= 'bar',
#'   label= list(show= TRUE, rotate= 90, position= 'insideBottom',
#'               align= 'left', verticalAlign= 'middle'
#'   ),
#'   itemStyle =list(
#'     color= htmlwidgets::JS("function (params) {
#'       return ({
#'         Engineer: '#5470c6',
#'         Teacher: '#91cc75',
#'         Musician: '#fac858'
#'       })[params.data[2]]
#'     }")
#'   ),
#'   encode= list(x= 'name', y= 'score', label= list('profession') ),
#'   datasetIndex= 1
#' )
#' p$x$opts$tooltip <- list(trigger= 'item', axisPointer= list(type= 'shadow'))
#' p
#'
#'
#' #------ Sunburst
#' # see website for different ways to set hierarchical data
#' data = list(list(name='Grandpa',children=list(list(name='Uncle Leo',value=15,
#'      children=list(list(name='Cousin Jack',value=2), list(name='Cousin Mary',value=5,
#'      children=list(list(name='Jackson',value=2))), list(name='Cousin Ben',value=4))), 
#'    list(name='Father',value=10,children=list(list(name='Me',value=5), 
#'    list(name='Brother Peter',value=1))))), list(name='Nancy',children=list(
#'    list(name='Uncle Nike',children=list(list(name='Cousin Betty',value=1), 
#'    list(name='Cousin Jenny',value=2))))))
#' p <- ec.init()
#' p$x$opts <- list(
#'   series = list(list(type='sunburst', data=data, 
#'                      radius=list(0, '90%'), label=list(rotate='radial')
#' )))
#' p
#' 
#' 
#' #------ Error Bars on grouped data
#' if (interactive()) {
#' library(dplyr)
#' df <- mtcars |> group_by(cyl,gear) |> summarise(yy=round(mean(mpg),2)) |>
#'   mutate(low=round(yy-cyl*runif(1),2), high=round(yy+cyl*runif(1),2)) |>
#'   relocate(cyl, .after = last_col())   # move group column behind first four cols
#' p <- df |> ec.init(ctype='bar', load='custom') |>
#'   ecr.ebars(df, name = 'eb'
#'      ,tooltip = list(formatter=ec.clmn('high <b>%@</b><br>low <b>%@</b>', 4,3)))
#' p$x$opts$tooltip <- list(show=TRUE)
#' p
#' }
#' 
#' #------ Gauge
#' p <- ec.init(preset=FALSE); 
#' p$x$opts$series <- list(list( 
#'   type = 'gauge', max = 160, min=40,
#'   detail = list(formatter='\U1F9E0={value}'), 
#'   data = list(list(value=85, name='IQ test')) ))
#' p
#' 
#' 
#' #------ Custom gauge with animation
#' p <- ec.init(js = "setInterval(function () { 
#'    opts.series[0].data[0].value = (Math.random() * 100).toFixed(2) - 0;  
#'    chart.setOption(opts, true);}, 2000);")
#' p$x$opts <- list(series=list(list(type = 'gauge',
#'   axisLine = list(lineStyle=list(width=30,
#'     color = list(c(0.3, '#67e0e3'),c(0.7, '#37a2da'),c(1, '#fd666d')))),
#'   pointer = list(itemStyle=list(color='auto')), 
#'   axisTick = list(distance=-30,length=8, lineStyle=list(color='#fff',width=2)),
#'   splitLine = list(distance=-30,length=30, lineStyle=list(color='#fff',width=4)),
#'   axisLabel = list(color='auto',distance=40,fontSize=20),
#'   detail = list(valueAnimation=TRUE, formatter='{value} km/h',color='auto'),
#'   data = list(list(value=70))
#' )))
#' p
#' 
#' 
#' #------ Sankey and graph plots
#' # prepare data
#' sankey <- data.frame(
#'   node = c("a","b", "c", "d", "e"),
#'   source = c("a", "b", "c", "d", "c"),
#'   target = c("b", "c", "d", "e", "e"),
#'   value = c(5, 6, 2, 8, 13),
#'   stringsAsFactors = FALSE
#' )
#' p <- ec.init(preset=FALSE)
#' p$x$opts$series[[1]] <- list( type='sankey',
#'   data = lapply(ec.data(sankey,'names'), function(x) list(name=x$node)),
#'   edges = ec.data(sankey,'names')
#' )
#' p
#'
#'
#' # graph plot with same data ---------------
#' p <- ec.init(preset= FALSE, title= list(text= 'Graph'))
#' p$x$opts$series[[1]] <- list( 
#'   type= 'graph',
#'   layout= 'force',   # try 'circular' too
#'   data= lapply(ec.data(sankey,'names'),
#'              function(x) list(name= x$node, tooltip= list(show=FALSE))),
#'   edges= lapply(ec.data(sankey,'names'),
#'              function(x) { x$lineStyle <- list(width=x$value); x }),
#'   emphasis= list(focus= 'adjacency',
#'                   label= list(position= 'right', show=TRUE)),
#'   label= list(show=TRUE), roam= TRUE, zoom= 4,
#'   tooltip= list(textStyle= list(color= 'blue')),
#'   lineStyle= list(curveness= 0.3)
#' )
#' p$x$opts$tooltip <- list(trigger='item')
#' p
#' 
#' 
#' #------ group connect 
#' main <- mtcars |> ec.init(height = 200)
#' main$x$opts$series[[1]]$name <- "this legend is shared"
#' main$x$opts$legend <- list(show=FALSE)
#' main$x$group <- 'group1' # same group name for all charts
#' 
#' q1 <- main; q1$x$opts$series[[1]]$encode <- list(y='hp', x='mpg'); 
#' q1$x$opts$legend <- list(show=TRUE)  # show first legend to share
#' q2 <- main; q2$x$opts$series[[1]]$encode <- list(y='wt', x='mpg'); 
#' q3 <- main; q3$x$opts$series[[1]]$encode <- list(y='drat', x='mpg'); 
#' q4 <- main; q4$x$opts$series[[1]]$encode <- list(y='qsec', x='mpg'); 
#' q4$x$connect <- 'group1'
#' # q4$x$disconnect <- 'group1'  # ok too
#' if (interactive()) {
#'   ec.layout(list(q1,q2,q3,q4), cols=2, title='group connect')
#' }
#' 
#' 
#' #------ Events in Shiny
#' if (interactive()) {
#'   library(shiny); library(dplyr); library(echarty)
#' 
#' ui <- fluidPage( ecs.output('plot') )
#' server <- function(input, output, session) {
#'   output$plot <- ecs.render({
#'     p <- cars |> group_by(speed) |> ec.init()
#'     p$x$opts$dataZoom <- list(type= 'inside')
#'     p$x$on <- list(         # event(s) with Javascript handler
#'       list(event= 'legendselectchanged',
#'            handler= htmlwidgets::JS("(event) => alert('selected: '+event.name);"))
#'     )
#'     p$x$capture <- 'datazoom'
#'     p
#'   })
#'   observeEvent(input$plot_datazoom, {   # captured event
#'     cat('\nZoom.start:',input$plot_datazoom$batch$start)
#'   })
#'   observeEvent(input$plot_mouseover, {  # built-in event
#'     cat('\n',toString(input$plot_mouseover))
#'   })
#' }
#' shinyApp(ui = ui, server = server)
#' }
#' 
#' #------------- Shiny interactive charts demo ---------------
#' #  run command: demo(eshiny)
#' 
#' }  # donttest
#' @export 
ec.examples <- function(){
  cat("copy/paste code from ?ec.examples Help\n Or run all examples at once with example('ec.examples') to see in the Viewer.")
}

