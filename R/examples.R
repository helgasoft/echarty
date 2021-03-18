# ----------- Examples --------------

#' Code Examples
#' 
#' Learn by example - copy/paste code from Examples below.\cr
#' This code collection is to demonstrate various concepts of 
#'   data preparation, conversion, grouping, 
#'   parameter setting, visual fine-tuning, 
#'   custom rendering, plugins attachment, 
#'   Shiny plots & interactions through Shiny proxy.\cr
#' Example usage is shown for all commands of this package.
#' 
#' @return No return value, used for help text
#' 
#' @examples  
#' \donttest{
#' 
#' #------ Basic scatter chart, instant display
#' cars %>% ec.init()
#' 
#' #------ Same chart, change theme and save for further processing
#' p <- cars %>% ec.init() %>% ec.theme('dark')
#' p
#' 
#' #------ JSON back and forth
#' tmp <- cars %>% ec.init()
#' tmp
#' json <- tmp %>% ec.inspect()
#' ec.fromJson(json) %>% ec.theme("dark")
#'
#'
#' #------ Data grouping
#' iris %>% dplyr::group_by(Species) %>% ec.init()     # by factor column
#' 
#' p <- Orange %>% dplyr::group_by(Tree) %>% ec.init() # no factor column
#' p$x$opts$series <- lapply(p$x$opts$series, function(x) { 
#'   x$symbolSize=10; x$encode=list(x='age', y='circumference'); x } )
#' p
#'
#'
#' #------ Pie
#' is <- sort(islands); is <- is[is>60]
#' is <- data.frame(name=names(is), value=as.character(unname(is)))
#' data <- apply(unname(is), 1, function(x) list(name=x[1], value=x[2]))
#' p <- ec.init()
#' p$x$opts <- list(
#'   title = list(text = "Landmasses over 60,000 mi\u00B2", left = 'center'),
#'   tooltip = list(trigger='item'),
#'   series = list(type='pie', radius='50%', data=data, name='mi\u00B2',
#'     emphasis=list(itemStyle=list(shadowBlur=10, shadowColor='rgba(0,0,0,0.5)'))))
#' p
#' 
#' #------ Liquidfill plugin
#' if (interactive()) {
#' p <- ec.init(load=c('liquid'), preset=FALSE)
#' p$x$opts$series[[1]] <- list(
#'   type='liquidFill', data=c(0.6, 0.5, 0.4, 0.3), # amplitude=0,
#'   waveAnimation=FALSE, animationDuration=0, animationDurationUpdate=0
#' )
#' p
#' }
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
#' #------ Plugin leaflet
#' tmp <- quakes %>% dplyr::relocate('long')  # set lon,lat order
#' p <- tmp %>% ec.init(load='leaflet')
#' p$x$opts$legend = list(data=list(list(name='quakes')))
#' p$x$opts$series[[1]]$name = 'quakes'
#' p$x$opts$series[[1]]$symbolSize = htmlwidgets::JS("function(x){ return x[3];}")
#' p
#'  
#' #------ Plugin 3D
#' if (interactive()) {
#' p <- ec.init(load = '3D')
#' p$x$opts$series <- list(
#'   type = 'surface',
#'   data = ec.data(as.data.frame(as.table(volcano)), 'values')  
#' )
#' p
#' }
#' 
#' #------ 3D chart with custom coloring
#' if (interactive()) {
#' p <- iris %>% ec.init(load = '3D')
#' p$x$opts$series[[1]] <- list(
#'   type='scatter3D', symbolSize=7, 
#'   encode=list(x='Sepal.Length', y='Sepal.Width', z='Petal.Length'),
#'   itemStyle=list(color = htmlwidgets::JS("function(params){
#'           if (params.value[4] == 1){ return '#FE8463'; }
#'      else if (params.value[4] == 2){ return '#27727B'; }
#'                                      return '#9BCA63';
#'   }") )  # [4] is the JS index of column Species
#' )
#' p
#' }
#' 
#' #------ Surface data equation with JS code
#' if (interactive()) {
#' p <- ec.init(load='3D')
#' p$x$opts$series[[1]] <- list(
#'   type = 'surface',
#'   equation = list(
#'     x = list(min=-3,max=4,step=0.05), 
#'     y = list(min=-3,max=3,step=0.05), 
#'     z = htmlwidgets::JS("function (x, y) { 
#'           return Math.sin(x * x + y * y) * x / Math.PI; }")
#'   )
#' )
#' p
#' }
#'
#' #------ Surface with data from a data.frame
#' if (interactive()) {
#' library(dplyr)
#' data <- expand.grid(
#'   x = seq(0, 2, by = 0.1),
#'   y = seq(0, 1, by = 0.1)
#' ) %>% mutate(z = x * (y ^ 2)) %>% select(x,y,z)
#' p <- ec.init(load='3D')
#' p$x$opts$series[[1]] <- list(
#'   type = 'surface',
#'   data = ec.data(data, 'values'))
#' p
#' }
#' 
#' #------ Band serie with customization
#' # first column ('day') usually goes to the X-axis
#' # try also alternative data setting - replace lines @1 & @2 with
#' #   @1: p <- dats %>% ec.init(load='custom') 
#' #   @2: encode=list(x='day', y='CAC')
#' library(dplyr)
#' dats <- as.data.frame(EuStockMarkets) %>% mutate(day=1:n()) %>%
#'   relocate(day) %>% slice_head(n=200)
#' p <- ec.init(load='custom')            # @1
#' p$x$opts <- list(
#'   xAxis = list(list()),
#'   yAxis = list(list()),
#'   series = list(
#'     append( ecr.band(dats, 'DAX','FTSE'), list(
#'       name='band', color='lemonchiffon')),  # band + customize
#'     list(type='line', name='CAC', color='red', symbolSize=1,
#'          data = ec.data(dats %>% select(day,CAC), 'values') )  # @2
#'   ),
#'   legend = list(data=list(
#'     list(name='band'), list(name='CAC') )),
#'   dataZoom = list(list(type='slider',start=50))
#' )
#' p
#' 
#' #------ Timeline animation
#' orng <- Orange
#' orng$Tree <- paste('tree',as.character(orng$Tree)) # to string
#' series <- list()
#' options <- list()
#' z <- 0
#' for(i in unique(orng$age)) {
#'   z <- z + 1
#'   myser <- list()
#'   for(k in unique(orng$Tree)) {
#'     if (z==1) series <- append(series,
#'            list(list(type='bar', label=list(show=TRUE))))
#'     y <- orng %>% dplyr::filter(age==i, Tree==k) %>% 
#'       dplyr::select(circumference) %>% unlist()
#'     myser <- append(myser, list(list(data=list(
#'       list(name=k, value=as.numeric(y))))))
#'   }
#'   options[[z]] <- list(title=list(
#'     text=paste('Age',i,'days'), left='center'), series=myser
#'   )
#' }
#' p <- ec.init()
#' p$x$opts$xAxis <- list(list(type='category', name='5 orange trees', nameLocation='center'))
#' p$x$opts$yAxis <- list(list(type='value', name='circumference (mm)',
#'                    max=240, nameRotate=90, nameLocation='center',nameGap=33))
#' p$x$opts$timeline <- list(axisType='category',
#'                    playInterval=1000, autoPlay=TRUE,
#'                    data=c(as.character(unique(orng$age))))
#' p$x$opts$series <- series
#' p$x$opts$options <- options
#' p
#' 
#' #------ Groups in a boxplot
#' # original JS: https://echarts.apache.org/examples/en/editor.html?c=boxplot-multi
#' grps <- list()   # data in 3 groups
#' for (grp in 1:3) {
#'   seriesData <- list()
#'   for (i in 1:18) {
#'     cate <- runif(10, 1, 200)
#'     seriesData <- append(seriesData, list(cate))
#'   }
#'   tmp <- lapply(seriesData, boxplot.stats)
#'   grps[[grp]] <- lapply(tmp, function(x) x$stats)
#' }
#' p <- ec.init() 
#' for (grp in 1:3) {
#'   p$x$opts$series[[grp]] <- list(
#'     name = paste0('category', grp),
#'     type = 'boxplot',
#'     data = grps[[grp]]
#'   )
#' }
#' p$x$opts$xAxis <- list(
#'   type = 'category',
#'   data = as.character(1:18),
#'   boundaryGap = TRUE, nameGap = 30,
#'   splitArea = list(show=TRUE),
#'   axisLabel = list(formatter = "exprmt {value}"),
#'   splitLine = list(show=FALSE)
#' )
#' p$x$opts$yAxis <- list(
#'   type = 'value', name = 'Value',
#'   min = -200, max = 400,
#'   splitArea = list(show=FALSE)  
#' )
#' p$x$opts$legend$data <- list('category1', 'category2', 'category3')
#' p$x$opts$tooltip <- list(trigger='item', axisPointer=list(type='shadow'))
#' p$x$opts$dataZoom <- list(list(type='slider',start=50)) 
#' p
#' 
#' #------ EChartsJS v.5 feature custom transform - a regression line
#' # presets for xAxis,yAxis,dataset and series are used
#' dset <- data.frame(x=1:10, y=sample(1:100,10))
#' p <- dset %>% ec.init(js='echarts.registerTransform(ecStat.transform.regression)')
#' p$x$opts$dataset[[2]] <- list(transform = list(type='ecStat:regression'))
#' p$x$opts$series[[2]] <- list(
#'   type='line', itemStyle=list(color='red'), datasetIndex=1)
#' p 
#' 
#' #------ EChartsJS v.5 features transform and sort
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
#' p <- ec.init(title = list(
#'         text='Data transform, multiple-sort bar', 
#'         subtext='JS source v.5',
#'         sublink=paste0('https://echarts.apache.org/next/examples/en/editor.html',
#'                  '?c=doc-example/data-transform-multiple-sort-bar'), 
#'         left='center')) 
#' p$x$opts$dataset <- datset
#' p$x$opts$xAxis <- list(type = 'category', axisLabel=list(interval=0, rotate=30))
#' p$x$opts$yAxis <- list(name='score')
#' p$x$opts$series[[1]] <- list(
#'   type='bar',
#'   label=list( show=TRUE, rotate=90, position='insideBottom',
#'               align='left', verticalAlign='middle'
#'   ),
#'   itemStyle=list(
#'     color = htmlwidgets::JS("function (params) {
#'       return ({
#'         Engineer: '#5470c6',
#'         Teacher: '#91cc75',
#'         Musician: '#fac858'
#'       })[params.data[2]]
#'     }")
#'   ),
#'   encode=list( x='name', y='score', label=list('profession') ),
#'   datasetIndex = 1
#' )
#' p$x$opts$tooltip <- list(trigger='item', axisPointer=list(type='shadow'))
#' p
#'
#' #------ Sunburst
#' data = list(list(name='Grandpa',children=list(list(name='Uncle Leo',value=15,
#'      children=list(list(name='Cousin Jack',value=2), list(name='Cousin Mary',value=5,
#'      children=list(list(name='Jackson',value=2))), list(name='Cousin Ben',value=4))), 
#'    list(name='Father',value=10,children=list(list(name='Me',value=5), 
#'    list(name='Brother Peter',value=1))))), list(name='Nancy',children=list(
#'    list(name='Uncle Nike',children=list(list(name='Cousin Betty',value=1), 
#'    list(name='Cousin Jenny',value=2))))))
#' p <- ec.init()
#' p$x$opts <- list(
#'   series = list(list(type='sunburst', data=data, radius=list(0, '90%'),label=list(rotate='radial')))
#' )
#' p
#' 
#' #------ registerMap JSON
#' json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
#' dusa <- USArrests %>% dplyr::mutate(states = row.names(.))
#' p <- ec.init(preset=FALSE)
#' p$x$registerMap <- list(list(mapName = 'USA', geoJSON = json))
#' p$x$opts <- list(
#'   visualMap = list(type='continuous', calculable=TRUE, 
#'                    min=min(dusa$UrbanPop), max=max(dusa$UrbanPop))
#'   ,series = list( list(type='map', map='USA', name='UrbanPop', roam=TRUE,
#'        data = lapply(ec.data(dusa,'names'), function(x) list(name=x$states, value=x$UrbanPop))
#'    ))
#' )
#' p  
#' 
#' #------ Gauge
#' p <- ec.init(preset = FALSE); 
#' p$x$opts$series[[1]] <- list( 
#'   type = 'gauge', max=160, min=40,
#'   detail = list(formatter = '🧠={value}', fontSize=20), 
#'   data = list(list(value=85, name='IQ test')) )
#' p
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
#' 
#' p <- ec.init(preset=FALSE)
#' p$x$opts$series[[1]] <- list( type='sankey',
#'   data = lapply(ec.data(sankey,'names'), function(x) list(name=x$node)),
#'   edges = ec.data(sankey,'names')
#' )
#' p
#' 
#' # graph plot with same data ---------------
#' p <- ec.init(preset=FALSE, title=list(text="Graph"))
#' p$x$opts$series[[1]] <- list( type='graph',
#'   layout = 'force',   # try 'circular' too
#'   data = lapply(ec.data(sankey,'names'),
#'              function(x) list(name=x$node, tooltip = list(show=FALSE))),
#'   edges = lapply(ec.data(sankey,'names'),
#'              function(x) { x$lineStyle <- list(width=x$value); x }),
#'   emphasis = list(focus='adjacency',
#'                   label=list( position='right', show=TRUE)),
#'   label = list(show=TRUE), roam = TRUE, zoom = 4,
#'   tooltip=list(textStyle=list(color='blue')),
#'   lineStyle = list(curveness=0.3)
#' )
#' p$x$opts$tooltip <- list(trigger='item')
#' p
#' 
#'    
#' #------ Shiny interactive chart
#' if (interactive()) {
#' library(shiny)
#' library(dplyr)
#' runApp( list(
#'   ui = fluidPage(
#'     ecs.output('plot'),
#'     actionButton('addm', 'Add marks'),
#'     actionButton('delm', 'Del area+line marks'), HTML('&nbsp; &nbsp; &nbsp; &nbsp;'),
#'     actionButton('adds', 'Add serie'),
#'     actionButton('dels', 'Del serie'), HTML('&nbsp; &nbsp; &nbsp; &nbsp;'),
#'     actionButton('hilit', 'Highlight'),
#'     actionButton('dnplay', 'Downplay')
#'   ),
#'   server = function(input, output, session){
#' 
#'     output$plot <- ecs.render({
#'       p <- mtcars %>% relocate(disp, .after=mpg) %>% group_by(cyl) %>% ec.init()
#'       p$x$opts$tooltip <- list(list(show=TRUE))
#'       p$x$opts$series[[1]]$emphasis <- list(focus='series', blurScope='coordinateSystem')
#'       p
#'     })
#' 
#'     observeEvent(input$addm, {
#'       p <- ecs.proxy('plot')
#'       p$x$opts$series[[1]] = list(
#'         markPoint = list(data = list(
#'           list(coord = c(22.5, 140.8)),
#'           list(coord = c(30.5, 95.1))
#'         ), itemStyle = list(color='lightblue')
#'         )
#'         ,markArea = list(data = list(list(
#'           list(xAxis = 15),
#'           list(xAxis = 25)
#'         ))
#'         ,silent=TRUE
#'         ,itemStyle = list(color='pink', opacity=0.2)
#'         ,label = list(formatter='X-area', position='insideTop')
#'         )
#'         ,markLine = list(data = list(list(type='average')))
#'       )
#'       p %>% ecs.exec() # ='p_merge'
#'     })
#'     observeEvent(input$adds, {
#'       p <- ecs.proxy('plot')
#'       p$x$opts$series[[1]] <- list(
#'         type = 'line', name = 'newLine',
#'         encode = list(x='mpg', y='disp')
#'       )
#'       p %>% ecs.exec('p_update')
#'     })
#'     observeEvent(input$dels, {
#'       p <- ecs.proxy('plot')
#'       p$x$opts$seriesName <- 'newLine'
#'       # p$x$opts$seriesIndex <- 4  # alternative ok
#'       p %>% ecs.exec('p_del_serie')
#'     })
#'     observeEvent(input$delm, {
#'       p <- ecs.proxy('plot')
#'       p$x$opts$seriesIndex <- 1
#'       p$x$opts$delMarks <- c('markArea','markLine')
#'       p %>% ecs.exec('p_del_marks')
#'     })
#'     observeEvent(input$hilit, {
#'       p <- ecs.proxy('plot')
#'       p$x$opts <- list(type='highlight', seriesName='4')
#'       p %>% ecs.exec('p_dispatch')
#'     })
#'     observeEvent(input$dnplay, {
#'       p <- ecs.proxy('plot')
#'       p$x$opts <- list(type='downplay', seriesName='4')
#'       p %>% ecs.exec('p_dispatch')
#'     })
#'   }
#' ))
#' }
#' 
#' }
#' 
#' @export 
ec.examples <- function(){
  cat("copy/paste code from ?ec.examples Help\n Or run all examples at once with example('ec.examples') and they'll show up in the Viewer.")
}

