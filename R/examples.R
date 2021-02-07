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
#' @examples  
#' \dontrun{
#' 
#' #--- Basic scatter chart, instant display
#' cars %>% ec.init()
#' 
#' #--- Same chart, change theme and save for further processing
#' e <- cars %>% ec.init() %>% ec.theme('dark')
#' e
#' 
#' #--- JSON back and forth
#' tmp <- cars %>% ec.init()
#' tmp
#' json <- tmp %>% ec.inspect()
#' ec.fromJson(json) %>% ec.theme("dark")
#'
#' #--- Data grouping
#' iris %>% group_by(Species) %>% ec.init()     # by factor column
#' e <- Orange %>% group_by(Tree) %>% ec.init() # no factor column
#' e$x$opts$series[[1]] <- append(e$x$opts$series[[1]], list(
#'   symbolSize = 10
#' ))   # further customization added
#' e
#'
#' #--- Plugin leaflet
#' tmp <- quakes %>% dplyr::relocate('long')  # set lon,lat order
#' p <- tmp %>% ec.init(load='leaflet')
#' p$x$opts$legend = list(data=list(list(name='quakes')))
#' p$x$opts$series[[1]]$name = 'quakes'
#' p$x$opts$series[[1]]$symbolSize = htmlwidgets::JS("function(x){ return x[3];}")
#' p
#'  
#' #--- Plugin geo
#' tmp <- quakes %>% dplyr::relocate('long')
#' p <- ec.init(load=c('geo'), preset=FALSE)
#' p$x$opts$geo <- list(map='world', roam=TRUE, silent=TRUE)
#' # p$x$renderer <- "webgl"  # works too
#' p$x$opts$series[[1]] <- list(
#'   name ='quakes',
#'   type = 'scatter',
#'   coordinateSystem = 'geo',
#'   symbolSize = 5,
#'   itemStyle = list( color='red'),
#'   data = ec.data(tmp, series=TRUE)
#' )
#' p$x$opts$legend = list(data = list(list(name = 'quakes')))
#' p
#' 
#' #--- Plugins 3D and GL
#' e <- ec.init(load = c('3D','GL'))
#' e$x$opts$series[[1]] <- list(
#'   type = 'surface',
#'   data = ec.data(as.data.frame(as.table(volcano)), TRUE)
#' )
#' e 
#' 
#' #--- Visual map for 3D chart
#' library(onion)
#' library(tibble)
#' p <- as_tibble(bunny) %>%  #head(100) %>%
#'   ec.init(load = c('3D','GL')) 
#' p$x$opts$series[[1]] <- list(
#'   type='scatter3D', symbolSize=2, encode=list(x='x',y='y',z='z'))
#' p$x$opts$visualMap <- ec.vismap(
#'   as_tibble(bunny), inRange=list(color=rainbow(10)))
#' p
#' 
#' #--- Data as JS equation
#' e <- ec.init(load=c('3D','GL'))
#' e$x$opts$series[[1]] <- list(
#'   type = 'surface',
#'   equation = list(
#'     x = list(min=-3,max=4,step=0.05), 
#'     y = list(min=-3,max=3,step=0.05), 
#'     z = htmlwidgets::JS("function (x, y) { 
#'           return Math.sin(x * x + y * y) * x / Math.PI; }")
#'   )
#' )
#' e
#' 
#' #--- Data as R equation in a data.frame
#' library(tidyverse)
#' data <- expand_grid(
#'   y = seq(0, 1, by = 0.1),
#'   x = seq(0, 2, by = 0.1)
#' ) %>% mutate(z = x * (y ^ 2)) %>% select(x,y,z)
#' e <- ec.init(load=c('3D','GL'))
#' e$x$opts$series[[1]] <- list(
#'   type = 'surface',
#'   data = ec.data(data, TRUE))
#' e
#' 
#' #--- Band serie with customization
#' # Custom plugin for ec.sband's renderer
#' # initial or later data loading is shown (switch commented lines)
#' # first column usually sets the X-axis
#' library(dplyr)
#' dats <- as.data.frame(EuStockMarkets) %>% mutate(day=1:n()) %>% 
#'   relocate(day) %>% slice_head(n=200)
#' # e <- dats %>% ec.init(load='custom') # needs encode below
#' e <- ec.init(load='custom')            # need data below
#' e$x$opts$series[[1]] <- ec.sband(dats, 'DAX','FTSE')
#' e$x$opts$series[[1]] <- append(e$x$opts$series[[1]], list(
#'   name = 'band', color = 'lemonchiffon'
#' ))
#' e$x$opts$series[[2]] <- list(
#'   type='line', name='CAC', color='red', symbolSize=1,
#'   # encode=list(x='day', y='CAC'),
#'   data = ec.data(dats %>% select(day,CAC), TRUE) )
#' e$x$opts$legend = list(data=list(
#'   list(name='band'), list(name='CAC') ))
#' e$x$opts$dataZoom <- list(list(type='slider',start=50))
#' e
#' 
#' #--- Timeline animation
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
#' e <- ec.init()
#' e$x$opts$xAxis <- list(list(type='category', name='5 orange trees', nameLocation='center'))
#' e$x$opts$yAxis <- list(list(type='value', name='circumference (mm)',
#'                    max=240, nameRotate=90, nameLocation='center',nameGap=33))
#' e$x$opts$timeline <- list(axisType='category',
#'                    playInterval=1000, autoPlay=TRUE,
#'                    data=c(as.character(unique(orng$age))))
#' e$x$opts$series <- series
#' e$x$opts$options <- options
#' e
#' 
#' #--- Groups in a boxplot
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
#' e <- ec.init() 
#' for (grp in 1:3) {
#'   e$x$opts$series[[grp]] <- list(
#'     name = paste0('category', grp),
#'     type = 'boxplot',
#'     data = grps[[grp]]
#'   )
#' }
#' e$x$opts$xAxis <- list(
#'   type = 'category',
#'   data = as.character(1:18),
#'   boundaryGap = TRUE, nameGap = 30,
#'   splitArea = list(show=TRUE),
#'   axisLabel = list(formatter = "exprmt {value}"),
#'   splitLine = list(show=FALSE)
#' )
#' e$x$opts$yAxis <- list(
#'   type = 'value', name = 'Value',
#'   min = -200, max = 400,
#'   splitArea = list(show=FALSE)  
#' )
#' e$x$opts$legend$data <- list('category1', 'category2', 'category3')
#' e$x$opts$tooltip <- list(trigger='item', axisPointer=list(type='shadow'))
#' e$x$opts$dataZoom <- list(list(type='slider',start=50)) #list(type='inside',start=50), 
#' e
#' 
#' #--- EChartsJS v.5 feature custom transform - a regresssion line
#' # presets for xAxis,yAxis,dataset and series are used
#' dset <- data.frame(x=1:10, y=sample(1:100,10))
#' e <- dset %>% ec.init(js='echarts.registerTransform(ecStat.transform.regression)')
#' e$x$opts$dataset[[2]] <- list(transform = list(type='ecStat:regression'))
#' e$x$opts$series[[2]] <- list(
#'   type='line', symbolSize=0.1, symbol='circle', datasetIndex=1)
#' e 
#' 
#' #--- EChartsJS v.5 features transform and sort
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
#' e <- ec.init(title = list(
#'         text='Data transform, multiple-sort bar', 
#'         subtext='JS source v.5',
#'         sublink=paste0('https://echarts.apache.org/next/examples/en/editor.html',
#'                  '?c=doc-example/data-transform-multiple-sort-bar'), 
#'         left='center')) 
#' e$x$opts$dataset <- datset
#' e$x$opts$xAxis <- list(type = 'category', axisLabel=list(interval=0, rotate=30))
#' e$x$opts$yAxis <- list(name='score')
#' e$x$opts$series[[1]] <- list(
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
#' e$x$opts$tooltip <- list(trigger='item', axisPointer=list(type='shadow'))
#' e
#'
#' #--- Sankey and graph plots
#' # prepare data
#' library(tidyverse)
#' letters <- read_csv('https://raw.githubusercontent.com/jessesadler/intro-to-r/master/data/correspondence-data-1585.csv')
#' sources <- letters %>%
#'   distinct(source) %>% rename(label = source)
#' destinations <- letters %>%
#'   distinct(destination) %>% rename(label = destination)
#' nodes <- full_join(sources, destinations, by = "label")
#' nodes <- nodes %>% rowid_to_column("id")
#' per_route <- letters %>% group_by(source, destination) %>% summarise(weight = n()) %>% ungroup()
#' edges <- per_route %>% 
#'   left_join(nodes, by = c("source" = "label")) %>% rename(from = id)
#' edges <- edges %>% 
#'   left_join(nodes, by = c("destination" = "label")) %>% rename(to = id)
#' edges <- select(edges, from, to, weight)
#' 
#' # Sankey chart --------------
#' p <- ec.init(title=list(text="Daniel's letters 1585"), preset=FALSE)
#' p$x$opts$series[[1]] <- list( type='sankey',
#'     data = lapply(ec.data(nodes,TRUE), function(x) 
#'       list(name=x$value[2])),
#'     edges = lapply(ec.data(per_route,TRUE), function(x) 
#'       list(source=as.character(x$value[1]), 
#'            target=as.character(x$value[2]), value=x$value[3]) )
#' )
#' p$x$opts$tooltip <- list(trigger='item')
#' p
#' 
#' # graph plot with same data ---------------
#' p <- ec.init(preset=FALSE, title=list(text="Daniel's letters 1585", 
#'         subtext='data source',
#'         sublink='https://www.jessesadler.com/post/network-analysis-with-r/', 
#'         left='center'))
#' p$x$opts$series[[1]] <- list( type = 'graph', 
#'   layout = 'force', #' try 'circular' too
#'   nodes = lapply(ec.data(nodes,TRUE), function(x) 
#'     list(id = x$value[1], name = x$value[2], tooltip = list(show=FALSE) )),
#'   edges = lapply(ec.data(edges,TRUE), function(x) 
#'     list(source = as.character(x$value[1]), 
#'          target = as.character(x$value[2]), 
#'          value = x$value[3], lineStyle = list(width=x$value[3])) ), 
#'   emphasis = list(focus='adjacency',
#'                   label=list( position='right', show=TRUE)),
#'   label = list(show=TRUE), roam = TRUE, zoom = 4, 
#'   tooltip=list(textStyle=list(color='blue'), formatter='{c} letters'),
#'   lineStyle = list(curveness=0.3)
#' )
#' p$x$opts$tooltip <- list(trigger='item')
#' p
#'
#'    
#' #--- Shiny interactive chart
#' library(shiny)
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
#'       e <- mtcars %>% group_by(cyl) %>% ec.init()
#'       e$x$opts$tooltip <- list(list(show=TRUE))
#'       e$x$opts$series[[1]]$emphasis <- list(focus='series', blurScope='coordinateSystem')
#'       e
#'     })
#' 
#'     observeEvent(input$addm, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts$series[[1]] = list(
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
#'       e %>% ecs.exec() #' ='p_merge'
#'     })
#'     observeEvent(input$adds, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts$series[[1]] <- list(
#'         type = 'line', name = 'newLine',
#'         encode = list(x='mpg', y='disp')
#'       )
#'       e %>% ecs.exec('p_update')
#'     })
#'     observeEvent(input$dels, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts$seriesName <- 'newLine'
#'       #'e$x$opts$seriesIndex <- 4  #' alternative ok
#'       e %>% ecs.exec('p_del_serie')
#'     })
#'     observeEvent(input$delm, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts$seriesIndex <- 1
#'       e$x$opts$delMarks <- c('markArea','markLine')
#'       e %>% ecs.exec('p_del_marks')
#'     })
#'     observeEvent(input$hilit, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts <- list(type='highlight', seriesName='4')
#'       e %>% ecs.exec('p_dispatch')
#'     })
#'     observeEvent(input$dnplay, {
#'       e <- ecs.proxy('plot')
#'       e$x$opts <- list(type='downplay', seriesName='4')
#'       e %>% ecs.exec('p_dispatch')
#'     })
#'   }
#' ))
#'
#' }
#' 
#' @export 
ec.examples <- function(){
  cat('copy/paste code from ?ec.examples Help')
}

