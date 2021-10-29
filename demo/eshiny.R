#'-----------  Interactive charts with echarty and Shiny ------------
#' run with command demo(eshiny, package ='echarty')
#' 
tryCatch ({
  library(shiny)
  library(dplyr)
  library(echarty)

base_df <- data.frame(ValX = c("A", "B", "C"), ValY = 1:3)
boxplot_df <- data.frame(ValX = sample(LETTERS[1:3], size = 20, replace = TRUE), 
                         ValY = rnorm(20))
lifo <- list(r=c(), t=c(), s=c(), b=c(), v=c())   # LIFO queue for deletion by name
toggle <- FALSE
js <- "$(document).on('shiny:connected', function(){
    barplot_vert._componentsViews
    .find(c => c._features && c._features.brush) 
    ._features.brush.model.iconPaths.zoom.trigger('click');
  });"


ui <- fluidPage( 
  tags$head(
    tags$style(HTML(" .ital { font-style: italic; } .sml { font-size: smaller; }"))
    #,tags$script(js)
  ),
  titlePanel("Shiny + echarty"),
  fluidRow(ecs.output('plot')),
  fluidRow(
    column(4, span(strong('Marks')), actionButton('addm', 'Add'),
           actionButton('delm', 'Delete'),
           br(),span('mark points stay, area/line deletable', class='sml')
    ),
    column(3, span(strong('Serie')), actionButton('adds1', 'Add'),
            actionButton('dels1', 'Del'),
            actionButton("modserie", label = "Modify last")),
    column(5, actionButton('adata', 'Add data'),
           actionButton('hilit', 'Highlight'),
           actionButton('dnplay', 'Downplay') )
  ),
  
  fluidRow( hr(),
      column(2,
         actionButton("add_radar", label = "Add radar"),
         br(),actionButton("add_pie", label = "Add pie"),
         br(),actionButton("add_funnel", label = "Add funnel"),
         br(),actionButton("add_boxplot", label = "Add boxplot"),
         p(HTML('&nbsp;')),
         actionButton("del_radar", label = "Del radar"),
         br(),actionButton("del_pie", label = "Del pie"),
         br(),actionButton("del_funnel", label = "Del funnel"),
         br(),actionButton("del_boxplot", label = "Del boxplot"),
         p(HTML('&nbsp;')),
         tags$div(style="display:inline-block",title="Replace a chart",
                  actionButton("replace.btn", label = "Replace"))
      ),
      column(2, ecs.output("radar")),
      column(2, ecs.output("pie")),
      column(2, ecs.output("funnel")),
      column(4, ecs.output("boxplot", height='350px'),
             span('Zoom range:'), strong(textOutput('tzoom', inline=TRUE))
      )
  ),
  fluidRow( hr(),
      column(2,  
         actionButton("addserie", label = "Add serie"),
         actionButton("delserie", label = "Del serie"), 
         br(), #div('Selection events'), #p(HTML('&nbsp;'))
         br(),
         span('Mouse events'),span('(vertical bars only)', class='sml'),
         br(), strong(textOutput('vmouse')),
         br(), 
         div('Vert.brush:', class='ital'), 
         strong(textOutput('vbrush')),
         br(),
         div('Horiz.brush:', class='ital'), 
         strong(textOutput('hbrush'))
      ),
      column(5, ecs.output("barplot_vert")),
      column(5, ecs.output("barplot_horiz"))
  )
)

server <- function(input, output, session) {
  
  init.boxplot <- function(p) {
    sname <- 'boxplo1'
    bgrp <- boxplot_df %>% group_by(ValX) %>% group_split()
    dats <- lapply(bgrp, function(x) boxplot.stats(x$ValY)$stats)
    # for zoom display
    session$userData$rng <- data.frame(sn=sname, limits=range(dats))
    p$x$opts$xAxis = list(ey='')
    p$x$opts$yAxis <- list(
      type = 'category', data = unique(unlist(lapply(bgrp, `[`, , 1))) )
    p$x$opts$series <- list(list(
      type = 'boxplot', data = dats, clip=FALSE, name=sname
    ))
    p$x$opts$tooltip <- list(trigger='axis',confine=TRUE)
    p$x$opts$dataZoom <- list(list(type = 'slider',
                                   startValue = min(boxplot_df$ValY), 
                                   endValue = max(boxplot_df$ValY) ))
    p$x$capture <- "datazoom"
    p
  }
  rpie <- function() { 
    tmp <- data.frame(name = sample(letters, size=4, replace = FALSE),
                      value = runif(n=4, min=1, max=15))
    ec.data(tmp,'names')
  }
  adds1 <- 0   # prevent add series to get out of hand
  output$tzoom <- renderText('^^ move slider ^^')
  
  output$plot <- ecs.render({
    p <- ec.init()
    p$x$opts$series <- lapply(
      mtcars %>% relocate(disp, .after=mpg) %>% group_by(cyl) %>% group_split(),
      function(s) { list(type='scatter', name=unique(s$cyl), 
                         data=ec.data(s, 'values')) })
    p$x$opts$legend <- list(ey='')
    p$x$opts$xAxis <- list(type="value");
    p$x$opts$yAxis <- list(ec='')
    p$x$opts$tooltip <- list(list(show=TRUE))
    p$x$opts$series[[1]]$emphasis <- list(
      focus='series', blurScope='coordinateSystem')
    p
  })
  
  observeEvent(input$addm, {
    p <- ecs.proxy('plot')
    p$x$opts$series = list( list(
      markPoint = list(data = list(
        list(coord = c(22.5, 140.8)),
        list(coord = c(30.5, 95.1))
      ),
      itemStyle = list(color='lightblue')
      )
      ,markArea = list(data = list(list(
        list(xAxis = 15),
        list(xAxis = 25)
      ))
      ,silent=TRUE
      ,itemStyle = list(color='pink', opacity=0.2)
      ,label = list(formatter='X-area', position='insideTop')
      )
      ,markLine = list(data = list(list(type='average')))
    ), list(
      markPoint = list(data = list(
        list(coord = c(25.5, 143.8)),
        list(coord = c(33.5, 98.1))
      ),
      itemStyle = list(color='forestgreen')
      )
    ))
    p %>% ecs.exec() #' ='p_merge'
  })
  
  observeEvent(input$adds1, {
    if (adds1>4) return()
    adds1 <<- adds1+1
    p <- ecs.proxy('plot')
    p$x$opts$series <- list(list(
      type = 'line', name = paste0('line',adds1),
      #encode = list(x='mpg', y='disp')  # for dataset only
      data = lapply(1:6, function(i) 
        lapply(unname(data.frame(x=sample(5:15,6),
                                 y=sample(100:400,6))), "[[", i))
    ))
    p %>% ecs.exec('p_update')
  })
  
  observeEvent(input$dels1, {
    p <- ecs.proxy('plot')
    p$x$opts$seriesName <- paste0('line',adds1)
    #p$x$opts$seriesIndex <- max(3, 2 + adds1)  # ok too (JS counts from 0)
    p %>% ecs.exec('p_del_serie')
    if (adds1>0) adds1 <<- adds1 - 1
  })
  
  observeEvent(input$modserie, {
    if (adds1==0) return()
    sname <- paste0('line',adds1)
    p <- ecs.proxy('plot')
    p$x$opts$series <- list(name=sname, symbolSize=15, lineStyle=list(width=4, type='dotted'))
    p %>% ecs.exec('p_merge')
  })
  
  observeEvent(input$adata, {
    tmp <- apply(unname(data.frame(rnorm(5, 10, 3), rnorm(5, 200, 33))),
                 1, function(x) { list(value=x) })
    p <- ecs.proxy('plot')
    p$x$opts$seriesName <- '6'
    # p$x$opts$seriesIndex <- 1   # same, works
    p$x$opts$data <- tmp
    p %>% ecs.exec('p_append_data')
  })
  observeEvent(input$delm, {
    p <- ecs.proxy('plot')
    p$x$opts$seriesIndex <- 1
    p$x$opts$delMarks <- c('markArea','markLine')
    p %>% ecs.exec('p_del_marks')
  })
  observeEvent(input$hilit, {
    p <- ecs.proxy('plot')
    p$x$opts <- list(type='highlight', seriesName='4')
    p %>% ecs.exec('p_dispatch')
  })
  observeEvent(input$dnplay, {
    p <- ecs.proxy('plot')
    p$x$opts <- list(type='downplay', seriesName='4')
    p %>% ecs.exec('p_dispatch')
  })
  
  # ------------- pie,funnel,radar ----
  output$radar <- ecs.render({
    p <- ec.init()
    p$x$opts <- list(
      radar = list(indicator=lapply(base_df$ValX, function(x) list(name=x)), 
                   center=list('50%','20%')),
      series = list(list(type='radar', data=list(base_df$ValY) ))
    )
    p
  })
  
  output$pie <- ecs.render({
    serie_name <- 'sun1st'
    lifo$s <<- c(lifo$s, serie_name)
    p <- ec.init()
    p$x$opts <- list(
      series = list(list(
        type='pie', name=serie_name, 
        center=list('50%','15%'),
        data = rpie(), 
        radius='50%', label=list(position='inside')))
    )
    p
  })
  output$funnel <- ecs.render({
    lifo$t <<- c(lifo$t, 'fst')
    p <- ec.init(preset=FALSE)
    p$x$opts$series = list( list(
      type='funnel', name='fst', 
      data=rpie(), height='20%', top=1)
    )
    p
  })
  
  output$boxplot <- ecs.render({
    p <- ec.init()
    init.boxplot(p)
  })
  observeEvent(input$boxplot_datazoom, {
    r <- range(session$userData$rng$limits)
    tmp <- input$boxplot_datazoom
    zdif <- r[2]-r[1]
    minx <- round(r[1] + (zdif * tmp$start / 100) ,2)
    maxx <- round(r[1] + (zdif * tmp$end / 100) ,2)
    output$tzoom <- renderText(paste(minx, 'to', maxx))
  })
  
  
  observeEvent(input$add_radar,{
    if (length(lifo$r)>4) return()
    serie_name <- sample(LETTERS, size = 5) %>% paste0(collapse = "")
    radar_serie <- base_df %>% mutate(value = runif(n = 3, min = 0, max = 5))
    p <- ecs.proxy("radar")
    p$x$opts$series = list( list(
      type = 'radar', name=serie_name,
      radar = list(indicator=ec.data(radar_serie, 'names')),
      data = list(radar_serie$value) )
    ) 
    p %>% ecs.exec('p_update')
    lifo$r <<- c(lifo$r, serie_name)
  })
  
  observeEvent(input$add_pie,{
    if (length(lifo$s)>4) return()
    serie_name <- sample(LETTERS, size = 5) %>% paste0(collapse = "")
    pos <- length(lifo$s) * 15 + 15
    p <- ecs.proxy("pie")
    p$x$opts$series = list(
      list(type='pie', name=serie_name, 
           center=list('50%', paste0(pos,'%')),
           data=rpie(), radius='50%', 
           label=list(position='inside'))
    )
    p %>% ecs.exec('p_update')
    lifo$s <<- c(lifo$s, serie_name)
  })
  
  observeEvent(input$add_funnel,{
    if (length(lifo$t)>4) return()
    serie_name <- sample(LETTERS, size=5) %>% paste0(collapse = "")
    pos <- length(lifo$t) * 80
    p <- ecs.proxy("funnel")
    p$x$opts$series = list( 
      list(type='funnel', name=serie_name, 
           data=rpie(), height='20%', top=pos)
    )
    p %>% ecs.exec('p_update')
    lifo$t <<- c(lifo$t, serie_name)
  })
  
  observeEvent(input$add_boxplot,{
    if (length(lifo$b)>4) return()
    serie_name <- sample(LETTERS, size = 5) %>% paste0(collapse = "")
    boxplot_df <<- boxplot_df %>% mutate(ValY = rnorm(20))
    # group by ABC then calc boxplot.stats for each
    dats <- lapply(boxplot_df %>% group_by(ValX) %>% group_split(), 
                   function(x) boxplot.stats(x$ValY)$stats)
    # save data range for zoom display
    session$userData$rng <- rbind(session$userData$rng, 
                                  data.frame(sn=serie_name, limits=range(dats)))
    p <- ecs.proxy("boxplot")
    p$x$opts$series = list( list(
      type = 'boxplot', name=serie_name,
      data = dats
    ))
    p %>% ecs.exec('p_update')
    lifo$b <<- c(lifo$b, serie_name)
  })
  
  observeEvent(input$del_radar,{
    if (length(lifo$r)==0) return()
    p <- ecs.proxy("radar")
    p$x$opts$seriesName <- lifo$r[length(lifo$r)]
    # p$x$opts$seriesIndex <- length(lifo$r)  # ok too
    p %>% ecs.exec('p_del_serie')
    lifo$r <<- lifo$r[-length(lifo$r)]
  })
  observeEvent(input$del_pie,{
    if (length(lifo$s)==0) return()
    p <- ecs.proxy("pie")
    p$x$opts$seriesName <- lifo$s[length(lifo$s)]
    p %>% ecs.exec('p_del_serie')
    lifo$s <<- lifo$s[-length(lifo$s)]
  })
  observeEvent(input$del_funnel,{
    if (length(lifo$t)==0) return()
    p <- ecs.proxy("funnel")
    p$x$opts$seriesName <- lifo$t[length(lifo$t)]
    p %>% ecs.exec('p_del_serie')
    lifo$t <<- lifo$t[-length(lifo$t)]
  })
  observeEvent(input$del_boxplot,{
    if (length(lifo$b)==0) return()
    p <- ecs.proxy("boxplot")
    p$x$opts$seriesName <- lifo$b[length(lifo$b)]
    p %>% ecs.exec('p_del_serie')
    lifo$b <<- lifo$b[-length(lifo$b)]
    # for zoom display
    session$userData$rng <- 
      session$userData$rng[which(session$userData$rng$sn != p$x$opts$seriesName),]
  })
  
  observeEvent(input$replace.btn, {
    p <- ecs.proxy("boxplot")
    if (toggle) {
      p <- init.boxplot(p)
    } else {
      p$x$opts$xAxis = list(data = boxplot_df$ValX)
      p$x$opts$yAxis = list(ey='')
      p$x$opts$series <- list(list(
        type = 'bar', data = boxplot_df$ValY
      ))
    }
    p %>% ecs.exec('p_replace')
    toggle <<- !toggle
    output$tzoom <- renderText('')
  })
  
  # ------------- bar series ------
  output$barplot_vert <- ecs.render({
    p <- ec.init()
    p$x$opts <- list(
      legend = list(ey=''),
      yAxis = list(ey=''),
      xAxis = list(data=base_df$ValX),
      series = list(list(type='bar', data=base_df$ValY, name='v1st')),
      toolbox = list( feature=list(brush=list(type=list("lineX", "clear"))))
      ,brush = list(brushLink='all', throttleType='debounce')
    )
    p$x$on <- list(list(event='mouseover', 
                        handler=htmlwidgets::JS("function (event) {
          document.getElementById('vmouse').innerHTML = event.seriesName+'_'+event.name; }") ),
                   list(event='mouseout',
                        handler=htmlwidgets::JS("function (event) {
          document.getElementById('vmouse').innerHTML = ''; }") )
    )
    p
  })
  
  output$barplot_horiz <- ecs.render({
    p <- ec.init()
    p$x$opts <- list(
      legend = list(ey=''),
      xAxis = list(ey=''),
      yAxis = list(data=base_df$ValX),
      series = list(list(type='bar', data=base_df$ValY, name='h1st')),
      toolbox = list(feature=list(brush=list(type=list("lineY", "clear"))))
      ,brush = list(brushLink='all', throttleType='debounce')  # group selection
      #,brush = list(ey='')   # one-by-one selection
    )
    p
  })
  
  observeEvent(input$addserie, {
    new_serie <- base_df %>%
      mutate(ValY = runif(n = 3, min = 0, max = 5))
    rndname <- sample(LETTERS, size = 5) %>% paste0(collapse = "")
    lifo$v <<- c(lifo$v, rndname)
    
    p <- ecs.proxy("barplot_vert")
    p$x$opts <- list(
      series = list(list(type='bar', name=rndname, data = new_serie$ValY ))
    )
    p %>% ecs.exec('p_update')
    
    p <- ecs.proxy("barplot_horiz")
    p$x$opts <- list(
      series = list(list(type='bar', name=rndname, data = new_serie$ValY ))
    )
    p %>% ecs.exec('p_update')
    output$vbrush <- renderText('')
    output$hbrush <- renderText('')
  })
  
  observeEvent(input$delserie, {
    if (length(lifo$v)==0) return()
    p <- ecs.proxy("barplot_vert")
    p$x$opts$seriesName <- lifo$v[length(lifo$v)]
    p %>% ecs.exec('p_del_serie')
    
    p <- ecs.proxy("barplot_horiz")
    p$x$opts$seriesName <- lifo$v[length(lifo$v)]
    p %>% ecs.exec('p_del_serie')
    lifo$v <<- lifo$v[-length(lifo$v)]
    output$vbrush <- renderText('')
    output$hbrush <- renderText('')
  })
  
  brushcap <- function(sele) {
    out <- lapply(sele, function(x) {
     if (length(x$dataIndex[[1]])==0) return(NA)  # toolbar clicked or selection cleared
     ifx <- paste(x$dataIndex[[1]]+1, collapse=',')
     bars <- eval(parse(text = paste('base_df$ValX[c(',ifx,')]') ))
     paste0(x$seriesName,': ', paste(bars, collapse='+') )
   })
    unlist(out)
  }
  observeEvent(input$barplot_vert_brush, {
    tmp <- brushcap(input$barplot_vert_brush$batch$selected)
    output$vbrush <- renderText(na.omit(tmp))
  })
  observeEvent(input$barplot_horiz_brush, {
    tmp <- brushcap(input$barplot_horiz_brush$batch$selected)
    output$hbrush <- renderText(na.omit(tmp))
  })
}

shinyApp(ui = ui, server = server)

}, 
error  = function(e) cat(e$message)
#warning= function(w) cat(w$message)
)
  


