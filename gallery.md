## Gallery

 Some interesting charts along with their **code**. Several have Live Demos hosted on [RPubs](https://rpubs.com/echarty). The *echarty* package has two dozen more examples - in RStudio type *?ec.examples* to see them in panel Help.


### Simple bar  
demo for presets  
<img src='img/cb.bar.png' alt='bar' />
<details><summary>🔻View code</summary>

```r
library(echarty); library(dplyr)
library(lubridate)
df <- data.frame(date= as.character(as.Date('2019-12-31') %m+% months(1:13)), 
                 num= runif(13))

#  with presets and df chained
df |> ec.init(ctype= 'bar') |> ec.theme('dark')

#  without presets all options are explicitly assigned
ec.init(preset= FALSE,
  yAxis= list(show= TRUE),
  xAxis= list(type= 'category', 
               axisLabel= list(interval= 0, rotate= 45)
               #, axisTick= list(alignWithLabel= TRUE)
          ),
  series= list(list(
    type= 'bar', data= ec.data(df, 'values')))
) |> ec.theme('dark')


```
</details>
<br />  

### Data models 
how to store data in echarty - 
[<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/data-models) with code  
<a href='https://rpubs.com/echarty/data-models' target=_blank> <img src='img/cb-datam.png' alt='data models' /></a>
<br><br>

### Horizontal bars
<img src='img/cb-33.png' alt='vertBars' />
<details><summary>🔻 View code</summary>

```r
library(echarty); library(dplyr)
df <- Orange |> mutate(Tree= as.character(Tree)) |>
	arrange(Tree) |> group_by(Tree) |> group_split()

ec.init(preset= FALSE,
	series= lapply(df, function(t) {
		list(type= 'bar', name= unique(t$Tree), data= t$circumference) }),
	legend= list(show=TRUE),
	xAxis= list(name= 'tree circumference in mm', nameLocation= 'center', nameGap= 22),
	yAxis= list(data= unique(Orange$age), name= 'age in days'),
	tooltip= list(formatter= 'circumference={c} mm')
) |> ec.upd({
	l <- length(series)
	series[[l]]$name <- paste(series[[l]]$name, ' trees')
}) |> ec.theme('dark')

```
</details>
<br />

### Easy as pie
<img src='img/cb-0.png' alt='pie' />
<details><summary>🔻 View code</summary>

```r
isl <- data.frame(name= names(islands), value= islands) |> filter(value>60) |> arrange(value)

library(echarty)
ec.init(preset= FALSE,
	title= list(text= "Landmasses over 60,000 mi\u00B2", left= 'center'),
	tooltip= list(show= TRUE),
	series= list(list(type= 'pie', data= ec.data(isl, 'names'))),
	backgroundColor= '#191919'
)
```
</details>
<br />

### Parallel chart
<img src='img/cb-parallel.png' alt='parallel' />
<details><summary>🔻 View code</summary>

```r
library(echarty)
iris |> group_by(Species) |> 
	ec.init(ctype='parallel', color= rainbow(10)) |> 
	ec.upd({   # update preset series
		series <- lapply(series, function(s) { 
			s$smooth=TRUE; s$lineStyle=list(width=3); s })  
	}) |> ec.theme('dark-mushroom')

```
</details>
<br />

### Custom chart
<img src='img/cb-1.png' alt='profit' />
<details><summary>🔻 View code</summary>

```r
# source https://echarts.apache.org/examples/en/editor.html?c=custom-profit
# GUI translated with demo(js2r) with rdata and ritem added

library(echarty); library(dplyr)
data <- list(c(10, 16, 3, "A"), c(16, 18, 15, "B"), c(18, 26, 12, "C"), c(26, 32, 22, "D"), c(32, 56, 7, "E"), c(56, 62, 17, "F"))
colorList <- c("#4f81bd", "#c0504d", "#9bbb59", "#604a7b", "#948a54", "#e46c0b")
rdata <- 1:6 %>% purrr::map(function(x) {
    list(value= data[[x]],
          itemStyle= list(color=colorList[x])) })
ritem <- "function renderItem(params, api) {
    var yValue= api.value(2);
    var start= api.coord([api.value(0), yValue]);
    var size= api.size([api.value(1) - api.value(0), yValue]);
    var style= api.style();

    return {
        type: 'rect',
        shape: {
            x: start[0],
            y: start[1],
            width: size[0],
            height: size[1]
        },
        style: style
    };
}"
ec.init(
	title= list(text= "Profit", left= "center"),
	tooltip= list(zz= ""),
	xAxis= list(scale= TRUE), yAxis= list(zz= ""),
	series= list(list(type= "custom",
		renderItem= htmlwidgets::JS(ritem),
		label= list(show= TRUE, position= "top"),
		dimensions= list("from", "to", "profit"),
		encode= list(x= list(0, 1), y= 2,
						 tooltip= list(0, 1, 2), itemName= 3),
		data= rdata ))
) |> ec.theme('dark-mushroom')      # only 2 commands used

```
</details>
<br />

### Error Bars
<img src='img/cb-2.png' alt='profit' />
<details><summary>🔻 View code</summary>

```r
# example by https://github.com/kuzmenkov111
library(data.table)
library(binom); library(dplyr)
# function for percent and CI calculation
myfun_binom <- function(n,all){
	round((binom::binom.confint(n, all, methods= "wilson", conf.level=0.95)[,c(4:6)])*100,2)
}
#  --- 1. data prep
stackbar <- data.table(Year= c(2010, 2010, 2010, 2011, 2011, 2011, 2012, 2012, 2012, 2013,2013, 2013),
							         Category= c("A", "B", "C", "A", "B", "C", "A", "B", "C", "A", "B", "C"),
       							   n= c(10, 20, 30, 30, 20, 10, 11,12,13, 15, 15, 15))
# calculate percent and 95% CI
stackbar <- stackbar[,`:=`(all=sum(n)), by= c("Year")][,c("perc","low","up") := myfun_binom(n,all)]
stackbar <- stackbar |> mutate(xlbl= paste0(Year,' (N=',all,')')) |>
	relocate(xlbl,perc) |>  # move in front as default X & Y columns
	group_by(Category)      # both ec.init & ecr.ebars need grouped data
#  --- 2. plot
q <- stackbar |> ec.init(ctype='bar', load='custom') |>
	ec.theme('dark-mushroom') |>
	# only columns for x,y,low,high,category
	ecr.ebars(stackbar[,c('xlbl','perc','low','up','Category')],
				    hwidth= 4)    # (optional) half-width of err.bar in pixels
#  --- 3. customization
groupColors <- c("#387e78","#eeb422","#d9534f")
q$x$opts$series <- lapply(q$x$opts$series, function(s, i) {
	if (s$type=='bar') {
		s$emphasis <- list(focus= 'series')
		s$color <- groupColors[parent.frame()$i[]]  # iteration hack, for fun only
	}
	else if (s$type=='custom')
		s$itemStyle$color <- 'cyan'
	s
})
q   # display customized
```
</details>
<br />

### Triple gauge with animation
<img src='img/cb-5.png' alt='gauge3' />
<details><summary>🔻 View code</summary>

```r
jcode <- "setInterval(function () {
    opts.series[0].data[0].value= (Math.random() * 100).toFixed(2) - 0;
    opts.series[0].data[1].value= (Math.random() * 100).toFixed(2) - 0;
    opts.series[0].data[2].value= (Math.random() * 100).toFixed(2) - 0;
    chart.setOption(opts, true);
}, 2000);"

library(echarty)
p <- ec.init(js= jcode) |> ec.theme('dark')
p$x$opts <- list(series= list(
    list(type= "gauge", 
    anchor= list(show= TRUE, showAbove= TRUE,
    size= 18, itemStyle= list(color= "#FAC858")), 
    pointer= list(icon= "path://M2.9,0.7L2.9,0.7c1.4,0,2.6,1.2,2.6,2.6v115c0,1.4-1.2,2.6-2.6,2.6l0,0c-1.4,0-2.6-1.2-2.6-2.6V3.3C0.3,1.9,1.4,0.7,2.9,0.7z",
    width= 8, length= "80%", offsetCenter= list(0, "8%")), 
    progress= list(show= TRUE,
      overlap= TRUE, roundCap= TRUE), axisLine= list(roundCap= TRUE), 
    data= list(
      list(value= 20, name= "One", title= list(offsetCenter= list("-40%", "80%")), detail= list(offsetCenter= list("-40%","95%"))), 
      list(value= 40, name= "Two", title= list(offsetCenter= list("0%", "80%")), detail= list(offsetCenter= list("0%", "95%"))), 
      list(value= 60, name= "Three", title= list(offsetCenter= list("40%", "80%")), detail= list(offsetCenter= list("40%","95%")))), 
    title= list(fontSize= 14), detail= list(width= 40, height= 14, fontSize= 14, color= "#fff", backgroundColor= "auto", borderRadius= 3, formatter= "{value}%"))))
p
```
</details>
<br />
<a id='3D'></a>

### Crosstalk in 3D

<img src='img/cb-3.png' alt='crosstalk 3D' />
<details><summary>🔻 View code</summary>

```r
# echarty can highlight 3D points selected by external controls
library(crosstalk); library(DT); library(d3scatter); 
library(htmltools); library(dplyr); library(tibble)
sdf <- mtcars |> rownames_to_column(var='name') |> relocate(mpg,wt,hp) 
sdf <- SharedData$new(sdf)

library(echarty)
p3 <- sdf |> ec.init(load='3D', 
            title= list(text="3D brush listener")) |>
            ec.theme('dark-mushroom')
p3$x$opts$series[[1]] <- list(
  type='scatter3D', symbolSize=11,
  itemStyle=list(color= htmlwidgets::JS("function(params){
    let cyl=params.value[4]; return (cyl==4 ? 'RoyalBlue' : cyl==6 ? 'OrangeRed':'green');}") ),
  emphasis= list(focus='self', blurScope='series', itemStyle=list(color='red'))
)

bscols( list(
    d3scatter(sdf, ~mpg, ~wt, ~factor(cyl), width="100%", height=300),br(),
    datatable(sdf, extensions="Scroller", style="bootstrap", class="compact", width="100%",
              options=list(deferRender=TRUE, scrollY=300, scroller=TRUE))
  ),  p3
)
```
</details>
<br />

### Crosstalk 2D
play with the [<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/crosstalk) with code 
<br />

### Crosstalk with leaflet
two-way selection between map and chart  
<img src='img/xtalk.png' alt='crosstalk' />
<details><summary>🔻 View code</summary>

```r
library(crosstalk)
sdf <- quakes[1:33,] |> SharedData$new(group= 'qk')

library(leaflet)
map <- leaflet(sdf) |> addTiles() |> addMarkers()

library(echarty)
p <- sdf |> ec.init(
	title= list(text= 'Crosstalk two-way selection'),
	toolbox= list(feature= list(brush= list(show=TRUE))),
	brush= list(brushLink='all', throttleType='debounce', 
					brushStyle= list(borderColor= 'red')),
	tooltip= list(show=TRUE),
	xAxis= list(scale=TRUE, boundaryGap= c('5%', '5%'))
) |> 
ec.upd({
		series[[1]] <- append(series[[1]], list(
			encode= list(x='mag', y='depth', tooltip=list(2,3)),
			selectedMode= 'multiple',
			emphasis= list(
				itemStyle= list(borderColor='yellow', borderWidth=2),
				focus= 'self', 
				blurScope='series'
			),
			blur= list(itemStyle= list(opacity = 0.4))  # when focus set
		))
}) |> ec.theme('dark-mushroom')

library(htmltools)
browsable(tagList(
	div(style="float:left;width:50%;", map), 
	div(style="float:right;width:50%;",p) 
))
```
</details>
<br />

### scatterGL
plugin **3D**, test with 5,000 points  
<img src='img/cb-6.png' alt='scatterGL' />
<details><summary>🔻 View code</summary>

```r
# example works also with slower type='scatter', with ec.data(dat, format='values')
# ------ 1) prepare data
library(tibble)
dim <- 2500   # sample data half-quantity, could be much more
slip <- if (dim %% 2) 0.1 else -0.1
setData <- function(offset) {
  t <- tibble(x= runif(dim, max=10),
              y= offset + sin(x) - x * slip * runif(dim))
  round(t,3)
}	
# two sets, same data shifted vertically
dat <- rbind(setData(0), setData(1))

# ------ 2) show data
library(echarty)
ec.init(load= '3D', preset= FALSE, 
	title= list(text=paste('scatterGL -',nrow(dat),'points + zoom')),
	xAxis= list(show=TRUE),
	yAxis= list(show=TRUE),
	series= list(list(type= 'scatterGL', data= ec.data(dat),
					 symbolSize= 3, large=TRUE,
					 itemStyle= list(opacity=0.4, color='cyan')
	)),
	dataZoom= list(type='inside',start=50)
) |> ec.theme('dark-mushroom')

```
</details>
<br />


### scatter3D
plugin **3D**, test with 36,000 points  
<img src='img/cb-7.png' alt='bunny' />
<details><summary>🔻 View code</summary>

```r
library(onion); library(echarty)
data(bunny)
tmp <- as.data.frame(bunny)
tmp |> ec.init(load= '3D', 
					visualMap= list(
	inRange= list(color= rainbow(10)), calculable= TRUE,
	min= min(tmp$y), max= max(tmp$y), dimension= 1)) |> 
ec.upd({ 
  series[[1]] <- list(type='scatter3D', symbolSize=2) }) |>
ec.theme('dark-mushroom')
```
</details>
<br />


### Bathymetry in 3D
up to 200,000 surface points. Good performance test for CPU/GPU.  
<img src='img/hawaii3d.png' alt='bathy' />
<details><summary>🔻 Shiny app - <span style="color:magenta">Live Demo</span></summary>

Multiple 3D examples based on ocean floor measurements in different locations across the planet.  
The app requires _shiny_ and several other libraries with their dependencies - [source code](https://gist.github.com/helgasoft/121d7d3ff7d292990c3e05cfc1cbf24b).  
Run the demo with command:
```r
shiny::runGist('https://gist.github.com/helgasoft/121d7d3ff7d292990c3e05cfc1cbf24b')
```
</details>
<br />
<a id='3Dbubbles'></a>

### Timeline in 3D
demographic data evolution in the last 200 years
<img src='img/cb-bubble.3D.png' alt='bubbles' />
<details><summary>🔻 View code</summary>

```r
# see also original 2D: https://helgasoft.github.io/echarty/uc5.html

library(dplyr)
# data download and preparation
tmp <- jsonlite::fromJSON('https://echarts.apache.org/examples/data/asset/data/life-expectancy.json')
tmp$series[,,2] <- round(as.numeric(tmp$series[,,2]), 1)  # life exp rounded
tmp$series[,,3] <- round(as.numeric(tmp$series[,,3])/1000000, 2)  # pop in Millions

df <- as.data.frame(tmp$series[1,,])
for(i in 2:nrow(tmp$series)) {
	df <- rbind(df, as.data.frame(tmp$series[i,,]))
}  # convert array to data.frame
colnames(df) <- c('Income','Life','Population','Country','Year')
tt <- df$Country;	df <- df[,-4]; df[] <- lapply(df, as.numeric); df$Country<-tt
df$SymSize <- (sqrt(df$Population / 5e2) + 0.1) *80
df <- df |> relocate(Year, .after= last_col())

# set colors for countries
colors <- rep(c('#8b0069','#75c165', '#ce5c5c', '#fbc357', '#8fbf8f', '#659d84',
					 '#fb8e6a', '#c77288', '#786090', '#91c4c5', '#6890ba'), 2)
i <- 0
pieces <- lapply(unique(df$Country), function(x) { 
  i <<- i+1;	list(value= x, color= colors[i]) 
})

# remotes::install_github("helgasoft/echarty")  # needs v.1.4.4+
library(echarty)
p <- df |> group_by(Year) |> ec.init(
	load= '3D',
	tl.series= list(
		type= 'scatter3D', coordinateSystem= 'cartesian3D',
		itemStyle= list(opacity= 0.8),
		encode= list(x= 'Income', y= 'Life', z= 'Year'),
		symbolSize= ec.clmn(5),  # 5 is SymSize
		tooltip= list( backgroundColor= 'transparent',
			formatter= ec.clmn('<b>%@</b><br>life exp: <b>%@</b><br>income: <b>$%@</b><br>populat: <b>%@M</b>',4,2,1,3)
		)
)) |> 
  ec.theme('dark-mushroom') |> ec.snip()

p$title= list(list(left=5, top='top', textStyle=list(fontSize=50, color='#11111166')),
					list(text= "Life expectancy and GDP by year", top= 10, 
						left= "center", textStyle= list(fontWeight= "normal", fontSize= 20)) )
p$timeline <- append(p$timeline, list(
		orient= "vertical", 
		autoPlay= TRUE, playInterval= 500, left= NULL, right= 0, top= 20, bottom= 20, 
		width= 55, height= NULL, symbol= "none", checkpointStyle= list(borderWidth= 2)
))
p$grid3D=  list(axisLabel= list(textStyle= list(color='#ddd')))
p$xAxis3D= list(name= 'Income', min= 15, axisLabel= list(formatter= "${value}"), 
					  nameTextStyle= list(color= '#ddd'), nameGap= 25)
p$yAxis3D= list(name= 'Life Expectancy', min= 15, 
					  nameTextStyle= list(color= '#ddd'))
p$zAxis3D= list(name= 'Year', min= 1790, max=2022, 
					  nameTextStyle= list(color= '#ddd'), nameGap= 25,
	# minInterval= 1 does not work in 3D, use formatter to show integers for Year
	axisLabel= list(formatter= htmlwidgets::JS("function(val) {if (val % 1 === 0) return val;}"))
)
p$visualMap= list(show= FALSE, dimension= 'Country', type= 'piecewise', pieces= pieces)
p$tooltip <- list(show= TRUE)
ec.snip(p)

```
</details>
<br />


### Radar chart customized
<img src='img/cb-8.png' alt='radar1' />
<details><summary>🔻 View code</summary>

```r
data <- data.frame(
  name= c(3,5,7,8,9),
  values= c(12,45,23,50,32), max= rep(60, 5)
)
# build a list for rich formatting
rifo <- lapply(data$name, function(x) { 
  list(height= 30, backgroundColor=list(
      image=paste0('https://raw.githubusercontent.com/googlefonts/noto-emoji/main/png/32/emoji_u1f30',x,'.png')))
}) 
names(rifo) <- data$name

library(echarty)
data |> ec.init(preset= FALSE,
	radar= list(
		indicator= ec.data(data, 'names'),
		name= list( 
			formatter= htmlwidgets::JS("v => '{'+v+'| }'"),
			rich= rifo)
	),
	series= list(list(
		type= 'radar',	data= list(data$values)
	))
) |> ec.theme('dark-mushroom')
 
```
</details>
<br />

<a id='boxplot'></a>

### Simple or grouped boxplots
boxplot calculations in R or ECharts
<img src='img/cb-9.png' alt='boxplot' />
<details><summary>🔻 View code</summary>

```r
library(echarty); library(dplyr)

# 1) boxplot calculation in R ---------------------

ec.init(series= list(
	list(type='boxplot', name='mpg', data=list(boxplot.stats(mtcars$mpg)$stats)), 
	list(type='boxplot', name='hp',  data=list(boxplot.stats(mtcars$hp)$stats)), 
	list(type='boxplot', name='disp',data=list(boxplot.stats(mtcars$disp)$stats))
	),	
	xAxis= list(type= 'category'),
	legend= list(show=TRUE)
)

# 2) boxplot calculation in ECharts ---------------------
df <- mtcars[,c(1,3,4)] |> mutate(mpg= mpg*10)
ec.init(
	dataset= list(
		list(source= ec.data(data.frame(t(df)), header=FALSE)),
		list(transform= list(type='boxplot')),
		list(fromDatasetIndex=1, fromTransformResult= 1)),
	series= list(
		list(name= 'boxplot', type= 'boxplot', datasetIndex= 1),
		list(name= 'outlier', type= 'scatter', encode= list(x=1, y=0), datasetIndex= 2)
	),
	yAxis= list(type= 'category', boundaryGap=TRUE),
	legend= list(show=TRUE)
)

# 3) grouped boxplots ---------------------
# remotes::install_github("helgasoft/echarty")   # needs new v.1.4.4+

ds <- mtcars |> relocate(am,mpg) |> group_by(cyl) |> ec.data(format='boxplot')
# Below we mutate to create less Y-axis items with more, sufficient data. Otherwise ECharts exits with errors.
# ds <- airquality |> mutate(Day=round(Day/10)) |> relocate(Day,Wind) |> ec.data(format='boxplot')
ec.init(
	dataset= ds$dataset, 
	series= ds$series, 
	yAxis= list(type= 'category'), 
	xAxis= list(show= TRUE),
	legend= list(show= TRUE)
)

```
</details>
<br />

### Boxplot + scatter overlay
a horizontal chart with zoom and tooltips  
<img src='img/cb-pumpkin.png' alt='box+scatter' />
<details><summary>🔻 View code</summary>

Inspired by [Julia Silge's article](https://juliasilge.com/blog/giant-pumpkins/).
ECharts advantage over ggplot is interactivity - zoom brush and tooltips.  
A vertical layout version is also [published](https://gist.github.com/helgasoft/bd057b64e9b89cc133de2a4c407b53ad).

```r
#' inspired by https://juliasilge.com/blog/giant-pumpkins/
#' advantage over ggplot is interactivity - zoom brush and tooltips
#' TODO:  totals by country, add year in tooltip
library(tidyverse)
pumpkins_raw <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-10-19/pumpkins.csv")
pumpkins <-
	pumpkins_raw |>
	separate(id, into= c("year", "type")) |>
	mutate(across(c(year, weight_lbs), parse_number)) |> 
	filter(type == "P") |>
	select(country, weight_lbs, year) |> 
	mutate(country= fct_lump(country, n= 10))

# set columns to: country, 2013, 2014, etc. (country by years)
tmp <- tidyr::pivot_wider(pumpkins, names_from= year, values_from= weight_lbs )

# merge each country's data into one list
rng <- lapply(tmp$country, function(x) unname(unlist(tmp[tmp$country==x, c(-1)])) )
asc.ord <- order(unlist(lapply(rng, median)))   # sort order
rnames <- as.character(tmp$country[asc.ord])
tt <- rng[asc.ord]
yax <- paste(rnames, collapse="','")   # for Y axis labels
yax <- paste0("function (params) { return ['",yax,"'][params.value]; }")

library(echarty)
p <- ec.init() |> ec.theme('dark-mushroom') |> ec.snip()
p$title <- list(
	list(text="Giant Pumpkins", subtext='inspiration',
		sublink='https://juliasilge.com/blog/giant-pumpkins/') 
	,list(text=paste(nrow(pumpkins),'records for 2013-2021'), 
		textStyle= list(fontSize= 12), left= '50%', top= '90%' )
)
p$xAxis <- list(name='weigth (lbs)', min=0, 
			nameLocation='center', nameGap=20)
p$yAxis <- list(
	list(type= 'category'), 
	list(type= 'value', max=11, show=FALSE))
p$dataset <- list(
	list(source= tt),
	list(transform= list(type='boxplot',
			config=list(itemNameFormatter= htmlwidgets::JS(yax))
	)),
	list(fromDatasetIndex= 1, fromTransformResult= 1)
)
p$series <- list(      # use ECharts built-in boxplot
	list(name= 'boxplot', type= 'boxplot', datasetIndex= 1
		  ,color='LightGrey', itemStyle= list(color='DimGray'), 
		  boxWidth=c(13,50) )
)
i <- 0.5
sers <- lapply(p$dataset[[1]]$source, function(xx) {
	yy <- jitter(rep(i, length(xx)), amount=0.2); i <<- i + 1
	xx <- jitter(xx, amount=0.2)
	data <- list()
	for(j in 1:length(xx)) data <- append(data, list(list(xx[j], yy[j])))
	list(name='data', type= 'scatter', data=data, yAxisIndex=1, 
		  symbolSize=3, itemStyle=list(opacity=0.3), color=heat.colors(11)[i-0.5],
		  emphasis= list(itemStyle= list(color= 'chartreuse', borderWidth=4, opacity=1)) )
})
p$series <- append(p$series, sers)
p$legend <- list(show=TRUE)
p$tooltip <- list(show=TRUE, 
		backgroundColor= 'rgba(30,30,30,0.5)', 
		textStyle= list(color='#eee'),
		formatter=ec.clmn('%@ lbs', 1, scale=0))
p$toolbox <- list(left='right', feature=list(dataZoom=list(show=TRUE)))
ec.snip(p)

```

</details>
<br />

### Correlation matrix
using heatmap chart  
<img src='img/cb-correlation.png' alt='correlation' />
<details><summary>🔻 View code</summary>

```r
library(dplyr)
# prepare and calculate data
mtx <- cor(infert %>% dplyr::mutate(education=as.numeric(education)))
order <- corrplot::corrMatOrder(mtx)
mtx <- mtx[order, order]
df <- as.data.frame(as.table(mtx))
for(i in 1:2) df[,i] <- as.character(df[,i])

# ECharts heatmap expects dataset columns in a certain order: relocate
library(echarty)
df |> relocate(Var2) |> ec.init(ctype='heatmap',
	title= list(text='Infertility after abortion correlation'),
	xAxis= list(axisLabel= list(rotate=45)),
	visualMap= list(min=-1, max=1, orient='vertical',left='right',
						 calculable=TRUE, inRange=list( color=heat.colors(11)) )
) |> ec.theme('dark')
```

</details>
<br />

### Histogram
using bar chart  
<img src='img/cb-histogram.png' alt='histogram' />
<details><summary>🔻 View code</summary>

```r
library(echarty); library(dplyr)
do.histogram <- function(x, breaks='Sturges') {
  # get histogram data from input 'x'
  histo <- hist(x, plot=FALSE, breaks)
  tmp <- data.frame(x=histo$mids, y=histo$counts)
  tmp
}
do.histogram(rnorm(44)) |> ec.init(ctype='bar') |> ec.theme('dark')

# with normal distribution line added
hh <- do.histogram(rnorm(44))
nrm <- dnorm(hh$x, mean=mean(hh$x), sd=sd(hh$x))  # normal distribution
hh |> ec.init(ctype= 'bar',
				  xAxis= list(list(show=TRUE), list(data=c(1:length(nrm)))),
				  yAxis= list(list(show=TRUE), list(show=TRUE))
) |> ec.upd({
	series <- append(series, 
		list(list(type='line', data=nrm, xAxisIndex=1, yAxisIndex=1, color='yellow')))
}) |> ec.theme('dark')

# same with timeline
hh <- data.frame()
for(i in 1:5) {
  tmp <- do.histogram(rnorm(44)) |> mutate(time=rep(i,n()))
  hh <- rbind(hh, tmp)
}
hh |> group_by(time) |> 
	ec.init(tl.series= list(type='bar', encode= list(x='x',y='y'))) |> 
	ec.theme('dark')
```

</details>
<br />


### Modularity plugin
DOW companies - size by market cap<br />
<img src='img/cb-10.png' alt='dow' />
<details><summary>🔻 View code</summary>

```r
# click and drag items to see auto-rearrange effect
library(dplyr)
tmp <- jsonlite::fromJSON('https://quote.cnbc.com/quote-html-webservice/quote.htm?noform=1&partnerId=2&fund=1&exthrs=0&output=json&symbolType=issue&symbols=55991|44503|36276|56858|70258|1607179|84090|142105|145043|148633|151846|167459|174239|178782|174614|197606|202757|205141|205778|212856|228324|260531|277095|81364|283359|10808544|283581|286571|89999|522511530&requestMethod=extended')
df <- tmp$ExtendedQuoteResult$ExtendedQuote$QuickQuote
wt <- data.frame(tic=df$symbol, name=df$altName, bn=NA, size=NA, 
      mcap= df$FundamentalData$mktcapView, 
      rev= df$FundamentalData$revenuettm)
wt$bn <- round(as.numeric(gsub('M','',wt$mcap, fixed=TRUE))/1000,1) # mkt.cap
bnMax <- max(wt$bn)
wt$size <- 30 + wt$bn/bnMax * 140   # size 30 to 140 px depending on mkt.cap
  
library(echarty)
ec.init(load='gmodular', preset=FALSE,
	title=list(text='DOW 2021',x='center',y='bottom',
				  backgroundColor='rgba(0,0,0,0)',borderColor='#ccc',    
				  borderWidth=0,padding=5,itemGap=10, 
				  textStyle=list(fontSize=18,fontWeight='bolder',color='#eee'),subtextStyle=list(color='#aaa')),
	backgroundColor= '#000',
	animationDurationUpdate= "function(idx) list(return idx * 100; )",
	animationEasingUpdate= 'bounceIn',
	series= list(list(
		type='graph', layout='force', 
		force=list(repulsion=250,edgeLength=10),
		modularity= list(resolution=7, sort=TRUE),
		roam=TRUE, label=list(show=TRUE),
		data= lapply(ec.data(wt, 'names'), function(x)
			list(name= x$tic, lname=x$name, value=x$bn, 
				  symbolSize=x$size, draggable=TRUE 
			)) )),
	tooltip= list(formatter= ec.clmn('<b>%@</b><br>%@ bn','lname','value'))
)
```

</details>
<br />


### Graph
Circular layout diagram for 'Les Miserables' characters<br />
<img src='img/cb-graph.png' alt='dow' />
<details><summary>🔻 View code</summary>

```r
# https://echarts.apache.org/examples/en/editor.html?c=graph-circular-layout
library(echarty); library(dplyr)
les <- jsonlite::fromJSON('https://echarts.apache.org/examples/data/asset/data/les-miserables.json')
les$categories$name <- as.character(1:9)
ec.init(preset=FALSE, 
		  title=list(text='Les Miserables',top='bottom',left='right'),
		  series= list(list(
				 	type='graph', layout='circular',
				 	circular= list(rotateLabel=TRUE),
				 	nodes= ec.data(les$nodes, 'names'), 
				 	links= ec.data(les$links, 'names'), 
				 	categories= ec.data(les$categories, 'names'),
				 	roam= TRUE, label=list(position='right', formatter='{b}'),
				 	lineStyle= list(color='source', curveness=0.3)
		  )),
		  legend= list(data=c(les$categories$name), textStyle=list(color='#ccc')),
		  tooltip= list(show=TRUE),
		  backgroundColor= '#191919'
) |> ec.upd({    # labels only for most important
	series[[1]]$nodes <- lapply(series[[1]]$nodes, function(n) {
		n$label <- list(show=n$symbolSize > 30); n })
})

```
</details>
<br />


### ecStat
Statistical tools plugin in echarty &nbsp; &nbsp; &nbsp; [<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/ecStat) with code 
<a href='https://rpubs.com/echarty/ecStat' target=_blank>
<img src='img/cb-cluster.png' alt='dow' /></a>
<br />

<a id='morph'></a>

### Morphing charts
Animated transitions between charts   &nbsp; &nbsp; &nbsp;
[<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/morph) with code
[<img src='img/morph.w.png' alt='morph' />](https://rpubs.com/echarty/morph)
<br /><br />


<a id='maps'></a>

### Custom SVG map 
with mouse events &nbsp; &nbsp; &nbsp; [<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/svg)
<img src='img/cb-12.organs.png' alt='organs' />
<details><summary>🔻 View code</summary>

```r
#' JS source https://echarts.apache.org/examples/en/editor.html?c=geo-organ
#' p$x$opts from original 'options' translated with demo(js2r)
#' p$x$on handlers added manually
#' demo @ https://rpubs.com/echarty/svg
library(echarty); library(dplyr)
url <- 'https://echarts.apache.org/examples/data/asset/geo/Veins_Medical_Diagram_clip_art.svg'
svg <- url |> readLines(encoding='UTF-8') |> paste0(collapse="")
p <- ec.init(preset=FALSE,
 	tooltip= list(zz= ""), 
 	geo= list(left= 10, right= "50%", map= "organs", selectedMode= "multiple",
 				 emphasis= list(focus= "self", itemStyle= list(color= NULL), 
 				 					label= list(position= "bottom", distance= 0, textBorderColor= "#fff", textBorderWidth= 2)),
 				 blur= list(zz= ""), 
 				 select= list(itemStyle= list(color= "#b50205"), 
 				 				 label= list(show= FALSE, textBorderColor= "#fff", textBorderWidth= 2))), 
 	grid= list(left= "60%", top= "20%", bottom= "20%"), 
 	xAxis= list(zz= ""), 
 	yAxis= list(data= list("heart", "large-intestine", "small-intestine", "spleen", "kidney", "lung", "liver")), 
 	series= list(list(type= "bar", emphasis= list(focus= "self"), 
 							data= list(121, 321, 141, 52, 198, 289, 139)))
) |> ec.theme('dark-mushroom')
p$x$registerMap <- list(list(mapName='organs', svg=svg))
p$x$on <- list(list(event='mouseover', query=list(seriesIndex=0), 
						  handler=htmlwidgets::JS("function (event) {
  this.dispatchAction({ type: 'highlight', geoIndex: 0, name: event.name }); }") ),
					list(event='mouseout', query=list(seriesIndex=0),
						  handler=htmlwidgets::JS("function (event) {
  this.dispatchAction({ type: 'downplay', geoIndex: 0, name: event.name }); }") )
)
p
```
</details>
<br>


### World map plugin
 with geo points/lines in a timeline<br />
<img src='img/cb-11.geo.gif' alt='dow' />
<details><summary>🔻 View code</summary>

```r
# inspired by data from https://github.com/etiennebacher
library(dplyr)
flights <- read.csv('https://raw.githubusercontent.com/plotly/datasets/master/2011_february_aa_flight_paths.csv')
# set first two columns to longitude/latitude as default for ECharts
df <- head(flights) |> relocate(start_lon,start_lat,end_lon) |> 
  group_by(airport1) |> group_split()
# timeline options are individual charts
options <-  lapply(df, function(y) {
  series <- list(
    list(type='scatter', coordinateSystem='geo',
         data= ec.data(y, 'values'), symbolSize= 8),
    list(type='lines', coordinateSystem='geo',
         data= lapply(ec.data(y, 'names'), function(x) 
           list(coords= list(c(x$start_lon, x$start_lat), 
                              c(x$end_lon, x$end_lat)))
         ),
         lineStyle= list(curveness=0.3, width=2, color='red') )
  )
  list(title=list(text=unique(y$airport1), top=30),
       backgroundColor= '#191919',
       geo= list(map="world", roam=TRUE, center=c(-97.0372, 32.89595), zoom=4), 
       series= series)
})

library(echarty)
ec.init(preset=FALSE, load='world',
	# timeline labels need to match option titles
	timeline= list(data=unlist(lapply(options, 
										function(x) x$title$text)), axisType='category'),
	options= options
)
```
</details>
<br />

<!--
### Baidu maps 
a proof-of-concept   
[<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/bmap) (no code)  
<br />  -->
<a id='leaflet'></a>

### Leaflet maps
and switching chart selection **without Shiny**  
[<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/mapjs) with code
<br /><br />

### Leaflet maps with shape files
demo for GIS polylines, points and polygons
<video id="vidshp" preload="auto" 
	src="img/shpfiles.mp4" type="video/mp4" muted="muted" controls>
	Your browser does not support the video tag.
</video>
<details><summary>🔻 View code</summary>

```r
library(echarty)  # v.1.4.6+
library(dplyr)
library(sf)
library(spData)  # https://jakubnowosad.com/spData/

xy2df <- function(val) {
  len2 <- length(unlist(val)) /2
  as.array(matrix(unlist(val), len2, 2))
}

# ----- MULTILINESTRING -----
nc <- as.data.frame( st_transform(seine, crs=4326)) 
# build animation effect series
sd <- list()
for(i in 1:nrow(nc)) {
	sd <- append(sd, list(
		list(type= 'lines', coordinateSystem= 'leaflet', polyline= TRUE, 
			  name= nc$name[i], lineStyle= list(width=0), color= 'blue',
			  effect= list(show= TRUE, constantSpeed= 80, trailLength= 0.1, symbolSize= 3),
			  data= list(list(coords= xy2df(nc$geometry[i]))
			  ))))
}

ec.init(load= c('leaflet'),
		  js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)), 
		  series= ec.util(df= nc, nid= 'name', lineStyle= list(width= 4), verbose=TRUE),
		  tooltip= list(formatter= '{a}'), legend= list(show= TRUE),
		  color=c('red','purple','green')
) |> 
ec.upd({ series <- append(series, sd) })

# ----- MULTIPOINT -----
nc <- as.data.frame(urban_agglomerations) |> filter(year==2020) |> 
  rename(NAME= urban_agglomeration) |> 
  select(NAME, country_or_area, population_millions, geometry) |>
  rowwise() |>  # set population as Z
  mutate(geometry= st_sfc(st_point(c(unlist(geometry), population_millions))) )

ec.init(load= c('leaflet'),
    js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)), 
    series= ec.util(df= nc, name= 'Largest Cities', itemStyle= list(color= 'red')
          ,symbolSize= ec.clmn(3, scale=0.5) # urban_agglomerations
    ),
    tooltip= list(formatter= '{a}'), legend= list(show= TRUE), animation= FALSE
)

# ----- MULTIPOLYGON -----
nc <- as.data.frame(st_transform(nz, crs=4326)) |> rename(geometry= geom)
  attr(nc, 'sf_column') <- 'geometry'

ec.init(load= c('leaflet', 'custom'),  # load custom for polygons
    js= ec.util(cmd= 'sf.bbox', bbox= st_bbox(nc$geometry)),
    series= ec.util(df= nc, nid= 'Name', itemStyle= list(opacity= 0.3)),
    tooltip= list(formatter= '{a}'), animation= FALSE
)

```
</details>
<br />

### World map
with live data, color coding filter, pan/zoom  &nbsp; &nbsp; &nbsp; 
[<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/inet)  
[<img src='img/cb-speed.png' alt='world_speed' />](https://rpubs.com/echarty/inet)
<br /><br />

### 3D Globe
Interactive 3D application with ECharts <a href='https://echarts.apache.org/examples/en/index.html#chart-type-globe'>3D Globe</a>  &nbsp; &nbsp; &nbsp; 
[<span style="color:magenta">Live Demo</span>](https://rpubs.com/echarty/satellites)   
[<img src='img/sattxt.600.gif' alt='3D Globe' />](https://rpubs.com/echarty/satellites)
<details><summary>🔻 Features</summary>

- real-time satellite data filtered by altitude
- charts: scatter3D for satellite location, bar3D for beams and lines3D for tracks
- controls: hover icons, timeline play/stop, animations toggle, zoom/rotate globe
- published as <a href='https://rpubs.com/echarty/satellites'>live demo</a>
</details>
<br />

<a id='quantiles'></a>

### Quantiles
Overlay data and quantiles, then identify each with tooltips <br />
<img src='img/quantil.gif' alt='quantiles' />
<details><summary>🔻 View code</summary>

```r
# data and inspiration from https://ptarroso.github.io/quantileplot/
set.seed(555)
counts <- 1:25
n <- 50  # original is 250
x <- rep(counts, each=n)
y <- rep(NA, length(x))
for (i in counts) {
  mean.val <- log(i)+1
  sdev.val <- runif(1, 0.2, 0.8)
  y[x==i] <- round(rnorm(n, mean.val, sdev.val), 3)
}
q <- seq(0, 1, 0.025)
mat <- matrix(NA, length(q), length(counts))
for (i in 1:length(counts)) {
  val <- counts[i]
  mat[,i] <- quantile(y[x==val], probs=q)
}
mx <- as.integer(length(q)/2)
colors <- hcl.colors(mx, palette= 'sunset', alpha= 0.9)
dxy <- data.frame(x=x, y=y)

library(echarty)
p <- dxy |> ec.init(load='custom', preset=FALSE) |> ec.theme('dark') |> ec.snip()
p$xAxis <- p$yAxis <- list(show=TRUE)
p$tooltip <- list(formatter= '{a}', backgroundColor= '#55555599', 
                  textStyle= list(color='#eee'))
p$title <- list(text= 'Data + Quantiles + Tooltips + Zoom', subtext= 'inspiration article', 
                sublink= 'https://ptarroso.github.io/quantileplot/')
# p$dataZoom <- list(type='inside', start=1)
p$toolbox=list(feature= list(dataZoom=list(show=TRUE), saveAsImage=list(show=TRUE)))
for (i in 1:mx) {
  tmp <- data.frame(x= counts, hi= mat[i,], low= mat[length(q)+1-i,])
  p$series <- append(p$series,
      ecr.band(tmp, 'low', 'hi', name=paste0(round((1-q[i]*2)*100),'%'), color=colors[i])
  )
}
p$series <- append(p$series, 
    list(list(type='scatter', symbolSize= 3, itemStyle= list(color='cyan'), 
              tooltip= list(formatter='{c}'))) )
p |> ec.snip()
```
</details>
<br />

<a id='dendro'></a>

### Dendrogram
Vertical/Radial layouts, symbol size for height, values in tooltips <br />
<img src='img/cb-dendro.png' alt='Dendrogram' />
<details><summary>🔻 View code</summary>

```r
# Hierarchical Clustering dendrogram charts
toolbox <- list(
  right= '10%', top= 3, backgroundColor= 'beige',
  feature= list(mySwitcher= list(
    show= TRUE,  title= 'switch', 
    icon= 'image://https://findicons.com/icon/download/direct/465124/toggle/32/png',
    onclick= htmlwidgets::JS("function() { toggleOpt(); }"))
  ))
# JavaScript code for the switch button above
jscode <- "window.toggleOpt= function () {
    opt= chart.getOption();
    optcurr= opt.o2;  // switch options
    opt.o2= null;
    optcurr.o2= opt;
    chart.setOption(optcurr, true);
}"

hc <- hclust(dist(USArrests), "ave")
subt <- paste(as.character(hc$call)[2:3], collapse=' ')

library(echarty)   # v.1.4.4+
p <- ec.init(preset=FALSE, js=jscode) |> ec.theme('dark-mushroom')
option1 <- list(
  title= list(text= 'Radial Dendrogram', subtext= subt),
  tooltip= list(show= TRUE),
  toolbox= toolbox,
  series= list(list( 
    type= 'tree', data= ec.data(hc, format='dendrogram'),
    roam= TRUE, initialTreeDepth= -1,  # initially show all
    symbolSize= ec.clmn(-1, scale= 0.33),
    # exclude added labels like 'p99', leaving only the originals
    label= list(formatter= htmlwidgets::JS(
  		"function(n) { out= /p\\d+/.test(n.name) ? '' : n.name; return out;}")),
    layout= 'radial',
    tooltip= list(formatter= "h={c}"),
    universalTransition= list(enabled= TRUE, delay= 600) # animation
  ))
)
option2 <- within(option1, {
	title <- list(text= 'Orthogonal Dendrogram', subtext= subt)
	series[[1]]$layout <- 'orthogonal'
	series[[1]]$orient <- 'TB'
	series[[1]]$leaves <- list(label= list(
		position= 'middle',	rotate= 90, verticalAlign= 'top', align= 'right' ))
	series[[1]]$label$offset <- c(-12,0)
})
p$x$opts <- option2
p$x$opts$o2 <- option1
p
```
</details>
<br />

<a id='lottie'></a>

### Lotties are lotta fun
Add animations to charts <br />
<img src='img/cb-ghost.gif' alt='lottie demo' />
<details><summary>🔻 View code</summary>

```r
# data from https://lottiefiles.com
# plugin by https://github.com/pissang/lottie-parser
json <- 'https://helgasoft.github.io/echarty/js/spooky-ghost.json'
cont <- jsonlite::fromJSON(json, simplifyDataFrame=FALSE)

# remotes::install_github('helgasoft/echarty')
library(echarty)     # v.1.4.6.04+ 
iris |> group_by(Species) |> ec.init(
  load= 'https://helgasoft.github.io/echarty/js/lottie-parser.js',
  graphic= list(elements= list(
    list( type= "group", 
		# lottie params: info + optional scale and loop 
		info= cont, scale= 250, # loop= FALSE,
        left= 'center', top= 'middle' # ,rotation= -20
    ),
    list( type= "image", left= 20, top= 'top',
       	style= list(
          image= 'https://www.r-project.org/logo/Rlogo.png',
          width= 150, height= 150, opacity= .4)
	)
  ))
)
```
</details>
<br />