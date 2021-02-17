<!--
---
title: "use case1"
author: "Helgasoft"
date: "2/17/2021"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

```{r cars}
summary(cars)
```

## Including Plots

You can also embed plots, for example:

```{r pressure, echo=FALSE}
plot(pressure)
```

Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.

<hr>  -->

# Use Case 1 - step-by-step tutorial

## Building an interactive map with regional data

<br /> We'll need a **map**, some **data**, and **echarty** to bring them together. <br /> Map is France by regions, from [here](https://raw.githubusercontent.com/echarts-maps/echarts-countries-js/master/echarts-countries-js/France.js). <br /> Data is France population by region, from [here](https://www.ined.fr/en/everything_about_population/data/france/population-structure/regions_departments/). <br /> Let start by initializing the chart, load the map (as a plugin) and make sure it shows correctly.

``` {.r}
library(echarty)
url <- 'https://raw.githubusercontent.com/echarts-maps/echarts-countries-js/master/echarts-countries-js/France.js'
p <- ec.init() %>% ec.depjs(url)
p$x$opts <- list(
   series = list(list(type='map', map='France', roam=TRUE))
)
p
```

We use *ec.init()* without worrying about presets(xAxis,yAxis) since we rewrite the option *p\$x\$opts* from scratch. Command *ec.depjs()* is to load the map as a plugin. We'll set only one option component - 'series', and [here](https://echarts.apache.org/en/option.html#series-map) is the documentation of its own sub-components - *type, map* and *roam*. <br /> 
Adding widget '**p**' at the end will display the chart in RStudio's Viewer panel.<p>
Run the code. We get a popup prompt *"One-time installation of plugin France.js"*, answer **Yes**. The map is installed with some feedback in the Console panel. And no error, but also ...no map ??!</p> 
The problem is in the **map name**. Reading the [docs](https://echarts.apache.org/en/option.html#series-map.map) we see that for file 'china.js' they use *map='china'*. So the easy conclusion would be to set *map='France'* for 'France.js'?<br /> 
Actually map name and file name may have nothing in common. We'll need to dig in inside the JS file to find the exact name.<br /> Open *France.js* in a text editor and look for *'registerMap('*. It turns out the name after is '法国' - Chinese for 'France'. Lets update the code:

``` {.r}
p <- ec.init(load='France.js')
p$x$opts <- list(
  title = list(show=TRUE, text='France'),
  series = list(list(type='map', map='法国', roam=TRUE))
)
p
```

The map has been already installed, so we do not need to use *ec.depjs()* again. We just load it inside *ec.init()*. Lets also add a [title](https://echarts.apache.org/en/option.html#title). Running the updated code results in the following chart <br /> 
<img src="img/uc1-1.png" alt="chart1"/> <br /> 
The map is indeed of France, it's zoomable and we see the regions highlighted on hover.<br /> Ok, map part done ✔. Data is next (and it has some surprises for us).   
<p>Our [data page](https://www.ined.fr/en/everything_about_population/data/france/population-structure/regions_departments/) shows a blue table. Source code inspection reveals a data table and we'll use library *rvest* to extract it.</p>
``` {.r}
library(rvest)
wp <- read_html('https://www.ined.fr/en/everything_about_population/data/france/population-structure/regions_departments/')
wt <- wp %>% html_node('#para_nb_1 > div > div > div > table') %>% html_table(header=TRUE)
wt
```

We'll have to do some cleanup, like rename columns and remove spaces and summary row. Then try adding the data to the series.<br />

``` {.r}
wp <- read_html('https://www.ined.fr/en/everything_about_population/data/france/population-structure/regions_departments/')
wt <- wp %>% html_node('#para_nb_1 > div > div > div > table') %>% html_table(header=TRUE)
names(wt) <- c('region','v1','v2','v3','ppl') # rename columns
wt$ppl <- as.numeric(gsub(' ','', wt$ppl))    # remove spaces
wt <- wt[-nrow(wt),]     # delete summary row, contaminates color values
p <- ec.init(load='France.js')
p$x$opts <- list(
  title = list(show=TRUE, text='France'),
  series = list(list(type='map', map='法国', 
    roam=TRUE,
    data = lapply(ec.data(wt,TRUE), function(x) 
      list(name=x$value[1], value=x$value[5]))
  )),
  visualMap = list(type='continuous',calculable=TRUE, max=max(wt$ppl))
)
p
```

As you can see, we are using *ec.data()* for data conversion from data.frame to a list. Each row becomes a sublist with *name* and *value*. Name is the region name, and value is the number of people ('ppl' is 5th column). <br /> Added also is a *visualMap* which will color the regions depending on their values (population). Running the above code brings us this chart:<br /> 
<img src="img/uc1-2.png" alt="chart1"/> <br /> 

Ok, there is some color, but why most of the regions are blank? It's again a data problem. The region names from the map and those from the web page do not match completely. It's more difficult to change the map, so we'll update the wt data.frame instead. Here is the conversion table:<br />

``` {.r}
library(dplyr)
wp <- read_html('https://www.ined.fr/en/everything_about_population/data/france/population-structure/regions_departments/')
wt <- wp %>% html_node('#para_nb_1 > div > div > div > table') %>% html_table(header=TRUE)
names(wt) <- c('region','v1','v2','v3','ppl') # rename columns
wt$ppl <- as.numeric(gsub(' ','', wt$ppl))    # remove spaces
wt <- wt[-nrow(wt),]     # delete summary row, contaminates color values
wt <- wt %>% mutate(region = case_when(
  region=='Grand Est'       ~'Alsace–Champagne-Ardenne–Lorraine',
  region=='Nouvelle Aquitaine'  ~'Aquitaine-Limousin-Poitou-Charentes',
  region=='Bretagne'        ~'Brittany',                                    
  region=='Île-de-France'   ~'Ile-de-France',
  region=='Occitanie'       ~'Languedoc-Roussillon-Midi-Pyrénées',
  region=='Hauts-de-France' ~'Nord-Pas-de-Calais and Picardy',
  region=='Normandie'       ~'Normandy',
  region=='Centre - Val de Loire'   ~'Centre-Val de Loire',
  region=='Corse'           ~'Corsica',
  region=='Provence-Alpes-Côte d’Azur'  ~"Provence-Alpes-Côte d'Azur",
  region=='Bourgogne- Franche-Comté'    ~'Bourgogne-Franche-Comté',
  TRUE ~ region))
p <- ec.init(load='France.js')
p$x$opts <- list(
  title = list(show=TRUE, text='France Population'),
  backgroundColor = 'whitesmoke',
  series = list(list(type='map', map='法国', 
    roam=TRUE,
    data = lapply(ec.data(wt,TRUE), function(x) 
      list(name=x$value[1], value=x$value[5]))
  )),
  visualMap = list(type='continuous',calculable=TRUE, min=0, max=max(wt$ppl),
      formatter = htmlwidgets::JS("function(value) { 
         return value.toLocaleString(undefined, {maximumFractionDigits: 0}); }"))
)
p
```

At last we have the final code above. The *visualMap* has been enhanced with a *formatter* ([docs](https://echarts.apache.org/en/option.html#visualMap-continuous.formatter)). It shows the *visualMap* values as formatted integers.<br /> 
Sometimes formatters are JS code that has to be wrapped in *htmlwidgets::JS()* so it can be sent to Echarts for execution.<br /> 
Other [simpler formatters](https://echarts.apache.org/en/option.html#series-scatter.tooltip.formatter) are just strings like *"{a}: {c}"*. <br />
So here is the result of our efforts - a nice map with pan/zoom and value coloring.<br />
<img src="img/uc1-3.png" alt="chart1"/> <br /> 

Wasn't too hard, was it? Share your thoughts in [Discussions](https://github.com/helgasoft/echarty/discussions). 
<br /> <br /> 
Oh wait, the boss just got another (always brilliant) idea: <br /> "Could you get the total population number from the summary row and add it as subtitle?"... 
<br />   <br />   <br />   <br />  
