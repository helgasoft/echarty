# Mapping

## Tutorial: Build an interactive map with regional data

  
We’ll need a **map**, some **data** - and **echarty** to bring them
together.  
Map is France by regions, from
[here](https://raw.githubusercontent.com/echarts-maps/echarts-countries-js/master/echarts-countries-js/France.js).  
Data is France population by region, from
[here](https://www.ined.fr/en/everything_about_population/data/france/population-structure/regions_departments).  
Let start by initializing the chart, load the map as a plugin and make
sure it shows correctly.

``` r
library(echarty); library(dplyr)
url <- 'https://raw.githubusercontent.com/echarts-maps/echarts-countries-js/master/echarts-countries-js/France.js'
ec.init(
    load= url, 
    series.param= list(type= 'map', map= 'France', roam= TRUE)
)
```

We use *ec.init()* to load the map plugin. We set also ‘series.param’
for series’ parameters *type, map* and *roam* documented
[here](https://echarts.apache.org/en/option.html#series-map).  
No error, but also …no map ??!  
  
The problem is in the **map name**. In
[docs](https://echarts.apache.org/en/option.html#series-map.map) we see
that for file ‘china.js’ they use *map=‘china’*. So by analogy, it’s
easy to set *map=‘France’* for ‘France.js’ ?  
Actually map name and file name may have nothing in common. We’ll need
to dig inside the JS file to find the exact name.  
Open *France.js* in a text editor and look for *‘registerMap(’*. It
turns out the name right after is ‘法国’ - Chinese for ‘France’. Let’s
update the code. The map has been already installed, so we just load it
by name (‘<file://France.js>’). We also add a
[title](https://echarts.apache.org/en/option.html#title).

``` r
ec.init( load= 'file://France.js',
    title = list(text= 'France'),
    series.param = list(type= 'map', map= '法国', roam= TRUE)
)
```

The map is indeed of France, it’s zoomable and we see the regions
highlighted on hover.  
Ok, map part done ✔. Data is next (with some surprises).  
Our [data
page](https://www.ined.fr/en/everything_about_population/data/france/population-structure/regions/)
shows a blue table. Source code inspection reveals a data table and
we’ll use library *rvest* to extract it.

``` r
library(rvest)
furl <- 'https://www.ined.fr/en/everything_about_population/data/france/population-structure/regions/'
wp <- read_html(furl)
wt <- wp %>% html_node('#para_nb_1 > div > div > div > table') %>% html_table(header=TRUE)
wt
```

    ## # A tibble: 14 × 5
    ##    ``      `Census 1990` `Census 1999` `January 1st 2008` `January 1st 2024 (p)`
    ##    <chr>   <chr>         <chr>         <chr>              <chr>                 
    ##  1 Auverg… 6 668 168     6 949 608     8 235 923          8 197 325             
    ##  2 Bourgo… 2 705 826     2 728 086     2 791 719          2 786 296             
    ##  3 Bretag… 2 794 317     2 904 075     3 453 023          3 429 882             
    ##  4 Centre… 2 369 808     2 440 295     2 573 295          2 572 278             
    ##  5 Corse   249 645       260 152       355 528            351 255               
    ##  6 Grand … 5 274 064     5 387 509     5 568 711          5 562 262             
    ##  7 Hauts-… 5 770 671     5 855 448     5 983 823          5 980 697             
    ##  8 Île-de… 10 644 665    10 946 012    12 419 961         12 358 932            
    ##  9 Norman… 3 126 859     3 202 449     3 327 077          3 317 023             
    ## 10 Nouvel… 5 114 287     5 257 954     6 154 772          6 110 365             
    ## 11 Occita… 4 546 249     4 842 680     6 154 729          6 101 005             
    ## 12 Pays d… 3 055 197     3 219 960     3 926 389          3 907 426             
    ## 13 Proven… 4 257 244     4 502 385     5 198 011          5 160 091             
    ## 14 France… 56 577 000    58 496 613    66 142 961         65 834 837

We’ll have to do some cleanup, like rename columns, remove all spaces
and the summary row. Then try adding the data to the series.  
As you can see, we are using *ec.data()* for data conversion from
data.frame to a list. Each row becomes a sublist with *name* and
*value*. Name is the region name, and value is the number of people
(ppl).  
Added also a *visualMap* which will color the regions depending on their
values (population).

``` r
library(rvest)
wp <- read_html(furl)
wt <- wp %>% html_node('#para_nb_1 > div > div > div > table') %>% html_table(header=TRUE)
names(wt) <- c('region','v1','v2','v3','ppl') # rename columns
wt <- wt[-nrow(wt),]     # delete summary row, contaminates color values
wt$ppl <- as.numeric(gsub('[^\x01-\x7f]', '', wt$ppl))    # remove weird spaces

ec.init(load='file://France.js',
    title= list(text= 'France'),
    series.param= list(type='map', map='法国', roam=TRUE,
       data= lapply(ec.data(wt, 'names'), 
                      function(x) list(name= x$region, value= x$ppl))
    ),
    visualMap= list(type= 'continuous', calculable= TRUE, max= max(wt$ppl))
)
```

Ok, there is some color, but why most of the regions are blank?  
It’s again a data problem. The region names from the map and those from
the web page do not match completely. It’s more difficult to change the
map, so we’ll update the *wt* data.frame instead. To replace region
names, lets use conditional *mutate* from *dplyr*.  
The *visualMap* is enhanced with a
[*formatter*](https://echarts.apache.org/en/option.html#visualMap-continuous.formatter).
It shows the *visualMap* values as formatted integers. Sometimes, like
in our case, formatters are JS code that needs to be wrapped in
*htmlwidgets::JS()* so it can be sent to ECharts for execution. Other
[template
formatters](https://echarts.apache.org/en/option.html#series-scatter.tooltip.formatter)
are just strings like *“{a}: {c}”*.  
Note also how the *max* parameter is set. Without it the color range
would be out of sync.

``` r
wp <- read_html(furl)
wt <- wp %>% html_node('#para_nb_1 > div > div > div > table') %>% html_table(header=T)
names(wt) <- c('region','v1','v2','v3','ppl') # rename columns
wt$ppl <- as.numeric(gsub(' ','', wt$ppl))    # remove weird(binary) spaces
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

url <- 'https://raw.githubusercontent.com/echarts-maps/echarts-countries-js/master/echarts-countries-js/France.js'
ec.init( load= url, 
    title= list(text='France Population (current)'),
    backgroundColor= 'whitesmoke', tooltip= list(show=T),
    series.param= list(type='map', map='法国', roam=TRUE, 
        data= lapply(ec.data(wt, 'names'), function(x) list(name= x$region, value= x$ppl)) 
    ),
    visualMap= list(type= 'continuous', calculable= TRUE, 
        max= max(wt$ppl), min= min(wt$ppl), formatter= ec.clmn('%L@', -1))        
)
```

So here is the final result - a nice map with pan/zoom and value
coloring.
