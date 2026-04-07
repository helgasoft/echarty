# Introduction to echarty

## Description

**echarty** provides a lean interface between R and Javascript library
[ECharts](https://echarts.apache.org/en/index.html).  
We encourage users to follow the original ECharts [API
documentation](https://echarts.apache.org/en/option.html) to construct
charts with echarty.  
Main command **ec.init** can set multiple native ECharts options to
build a chart.  
The benefits - learn a very limited set of commands, and enjoy the
**full functionality** of ECharts.

## Package Conventions

pipe-friendly - supports both %\>% and \|\>  
commands have three prefixes to help with auto-completion:

- **ec.** for general functions, like *ec.init*
- **ecs.** for Shiny functions, like *ecs.output*
- **ecr.** for rendering functions, like *ecr.band*

## Events

For event handling in Shiny see sample code in
[eshiny.R](https://github.com/helgasoft/echarty/blob/main/demo/eshiny.R),
run as `demo(eshiny)`.  
Echarty has three built-in event callbacks - *click*, *mouseover*,
*mouseout*. All other ECharts
[events](https://echarts.apache.org/en/api.html#events) could be
initialized through `p$x$capture`. Another option is to use `p$x$on`
with JavaScript handlers, see code in
[examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R).

## Widget **x** parameters

These are *htmlwidget* and *ECharts* initialization parameters supported
by echarty. There are code samples for most of them in
[examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R):

- capture = event name(s), to monitor events, usually in Shiny
- on = define JavaScript code for event handling, see
  [ECharts](https://echarts.apache.org/en/api.html#echartsInstance.on)
- registerMap = define a map from a geoJSON file, see
  [ECharts](https://echarts.apache.org/en/api.html#echartsInstance.registerMap)
- group = group-name of a chart, see
  [ECharts](https://echarts.apache.org/en/api.html#echartsInstance.group)
- connect = command to connect charts with same group-name, see
  [ECharts](https://echarts.apache.org/en/api.html#echarts.connect)
- locale = EN(default) or ZH, set from *locale* parameter of *ec.init*,
  see [ECharts](https://echarts.apache.org/en/api.html#echarts.init).
- renderer = *canvas*(default) or *svg*, set from *renderer* in
  *ec.init*, see
  [ECharts](https://echarts.apache.org/en/api.html#echarts.init).
- jcode = custom JavaScript code to execute, set from *js* parameter of
  *ec.init*

## R vs Javascript numbering

R language counting starts from 1.  
Javascript (JS) counting starts from 0.  
*ec.init* supports R-counting of indexes (ex. encode) and dimension (ex.
visualMap).  
*ec.upd* requires indexes and dimensions to be set with JS-counting.

## Javascript built-in functions

To allow access to charts from JS.  
*ec_chart(id)* - get the chart object by id (former get_e_charts)  
*ec_option(id)* - get the chart’s option object by id (former
get_e_charts_opt)  
Parameter *id* could be the internal variable *echwid*, or the value set
through *ec.init* parameter *elementId*. See demo code in
[examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R)

## Code examples

Here is the complete list of sample code **locations**:

- website
  [gallery](https://helgasoft.github.io/echarty/articles/gallery.html)
- [demo
  examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R)
- Shiny code is in
  [eshiny.R](https://github.com/helgasoft/echarty/blob/main/demo/eshiny.R),
  run with `demo(eshiny)`
- demos on [RPubs](https://rpubs.com/echarty)
- searchable [gists](https://gist.github.com/helgasoft)
- answers to [Github
  issues](https://github.com/helgasoft/echarty/issues)
- code in [Github
  tests](https://github.com/helgasoft/echarty/tree/main/tests/testthat)
- command examples, like in *?ec.init*

## Global Options

Options are set with R command
[options](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/options).  
Echarty uses the following options:

- echarty.theme = name of theme file, without extension, from folder
  `/inst/themes`
- echarty.font = font family name
- echarty.urlTiles = tiles URL template for leaflet maps

``` r
# set/get global options
options('echarty.theme'='jazz') # set
getOption('echarty.theme')      # get
#> [1] "jazz"
options('echarty.theme'=NULL)   # remove
```
