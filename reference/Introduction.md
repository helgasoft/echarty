# echarty

echarty

## Details

### Description

**echarty** provides a lean interface between R and Javascript library
[ECharts](https://echarts.apache.org/en/index.html). We encourage users
to follow the original ECharts [API
documentation](https://echarts.apache.org/en/option.html) to construct
charts with echarty. Main command **ec.init** can set multiple native
ECharts options to build a chart. The benefits - learn a very limited
set of commands, and enjoy the **full functionality** of ECharts.

### Package Conventions

pipe-friendly - supports both %\>% and \|\> commands have three prefixes
to help with auto-completion:

- **ec.** for general functions, like *ec.init*

- **ecs.** for Shiny functions, like *ecs.output*

- **ecr.** for rendering functions, like *ecr.band*

### Events

Event handling in **Shiny** is done through callbacks.  
See considerable sample code in
[eshiny.R](https://github.com/helgasoft/echarty/blob/main/demo/eshiny.R),
run as `demo(eshiny)`.  
There are three built-in event callbacks - *click*, *mouseover*,
*mouseout*. All other ECharts
[events](https://echarts.apache.org/en/api.html#events) could be
initialized through `ec.init(capture=...)`.  
For event handling in R (without Shiny) use parameter `ec.init(on=...)`
which expects JavaScript handlers. Search for ‘event’ in [code
examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R).

### ECharts initialization parameters

Chart initialization is performed by the *echarty::ec.init()* command.  
Here is full list of *ec.init* optional parameters:

- *ask, js, elementId, ctype, xtKey, dbg* are specific to *echarty*  

- *theme, iniOpts, on, off, capture, group* belong to the ECharts [chart
  instance](https://echarts.apache.org/en/api.html#echartsInstance)
  object.  

- *connect, disconnect, registerMap, registerTheme, registerLocale,
  registerCustomSeries* are commands of the global [ECharts
  object](https://echarts.apache.org/en/api.html#echarts).

There are R [code
examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R)
for some of these parameters.

### R vs Javascript numbering

R language counting starts from 1.  
Javascript (JS) counting starts from 0.  
*ec.init* supports R-counting of indexes (ex. encode) and dimension (ex.
visualMap).  
All other contexts like *ec.upd* or *ecs.proxy* require JS-counting of
indexes and dimensions.

### Javascript built-in functions

To allow access to charts from JS.  
**ec_chart(id)** - get the chart object by id  
**ec_option(id)** - get the chart’s option object by id  
Parameter *id* could be the internal JS variable *echwid*, or the value
set through *ec.init* parameter *elementId*. See [code
examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R).

### Column-to-style binding with encode

ECharts [series
encode](https://echarts.apache.org/en/option.html#series-bar.encode)
enables binding axes and tooltip to data columns. Echarty enhances this
method for all [series
data](https://echarts.apache.org/en/option.html#series-bar.data)
parameters like itemStyle,labels,emphasis,etc. The bindings are set
through `series$encode$data`. For instance  
`encode= list(data= list(value= c('xc','yc'), itemStyle= list(opacity= 'oc')))`  
would match columns *xc,yc,oc* to each item’s value and opacity. The
result is a new `series$data` added to the series. It permits to finely
customize chart elements directly from data.  
Echarty has also an alternative tool, style-named columns with
`ec.data(..nasep)`, but `encode$data` offers more flexibility. It is not
compatible with *timeline* however.

### Code examples

Here is the complete list of sample code **locations**:

- website
  [gallery](https://helgasoft.github.io/echarty/articles/gallery.html)

- collection of [code
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

### Global Options

Options are set with R command
[options](https://www.rdocumentation.org/packages/base/versions/3.6.2/topics/options).
Echarty uses the following options:

- echarty.theme = name of theme file, without extension, from folder
  `/inst/themes`

- echarty.font = font family name

- echarty.urlTiles = tiles URL template for leaflet maps

    # set/get global options
    options('echarty.theme'='jazz') # set
    getOption('echarty.theme')      # get
    #> [1] "jazz"
    options('echarty.theme'=NULL)   # remove
