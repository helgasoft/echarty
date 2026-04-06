# Initialize a chart

Required to build a chart. In most cases this will be the only command
necessary.

## Usage

``` r
ec.init(
  df = NULL,
  preset = TRUE,
  ...,
  series.param = NULL,
  tl.series = NULL,
  width = NULL,
  height = NULL
)
```

## Arguments

- df:

  Optional data.frame to be preset as
  [dataset](https://echarts.apache.org/en/option.html#dataset), default
  NULL  
  By default the first column is for X values, second column is for Y,
  and third is for Z when in 3D.  
  Best practice is to have the grouping column placed last. Grouping
  column cannot be used as axis.  
  Timeline requires a *grouped data.frame* to build its
  [options](https://echarts.apache.org/en/option.html#options).  
  If grouping is on multiple columns, only the first one is used to
  determine settings.

- preset:

  Boolean (default TRUE). Build preset attributes like dataset, series,
  xAxis, yAxis, etc.  
  When preset is FALSE, these attributes need to be set explicitly.  

- ...:

  Optional widget attributes. See Details.  

- series.param:

  Additional attributes for single preset series, default is NULL.  
  Defines a **single** series for both non-timeline and timeline charts.
  Default type is 'scatter'.  
  **Multiple** series need to be defined directly with
  *series=list(list(type=...),list(type=...))* or added with
  [ec.upd](https://helgasoft.github.io/echarty/reference/ec.upd.md).

- tl.series:

  Deprecated, use *timeline* and *series.param* instead.  

- width, height:

  Optional valid CSS unit (like `'100%'`, `'500px'`, `'auto'`) or a
  number, which will be coerced to a string and have `'px'` appended.

## Value

A widget to plot, or to save and expand with more features.

## Details

Command *ec.init* creates a widget with
[createWidget](https://rdrr.io/pkg/htmlwidgets/man/createWidget.html),
then adds some ECharts features to it.  
Numerical indexes for series,visualMap,etc. are R-counted (1,2...)  

**Presets**  
A [dataset](https://echarts.apache.org/en/option.html#dataset) is
pre-set when data.frame **df** is present.  
When **df** is grouped, more datasets with legend and series are also
preset.  
Axes for some charts are preset with name and type when suitable.  
Plugin '3D' (load='3D') is required for GL series like *scatterGL,
linesGL*, etc.  
Plugins 'leaflet' and 'world' preset *center* to the mean of all
coordinates from **df**.  
Users can delete or overwrite any presets as needed.  

**Widget attributes**  
Optional echarty widget attributes include:  

- elementId - Id of the widget, default is NULL(auto-generated, stored
  as *echwid* variable for JS)

- load - name(s) of plugin(s) to load. A character vector or
  comma-delimited string. default NULL.

- ask - boolean to prompt user before downloading plugins when *load* is
  present, default is FALSE.  
    Could also be string 'loadRemote' to load plugins remotely.  

- ctype - alternative way of setting chart type name, default is
  'scatter'.  

- js - single string or a vector with JavaScript expressions to
  evaluate.  
  single: exposed *chart* object (most common)  
  vector:   see code in
  [examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R)  
    First expression evaluated with exposed objects *window* and
  *echarts*  
    Second is evaluated with exposed object *opts*.  
    Third is evaluated with exposed *chart* object after initialization
  with *opts* already set.

- theme - name of built-in theme to apply, or JSON object from
  *fromJSON*, see *opts* in
  [echarts.init](https://echarts.apache.org/en/api.html#echarts.init)  

- iniOpts - a list of initialization options, see *opts* in
  [echarts.init](https://echarts.apache.org/en/api.html#echarts.init)  
    Defaults: renderer='canvas', locale='EN', useDirtyRect=FALSE  

- on,off,capture,group - chart instance properties, namely:  
    on/off is a list of events to handle with JS, each in a list, see
  [chart.on](https://echarts.apache.org/en/api.html#echartsInstance.on)
  and example below  
    capture is a vector of event names to capture in Shiny, etc.  

- connect,disconnect,register,etc. - see [echarts
  object](https://echarts.apache.org/en/api.html#echarts) methods  

**Built-in plugins**  

- leaflet - Leaflet maps with customizable tiles, see
  [source](https://github.com/gnijuohz/echarts-leaflet#readme)  

- world - world map with country boundaries, see
  [source](https://github.com/apache/echarts/tree/master/test/data/map/js)  

- lottie - support for https://lottiefiles.com  

- ecStat - statistical tools,
  see[echarts-stat](https://github.com/ecomfe/echarts-stat)  

- custom - renderers for echarty plugins like
  [ecr.band](https://helgasoft.github.io/echarty/reference/ecr.band.md)
  and
  [ecr.ebars](https://helgasoft.github.io/echarty/reference/ecr.ebars.md)  

**Plugins with one-time installation**  

- 3D - support for 3D charts and WebGL acceleration, see
  [source](https://github.com/ecomfe/echarts-gl) and
  [docs](https://echarts.apache.org/en/option-gl.html#series)  
    This plugin is auto-loaded when 3D/GL axes/series are detected.  

- gmodular - graph modularity, see
  [source](https://github.com/ecomfe/echarts-graph-modularity)  

- liquid - liquid fill, see
  [source](https://github.com/ecomfe/echarts-liquidfill)  

- wordcloud - cloud of words, see
  [source](https://github.com/ecomfe/echarts-wordcloud)  
  Note: the last three are being moved to the [official custom
  series](https://github.com/apache/echarts-custom-series).  
  OR install your own third-party plugins like *confetti*, see example
  below.  

**Crosstalk**  
Parameter *df* should be of type
[SharedData](https://rdrr.io/pkg/crosstalk/man/SharedData.html), see
[more
info](https://helgasoft.github.io/echarty/articles/gallery.html#crosstalk-2d).  
Optional parameter *xtKey*: unique ID column name of data frame *df*.
Must be same as *key* parameter used in *SharedData\$new()*. If missing,
a new column *XkeyX* will be appended to df.  
Enabling *crosstalk* will also generate an additional dataset called
*Xtalk* and bind the **first series** to it.  

**Timeline**  
Defined by *series.param* for the [options
series](https://echarts.apache.org/en/option.html#series) and a
*timeline* list for the [actual
control](https://echarts.apache.org/en/option.html#timeline). A grouped
*df* is required, each group providing data for one option serie.
Timeline [data](https://echarts.apache.org/en/option.html#timeline.data)
and [options](https://echarts.apache.org/en/option.html#options) will be
preset for the chart.  
Each option title can include the current timeline item by adding a
placeholder '%@' in title\$text. See example below.  
Another preset is *encode(x=1,y=2,z=3)*, which are the first 3 columns
of *df*. Parameter *z* is ignored in 2D. See Details below.  
Optional attribute *groupBy*, a *df* column name, can create series
groups inside each timeline option.  
Options/timeline for hierarchical charts like graph,tree,treemap,sankey
have to be built directly, see
[example](https://helgasoft.github.io/echarty/uc4.html).

Optional series attribute
[encode](https://echarts.apache.org/en/option.html#series-line.encode)
defines which columns to use for the axes, depending on chart type and
coordinate system:  

- set *x* and *y* for coordinateSystem *cartesian2d*

- set *lng* and *lat* for coordinateSystem *geo* and *scatter* series

- set *value* and *name* for coordinateSystem *geo* and *map* series

- set *radius* and *angle* for coordinateSystem *polar*

- set *value* and *itemName* for *pie* chart.

There is an advanced usage of *encode* when each series' item needs to
be customized.  
For example `encode= list(itemStyle= list(opacity='opac'))` will create
series data where each series item's opacity comes from df column
'opac'.  
This binding feature is specific to *echarty* and does not exist in
ECharts. See example below.  

## Examples

``` r
 # basic scatter chart from a data.frame using presets
cars |> ec.init()

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"series":[{"type":"scatter","id":"ec.auto"}],"xAxis":{"type":"value","name":"speed","show":true},"yAxis":{"type":"value","name":"dist","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
 # custom inititlization options and theme
myth <- '{"color": ["green"], "backgroundColor": "lemonchiffon"}'
ec.init( cars,
  iniOpts= list(renderer= 'svg', width= '222px'),
  theme= jsonlite::fromJSON(myth),
  toolbox= list(feature= list(saveAsImage= list()))
)

{"x":{"iniOpts":{"renderer":"svg","width":"222px","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"theme":{"color":"green","backgroundColor":"lemonchiffon"},"opts":{"toolbox":{"feature":{"saveAsImage":[]}},"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"series":[{"type":"scatter","id":"ec.auto"}],"xAxis":{"type":"value","name":"speed","show":true},"yAxis":{"type":"value","name":"dist","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]} 
 # grouping, tooltips, formatting, events
iris |> dplyr::group_by(Species) |> 
ec.init(        # init with presets
  tooltip= list(show= TRUE),
  series.param= list( 
    symbolSize= ec.clmn('Petal.Width', scale=7),
    tooltip= list(formatter= ec.clmn('Petal.Width: %@', 'Petal.Width'))
  ),
  on= list(   # events with Javascript handler
    list(event= 'legendselectchanged', handler= ec.clmn("(e) => alert('legend:'+e.name);"))
 )
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"on":[{"event":"legendselectchanged","handler":"(e) => alert('legend:'+e.name);"}],"opts":{"tooltip":{"show":true},"dataset":[{"dimensions":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"],"source":[[5.1,3.5,1.4,0.2,"setosa"],[4.9,3,1.4,0.2,"setosa"],[4.7,3.2,1.3,0.2,"setosa"],[4.6,3.1,1.5,0.2,"setosa"],[5,3.6,1.4,0.2,"setosa"],[5.4,3.9,1.7,0.4,"setosa"],[4.6,3.4,1.4,0.3,"setosa"],[5,3.4,1.5,0.2,"setosa"],[4.4,2.9,1.4,0.2,"setosa"],[4.9,3.1,1.5,0.1,"setosa"],[5.4,3.7,1.5,0.2,"setosa"],[4.8,3.4,1.6,0.2,"setosa"],[4.8,3,1.4,0.1,"setosa"],[4.3,3,1.1,0.1,"setosa"],[5.8,4,1.2,0.2,"setosa"],[5.7,4.4,1.5,0.4,"setosa"],[5.4,3.9,1.3,0.4,"setosa"],[5.1,3.5,1.4,0.3,"setosa"],[5.7,3.8,1.7,0.3,"setosa"],[5.1,3.8,1.5,0.3,"setosa"],[5.4,3.4,1.7,0.2,"setosa"],[5.1,3.7,1.5,0.4,"setosa"],[4.6,3.6,1,0.2,"setosa"],[5.1,3.3,1.7,0.5,"setosa"],[4.8,3.4,1.9,0.2,"setosa"],[5,3,1.6,0.2,"setosa"],[5,3.4,1.6,0.4,"setosa"],[5.2,3.5,1.5,0.2,"setosa"],[5.2,3.4,1.4,0.2,"setosa"],[4.7,3.2,1.6,0.2,"setosa"],[4.8,3.1,1.6,0.2,"setosa"],[5.4,3.4,1.5,0.4,"setosa"],[5.2,4.1,1.5,0.1,"setosa"],[5.5,4.2,1.4,0.2,"setosa"],[4.9,3.1,1.5,0.2,"setosa"],[5,3.2,1.2,0.2,"setosa"],[5.5,3.5,1.3,0.2,"setosa"],[4.9,3.6,1.4,0.1,"setosa"],[4.4,3,1.3,0.2,"setosa"],[5.1,3.4,1.5,0.2,"setosa"],[5,3.5,1.3,0.3,"setosa"],[4.5,2.3,1.3,0.3,"setosa"],[4.4,3.2,1.3,0.2,"setosa"],[5,3.5,1.6,0.6,"setosa"],[5.1,3.8,1.9,0.4,"setosa"],[4.8,3,1.4,0.3,"setosa"],[5.1,3.8,1.6,0.2,"setosa"],[4.6,3.2,1.4,0.2,"setosa"],[5.3,3.7,1.5,0.2,"setosa"],[5,3.3,1.4,0.2,"setosa"],[7,3.2,4.7,1.4,"versicolor"],[6.4,3.2,4.5,1.5,"versicolor"],[6.9,3.1,4.9,1.5,"versicolor"],[5.5,2.3,4,1.3,"versicolor"],[6.5,2.8,4.6,1.5,"versicolor"],[5.7,2.8,4.5,1.3,"versicolor"],[6.3,3.3,4.7,1.6,"versicolor"],[4.9,2.4,3.3,1,"versicolor"],[6.6,2.9,4.6,1.3,"versicolor"],[5.2,2.7,3.9,1.4,"versicolor"],[5,2,3.5,1,"versicolor"],[5.9,3,4.2,1.5,"versicolor"],[6,2.2,4,1,"versicolor"],[6.1,2.9,4.7,1.4,"versicolor"],[5.6,2.9,3.6,1.3,"versicolor"],[6.7,3.1,4.4,1.4,"versicolor"],[5.6,3,4.5,1.5,"versicolor"],[5.8,2.7,4.1,1,"versicolor"],[6.2,2.2,4.5,1.5,"versicolor"],[5.6,2.5,3.9,1.1,"versicolor"],[5.9,3.2,4.8,1.8,"versicolor"],[6.1,2.8,4,1.3,"versicolor"],[6.3,2.5,4.9,1.5,"versicolor"],[6.1,2.8,4.7,1.2,"versicolor"],[6.4,2.9,4.3,1.3,"versicolor"],[6.6,3,4.4,1.4,"versicolor"],[6.8,2.8,4.8,1.4,"versicolor"],[6.7,3,5,1.7,"versicolor"],[6,2.9,4.5,1.5,"versicolor"],[5.7,2.6,3.5,1,"versicolor"],[5.5,2.4,3.8,1.1,"versicolor"],[5.5,2.4,3.7,1,"versicolor"],[5.8,2.7,3.9,1.2,"versicolor"],[6,2.7,5.1,1.6,"versicolor"],[5.4,3,4.5,1.5,"versicolor"],[6,3.4,4.5,1.6,"versicolor"],[6.7,3.1,4.7,1.5,"versicolor"],[6.3,2.3,4.4,1.3,"versicolor"],[5.6,3,4.1,1.3,"versicolor"],[5.5,2.5,4,1.3,"versicolor"],[5.5,2.6,4.4,1.2,"versicolor"],[6.1,3,4.6,1.4,"versicolor"],[5.8,2.6,4,1.2,"versicolor"],[5,2.3,3.3,1,"versicolor"],[5.6,2.7,4.2,1.3,"versicolor"],[5.7,3,4.2,1.2,"versicolor"],[5.7,2.9,4.2,1.3,"versicolor"],[6.2,2.9,4.3,1.3,"versicolor"],[5.1,2.5,3,1.1,"versicolor"],[5.7,2.8,4.1,1.3,"versicolor"],[6.3,3.3,6,2.5,"virginica"],[5.8,2.7,5.1,1.9,"virginica"],[7.1,3,5.9,2.1,"virginica"],[6.3,2.9,5.6,1.8,"virginica"],[6.5,3,5.8,2.2,"virginica"],[7.6,3,6.6,2.1,"virginica"],[4.9,2.5,4.5,1.7,"virginica"],[7.3,2.9,6.3,1.8,"virginica"],[6.7,2.5,5.8,1.8,"virginica"],[7.2,3.6,6.1,2.5,"virginica"],[6.5,3.2,5.1,2,"virginica"],[6.4,2.7,5.3,1.9,"virginica"],[6.8,3,5.5,2.1,"virginica"],[5.7,2.5,5,2,"virginica"],[5.8,2.8,5.1,2.4,"virginica"],[6.4,3.2,5.3,2.3,"virginica"],[6.5,3,5.5,1.8,"virginica"],[7.7,3.8,6.7,2.2,"virginica"],[7.7,2.6,6.9,2.3,"virginica"],[6,2.2,5,1.5,"virginica"],[6.9,3.2,5.7,2.3,"virginica"],[5.6,2.8,4.9,2,"virginica"],[7.7,2.8,6.7,2,"virginica"],[6.3,2.7,4.9,1.8,"virginica"],[6.7,3.3,5.7,2.1,"virginica"],[7.2,3.2,6,1.8,"virginica"],[6.2,2.8,4.8,1.8,"virginica"],[6.1,3,4.9,1.8,"virginica"],[6.4,2.8,5.6,2.1,"virginica"],[7.2,3,5.8,1.6,"virginica"],[7.4,2.8,6.1,1.9,"virginica"],[7.9,3.8,6.4,2,"virginica"],[6.4,2.8,5.6,2.2,"virginica"],[6.3,2.8,5.1,1.5,"virginica"],[6.1,2.6,5.6,1.4,"virginica"],[7.7,3,6.1,2.3,"virginica"],[6.3,3.4,5.6,2.4,"virginica"],[6.4,3.1,5.5,1.8,"virginica"],[6,3,4.8,1.8,"virginica"],[6.9,3.1,5.4,2.1,"virginica"],[6.7,3.1,5.6,2.4,"virginica"],[6.9,3.1,5.1,2.3,"virginica"],[5.8,2.7,5.1,1.9,"virginica"],[6.8,3.2,5.9,2.3,"virginica"],[6.7,3.3,5.7,2.5,"virginica"],[6.7,3,5.2,2.3,"virginica"],[6.3,2.5,5,1.9,"virginica"],[6.5,3,5.2,2,"virginica"],[6.2,3.4,5.4,2.3,"virginica"],[5.9,3,5.1,1.8,"virginica"]]},{"transform":{"type":"filter","config":{"dimension":"Species","=":"setosa"}},"id":"setosa"},{"transform":{"type":"filter","config":{"dimension":"Species","=":"versicolor"}},"id":"versicolor"},{"transform":{"type":"filter","config":{"dimension":"Species","=":"virginica"}},"id":"virginica"}],"series":[{"name":"setosa","type":"scatter","datasetIndex":1,"symbolSize":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['Petal.Width']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`Petal.Width`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } vv = vv.map(v => (v == null || isNaN(v)) ? v : v * 7); const c = sprintf(`%@`, vv); return c; }","tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['Petal.Width']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`Petal.Width`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } const c = sprintf(`Petal.Width: %@`, vv); return c; }"}},{"name":"versicolor","type":"scatter","datasetIndex":2,"symbolSize":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['Petal.Width']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`Petal.Width`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } vv = vv.map(v => (v == null || isNaN(v)) ? v : v * 7); const c = sprintf(`%@`, vv); return c; }","tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['Petal.Width']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`Petal.Width`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } const c = sprintf(`Petal.Width: %@`, vv); return c; }"}},{"name":"virginica","type":"scatter","datasetIndex":3,"symbolSize":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['Petal.Width']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`Petal.Width`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } vv = vv.map(v => (v == null || isNaN(v)) ? v : v * 7); const c = sprintf(`%@`, vv); return c; }","tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['Petal.Width']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`Petal.Width`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } const c = sprintf(`Petal.Width: %@`, vv); return c; }"}}],"legend":{"show":true},"xAxis":{"type":"value","name":"Sepal.Length","show":true},"yAxis":{"type":"value","name":"Sepal.Width","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["on.0.handler","opts.series.0.symbolSize","opts.series.0.tooltip.formatter","opts.series.1.symbolSize","opts.series.1.tooltip.formatter","opts.series.2.symbolSize","opts.series.2.tooltip.formatter"],"jsHooks":[]}
data.frame(n=1:5) |> dplyr::group_by(n) |> ec.init(
  title= list(text= "gauge #%@"),
  timeline= list(show=TRUE, autoPlay=TRUE),
  series.param= list(type='gauge', max=5)
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"title":{"text":"gauge #%@"},"timeline":{"show":true,"autoPlay":true,"data":["1","2","3","4","5"],"axisType":"category"},"dataset":[{"dimensions":"n","source":[[1],[2],[3],[4],[5]]},{"transform":{"type":"filter","config":{"dimension":"n","=":1}},"id":1},{"transform":{"type":"filter","config":{"dimension":"n","=":2}},"id":2},{"transform":{"type":"filter","config":{"dimension":"n","=":3}},"id":3},{"transform":{"type":"filter","config":{"dimension":"n","=":4}},"id":4},{"transform":{"type":"filter","config":{"dimension":"n","=":5}},"id":5}],"legend":{"show":true},"options":[{"series":[{"datasetIndex":1,"type":"gauge","max":5,"encode":{"x":null,"y":0}}],"title":{"text":"gauge #1"}},{"series":[{"datasetIndex":2,"type":"gauge","max":5,"encode":{"x":null,"y":0}}],"title":{"text":"gauge #2"}},{"series":[{"datasetIndex":3,"type":"gauge","max":5,"encode":{"x":null,"y":0}}],"title":{"text":"gauge #3"}},{"series":[{"datasetIndex":4,"type":"gauge","max":5,"encode":{"x":null,"y":0}}],"title":{"text":"gauge #4"}},{"series":[{"datasetIndex":5,"type":"gauge","max":5,"encode":{"x":null,"y":0}}],"title":{"text":"gauge #5"}}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
ec.init(
  series.param= list(
    renderItem= 'segmentedDoughnut',   # v.6  from https://github.com/apache/echarts-custom-series
    itemPayload= list(segmentCount= 8, label= list(show=TRUE, formatter= '{c}/{b}', fontSize=35) ),
    data= list(5) )
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"series":[{"type":"custom","id":"ec.auto","renderItem":"segmentedDoughnut","itemPayload":{"segmentCount":8,"label":{"show":true,"formatter":"{c}/{b}","fontSize":35}},"data":[5],"coordinateSystem":"none"}]}},"evals":[],"jsHooks":[]}
ec.init(cars, js= 'confetti();',  # js code executes on init
  load= 'https://cdn.jsdelivr.net/npm/canvas-confetti@1.9.4/dist/confetti.browser.min.js',
  ask= 'loadRemote',
  on= list(list(event= 'click', handler= ec.clmn('() => confetti()')) )
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":"confetti();","dbg":false,"on":[{"event":"click","handler":"() => confetti()"}],"opts":{"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"series":[{"type":"scatter","id":"ec.auto"}],"xAxis":{"type":"value","name":"speed","show":true},"yAxis":{"type":"value","name":"dist","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["on.0.handler"],"jsHooks":[]}
```
