# Data helper

Make data lists from a data.frame

## Usage

``` r
ec.data(df, format = "dataset", header = FALSE, ...)
```

## Arguments

- df:

  Required chart data as **data.frame**.  
  For format *dendrogram* df is a **list**, result of
  [hclust](https://rdrr.io/r/stats/hclust.html) function.  
  For format *flame* df is an hierarchical **list** with
  name,value,children.  

- format:

  Output list format  

  - **dataset** = list to be used in
    [dataset](https://echarts.apache.org/en/option.html#dataset.source)
    (default), or in
    [series.data](https://echarts.apache.org/en/option.html#series-scatter.data)
    (without header).  

  - **values** = list for customized
    [series.data](https://echarts.apache.org/en/option.html#series-scatter.data)  

  - **names** = named lists useful for named data like [sankey
    links](https://echarts.apache.org/en/option.html#series-sankey.links).

  - **dendrogram** = build series data for Hierarchical Clustering
    dendrogram

  - **flame** = build series data (lists of name,id,value) for hierarchy
    display by *renderItem*

  - **treePC** = build series data for tree charts from parent/children
    data.frame

  - **treeTT** = build series data for tree charts from data.frame like
    Titanic.

  - **boxplot** = build dataset and source lists, see Details

  - **borders** = build geoJson string from map_data region borders, see
    Details

- header:

  for dataset, to include the column names or not, default TRUE. Set it
  to FALSE for
  [series.data](https://echarts.apache.org/en/option.html#series-scatter.data).  

- ...:

  optional parameters  
  Optional parameters for **boxplot** are:  

  - *layout* = 'h' for horizontal(default) or 'v' for vertical layout  

  - *outliers* boolean to add outlier points (default FALSE)  

  - *jitter* value for [jitter](https://rdrr.io/r/base/jitter.html) of
    numerical values in second column, default 0 (no scatter). Adds
    scatter series on top of boxplot.  

  Optional parameter for **names**:  

  - *nasep* = single character name separator for nested lists, see
    Examples.  
    Purpose is to facilitate conversion from *data.frame* to nested
    named lists.  

  Optional parameter for **flame**:  

  - *name* = name of subtree to search for.  

## Value

A list for *dataset.source*, *series.data* or other lists:  
For boxplot - a named list, see Details and Examples  
For dendrogram, treePC, flame - a tree structure, see format in [tree
data](https://echarts.apache.org/en/option.html#series-tree.data)

## Details

`format='boxplot'` requires the first two *df* columns as:  
   column for the non-computational categorical axis  
   column with (numeric) data to compute the five boxplot values  
Additional grouping is supported on a column after the second. Groups
will show in the legend, if enabled.  
Returns a `list(dataset, series, xAxis, yAxis)` to set params in
[ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md).
Make sure there is enough data for computation, 4+ values per boxplot.  
  
`format='treeTT'` expects data.frame *df* columns
*pathString,value,(optional itemStyle)* for
[FromDataFrameTable](https://rdrr.io/pkg/data.tree/man/as.Node.data.frame.html).  
It will add column 'pct' with value percentage for each node. See
example below.  
  
`format='borders'` expects *df* columns *long,lat,region,subregion* as
in ggplot2::map_data.  
Result to be used as map in
[ec.registerMap](https://helgasoft.github.io/echarty/reference/ec.registerMap.md).
See borders code example in *examples.R*.  
This is a slow version for borders, another very fast one is offered as
echarty extra, see website.  

## See also

some live [code samples](https://rpubs.com/echarty/data-models)

## Examples

``` r
library(dplyr)
ds <- iris |> relocate(Species) |>
   ec.data(format= 'boxplot', jitter= 0.1, layout= 'v')
ec.init(
  dataset= ds$dataset, series= ds$series, xAxis= ds$xAxis, yAxis= ds$yAxis,
  legend= list(show= TRUE), tooltip= list(show= TRUE)
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"dataset":[{"source":[[5.1,4.9,4.7,4.6,5,5.4,4.6,5,4.4,4.9,5.4,4.8,4.8,4.3,5.8,5.7,5.4,5.1,5.7,5.1,5.4,5.1,4.6,5.1,4.8,5,5,5.2,5.2,4.7,4.8,5.4,5.2,5.5,4.9,5,5.5,4.9,4.4,5.1,5,4.5,4.4,5,5.1,4.8,5.1,4.6,5.3,5],[7,6.4,6.9,5.5,6.5,5.7,6.3,4.9,6.6,5.2,5,5.9,6,6.1,5.6,6.7,5.6,5.8,6.2,5.6,5.9,6.1,6.3,6.1,6.4,6.6,6.8,6.7,6,5.7,5.5,5.5,5.8,6,5.4,6,6.7,6.3,5.6,5.5,5.5,6.1,5.8,5,5.6,5.7,5.7,6.2,5.1,5.7],[6.3,5.8,7.1,6.3,6.5,7.6,4.9,7.3,6.7,7.2,6.5,6.4,6.8,5.7,5.8,6.4,6.5,7.7,7.7,6,6.9,5.6,7.7,6.3,6.7,7.2,6.2,6.1,6.4,7.2,7.4,7.9,6.4,6.3,6.1,7.7,6.3,6.4,6,6.9,6.7,6.9,5.8,6.8,6.7,6.7,6.3,6.5,6.2,5.9]]},{"transform":{"type":"boxplot","config":{"itemNameFormatter":"(p) => ['setosa','versicolor','virginica'][p.value]"}}}],"series":[{"type":"boxplot","name":"boxplot","datasetIndex":1,"encode":{"x":null,"y":null,"tooltip":["Low","Q1","Q2","Q3","High"]},"coordinateSystem":"cartesian2d"},{"type":"scatter","jitter":0.1,"layout":"v","name":"setosa","large":true,"z":3,"data":[[0.4161500275135041,5.1],[0.5668666074518114,4.9],[0.5201521772425621,4.7],[0.431441688304767,4.6],[0.4014798882417381,5],[0.4932786994613707,5.4],[0.4995554777327925,4.6],[0.4579534489195794,5],[0.5465763974003494,4.4],[0.5545043022371828,4.9],[0.5749201321508736,5.4],[0.4349881253670901,4.8],[0.4068482665345073,4.8],[0.4640771461650729,4.3],[0.4804656476713717,5.8],[0.4391339669469744,5.7],[0.4807076234836131,5.4],[0.4127322914544493,5.1],[0.4777402626350522,5.7],[0.5951095670461655,5.1],[0.4579784590750933,5.4],[0.5356760854832828,5.1],[0.5470639197621494,4.6],[0.4391913466155529,5.1],[0.5961079349275679,4.8],[0.5483043058309705,5],[0.4102892552502453,5],[0.5060424927156418,5.2],[0.5391647757962346,5.2],[0.5377112006768584,4.7],[0.40624606506899,4.8],[0.4451125069055706,5.4],[0.4601661612279713,5.2],[0.527293122978881,5.5],[0.4958049099426717,4.9],[0.4864342516288161,5],[0.5412867675535381,5.5],[0.5897153152618557,4.9],[0.4360677536111325,4.4],[0.4433799752965569,5.1],[0.5360325835179538,5],[0.4997691221069545,4.5],[0.5283358696848154,4.4],[0.5320568698458373,5],[0.4192048316355795,5.1],[0.5531200327910483,4.8],[0.5539349608588964,5.1],[0.5981424624565989,4.6],[0.5941041805781424,5.3],[0.4778365521226078,5]],"xAxisIndex":1},{"type":"scatter","jitter":0.1,"layout":"v","name":"versicolor","large":true,"z":3,"data":[[1.492237292928621,7],[1.463048350485042,6.4],[1.434935178793967,6.9],[1.506314708152786,5.5],[1.498727403208613,6.5],[1.555861725192517,5.7],[1.440835668565705,6.3],[1.542679455783218,4.9],[1.413043222343549,6.6],[1.4708413597662,5.2],[1.565039884205908,5],[1.45476364903152,5.9],[1.51400899020955,6],[1.467143816128373,6.1],[1.519252557773143,5.6],[1.438303606305271,6.7],[1.589552787551656,5.6],[1.508496081735939,5.8],[1.508920678682625,6.2],[1.455719430791214,5.6],[1.489340493828058,5.9],[1.474302236875519,6.1],[1.405612194864079,6.3],[1.493197438167408,6.1],[1.478006277466193,6.4],[1.404013043548912,6.6],[1.475394185539335,6.8],[1.511982567980886,6.7],[1.571416717208922,6],[1.476961942203343,5.7],[1.505583407124504,5.5],[1.520127504738048,5.5],[1.452274271659553,5.8],[1.458010032307357,6],[1.496015034802258,5.4],[1.584001109236851,6],[1.48014403693378,6.7],[1.44263454223983,6.3],[1.534353363141417,5.6],[1.411722822207957,5.5],[1.599413827061653,5.5],[1.429807093460113,6.1],[1.503711327118799,5.8],[1.569224010920152,5],[1.543653944833204,5.6],[1.44826280400157,5.7],[1.509408673690632,5.7],[1.566960363043472,6.2],[1.405591205088422,5.1],[1.493876859964803,5.7]],"xAxisIndex":1},{"type":"scatter","jitter":0.1,"layout":"v","name":"virginica","large":true,"z":3,"data":[[2.561136006144807,6.3],[2.562810262013227,5.8],[2.480782200396061,7.1],[2.44368620174937,6.3],[2.483672280469909,6.5],[2.533774149557575,7.6],[2.501530056400225,4.9],[2.532071861298755,7.3],[2.502358262753114,6.7],[2.567110487399623,7.2],[2.541756232269108,6.5],[2.574841188173741,6.4],[2.402295907586813,6.8],[2.577649913588539,5.7],[2.599269383773207,5.8],[2.50003830017522,6.4],[2.47179340487346,6.5],[2.554982604458928,7.7],[2.516895050182939,7.7],[2.526795274205506,6],[2.571733230957761,6.9],[2.513378867739811,5.6],[2.450599403865636,7.7],[2.583760642912239,6.3],[2.573470040969551,6.7],[2.44970773938112,7.2],[2.480576242366806,6.2],[2.55392603520304,6.1],[2.423897074908018,6.4],[2.438938992330804,7.2],[2.4329138496425,7.4],[2.532641316251829,7.9],[2.571315000904724,6.4],[2.585309289675206,6.3],[2.510475518926978,6.1],[2.515413138875738,7.7],[2.537489549117163,6.3],[2.448943645926192,6.4],[2.408923431672156,6],[2.581970911333337,6.9],[2.414136243844405,6.7],[2.599378294683993,6.9],[2.522370483493432,5.8],[2.434511769143865,6.8],[2.581888193031773,6.7],[2.407490233378485,6.7],[2.518710758071393,6.3],[2.447395510971546,6.5],[2.581259453343228,6.2],[2.563774596806616,5.9]],"xAxisIndex":1}],"xAxis":[{"type":"category","name":"Species","axisLabel":{"formatter":"function(v) { return v;}"}},{"max":3,"show":false}],"yAxis":[{"scale":true,"name":"Sepal.Length"}],"legend":{"show":true},"tooltip":{"show":true}}},"evals":["opts.dataset.1.transform.config.itemNameFormatter","opts.xAxis.0.axisLabel.formatter"],"jsHooks":[]}
hc <- hclust(dist(USArrests), "complete")
ec.init(preset= FALSE,
  series= list(list(
    type= 'tree', orient= 'TB', roam= TRUE, initialTreeDepth= -1,
    data= ec.data(hc, format='dendrogram'),
    layout= 'radial', # symbolSize= ec.clmn(scale= 0.33),
    ## exclude added labels like 'pXX', leaving only the originals
    label= list(formatter= htmlwidgets::JS(
      "function(n) { out= /p\\d+/.test(n.name) ? '' : n.name; return out;}"))
  ))
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"series":[{"type":"tree","orient":"TB","roam":true,"initialTreeDepth":-1,"data":[{"name":"p49","value":1,"children":[{"name":"p47","value":293.62,"children":[{"name":"p39","value":102.86,"children":[{"name":"Florida","value":38.53},{"name":"North Carolina","value":38.53}]},{"name":"p44","value":102.86,"children":[{"name":"p41","value":64.98999999999999,"children":[{"name":"p23","value":48.73,"children":[{"name":"Delaware","value":16.98},{"name":"p20","value":16.98,"children":[{"name":"Alabama","value":15.45},{"name":"Louisiana","value":15.45}]}]},{"name":"p32","value":48.73,"children":[{"name":"Alaska","value":28.64},{"name":"p27","value":28.64,"children":[{"name":"Mississippi","value":21.17},{"name":"South Carolina","value":21.17}]}]}]},{"name":"p42","value":64.98999999999999,"children":[{"name":"p22","value":53.59,"children":[{"name":"Maryland","value":15.89},{"name":"p17","value":15.89,"children":[{"name":"Arizona","value":13.9},{"name":"New Mexico","value":13.9}]}]},{"name":"p36","value":53.59,"children":[{"name":"California","value":32.72},{"name":"p28","value":32.72,"children":[{"name":"p4","value":22.37,"children":[{"name":"Illinois","value":6.24},{"name":"New York","value":6.24}]},{"name":"p15","value":22.37,"children":[{"name":"Michigan","value":13.3},{"name":"Nevada","value":13.3}]}]}]}]}]}]},{"name":"p48","value":293.62,"children":[{"name":"p45","value":168.61,"children":[{"name":"p37","value":68.76000000000001,"children":[{"name":"p30","value":36.73,"children":[{"name":"Missouri","value":24.89},{"name":"p12","value":24.89,"children":[{"name":"Arkansas","value":12.61},{"name":"Tennessee","value":12.61}]}]},{"name":"p31","value":36.73,"children":[{"name":"Georgia","value":25.09},{"name":"p18","value":25.09,"children":[{"name":"Colorado","value":14.5},{"name":"Texas","value":14.5}]}]}]},{"name":"p38","value":68.76000000000001,"children":[{"name":"Rhode Island","value":36.85},{"name":"p33","value":36.85,"children":[{"name":"p21","value":29.25,"children":[{"name":"Wyoming","value":15.63},{"name":"p11","value":15.63,"children":[{"name":"Oregon","value":12.42},{"name":"p6","value":12.42,"children":[{"name":"Oklahoma","value":7.36},{"name":"Virginia","value":7.36}]}]}]},{"name":"p29","value":29.25,"children":[{"name":"Washington","value":22.77},{"name":"p10","value":22.77,"children":[{"name":"Massachusetts","value":11.46},{"name":"New Jersey","value":11.46}]}]}]}]}]},{"name":"p46","value":168.61,"children":[{"name":"p34","value":87.33,"children":[{"name":"p24","value":31.48,"children":[{"name":"p5","value":18.26,"children":[{"name":"Ohio","value":6.64},{"name":"Utah","value":6.64}]},{"name":"p7","value":18.26,"children":[{"name":"Connecticut","value":8.029999999999999},{"name":"Pennsylvania","value":8.029999999999999}]}]},{"name":"p26","value":31.48,"children":[{"name":"p16","value":19.9,"children":[{"name":"Nebraska","value":13.35},{"name":"p2","value":13.35,"children":[{"name":"Kentucky","value":3.83},{"name":"Montana","value":3.83}]}]},{"name":"p19","value":19.9,"children":[{"name":"Idaho","value":15.41},{"name":"p3","value":15.41,"children":[{"name":"Indiana","value":3.93},{"name":"Kansas","value":3.93}]}]}]}]},{"name":"p43","value":87.33,"children":[{"name":"p35","value":57.27,"children":[{"name":"Hawaii","value":31.62},{"name":"p25","value":31.62,"children":[{"name":"Minnesota","value":19.44},{"name":"p9","value":19.44,"children":[{"name":"Wisconsin","value":10.86},{"name":"p1","value":10.86,"children":[{"name":"Iowa","value":2.29},{"name":"New Hampshire","value":2.29}]}]}]}]},{"name":"p40","value":57.27,"children":[{"name":"p13","value":41.49,"children":[{"name":"West Virginia","value":12.78},{"name":"p8","value":12.78,"children":[{"name":"Maine","value":8.539999999999999},{"name":"South Dakota","value":8.539999999999999}]}]},{"name":"p14","value":41.49,"children":[{"name":"North Dakota","value":13.04},{"name":"Vermont","value":13.04}]}]}]}]}]}]}],"layout":"radial","label":{"formatter":"function(n) { out= /p\\d+/.test(n.name) ? '' : n.name; return out;}"}}]}},"evals":["opts.series.0.label.formatter"],"jsHooks":[]}
# build required pathString,value and optional itemStyle columns
df <- as.data.frame(Titanic) |> rename(value= Freq) |> mutate(
  pathString= paste('Titanic\nSurvival', Survived, Age, Sex, Class, sep='/'),
   itemStyle= case_when(Survived=='Yes' ~"color='green'", TRUE ~"color='LightSalmon'")) |>
   select(pathString, value, itemStyle)
ec.init(
    series= list(list(
      data= ec.data(df, format='treeTT'),
      type= 'tree', symbolSize= ec.clmn("(x) => {return Math.log(x)*10}")
    )),
    tooltip= list(formatter= ec.clmn('%@<br>%@%','value','pct'))
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"series":[{"data":[{"name":"Titanic\nSurvival","value":2201,"children":[{"value":1490,"children":[{"value":52,"children":[{"value":35,"children":[{"itemStyle":{"color":"LightSalmon"},"value":0,"name":"1st","pct":0},{"itemStyle":{"color":"LightSalmon"},"value":0,"name":"2nd","pct":0},{"itemStyle":{"color":"LightSalmon"},"value":35,"name":"3rd","pct":100},{"itemStyle":{"color":"LightSalmon"},"value":0,"name":"Crew","pct":0}],"name":"Male","pct":67.31,"itemStyle":{"color":"LightSalmon"}},{"value":17,"children":[{"itemStyle":{"color":"LightSalmon"},"value":0,"name":"1st","pct":0},{"itemStyle":{"color":"LightSalmon"},"value":0,"name":"2nd","pct":0},{"itemStyle":{"color":"LightSalmon"},"value":17,"name":"3rd","pct":100},{"itemStyle":{"color":"LightSalmon"},"value":0,"name":"Crew","pct":0}],"name":"Female","pct":32.69,"itemStyle":{"color":"LightSalmon"}}],"name":"Child","pct":3.49,"itemStyle":{"color":"LightSalmon"}},{"value":1438,"children":[{"value":1329,"children":[{"itemStyle":{"color":"LightSalmon"},"value":118,"name":"1st","pct":8.880000000000001},{"itemStyle":{"color":"LightSalmon"},"value":154,"name":"2nd","pct":11.59},{"itemStyle":{"color":"LightSalmon"},"value":387,"name":"3rd","pct":29.12},{"itemStyle":{"color":"LightSalmon"},"value":670,"name":"Crew","pct":50.41}],"name":"Male","pct":92.42,"itemStyle":{"color":"LightSalmon"}},{"value":109,"children":[{"itemStyle":{"color":"LightSalmon"},"value":4,"name":"1st","pct":3.67},{"itemStyle":{"color":"LightSalmon"},"value":13,"name":"2nd","pct":11.93},{"itemStyle":{"color":"LightSalmon"},"value":89,"name":"3rd","pct":81.65000000000001},{"itemStyle":{"color":"LightSalmon"},"value":3,"name":"Crew","pct":2.75}],"name":"Female","pct":7.58,"itemStyle":{"color":"LightSalmon"}}],"name":"Adult","pct":96.51000000000001,"itemStyle":{"color":"LightSalmon"}}],"name":"No","pct":67.7,"itemStyle":{"color":"LightSalmon"}},{"value":711,"children":[{"value":57,"children":[{"value":29,"children":[{"itemStyle":{"color":"green"},"value":5,"name":"1st","pct":17.24},{"itemStyle":{"color":"green"},"value":11,"name":"2nd","pct":37.93},{"itemStyle":{"color":"green"},"value":13,"name":"3rd","pct":44.83},{"itemStyle":{"color":"green"},"value":0,"name":"Crew","pct":0}],"name":"Male","pct":50.88,"itemStyle":{"color":"green"}},{"value":28,"children":[{"itemStyle":{"color":"green"},"value":1,"name":"1st","pct":3.57},{"itemStyle":{"color":"green"},"value":13,"name":"2nd","pct":46.43},{"itemStyle":{"color":"green"},"value":14,"name":"3rd","pct":50},{"itemStyle":{"color":"green"},"value":0,"name":"Crew","pct":0}],"name":"Female","pct":49.12,"itemStyle":{"color":"green"}}],"name":"Child","pct":8.02,"itemStyle":{"color":"green"}},{"value":654,"children":[{"value":338,"children":[{"itemStyle":{"color":"green"},"value":57,"name":"1st","pct":16.86},{"itemStyle":{"color":"green"},"value":14,"name":"2nd","pct":4.14},{"itemStyle":{"color":"green"},"value":75,"name":"3rd","pct":22.19},{"itemStyle":{"color":"green"},"value":192,"name":"Crew","pct":56.8}],"name":"Male","pct":51.68,"itemStyle":{"color":"green"}},{"value":316,"children":[{"itemStyle":{"color":"green"},"value":140,"name":"1st","pct":44.3},{"itemStyle":{"color":"green"},"value":80,"name":"2nd","pct":25.32},{"itemStyle":{"color":"green"},"value":76,"name":"3rd","pct":24.05},{"itemStyle":{"color":"green"},"value":20,"name":"Crew","pct":6.33}],"name":"Female","pct":48.32,"itemStyle":{"color":"green"}}],"name":"Adult","pct":91.98,"itemStyle":{"color":"green"}}],"name":"Yes","pct":32.3,"itemStyle":{"color":"green"}}],"itemStyle":{"color":"LightSalmon"}}],"type":"tree","symbolSize":"(x) => {return Math.log(x)*10}"}],"tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['pathString','value','itemStyle']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['value'], data['pct']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`value`,`pct`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } const c = sprintf(`%@<br>%@%`, vv); return c; }"}}},"evals":["opts.series.0.symbolSize","opts.tooltip.formatter"],"jsHooks":[]}
# column itemStyle_color will become itemStyle= list(color=...) in data list
# attribute names separator (nasep) is "_"
df <- data.frame(name= c('A','B','C'), value= c(1,2,3), 
     itemStyle_color= c('chartreuse','lightblue','pink'),
     itemStyle_decal_symbol= c('rect','diamond','none'),
     emphasis_itemStyle_color= c('darkgreen','blue','red')
)
ec.init(series.param= list(type='pie', data= ec.data(df, 'names', nasep='_')))

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"series":[{"id":"ec.auto","type":"pie","data":[{"name":"A","value":1,"itemStyle":{"color":"chartreuse","decal":{"symbol":"rect"}},"emphasis":{"itemStyle":{"color":"darkgreen"}}},{"name":"B","value":2,"itemStyle":{"color":"lightblue","decal":{"symbol":"diamond"}},"emphasis":{"itemStyle":{"color":"blue"}}},{"name":"C","value":3,"itemStyle":{"color":"pink","decal":{"symbol":"none"}},"emphasis":{"itemStyle":{"color":"red"}}}]}]}},"evals":[],"jsHooks":[]}
```
