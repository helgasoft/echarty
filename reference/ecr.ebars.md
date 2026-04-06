# Error bars

Custom series to display error-bars for scatter, bar or line series

## Usage

``` r
ecr.ebars(wt, encode = list(x = 1, y = c(2, 3, 4)), hwidth = 6, ...)
```

## Arguments

- wt:

  An echarty widget to add error bars to, see
  [ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md).

- encode:

  Column selection for both axes (x & y) as vectors, see
  [encode](https://echarts.apache.org/en/option.html#series-bar.encode)

- hwidth:

  Half-width of error bar in pixels, default is 6.

- ...:

  More parameters for [custom
  serie](https://echarts.apache.org/en/option.html#series-custom.type)

## Value

A widget with error bars added if successful, otherwise the input widget

## Details

Command should be called after *ec.init* where main series are set.  
*ecr.ebars* are custom series, so *ec.init(load='custom')* is
required.  
Horizontal and vertical layouts supported, just switch *encode* values
*x* and *y* for both for series and ecr.ebars.  
Have own default tooltip format showing *value, high & low*.  
Grouped bar series are supported.  
Non-grouped series could be shown with formatter *riErrBarSimple*
instead of *ecr.ebars*. This is limited to vertical only, see example
below.  
Other limitations:  
  manually add axis type='category' when needed  
  error bars cannot have own name when data is grouped  
  legend select/deselect will not re-position grouped error bars  

## Examples

``` r
library(dplyr)
df <- mtcars |> group_by(cyl,gear) |> summarise(avg.mpg= round(mean(mpg),2)) |>
  mutate(low = round(avg.mpg-cyl*runif(1),2), 
         high= round(avg.mpg+cyl*runif(1),2))
#> `summarise()` has regrouped the output.
#> ℹ Summaries were computed grouped by cyl and gear.
#> ℹ Output is grouped by cyl.
#> ℹ Use `summarise(.groups = "drop_last")` to silence this message.
#> ℹ Use `summarise(.by = c(cyl, gear))` for per-operation grouping
#>   (`?dplyr::dplyr_by`) instead.
ec.init(df, load= 'custom', series.param= list(type='bar'),
      xAxis= list(type='category'), tooltip= list(show=TRUE)) |>
ecr.ebars(encode= list(y=c('avg.mpg','low','high'), x='gear'))

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"xAxis":{"name":"gear","type":"category"},"tooltip":{"show":true},"dataset":[{"dimensions":["cyl","gear","avg.mpg","low","high"],"source":[[4,3,21.5,21.03,23.65],[4,4,26.92,26.45,29.07],[4,5,28.2,27.73,30.35],[6,3,19.75,15.67,22.77],[6,4,19.75,15.67,22.77],[6,5,19.7,15.62,22.72],[8,3,15.05,14.87,22.62],[8,5,15.4,15.22,22.97]]},{"transform":{"type":"filter","config":{"dimension":"cyl","=":4}},"id":4},{"transform":{"type":"filter","config":{"dimension":"cyl","=":6}},"id":6},{"transform":{"type":"filter","config":{"dimension":"cyl","=":8}},"id":8}],"series":[{"name":"4","datasetIndex":1,"type":"bar","encode":{"x":1,"y":2}},{"name":"6","datasetIndex":2,"type":"bar","encode":{"x":1,"y":2}},{"name":"8","datasetIndex":3,"type":"bar","encode":{"x":1,"y":2}},{"type":"custom","datasetIndex":1,"encode":{"y":[2,3,4],"x":1},"renderItem":"riErrBars","name":"4","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6},"tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const ss = [2, 3, 4]; let vv = ss.map(e => { if (e < 0) return x.value != null ? x.value : x; const i = Math.floor(e); const raw = x.value != null ? x.value[i] : x.data != null ? x.data[i] : x[i]; return raw !== undefined ? raw : 'no data'; }); vv = ss.map((e, i) => { const v = vv[i]; if (v == null || typeof v !== 'object') return v; const f = Math.round(e % 1 * 10) - 1; return v.value != null ? v.value[f] : null; }); const c = sprintf(`<br>value <b>%@<\/b> <br>range <b>%@<\/b> to <b>%@<\/b>`, vv); return c; }"}},{"type":"custom","datasetIndex":2,"encode":{"y":[2,3,4],"x":1},"renderItem":"riErrBars","name":"6","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6},"tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const ss = [2, 3, 4]; let vv = ss.map(e => { if (e < 0) return x.value != null ? x.value : x; const i = Math.floor(e); const raw = x.value != null ? x.value[i] : x.data != null ? x.data[i] : x[i]; return raw !== undefined ? raw : 'no data'; }); vv = ss.map((e, i) => { const v = vv[i]; if (v == null || typeof v !== 'object') return v; const f = Math.round(e % 1 * 10) - 1; return v.value != null ? v.value[f] : null; }); const c = sprintf(`<br>value <b>%@<\/b> <br>range <b>%@<\/b> to <b>%@<\/b>`, vv); return c; }"}},{"type":"custom","datasetIndex":3,"encode":{"y":[2,3,4],"x":1},"renderItem":"riErrBars","name":"8","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6},"tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const ss = [2, 3, 4]; let vv = ss.map(e => { if (e < 0) return x.value != null ? x.value : x; const i = Math.floor(e); const raw = x.value != null ? x.value[i] : x.data != null ? x.data[i] : x[i]; return raw !== undefined ? raw : 'no data'; }); vv = ss.map((e, i) => { const v = vv[i]; if (v == null || typeof v !== 'object') return v; const f = Math.round(e % 1 * 10) - 1; return v.value != null ? v.value[f] : null; }); const c = sprintf(`<br>value <b>%@<\/b> <br>range <b>%@<\/b> to <b>%@<\/b>`, vv); return c; }"}}],"legend":{"show":true},"yAxis":{"type":"value","name":"avg.mpg","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.series.3.renderItem","opts.series.3.tooltip.formatter","opts.series.4.renderItem","opts.series.4.tooltip.formatter","opts.series.5.renderItem","opts.series.5.tooltip.formatter"],"jsHooks":[]}#ecr.ebars(encode= list(y=c(3,4,5), x=2))  # ok with data indexes

# same but horizontal
ec.init(df, load= 'custom',
  yAxis= list(type='category'), tooltip= list(show=TRUE),
  series.param= list(type='bar', encode= list(x='avg.mpg', y='gear') )) |>
ecr.ebars(encode= list(x=c('avg.mpg','low','high'), y='gear'))

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"yAxis":{"name":"gear","type":"category"},"tooltip":{"show":true},"dataset":[{"dimensions":["cyl","gear","avg.mpg","low","high"],"source":[[4,3,21.5,21.03,23.65],[4,4,26.92,26.45,29.07],[4,5,28.2,27.73,30.35],[6,3,19.75,15.67,22.77],[6,4,19.75,15.67,22.77],[6,5,19.7,15.62,22.72],[8,3,15.05,14.87,22.62],[8,5,15.4,15.22,22.97]]},{"transform":{"type":"filter","config":{"dimension":"cyl","=":4}},"id":4},{"transform":{"type":"filter","config":{"dimension":"cyl","=":6}},"id":6},{"transform":{"type":"filter","config":{"dimension":"cyl","=":8}},"id":8}],"series":[{"name":"4","datasetIndex":1,"type":"bar","encode":{"x":"avg.mpg","y":"gear"}},{"name":"6","datasetIndex":2,"type":"bar","encode":{"x":"avg.mpg","y":"gear"}},{"name":"8","datasetIndex":3,"type":"bar","encode":{"x":"avg.mpg","y":"gear"}},{"type":"custom","datasetIndex":1,"encode":{"x":[2,3,4],"y":1},"renderItem":"riErrBars","name":"4","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6},"tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const ss = [2, 3, 4]; let vv = ss.map(e => { if (e < 0) return x.value != null ? x.value : x; const i = Math.floor(e); const raw = x.value != null ? x.value[i] : x.data != null ? x.data[i] : x[i]; return raw !== undefined ? raw : 'no data'; }); vv = ss.map((e, i) => { const v = vv[i]; if (v == null || typeof v !== 'object') return v; const f = Math.round(e % 1 * 10) - 1; return v.value != null ? v.value[f] : null; }); const c = sprintf(`<br>value <b>%@<\/b> <br>range <b>%@<\/b> to <b>%@<\/b>`, vv); return c; }"}},{"type":"custom","datasetIndex":2,"encode":{"x":[2,3,4],"y":1},"renderItem":"riErrBars","name":"6","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6},"tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const ss = [2, 3, 4]; let vv = ss.map(e => { if (e < 0) return x.value != null ? x.value : x; const i = Math.floor(e); const raw = x.value != null ? x.value[i] : x.data != null ? x.data[i] : x[i]; return raw !== undefined ? raw : 'no data'; }); vv = ss.map((e, i) => { const v = vv[i]; if (v == null || typeof v !== 'object') return v; const f = Math.round(e % 1 * 10) - 1; return v.value != null ? v.value[f] : null; }); const c = sprintf(`<br>value <b>%@<\/b> <br>range <b>%@<\/b> to <b>%@<\/b>`, vv); return c; }"}},{"type":"custom","datasetIndex":3,"encode":{"x":[2,3,4],"y":1},"renderItem":"riErrBars","name":"8","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6},"tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const ss = [2, 3, 4]; let vv = ss.map(e => { if (e < 0) return x.value != null ? x.value : x; const i = Math.floor(e); const raw = x.value != null ? x.value[i] : x.data != null ? x.data[i] : x[i]; return raw !== undefined ? raw : 'no data'; }); vv = ss.map((e, i) => { const v = vv[i]; if (v == null || typeof v !== 'object') return v; const f = Math.round(e % 1 * 10) - 1; return v.value != null ? v.value[f] : null; }); const c = sprintf(`<br>value <b>%@<\/b> <br>range <b>%@<\/b> to <b>%@<\/b>`, vv); return c; }"}}],"legend":{"show":true},"xAxis":{"type":"value","name":"avg.mpg","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.series.3.renderItem","opts.series.3.tooltip.formatter","opts.series.4.renderItem","opts.series.4.tooltip.formatter","opts.series.5.renderItem","opts.series.5.tooltip.formatter"],"jsHooks":[]}
# ----- riErrBarSimple ------
df <- mtcars |> mutate(name= row.names(mtcars), hi= hp-drat*3, lo= hp+wt*3) |> 
  filter(cyl==4) |> select(name,hp,hi,lo)
ec.init(df, load= 'custom', legend= list(show=TRUE)) |>
ec.upd({ series <- append(series, list(
  list(type= 'custom', name= 'error',
    data= ec.data(df |> select(name,hi,lo)),
    renderItem= htmlwidgets::JS('riErrBarSimple')
  )))
})
#> Error in UseMethod("select"): no applicable method for 'select' applied to an object of class "function"
```
