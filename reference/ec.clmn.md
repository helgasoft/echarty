# Data column format

Helper function to display/format data column(s) by index or name

## Usage

``` r
ec.clmn(col = NULL, ..., scale = 1)
```

## Arguments

- col:

  Can contain one of several types of values:  
    NULL(default) for charts with single values like tree, pie.  
    a single column index(number) or column name(quoted string)  
    a [sprintf](https://rdrr.io/r/base/sprintf.html) string template for
  multiple columns  
    'json' to display tooltip with all available values to choose from  
    'log' to write all values in the JS console (F12) for debugging.  
    a string containing a JS function starting with *'function('* or
  *'(x) =\>'*.  

- ...:

  Comma separated column indexes or names, only when *col* is *sprintf*.
  This allows formatting of multiple columns, as for a tooltip.  

- scale:

  A positive number, multiplier for numeric columns. When scale is 0,
  all numeric values are rounded.

## Value

A JavaScript code string (usually a function) marked as executable, see
[JS](https://rdrr.io/pkg/htmlwidgets/man/JS.html).

## Details

This function is useful for attributes like formatter, color,
symbolSize, label.  
Column indexes are counted in R and start with 1.  
Omit *col* or use index -1 for single values in tree/pie charts,
*axisLabel.formatter* or *valueFormatter*. See
[ec.data](https://helgasoft.github.io/echarty/reference/ec.data.md)
dendrogram example.  
Column indexes are decimals for combo charts with multiple series, see
[ecr.band](https://helgasoft.github.io/echarty/reference/ecr.band.md)
example. The whole number part is the serie index, the decimal part is
the column index inside.  
*col* as sprintf has the same placeholder *%@* for both column indexes
or column names.  
*col* as sprintf can contain double quotes, but not single or
backquotes.  
Template placeholders with formatting:  

- *%@* will display column value as-is.  

- *%L@* will display a number in locale format, like '12,345.09'.  

- *%LR@* rounded number in locale format, like '12,345'.  

- *%R@* rounded number, like '12345'.  

- *%R2@* rounded number, two digits after decimal point.  

- *%M@* marker in series' color.  
  For *trigger='axis'* (multiple series) one can use decimal column
  indexes.  
  See definition above and example below.

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union
tmp <- data.frame(Species = as.vector(unique(iris$Species)),
                  emoji = c('A','B','C'))
df <- iris |> inner_join(tmp)      # add 6th column emoji
#> Joining with `by = join_by(Species)`
df |> group_by(Species) |> ec.init(
  series.param= list(label= list(show= TRUE, formatter= ec.clmn('emoji'))),
  tooltip= list(formatter=
    # with sprintf template + multiple column indexes
    ec.clmn('%M@ species <b>%@</b><br>s.len <b>%@</b><br>s.wid <b>%@</b>', 5,1,2))
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"tooltip":{"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const ss = [4, 0, 1]; let vv = ss.map(e => { if (e < 0) return x.value != null ? x.value : x; const i = Math.floor(e); const raw = x.value != null ? x.value[i] : x.data != null ? x.data[i] : x[i]; return raw !== undefined ? raw : 'no data'; }); vv = ss.map((e, i) => { const v = vv[i]; if (v == null || typeof v !== 'object') return v; const f = Math.round(e % 1 * 10) - 1; return v.value != null ? v.value[f] : null; }); const c = sprintf(`%M@ species <b>%@<\/b><br>s.len <b>%@<\/b><br>s.wid <b>%@<\/b>`, vv); return c; }"},"dataset":[{"dimensions":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species","emoji"],"source":[[5.1,3.5,1.4,0.2,"setosa","A"],[4.9,3,1.4,0.2,"setosa","A"],[4.7,3.2,1.3,0.2,"setosa","A"],[4.6,3.1,1.5,0.2,"setosa","A"],[5,3.6,1.4,0.2,"setosa","A"],[5.4,3.9,1.7,0.4,"setosa","A"],[4.6,3.4,1.4,0.3,"setosa","A"],[5,3.4,1.5,0.2,"setosa","A"],[4.4,2.9,1.4,0.2,"setosa","A"],[4.9,3.1,1.5,0.1,"setosa","A"],[5.4,3.7,1.5,0.2,"setosa","A"],[4.8,3.4,1.6,0.2,"setosa","A"],[4.8,3,1.4,0.1,"setosa","A"],[4.3,3,1.1,0.1,"setosa","A"],[5.8,4,1.2,0.2,"setosa","A"],[5.7,4.4,1.5,0.4,"setosa","A"],[5.4,3.9,1.3,0.4,"setosa","A"],[5.1,3.5,1.4,0.3,"setosa","A"],[5.7,3.8,1.7,0.3,"setosa","A"],[5.1,3.8,1.5,0.3,"setosa","A"],[5.4,3.4,1.7,0.2,"setosa","A"],[5.1,3.7,1.5,0.4,"setosa","A"],[4.6,3.6,1,0.2,"setosa","A"],[5.1,3.3,1.7,0.5,"setosa","A"],[4.8,3.4,1.9,0.2,"setosa","A"],[5,3,1.6,0.2,"setosa","A"],[5,3.4,1.6,0.4,"setosa","A"],[5.2,3.5,1.5,0.2,"setosa","A"],[5.2,3.4,1.4,0.2,"setosa","A"],[4.7,3.2,1.6,0.2,"setosa","A"],[4.8,3.1,1.6,0.2,"setosa","A"],[5.4,3.4,1.5,0.4,"setosa","A"],[5.2,4.1,1.5,0.1,"setosa","A"],[5.5,4.2,1.4,0.2,"setosa","A"],[4.9,3.1,1.5,0.2,"setosa","A"],[5,3.2,1.2,0.2,"setosa","A"],[5.5,3.5,1.3,0.2,"setosa","A"],[4.9,3.6,1.4,0.1,"setosa","A"],[4.4,3,1.3,0.2,"setosa","A"],[5.1,3.4,1.5,0.2,"setosa","A"],[5,3.5,1.3,0.3,"setosa","A"],[4.5,2.3,1.3,0.3,"setosa","A"],[4.4,3.2,1.3,0.2,"setosa","A"],[5,3.5,1.6,0.6,"setosa","A"],[5.1,3.8,1.9,0.4,"setosa","A"],[4.8,3,1.4,0.3,"setosa","A"],[5.1,3.8,1.6,0.2,"setosa","A"],[4.6,3.2,1.4,0.2,"setosa","A"],[5.3,3.7,1.5,0.2,"setosa","A"],[5,3.3,1.4,0.2,"setosa","A"],[7,3.2,4.7,1.4,"versicolor","B"],[6.4,3.2,4.5,1.5,"versicolor","B"],[6.9,3.1,4.9,1.5,"versicolor","B"],[5.5,2.3,4,1.3,"versicolor","B"],[6.5,2.8,4.6,1.5,"versicolor","B"],[5.7,2.8,4.5,1.3,"versicolor","B"],[6.3,3.3,4.7,1.6,"versicolor","B"],[4.9,2.4,3.3,1,"versicolor","B"],[6.6,2.9,4.6,1.3,"versicolor","B"],[5.2,2.7,3.9,1.4,"versicolor","B"],[5,2,3.5,1,"versicolor","B"],[5.9,3,4.2,1.5,"versicolor","B"],[6,2.2,4,1,"versicolor","B"],[6.1,2.9,4.7,1.4,"versicolor","B"],[5.6,2.9,3.6,1.3,"versicolor","B"],[6.7,3.1,4.4,1.4,"versicolor","B"],[5.6,3,4.5,1.5,"versicolor","B"],[5.8,2.7,4.1,1,"versicolor","B"],[6.2,2.2,4.5,1.5,"versicolor","B"],[5.6,2.5,3.9,1.1,"versicolor","B"],[5.9,3.2,4.8,1.8,"versicolor","B"],[6.1,2.8,4,1.3,"versicolor","B"],[6.3,2.5,4.9,1.5,"versicolor","B"],[6.1,2.8,4.7,1.2,"versicolor","B"],[6.4,2.9,4.3,1.3,"versicolor","B"],[6.6,3,4.4,1.4,"versicolor","B"],[6.8,2.8,4.8,1.4,"versicolor","B"],[6.7,3,5,1.7,"versicolor","B"],[6,2.9,4.5,1.5,"versicolor","B"],[5.7,2.6,3.5,1,"versicolor","B"],[5.5,2.4,3.8,1.1,"versicolor","B"],[5.5,2.4,3.7,1,"versicolor","B"],[5.8,2.7,3.9,1.2,"versicolor","B"],[6,2.7,5.1,1.6,"versicolor","B"],[5.4,3,4.5,1.5,"versicolor","B"],[6,3.4,4.5,1.6,"versicolor","B"],[6.7,3.1,4.7,1.5,"versicolor","B"],[6.3,2.3,4.4,1.3,"versicolor","B"],[5.6,3,4.1,1.3,"versicolor","B"],[5.5,2.5,4,1.3,"versicolor","B"],[5.5,2.6,4.4,1.2,"versicolor","B"],[6.1,3,4.6,1.4,"versicolor","B"],[5.8,2.6,4,1.2,"versicolor","B"],[5,2.3,3.3,1,"versicolor","B"],[5.6,2.7,4.2,1.3,"versicolor","B"],[5.7,3,4.2,1.2,"versicolor","B"],[5.7,2.9,4.2,1.3,"versicolor","B"],[6.2,2.9,4.3,1.3,"versicolor","B"],[5.1,2.5,3,1.1,"versicolor","B"],[5.7,2.8,4.1,1.3,"versicolor","B"],[6.3,3.3,6,2.5,"virginica","C"],[5.8,2.7,5.1,1.9,"virginica","C"],[7.1,3,5.9,2.1,"virginica","C"],[6.3,2.9,5.6,1.8,"virginica","C"],[6.5,3,5.8,2.2,"virginica","C"],[7.6,3,6.6,2.1,"virginica","C"],[4.9,2.5,4.5,1.7,"virginica","C"],[7.3,2.9,6.3,1.8,"virginica","C"],[6.7,2.5,5.8,1.8,"virginica","C"],[7.2,3.6,6.1,2.5,"virginica","C"],[6.5,3.2,5.1,2,"virginica","C"],[6.4,2.7,5.3,1.9,"virginica","C"],[6.8,3,5.5,2.1,"virginica","C"],[5.7,2.5,5,2,"virginica","C"],[5.8,2.8,5.1,2.4,"virginica","C"],[6.4,3.2,5.3,2.3,"virginica","C"],[6.5,3,5.5,1.8,"virginica","C"],[7.7,3.8,6.7,2.2,"virginica","C"],[7.7,2.6,6.9,2.3,"virginica","C"],[6,2.2,5,1.5,"virginica","C"],[6.9,3.2,5.7,2.3,"virginica","C"],[5.6,2.8,4.9,2,"virginica","C"],[7.7,2.8,6.7,2,"virginica","C"],[6.3,2.7,4.9,1.8,"virginica","C"],[6.7,3.3,5.7,2.1,"virginica","C"],[7.2,3.2,6,1.8,"virginica","C"],[6.2,2.8,4.8,1.8,"virginica","C"],[6.1,3,4.9,1.8,"virginica","C"],[6.4,2.8,5.6,2.1,"virginica","C"],[7.2,3,5.8,1.6,"virginica","C"],[7.4,2.8,6.1,1.9,"virginica","C"],[7.9,3.8,6.4,2,"virginica","C"],[6.4,2.8,5.6,2.2,"virginica","C"],[6.3,2.8,5.1,1.5,"virginica","C"],[6.1,2.6,5.6,1.4,"virginica","C"],[7.7,3,6.1,2.3,"virginica","C"],[6.3,3.4,5.6,2.4,"virginica","C"],[6.4,3.1,5.5,1.8,"virginica","C"],[6,3,4.8,1.8,"virginica","C"],[6.9,3.1,5.4,2.1,"virginica","C"],[6.7,3.1,5.6,2.4,"virginica","C"],[6.9,3.1,5.1,2.3,"virginica","C"],[5.8,2.7,5.1,1.9,"virginica","C"],[6.8,3.2,5.9,2.3,"virginica","C"],[6.7,3.3,5.7,2.5,"virginica","C"],[6.7,3,5.2,2.3,"virginica","C"],[6.3,2.5,5,1.9,"virginica","C"],[6.5,3,5.2,2,"virginica","C"],[6.2,3.4,5.4,2.3,"virginica","C"],[5.9,3,5.1,1.8,"virginica","C"]]},{"transform":{"type":"filter","config":{"dimension":"Species","=":"setosa"}},"id":"setosa"},{"transform":{"type":"filter","config":{"dimension":"Species","=":"versicolor"}},"id":"versicolor"},{"transform":{"type":"filter","config":{"dimension":"Species","=":"virginica"}},"id":"virginica"}],"series":[{"name":"setosa","type":"scatter","datasetIndex":1,"label":{"show":true,"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species','emoji']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['emoji']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`emoji`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } const c = sprintf(`%@`, vv); return c; }"}},{"name":"versicolor","type":"scatter","datasetIndex":2,"label":{"show":true,"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species','emoji']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['emoji']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`emoji`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } const c = sprintf(`%@`, vv); return c; }"}},{"name":"virginica","type":"scatter","datasetIndex":3,"label":{"show":true,"formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const colNames = ['Sepal.Length','Sepal.Width','Petal.Length','Petal.Width','Species','emoji']; let aa = Array.isArray(x) ? x : x.data; let vv; if (aa && !Array.isArray(aa) && aa instanceof Object) { const keys = Object.keys(aa); if (keys.length === 1 && keys[0] === 'value') aa = aa.value; } if (aa && !Array.isArray(aa) && Object.keys(aa).length > 1) { vv = [data['emoji']]; } else { if (!aa || !aa.length) return 'no data'; const argNames = [`emoji`]; const pos = argNames.map(z => colNames.indexOf(z)); vv = pos.map(p => aa[p]); } const c = sprintf(`%@`, vv); return c; }"}}],"legend":{"show":true},"xAxis":{"type":"value","name":"Sepal.Length","show":true},"yAxis":{"type":"value","name":"Sepal.Width","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.tooltip.formatter","opts.series.0.label.formatter","opts.series.1.label.formatter","opts.series.2.label.formatter"],"jsHooks":[]}
# tooltip decimal indexes work with full data sets (no missing/partial data)
ChickWeight |> mutate(Chick=as.numeric(Chick)) |> filter(Chick>47) |> group_by(Chick) |>
ec.init(
  tooltip= list(trigger='axis', 
                formatter= ec.clmn("48: %@<br>49: %@<br>50: %@", 1.1, 2.1, 3.1)),
  xAxis= list(type='category'), legend= list(formatter= 'Ch.{name}'),
  series.param= list(type='line', encode= list(x='Time', y='weight')),
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"tooltip":{"trigger":"axis","formatter":"function(x) { const sprintf = (tmpl, vals) => { if (tmpl === '%@') return vals[0]; let j = 0; return tmpl.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => { if (m === '%@') return vals[j++]; if (m === '%L@') return Number(vals[j++]).toLocaleString(); if (m === '%LR@') return Math.round(Number(vals[j++])).toLocaleString(); if (m === '%R@') return Math.round(Number(vals[j++])); if (m === '%R2@') return Number(vals[j++]).toFixed(2); if (m === '%M@') return x.marker; }); }; const ss = [0.1, 1.1, 2.1]; let vv = ss.map(e => { if (e < 0) return x.value != null ? x.value : x; const i = Math.floor(e); const raw = x.value != null ? x.value[i] : x.data != null ? x.data[i] : x[i]; return raw !== undefined ? raw : 'no data'; }); vv = ss.map((e, i) => { const v = vv[i]; if (v == null || typeof v !== 'object') return v; const f = Math.round(e % 1 * 10) - 1; return v.value != null ? v.value[f] : null; }); const c = sprintf(`48: %@<br>49: %@<br>50: %@`, vv); return c; }"},"xAxis":{"name":"Time","type":"category"},"legend":{"formatter":"Ch.{name}"},"dataset":[{"dimensions":["weight","Time","Chick","Diet"],"source":[[42,0,49,"4"],[49,2,49,"4"],[63,4,49,"4"],[84,6,49,"4"],[103,8,49,"4"],[126,10,49,"4"],[160,12,49,"4"],[174,14,49,"4"],[204,16,49,"4"],[234,18,49,"4"],[269,20,49,"4"],[281,21,49,"4"],[39,0,50,"4"],[50,2,50,"4"],[62,4,50,"4"],[80,6,50,"4"],[104,8,50,"4"],[125,10,50,"4"],[154,12,50,"4"],[170,14,50,"4"],[222,16,50,"4"],[261,18,50,"4"],[303,20,50,"4"],[322,21,50,"4"],[41,0,48,"4"],[54,2,48,"4"],[67,4,48,"4"],[84,6,48,"4"],[105,8,48,"4"],[122,10,48,"4"],[155,12,48,"4"],[175,14,48,"4"],[205,16,48,"4"],[234,18,48,"4"],[264,20,48,"4"],[264,21,48,"4"]]},{"transform":{"type":"filter","config":{"dimension":"Chick","=":48}},"id":48},{"transform":{"type":"filter","config":{"dimension":"Chick","=":49}},"id":49},{"transform":{"type":"filter","config":{"dimension":"Chick","=":50}},"id":50}],"series":[{"name":"48","datasetIndex":1,"type":"line","encode":{"x":"Time","y":"weight"}},{"name":"49","datasetIndex":2,"type":"line","encode":{"x":"Time","y":"weight"}},{"name":"50","datasetIndex":3,"type":"line","encode":{"x":"Time","y":"weight"}}],"yAxis":{"type":"value","name":"weight","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.tooltip.formatter"],"jsHooks":[]}
```
