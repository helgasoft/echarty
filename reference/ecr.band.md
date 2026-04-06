# Area band

A 'custom' serie with lower and upper boundaries

## Usage

``` r
ecr.band(df = NULL, lower = NULL, upper = NULL, type = "polygon", ...)
```

## Arguments

- df:

  A data.frame with lower and upper numerical columns and first column
  with X coordinates.

- lower:

  The column name of band's lower boundary (string).

- upper:

  The column name of band's upper boundary (string).

- type:

  Type of rendering

  - 'polygon' - by drawing a polygon as polyline from upper/lower points
    (default)

  - 'stack' - by two [stacked
    lines](https://echarts.apache.org/en/option.html#series-line.stack)

- ...:

  More attributes for
  [serie](https://echarts.apache.org/en/option.html#series-line.type)

## Value

A list of **one serie** when type='polygon', or list of **two series**
when type='stack'

## Details

- type='polygon': coordinates of the two boundaries are chained into one
  polygon.  
    *xAxis type* could be 'category' or 'value'.  
    Set fill color with attribute *color*.

- type='stack': two *stacked* lines are drawn, the lower with
  customizable areaStyle.  
    *xAxis type* should be 'category' !  
    Set fill color with attribute *areaStyle\$color*.  
    Optional tooltip formatter available in *band\[\[1\]\]\$tipFmt*.

Optional parameter *name*, if given, will show up in legend. Legend
merges all series with same name into one item.

## Examples

``` r
set.seed(222)
df <- data.frame( x = 1:10, y = round(runif(10, 5, 10),2)) |>
  dplyr::mutate(lwr= round(y-runif(10, 1, 3),2), upr= round(y+runif(10, 2, 4),2) )
banda <- ecr.band(df, 'lwr', 'upr', type='stack', name='stak', areaStyle= list(color='green'))
#banda <- ecr.band(df, 'lwr', 'upr', type='polygon', name='poly1')

df |> ec.init( load='custom', # polygon only
  legend= list(show= TRUE),
  xAxis= list(type='category', boundaryGap=FALSE), # stack
  #xAxis= list(scale=TRUE, min='dataMin'),            # polygon 
  series= append(
    list(list(type='line', color='blue', name='line1')),
    banda
  ),
  tooltip= list(trigger='axis', formatter= banda[[1]]$tipFmt)
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"legend":{"show":true},"xAxis":{"name":"x","type":"category","boundaryGap":false},"series":[{"type":"line","color":"blue","name":"line1"},{"type":"line","name":"stak","stack":"stak","showSymbol":false,"lineStyle":{"width":0},"data":[[1,7.86],[2,4.18],[3,6.28],[4,2.59],[5,6.7],[6,8.68],[7,4.43],[8,6],[9,6.64],[10,4.29]],"tipFmt":"(ss) => { lo=''; hi=''; lin='';\nss.map(o => { nn = o.dimensionNames[1]; vv= o.value[1];\nif (nn==='.s.lo') lo= vv; \nelse if (nn==='.s.hi') hi= vv;\nelse lin= '<br>line <b>'+vv+'<\/b>'; });\nstr='high <b>'+(lo+hi)+'<\/b>'+lin+'<br>low <b>'+lo+'<\/b>'; return str;}","dimensions":["x",".s.lo"]},{"type":"line","name":"stak","areaStyle":{"color":"green"},"stack":"stak","showSymbol":false,"lineStyle":{"width":0},"data":[[1,5.509999999999999,13.37],[2,3.67,7.85],[3,3.409999999999999,9.69],[4,5.43,8.02],[5,5.37,12.07],[6,5.030000000000001,13.71],[7,5.780000000000001,10.21],[8,4.039999999999999,10.04],[9,4.070000000000001,10.71],[10,5.239999999999999,9.529999999999999]],"dimensions":["x",".s.hi",".s.tip"]}],"tooltip":{"trigger":"axis","formatter":"(ss) => { lo=''; hi=''; lin='';\nss.map(o => { nn = o.dimensionNames[1]; vv= o.value[1];\nif (nn==='.s.lo') lo= vv; \nelse if (nn==='.s.hi') hi= vv;\nelse lin= '<br>line <b>'+vv+'<\/b>'; });\nstr='high <b>'+(lo+hi)+'<\/b>'+lin+'<br>low <b>'+lo+'<\/b>'; return str;}"},"dataset":[{"dimensions":["x","y","lwr","upr"],"source":[[1,9.66,7.86,13.37],[2,5.33,4.18,7.85],[3,7.5,6.28,9.69],[4,5,2.59,8.02],[5,9.58,6.7,12.07],[6,9.789999999999999,8.68,13.71],[7,6.76,4.43,10.21],[8,7.1,6,10.04],[9,7.87,6.64,10.71],[10,5.72,4.29,9.529999999999999]]}],"yAxis":{"type":"value","name":"y","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.series.1.tipFmt","opts.tooltip.formatter"],"jsHooks":[]}
```
