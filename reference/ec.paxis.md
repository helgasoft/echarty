# Parallel Axis

Build 'parallelAxis' for a parallel chart

## Usage

``` r
ec.paxis(dfwt = NULL, cols = NULL, minmax = TRUE, ...)
```

## Arguments

- dfwt:

  An echarty widget OR a data.frame(regular or grouped)

- cols:

  A string vector with columns names in desired order

- minmax:

  Boolean to add max/min limits or not, default TRUE

- ...:

  Additional attributes for
  [parallelAxis](https://echarts.apache.org/en/option.html#parallelAxis).

## Value

A list, see format in
[parallelAxis](https://echarts.apache.org/en/option.html#parallelAxis).

## Details

This function could be chained to *ec.init* or used with a
*data.frame*  

## Examples

``` r
iris |> dplyr::group_by(Species) |>    # chained
ec.init(series.param= list(type= 'parallel', lineStyle= list(width=3))) |> 
ec.paxis(cols= c('Petal.Length','Petal.Width','Sepal.Width'))

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"],"source":[[5.1,3.5,1.4,0.2,"setosa"],[4.9,3,1.4,0.2,"setosa"],[4.7,3.2,1.3,0.2,"setosa"],[4.6,3.1,1.5,0.2,"setosa"],[5,3.6,1.4,0.2,"setosa"],[5.4,3.9,1.7,0.4,"setosa"],[4.6,3.4,1.4,0.3,"setosa"],[5,3.4,1.5,0.2,"setosa"],[4.4,2.9,1.4,0.2,"setosa"],[4.9,3.1,1.5,0.1,"setosa"],[5.4,3.7,1.5,0.2,"setosa"],[4.8,3.4,1.6,0.2,"setosa"],[4.8,3,1.4,0.1,"setosa"],[4.3,3,1.1,0.1,"setosa"],[5.8,4,1.2,0.2,"setosa"],[5.7,4.4,1.5,0.4,"setosa"],[5.4,3.9,1.3,0.4,"setosa"],[5.1,3.5,1.4,0.3,"setosa"],[5.7,3.8,1.7,0.3,"setosa"],[5.1,3.8,1.5,0.3,"setosa"],[5.4,3.4,1.7,0.2,"setosa"],[5.1,3.7,1.5,0.4,"setosa"],[4.6,3.6,1,0.2,"setosa"],[5.1,3.3,1.7,0.5,"setosa"],[4.8,3.4,1.9,0.2,"setosa"],[5,3,1.6,0.2,"setosa"],[5,3.4,1.6,0.4,"setosa"],[5.2,3.5,1.5,0.2,"setosa"],[5.2,3.4,1.4,0.2,"setosa"],[4.7,3.2,1.6,0.2,"setosa"],[4.8,3.1,1.6,0.2,"setosa"],[5.4,3.4,1.5,0.4,"setosa"],[5.2,4.1,1.5,0.1,"setosa"],[5.5,4.2,1.4,0.2,"setosa"],[4.9,3.1,1.5,0.2,"setosa"],[5,3.2,1.2,0.2,"setosa"],[5.5,3.5,1.3,0.2,"setosa"],[4.9,3.6,1.4,0.1,"setosa"],[4.4,3,1.3,0.2,"setosa"],[5.1,3.4,1.5,0.2,"setosa"],[5,3.5,1.3,0.3,"setosa"],[4.5,2.3,1.3,0.3,"setosa"],[4.4,3.2,1.3,0.2,"setosa"],[5,3.5,1.6,0.6,"setosa"],[5.1,3.8,1.9,0.4,"setosa"],[4.8,3,1.4,0.3,"setosa"],[5.1,3.8,1.6,0.2,"setosa"],[4.6,3.2,1.4,0.2,"setosa"],[5.3,3.7,1.5,0.2,"setosa"],[5,3.3,1.4,0.2,"setosa"],[7,3.2,4.7,1.4,"versicolor"],[6.4,3.2,4.5,1.5,"versicolor"],[6.9,3.1,4.9,1.5,"versicolor"],[5.5,2.3,4,1.3,"versicolor"],[6.5,2.8,4.6,1.5,"versicolor"],[5.7,2.8,4.5,1.3,"versicolor"],[6.3,3.3,4.7,1.6,"versicolor"],[4.9,2.4,3.3,1,"versicolor"],[6.6,2.9,4.6,1.3,"versicolor"],[5.2,2.7,3.9,1.4,"versicolor"],[5,2,3.5,1,"versicolor"],[5.9,3,4.2,1.5,"versicolor"],[6,2.2,4,1,"versicolor"],[6.1,2.9,4.7,1.4,"versicolor"],[5.6,2.9,3.6,1.3,"versicolor"],[6.7,3.1,4.4,1.4,"versicolor"],[5.6,3,4.5,1.5,"versicolor"],[5.8,2.7,4.1,1,"versicolor"],[6.2,2.2,4.5,1.5,"versicolor"],[5.6,2.5,3.9,1.1,"versicolor"],[5.9,3.2,4.8,1.8,"versicolor"],[6.1,2.8,4,1.3,"versicolor"],[6.3,2.5,4.9,1.5,"versicolor"],[6.1,2.8,4.7,1.2,"versicolor"],[6.4,2.9,4.3,1.3,"versicolor"],[6.6,3,4.4,1.4,"versicolor"],[6.8,2.8,4.8,1.4,"versicolor"],[6.7,3,5,1.7,"versicolor"],[6,2.9,4.5,1.5,"versicolor"],[5.7,2.6,3.5,1,"versicolor"],[5.5,2.4,3.8,1.1,"versicolor"],[5.5,2.4,3.7,1,"versicolor"],[5.8,2.7,3.9,1.2,"versicolor"],[6,2.7,5.1,1.6,"versicolor"],[5.4,3,4.5,1.5,"versicolor"],[6,3.4,4.5,1.6,"versicolor"],[6.7,3.1,4.7,1.5,"versicolor"],[6.3,2.3,4.4,1.3,"versicolor"],[5.6,3,4.1,1.3,"versicolor"],[5.5,2.5,4,1.3,"versicolor"],[5.5,2.6,4.4,1.2,"versicolor"],[6.1,3,4.6,1.4,"versicolor"],[5.8,2.6,4,1.2,"versicolor"],[5,2.3,3.3,1,"versicolor"],[5.6,2.7,4.2,1.3,"versicolor"],[5.7,3,4.2,1.2,"versicolor"],[5.7,2.9,4.2,1.3,"versicolor"],[6.2,2.9,4.3,1.3,"versicolor"],[5.1,2.5,3,1.1,"versicolor"],[5.7,2.8,4.1,1.3,"versicolor"],[6.3,3.3,6,2.5,"virginica"],[5.8,2.7,5.1,1.9,"virginica"],[7.1,3,5.9,2.1,"virginica"],[6.3,2.9,5.6,1.8,"virginica"],[6.5,3,5.8,2.2,"virginica"],[7.6,3,6.6,2.1,"virginica"],[4.9,2.5,4.5,1.7,"virginica"],[7.3,2.9,6.3,1.8,"virginica"],[6.7,2.5,5.8,1.8,"virginica"],[7.2,3.6,6.1,2.5,"virginica"],[6.5,3.2,5.1,2,"virginica"],[6.4,2.7,5.3,1.9,"virginica"],[6.8,3,5.5,2.1,"virginica"],[5.7,2.5,5,2,"virginica"],[5.8,2.8,5.1,2.4,"virginica"],[6.4,3.2,5.3,2.3,"virginica"],[6.5,3,5.5,1.8,"virginica"],[7.7,3.8,6.7,2.2,"virginica"],[7.7,2.6,6.9,2.3,"virginica"],[6,2.2,5,1.5,"virginica"],[6.9,3.2,5.7,2.3,"virginica"],[5.6,2.8,4.9,2,"virginica"],[7.7,2.8,6.7,2,"virginica"],[6.3,2.7,4.9,1.8,"virginica"],[6.7,3.3,5.7,2.1,"virginica"],[7.2,3.2,6,1.8,"virginica"],[6.2,2.8,4.8,1.8,"virginica"],[6.1,3,4.9,1.8,"virginica"],[6.4,2.8,5.6,2.1,"virginica"],[7.2,3,5.8,1.6,"virginica"],[7.4,2.8,6.1,1.9,"virginica"],[7.9,3.8,6.4,2,"virginica"],[6.4,2.8,5.6,2.2,"virginica"],[6.3,2.8,5.1,1.5,"virginica"],[6.1,2.6,5.6,1.4,"virginica"],[7.7,3,6.1,2.3,"virginica"],[6.3,3.4,5.6,2.4,"virginica"],[6.4,3.1,5.5,1.8,"virginica"],[6,3,4.8,1.8,"virginica"],[6.9,3.1,5.4,2.1,"virginica"],[6.7,3.1,5.6,2.4,"virginica"],[6.9,3.1,5.1,2.3,"virginica"],[5.8,2.7,5.1,1.9,"virginica"],[6.8,3.2,5.9,2.3,"virginica"],[6.7,3.3,5.7,2.5,"virginica"],[6.7,3,5.2,2.3,"virginica"],[6.3,2.5,5,1.9,"virginica"],[6.5,3,5.2,2,"virginica"],[6.2,3.4,5.4,2.3,"virginica"],[5.9,3,5.1,1.8,"virginica"]]},{"transform":{"type":"filter","config":{"dimension":"Species","=":"setosa"}},"id":"setosa"},{"transform":{"type":"filter","config":{"dimension":"Species","=":"versicolor"}},"id":"versicolor"},{"transform":{"type":"filter","config":{"dimension":"Species","=":"virginica"}},"id":"virginica"}],"series":[{"name":"setosa","datasetIndex":1,"type":"parallel","lineStyle":{"width":3}},{"name":"versicolor","datasetIndex":2,"type":"parallel","lineStyle":{"width":3}},{"name":"virginica","datasetIndex":3,"type":"parallel","lineStyle":{"width":3}}],"legend":{"show":true},"parallelAxis":[{"dim":2,"name":"Petal.Length","max":6.9,"min":1},{"dim":3,"name":"Petal.Width","max":2.5,"min":0.1},{"dim":1,"name":"Sepal.Width","max":4.4,"min":2}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
mtcars |> ec.init( 
   parallelAxis= ec.paxis(mtcars, cols= c('gear','cyl','hp','carb'), nameRotate= 45),
   series.param= list(type= 'parallel', smooth= TRUE)
)

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"parallelAxis":[{"dim":9,"name":"gear","nameRotate":45,"max":5,"min":3},{"dim":1,"name":"cyl","nameRotate":45,"max":8,"min":4},{"dim":3,"name":"hp","nameRotate":45,"max":335,"min":52},{"dim":10,"name":"carb","nameRotate":45,"max":8,"min":1}],"dataset":[{"dimensions":["mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"],"source":[[21,6,160,110,3.9,2.62,16.46,0,1,4,4],[21,6,160,110,3.9,2.875,17.02,0,1,4,4],[22.8,4,108,93,3.85,2.32,18.61,1,1,4,1],[21.4,6,258,110,3.08,3.215,19.44,1,0,3,1],[18.7,8,360,175,3.15,3.44,17.02,0,0,3,2],[18.1,6,225,105,2.76,3.46,20.22,1,0,3,1],[14.3,8,360,245,3.21,3.57,15.84,0,0,3,4],[24.4,4,146.7,62,3.69,3.19,20,1,0,4,2],[22.8,4,140.8,95,3.92,3.15,22.9,1,0,4,2],[19.2,6,167.6,123,3.92,3.44,18.3,1,0,4,4],[17.8,6,167.6,123,3.92,3.44,18.9,1,0,4,4],[16.4,8,275.8,180,3.07,4.07,17.4,0,0,3,3],[17.3,8,275.8,180,3.07,3.73,17.6,0,0,3,3],[15.2,8,275.8,180,3.07,3.78,18,0,0,3,3],[10.4,8,472,205,2.93,5.25,17.98,0,0,3,4],[10.4,8,460,215,3,5.424,17.82,0,0,3,4],[14.7,8,440,230,3.23,5.345,17.42,0,0,3,4],[32.4,4,78.7,66,4.08,2.2,19.47,1,1,4,1],[30.4,4,75.7,52,4.93,1.615,18.52,1,1,4,2],[33.9,4,71.09999999999999,65,4.22,1.835,19.9,1,1,4,1],[21.5,4,120.1,97,3.7,2.465,20.01,1,0,3,1],[15.5,8,318,150,2.76,3.52,16.87,0,0,3,2],[15.2,8,304,150,3.15,3.435,17.3,0,0,3,2],[13.3,8,350,245,3.73,3.84,15.41,0,0,3,4],[19.2,8,400,175,3.08,3.845,17.05,0,0,3,2],[27.3,4,79,66,4.08,1.935,18.9,1,1,4,1],[26,4,120.3,91,4.43,2.14,16.7,0,1,5,2],[30.4,4,95.09999999999999,113,3.77,1.513,16.9,1,1,5,2],[15.8,8,351,264,4.22,3.17,14.5,0,1,5,4],[19.7,6,145,175,3.62,2.77,15.5,0,1,5,6],[15,8,301,335,3.54,3.57,14.6,0,1,5,8],[21.4,4,121,109,4.11,2.78,18.6,1,1,4,2]]}],"series":[{"id":"ec.auto","type":"parallel","smooth":true}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
```
