# Themes

Apply a pre-built or custom coded theme to a chart

## Usage

``` r
ec.theme(wt, name = "custom", code = NULL)
```

## Arguments

- wt:

  Required `echarty` widget as returned by
  [ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md)

- name:

  Name of existing theme file (without extension), or name of custom
  theme defined in `code`.

- code:

  Custom theme as JSON formatted string, default NULL.

## Value

An `echarty` widget.

## Details

Just a few built-in themes are included in folder `inst/themes`.  
Their names are dark, gray, jazz, dark-mushroom and macarons.  
The entire ECharts theme collection could be found
[here](https://github.com/apache/echarts/tree/master/theme) and files
copied if needed.  
To create custom themes or view predefined ones, visit
[theme-builder](https://echarts.apache.org/en/theme-builder.html).  
See also alternative *registerTheme* in
[ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md).

## Examples

``` r
mtcars |> ec.init() |> ec.theme('dark-mushroom')

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"],"source":[[21,6,160,110,3.9,2.62,16.46,0,1,4,4],[21,6,160,110,3.9,2.875,17.02,0,1,4,4],[22.8,4,108,93,3.85,2.32,18.61,1,1,4,1],[21.4,6,258,110,3.08,3.215,19.44,1,0,3,1],[18.7,8,360,175,3.15,3.44,17.02,0,0,3,2],[18.1,6,225,105,2.76,3.46,20.22,1,0,3,1],[14.3,8,360,245,3.21,3.57,15.84,0,0,3,4],[24.4,4,146.7,62,3.69,3.19,20,1,0,4,2],[22.8,4,140.8,95,3.92,3.15,22.9,1,0,4,2],[19.2,6,167.6,123,3.92,3.44,18.3,1,0,4,4],[17.8,6,167.6,123,3.92,3.44,18.9,1,0,4,4],[16.4,8,275.8,180,3.07,4.07,17.4,0,0,3,3],[17.3,8,275.8,180,3.07,3.73,17.6,0,0,3,3],[15.2,8,275.8,180,3.07,3.78,18,0,0,3,3],[10.4,8,472,205,2.93,5.25,17.98,0,0,3,4],[10.4,8,460,215,3,5.424,17.82,0,0,3,4],[14.7,8,440,230,3.23,5.345,17.42,0,0,3,4],[32.4,4,78.7,66,4.08,2.2,19.47,1,1,4,1],[30.4,4,75.7,52,4.93,1.615,18.52,1,1,4,2],[33.9,4,71.09999999999999,65,4.22,1.835,19.9,1,1,4,1],[21.5,4,120.1,97,3.7,2.465,20.01,1,0,3,1],[15.5,8,318,150,2.76,3.52,16.87,0,0,3,2],[15.2,8,304,150,3.15,3.435,17.3,0,0,3,2],[13.3,8,350,245,3.73,3.84,15.41,0,0,3,4],[19.2,8,400,175,3.08,3.845,17.05,0,0,3,2],[27.3,4,79,66,4.08,1.935,18.9,1,1,4,1],[26,4,120.3,91,4.43,2.14,16.7,0,1,5,2],[30.4,4,95.09999999999999,113,3.77,1.513,16.9,1,1,5,2],[15.8,8,351,264,4.22,3.17,14.5,0,1,5,4],[19.7,6,145,175,3.62,2.77,15.5,0,1,5,6],[15,8,301,335,3.54,3.57,14.6,0,1,5,8],[21.4,4,121,109,4.11,2.78,18.6,1,1,4,2]]}],"series":[{"type":"scatter","id":"ec.auto"}],"xAxis":{"type":"value","name":"mpg","show":true},"yAxis":{"type":"value","name":"cyl","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null},"theme":"dark-mushroom"},"evals":[],"jsHooks":[]}cars |> ec.init() |> ec.theme('mine', code=
  '{"color": ["green","#eeaa33"], "backgroundColor": "lemonchiffon"}')

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"series":[{"type":"scatter","id":"ec.auto"}],"xAxis":{"type":"value","name":"speed","show":true},"yAxis":{"type":"value","name":"dist","show":true}},"settings":{"crosstalk_key":null,"crosstalk_group":null},"theme":"mine","themeCode":"{\"color\": [\"green\",\"#eeaa33\"], \"backgroundColor\": \"lemonchiffon\"}"},"evals":[],"jsHooks":[]}
```
