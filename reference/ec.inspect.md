# Chart to JSON

Convert chart to JSON string

## Usage

``` r
ec.inspect(wt, target = "opts", ...)
```

## Arguments

- wt:

  An `echarty` widget as returned by
  [ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md)

- target:

  type of resulting value:  
     'opts' - the htmlwidget *options* as JSON (default)  
     'full' - the *entire* htmlwidget as JSON  
     'data' - info about chart's embedded data (char vector)

- ...:

  Additional attributes to pass to
  [toJSON](https://jeroen.r-universe.dev/jsonlite/reference/fromJSON.html)  
  'file' - optional file name to save to when target='full'  

## Value

A JSON string, except when `target` is 'data' - then a character vector.

## Details

Must be invoked or chained as last command.  
target='full' will export all JavaScript custom code, ready to be used
on import.  
See also
[ec.fromJson](https://helgasoft.github.io/echarty/reference/ec.fromJson.md).

## Examples

``` r
# extract JSON
json <- cars |> ec.init() |> ec.inspect()
json
#> {
#>   "dataset": [
#>     {
#>       "dimensions": ["speed", "dist"],
#>       "source": [
#>         [
#>           4,
#>           2
#>         ],
#>         [
#>           4,
#>           10
#>         ],
#>         [
#>           7,
#>           4
#>         ],
#>         [
#>           7,
#>           22
#>         ],
#>         [
#>           8,
#>           16
#>         ],
#>         [
#>           9,
#>           10
#>         ],
#>         [
#>           10,
#>           18
#>         ],
#>         [
#>           10,
#>           26
#>         ],
#>         [
#>           10,
#>           34
#>         ],
#>         [
#>           11,
#>           17
#>         ],
#>         [
#>           11,
#>           28
#>         ],
#>         [
#>           12,
#>           14
#>         ],
#>         [
#>           12,
#>           20
#>         ],
#>         [
#>           12,
#>           24
#>         ],
#>         [
#>           12,
#>           28
#>         ],
#>         [
#>           13,
#>           26
#>         ],
#>         [
#>           13,
#>           34
#>         ],
#>         [
#>           13,
#>           34
#>         ],
#>         [
#>           13,
#>           46
#>         ],
#>         [
#>           14,
#>           26
#>         ],
#>         [
#>           14,
#>           36
#>         ],
#>         [
#>           14,
#>           60
#>         ],
#>         [
#>           14,
#>           80
#>         ],
#>         [
#>           15,
#>           20
#>         ],
#>         [
#>           15,
#>           26
#>         ],
#>         [
#>           15,
#>           54
#>         ],
#>         [
#>           16,
#>           32
#>         ],
#>         [
#>           16,
#>           40
#>         ],
#>         [
#>           17,
#>           32
#>         ],
#>         [
#>           17,
#>           40
#>         ],
#>         [
#>           17,
#>           50
#>         ],
#>         [
#>           18,
#>           42
#>         ],
#>         [
#>           18,
#>           56
#>         ],
#>         [
#>           18,
#>           76
#>         ],
#>         [
#>           18,
#>           84
#>         ],
#>         [
#>           19,
#>           36
#>         ],
#>         [
#>           19,
#>           46
#>         ],
#>         [
#>           19,
#>           68
#>         ],
#>         [
#>           20,
#>           32
#>         ],
#>         [
#>           20,
#>           48
#>         ],
#>         [
#>           20,
#>           52
#>         ],
#>         [
#>           20,
#>           56
#>         ],
#>         [
#>           20,
#>           64
#>         ],
#>         [
#>           22,
#>           66
#>         ],
#>         [
#>           23,
#>           54
#>         ],
#>         [
#>           24,
#>           70
#>         ],
#>         [
#>           24,
#>           92
#>         ],
#>         [
#>           24,
#>           93
#>         ],
#>         [
#>           24,
#>           120
#>         ],
#>         [
#>           25,
#>           85
#>         ]
#>       ]
#>     }
#>   ],
#>   "series": [
#>     {
#>       "type": "scatter",
#>       "id": "ec.auto"
#>     }
#>   ],
#>   "xAxis": {
#>     "type": "value",
#>     "name": "speed",
#>     "show": true
#>   },
#>   "yAxis": {
#>     "type": "value",
#>     "name": "dist",
#>     "show": true
#>   }
#> } 

# get from JSON and modify plot
ec.fromJson(json) |> ec.theme('macarons')

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"series":[{"type":"scatter","id":"ec.auto"}],"xAxis":{"type":"value","name":"speed","show":true},"yAxis":{"type":"value","name":"dist","show":true}},"theme":"macarons"},"evals":[],"jsHooks":[]}
```
