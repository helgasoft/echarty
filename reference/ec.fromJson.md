# JSON to chart

Convert JSON string or file to chart

## Usage

``` r
ec.fromJson(txt, ...)
```

## Arguments

- txt:

  Could be one of the following:  
    class *url*, like `url('https://serv.us/cars.txt')`  
    class *file*, like `file('c:/temp/cars.txt','rb')`  
    class *json*, like `ec.inspect(p)`, for options or full  
    class *character*, JSON string with options only, see example
  below  

- ...:

  Any attributes to pass to internal
  [ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md)
  when *txt* is options only

## Value

An *echarty* widget.

## Details

*txt* could be either a list of options (x\$opts) to be set by
[setOption](https://echarts.apache.org/en/api.html#echartsInstance.setOption),  
OR an entire *htmlwidget* generated thru
[ec.inspect](https://helgasoft.github.io/echarty/reference/ec.inspect.md)
with *target='full'*.  
The latter imports all JavaScript functions defined by the user.

## Examples

``` r
txt <- '{
   "xAxis": { "data": ["Mon", "Tue", "Wed"]}, "yAxis": { },
   "series": { "type": "line", "data": [150, 230, 224] } }'
ec.fromJson(txt)  # text json

{"x":{"iniOpts":{"renderer":"canvas","locale":"EN","useDirtyRect":false},"jcode":null,"dbg":false,"opts":{"xAxis":{"data":["Mon","Tue","Wed"]},"yAxis":{},"series":{"type":"line","data":[150,230,224]}}},"evals":[],"jsHooks":[]}# outFile <- 'c:/temp/cars.json'
# cars |> ec.init() |> ec.inspect(target='full', file=outFile)
# ec.fromJson(file(outFile, 'rb'))
# ec.fromJson(url('http://localhost/echarty/cars.json'))

ec.fromJson('https://helgasoft.github.io/echarty/test/pfull.json')
#> Warning: cannot open URL 'https://helgasoft.github.io/echarty/test/pfull.json': HTTP status was '404 Not Found'
#> Error in open.connection(con, "rb"): cannot open the connection to 'https://helgasoft.github.io/echarty/test/pfull.json'
```
