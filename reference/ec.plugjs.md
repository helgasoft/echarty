# Install Javascript plugin from URL source

Install Javascript plugin from URL source

## Usage

``` r
ec.plugjs(wt = NULL, source = NULL, ask = FALSE)
```

## Arguments

- wt:

  A widget to add dependency to, see
  [createWidget](https://rdrr.io/pkg/htmlwidgets/man/createWidget.html)

- source:

  URL or file:// of a Javascript plugin,  
  file name suffix is '.js'. Default is NULL.

- ask:

  Boolean, whether to ask the user to download source if missing,
  default is FALSE.   Could also be string 'loadRemote' to load plugins
  remotely.  

## Value

A widget with JS dependency added if successful, otherwise input wt

## Details

When *source* is URL, the plugin file is installed with an optional
popup prompt.  
When *source* is a file name (file://xxx.js), it is assumed installed
and only a dependency is added.  
When *source* is invalid, an error message will be written in the
chart's title.  
Called internally by
[ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md). It
is recommended to use *ec.init(load=...)* instead of *ec.plugjs*.

## Examples

``` r
# import map plugin and display two (lon,lat) locations
if (interactive()) {
  durl <- paste0('https://raw.githubusercontent.com/apache/echarts/',
           'master/test/data/map/js/china-contour.js')
  ec.init(  # load= durl,
    geo = list(map= 'china-contour', roam= TRUE),
    series.param = list(
      type= 'scatter', coordinateSystem= 'geo',
      symbolSize= 9, itemStyle= list(color= 'red'),
      data= list(list(value= c(113, 40)), list(value= c(118, 39))) )
  ) |> 
  ec.plugjs(durl)
}
```
