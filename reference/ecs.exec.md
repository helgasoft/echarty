# Shiny: Execute a proxy command

Once chart changes had been made, they need to be sent back to the
widget for display

## Usage

``` r
ecs.exec(proxy, cmd = "p_merge")
```

## Arguments

- proxy:

  A
  [ecs.proxy](https://helgasoft.github.io/echarty/reference/ecs.proxy.md)
  object

- cmd:

  Name of command, default is *p_merge*  
  The proxy commands are:  
  *p_update* - add new series and axes  
  *p_merge* - modify or add series features like style,marks,etc.  
  *p_replace* - replace entire chart  
  *p_del_serie* - delete a serie by index or name  
  *p_del_marks* - delete marks of a serie  
  *p_append_data* - add data to existing series  
  *p_dispatch* - send action commands, see
  [documentation](https://echarts.apache.org/en/api.html#echartsInstance.dispatchAction)

## Value

A proxy object to update the chart.

## See also

[ecs.proxy](https://helgasoft.github.io/echarty/reference/ecs.proxy.md),
[ecs.render](https://helgasoft.github.io/echarty/reference/ecs.render.md),
[ecs.output](https://helgasoft.github.io/echarty/reference/ecs.output.md)  
Read about event handling in – Introduction –, or from
[examples](https://github.com/helgasoft/echarty/blob/main/demo/examples.R).

## Examples

``` r
if (interactive()) {
 # run with  demo(eshiny, package='echarty')
}
```
