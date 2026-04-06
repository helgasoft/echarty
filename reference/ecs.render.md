# Shiny: Plot command to render chart

This is the initial rendering of a chart in the UI.

## Usage

``` r
ecs.render(wt, env = parent.frame(), quoted = FALSE)
```

## Arguments

- wt:

  An `echarty` widget to generate the chart.

- env:

  The environment in which to evaluate `expr`.

- quoted:

  Is `expr` a quoted expression? default FALSE.

## Value

An output or render function that enables the use of the widget within
Shiny applications.

## See also

[ecs.exec](https://helgasoft.github.io/echarty/reference/ecs.exec.md)
for example,
[shinyRenderWidget](https://rdrr.io/pkg/htmlwidgets/man/htmlwidgets-shiny.html)
for return value.
