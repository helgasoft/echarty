# Shiny: UI chart

Placeholder for a chart in Shiny UI

## Usage

``` r
ecs.output(outputId, width = "100%", height = "400px")
```

## Arguments

- outputId:

  Name of output UI element.

- width, height:

  Must be a valid CSS unit (like `'100%'`, `'400px'`, *'auto'*) or a
  number, which will be coerced to a string and have *'px'* appended.

## Value

An output or render function that enables the use of the widget within
Shiny applications.

## See also

[ecs.exec](https://helgasoft.github.io/echarty/reference/ecs.exec.md)
for example,
[shinyWidgetOutput](https://rdrr.io/pkg/htmlwidgets/man/htmlwidgets-shiny.html)
for return value.
