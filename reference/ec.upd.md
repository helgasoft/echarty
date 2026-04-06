# Update option lists

Chain commands after ec.init to add or update chart items

## Usage

``` r
ec.upd(wt, ...)
```

## Arguments

- wt:

  An echarty widget

- ...:

  R commands to add/update chart option lists

## Details

*ec.upd* makes changes to a chart already set by
[ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md).  
It should be always piped(chained) after
[ec.init](https://helgasoft.github.io/echarty/reference/ec.init.md).  
All numerical indexes for series,visualMap,etc. are JS-counted starting
at 0.  

## Examples

``` r
library(dplyr)
df <- data.frame(x= 1:30, y= runif(30, 5, 10), cat= sample(LETTERS[1:3],size=30,replace=TRUE)) |>
      mutate(lwr= y-runif(30, 1, 3), upr= y+runif(30, 2, 4))
band.df <- df  |> group_by(cat) |> group_split()
sband <- list()
for(ii in seq_along(band.df))   # build all bands
  sband <- append(sband,
    ecr.band(band.df[[ii]], 'lwr', 'upr', type='stack', smooth=FALSE,
       name= unique(band.df[[ii]]$cat), areaStyle= list(color=c('blue','green','yellow')[ii]))
  )

df |> group_by(cat) |> 
ec.init(load='custom', series.param= list(type='line'), 
        xAxis=list(data=c(0,unique(df$x)), boundaryGap=FALSE) ) |> 
ec.upd({ series <- append(series, sband) })
#> Error in eval(substitute(expr), e): object 'sband' not found
```
