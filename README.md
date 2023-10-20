
<!-- README.md is generated from README.Rmd. Please edit that file -->

# <img src="man/figs/logo.png" width='70px' alt="" /> echarty

<!-- badges: start -->

[![R-CMD-check](https://github.com/helgasoft/echarty/workflows/R-CMD-check/badge.svg)](https://github.com/helgasoft/echarty/actions)
[![Coveralls test coverage](https://coveralls.io/repos/github/helgasoft/echarty/badge.svg)](https://coveralls.io/r/helgasoft/echarty?branch=main)
[![size](https://img.shields.io/github/languages/code-size/helgasoft/echarty)](https://github.com/helgasoft/echarty/releases/)
[![website](https://img.shields.io/badge/Website-Visit-blue)](https://helgasoft.github.io/echarty)
<!--
[![CRAN
downloads](https://cranlogs.r-pkg.org/badges/last-day/echarty)](https://cranlogs.r-pkg.org/badges/last-day/echarty)   -->

<!-- badges: end -->

<a href='https://helgasoft.github.io/echarty'><img src="man/figs/echarty.gallery.png" alt="echarty.gallery" /></a>

This package is a thin R wrapper around Javascript library
[ECharts](https://echarts.apache.org/en/index.html).  
**One** major command(_ec.init_) uses R lists to support the [ECharts API](https://echarts.apache.org/en/option.html).  
Benefit from ECharts **full functionality** and build
interactive charts in R and Shiny with minimal overhead.  

Wider connectivity and deployment potential through [crosstalk](https://rpubs.com/echarty/crosstalk).  

<details> <summary><strong>Compare to echarts4r</strong></summary>

R package | echarts4r | echarty
--- | --- | ---
initial commit | Mar 12, 2018 | Feb 5, 2021
library size | ![878 KB](https://img.shields.io/github/languages/code-size/JohnCoene/echarts4r.svg) | ![224KB](https://img.shields.io/github/languages/code-size/helgasoft/echarty)
test coverage | ![32%](https://coveralls.io/repos/github/JohnCoene/echarts4r/badge.svg) [![link](man/figs/external-link-16.png)](https://coveralls.io/github/JohnCoene/echarts4r) | ![93%](https://coveralls.io/repos/github/helgasoft/echarty/badge.svg) [![link](man/figs/external-link-16.png)](https://coveralls.io/github/helgasoft/echarty)
lines of code | 1,171,938 [![link](man/figs/external-link-16.png)](https://api.codetabs.com/v1/loc/?github=JohnCoene/echarts4r)| 5,061 [![link](man/figs/external-link-16.png)](https://api.codetabs.com/v1/loc?github=helgasoft/echarty)
API design <sup>(1)</sup>| own commands with parameters | mostly [ECharts option](https://echarts.apache.org/en/option.html) lists
number of commands | over [200](https://echarts4r.john-coene.com/reference/) | **one** command + optional utilities
data storage support | series data | **[datasets](https://echarts.apache.org/en/option.html#dataset)**, series data
[crosstalk](https://rstudio.github.io/crosstalk/) support | no	 | **yes**
utilities | bezier, correlations, histogram, density, loess, flip, nesting, more | extended boxplots, tabsets, layouts, shapefiles, lotties, more

This review done Sept 2023 for echarts4R v.0.4.5 and echarty v.1.5.4.03.

(1) We encourage users to follow the original ECharts documentation to construct charts with echarty. 
	This differs from echarts4r which uses own commands for most chart options.   
</details>
  <br />
Please consider granting a Github star ⭐ to show your support.  

## Installation

<!-- [![Github version](https://img.shields.io/github/v/release/helgasoft/echarty?label=github)](https://github.com/helgasoft/echarty/releases)  <sup>.02</sup>  -->
Latest development build <strong>1.6.2.01</strong>

``` r
if (!requireNamespace('remotes')) install.packages('remotes')
remotes::install_github('helgasoft/echarty')
```

[![CRAN
status](https://www.r-pkg.org/badges/version/echarty)](https://cran.r-project.org/package=echarty) 
From [CRAN](https://CRAN.R-project.org):

``` r
install.packages('echarty')
```

## Examples

``` r
library(echarty); library(dplyr)

#  scatter chart (default)
cars |> ec.init()

#  parallel chart
ToothGrowth |> ec.init(ctype= 'parallel')

#  3D chart with GL plugin
iris |> group_by(Species) |> ec.init(load= '3D')

#  timeline of two series with grouping, formatting, autoPlay
iris |> group_by(Species) |> 
ec.init(
	timeline= list(autoPlay= TRUE),
  tl.series= list(
    symbolSize= ec.clmn('Petal.Width', scale= 3),
    encode= list(y= c('Sepal.Width', 'Petal.Length')),
    markLine= list(data= list(list(type='max'), list(type='min')))
  )
)

# show a remote chart
echarty::ec.fromJson('https://helgasoft.github.io/echarty/test/pfull.json')

```

## Get started

The [**WEBSITE**](https://helgasoft.github.io/echarty) has a gallery with code and tutorials.  
<br /> The package has plenty of [**code
examples**](https://github.com/helgasoft/echarty/blob/main/R/examples.R)
included. Type
**?ec.examples**, then copy/paste any code to
see the result.  

Now you can start building [**beautiful
ECharts**](https://echarts.apache.org/examples/en/index.html) (and
[**more**](https://www.makeapie.cn/echarts)) with R and Shiny!

<br />
<p align="center">
<a href='https://helgasoft.github.io/echarty/gallery.html' target='_blank'>
<img src="man/figs/ssPolarStack.png" alt="Polar Stack" width="180"/>
<img src="man/figs/ssBars.gif"/>
<img src="man/figs/ssThemeRiver.png" width="180"/>
<img src="man/figs/ssBunny.gif"/> <br />
<!-- img src="man/figs/ssMorph.gif" width="180"/ -->
<img src="man/figs/ssRose.png" width="180"/>
<img src="man/figs/ssSpeed.png" width="180"/>
<img src="man/figs/ssStackBar.png" width="180"/>
</a> 
<br />Made with echarty. Powered by ECharts.
</p>
