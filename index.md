<meta name="twitter:card" content="summary_large_image">
<meta name="twitter:site" content="@echarty_R">
<meta name="twitter:title" content="echarty - a thin R/Shiny wrapper to ECharts.js">
<meta name="twitter:description" content="Have the full functionality of ECharts in R with minimal overhead!">
<meta name="twitter:image" content="https://helgasoft.github.io/echarty/img/echarty.gallery.300.png">

  

## Introduction

The goal of **echarty** is to provide a minimal interface, a "glue", between R and ECharts, then let your R data lists build the chart. There are utilities included to assist with data preparation (*ec.data*, *js2r*), Shiny with proxy (*ecs.\**) and JS plugins (*ec.plugjs*).  
Have the **full functionality** of [ECharts](https://echarts.apache.org/examples/en/index.html) in R with minimal overhead!   
<br/>  
<!-- 
Building complex charts as data structures is a powerful concept, also simple and easy to use.  As decribed in [this paper](https://doi.org/10.1016/j.visinf.2018.04.011), library ECharts' foundation lays on the "*user-configurable declarative object* **option**". [Option](https://echarts.apache.org/en/option.html) is JSON-like data.  
-->

## Gallery
Enjoy [the gallery](gallery.md) and grab some chart code!  
Best place to get started with **echarty**
<a href='gallery.html'><img src='img/echarty.gallery.png'/></a>
<br /><br />

## Tutorials
More detailed step-by-step examples with tips and tricks.

[Use Case 1](uc1.md) - Building an interactive map with regional data

[Use Case 2](uc2.md) - Like Clockwork

[Use Case 3](uc3.md) - Hierarchical data

[Use Case 4](uc4.md) - Sankey with timeline

[Use Case 5](uc5.html) - Bubbles without troubles with JavaScript - <span style="color:magenta">*live demo*</span>

[Use Case 6](uc6.md) - Grouped, stacked and timelined
<br />

<br/> 

## Crosstalk 
Social media for charts - [drive the action](xtalk.html)  &nbsp; &nbsp; <span style="color:magenta">*live demo*</span>
<br />
<br/>

## Learn by example
See what you can do with echarty and **Shiny**
```r
demo(eshiny, package='echarty')
```
Have fun with **Translator Assistant**  - translate Javascript data to R! 
&nbsp; ECharts has lots of great [examples](https://echarts.apache.org/examples/en/)
. The goal is to facilitate translation of their JSON-like data (*option* object) to R lists. Give it a go with the following R command
```r
demo(js2r, package='echarty')
```
Or browse some interesting <a href='https://gist.github.com/helgasoft'>code gists</a>.
<br/>
<br/>

## Open Source Acknowledgements
 **echarty** was inspired by and benefits some code from [echarts4R](https://github.com/JohnCoene/echarts4r),  
 hat tip also to the pioneers - [recharts](https://github.com/yihui/recharts) and [ECharts2Shiny](https://github.com/XD-DENG/ECharts2Shiny),  
 and cheers for [htmlwidgets](https://github.com/ramnathv/htmlwidgets/) and [ECharts](https://echarts.apache.org/en/) to make it all possible.  
 <br/>
<br/>
<!--
<img src='https://www.r-pkg.org/badges/version/echarty' alt='CRAN' />  <img src='https://cranlogs.r-pkg.org/badges/last-day/echarty' alt='counter'/>
-->
