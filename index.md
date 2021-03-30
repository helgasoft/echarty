
## Introduction

As decribed in [this paper](https://doi.org/10.1016/j.visinf.2018.04.011), library ECharts' foundation lays on the "*user-configurable declarative object* **option**". [Option](https://echarts.apache.org/en/option.html) is JSON-like data. Building complex charts as data structures is a powerful concept, and also simple and easy-to-use.  
The goal of **echarty** is to provide a minimal interface, a "glue", between R and ECharts, then stay out of the way and let your R data lists do the talking. There are utilities included to assist with data preparation (*ec.data*, *ec.js2r*), Shiny with proxy (*ecs.\**) and JS plugins (*ec.plugjs*).  
Have the **full functionality** of [ECharts](https://echarts.apache.org/examples/en/index.html) in R with minimal overhead!   
<br/>  

## Gallery
Enjoy [the gallery](gallery.md) and grab a chart!  
<br />

## Tutorials
Tips and tricks to help you get started with **echarty**

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
Have fun with **echarty Translator Assistant**  - translate Javascript data to R !  
Javascript library ECharts has lots of great [examples](href="https://echarts.apache.org/examples/en/)
. The goal is to facilitate translation of their JSON-like data (*option* object) to R lists. Give it a go with the following R command:
```r
echarty::ec.js2r()   # echarty v.0.1.3
```
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

