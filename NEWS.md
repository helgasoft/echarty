# history of echarty package development

## v. 0.2.2

- _ec.init_ default _ask_=FALSE to _load_ without prompts
- _echarty.js_: add 'mouseout' event
- _echarty.js_: move legend to _p_append_data_

## v. 0.2.1

- upgrade to ECharts v.5.1.2
- _ec.plugjs_: new parameter _ask_ to allow/suppress prompts
- _ec.plugjs_: parameter _source_ now accepts 'file://' format

## v. 0.2.0

- removed dependencies for Shiny and crosstalk
- _js2r_ and _eshiny_ are now R demos
- change leaflet plugin source.data to dataset, preset center and zoom
- built with latest R v.4.1.0

## v. 0.1.4

- upgrade to ECharts v.5.1.1
- fixed _p_del_serie_ by seriesName
- expand Shiny demo and moved from ec.examples() to a gist 
- replaced purrr with apply in ec.data, purrr no longer a dependency
- _ec.init_ parameter _group1_ now is also default for ungrouped serie 
- added svg maps, like ```registerMap=list(list(mapName=?, svg=?))```
- new _ec.layout()_, a charts layout helper

## v. 0.1.3

- **crosstalk** support added, send & receive for brush and filter actions
- _ec.timegrp_ added, a helper to build timeline data for a grouped data.frame
- _ec.data_ with additional parameter *header*

## v. 0.1.2

- _ec.data_ now with three format values
- _ec.fromJson_ has now "..." (additional arguments)
- _ecr.band_ has new parameter 'two' for alternative rendering
- error bars added as _ecr.ebars_ with support for grouped data
- more examples in _ec.examples_
- some code optimization

## v. 0.1.1

- added _ec.plugjs_ - install unknown plugins, like JS maps
- added _ec.js2r_ - a JS to R translation assistant, a Shiny app
- _ec.init_ update: one-time install for all plugins except built-in leaflet/custom
- GL merged with 3D as plugin '3D'
- more plugins: liquidfill, world, wordcloud, gmodular
- ec.sband renamed to _ec.band_

## v. 0.1.0
- initial release, approved by CRAN Feb 16, 2021
