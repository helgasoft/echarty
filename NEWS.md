# log history of echarty package development

## v. 0.1.4

- upgrade to ECharts v.5.1.1
- fixed p_del_serie by seriesName
- expand Shiny demo and moved from ec.examples() to a gist 
- replaced purrr with apply in ec.data, purrr no longer a dependency
- ec.init group1 now is also default for ungrouped serie 
- added svg maps, like registerMap=list(list(mapName=?, svg=?))
- new ec.layout(), a charts layout helper

## v. 0.1.3

- crosstalk support added, send & receive for brush and filter actions
- ec.timegrp added, a helper to build timeline data for a grouped data.frame
- ec.data with additional parameter *header*

## v. 0.1.2

- ec.data now with three format values
- ec.fromJson has now "..." (additional arguments)
- ecr.band has new parameter 'two' for alternative rendering
- error bars added as ecr.ebars with support for grouped data
- more examples in ec.examples
- some code optimization

## v. 0.1.1

- added ec.plugjs - install unknown plugins, like JS maps
- added ec.js2r - a JS to R translation assistant, a Shiny app
- ec.init update: one-time install for all plugins except built-in leaflet/custom
- GL merged with 3D as plugin '3D'
- more plugins: liquidfill, world, wordcloud, gmodular
- ec.sband renamed to ec.band 

## v. 0.1.0
- initial release, approved by CRAN Feb 16, 2021
