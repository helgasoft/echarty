# Changelog

## echarty 1.7.3 in development

- fix series\$id bug

## echarty 1.7.2 on CRAN

CRAN release: 2026-04-04

- added *matrixIndex*, *calendarIndex* and other indexes in .renumber
- fixed some *globe, 3D* presets
- new v.6 custom series can be loaded by *renderItem* name only, no
  load/ask needed
- cmd=‘sf.series’ in *ec.util*: removed parameter *cs*, use
  *coordinateSystem* instead
- *ec.clmn* refactored

## echarty 1.7.1 on CRAN

CRAN release: 2026-01-09

- fixes: updated demos code; check for empty df; custom charts .js file
  names
- enhance flame chart with vertical labels (ec\$vlevel)

## echarty 1.7.0

CRAN release: 2025-10-23

- new *ec.init* parameter ‘iniOpts’ to support
  *on*,*off*,\_register\*\_,etc. for chart instance
- new value ‘loadRemote’ for parameter *ask* to serve new v.6
  custom-series
- ability to attach data columns to item styles through *encode\$data*
- new flame (or icicle) hierarchical chart support as custom series

## echarty 1.6.7

- upgrade ECharts to v.6.0.0, built with R v.4.4.3.
- new signature of *ec.util*: *cmd* is now first, to avoid writing
  ‘cmd=…’
- improved *tabset* in *ec.util*
- new *ec.init* parameter *iniOpts* for initialization options
- new helper *ec.registerMap* for geoJSON and SVG maps
- new *ec.data* helper *‘borders’*: get geoJSON region borders from
  data.frame
- auto-add *load=‘world’* if missing in series *map=‘world’*
- auto-register *ecStat.transform* when *load=‘ecStat’* is set

## echarty 1.6.6 on CRAN

CRAN release: 2025-01-15

- upgrade ECharts to v.5.6.0, built with R v.4.4.2.
- auto-load 3D plugin when 3D attributes present (xAxis3D, bar3D, etc.).
- auto-set 3D axes from data (name, type).
- change in *dataset*: store column names in *dimensions* instead of
  *source*.
- fixed bug in ecr.ebars for single series.
- add optional placeholder ‘%@’ in title\$text for timeline.
- add *event* parameter in *ec.util* for cmd=‘morph’.
- integrate website with library using *pkgdown*.
- moved *examples.R* into ‘demo’ folder, *ec.examples* is no longer a
  command.
- more tests to increase coverage without disturbing CRAN submission

## echarty 1.6.4

CRAN release: 2024-06-05

- upgrade ECharts to v.5.5.0, built with R v.4.4.0.
- add *nasep* parameter to *ec.data(‘names’)* to easily set nested lists
  from a *data.frame*.
- add [WebR](https://docs.r-wasm.org/webr/latest/) support and
  [test](https://helgasoft.github.io/echarty/test/coder.html).
- add explicit *leaflet* dependency since dependencies changed in
  leaflet v.2.2.0.
- add optional tooltip formatter (tipFmt) in *ecr.band*.
- refactoring (leaflet, geo, geoJson, tests).
- add debug flags for messages in JS and R.
- fix *crosstalk* bug for checkboxes unselect.

## echarty 1.6.3

CRAN release: 2024-01-12

- added coder.R demo.
- correct gridIndex numbering for xAxis/yAxis.
- deprecate *tl.series*, replace with *timeline* and *series.param*.

## echarty 1.6.2.01

- make crosstalk work with improved *ec.clmn*.
- add *ecStat* to built-in plugins.
- removed *dataTool.min.js* from dependencies (yaml).
- dataset,geo,polar,etc. indexes in series now with R-counting.
- built with R v.4.3.2.
- add default *coordinateSystem* for 3D charts.
- fix *datasetIndex* for timeline 3D series.
- add *encode* default, improve *ec.init* docs.
- timeline ‘map’: add support dimension for visualMap, fix legend,title
  bugs.
- removed width/height from ‘tabset’, handled by *ec.init*.
- *ecr.band* replaced default type from ‘stack’ to ‘polygon’.

## echarty 1.6.2

CRAN release: 2023-10-15

- allow axis rename (fix).
- *ec.data* grouped boxplots: outliers are custom series.
- *ec.clmn* expanded usage of column names.

## echarty 1.6.0

CRAN release: 2023-09-20

- *ec.paxis* could be chained now.
- *ec.data* format=‘boxplot’ with optional outliers.
- *ecr.bars* with better alignment and horizontal layout.

## echarty 1.5.4.03

- upgrade ECharts to v.5.4.3, built with R v.4.3.1.
- breaking change in *ec.init* signature.
- improved *ec.inspect* and *ec.fromJson*.
- fixed small bugs and improved code.
- added more tests.

## echarty 1.5.4

CRAN release: 2023-05-28

- *ec.util* command ‘layout’ updated.
- changes in axis names preset, *ec.clmn* for empty values.
- problem running Shiny in R console: *No handler registered for
  mouseover*.

## echarty 1.5.3

- upgrade ECharts to v.5.4.2.
- *ec.util* support for map GeoJSON objects, see test-renderers.R.
- *ec.init* new parameter *xtKey* for crosstalk (ID-column name).
- code cleanup in *ecr.band*.
- expand auto-generated axes names to multiple.

## echarty 1.5.2

- *ec.init* to set default xAxis/yAxis names from *df*.
- *ec.clmn* accepts JS functions in parameter *col*.
- *ec.util* new command ‘button’ to run JS function.
- fix *tabset* bug introduced in v.1.5.1.

## echarty 1.5.1

- upgrade ECharts to v.5.4.1.
- remove redundant dependencies, only 3 used now.
- *ecr.band* default type changed from ‘polygon’ to ‘stack’.
- *ec.data* type ‘boxplot’ remodeled and jitter-scatter added.
- new *series.param* in *ec.init* for additional parameters for preset
  series.
- make *ec.util* tabset responsive by resizing tabs onclick.
- fix *visualMap* piecewise dimension R-counts.
- set *visualMap* default min/max values from df.
- *echarty.js*: add built-in ‘mouseout’ event.

## echarty 1.5.0 on CRAN

CRAN release: 2022-11-24

- upgrade ECharts to v.5.4.0.
- built with R v.4.2.2.
- added vignette with introduction.
- *ecr.ebars* fix bug, enhance tooltips.
- *ec.util* improvements in sf.series.
- *ec.util* new commands fullscreen, morph, rescale, level.
- *ec.clmn* new options ‘json’ and ‘log’.
- *ec.init* set R-counting for encode/xAxisIndex, visualMap\$dimension.
- fixes to global options theme & font, group legend.
- fixed *load=‘world’* with series set.
- new *load=‘lottie’*, lotties support now built-in.
- reorganize crosstalk code.

## echarty 1.4.7

CRAN release: 2022-08-28

- *ec.util*: add POLYGON/MULTIPOINT, new *sf.unzip*.
- *ec.util*: new cmd=‘tabset’ to show multiple charts in tabs.
- *ec.util*: new cmd=‘layout’ for multiple charts in table-like format.
- *ec.upd* replaces *ec.snip* for chained commands.
- improved *ec.data* format=‘treeTK’, see example in *test-other.R*.
- support for *lotties* in *graphic*, see web gallery for sample.
- grouping column in 2D charts now could be at any position.

## echarty 1.4.6

- upgrade ECharts to v.5.3.3.
- expand docs for *ec.init*.
- fix timeline *tl.series* for candlestick,boxplot,pictorialBar.
- add custom attribute *groupBy* for *tl.series*.
- add *riErrBarSimple* renderer.
- new *ec.data* formats ‘treePC’ and ‘treeTK’ for hierarchies.
- changed *ec.data* default parameter *header* to FALSE.
- fixes in *ec.clmn* and allow empty *col* parameter for
  pie,sunburst,etc.
- add *ec.util* to support map shapefiles thru library ‘sf’.

## echarty 1.4.5

CRAN release: 2022-04-01

- upgrade ECharts to v.5.3.1, GL to v.2.0.9.
- *ec.init* timeline (*tl.series*) to support 3D.
- *ec.init* timeline (*tl.series*) to support map chart.
- updated *ec.clmn* for single value charts like tree. Scale 0 to round
  values. Scale expanded to all numeric columns. Decimal indexes for
  combo charts. L: locale number format. LR: locale rounded format.
- new *ec.data* format=‘dendrogram’ with new dependency *data.tree*.
- new *ec.data* format=‘boxplot’ - a helper for boxplot data.
- new package info via command “?echarty”.
- added new brush/click events to demo(*eshiny*).
- cleanup `echarty.js` code for events.

## echarty 1.4.2

CRAN release: 2021-11-06

- replaced magrittr %\>% dependency with R native pipe \|\>.
- added *ec.snip* as option for better code readability.
- removed *ec.global*, use options(‘echarty.xxx’) instead.
- added *shiny* dependency to avoid CRAN warnings.
- added more unit tests.

## echarty 1.4.0

CRAN release: 2021-10-24

- *ec.init*: renamed ‘group1’ to ‘ctype’. Presets for parallel chart,
  xAxis.
- *ec.paxis* new helper for parallelAxis.
- *ec.clmn* placeholders %d %s replaced with %@.
- added initial unit tests.

## echarty 0.3.2

- *ec.init*: *js* parameter has now 3 levels of execution.
- *ec.clmn*: fix order of parameters, add message for missing data.
- *eshiny* demo: button and code added to modify series.

## echarty 0.3.1.3

- *ec.init*: fix ‘3D’ presets for groups and ‘world’ presets.
- *ec.clmn*: added new parameter *scale*.
- *echarty.js* add resize parameter.
- updates to examples.R.
- upgrade ECharts to v.5.2.1, liquidfill to v.3.1.0.

## echarty 0.3.1.2

- *ec.init* added ECharts parameters *locale* and *useDirtyRect*.

## echarty 0.3.1

CRAN release: 2021-07-30

- *ec.init* param *timeline* renamed to *tl.series*.
- tweaks in docs, examples and *echarty.js*.

## echarty 0.3.0

- *ec.timegrp* merged into *ec.init* and removed.
- *ec.init* can now build timeline, parameters simplified.
- *ecr.ebars* now supports dataset, has default tooltip.
- *ecr.band* changed parameter ‘two’ to ‘type’, improved tooltip
  support.
- *ec.inspect* expanded with new parameter ‘data’.
- *eshiny* demo updated: brush parameters, new zoom capture.
- new *ec.clmn* to reference data column(s) in
  formatter,color,symbolSize,etc.

## echarty 0.2.2

- *ec.init* default *ask*=FALSE to *load* without prompts.
- *echarty.js*: add ‘mouseout’ event.
- *echarty.js*: move legend to *p_append_data*.

## echarty 0.2.1

- upgrade to ECharts v.5.1.2.
- *ec.plugjs*: new parameter *ask* to allow/suppress prompts.
- *ec.plugjs*: parameter *source* now accepts ‘<file://>’ format.

## echarty 0.2.0

CRAN release: 2021-06-13

- removed dependencies for Shiny and crosstalk.
- *js2r* and *eshiny* are now R demos.
- change leaflet plugin source.data to dataset, preset center and zoom.
- built with latest R v.4.1.0.

## echarty 0.1.4

- upgrade to ECharts v.5.1.1.
- fixed *p_del_serie* by seriesName.
- expand Shiny demo and moved from ec.examples() to a gist.
- replaced purrr with apply in ec.data, purrr no longer a dependency.
- *ec.init* parameter *group1* now is also default for ungrouped serie.
- added svg maps, like `registerMap=list(list(mapName=?, svg=?))`.
- new *ec.layout()*, a charts layout helper.

## echarty 0.1.3

CRAN release: 2021-03-31

- **crosstalk** support added, send & receive for brush and filter
  actions.
- *ec.timegrp* added, a helper to build timeline data for a grouped
  data.frame.
- *ec.data* with additional parameter *header*.

## echarty 0.1.2

- *ec.data* now with three format values.
- *ec.fromJson* has now “…” (additional arguments).
- *ecr.band* has new parameter ‘two’ for alternative rendering.
- error bars added as *ecr.ebars* with support for grouped data.
- more examples in *ec.examples*.
- some code optimization.

## echarty 0.1.1

- added *ec.plugjs* - install unknown plugins, like JS maps.
- added *ec.js2r* - a JS to R translation assistant, a Shiny app.
- *ec.init* update: one-time install for all plugins except built-in
  leaflet/custom.
- GL merged with 3D as plugin ‘3D’.
- more plugins: liquidfill, world, wordcloud, gmodular.
- ec.sband renamed to *ec.band*.

## echarty 0.1.0

CRAN release: 2021-02-16

- initial release, approved by CRAN Feb 16, 2021.
