# Code Examples

Learn by example - copy/paste code from Examples below.  
This code collection is to demonstrate various concepts of data
preparation, conversion, grouping, parameter setting, visual
fine-tuning, custom rendering, plugins attachment, Shiny plots &
interactions through Shiny proxy.  

## Usage

``` r
ec.examples()
```

## Value

No return value, used only for help

## See also

[website](https://helgasoft.github.io/echarty/) has many more examples

## Examples

``` r
# \donttest{
library(dplyr); library(echarty)

#------ Basic scatter chart, instant display
cars |> ec.init()

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"xAxis":{"show":true,"type":"value","name":"speed"},"yAxis":{"show":true,"type":"value","name":"dist"},"series":[{"type":"scatter"}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
#------ Same chart, change theme and save for further processing
p <- cars |> ec.init() |> ec.theme('dark')
p

{"x":{"theme":"dark","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"xAxis":{"show":true,"type":"value","name":"speed"},"yAxis":{"show":true,"type":"value","name":"dist"},"series":[{"type":"scatter"}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
#------ parallel chart
ToothGrowth |> ec.init(ctype= 'parallel')

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["len","supp","dose"],"source":[[4.2,"VC",0.5],[11.5,"VC",0.5],[7.3,"VC",0.5],[5.8,"VC",0.5],[6.4,"VC",0.5],[10,"VC",0.5],[11.2,"VC",0.5],[11.2,"VC",0.5],[5.2,"VC",0.5],[7,"VC",0.5],[16.5,"VC",1],[16.5,"VC",1],[15.2,"VC",1],[17.3,"VC",1],[22.5,"VC",1],[17.3,"VC",1],[13.6,"VC",1],[14.5,"VC",1],[18.8,"VC",1],[15.5,"VC",1],[23.6,"VC",2],[18.5,"VC",2],[33.9,"VC",2],[25.5,"VC",2],[26.4,"VC",2],[32.5,"VC",2],[26.7,"VC",2],[21.5,"VC",2],[23.3,"VC",2],[29.5,"VC",2],[15.2,"OJ",0.5],[21.5,"OJ",0.5],[17.6,"OJ",0.5],[9.699999999999999,"OJ",0.5],[14.5,"OJ",0.5],[10,"OJ",0.5],[8.199999999999999,"OJ",0.5],[9.4,"OJ",0.5],[16.5,"OJ",0.5],[9.699999999999999,"OJ",0.5],[19.7,"OJ",1],[23.3,"OJ",1],[23.6,"OJ",1],[26.4,"OJ",1],[20,"OJ",1],[25.2,"OJ",1],[25.8,"OJ",1],[21.2,"OJ",1],[14.5,"OJ",1],[27.3,"OJ",1],[25.5,"OJ",2],[26.4,"OJ",2],[22.4,"OJ",2],[24.5,"OJ",2],[24.8,"OJ",2],[30.9,"OJ",2],[26.4,"OJ",2],[27.3,"OJ",2],[29.4,"OJ",2],[23,"OJ",2]]}],"series":[{"type":"parallel"}],"parallelAxis":[{"dim":0,"name":"len","max":33.9,"min":4.2},{"dim":1,"name":"supp","type":"category"},{"dim":2,"name":"dose","max":2,"min":0.5}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
#------ JSON back and forth
tmp <- cars |> ec.init()
tmp

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"xAxis":{"show":true,"type":"value","name":"speed"},"yAxis":{"show":true,"type":"value","name":"dist"},"series":[{"type":"scatter"}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}json <- tmp |> ec.inspect()
ec.fromJson(json) |> ec.theme("dark")

{"x":{"theme":"dark","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["speed","dist"],"source":[[4,2],[4,10],[7,4],[7,22],[8,16],[9,10],[10,18],[10,26],[10,34],[11,17],[11,28],[12,14],[12,20],[12,24],[12,28],[13,26],[13,34],[13,34],[13,46],[14,26],[14,36],[14,60],[14,80],[15,20],[15,26],[15,54],[16,32],[16,40],[17,32],[17,40],[17,50],[18,42],[18,56],[18,76],[18,84],[19,36],[19,46],[19,68],[20,32],[20,48],[20,52],[20,56],[20,64],[22,66],[23,54],[24,70],[24,92],[24,93],[24,120],[25,85]]}],"xAxis":{"show":true,"type":"value","name":"speed"},"yAxis":{"show":true,"type":"value","name":"dist"},"series":[{"type":"scatter"}]}},"evals":[],"jsHooks":[]}

#------ Data grouping
iris |> mutate(Species= as.character(Species)) |>
        group_by(Species) |> ec.init()      # by non-factor column

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["Sepal.Length","Sepal.Width","Petal.Length","Petal.Width","Species"],"source":[[5.1,3.5,1.4,0.2,"setosa"],[4.9,3,1.4,0.2,"setosa"],[4.7,3.2,1.3,0.2,"setosa"],[4.6,3.1,1.5,0.2,"setosa"],[5,3.6,1.4,0.2,"setosa"],[5.4,3.9,1.7,0.4,"setosa"],[4.6,3.4,1.4,0.3,"setosa"],[5,3.4,1.5,0.2,"setosa"],[4.4,2.9,1.4,0.2,"setosa"],[4.9,3.1,1.5,0.1,"setosa"],[5.4,3.7,1.5,0.2,"setosa"],[4.8,3.4,1.6,0.2,"setosa"],[4.8,3,1.4,0.1,"setosa"],[4.3,3,1.1,0.1,"setosa"],[5.8,4,1.2,0.2,"setosa"],[5.7,4.4,1.5,0.4,"setosa"],[5.4,3.9,1.3,0.4,"setosa"],[5.1,3.5,1.4,0.3,"setosa"],[5.7,3.8,1.7,0.3,"setosa"],[5.1,3.8,1.5,0.3,"setosa"],[5.4,3.4,1.7,0.2,"setosa"],[5.1,3.7,1.5,0.4,"setosa"],[4.6,3.6,1,0.2,"setosa"],[5.1,3.3,1.7,0.5,"setosa"],[4.8,3.4,1.9,0.2,"setosa"],[5,3,1.6,0.2,"setosa"],[5,3.4,1.6,0.4,"setosa"],[5.2,3.5,1.5,0.2,"setosa"],[5.2,3.4,1.4,0.2,"setosa"],[4.7,3.2,1.6,0.2,"setosa"],[4.8,3.1,1.6,0.2,"setosa"],[5.4,3.4,1.5,0.4,"setosa"],[5.2,4.1,1.5,0.1,"setosa"],[5.5,4.2,1.4,0.2,"setosa"],[4.9,3.1,1.5,0.2,"setosa"],[5,3.2,1.2,0.2,"setosa"],[5.5,3.5,1.3,0.2,"setosa"],[4.9,3.6,1.4,0.1,"setosa"],[4.4,3,1.3,0.2,"setosa"],[5.1,3.4,1.5,0.2,"setosa"],[5,3.5,1.3,0.3,"setosa"],[4.5,2.3,1.3,0.3,"setosa"],[4.4,3.2,1.3,0.2,"setosa"],[5,3.5,1.6,0.6,"setosa"],[5.1,3.8,1.9,0.4,"setosa"],[4.8,3,1.4,0.3,"setosa"],[5.1,3.8,1.6,0.2,"setosa"],[4.6,3.2,1.4,0.2,"setosa"],[5.3,3.7,1.5,0.2,"setosa"],[5,3.3,1.4,0.2,"setosa"],[7,3.2,4.7,1.4,"versicolor"],[6.4,3.2,4.5,1.5,"versicolor"],[6.9,3.1,4.9,1.5,"versicolor"],[5.5,2.3,4,1.3,"versicolor"],[6.5,2.8,4.6,1.5,"versicolor"],[5.7,2.8,4.5,1.3,"versicolor"],[6.3,3.3,4.7,1.6,"versicolor"],[4.9,2.4,3.3,1,"versicolor"],[6.6,2.9,4.6,1.3,"versicolor"],[5.2,2.7,3.9,1.4,"versicolor"],[5,2,3.5,1,"versicolor"],[5.9,3,4.2,1.5,"versicolor"],[6,2.2,4,1,"versicolor"],[6.1,2.9,4.7,1.4,"versicolor"],[5.6,2.9,3.6,1.3,"versicolor"],[6.7,3.1,4.4,1.4,"versicolor"],[5.6,3,4.5,1.5,"versicolor"],[5.8,2.7,4.1,1,"versicolor"],[6.2,2.2,4.5,1.5,"versicolor"],[5.6,2.5,3.9,1.1,"versicolor"],[5.9,3.2,4.8,1.8,"versicolor"],[6.1,2.8,4,1.3,"versicolor"],[6.3,2.5,4.9,1.5,"versicolor"],[6.1,2.8,4.7,1.2,"versicolor"],[6.4,2.9,4.3,1.3,"versicolor"],[6.6,3,4.4,1.4,"versicolor"],[6.8,2.8,4.8,1.4,"versicolor"],[6.7,3,5,1.7,"versicolor"],[6,2.9,4.5,1.5,"versicolor"],[5.7,2.6,3.5,1,"versicolor"],[5.5,2.4,3.8,1.1,"versicolor"],[5.5,2.4,3.7,1,"versicolor"],[5.8,2.7,3.9,1.2,"versicolor"],[6,2.7,5.1,1.6,"versicolor"],[5.4,3,4.5,1.5,"versicolor"],[6,3.4,4.5,1.6,"versicolor"],[6.7,3.1,4.7,1.5,"versicolor"],[6.3,2.3,4.4,1.3,"versicolor"],[5.6,3,4.1,1.3,"versicolor"],[5.5,2.5,4,1.3,"versicolor"],[5.5,2.6,4.4,1.2,"versicolor"],[6.1,3,4.6,1.4,"versicolor"],[5.8,2.6,4,1.2,"versicolor"],[5,2.3,3.3,1,"versicolor"],[5.6,2.7,4.2,1.3,"versicolor"],[5.7,3,4.2,1.2,"versicolor"],[5.7,2.9,4.2,1.3,"versicolor"],[6.2,2.9,4.3,1.3,"versicolor"],[5.1,2.5,3,1.1,"versicolor"],[5.7,2.8,4.1,1.3,"versicolor"],[6.3,3.3,6,2.5,"virginica"],[5.8,2.7,5.1,1.9,"virginica"],[7.1,3,5.9,2.1,"virginica"],[6.3,2.9,5.6,1.8,"virginica"],[6.5,3,5.8,2.2,"virginica"],[7.6,3,6.6,2.1,"virginica"],[4.9,2.5,4.5,1.7,"virginica"],[7.3,2.9,6.3,1.8,"virginica"],[6.7,2.5,5.8,1.8,"virginica"],[7.2,3.6,6.1,2.5,"virginica"],[6.5,3.2,5.1,2,"virginica"],[6.4,2.7,5.3,1.9,"virginica"],[6.8,3,5.5,2.1,"virginica"],[5.7,2.5,5,2,"virginica"],[5.8,2.8,5.1,2.4,"virginica"],[6.4,3.2,5.3,2.3,"virginica"],[6.5,3,5.5,1.8,"virginica"],[7.7,3.8,6.7,2.2,"virginica"],[7.7,2.6,6.9,2.3,"virginica"],[6,2.2,5,1.5,"virginica"],[6.9,3.2,5.7,2.3,"virginica"],[5.6,2.8,4.9,2,"virginica"],[7.7,2.8,6.7,2,"virginica"],[6.3,2.7,4.9,1.8,"virginica"],[6.7,3.3,5.7,2.1,"virginica"],[7.2,3.2,6,1.8,"virginica"],[6.2,2.8,4.8,1.8,"virginica"],[6.1,3,4.9,1.8,"virginica"],[6.4,2.8,5.6,2.1,"virginica"],[7.2,3,5.8,1.6,"virginica"],[7.4,2.8,6.1,1.9,"virginica"],[7.9,3.8,6.4,2,"virginica"],[6.4,2.8,5.6,2.2,"virginica"],[6.3,2.8,5.1,1.5,"virginica"],[6.1,2.6,5.6,1.4,"virginica"],[7.7,3,6.1,2.3,"virginica"],[6.3,3.4,5.6,2.4,"virginica"],[6.4,3.1,5.5,1.8,"virginica"],[6,3,4.8,1.8,"virginica"],[6.9,3.1,5.4,2.1,"virginica"],[6.7,3.1,5.6,2.4,"virginica"],[6.9,3.1,5.1,2.3,"virginica"],[5.8,2.7,5.1,1.9,"virginica"],[6.8,3.2,5.9,2.3,"virginica"],[6.7,3.3,5.7,2.5,"virginica"],[6.7,3,5.2,2.3,"virginica"],[6.3,2.5,5,1.9,"virginica"],[6.5,3,5.2,2,"virginica"],[6.2,3.4,5.4,2.3,"virginica"],[5.9,3,5.1,1.8,"virginica"]]},{"transform":{"type":"filter","config":{"dimension":"Species","=":"setosa"}},"id":"setosa"},{"transform":{"type":"filter","config":{"dimension":"Species","=":"versicolor"}},"id":"versicolor"},{"transform":{"type":"filter","config":{"dimension":"Species","=":"virginica"}},"id":"virginica"}],"series":[{"type":"scatter","datasetIndex":1,"name":"setosa"},{"type":"scatter","datasetIndex":2,"name":"versicolor"},{"type":"scatter","datasetIndex":3,"name":"virginica"}],"legend":{"data":[{"name":"setosa"},{"name":"versicolor"},{"name":"virginica"}]},"xAxis":{"show":true,"type":"value","name":"Sepal.Length"},"yAxis":{"show":true,"type":"value","name":"Sepal.Width"}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
Orange |> group_by(Tree) |> ec.init(
  series.param= list(symbolSize= 10, encode= list(x='age', y='circumference'))
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["Tree","age","circumference"],"source":[["1",118,30],["1",484,58],["1",664,87],["1",1004,115],["1",1231,120],["1",1372,142],["1",1582,145],["2",118,33],["2",484,69],["2",664,111],["2",1004,156],["2",1231,172],["2",1372,203],["2",1582,203],["3",118,30],["3",484,51],["3",664,75],["3",1004,108],["3",1231,115],["3",1372,139],["3",1582,140],["4",118,32],["4",484,62],["4",664,112],["4",1004,167],["4",1231,179],["4",1372,209],["4",1582,214],["5",118,30],["5",484,49],["5",664,81],["5",1004,125],["5",1231,142],["5",1372,174],["5",1582,177]]},{"transform":{"type":"filter","config":{"dimension":"Tree","=":"3"}},"id":"3"},{"transform":{"type":"filter","config":{"dimension":"Tree","=":"1"}},"id":"1"},{"transform":{"type":"filter","config":{"dimension":"Tree","=":"5"}},"id":"5"},{"transform":{"type":"filter","config":{"dimension":"Tree","=":"2"}},"id":"2"},{"transform":{"type":"filter","config":{"dimension":"Tree","=":"4"}},"id":"4"}],"series":[{"type":"scatter","datasetIndex":1,"name":"3","symbolSize":10,"encode":{"x":"age","y":"circumference"}},{"type":"scatter","datasetIndex":2,"name":"1","symbolSize":10,"encode":{"x":"age","y":"circumference"}},{"type":"scatter","datasetIndex":3,"name":"5","symbolSize":10,"encode":{"x":"age","y":"circumference"}},{"type":"scatter","datasetIndex":4,"name":"2","symbolSize":10,"encode":{"x":"age","y":"circumference"}},{"type":"scatter","datasetIndex":5,"name":"4","symbolSize":10,"encode":{"x":"age","y":"circumference"}}],"legend":{"data":[{"name":"3"},{"name":"1"},{"name":"5"},{"name":"2"},{"name":"4"}]},"xAxis":{"show":true,"name":"age","type":"value"},"yAxis":{"show":true,"name":"circumference","type":"value"}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
#------ Polar bar chart
cnt <- 5; set.seed(222)
data.frame(
    x = seq(cnt),
    y = round(rnorm(cnt, 10, 3)), 
    z = round(rnorm(cnt, 11, 2)),
    colr = rainbow(cnt) 
) |> 
ec.init( preset= FALSE,
   polar= list(radius= '90%'),
   radiusAxis= list(max= 'dataMax'), 
   angleAxis= list(type= "category"),
   series= list(
     list(type= "bar", coordinateSystem= "polar",
       itemStyle= list(color= ec.clmn('colr')),
       label= list(show= TRUE, position= "middle", formatter= "y={@[1]}")
     ),
     list(type= 'scatter', coordinateSystem= "polar", 
       itemStyle= list(color= 'black'),
         encode= list(angle='x', radius='z'))
   )
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"polar":{"radius":"90%"},"radiusAxis":{"max":"dataMax"},"angleAxis":{"type":"category"},"series":[{"type":"bar","coordinateSystem":"polar","itemStyle":{"color":"function(x) {var sprintf= (template, vals) => { j=0; if (template=='%@') return vals[j++]; return template.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => {   if (m=='%@') return vals[j++];   if (m=='%L@') return Number(vals[j++]).toLocaleString();   if (m=='%LR@') return Math.round(Number(vals[j++])).toLocaleString();   if (m=='%R@') return Math.round(Number(vals[j++]));   if (m=='%R2@') return Number(vals[j++]).toFixed(2);   if (m=='%M@') return x.marker; }); }; pos=['Tree','age','circumference'];  aa= Array.isArray(x) ? x : x.data; tmp= null; if (aa && aa instanceof Object && !Array.isArray(aa)) {  tmp= Object.keys(aa); if (tmp.length==1 && tmp[0]=='value') aa= x.data.value;} if (tmp && tmp.length>1)  vv=[x.data['colr']]; else {  if (!aa || !aa.length) return `no data`;  args= [`colr`];   pos= args.map(z => pos.indexOf(z));  vv= pos.map(p => aa[p]); } vv= vv.map(e => isNaN(e) | !e ? e : e* 1 );c= sprintf(`%@`, vv); return c; }"},"label":{"show":true,"position":"middle","formatter":"y={@[1]}"}},{"type":"scatter","coordinateSystem":"polar","itemStyle":{"color":"black"},"encode":{"angle":"x","radius":"z"}}],"dataset":[{"dimensions":["x","y","z","colr"],"source":[[1,14,11,"#FF0000"],[2,10,9,"#CCFF00"],[3,14,14,"#00FF66"],[4,9,12,"#0066FF"],[5,11,9,"#CC00FF"]]}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.series.0.itemStyle.color"],"jsHooks":[]}

#------ Area chart
mtcars |> dplyr::relocate(wt,mpg) |> arrange(wt) |> group_by(cyl) |>
  ec.init(ctype= 'line', series.param= list(areaStyle= list(show=TRUE)) )

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"dimensions":["wt","mpg","cyl","disp","hp","drat","qsec","vs","am","gear","carb"],"source":[[1.513,30.4,4,95.09999999999999,113,3.77,16.9,1,1,5,2],[1.615,30.4,4,75.7,52,4.93,18.52,1,1,4,2],[1.835,33.9,4,71.09999999999999,65,4.22,19.9,1,1,4,1],[1.935,27.3,4,79,66,4.08,18.9,1,1,4,1],[2.14,26,4,120.3,91,4.43,16.7,0,1,5,2],[2.2,32.4,4,78.7,66,4.08,19.47,1,1,4,1],[2.32,22.8,4,108,93,3.85,18.61,1,1,4,1],[2.465,21.5,4,120.1,97,3.7,20.01,1,0,3,1],[2.62,21,6,160,110,3.9,16.46,0,1,4,4],[2.77,19.7,6,145,175,3.62,15.5,0,1,5,6],[2.78,21.4,4,121,109,4.11,18.6,1,1,4,2],[2.875,21,6,160,110,3.9,17.02,0,1,4,4],[3.15,22.8,4,140.8,95,3.92,22.9,1,0,4,2],[3.17,15.8,8,351,264,4.22,14.5,0,1,5,4],[3.19,24.4,4,146.7,62,3.69,20,1,0,4,2],[3.215,21.4,6,258,110,3.08,19.44,1,0,3,1],[3.435,15.2,8,304,150,3.15,17.3,0,0,3,2],[3.44,18.7,8,360,175,3.15,17.02,0,0,3,2],[3.44,19.2,6,167.6,123,3.92,18.3,1,0,4,4],[3.44,17.8,6,167.6,123,3.92,18.9,1,0,4,4],[3.46,18.1,6,225,105,2.76,20.22,1,0,3,1],[3.52,15.5,8,318,150,2.76,16.87,0,0,3,2],[3.57,14.3,8,360,245,3.21,15.84,0,0,3,4],[3.57,15,8,301,335,3.54,14.6,0,1,5,8],[3.73,17.3,8,275.8,180,3.07,17.6,0,0,3,3],[3.78,15.2,8,275.8,180,3.07,18,0,0,3,3],[3.84,13.3,8,350,245,3.73,15.41,0,0,3,4],[3.845,19.2,8,400,175,3.08,17.05,0,0,3,2],[4.07,16.4,8,275.8,180,3.07,17.4,0,0,3,3],[5.25,10.4,8,472,205,2.93,17.98,0,0,3,4],[5.345,14.7,8,440,230,3.23,17.42,0,0,3,4],[5.424,10.4,8,460,215,3,17.82,0,0,3,4]]},{"transform":{"type":"filter","config":{"dimension":"cyl","=":4}},"id":4},{"transform":{"type":"filter","config":{"dimension":"cyl","=":6}},"id":6},{"transform":{"type":"filter","config":{"dimension":"cyl","=":8}},"id":8}],"series":[{"type":"line","datasetIndex":1,"name":"4","areaStyle":{"show":true}},{"type":"line","datasetIndex":2,"name":"6","areaStyle":{"show":true}},{"type":"line","datasetIndex":3,"name":"8","areaStyle":{"show":true}}],"legend":{"data":[{"name":"4"},{"name":"6"},{"name":"8"}]},"xAxis":{"show":true,"type":"value","name":"wt"},"yAxis":{"show":true,"type":"value","name":"mpg"}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
#------ Plugin leaflet
quakes |> dplyr::relocate('long') |>  # set order to long,lat
  mutate(size= exp(mag)/20) |> head(100) |>  # add accented size
ec.init(load= 'leaflet',
   tooltip= list(formatter= ec.clmn('magnitude %@', 'mag')),
   legend= list(show=TRUE),
  series.param= list(name= 'quakes', symbolSize= ec.clmn(6, scale=2))  # 6th column is size
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"tooltip":{"formatter":"function(x) {var sprintf= (template, vals) => { j=0; if (template=='%@') return vals[j++]; return template.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => {   if (m=='%@') return vals[j++];   if (m=='%L@') return Number(vals[j++]).toLocaleString();   if (m=='%LR@') return Math.round(Number(vals[j++])).toLocaleString();   if (m=='%R@') return Math.round(Number(vals[j++]));   if (m=='%R2@') return Number(vals[j++]).toFixed(2);   if (m=='%M@') return x.marker; }); }; pos=['wt','mpg','cyl','disp','hp','drat','qsec','vs','am','gear','carb'];  aa= Array.isArray(x) ? x : x.data; tmp= null; if (aa && aa instanceof Object && !Array.isArray(aa)) {  tmp= Object.keys(aa); if (tmp.length==1 && tmp[0]=='value') aa= x.data.value;} if (tmp && tmp.length>1)  vv=[x.data['mag']]; else {  if (!aa || !aa.length) return `no data`;  args= [`mag`];   pos= args.map(z => pos.indexOf(z));  vv= pos.map(p => aa[p]); } vv= vv.map(e => isNaN(e) | !e ? e : e* 1 );c= sprintf(`magnitude %@`, vv); return c; }"},"legend":{"show":true},"dataset":[{"dimensions":["long","lat","depth","mag","stations","size"],"source":[[181.62,-20.42,562,4.8,41,6.075520875936743],[181.03,-20.62,650,4.2,15,3.334316552046258],[184.1,-26,42,5.4,43,11.07032081020936],[181.66,-17.97,626,4.1,19,3.017014379868098],[181.96,-20.42,649,4,11,2.729907501657212],[184.31,-19.68,195,4,12,2.729907501657212],[166.1,-11.7,82,4.8,43,6.075520875936743],[181.93,-28.11,194,4.4,15,4.072543433248407],[181.74,-28.74,211,4.7,35,5.497358622606177],[179.59,-17.47,622,4.3,19,3.684989684979789],[180.69,-21.44,583,4.4,13,4.072543433248407],[167,-12.26,249,4.6,16,4.974215782096689],[182.11,-18.54,554,4.4,19,4.072543433248407],[181.66,-21,600,4.4,10,4.072543433248407],[169.92,-20.7,139,6.1,94,22.29288850412584],[184.95,-15.94,306,4.3,11,3.684989684979789],[165.96,-13.64,50,6,83,20.17143967463675],[181.5,-17.83,590,4.5,21,4.500856565026091],[179.78,-23.5,570,4.4,13,4.072543433248407],[180.31,-22.63,598,4.4,18,4.072543433248407],[181.16,-20.84,576,4.5,17,4.500856565026091],[166.32,-10.98,211,4.2,12,3.334316552046258],[180.16,-23.3,512,4.4,18,4.072543433248407],[182,-30.2,125,4.7,22,5.497358622606177],[180.28,-19.66,431,5.4,57,11.07032081020936],[181.49,-17.94,537,4,15,2.729907501657212],[167.51,-14.72,155,4.6,18,4.974215782096689],[180.79,-16.46,498,5.2,79,9.063612093757561],[181.47,-20.97,582,4.5,25,4.500856565026091],[182.37,-19.84,328,4.4,17,4.072543433248407],[179.24,-22.58,553,4.6,21,4.974215782096689],[166.74,-16.32,50,4.7,30,5.497358622606177],[185.05,-15.55,292,4.8,42,6.075520875936743],[180.8,-23.55,349,4,10,2.729907501657212],[186,-16.3,48,4.5,10,4.500856565026091],[179.33,-25.82,600,4.3,13,3.684989684979789],[169.23,-18.73,206,4.5,17,4.500856565026091],[181.28,-17.64,574,4.6,17,4.974215782096689],[181.4,-17.66,585,4.1,17,3.017014379868098],[169.33,-18.82,230,4.4,11,4.072543433248407],[176.78,-37.37,263,4.7,34,5.497358622606177],[186.1,-15.31,96,4.6,32,4.974215782096689],[179.82,-24.97,511,4.4,23,4.072543433248407],[186.04,-15.49,94,4.3,26,3.684989684979789],[169.41,-19.23,246,4.6,27,4.974215782096689],[182.3,-30.1,56,4.9,34,6.714488984246776],[181.7,-26.4,329,4.5,24,4.500856565026091],[166.32,-11.77,70,4.4,18,4.072543433248407],[180.08,-24.12,493,4.3,21,3.684989684979789],[185.25,-18.97,129,5.1,73,8.201095364995085],[182.35,-18.75,554,4.2,13,3.334316552046258],[184.42,-19.26,223,4,15,2.729907501657212],[173.2,-22.75,46,4.6,26,4.974215782096689],[180.67,-21.37,593,4.3,13,3.684989684979789],[182.16,-20.1,489,4.2,16,3.334316552046258],[182.13,-19.85,562,4.4,31,4.072543433248407],[181,-22.7,445,4.5,17,4.500856565026091],[180.6,-22.06,584,4,11,2.729907501657212],[181.35,-17.8,535,4.4,23,4.072543433248407],[179.2,-24.2,530,4.3,12,3.684989684979789],[181.55,-20.69,582,4.7,35,5.497358622606177],[182.4,-21.16,260,4.1,12,3.017014379868098],[172.38,-13.82,613,5,61,7.42065795512883],[166.22,-11.49,84,4.6,32,4.974215782096689],[181.41,-20.68,593,4.9,40,6.714488984246776],[184.93,-17.1,286,4.7,25,5.497358622606177],[181.6,-20.14,587,4.1,13,3.017014379868098],[179.62,-21.96,627,5,45,7.42065795512883],[181.86,-20.42,530,4.5,27,4.500856565026091],[187.81,-15.46,40,5.5,91,12.23459661321102],[185.8,-15.31,152,4,11,2.729907501657212],[184.35,-19.86,201,4.5,30,4.500856565026091],[166.2,-11.55,96,4.3,14,3.684989684979789],[179.99,-23.74,506,5.2,75,9.063612093757561],[181.23,-17.7,546,4.4,35,4.072543433248407],[180.04,-23.54,564,4.3,15,3.684989684979789],[184.7,-19.21,197,4.1,11,3.017014379868098],[167.06,-12.11,265,4.5,23,4.500856565026091],[181.71,-21.81,323,4.2,15,3.334316552046258],[181.11,-28.98,304,5.3,60,10.01684049873958],[180.21,-34.02,75,5.2,65,9.063612093757561],[180.99,-23.84,367,4.5,27,4.500856565026091],[182.38,-19.57,579,4.6,38,4.974215782096689],[183.4,-20.12,284,4.3,15,3.684989684979789],[181.7,-17.7,450,4,11,2.729907501657212],[184.31,-19.66,170,4.3,15,3.684989684979789],[170.5,-21.5,117,4.7,32,5.497358622606177],[179.96,-23.64,538,4.5,26,4.500856565026091],[186.3,-15.43,123,4.2,16,3.334316552046258],[186.44,-15.41,69,4.3,42,3.684989684979789],[167.53,-15.48,128,5.1,61,8.201095364995085],[167.06,-13.36,236,4.7,22,5.497358622606177],[182.02,-20.64,497,5.2,64,9.063612093757561],[169.71,-19.72,271,4.2,14,3.334316552046258],[185.26,-15.44,224,4.2,21,3.334316552046258],[182.4,-19.73,375,4,18,2.729907501657212],[181.11,-27.24,365,4.5,21,4.500856565026091],[183.41,-18.16,306,5.2,54,9.063612093757561],[166.54,-13.66,50,5.1,45,8.201095364995085],[179.92,-24.57,484,4.7,33,5.497358622606177]]}],"series":[{"type":"scatter","name":"quakes","symbolSize":"function(x) {c= String(x.value!=null ? x.value[5] : x.data!=null ? x.data[5] : x[5] ); return (parseFloat(c)*2);}","coordinateSystem":"leaflet"}],"leaflet":{"roam":true,"tiles":[{"urlTemplate":"https://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"}],"zoom":6,"center":[179.1387,-19.9468]}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.tooltip.formatter","opts.series.0.symbolSize"],"jsHooks":[]}
#------ Plugin 'world' with visualMap
set.seed(333)
cns <- data.frame(   
  val = runif(3, 1, 100),
  dim = runif(3, 1, 100),
  nam = c('Brazil','China','India')
)
cns |> group_by(nam) |> ec.init(load= 'world', timeline= list(s=TRUE),
  series.param= list(type='map', 
      encode=list(value='val', name='nam')),
  toolbox= list(feature= list(restore= list())),
  visualMap= list(calculable=TRUE, dimension=2)
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"timeline":{"s":true,"data":["Brazil","China","India"],"axisType":"category"},"toolbox":{"feature":{"restore":[]}},"visualMap":{"dimension":1,"min":2.991817211266607,"max":72.63218200579286,"calculable":true},"dataset":[{"dimensions":["val","dim","nam"],"source":[[47.23306561866775,57.55925278598443,"Brazil"],[9.375216445419937,2.991817211266607,"China"],[97.37504198844545,72.63218200579286,"India"]]},{"transform":{"type":"filter","config":{"dimension":"nam","=":"Brazil"}},"id":"Brazil"},{"transform":{"type":"filter","config":{"dimension":"nam","=":"China"}},"id":"China"},{"transform":{"type":"filter","config":{"dimension":"nam","=":"India"}},"id":"India"}],"legend":{"data":[{"name":"Brazil"},{"name":"China"},{"name":"India"}]},"geo":{"map":"world","roam":true},"options":[{"series":[{"type":"map","geoIndex":0,"datasetIndex":1}]},{"series":[{"type":"map","geoIndex":0,"datasetIndex":2}]},{"series":[{"type":"map","geoIndex":0,"datasetIndex":3}]}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}
#------ Plugin 'world' with lines and color coding
if (interactive()) {
flights <- NULL
flights <- try(read.csv(paste0('https://raw.githubusercontent.com/plotly/datasets/master/',
                               '2011_february_aa_flight_paths.csv')), silent = TRUE)
if (!is.null(flights)) {
  tmp <- data.frame(airport1 = unique(head(flights,10)$airport1),
                    color = c("#387e78","#eeb422","#d9534f",'magenta'))
  tmp <- head(flights,10) |> inner_join(tmp)    # add color by airport
  ec.init(load= 'world',
    geo= list(center= c(mean(flights$start_lon), mean(flights$start_lat)), 
            zoom= 7, map='world' ),
    series= list(list(
      type= 'lines', coordinateSystem= 'geo',
      data= lapply(ec.data(tmp, 'names'), function(x)
        list(coords = list(c(x$start_lon,x$start_lat),
                           c(x$end_lon,x$end_lat)),
             colr = x$color)
      ),
      lineStyle= list(curveness=0.3, width=3, color=ec.clmn('colr'))
    ))
  )
} }

#------ registerMap JSON
# registerMap supports also maps in SVG format, see website gallery
if (interactive()) {
json <- jsonlite::read_json("https://echarts.apache.org/examples/data/asset/geo/USA.json")
dusa <- USArrests
dusa$states <- row.names(dusa)
p <- ec.init(preset= FALSE,
   series= list(list(type= 'map', map= 'USA', roam= TRUE, zoom= 3, left= -100, top= -30,
       data= lapply(ec.data(dusa, 'names'), 
           function(x) list(name=x$states, value=x$UrbanPop))
   )),
   visualMap= list(type='continuous', calculable=TRUE, 
       inRange= list(color = rainbow(8)),
       min= min(dusa$UrbanPop), max= max(dusa$UrbanPop))
)
p$x$registerMap <- list(list(mapName= 'USA', geoJSON= json))
p
}

#------ locale
mo <- seq.Date(Sys.Date() - 444, Sys.Date(), by= "month")
df <- data.frame(date= mo, val= runif(length(mo), 1, 10))
p <- df |> ec.init(title= list(text= 'locale test'))
p$x$locale <- 'ZH'
p$x$renderer <- 'svg'
p

{"x":{"theme":"","draw":true,"renderer":"svg","locale":"ZH","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"title":{"text":"locale test"},"dataset":[{"dimensions":["date","val"],"source":[["2023-09-22",6.48454268835485],["2023-10-22",3.760474134702235],["2023-11-22",1.571588575141504],["2023-12-22",3.748181231319904],["2024-01-22",4.545117005007342],["2024-02-22",5.267960811499506],["2024-03-22",9.020999480504543],["2024-04-22",6.985103074694052],["2024-05-22",7.631484168116003],["2024-06-22",9.20321844285354],["2024-07-22",6.74064878304489],["2024-08-22",8.781862691277638],["2024-09-22",3.588176757795736],["2024-10-22",2.169632331002504],["2024-11-22",2.172840852290392]]}],"xAxis":{"show":true,"type":"time","name":"date"},"yAxis":{"show":true,"type":"value","name":"val"},"series":[{"type":"scatter"}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}

#------ Pie
isl <- data.frame(name=names(islands), value=islands) |> filter(value>100) |> arrange(value)

ec.init( preset= FALSE,
   title= list(text = "Landmasses over 60,000 mi\u00B2", left = 'center'),
   tooltip= list(trigger='item'),   #, formatter= ec.clmn()),
   series= list(list(type= 'pie', radius= '50%', 
                     data= ec.data(isl, 'names'), name='mi\u00B2'))
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"title":{"text":"Landmasses over 60,000 mi²","left":"center"},"tooltip":{"trigger":"item"},"series":[{"type":"pie","radius":"50%","data":[{"name":"Sumatra","value":183},{"name":"Baffin","value":184},{"name":"Madagascar","value":227},{"name":"Borneo","value":280},{"name":"New Guinea","value":306},{"name":"Greenland","value":840},{"name":"Australia","value":2968},{"name":"Europe","value":3745},{"name":"Antarctica","value":5500},{"name":"South America","value":6795},{"name":"North America","value":9390},{"name":"Africa","value":11506},{"name":"Asia","value":16988}],"name":"mi²"}]}},"evals":[],"jsHooks":[]}

#------ Liquidfill plugin
if (interactive()) {
  ec.init(load= 'liquid', preset=FALSE,
    series= list(list(
    type='liquidFill', data=c(0.66, 0.5, 0.4, 0.3),
    waveAnimation= FALSE, animationDuration=0, animationDurationUpdate=0))
  )
}


#------ Heatmap
times <- c(5,1,0,0,0,0,0,0,0,0,0,2,4,1,1,3,4,6,4,4,3,3,2,5,7,0,0,0,0,0,
           0,0,0,0,5,2,2,6,9,11,6,7,8,12,5,5,7,2,1,1,0,0,0,0,0,0,0,0,3,2,
           1,9,8,10,6,5,5,5,7,4,2,4,7,3,0,0,0,0,0,0,1,0,5,4,7,14,13,12,9,5,
           5,10,6,4,4,1,1,3,0,0,0,1,0,0,0,2,4,4,2,4,4,14,12,1,8,5,3,7,3,0,
           2,1,0,3,0,0,0,0,2,0,4,1,5,10,5,7,11,6,0,5,3,4,2,0,1,0,0,0,0,0,
           0,0,0,0,1,0,2,1,3,4,0,0,0,0,1,2,2,6)
df <- NULL; n <- 1; 
for(i in 0:6) { df <- rbind(df, data.frame(0:23, rep(i,24), times[n:(n+23)]));  n<-n+24  }
hours <- ec.data(df); hours <- hours[-1]    # remove columns row
times <- c('12a',paste0(1:11,'a'),'12p',paste0(1:11,'p'))
days <- c('Saturday','Friday','Thursday','Wednesday','Tuesday','Monday','Sunday')
ec.init(preset= FALSE,
  title= list(text='Punch Card Heatmap'),
  tooltip= list(position='top'),grid=list(height='50%',top='10%'),
  xAxis= list(type='category', data=times, splitArea=list(show=TRUE)),
  yAxis= list(type='category', data=days,  splitArea=list(show=TRUE)),
  visualMap= list(min=0,max=10,calculable=TRUE,orient='horizontal',left='center',bottom='15%'),
  series= list(list(name='Hours', type = 'heatmap', data= hours,label=list(show=TRUE),
                    emphasis=list(itemStyle=list(shadowBlur=10,shadowColor='rgba(0,0,0,0.5)'))))
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"title":{"text":"Punch Card Heatmap"},"tooltip":{"position":"top"},"grid":{"height":"50%","top":"10%"},"xAxis":{"type":"category","data":["12a","1a","2a","3a","4a","5a","6a","7a","8a","9a","10a","11a","12p","1p","2p","3p","4p","5p","6p","7p","8p","9p","10p","11p"],"splitArea":{"show":true}},"yAxis":{"type":"category","data":["Saturday","Friday","Thursday","Wednesday","Tuesday","Monday","Sunday"],"splitArea":{"show":true}},"visualMap":{"min":0,"max":10,"calculable":true,"orient":"horizontal","left":"center","bottom":"15%"},"series":[{"name":"Hours","type":"heatmap","data":[[1,0,1],[2,0,0],[3,0,0],[4,0,0],[5,0,0],[6,0,0],[7,0,0],[8,0,0],[9,0,0],[10,0,0],[11,0,2],[12,0,4],[13,0,1],[14,0,1],[15,0,3],[16,0,4],[17,0,6],[18,0,4],[19,0,4],[20,0,3],[21,0,3],[22,0,2],[23,0,5],[0,1,7],[1,1,0],[2,1,0],[3,1,0],[4,1,0],[5,1,0],[6,1,0],[7,1,0],[8,1,0],[9,1,0],[10,1,5],[11,1,2],[12,1,2],[13,1,6],[14,1,9],[15,1,11],[16,1,6],[17,1,7],[18,1,8],[19,1,12],[20,1,5],[21,1,5],[22,1,7],[23,1,2],[0,2,1],[1,2,1],[2,2,0],[3,2,0],[4,2,0],[5,2,0],[6,2,0],[7,2,0],[8,2,0],[9,2,0],[10,2,3],[11,2,2],[12,2,1],[13,2,9],[14,2,8],[15,2,10],[16,2,6],[17,2,5],[18,2,5],[19,2,5],[20,2,7],[21,2,4],[22,2,2],[23,2,4],[0,3,7],[1,3,3],[2,3,0],[3,3,0],[4,3,0],[5,3,0],[6,3,0],[7,3,0],[8,3,1],[9,3,0],[10,3,5],[11,3,4],[12,3,7],[13,3,14],[14,3,13],[15,3,12],[16,3,9],[17,3,5],[18,3,5],[19,3,10],[20,3,6],[21,3,4],[22,3,4],[23,3,1],[0,4,1],[1,4,3],[2,4,0],[3,4,0],[4,4,0],[5,4,1],[6,4,0],[7,4,0],[8,4,0],[9,4,2],[10,4,4],[11,4,4],[12,4,2],[13,4,4],[14,4,4],[15,4,14],[16,4,12],[17,4,1],[18,4,8],[19,4,5],[20,4,3],[21,4,7],[22,4,3],[23,4,0],[0,5,2],[1,5,1],[2,5,0],[3,5,3],[4,5,0],[5,5,0],[6,5,0],[7,5,0],[8,5,2],[9,5,0],[10,5,4],[11,5,1],[12,5,5],[13,5,10],[14,5,5],[15,5,7],[16,5,11],[17,5,6],[18,5,0],[19,5,5],[20,5,3],[21,5,4],[22,5,2],[23,5,0],[0,6,1],[1,6,0],[2,6,0],[3,6,0],[4,6,0],[5,6,0],[6,6,0],[7,6,0],[8,6,0],[9,6,0],[10,6,1],[11,6,0],[12,6,2],[13,6,1],[14,6,3],[15,6,4],[16,6,0],[17,6,0],[18,6,0],[19,6,0],[20,6,1],[21,6,2],[22,6,2],[23,6,6]],"label":{"show":true},"emphasis":{"itemStyle":{"shadowBlur":10,"shadowColor":"rgba(0,0,0,0.5)"}}}]}},"evals":[],"jsHooks":[]}

#------ Plugin 3D
if (interactive()) {
  data <- list()
  for(y in 1:dim(volcano)[2]) for(x in 1:dim(volcano)[1]) 
    data <- append(data, list(c(x, y, volcano[x,y])))
  ec.init(load= '3D',
          series= list(list(type= 'surface',  data= data))
  )
}


#------ 3D chart with custom item size
if (interactive()) {
iris |> group_by(Species) |>
  mutate(size= log(Petal.Width*10)) |>  # add size as 6th column
  ec.init(load= '3D',
          xAxis3D= list(name= 'Petal.Length'),
          yAxis3D= list(name= 'Sepal.Width'),
          zAxis3D= list(name= 'Sepal.Length'),
          legend= list(show= TRUE),
    series.param= list(symbolSize= ec.clmn(6, scale=10))
  )
}


#------ Surface data equation with JS code
if (interactive()) {
 ec.init(load= '3D',
   series= list(list(
     type= 'surface',
     equation= list(
       x = list(min= -3, max= 4, step= 0.05),
       y = list(min= -3, max= 3, step= 0.05),
       z = htmlwidgets::JS("function (x, y) {
                           return Math.sin(x * x + y * y) * x / Math.PI; }")
     )
   )))
}


#------ Surface with data from a data.frame
if (interactive()) {
  data <- expand.grid(
    x = seq(0, 2, by = 0.1),
    y = seq(0, 1, by = 0.1)
  ) |> mutate(z = x * (y ^ 2)) |> select(x,y,z)
  ec.init(load= '3D',
          series= list(list(
            type= 'surface',
            data= ec.data(data, 'values'))) )
}

 
#------ Band series with customization
dats <- as.data.frame(EuStockMarkets) |> mutate(day= 1:n()) |>
  # first column ('day') becomes X-axis by default
  dplyr::relocate(day) |> slice_head(n= 100)

# 1. with unnamed data
bands <- ecr.band(dats, 'DAX','FTSE', name= 'Ftse-Dax', 
            areaStyle= list(color='pink'))
ec.init(load= 'custom', 
  tooltip= list(trigger= 'axis'),
  legend= list(show= TRUE), xAxis= list(type= 'category'),
  dataZoom= list(type= 'slider', end= 50),
  series = append( bands,
    list(list(type= 'line', name= 'CAC', color= 'red', symbolSize= 1,
              data= ec.data(dats |> select(day,CAC), 'values')
    ))
  )
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"tooltip":{"trigger":"axis"},"legend":{"show":true},"xAxis":{"type":"category"},"dataZoom":{"type":"slider","end":50},"series":[{"type":"custom","renderItem":"riPolygon","data":[[100,2546.6],[99,2561.6],[98,2546.5],[97,2575.5],[96,2554.9],[95,2559],[94,2538],[93,2534.2],[92,2540.9],[91,2527.8],[90,2549.5],[89,2566],[88,2577.1],[87,2553.3],[86,2558.5],[85,2514.7],[84,2528.3],[83,2561.1],[82,2559.5],[81,2575.7],[80,2601.1],[79,2588.7],[78,2579],[77,2576.7],[76,2574.5],[75,2555],[74,2570.8],[73,2584.1],[72,2599.5],[71,2596.2],[70,2624.6],[69,2625.6],[68,2644.2],[67,2645.6],[66,2621.7],[65,2599],[64,2595.6],[63,2597.8],[62,2576.6],[61,2579.5],[60,2600.3],[59,2588.7],[58,2583.6],[57,2594.4],[56,2606],[55,2625.8],[54,2641.9],[53,2626.6],[52,2630.8],[51,2653.2],[50,2667.4],[49,2663.3],[48,2664.6],[47,2669],[46,2679.6],[45,2645.7],[44,2638.2],[43,2624.2],[42,2619.8],[41,2640.7],[40,2640.7],[39,2623],[38,2601.9],[37,2554.5],[36,2540.5],[35,2621],[34,2617.2],[33,2608.8],[32,2584.9],[31,2569.4],[30,2570.6],[29,2600.6],[28,2597.4],[27,2573.3],[26,2585.4],[25,2601.7],[24,2591.7],[23,2588.8],[22,2595.6],[21,2595],[20,2589.3],[19,2579.6],[18,2580.5],[17,2587.9],[16,2558.5],[15,2541.5],[14,2547.3],[13,2561],[12,2556.8],[11,2532.5],[10,2497.4],[9,2510.5],[8,2508.4],[7,2487.9],[6,2466.8],[5,2484.7],[4,2470.4],[3,2448.2],[2,2460.2],[1,2443.6],[1,1628.75],[2,1613.63],[3,1606.51],[4,1621.04],[5,1618.16],[6,1610.61],[7,1630.75],[8,1640.17],[9,1635.47],[10,1645.89],[11,1647.84],[12,1638.35],[13,1629.93],[14,1621.49],[15,1624.74],[16,1627.63],[17,1631.99],[18,1621.18],[19,1613.42],[20,1604.95],[21,1605.75],[22,1616.67],[23,1619.29],[24,1620.49],[25,1619.67],[26,1623.07],[27,1613.98],[28,1631.87],[29,1630.37],[30,1633.47],[31,1626.55],[32,1650.43],[33,1650.06],[34,1654.11],[35,1653.6],[36,1501.82],[37,1524.28],[38,1603.65],[39,1622.49],[40,1636.68],[41,1652.1],[42,1645.81],[43,1650.36],[44,1651.55],[45,1649.88],[46,1653.52],[47,1657.51],[48,1649.55],[49,1649.09],[50,1646.41],[51,1638.65],[52,1625.8],[53,1628.64],[54,1632.22],[55,1633.65],[56,1631.17],[57,1635.8],[58,1621.27],[59,1624.7],[60,1616.13],[61,1618.12],[62,1627.8],[63,1625.79],[64,1614.8],[65,1612.8],[66,1605.47],[67,1609.32],[68,1607.48],[69,1607.48],[70,1604.89],[71,1589.12],[72,1582.27],[73,1567.99],[74,1568.16],[75,1569.71],[76,1571.74],[77,1585.41],[78,1570.01],[79,1561.89],[80,1565.18],[81,1570.34],[82,1577],[83,1590.29],[84,1572.72],[85,1572.07],[86,1579.19],[87,1588.73],[88,1586.01],[89,1579.77],[90,1572.58],[91,1568.09],[92,1578.21],[93,1573.94],[94,1582.06],[95,1610.18],[96,1605.16],[97,1623.84],[98,1615.26],[99,1627.08],[100,1626.97]],"name":"Ftse-Dax","areaStyle":{"color":"pink"},"itemStyle":{"borderWidth":0.5},"boundaryGap":false},{"type":"line","name":"CAC","color":"red","symbolSize":1,"data":[{"value":[1,1772.8]},{"value":[2,1750.5]},{"value":[3,1718]},{"value":[4,1708.1]},{"value":[5,1723.1]},{"value":[6,1714.3]},{"value":[7,1734.5]},{"value":[8,1757.4]},{"value":[9,1754]},{"value":[10,1754.3]},{"value":[11,1759.8]},{"value":[12,1755.5]},{"value":[13,1758.1]},{"value":[14,1757.5]},{"value":[15,1763.5]},{"value":[16,1762.8]},{"value":[17,1768.9]},{"value":[18,1778.1]},{"value":[19,1780.1]},{"value":[20,1767.7]},{"value":[21,1757.9]},{"value":[22,1756.6]},{"value":[23,1754.7]},{"value":[24,1766.8]},{"value":[25,1766.5]},{"value":[26,1762.2]},{"value":[27,1759.5]},{"value":[28,1782.4]},{"value":[29,1789.5]},{"value":[30,1783.5]},{"value":[31,1780.4]},{"value":[32,1808.8]},{"value":[33,1820.3]},{"value":[34,1820.3]},{"value":[35,1820.3]},{"value":[36,1687.5]},{"value":[37,1725.6]},{"value":[38,1792.9]},{"value":[39,1819.1]},{"value":[40,1833.5]},{"value":[41,1853.4]},{"value":[42,1849.7]},{"value":[43,1851.8]},{"value":[44,1857.7]},{"value":[45,1864.3]},{"value":[46,1863.5]},{"value":[47,1873.2]},{"value":[48,1860.8]},{"value":[49,1868.7]},{"value":[50,1860.4]},{"value":[51,1855.9]},{"value":[52,1840.5]},{"value":[53,1842.6]},{"value":[54,1861.2]},{"value":[55,1876.2]},{"value":[56,1878.3]},{"value":[57,1878.4]},{"value":[58,1869.4]},{"value":[59,1880.4]},{"value":[60,1885.5]},{"value":[61,1888.4]},{"value":[62,1885.2]},{"value":[63,1877.9]},{"value":[64,1876.5]},{"value":[65,1883.8]},{"value":[66,1880.6]},{"value":[67,1887.4]},{"value":[68,1878.3]},{"value":[69,1867.1]},{"value":[70,1851.9]},{"value":[71,1843.6]},{"value":[72,1848.1]},{"value":[73,1843.4]},{"value":[74,1843.6]},{"value":[75,1833.8]},{"value":[76,1833.4]},{"value":[77,1856.9]},{"value":[78,1863.4]},{"value":[79,1855.5]},{"value":[80,1864.2]},{"value":[81,1846]},{"value":[82,1836.8]},{"value":[83,1830.4]},{"value":[84,1831.6]},{"value":[85,1834.8]},{"value":[86,1852.1]},{"value":[87,1849.8]},{"value":[88,1861.8]},{"value":[89,1856.7]},{"value":[90,1856.7]},{"value":[91,1841.5]},{"value":[92,1846.9]},{"value":[93,1836.1]},{"value":[94,1838.6]},{"value":[95,1857.6]},{"value":[96,1857.6]},{"value":[97,1858.4]},{"value":[98,1846.8]},{"value":[99,1868.5]},{"value":[100,1863.2]}]}],"yAxis":{"show":true}}},"evals":["opts.series.0.renderItem"],"jsHooks":[]}
# 2. with a dataset
# dats |> ec.init(load= 'custom', ...
#   + replace data=... with encode= list(x='day', y='CAC')


#------ Error Bars on grouped data
df <- mtcars |> group_by(cyl,gear) |> summarise(yy= round(mean(mpg),2)) |>
  mutate(low= round(yy-cyl*runif(1),2), 
         high= round(yy+cyl*runif(1),2))
#> `summarise()` has grouped output by 'cyl'. You can override using the `.groups`
#> argument.
df |> ec.init(load='custom', ctype='bar',
              xAxis= list(type='category'), tooltip= list(show=TRUE)) |>
  ecr.ebars( # name = 'eb',  # cannot have own name in grouped series
    encode= list(x='gear', y=c('yy','low','high')),
    tooltip = list(formatter=ec.clmn('high <b>%@</b><br>low <b>%@</b>', 'high','low')))

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"xAxis":{"type":"category","name":"gear"},"tooltip":{"show":true},"dataset":[{"dimensions":["cyl","gear","yy","low","high"],"source":[[4,3,21.5,21.21,23.57],[4,4,26.92,26.63,28.99],[4,5,28.2,27.91,30.27],[6,3,19.75,18.6,20.54],[6,4,19.75,18.6,20.54],[6,5,19.7,18.55,20.49],[8,3,15.05,12.91,17.29],[8,5,15.4,13.26,17.64]]},{"transform":{"type":"filter","config":{"dimension":"cyl","=":4}},"id":4},{"transform":{"type":"filter","config":{"dimension":"cyl","=":6}},"id":6},{"transform":{"type":"filter","config":{"dimension":"cyl","=":8}},"id":8}],"series":[{"type":"bar","datasetIndex":1,"name":"4","encode":{"x":1,"y":2}},{"type":"bar","datasetIndex":2,"name":"6","encode":{"x":1,"y":2}},{"type":"bar","datasetIndex":3,"name":"8","encode":{"x":1,"y":2}},{"type":"custom","datasetIndex":1,"encode":{"x":1,"y":[2,3,4]},"renderItem":"riErrBars","tooltip":{"formatter":"function(x) {var sprintf= (template, vals) => { j=0; if (template=='%@') return vals[j++]; return template.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => {   if (m=='%@') return vals[j++];   if (m=='%L@') return Number(vals[j++]).toLocaleString();   if (m=='%LR@') return Math.round(Number(vals[j++])).toLocaleString();   if (m=='%R@') return Math.round(Number(vals[j++]));   if (m=='%R2@') return Number(vals[j++]).toFixed(2);   if (m=='%M@') return x.marker; }); }; pos=['cyl','gear','yy','low','high'];  aa= Array.isArray(x) ? x : x.data; tmp= null; if (aa && aa instanceof Object && !Array.isArray(aa)) {  tmp= Object.keys(aa); if (tmp.length==1 && tmp[0]=='value') aa= x.data.value;} if (tmp && tmp.length>1)  vv=[x.data['high'],x.data['low']]; else {  if (!aa || !aa.length) return `no data`;  args= [`high`,`low`];   pos= args.map(z => pos.indexOf(z));  vv= pos.map(p => aa[p]); } vv= vv.map(e => isNaN(e) | !e ? e : e* 1 );c= sprintf(`high <b>%@<\/b><br>low <b>%@<\/b>`, vv); return c; }"},"name":"4","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6}},{"type":"custom","datasetIndex":2,"encode":{"x":1,"y":[2,3,4]},"renderItem":"riErrBars","tooltip":{"formatter":"function(x) {var sprintf= (template, vals) => { j=0; if (template=='%@') return vals[j++]; return template.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => {   if (m=='%@') return vals[j++];   if (m=='%L@') return Number(vals[j++]).toLocaleString();   if (m=='%LR@') return Math.round(Number(vals[j++])).toLocaleString();   if (m=='%R@') return Math.round(Number(vals[j++]));   if (m=='%R2@') return Number(vals[j++]).toFixed(2);   if (m=='%M@') return x.marker; }); }; pos=['cyl','gear','yy','low','high'];  aa= Array.isArray(x) ? x : x.data; tmp= null; if (aa && aa instanceof Object && !Array.isArray(aa)) {  tmp= Object.keys(aa); if (tmp.length==1 && tmp[0]=='value') aa= x.data.value;} if (tmp && tmp.length>1)  vv=[x.data['high'],x.data['low']]; else {  if (!aa || !aa.length) return `no data`;  args= [`high`,`low`];   pos= args.map(z => pos.indexOf(z));  vv= pos.map(p => aa[p]); } vv= vv.map(e => isNaN(e) | !e ? e : e* 1 );c= sprintf(`high <b>%@<\/b><br>low <b>%@<\/b>`, vv); return c; }"},"name":"6","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6}},{"type":"custom","datasetIndex":3,"encode":{"x":1,"y":[2,3,4]},"renderItem":"riErrBars","tooltip":{"formatter":"function(x) {var sprintf= (template, vals) => { j=0; if (template=='%@') return vals[j++]; return template.replace(/%@|%L@|%LR@|%R@|%R2@|%M@/g, (m) => {   if (m=='%@') return vals[j++];   if (m=='%L@') return Number(vals[j++]).toLocaleString();   if (m=='%LR@') return Math.round(Number(vals[j++])).toLocaleString();   if (m=='%R@') return Math.round(Number(vals[j++]));   if (m=='%R2@') return Number(vals[j++]).toFixed(2);   if (m=='%M@') return x.marker; }); }; pos=['cyl','gear','yy','low','high'];  aa= Array.isArray(x) ? x : x.data; tmp= null; if (aa && aa instanceof Object && !Array.isArray(aa)) {  tmp= Object.keys(aa); if (tmp.length==1 && tmp[0]=='value') aa= x.data.value;} if (tmp && tmp.length>1)  vv=[x.data['high'],x.data['low']]; else {  if (!aa || !aa.length) return `no data`;  args= [`high`,`low`];   pos= args.map(z => pos.indexOf(z));  vv= pos.map(p => aa[p]); } vv= vv.map(e => isNaN(e) | !e ? e : e* 1 );c= sprintf(`high <b>%@<\/b><br>low <b>%@<\/b>`, vv); return c; }"},"name":"8","z":3,"itemStyle":{"borderWidth":1.5,"color":"brown","borderDashOffset":6}}],"legend":{"data":[{"name":"4"},{"name":"6"},{"name":"8"}]},"yAxis":{"show":true,"type":"value","name":"yy"}},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":["opts.series.3.renderItem","opts.series.3.tooltip.formatter","opts.series.4.renderItem","opts.series.4.tooltip.formatter","opts.series.5.renderItem","opts.series.5.tooltip.formatter"],"jsHooks":[]}

#------ Timeline animation and use of ec.upd for readability
Orange |> group_by(age) |> ec.init(
  xAxis= list(type= 'category', name= 'tree'),
  yAxis= list(max= max(Orange$circumference)),
  timeline= list(autoPlay= TRUE),
  series.param= list(type= 'bar', encode= list(x='Tree', y='circumference'))
) |> ec.upd({
  options <- lapply(options,
     function(o) {
       vv <- o$series[[1]]$datasetIndex +1;
       vv <- dataset[[vv]]$transform$config[["="]]
       o$title$text <- paste('age',vv,'days'); 
       o })
})

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"xAxis":{"type":"category","name":"tree"},"yAxis":{"max":214,"name":"circumference","type":"value"},"timeline":{"autoPlay":true,"data":["118","484","664","1004","1231","1372","1582"],"axisType":"category"},"dataset":[{"dimensions":["Tree","age","circumference"],"source":[["1",118,30],["1",484,58],["1",664,87],["1",1004,115],["1",1231,120],["1",1372,142],["1",1582,145],["2",118,33],["2",484,69],["2",664,111],["2",1004,156],["2",1231,172],["2",1372,203],["2",1582,203],["3",118,30],["3",484,51],["3",664,75],["3",1004,108],["3",1231,115],["3",1372,139],["3",1582,140],["4",118,32],["4",484,62],["4",664,112],["4",1004,167],["4",1231,179],["4",1372,209],["4",1582,214],["5",118,30],["5",484,49],["5",664,81],["5",1004,125],["5",1231,142],["5",1372,174],["5",1582,177]]},{"transform":{"type":"filter","config":{"dimension":"age","=":118}},"id":118},{"transform":{"type":"filter","config":{"dimension":"age","=":484}},"id":484},{"transform":{"type":"filter","config":{"dimension":"age","=":664}},"id":664},{"transform":{"type":"filter","config":{"dimension":"age","=":1004}},"id":1004},{"transform":{"type":"filter","config":{"dimension":"age","=":1231}},"id":1231},{"transform":{"type":"filter","config":{"dimension":"age","=":1372}},"id":1372},{"transform":{"type":"filter","config":{"dimension":"age","=":1582}},"id":1582}],"legend":{"data":[{"name":"118"},{"name":"484"},{"name":"664"},{"name":"1004"},{"name":"1231"},{"name":"1372"},{"name":"1582"}]},"options":[{"series":[{"datasetIndex":1,"type":"bar","encode":{"x":"Tree","y":"circumference"}}],"title":{"text":"age 118 days"}},{"series":[{"datasetIndex":2,"type":"bar","encode":{"x":"Tree","y":"circumference"}}],"title":{"text":"age 484 days"}},{"series":[{"datasetIndex":3,"type":"bar","encode":{"x":"Tree","y":"circumference"}}],"title":{"text":"age 664 days"}},{"series":[{"datasetIndex":4,"type":"bar","encode":{"x":"Tree","y":"circumference"}}],"title":{"text":"age 1004 days"}},{"series":[{"datasetIndex":5,"type":"bar","encode":{"x":"Tree","y":"circumference"}}],"title":{"text":"age 1231 days"}},{"series":[{"datasetIndex":6,"type":"bar","encode":{"x":"Tree","y":"circumference"}}],"title":{"text":"age 1372 days"}},{"series":[{"datasetIndex":7,"type":"bar","encode":{"x":"Tree","y":"circumference"}}],"title":{"text":"age 1582 days"}}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}

#------ Timeline with pies
df <- data.frame(
  group= c(1,1,1,1,2,2,2,2),
  type=  c("type1","type1","type2","type2","type1","type1","type2","type2"),
  value= c(5,2,2,1,4,3,1,4),
  label= c("name1","name2","name3","name4","name1","name2","name3","name4"),
  color= c("blue","purple","red","gold","blue","purple","red","gold")
)
df |> group_by(group) |> ec.init( 
     preset= FALSE, legend= list(selectedMode= "single"),
     timeline= list(show=TRUE),
     series.param= list(type= 'pie', roseType= 'radius',
       itemStyle= list(color=ec.clmn(5)), 
       label= list(formatter=ec.clmn(4)),
       encode=list(value='value', itemName='type'))
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"legend":{"selectedMode":"single"},"timeline":{"show":true},"dataset":[{"dimensions":["group","type","value","label","color"],"source":[[1,"type1",5,"name1","blue"],[1,"type1",2,"name2","purple"],[1,"type2",2,"name3","red"],[1,"type2",1,"name4","gold"],[2,"type1",4,"name1","blue"],[2,"type1",3,"name2","purple"],[2,"type2",1,"name3","red"],[2,"type2",4,"name4","gold"]]},{"transform":{"type":"filter","config":{"dimension":"group","=":1}},"id":1},{"transform":{"type":"filter","config":{"dimension":"group","=":2}},"id":2}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}

#------ Boxplot without grouping
ds <- mtcars |> select(cyl, drat) |>
   ec.data(format='boxplot', jitter=0.1, #layout= 'h',
      symbolSize=5, itemStyle=list(opacity=0.9), 
        emphasis= list(itemStyle= list(
           color= 'chartreuse', borderWidth=4, opacity=1))
  )
ec.init(
  #colors= heat.colors(length(mcyl)),
  legend= list(show= TRUE), tooltip= list(show=TRUE),
  dataset= ds$dataset, series= ds$series, xAxis= ds$xAxis, yAxis= ds$yAxis,
  series.param= list(color= 'LightGrey', itemStyle= list(color='DimGray'))
) |> ec.theme('dark-mushroom')

{"x":{"theme":"dark-mushroom","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"legend":{"show":true},"tooltip":{"show":true},"dataset":[{"source":[[3.85,3.69,3.92,4.08,4.93,4.22,3.7,4.08,4.43,3.77,4.11],[3.9,3.9,3.08,2.76,3.92,3.92,3.62],[3.15,3.21,3.07,3.07,3.07,2.93,3,3.23,2.76,3.15,3.73,3.08,4.22,3.54]]},{"transform":{"type":"boxplot","config":{"itemNameFormatter":"(p) => ['4','6','8'][p.value]"}}}],"series":[{"type":"boxplot","name":"boxplot","datasetIndex":1,"encode":{"tooltip":["Low","Q1","Q2","Q3","High"]},"color":"LightGrey","itemStyle":{"color":"DimGray"},"coordinateSystem":"cartesian2d"},{"type":"scatter","jitter":0.1,"symbolSize":5,"emphasis":{"itemStyle":{"color":"chartreuse","borderWidth":4,"opacity":1}},"name":"4","large":true,"z":3,"data":[[3.85,0.4374098815023899],[3.69,0.441033821972087],[3.92,0.5695795671083033],[4.08,0.5088060744572431],[4.93,0.4977616367861629],[4.22,0.5284685384947807],[3.7,0.4888973274733871],[4.08,0.5042826620861888],[4.43,0.4909801453817636],[3.77,0.5828898282721638],[4.11,0.4195588378235698]],"yAxisIndex":1,"color":"LightGrey","itemStyle":{"color":"DimGray"}},{"type":"scatter","jitter":0.1,"symbolSize":5,"emphasis":{"itemStyle":{"color":"chartreuse","borderWidth":4,"opacity":1}},"name":"6","large":true,"z":3,"data":[[3.9,1.470360603369772],[3.9,1.471299200691283],[3.08,1.44799889177084],[2.76,1.589832419203594],[3.92,1.566639425372705],[3.92,1.48746474715881],[3.62,1.447760090464726]],"yAxisIndex":1,"color":"LightGrey","itemStyle":{"color":"DimGray"}},{"type":"scatter","jitter":0.1,"symbolSize":5,"emphasis":{"itemStyle":{"color":"chartreuse","borderWidth":4,"opacity":1}},"name":"8","large":true,"z":3,"data":[[3.15,2.41174493143335],[3.21,2.495281379017979],[3.07,2.519692929228768],[3.07,2.481735875317827],[3.07,2.592519412562251],[2.93,2.551106295082718],[3,2.464575633406639],[3.23,2.582516251457855],[2.76,2.52844294430688],[3.15,2.592634053668007],[3.73,2.431809643190354],[3.08,2.531871907226741],[4.22,2.592210751539096],[3.54,2.402868018345907]],"yAxisIndex":1,"color":"LightGrey","itemStyle":{"color":"DimGray"}}],"xAxis":[{"scale":true,"name":"drat"}],"yAxis":[{"type":"category","name":"cyl"},{"max":3,"show":false}]}},"evals":["opts.dataset.1.transform.config.itemNameFormatter"],"jsHooks":[]}

#------ Boxplot with grouping
ds = airquality |> mutate(Day=round(Day/10)) |> 
  dplyr::relocate(Day,Wind,Month) |> group_by(Month) |>
   ec.data(format='boxplot', jitter=0.1, layout= 'h')
ec.init(
  dataset= ds$dataset, series= ds$series,xAxis= ds$xAxis, yAxis= ds$yAxis,
  legend= list(show= TRUE), tooltip= list(show=TRUE)
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"dataset":[{"source":[[7.4,8,12.6,11.5,14.3],[14.9,8.6,13.8,20.1,8.6,6.9,9.699999999999999,9.199999999999999,10.9],[13.2,11.5,12,18.4,11.5,9.699999999999999,9.699999999999999,16.6,9.699999999999999,12,16.6],[14.9,8,12,14.9,5.7,7.4]]},{"source":[[8.6,9.699999999999999,16.1,9.199999999999999,8.6],[14.3,9.699999999999999,6.9,13.8,11.5,10.9,9.199999999999999,8,13.8],[11.5,14.9,20.7,9.199999999999999,11.5,10.3,6.3,1.7,4.6,6.3,8],[8,10.3,11.5,14.9,8]]},{"source":[[4.1,9.199999999999999,9.199999999999999,10.9,4.6],[10.9,5.1,6.3,5.7,7.4,8.6,14.3,14.9,14.9],[14.3,6.9,10.3,6.3,5.1,11.5,6.9,9.699999999999999,11.5,8.6,8],[8.6,12,7.4,7.4,7.4,9.199999999999999]]},{"source":[[6.9,13.8,7.4,6.9,7.4],[4.6,4,10.3,8,8.6,11.5,11.5,11.5,9.699999999999999],[11.5,10.3,6.3,7.4,10.9,10.3,15.5,14.3,12.6,9.699999999999999,3.4],[8,5.7,9.699999999999999,2.3,6.3,6.3]]},{"source":[[6.9,5.1,2.8,4.6,7.4],[15.5,10.9,10.3,10.9,9.699999999999999,14.9,15.5,6.3,10.9],[11.5,6.9,13.8,10.3,10.3,8,12.6,9.199999999999999,10.3,10.3,16.6],[6.9,13.2,14.3,8,11.5]]},{"fromDatasetIndex":0,"transform":{"type":"boxplot"}},{"fromDatasetIndex":1,"transform":{"type":"boxplot"}},{"fromDatasetIndex":2,"transform":{"type":"boxplot"}},{"fromDatasetIndex":3,"transform":{"type":"boxplot"}},{"fromDatasetIndex":4,"transform":{"type":"boxplot"}}],"series":[{"name":5,"encode":{"tooltip":["Low","Q1","Q2","Q3","High"]},"type":"boxplot","datasetIndex":5,"coordinateSystem":"cartesian2d"},{"name":6,"encode":{"tooltip":["Low","Q1","Q2","Q3","High"]},"type":"boxplot","datasetIndex":6,"coordinateSystem":"cartesian2d"},{"name":7,"encode":{"tooltip":["Low","Q1","Q2","Q3","High"]},"type":"boxplot","datasetIndex":7,"coordinateSystem":"cartesian2d"},{"name":8,"encode":{"tooltip":["Low","Q1","Q2","Q3","High"]},"type":"boxplot","datasetIndex":8,"coordinateSystem":"cartesian2d"},{"name":9,"encode":{"tooltip":["Low","Q1","Q2","Q3","High"]},"type":"boxplot","datasetIndex":9,"coordinateSystem":"cartesian2d"},{"type":"scatter","jitter":0.1,"layout":"h","name":"0","large":true,"z":3,"data":[[7.4,0.5649348780047149],[8,0.4482186965178698],[12.6,0.4779113424941898],[11.5,0.5235719233285636],[14.3,0.4794015309773386],[8.6,0.5749637642875314],[9.699999999999999,0.4820341441314667],[16.1,0.4762925378978252],[9.199999999999999,0.4269003714900464],[8.6,0.4584145614877343],[4.1,0.4895307634957135],[9.199999999999999,0.4699938002042472],[9.199999999999999,0.5408015524968505],[10.9,0.5217737669590861],[4.6,0.4102198396809399],[6.9,0.5120703286956996],[13.8,0.4307911094743758],[7.4,0.4666191508527845],[6.9,0.5970116352662445],[7.4,0.406141440756619],[6.9,0.4433785846456885],[5.1,0.5060846041422338],[2.8,0.4855665843468159],[4.6,0.481781518086791],[7.4,0.5950098725501448]],"yAxisIndex":1},{"type":"scatter","jitter":0.1,"layout":"h","name":"1","large":true,"z":3,"data":[[14.9,1.524910907726735],[8.6,1.565500794351101],[13.8,1.411123101226985],[20.1,1.519930114597082],[8.6,1.48556293239817],[6.9,1.568114725360647],[9.699999999999999,1.592093579238281],[9.199999999999999,1.543853415129706],[10.9,1.509072202723473],[14.3,1.529997843969613],[9.699999999999999,1.477016041008756],[6.9,1.404040939453989],[13.8,1.59205854581669],[11.5,1.412272387789562],[10.9,1.592963976878673],[9.199999999999999,1.478301117103547],[8,1.486731585813686],[13.8,1.410988914314657],[10.9,1.578549987543374],[5.1,1.452944543026388],[6.3,1.522826346941292],[5.7,1.436987682431936],[7.4,1.495715543162078],[8.6,1.422453434253111],[14.3,1.505923212040216],[14.9,1.518266140576452],[14.9,1.49089255426079],[4.6,1.584281397936866],[4,1.415162469772622],[10.3,1.421445958968252],[8,1.473229450406507],[8.6,1.581304082321003],[11.5,1.533680378040299],[11.5,1.565669737290591],[11.5,1.523045716341585],[9.699999999999999,1.495308610936627],[15.5,1.438988943351433],[10.9,1.597833096981049],[10.3,1.570943973818794],[10.9,1.430493742367253],[9.699999999999999,1.58227532110177],[14.9,1.476156639307737],[15.5,1.511666337447241],[6.3,1.582458850834519],[10.9,1.55830879220739]],"yAxisIndex":1},{"type":"scatter","jitter":0.1,"layout":"h","name":"2","large":true,"z":3,"data":[[13.2,2.511897438764572],[11.5,2.412761038122698],[12,2.554112134408206],[18.4,2.404638435319066],[11.5,2.431273262295872],[9.699999999999999,2.58613011254929],[9.699999999999999,2.57683590981178],[16.6,2.409563561342657],[9.699999999999999,2.482879488449544],[12,2.425753170065582],[16.6,2.541817742306739],[11.5,2.444515778776258],[14.9,2.422605832340196],[20.7,2.45870299260132],[9.199999999999999,2.519034698838368],[11.5,2.441131954919547],[10.3,2.440867575723678],[6.3,2.518479074025527],[1.7,2.459001657553017],[4.6,2.446269546030089],[6.3,2.542517377436161],[8,2.470540576288477],[14.3,2.491609837766737],[6.9,2.44901534281671],[10.3,2.517109263129532],[6.3,2.590044228034094],[5.1,2.488243650179356],[11.5,2.560992124769837],[6.9,2.540305356308818],[9.699999999999999,2.528776411851868],[11.5,2.555002425145358],[8.6,2.532481737900525],[8,2.4201080232393],[11.5,2.564248013822362],[10.3,2.484105567308143],[6.3,2.423091737646609],[7.4,2.588277657376602],[10.9,2.415241212258115],[10.3,2.474630958819762],[15.5,2.467529328446835],[14.3,2.427955927466974],[12.6,2.518469676328823],[9.699999999999999,2.577402457315475],[3.4,2.512227683095261],[11.5,2.574471351318061],[6.9,2.539962285710499],[13.8,2.436102973902598],[10.3,2.507250686921179],[10.3,2.513503108872101],[8,2.418206842243671],[12.6,2.442835683608428],[9.199999999999999,2.565737249562517],[10.3,2.446413383260369],[10.3,2.522717388765886],[16.6,2.408161667315289]],"yAxisIndex":1},{"type":"scatter","jitter":0.1,"layout":"h","name":"3","large":true,"z":3,"data":[[14.9,3.565568578708917],[8,3.588345461059362],[12,3.586102445842698],[14.9,3.428722152393311],[5.7,3.468181739095598],[7.4,3.551575601100922],[8,3.49706057771109],[10.3,3.419490188779309],[11.5,3.527461167890579],[14.9,3.558924597268924],[8,3.406893318053335],[8.6,3.404187520314008],[12,3.497597485873848],[7.4,3.530253436835483],[7.4,3.52997305332683],[7.4,3.534529692213982],[9.199999999999999,3.487594704143703],[8,3.501079880353063],[5.7,3.539384345291182],[9.699999999999999,3.573650869121775],[2.3,3.451375026116148],[6.3,3.594912756513804],[6.3,3.596731647942215],[6.9,3.529427269659936],[13.2,3.492863710317761],[14.3,3.410644552204758],[8,3.419709953851998],[11.5,3.464272750774398]],"yAxisIndex":1}],"xAxis":[{"scale":true,"name":"Wind"}],"yAxis":[{"type":"category","name":"Day"},{"max":4,"show":false}],"legend":{"show":true},"tooltip":{"show":true}}},"evals":[],"jsHooks":[]}

#------ ecStat plugin: dataset transform to regression line
# presets for xAxis,yAxis,dataset and series are used
data.frame(x= 1:10, y= sample(1:100,10)) |>
ec.init(load= 'ecStat',
  js= c('echarts.registerTransform(ecStat.transform.regression)','','')) |>
ec.upd({
  dataset[[2]] <- list(
     transform= list(type= 'ecStat:regression', 
                    config= list(method= 'polynomial', order= 3)))
  series[[2]] <- list(
    type= 'line', itemStyle=list(color= 'red'), datasetIndex= 1)
})

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":["echarts.registerTransform(ecStat.transform.regression)","",""],"dbg":false,"opts":{"dataset":[{"dimensions":["x","y"],"source":[[1,13],[2,53],[3,56],[4,63],[5,80],[6,93],[7,36],[8,62],[9,28],[10,6]]},{"transform":{"type":"ecStat:regression","config":{"method":"polynomial","order":3}}}],"xAxis":{"show":true,"type":"value","name":"x"},"yAxis":{"show":true,"type":"value","name":"y"},"series":[{"type":"scatter"},{"type":"line","itemStyle":{"color":"red"},"datasetIndex":1}]},"settings":{"crosstalk_key":null,"crosstalk_group":null}},"evals":[],"jsHooks":[]}

#------ ECharts: dataset, transform and sort
datset <- list(
  list(source=list(
    list('name', 'age', 'profession', 'score', 'date'),
    list('Hannah Krause', 41, 'Engineer', 314, '2011-02-12'),
    list('Zhao Qian', 20, 'Teacher', 351, '2011-03-01'),
    list('Jasmin Krause', 52, 'Musician', 287, '2011-02-14'),
    list('Li Lei', 37, 'Teacher', 219, '2011-02-18'),
    list('Karle Neumann', 25, 'Engineer', 253, '2011-04-02'),
    list('Adrian Groß', 19, 'Teacher', NULL, '2011-01-16'),
    list('Mia Neumann', 71, 'Engineer', 165, '2011-03-19'),
    list('Böhm Fuchs', 36, 'Musician', 318, '2011-02-24'),
    list('Han Meimei', 67, 'Engineer', 366, '2011-03-12'))),
  list(transform = list(type= 'sort', config=list(
    list(dimension='profession', order='desc'),
    list(dimension='score', order='desc'))
  )))
ec.init(
  title= list(
    text= 'Data transform, multiple-sort bar',
    subtext= 'JS source',
    sublink= paste0('https://echarts.apache.org/next/examples/en/editor.html',
                    '?c=doc-example/data-transform-multiple-sort-bar'),
    left= 'center'),
  tooltip= list(trigger= 'item', axisPointer= list(type= 'shadow')),
  dataset= datset,
  xAxis= list(type= 'category', axisLabel= list(interval=0, rotate=30)),
  yAxis= list(name= 'score'),
  series= list(list(
    type= 'bar',
    label= list(show= TRUE, rotate= 90, position= 'insideBottom',
                align= 'left', verticalAlign= 'middle'),
    itemStyle =list(color= htmlwidgets::JS("function (params) {
        return ({
          Engineer: '#5470c6',
          Teacher: '#91cc75',
          Musician: '#fac858'
        })[params.data[2]]
      }")),
    encode= list(x= 'name', y= 'score', label= list('profession') ),
    datasetIndex= 1
  ))
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"title":{"text":"Data transform, multiple-sort bar","subtext":"JS source","sublink":"https://echarts.apache.org/next/examples/en/editor.html?c=doc-example/data-transform-multiple-sort-bar","left":"center"},"tooltip":{"trigger":"item","axisPointer":{"type":"shadow"}},"dataset":[{"source":[["name","age","profession","score","date"],["Hannah Krause",41,"Engineer",314,"2011-02-12"],["Zhao Qian",20,"Teacher",351,"2011-03-01"],["Jasmin Krause",52,"Musician",287,"2011-02-14"],["Li Lei",37,"Teacher",219,"2011-02-18"],["Karle Neumann",25,"Engineer",253,"2011-04-02"],["Adrian Groß",19,"Teacher",null,"2011-01-16"],["Mia Neumann",71,"Engineer",165,"2011-03-19"],["Böhm Fuchs",36,"Musician",318,"2011-02-24"],["Han Meimei",67,"Engineer",366,"2011-03-12"]]},{"transform":{"type":"sort","config":[{"dimension":"profession","order":"desc"},{"dimension":"score","order":"desc"}]}}],"xAxis":{"type":"category","axisLabel":{"interval":0,"rotate":30}},"yAxis":{"name":"score"},"series":[{"type":"bar","label":{"show":true,"rotate":90,"position":"insideBottom","align":"left","verticalAlign":"middle"},"itemStyle":{"color":"function (params) {\n        return ({\n          Engineer: '#5470c6',\n          Teacher: '#91cc75',\n          Musician: '#fac858'\n        })[params.data[2]]\n      }"},"encode":{"x":"name","y":"score","label":["profession"]},"datasetIndex":0}]}},"evals":["opts.series.0.itemStyle.color"],"jsHooks":[]}

#------ Sunburst
# see website for different ways to set hierarchical data
# https://helgasoft.github.io/echarty/uc3.html
data = list(list(name='Grandpa',children=list(list(name='Uncle Leo',value=15,
     children=list(list(name='Cousin Jack',value=2), list(name='Cousin Mary',value=5,
     children=list(list(name='Jackson',value=2))), list(name='Cousin Ben',value=4))), 
   list(name='Father',value=10,children=list(list(name='Me',value=5),
   list(name='Brother Peter',value=1))))), list(name='Nancy',children=list(
   list(name='Uncle Nike',children=list(list(name='Cousin Betty',value=1),
   list(name='Cousin Jenny',value=2))))))
ec.init( preset= FALSE,
         series= list(list(type= 'sunburst', data= data,
                           radius= list(0, '90%'),
                           label= list(rotate='radial') ))
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"series":[{"type":"sunburst","data":[{"name":"Grandpa","children":[{"name":"Uncle Leo","value":15,"children":[{"name":"Cousin Jack","value":2},{"name":"Cousin Mary","value":5,"children":[{"name":"Jackson","value":2}]},{"name":"Cousin Ben","value":4}]},{"name":"Father","value":10,"children":[{"name":"Me","value":5},{"name":"Brother Peter","value":1}]}]},{"name":"Nancy","children":[{"name":"Uncle Nike","children":[{"name":"Cousin Betty","value":1},{"name":"Cousin Jenny","value":2}]}]}],"radius":[0,"90%"],"label":{"rotate":"radial"}}]}},"evals":[],"jsHooks":[]}
#------ Gauge
ec.init(preset= FALSE,
        series= list(list(
          type = 'gauge', max = 160, min=40,
          detail = list(formatter='\U1F9E0={value}'),
          data = list(list(value=85, name='IQ test')) )) )

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"series":[{"type":"gauge","max":160,"min":40,"detail":{"formatter":"🧠={value}"},"data":[{"value":85,"name":"IQ test"}]}]}},"evals":[],"jsHooks":[]}

#------ Custom gauge with animation
jcode <- "setInterval(function () { 
    opts.series[0].data[0].value = (Math.random() * 100).toFixed(2) - 0;  
    chart.setOption(opts, true);}, 2000);"
ec.init(preset= FALSE, js= jcode,
        series= list(list(
          type= 'gauge',
          axisLine= list(lineStyle=list(width=30,
            color= list(c(0.3, '#67e0e3'),c(0.7, '#37a2da'),c(1, '#fd666d')))),
            pointer= list(itemStyle=list(color='auto')),
            axisTick= list(distance=-30,length=8, lineStyle=list(color='#fff',width=2)),
            splitLine= list(distance=-30,length=30, lineStyle=list(color='#fff',width=4)),
            axisLabel= list(color='auto',distance=40,fontSize=20),
            detail= list(valueAnimation=TRUE, formatter='{value} km/h',color='auto'),
            data= list(list(value=70))
)))

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":"setInterval(function () { \n    opts.series[0].data[0].value = (Math.random() * 100).toFixed(2) - 0;  \n    chart.setOption(opts, true);}, 2000);","dbg":false,"opts":{"series":[{"type":"gauge","axisLine":{"lineStyle":{"width":30,"color":[["0.3","#67e0e3"],["0.7","#37a2da"],["1","#fd666d"]]}},"pointer":{"itemStyle":{"color":"auto"}},"axisTick":{"distance":-30,"length":8,"lineStyle":{"color":"#fff","width":2}},"splitLine":{"distance":-30,"length":30,"lineStyle":{"color":"#fff","width":4}},"axisLabel":{"color":"auto","distance":40,"fontSize":20},"detail":{"valueAnimation":true,"formatter":"{value} km/h","color":"auto"},"data":[{"value":70}]}]}},"evals":[],"jsHooks":[]}

#------ Sankey and graph plots
sankey <- data.frame(
  name   = c("a","b", "c", "d", "e"),
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value  = c(5, 6, 2, 8, 13)
)
data <- ec.data(sankey, 'names')
ec.init(preset= FALSE,
  series= list(list( type= 'sankey',
    data= data,
    edges= data ))
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"series":[{"type":"sankey","data":[{"name":"a","source":"a","target":"b","value":5},{"name":"b","source":"b","target":"c","value":6},{"name":"c","source":"c","target":"d","value":2},{"name":"d","source":"d","target":"e","value":8},{"name":"e","source":"c","target":"e","value":13}],"edges":[{"name":"a","source":"a","target":"b","value":5},{"name":"b","source":"b","target":"c","value":6},{"name":"c","source":"c","target":"d","value":2},{"name":"d","source":"d","target":"e","value":8},{"name":"e","source":"c","target":"e","value":13}]}]}},"evals":[],"jsHooks":[]}

# graph plot with same data ---------------
ec.init(preset= FALSE,
        title= list(text= 'Graph'),
        tooltip= list(show= TRUE),
        series= list(list(
          type= 'graph',
          layout= 'force',   # try 'circular' too
          data= lapply(data,
             function(x) list(name= x$node, tooltip= list(show=FALSE))),
          edges= lapply(data,
             function(x) { x$lineStyle <- list(width=x$value); x }),
          emphasis= list(focus= 'adjacency',
                         label= list(position= 'right', show=TRUE)),
          label= list(show=TRUE), roam= TRUE, zoom= 4,
          tooltip= list(textStyle= list(color= 'blue')),
          lineStyle= list(curveness= 0.3) ))
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"title":{"text":"Graph"},"tooltip":{"show":true},"series":[{"type":"graph","layout":"force","data":[{"name":null,"tooltip":{"show":false}},{"name":null,"tooltip":{"show":false}},{"name":null,"tooltip":{"show":false}},{"name":null,"tooltip":{"show":false}},{"name":null,"tooltip":{"show":false}}],"edges":[{"name":"a","source":"a","target":"b","value":5,"lineStyle":{"width":5}},{"name":"b","source":"b","target":"c","value":6,"lineStyle":{"width":6}},{"name":"c","source":"c","target":"d","value":2,"lineStyle":{"width":2}},{"name":"d","source":"d","target":"e","value":8,"lineStyle":{"width":8}},{"name":"e","source":"c","target":"e","value":13,"lineStyle":{"width":13}}],"emphasis":{"focus":"adjacency","label":{"position":"right","show":true}},"label":{"show":true},"roam":true,"zoom":4,"tooltip":{"textStyle":{"color":"blue"}},"lineStyle":{"curveness":0.3}}]}},"evals":[],"jsHooks":[]}

#------ group connect 
main <- mtcars |> ec.init(height= 200, legend= list(show=FALSE),
     tooltip= list(axisPointer= list(axis='x')),
    series.param= list(name= "this legend is shared"))
main$x$group <- 'group1' # same group name for all charts
main$x$connect <- 'group1'
q1 <- main |> ec.upd({ series[[1]]$encode <- list(y='hp'); yAxis$name <- 'hp'
       legend <- list(show=TRUE)  # show first legend to share
})
q2 <- main |> ec.upd({ series[[1]]$encode <- list(y='wt'); yAxis$name <- 'wt' })
#if (interactive()) {   # browsable
  ec.util(cmd='layout', list(q1,q2), cols=2, title='group connect')

  
    group connect
  
  

    
      
      {"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"legend":{"show":true},"tooltip":{"axisPointer":{"axis":"x"}},"dataset":[{"dimensions":["mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"],"source":[[21,6,160,110,3.9,2.62,16.46,0,1,4,4],[21,6,160,110,3.9,2.875,17.02,0,1,4,4],[22.8,4,108,93,3.85,2.32,18.61,1,1,4,1],[21.4,6,258,110,3.08,3.215,19.44,1,0,3,1],[18.7,8,360,175,3.15,3.44,17.02,0,0,3,2],[18.1,6,225,105,2.76,3.46,20.22,1,0,3,1],[14.3,8,360,245,3.21,3.57,15.84,0,0,3,4],[24.4,4,146.7,62,3.69,3.19,20,1,0,4,2],[22.8,4,140.8,95,3.92,3.15,22.9,1,0,4,2],[19.2,6,167.6,123,3.92,3.44,18.3,1,0,4,4],[17.8,6,167.6,123,3.92,3.44,18.9,1,0,4,4],[16.4,8,275.8,180,3.07,4.07,17.4,0,0,3,3],[17.3,8,275.8,180,3.07,3.73,17.6,0,0,3,3],[15.2,8,275.8,180,3.07,3.78,18,0,0,3,3],[10.4,8,472,205,2.93,5.25,17.98,0,0,3,4],[10.4,8,460,215,3,5.424,17.82,0,0,3,4],[14.7,8,440,230,3.23,5.345,17.42,0,0,3,4],[32.4,4,78.7,66,4.08,2.2,19.47,1,1,4,1],[30.4,4,75.7,52,4.93,1.615,18.52,1,1,4,2],[33.9,4,71.09999999999999,65,4.22,1.835,19.9,1,1,4,1],[21.5,4,120.1,97,3.7,2.465,20.01,1,0,3,1],[15.5,8,318,150,2.76,3.52,16.87,0,0,3,2],[15.2,8,304,150,3.15,3.435,17.3,0,0,3,2],[13.3,8,350,245,3.73,3.84,15.41,0,0,3,4],[19.2,8,400,175,3.08,3.845,17.05,0,0,3,2],[27.3,4,79,66,4.08,1.935,18.9,1,1,4,1],[26,4,120.3,91,4.43,2.14,16.7,0,1,5,2],[30.4,4,95.09999999999999,113,3.77,1.513,16.9,1,1,5,2],[15.8,8,351,264,4.22,3.17,14.5,0,1,5,4],[19.7,6,145,175,3.62,2.77,15.5,0,1,5,6],[15,8,301,335,3.54,3.57,14.6,0,1,5,8],[21.4,4,121,109,4.11,2.78,18.6,1,1,4,2]]}],"xAxis":{"show":true,"type":"value","name":"mpg"},"yAxis":{"show":true,"type":"value","name":"hp"},"series":[{"type":"scatter","name":"this legend is shared","encode":{"y":"hp"}}]},"settings":{"crosstalk_key":null,"crosstalk_group":null},"group":"group1","connect":"group1"},"evals":[],"jsHooks":[]}

    
      
      {"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"legend":{"show":false},"tooltip":{"axisPointer":{"axis":"x"}},"dataset":[{"dimensions":["mpg","cyl","disp","hp","drat","wt","qsec","vs","am","gear","carb"],"source":[[21,6,160,110,3.9,2.62,16.46,0,1,4,4],[21,6,160,110,3.9,2.875,17.02,0,1,4,4],[22.8,4,108,93,3.85,2.32,18.61,1,1,4,1],[21.4,6,258,110,3.08,3.215,19.44,1,0,3,1],[18.7,8,360,175,3.15,3.44,17.02,0,0,3,2],[18.1,6,225,105,2.76,3.46,20.22,1,0,3,1],[14.3,8,360,245,3.21,3.57,15.84,0,0,3,4],[24.4,4,146.7,62,3.69,3.19,20,1,0,4,2],[22.8,4,140.8,95,3.92,3.15,22.9,1,0,4,2],[19.2,6,167.6,123,3.92,3.44,18.3,1,0,4,4],[17.8,6,167.6,123,3.92,3.44,18.9,1,0,4,4],[16.4,8,275.8,180,3.07,4.07,17.4,0,0,3,3],[17.3,8,275.8,180,3.07,3.73,17.6,0,0,3,3],[15.2,8,275.8,180,3.07,3.78,18,0,0,3,3],[10.4,8,472,205,2.93,5.25,17.98,0,0,3,4],[10.4,8,460,215,3,5.424,17.82,0,0,3,4],[14.7,8,440,230,3.23,5.345,17.42,0,0,3,4],[32.4,4,78.7,66,4.08,2.2,19.47,1,1,4,1],[30.4,4,75.7,52,4.93,1.615,18.52,1,1,4,2],[33.9,4,71.09999999999999,65,4.22,1.835,19.9,1,1,4,1],[21.5,4,120.1,97,3.7,2.465,20.01,1,0,3,1],[15.5,8,318,150,2.76,3.52,16.87,0,0,3,2],[15.2,8,304,150,3.15,3.435,17.3,0,0,3,2],[13.3,8,350,245,3.73,3.84,15.41,0,0,3,4],[19.2,8,400,175,3.08,3.845,17.05,0,0,3,2],[27.3,4,79,66,4.08,1.935,18.9,1,1,4,1],[26,4,120.3,91,4.43,2.14,16.7,0,1,5,2],[30.4,4,95.09999999999999,113,3.77,1.513,16.9,1,1,5,2],[15.8,8,351,264,4.22,3.17,14.5,0,1,5,4],[19.7,6,145,175,3.62,2.77,15.5,0,1,5,6],[15,8,301,335,3.54,3.57,14.6,0,1,5,8],[21.4,4,121,109,4.11,2.78,18.6,1,1,4,2]]}],"xAxis":{"show":true,"type":"value","name":"mpg"},"yAxis":{"show":true,"type":"value","name":"wt"},"series":[{"type":"scatter","name":"this legend is shared","encode":{"y":"wt"}}]},"settings":{"crosstalk_key":null,"crosstalk_group":null},"group":"group1","connect":"group1"},"evals":[],"jsHooks":[]}

  

#}


#------ Javascript execution: ec.init 'js' parameter demo
# in single item scenario (js=jcode), execution is same as j3 below
if (interactive()) {
 j1 <- "winvar= 'j1';" # set window variables
 j2 <- "opts.title.text= 'changed';"   # opts exposed
 j3 <- "ww= chart.getWidth(); alert('width:'+ww);"  # chart exposed
 ec.init(js= c(j1, j2, j3), title= list(text= 'Title'), 
   series.param= list(name='sname'),
   legend= list(formatter= ec.clmn("function(name) {
     return name +' - '+ this.winvar; }"))
 )
}

#------ echarty Javascript built-in functions
jtgl <- "() => { 
  ch1 = ec_chart(echwid);   // takes the auto-assigned id
//ch1 = ec_chart('myTree'); // manual id is OK too
  opts = ch1.getOption();
//opts = ec_option(echwid);  // for reading, without setOption
  opts.series[0].orient= opts.series[0].orient=='TB' ? 'LR':'TB';
  ch1.setOption(opts); }"
dbut <- ec.util(cmd='button', text='toggle', js=jtgl)
data <- list(list(name='root', children=list(list(name='A',value=1),list(name='B',value=3))))
ec.init( # elementId='myTree',
  series.param= list(type='tree', data=data), graphic= list(dbut)
)

{"x":{"theme":"","draw":true,"renderer":"canvas","locale":"EN","useDirtyRect":false,"jcode":null,"dbg":false,"opts":{"graphic":[{"type":"rect","right":40,"top":20,"zlevel":4,"shape":{"height":20,"width":60,"r":5},"style":{"fill":"lightgray"},"textContent":{"zlevel":4,"style":{"text":"toggle","fill":"black"}},"textConfig":{"position":"inside"},"onclick":"() => { \n  ch1 = ec_chart(echwid);   // takes the auto-assigned id\n//ch1 = ec_chart('myTree'); // manual id is OK too\n  opts = ch1.getOption();\n//opts = ec_option(echwid);  // for reading, without setOption\n  opts.series[0].orient= opts.series[0].orient=='TB' ? 'LR':'TB';\n  ch1.setOption(opts); }","text":"toggle"}],"series":[{"type":"tree","data":[{"name":"root","children":[{"name":"A","value":1},{"name":"B","value":3}]}]}]}},"evals":["opts.graphic.0.onclick"],"jsHooks":[]}
#------ Events in Shiny ----------
if (interactive()) {
  library(shiny); library(dplyr); library(echarty)

ui <- fluidPage(ecs.output('plot'), textOutput('out1') )
server <- function(input, output, session) {
  output$plot <- ecs.render({
    p <- mtcars |> group_by(cyl) |> ec.init(dataZoom= list(type= 'inside'))
    p$x$on <- list(                     # event(s) with Javascript handler
      list(event= 'legendselectchanged', 
        handler= htmlwidgets::JS("(e) => Shiny.setInputValue('lgnd', 'legend:'+e.name);"))
    )
    p$x$capture <- 'datazoom'
    p
  })
  observeEvent(input$plot_datazoom, {   # captured event
    output$out1 <- renderText({ 
      paste('Zoom.start:',input$plot_datazoom$batch[[1]]$start,'%') })
  })
  observeEvent(input$plot_mouseover, {  # built-in event
    v <- input$plot_mouseover
    output$out1 <- renderText({ paste('s:',v$seriesName,'d:',v$data[v$dataIndex+1]) })
  })
  observeEvent(input$lgnd, {            # reactive response to on:legend event
    output$out1 <- renderText({ input$lgnd })
  })
}
shinyApp(ui, server)
}

#------------- Shiny interactive charts demo ---------------
#  run command: demo(eshiny)

# }  # donttest
```
