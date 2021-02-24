# Use Case 4 - Timeline with sankey
<br />

Heard it through the grapevine - an interesting [question](https://twitter.com/rdatasculptor/status/1363235363200892930) by [rdatasculptor](https://twitter.com/rdatasculptor).  
Will sankey chart work with timeline ?  Never tried, let's do it now.  
To speed things up, we go to echarty's examples. In the Console type **?ec.examples**, then in Help panel, hit Ctrl/F and seach for 'sankey'. We find the following code  
<br />

```r
#------ Sankey and graph plots
# prepare data
sankey <- data.frame(
  node = c("a","b", "c", "d", "e"),
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value = c(5, 6, 2, 8, 13),
  stringsAsFactors = FALSE
)

p <- ec.init(preset=FALSE)
p$x$opts$series[[1]] <- list( type='sankey',
  data = lapply(ec.data(sankey,TRUE),
                function(x) list(name=x$value[1])),
  edges = lapply(ec.data(sankey,TRUE), function(x)
    list(source=as.character(x$value[2]), 
         target=as.character(x$value[3]), value=x$value[4]) ) 
)
p
```

Sankey chart data consists of nodes connected by edges having a value. If the data is defined as *data.frame*, we need to transform it into a list with command *ec.data()*. Once in that format, we can access the columns by index - *node* is value[1], ..., *value* is value[4]. The [edges](https://echarts.apache.org/en/option.html#series-sankey.edges) definition requires three parameters - source, target and value.  

Now let's add [timeline](https://echarts.apache.org/en/option.html#timeline). Timeline is just a collection of data states targeted to some chart. An animated display shows the chart transition from one state to the next.  
We will **not** try to change nodes or edges on each step, just **edge values**. Let's have three states(steps) and build three edge lists with slightly different values - edges1, edges2, edges3. That wraps up the data preparation.  

The GUI part starts with chart initialization with *ec.init()*, then setting chart parameters which here are series, timeline and options.  
Parameter *timeline* defines labels for the timeline legend.  
Parameter *options* (ill-named) defines the timeline data states.  
The complete code is below. We've added also series *levels* to fine-tune visuals on each step, especially colors. Without *levels* node colors would change on each step, which is confusing. 
<br />
<br />

```r
sankey <- data.frame(
  node = c("a","b", "c", "d", "e"),
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value = c(5, 3, 2, 8, 13),
  stringsAsFactors = FALSE
)
# prepare timeline state data
st <- function() ec.data(sankey, TRUE)  # data.frame to list
nodes <- lapply(st(), function(x) list(name = x$value[1]))
edo <- function(x) list(source=as.character(x$value[2]),  
                        target=as.character(x$value[3]), value=x$value[4])
edges1 <- lapply(st(), edo)
sankey$value <- c(4, 5, 4, 7, 8)
edges2 = lapply(st(), edo)
sankey$value <- c(2, 7, 6, 5, 4)
edges3 = lapply(st(), edo)

p <- ec.init(preset=FALSE)
p$x$opts <- list(
  series = list(list(type='sankey',  data = nodes,  edges = edges1,
     # optional levels to keep colors persistent
     levels = list(list(depth=0,itemStyle=list(color=list('blue'))),
                   list(depth=1,itemStyle=list(color=list('red'))),
                   list(depth=2,itemStyle=list(color=list('green'))),
                   list(depth=3,itemStyle=list(color=list('brown'))),
                   list(depth=4,itemStyle=list(color=list('yellow'))) ) 
  )),
  timeline = list(axisType='category', data=list('s1','s2','s3')),
  options = list(
    list(title=list(text='step1'), series=list(list(type='sankey', data=nodes, edges=edges1))),
    list(title=list(text='step2'), series=list(list(type='sankey', data=nodes, edges=edges2))),
    list(title=list(text='step3'), series=list(list(type='sankey', data=nodes, edges=edges3))))
)
p
```

<img src="img/uc4-1.png" alt="sankey" />

<br/>
<br />

So the final answer is **yes** - sankey and timeline work together just fine.  
For more *sankey* customizations check [this Echarts example](https://echarts.apache.org/examples/en/editor.html?c=sankey-levels).  
<br/>


