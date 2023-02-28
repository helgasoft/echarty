# Use Case 4 - Sankey with timeline
<br />

Will sankey chart work with timeline ? An interesting question by [rdatasculptor](https://github.com/rdatasculptor).  
To speed things up, we go to echarty's examples. In the Console type **?ec.examples**, then in Help panel, hit Ctrl/F and seach for 'sankey'. We find the following code  
<br />

```r
#------ Sankey and graph plots
sankey <- data.frame(
  node   = c("a","b", "c", "d", "e"),
  source = c("a", "b", "c", "d", "c"),
  target = c("b", "c", "d", "e", "e"),
  value  = c(5, 6, 2, 8, 13)
)
data <- ec.data(sankey, 'names')

ec.init(preset= FALSE,
        series= list(list(
          type= 'sankey',
          data= lapply(data, function(x) list(name= x$node)),
          edges= data ))
)
```

Sankey chart data consists of nodes connected by edges having a value. If the data is defined as *data.frame*, we need to transform it into a list with command *ec.data()*. Once in that format, we can access the columns by index - *node* is value[1], ..., *value* is value[4]. The [edges](https://echarts.apache.org/en/option.html#series-sankey.edges) definition requires three parameters - source, target and value.  

Now let's add [timeline](https://echarts.apache.org/en/option.html#timeline). Timeline is just a collection of data states targeted to some chart. An animated display shows the chart transition from one state to the next.  
We will **not** try to change nodes or edges on each step, just **edge values**. Let's have three states(steps) and build three edge lists with slightly different values. That wraps up the data preparation.  

The GUI part starts with chart initialization with *ec.init()*, then setting chart parameters which here are series, timeline and options.  
Parameter *timeline* defines labels for the timeline legend.  
Parameter *options* (ill-named) defines the timeline series.  
Note: command *ec.init* can set timeline options through parameter *tl.series*, but that requires grouped dataframe data, which is not the case in this example.  
The complete code is below. We've added also series *levels* to fine-tune visuals on each step, especially colors. Without *levels* node colors would change on each step, which is confusing. 
<br />
<br />

```r
df <- data.frame(
    node = c("a","b", "c", "d", "e"),
    source = c("a", "b", "c", "d", "c"),
    target = c("b", "c", "d", "e", "e")
)
val <- list(c(5, 3, 2, 8, 13), 
			c(4, 5, 4, 7, 8), 
			c(2, 7, 6, 5, 4))

# prepare timeline state data
st <- \(i) {		# data.frame to list
	df$value <- unlist(val[i])
	ec.data(df, 'values')
}  
nodes <- lapply(st(1), \(x) list(name = x$value[1]))
edo <- \(x) list(source= as.character(x$value[2]),
				 target= as.character(x$value[3]), value= x$value[4])
options <- lapply(1:3, \(i) {
	edges <- lapply(st(i), edo)
	list(title= list(text= paste('step',i)), 
		 series= list(list(type='sankey', data= nodes, edges= edges)))
})
# optional levels to keep colors persistent
levcol = lapply(1:5, \(i) {
	list(depth=i, itemStyle=list(color= c('blue','red','green','brown','yellow')[i]))
})

ec.init(
	series = list(list(type='sankey', data= nodes, edges= edges[[1]], levels= levcol)),
	timeline = list(axisType='category', data= list('s1','s2','s3')),
	options = options
)

```

<img src="img/uc4-1.png" alt="sankey" />

<br/>
<br />

So the final answer is **yes** - sankey and timeline work together just fine.  
For more *sankey* customizations check [this ECharts example](https://echarts.apache.org/examples/en/editor.html?c=sankey-levels).  
<br/>


