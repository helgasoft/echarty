# Use Case 3 -  Hierarchies in R and echarty  

There are several types of charts to represent hierarchical structures - sunburst, tree, treemap, sankey, graph.
Hierarchical data in R can be built with:
- list of lists
- data.frame with nested children
- data.frame with parent-child

Many users find the latter representation most intuitive and straightforward. However note that it requires library *data.tree*.  
Note also the difference in data setting for chart *tree* compared to *sunburst* and *treemap*.  *Tree*'s data is the top-level node, while *sunburst* and *treemap* use the first lower level.
Here are examples for the three types.  
<br />
## List of lists
#
```r
data <- list(list(name='Grandpa',children=list(list(name='Uncle Leo',value=15,
     children=list(list(name='Cousin Jack',value=2), list(name='Cousin Mary',value=5,
     children=list(list(name='Jackson',value=2))), list(name='Cousin Ben',value=4))),
   list(name='Father',value=10,children=list(list(name='Me',value=5),
   list(name='Brother Peter',value=1))))), list(name='Nancy',children=list(
   list(name='Uncle Nike',children=list(list(name='Cousin Betty',value=1),
   list(name='Cousin Jenny',value=2))))))

p <- ec.init(preset=FALSE)
p$x$opts$series <- list(list(type = 'sunburst', 
  data = data, radius=list(0, '90%'), label=list(rotate='radial')))
p
```

## Data frame with nested children

```r
level1 <- data.frame(name = "Animals", value = 1)  # top level
level11 <- data.frame(name = c("Mammals", "Reptiles","Fish"), value = c(100,100,100))   # 2nd level 
level111 <- data.frame(name = c("Dogs", "Humans"), value = c( 15, 15))    # 3rd level
level112 <- data.frame(name = c("Snakes", "Lizards"), value = c(30, 40))
level113 <- data.frame(name = c("Sharks"), value = 30)
level1131 <- data.frame(name = c("hammerhead", "thresher"), value = c(10,20))   # 4th level
level113[1, "children"][[1]] <- list(level1131)
level11[1, "children"][[1]] <- list(level111)
level11[2, "children"][[1]] <- list(level112)
level11[3, "children"][[1]] <- list(level113)
level1[1, "children"][[1]] <- list(level11)

p <- ec.init(preset=FALSE)
p$x$opts$series <- list(list(type='tree', 
    data = jsonlite::toJSON(level1), 
    label = list(offset=c(0, -12)), 
    symbolSize = htmlwidgets::JS("function(d) { return d; }") ))  # size by value
p

p$x$opts$series <- list(list(type='treemap', 
    data=jsonlite::toJSON(level11), leafDepth=1)) 
p

p$x$opts$series <- list(list(type='sunburst', 
    data=jsonlite::toJSON(level11), 
    radius=c(0, '90%'), label=list(rotate='radial') ))
p

```
## Data frame with parent-child

```r
df <- data.frame(parents = c("","Reptiles", "Reptiles", "Mammals", "Mammals", "Fish", "Sharks", "Sharks", "Animals", "Animals", "Animals"),
                 children = c("Animals", "Snakes", "Lizards", "Dogs", "Humans", "Sharks", "hammerhead", "thresher", "Reptiles", "Mammals", "Fish"),
                 value = c(55, 30, 40, 15, 15, 30, 10, 20, 100, 100, 100)) 
library(data.tree)
tmp <- data.tree::FromDataFrameNetwork(df)
json <- data.tree::ToListExplicit(tmp, unname=TRUE)

p <- ec.init(preset=FALSE)
p$x$opts$series <- list(list(type='sunburst', 
    data=json$children[[1]]$children, 
    radius=c(0, '90%'), label=list(rotate='radial'), emphasis=list(focus='ancestor') ))
p

p$x$opts$series <- list(list(type='tree', 
    data=json$children, 
    label = list(offset = c(0, -12)), symbolSize = htmlwidgets::JS("function(d) { return d; }") ))
p

p$x$opts$series <- list(list(type='treemap', 
    data=json$children[[1]]$children, leafDepth=1))
p$x$opts$tooltip <- list(ey='')   # ey is a dummy parameter, we want the same empty object in JS - "tooltip:{ey:''}"
p
```

