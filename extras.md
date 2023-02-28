# echarty extras 💲

Utilities and small applications built on top of open-source library _echarty_. They accomplish very specific and uncommon tasks.
Considerable amount of time was spent in design, development and testing of these modules.  
<br>
They are fairly priced at **$40 each** and payable through [**Paypal**](https://www.paypal.com/paypalme/helgasoft).  
Please include module description when ordering under "_What is this payment for ?_". Delivery and support by email.  
Future versions will be sent on demand (not automatically).  

<div style="display:none;">  The entire (all as a) bundle for $200.  
(not?) included in the price and code will be sent on demand.  
(external, enhancements, based on, ,  (not part of) favorably)
These utilities were developed ...with the intent of ( and destined to)    to help clients solve/achieve very specific and uncommon tasks/results.
A knowledgable soft eng could..   singular 
</div>
<br />

## Violin chart  
<br />  
Violin charts are similar to boxplot, but show the full distribution of the data. Supports single or grouped datasets.   
<details><summary> Parameters</summary>  

- <strong>df</strong> data.frame with X,Y values (required)
- <strong>cvalue</strong> name of values column (required)
- <strong>cname</strong> optional name of names(categories) column
- <strong>jitter</strong> value (>=0) of parameter _amount_ in jitter, default NULL(no jittered points)
- <strong>vertical</strong> boolean for vertical(default) or horizontal layout
- <strong>lineWidth</strong> width of the contour line, default 3
- <strong>d.*</strong> additional density parameters, like d.adjust,d.cut,etc. d.n default is 64
- <strong>j.size</strong> jitter points size, default 3
- <strong>j.color</strong> jitter points color, default 'black'
- <strong>wcoeff</strong> width coefficient for each violin, default is 0.45
- <strong>meanColor</strong> color of mean point, default NULL(no mean added)
- <strong>...</strong> additional attributes for the violin series
- <strong>returns</strong> a named list of xAxis, yAxis and series. Series are: custom for violin, optional scatter for jitter and mean.
</details>  
<br />

<video id="vidshp" preload="auto" 
   src="img/violin.mp4" type="video/mp4" muted="muted" controls>
   Your browser does not support the video tag.
</video>

<br />
  
## 3D regression surface
<br />

The familiar _quakes_ R dataset is visualized in 3D environment (lat,long,depth) over an area map with tectonic plate boundaries. 
User can show/hide 3D regression surface(s) built from selected 3D points.  
<br />
<video id="3dreg" preload="auto" 
   src="img/quake.mp4" type="video/mp4" muted="muted" controls>
   Your browser does not support the video tag.
</video>
<br />
<br />
## Morphing drilldown
<br />

The _mtcars_ R dataset presented with morphing drilldown. There are two levels of data:
- top level is an aggregate, like counts by type, or average by type
- lower level are the actual data rows

Morphing is achieved between two top levels, and between top and lower levels.  
<br />
<video id="3dreg" preload="auto" 
   src="img/morph.cars.mp4" type="video/mp4" muted="muted" controls>
   Your browser does not support the video tag.
</video>
<br />
<br />


## Eurobarometer Survey
<br />

Eurobarometer public opinion [survey Summer 2022](https://europa.eu/eurobarometer/surveys/detail/2693).  
180 questions with multiple answers mapped by country, 26K participants from 27 countries.
An interactive R/Shiny/echarty app.  
[**Live Demo**](https://helgalabs.shinyapps.io/eurobarometer)


<br><br>
<!--

## Panoramas and maps  

Leaflet map with POI markers. Clicking on a marker switches to an interactive custom 360° panorama of the location. Each pano could also have POI as clickable hyperlinks inside.  
video  ech.tmp.R
-->

