//  JS renderers for error bars, bands, etc.
//  Prefix 'ri' stands for 'renderItem' - the calling origin.
	
/*
  Error Bar support for grouped bars, barGap and barCategoryGap
     Notes:	
  Error bars can have chart bars, lines and scatter points as "hosts".
  Error bars will "attach" to their host series and show/hide 
  together when user clicks on a legend button.	
  Attaching is done automatically (by type), or by name.
  Error bars will inherit color from their host bar, blending with them.
  Therefore it is preferable to use a different color, default is 'black'.
  ecr.ebars will add a legend if none is found.
  ecr.ebars should be set at the end, after all other series.
  ecr.ebars are custom series, so ec.init(load='custom') is required.

*/
function riErrorBar(params, api) {

  // input oss contains 
  //   [last.barGap, last.barCategoryGap, series.count, ends.half.width]
  let oss = JSON.parse(sessionStorage.getItem('ErrorBar.oss'));
  if (oss===null || !Object.keys(oss).length) return null;   // needs 4 input values

  let totSeries = Number(oss[2]);

  let xValue = api.value(0);  // data order is x,y,low,high
  let highPoint = api.coord([xValue, api.value(3)]);
  let lowPoint = api.coord([xValue, api.value(2)]);
  let endsWidth = Number(oss[3]);  //api.size([1, 0])[0] * 0.1;
	
  let csil = api.currentSeriesIndices().length / 2;
	// idx is index of related main bar
  let idx = params.seriesIndex - (params.seriesIndex < totSeries ? 0 : totSeries);	

  if (csil > 1 && totSeries > 1) {
  	let bgm = oss[0];
  	let bcgm = oss[1];
  	let olay = { count: csil };
  	olay.barGap = bgm!=='' ? bgm : '30%';		// '30%' is default for e_bar
  	olay.barCategoryGap = bcgm!=='' ? bcgm : '20%';
  	let barLayouts = api.barLayout(olay);		// will be csil # of barLayouts
  	
  	if (barLayouts) {
	  	let mbar = 0;
	  	api.currentSeriesIndices().some( (item, index) => {
	  		if (item == idx) {
	  			highPoint[0] += barLayouts[mbar].offsetCenter;
	  			// endsWidth = barLayouts[mbar].width /2;
	  			return true;
	  		}
	  		mbar++;
	  		return mbar >= csil;  // false until true
	  	});
  	}
  }
  lowPoint[0] = highPoint[0];
  
  var style = api.style({
      stroke: api.visual('color'),
      fill: null
  });
  return {
      type: 'group',
      children: [{
          type: 'line',
          shape: {
              x1: highPoint[0] - endsWidth, y1: highPoint[1],
              x2: highPoint[0] + endsWidth, y2: highPoint[1]
          },
          style: style
      }, {
          type: 'line',		// vertical
          shape: {
              x1: highPoint[0], y1: highPoint[1],
              x2: lowPoint[0], y2: lowPoint[1]
          },
          style: style
      }, {
          type: 'line',
          shape: {
              x1: lowPoint[0] - endsWidth, y1: lowPoint[1],
              x2: lowPoint[0] + endsWidth, y2: lowPoint[1]
          },
          style: style
      }]
  };
}

/*
  renderItem function for polygons
  used by ecr.band
*/
function riPolygon(params, api) {
    if (params.context.rendered) return;
    params.context.rendered = true;
    
    // set polygon vertices
    let points = [];
    let i = 0;
    while (typeof api.value(0,i) != 'undefined' && !isNaN(api.value(0,i))) {
    	points.push(api.coord([api.value(0,i), api.value(1,i)]));
    	i++;
    }
    var color = api.visual('color');

    return {
        type: 'polygon',
        shape: {
            points: echarts.graphic.clipPointsByRect(points, {
                x: params.coordSys.x,
                y: params.coordSys.y,
                width: params.coordSys.width,
                height: params.coordSys.height
            })
        },
        style: api.style({
            fill: color,
            stroke: echarts.color.lift(color)
        })
    };
}

/*
  error bar function for simple data without grouping
  usage: renderItem = htmlwidgets::JS("riErrBarSimple")
*/
function riErrBarSimple(params, api) {
    var xValue = api.value(0);
    var highPoint = api.coord([xValue, api.value(1)]);
    var lowPoint = api.coord([xValue, api.value(2)]);
    var halfWidth = api.size([1, 0])[0] * 0.1;
    var style = api.style({
        stroke: api.visual('color'),
        fill: null
    });

    return {
        type: 'group',
        children: [{
            type: 'line',
            shape: {
                x1: highPoint[0] - halfWidth, y1: highPoint[1],
                x2: highPoint[0] + halfWidth, y2: highPoint[1]
            },
            style: style
        }, {
            type: 'line',
            shape: {
                x1: highPoint[0], y1: highPoint[1],
                x2: lowPoint[0], y2: lowPoint[1]
            },
            style: style
        }, {
            type: 'line',
            shape: {
                x1: lowPoint[0] - halfWidth, y1: lowPoint[1],
                x2: lowPoint[0] + halfWidth, y2: lowPoint[1]
            },
            style: style
        }]
    };
}