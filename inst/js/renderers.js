//  JS renderers for error bars, bands, geoJson
//  Prefix 'ri' stands for 'renderItem' - the calling origin.

/*
  Error bars attach to "host" series - bar,line or scatter. 
  Attaching is done automatically by type, or by name.
  Show/hide together with host series when user clicks on a legend button.	
  Inherit color from the host series, so it is recommended to use a custom color(itemStyle.color), default is 'brown'.
  They add a legend if none is found.
  Error bars should be set at the end, after all other series.
  They are custom series, so ec.init(load='custom') is required.

  Support for horizontal/vertical layouts, use 'encode' for column selection (data or dataset)
  Support for GROUPED series, also for barGap and barCategoryGap.
  Limitations for grouped host bars: 
    Error bars cannot have their own names - will disrupt positioning
    Legend select/deselect will not re-position error bars
*/

function riErrBars(params, api) {
  const group = { type: 'group', children: [] };
  chart = get_e_charts(echwid);
  halfWidth = api.style().lineDashOffset;
  let encode = params.encode;
  isHor = encode['x'].length > encode['y'].length;
  baseDimIdx = isHor ? 1 : 0
  let coordDims = ['x', 'y'];

    let otherDimIdx = 1 - baseDimIdx;
    let baseValue = api.value(encode[coordDims[baseDimIdx]][0]);
    let param = [];
    param[baseDimIdx] = baseValue;
    param[otherDimIdx] = api.value(encode[coordDims[otherDimIdx]][1]);
    let highPoint = api.coord(param);
    param[otherDimIdx] = api.value(encode[coordDims[otherDimIdx]][2]);
    let lowPoint = api.coord(param);
    var style = api.style({
      stroke: api.visual('color'),
      fill: undefined
    });
    
    offset = 0;  // calc bar offset
    xx = isHor ? 0 : 1; yy = 1 - xx;
    sers = chart.getModel().getSeries().filter(o => o.subType!='custom');
    if (sers.length > 0) {
      tmp = sers.filter(o => o.subType=='bar')
      if (tmp.length > 0) {
        tmp = tmp.find(o => o.name === params.seriesName);
        if (tmp) {
          // idx is index of related bar (by name)
          idx = tmp.seriesIndex;
        	bgm = bcgm = null;
          tmp = sers.findLast(o => o.option && o.option.barCategoryGap != undefined);
          if (tmp) {
            bgm = tmp.option.barGap;
            bcgm = tmp.option.barCategoryGap;
          }
        	olay = { count: sers.length };
        	if (bgm) olay.barGap = bgm //!=='' ? bgm : '30%' is default for e_bar
        	if (bcgm) olay.barCategoryGap = bcgm //!=='' ? bcgm : '20%';
        	barLayouts = api.barLayout(olay);
      	  offset = barLayouts[idx].offsetCenter;
        }
      }
    }
    group.children.push(
      {
        type: 'line', x: xx*offset, y: yy*offset,
        transition: ['shape'],
        shape: makeShape(
          baseDimIdx,
          highPoint[baseDimIdx] - halfWidth,
          highPoint[otherDimIdx],
          highPoint[baseDimIdx] + halfWidth,
          highPoint[otherDimIdx]
        ),
        style: style
      },
      {
        type: 'line', x: xx*offset, y: yy*offset,
        transition: ['shape'],
        shape: makeShape(
          baseDimIdx,
          highPoint[baseDimIdx],
          highPoint[otherDimIdx],
          lowPoint[baseDimIdx],
          lowPoint[otherDimIdx]
        ),
        style: style
      },
      {
        type: 'line', x: xx*offset, y: yy*offset,
        transition: ['shape'],
        shape: makeShape(
          baseDimIdx,
          lowPoint[baseDimIdx] - halfWidth,
          lowPoint[otherDimIdx],
          lowPoint[baseDimIdx] + halfWidth,
          lowPoint[otherDimIdx]
        ),
        style: style
      }
    );

  function makeShape(baseDimIdx, base1, value1, base2, value2) {
    var shape = {};
    shape[coordDims[baseDimIdx] + '1'] = base1;
    shape[coordDims[1 - baseDimIdx] + '1'] = value1;
    shape[coordDims[baseDimIdx] + '2'] = base2;
    shape[coordDims[1 - baseDimIdx] + '2'] = value2;
    return shape;
  }
  return group;
}
riErrorBar = riErrBars  // legacy name

/*
  error bar function for simple data without grouping
  usage: renderItem= htmlwidgets::JS("riErrBarSimple")
*/
function riErrBarSimple(params, api) {
    var xValue = api.value(0);
    var highPoint = api.coord([xValue, api.value(1)]);
    var lowPoint = api.coord([xValue, api.value(2)]);
    //var halfWidth = api.size([1, 0])[0] * 0.1;
    var halfWidth = (api.value(1)-api.value(2)) * 0.2;
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
  renderItem function for geoJSON objects
  supports Point, MultiPoint, LineString, MultiLineString, Polygon, MultiPolygon
  see test-renderers.R for code example
  usage: 
    myGeojson <- gsub('\n', '', '{...}')
    ec.init(load= c('leaflet','custom'), 
      js= paste('ecfun.geojson=',myGeojson),
      series= list(list(
        type= 'custom',
        coordinateSystem= 'leaflet',  # or 'gmap',etc.
        renderItem= htmlwidgets::JS("riGeoJson") ...
*/
function riGeoJson(params, api) {
  gj = ecfun.geojson.features[params.dataIndex];
  type = gj.geometry.type.toLowerCase();
  ccc = gj.geometry.coordinates;
  colr = gj.properties.color;
  if (colr==undefined) colr = api.visual('color');
  fill = gj.properties.ppfill;
  if (fill==undefined) {
	  fill = ecfun.geofill;
	  if (fill==0) fill = colr;
  }
  lwi = gj.properties.lwidth;
  if (lwi==undefined) lwi = 3;
	ldash = gj.properties.ldash;
	if (ldash)
		if (Array.isArray(ldash.match(/\[.*?\]/g))) eval('ldash = '+ldash);
	if (!isNaN(ldash)) ldash = Number(ldash);
	if (ldash==undefined) ldash = null;
  points = [];
  //z2 = 1;
  
  switch(type) {
	case 'linestring':
		type = 'polyline';
		break;
	case 'point':
		type = 'circle';
		rad = gj.properties.radius;
      		if (rad==undefined) rad = 5;
		break;
	}
  if (type == 'circle') ccc = [ccc];
  return {
   type: 'group',
   children:
      ccc.map(coords => {

      switch(type) {
      case 'multipoint':
	points = [];
      case 'circle':
	points.push(api.coord(coords));
	out = {
		type: 'circle',
		shape: {cx: points[0][0], cy: points[0][1], r:rad },
		style: api.style({
			lineWidth: lwi, stroke: colr, fill: fill })
	}
	break;
      case 'polyline':
	points.push(api.coord(coords));
	out = {
		type: type,
        	shape: { points: points },
		style: api.style({
  			stroke: colr, lineWidth: lwi, fill: null, lineDash: ldash })
	}
	break;
      case 'multilinestring':
      case 'multipolygon':        
      case 'polygon':
  	points = [];
        if (coords.length === 1) coords = coords[0];
        for (var i = 0; i < coords.length; i++) {
          // why are nested polygon defs allowed in geoJson? how deep?
          if (coords[i].length>2) {
            for (var j = 0; j < coords[i].length; j++) 
              points.push(api.coord(coords[i][j]));
          } else
          points.push(api.coord(coords[i]));
        }
        out = {
          type: type,
          shape: { points: points },
          style: api.style({
             stroke: colr,
             lineWidth: lwi,
             fill: fill, lineDash: ldash
          })
        }
        if (type=='multipolygon') 
          out.type= 'polygon'; 
    	if (type=='multilinestring') {
    		out.type= 'polyline';  
		    out.style.fill = null;
    	}
        break;
      }
      ecfun.geoz2++; //z2++;
      out.z2 = ecfun.geoz2;
      return out;
      })
  }
}


/*
 ------------- Licence -----------------

 Original work Copyright 2023 Larry Helgason
 
 Licensed under the Apache License, Version 2.0 (the "License");
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at
 
 http://www.apache.org/licenses/LICENSE-2.0
 
 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
 ---------------------------------------
*/
