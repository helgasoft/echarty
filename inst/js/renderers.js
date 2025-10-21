//  JS renderers for error bars, bands, geoJson, flame
//  Prefix 'ri' stands for 'renderItem' - the calling origin.

/*
  Error bars attach to "host" series - bar,line or scatter. 
  Attaching is done automatically by type, or by name.
  Show/hide together with host series when user clicks on a legend button.	
  Inherit color from the host series, so it is recommended to use a custom itemStyle.color, default is 'brown'.
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
  let chart = get_e_charts(echwid);
  let halfWidth = api.style().lineDashOffset;
  let encode = params.encode;
  let isHor = encode['x'].length > encode['y'].length;
  let baseDimIdx = isHor ? 1 : 0
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
    
    let offset = 0;  // calc bar offset
    let xx = isHor ? 0 : 1;
    let yy = 1 - xx;
    let sers = chart.getModel().getSeries().filter(o => o.subType!='custom');
    if (sers.length > 0) {
      let tmp = sers.filter(o => o.subType=='bar')
      if (tmp.length > 0) {
        tmp = tmp.find(o => o.name === params.seriesName);
        if (tmp) {
          // idx is index of related bar (by name)
          let idx = tmp.seriesIndex;
          let bgm = null; let bcgm = null;
          tmp = sers.findLast(o => o.option && o.option.barCategoryGap != undefined);
          if (tmp) {
            bgm = tmp.option.barGap;
            bcgm = tmp.option.barCategoryGap;
          }
          let olay = { count: sers.length };
          if (bgm) olay.barGap = bgm //!=='' ? bgm : '30%' is default for bar
          if (bcgm) olay.barCategoryGap = bcgm //!=='' ? bcgm : '20%';
          let barLayouts = api.barLayout(olay);
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
    let shape = {};
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
    let xValue = api.value(0);
    let highPoint = api.coord([xValue, api.value(1)]);
    let lowPoint = api.coord([xValue, api.value(2)]);
    //let halfWidth = api.size([1, 0])[0] * 0.1;
    let halfWidth = (api.value(1)-api.value(2)) * 0.2;
    let style = api.style({
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
    let color = api.visual('color');

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
  renderItem function for geoJSON objects, used in ec.util(cmd='geojson')
  supports Point, MultiPoint, LineString, MultiLineString, Polygon, MultiPolygon
  usage: 
  	myGeojson <- "{"type": "FeatureCollection", ...}"
	ec.init(load= c('world','custom'), 
	  geo= list(type='map', map='world', center= c(-116.35, 35.5), zoom= 17, roam= T), 
	  series= list( 
	    ec.util(cmd= 'geojson', geojson= jsonlite::fromJSON(myGeojson), cs='geo',
	            itemStyle= list(opacity= 0.5), ppfill= 'red' )
	  )
	)
  See test-renderers.R for full leaflet example
*/
function riGeoJson(params, api) {
  let gj = ecf.geojson.features[params.dataIndex];
  let type = gj.geometry.type.toLowerCase();
  let ccc = gj.geometry.coordinates;
  let colr = gj.properties.color;
  if (colr==undefined) colr = api.visual('color');
  let fill = gj.properties.ppfill;
  if (fill==undefined) {
	  fill = ecf.geofill;
	  if (fill==0) fill = colr;
  }
  let lwi = gj.properties.lwidth;
  if (lwi==undefined) lwi = 3;
  let ldash = gj.properties.ldash;
  if (ldash)
  	if (Array.isArray(ldash.match(/\[.*?\]/g))) eval('ldash = '+ldash);
  if (!isNaN(ldash)) ldash = Number(ldash);
  if (ldash==undefined) ldash = null;
  var points = [];
  
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
	  style: api.style({ lineWidth: lwi, stroke: colr, fill: fill })
	}
	break;
      case 'polyline':
	points.push(api.coord(coords));
	out = {
	  type: type,
          shape: { points: points },
	  style: api.style({ stroke: colr, lineWidth: lwi, fill: null, lineDash: ldash })
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
      ecf.geoz2++;
      out.z2 = ecf.geoz2;
      return out;
      })
  }
}

/*
  outliers for grouped boxplots
  used in ec.data(df, format='boxplot')
*/
function riOutliers(params, api) {
  let chart = get_e_charts(echwid);
  let rady = api.style().lineDashOffset; if (!rady) rady = 5;  // ol.radius
  let encode = params.encode;
  let isHor = encode['x'] > encode['y']
  let baseDimIdx = isHor ? 1 : 0
  let otherDimIdx = 1 - baseDimIdx;
  let coordDims = ['x', 'y'];

  let prm = [];
  prm[baseDimIdx] = api.value(encode[coordDims[baseDimIdx]][0]);
  prm[otherDimIdx] = api.value(encode[coordDims[otherDimIdx]][0]);

  let thePoint = api.coord(prm);
  if (isHor) thePoint.reverse();
  var style = api.style({
     //stroke: api.visual('color'), lineWidth: 2,
     fill: api.visual('color')
  });
 
  let offset = 0;  // calc boxplot offset
  xx = isHor ? 0 : 1; yy = 1 - xx;
  let sers = chart.getModel().getSeries().filter(o => o.subType=='boxplot');
  if (sers.length > 0) {
     let tmp = sers.find(o => o.name === params.seriesName);
     if (tmp) {
        // idx is index of related boxplot (by name)
        idx = tmp.seriesIndex;
      	olay = { count: sers.length, barGap:'30%',  barCategoryGap:'20%' };
      	barLayouts = api.barLayout(olay);
    	  offset = barLayouts[idx].offsetCenter;
     }
  }
  return {
     type: 'circle', x: xx*offset, y: yy*offset,
     transition: ['shape'],
     shape: { 
       cx: thePoint[baseDimIdx], 
       cy: thePoint[otherDimIdx], 
       r:  rady },
     style: style
  }
}

/* renderItem for flame chart */
function riFlame(params, api) {
  const level = api.value(0);
  const start = api.coord([api.value(1), level]);
  const end = api.coord([api.value(2), level]);
  const height = ((api.size && api.size([0, 1])) || [0, 20])[1];
  const width = end[0] - start[0] -1;  // horiz border width
  return {
    type: 'rect',
    transition: ['shape'],
    shape: {
      x: start[0],
      y: start[1] - height / 2,
      width,
      height: height - 2 /* itemGap */,
      r: 2
    },
    style: {
      fill: api.visual('color')
    },
    textConfig: {
      position: 'insideLeft'
    },
    textContent: {
      style: {
        text: api.value(3),
        fontFamily: 'Verdana',
        fill: '#000',
        width: width - 4,
        overflow: 'truncate',
        ellipsis: '..',
        truncateMinChar: 1
      }
    }
  };
}

/*  utils for flame chart click event */
function flameFilterJson(json, id) {
  if (id == null) {
    return json;
  }
  const recur = (item, id) => {
    if (item.name === id) {
      return item;
    }
    for (const child of item.children || []) {
      const temp = recur(child, id);
      if (temp) {
        item.children = [temp];
        //item.value = temp.value; // change the parents' values
        return item;
      }
    }
  };
  return recur(json, id) || json;
};
function flameRecursionJson(jsonObj, id) {
  const data = [];
  const filteredJson = flameFilterJson(structuredClone(jsonObj), id);
  const rootVal = 1000;
  const recur = (item, start= 0, level= 0, wit=null) => {
    const temp = {
      name: item.name, 
      id: item.name,
      // [level, start_val, end_val, name, percentage, value]
      value: [
        level,
        start,
        start + wit, 
        item.name,
        (wit / rootVal * 100).toFixed(2),
        item.value
      ]
    };
    data.push(temp);
    let prevStart = start;
    if (item.children) 
      wit /= item.children.length;
    for (const child of item.children || []) {
      recur(child, prevStart, level + 1, wit);
      prevStart = prevStart + wit; 
    }
  };
  recur(filteredJson, 0,0, rootVal);
  return data;
};
function flameClick(params) {    // default event handler
  // flameData needs to be provided by calling R code
  const data = flameRecursionJson(flameData, params.data.name);
  if (data) {
    const rootValue = data[0].value[2];
    chart = ec_chart(echwid);
    chart.setOption({
      xAxis: { max: rootValue },
      series: [{ data }]
    });
  }
}
  
/*
 ------------- Licence -----------------

 Original work Copyright 2021-2024 Larry Helgason
 
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
