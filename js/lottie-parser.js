var Ur=Object.defineProperty;var Re=Object.getOwnPropertySymbols;var Wr=Object.prototype.hasOwnProperty,Tr=Object.prototype.propertyIsEnumerable;var ae=(m,y,A)=>y in m?Ur(m,y,{enumerable:!0,configurable:!0,writable:!0,value:A}):m[y]=A,R=(m,y)=>{for(var A in y||(y={}))Wr.call(y,A)&&ae(m,A,y[A]);if(Re)for(var A of Re(y))Tr.call(y,A)&&ae(m,A,y[A]);return m};var T=(m,y,A)=>(ae(m,typeof y!="symbol"?y+"":y,A),A);(function(m,y){typeof exports=="object"&&typeof module!="undefined"?y(exports):typeof define=="function"&&define.amd?define(["exports"],y):(m=typeof globalThis!="undefined"?globalThis:m||self,y(m.lottieParser={}))})(this,function(m){"use strict";var y;(function(e){e[e.Normal=0]="Normal",e[e.Multiply=1]="Multiply",e[e.Screen=2]="Screen",e[e.Overlay=3]="Overlay",e[e.Darken=4]="Darken",e[e.Lighten=5]="Lighten",e[e.ColorDodge=6]="ColorDodge",e[e.ColorBurn=7]="ColorBurn",e[e.HardLight=8]="HardLight",e[e.SoftLight=9]="SoftLight",e[e.Difference=10]="Difference",e[e.Exclusion=11]="Exclusion",e[e.Hue=12]="Hue",e[e.Saturation=13]="Saturation",e[e.Color=14]="Color",e[e.Luminosity=15]="Luminosity"})(y||(y={}));var A;(function(e){e[e.Normal=0]="Normal",e[e.Alpha=1]="Alpha",e[e.InvertedAlpha=2]="InvertedAlpha",e[e.Luma=3]="Luma",e[e.InvertedLuma=4]="InvertedLuma"})(A||(A={}));var he;(function(e){e[e.Off=0]="Off",e[e.On=1]="On"})(he||(he={}));var ve;(function(e){e[e.Off=0]="Off",e[e.On=1]="On"})(ve||(ve={}));var ke;(function(e){e[e.Number=0]="Number",e[e.Color=2]="Color",e[e.MultiDimensional=3]="MultiDimensional",e[e.Boolean=7]="Boolean"})(ke||(ke={}));var ye;(function(e){e[e.Transform=5]="Transform",e[e.DropShadow=25]="DropShadow"})(ye||(ye={}));var ge;(function(e){e.No="n",e.Add="a",e.Subtract="s",e.Intersect="i",e.Lighten="l",e.Darken="d",e.Difference="f"})(ge||(ge={}));var W;(function(e){e[e.precomp=0]="precomp",e[e.solid=1]="solid",e[e.still=2]="still",e[e.null=3]="null",e[e.shape=4]="shape",e[e.text=5]="text",e[e.audio=6]="audio",e[e.pholderVideo=7]="pholderVideo",e[e.imageSeq=8]="imageSeq",e[e.video=9]="video",e[e.pholderStill=10]="pholderStill",e[e.guide=11]="guide",e[e.adjustment=12]="adjustment",e[e.camera=13]="camera",e[e.light=14]="light"})(W||(W={}));var be;(function(e){e[e.Left=0]="Left",e[e.Right=1]="Right",e[e.Center=2]="Center"})(be||(be={}));var me;(function(e){e[e.Top=0]="Top",e[e.Center=1]="Center",e[e.Bottom=2]="Bottom"})(me||(me={}));var de;(function(e){e[e.Characters=1]="Characters",e[e.CharactersExcludingSpaces=2]="CharactersExcludingSpaces",e[e.Words=3]="Words",e[e.Lines=4]="Lines"})(de||(de={}));var Ae;(function(e){e[e.Square=1]="Square",e[e.RampUp=2]="RampUp",e[e.RampDown=3]="RampDown",e[e.Triangle=4]="Triangle",e[e.Round=5]="Round",e[e.Smooth=6]="Smooth"})(Ae||(Ae={}));var pe;(function(e){e[e.Percentage=1]="Percentage",e[e.Index=2]="Index"})(pe||(pe={}));var Ce;(function(e){e[e.Add=1]="Add",e[e.Subtract=2]="Subtract",e[e.Intersect=3]="Intersect",e[e.Min=4]="Min",e[e.Max=5]="Max",e[e.Difference=6]="Difference"})(Ce||(Ce={}));var p;(function(e){e.Group="gr",e.Rect="rc",e.Ellipse="el",e.Fill="fl",e.GradientFill="gf",e.GradientStroke="gs",e.Stroke="st",e.Transform="tr",e.Path="sh",e.Repeat="rp",e.Trim="tm"})(p||(p={}));var Fe;(function(e){e[e.NonZero=1]="NonZero",e[e.EvenOdd=2]="EvenOdd"})(Fe||(Fe={}));var q;(function(e){e[e.Miter=1]="Miter",e[e.Round=2]="Round",e[e.Bevel=3]="Bevel"})(q||(q={}));var N;(function(e){e[e.Butt=1]="Butt",e[e.Round=2]="Round",e[e.Square=3]="Square"})(N||(N={}));var K;(function(e){e[e.Linear=1]="Linear",e[e.Radial=2]="Radial"})(K||(K={}));var Le;(function(e){e[e.CssUrl=1]="CssUrl",e[e.ScriptUrl=2]="ScriptUrl",e[e.FontUrl=3]="FontUrl"})(Le||(Le={}));var _e=12,Ke="sans-serif",Ee=_e+"px "+Ke,Je=20,De=100,Ve="007LLmW'55;N0500LLLLLLLLLL00NNNLzWW\\\\WQb\\0FWLg\\bWb\\WQ\\WrWWQ000CL5LLFLL0LL**F*gLLLL5F0LF\\FFF5.5N";function Be(e){var r={};if(typeof JSON=="undefined")return r;for(var i=0;i<e.length;i++){var n=String.fromCharCode(i+32),t=(e.charCodeAt(i)-Je)/De;r[n]=t}return r}var Me=Be(Ve),Oe={createCanvas:function(){return typeof document!="undefined"&&document.createElement("canvas")},measureText:function(){var e,r;return function(i,n){if(!e){var t=Oe.createCanvas();e=t&&t.getContext("2d")}if(e)return r!==n&&(r=e.font=n||Ee),e.measureText(i);i=i||"",n=n||Ee;var s=/^([0-9]*?)px$/.exec(n),f=+(s&&s[1])||_e,l=0;if(n.indexOf("mono")>=0)l=f*i.length;else for(var c=0;c<i.length;c++){var u=Me[i[c]];l+=u==null?f:u*f}return{width:l}}}(),loadImage:function(e,r,i){var n=new Image;return n.onload=r,n.onerror=i,n.src=e,n}},Ie=we(["Function","RegExp","Date","Error","CanvasGradient","CanvasPattern","Image","Canvas"],function(e,r){return e["[object "+r+"]"]=!0,e},{}),Pe=we(["Int8","Uint8","Uint8Clamped","Int16","Uint16","Int32","Uint32","Float32","Float64"],function(e,r){return e["[object "+r+"Array]"]=!0,e},{}),x=Object.prototype.toString,xe=Array.prototype,er=xe.slice,Se=function(){}.constructor,J=Se?Se.prototype:null,je="__proto__";function D(e){if(e==null||typeof e!="object")return e;var r=e,i=x.call(e);if(i==="[object Array]"){if(!z(e)){r=[];for(var n=0,t=e.length;n<t;n++)r[n]=D(e[n])}}else if(Pe[i]){if(!z(e)){var s=e.constructor;if(s.from)r=s.from(e);else{r=new s(e.length);for(var n=0,t=e.length;n<t;n++)r[n]=e[n]}}}else if(!Ie[i]&&!z(e)&&!re(e)){r={};for(var f in e)e.hasOwnProperty(f)&&f!==je&&(r[f]=D(e[f]))}return r}function ee(e,r,i){if(!V(r)||!V(e))return i?D(r):e;for(var n in r)if(r.hasOwnProperty(n)&&n!==je){var t=e[n],s=r[n];V(s)&&V(t)&&!Ye(s)&&!Ye(t)&&!re(s)&&!re(t)&&!Xe(s)&&!Xe(t)&&!z(s)&&!z(t)?ee(t,s,i):(i||!(n in e))&&(e[n]=D(r[n]))}return e}Oe.createCanvas;function we(e,r,i,n){if(!!(e&&r)){for(var t=0,s=e.length;t<s;t++)i=r.call(n,i,e[t],t,e);return i}}function rr(e,r){for(var i=[],n=2;n<arguments.length;n++)i[n-2]=arguments[n];return function(){return e.apply(r,i.concat(er.call(arguments)))}}J&&nr(J.bind)&&J.call.bind(J.bind);function Ye(e){return Array.isArray?Array.isArray(e):x.call(e)==="[object Array]"}function nr(e){return typeof e=="function"}function V(e){var r=typeof e;return r==="function"||!!e&&r==="object"}function Xe(e){return!!Ie[x.call(e)]}function re(e){return typeof e=="object"&&typeof e.nodeType=="number"&&typeof e.ownerDocument=="object"}var tr="__ec_primitive__";function z(e){return e[tr]}function ir(e,r){return Math.sqrt((e[0]-r[0])*(e[0]-r[0])+(e[1]-r[1])*(e[1]-r[1]))}var fr=ir;function sr(e,r,i){var n=r[0],t=r[1];return e[0]=i[0]*n+i[2]*t+i[4],e[1]=i[1]*n+i[3]*t+i[5],e}function Ue(e,r,i,n,t){var s=1-t;return s*s*(s*e+3*t*r)+t*t*(t*n+3*s*i)}function B(e,r,i,n,t,s){var f=(r-e)*t+e,l=(i-r)*t+r,c=(n-i)*t+i,u=(l-f)*t+f,o=(c-l)*t+l,a=(o-u)*t+u;s[0]=e,s[1]=f,s[2]=u,s[3]=a,s[4]=a,s[5]=o,s[6]=c,s[7]=n}function lr(e,r,i,n,t,s,f,l,c){for(var u=e,o=r,a=0,h=1/c,k=1;k<=c;k++){var g=k*h,v=Ue(e,i,t,f,g),b=Ue(r,n,s,l,g),C=v-u,F=b-o;a+=Math.sqrt(C*C+F*F),u=v,o=b}return a}function cr(e,r,i){var n=r[0],t=r[2],s=r[4],f=r[1],l=r[3],c=r[5],u=Math.sin(i),o=Math.cos(i);return e[0]=n*o+f*u,e[1]=-n*u+f*o,e[2]=t*o+l*u,e[3]=-t*u+o*l,e[4]=o*s+u*c,e[5]=o*c-u*s,e}function M(e,r){return Math.abs(e[0]-r[0])<1e-8&&Math.abs(e[1]-r[1])<1e-8}function ur(e,r,i,n,t,s,f,l){return e[4]=-f||0,e[5]=-l||0,e[0]=t==null?1:t,e[3]=s==null?1:s,P[1]=P[2]=0,n&&cr(e,e,n),e[4]+=r,e[5]+=i,e}function ne(e,r,i){for(let n=0;n<r.length;n++)r[n]=r[n],e[n]=e[n]||[],sr(e[n],r[n],i)}const te={repeat:0,repeatX:0,repeatY:0,repeatRot:0,repeatScaleX:1,repeatScaleY:1,repeatAnchorX:0,repeatAnchorY:0};let P=[];function ie(e,r,i){e(i,r,void 0);let n=0,t=0,s=0,f=1,l=1;for(let c=0;c<r.repeat;c++)n+=r.repeatX,t+=r.repeatY,s+=r.repeatRot,f*=r.repeatScaleX,l*=r.repeatScaleY,ur(P,n,t,s,f,l,r.repeatAnchorX,r.repeatAnchorY),e(i,r,P)}function fe(e,r,i,n,t){const s=i.length;if(!!s){for(let f=1;f<s;f++){const l=f-1;!M(r[l],i[l])||!M(e[f],i[f])?t(i[l],r[l],e[f],i[f]):t(i[l],i[f])}if(n){const f=s-1;!M(r[f],i[f])||!M(e[0],i[0])?t(i[f],r[f],e[0],i[0]):t(i[f],i[0])}}}function or(e,r,i){let n=r.trimStart/100,t=r.trimEnd/100;if(n===t)return;if(n>t){const c=n;n=t,t=c}let s,f,l;if(i?(s=this._inPts||(this._inPts=[]),f=this._outPts||(this._outPts=[]),l=this._vPts||(this._vPts=[]),ne(s,r.in,i),ne(f,r.out,i),ne(l,r.v,i)):(s=r.in,f=r.out,l=r.v),n>0||t<1){let c=[],u=0,o=0;fe(s,f,l,!1,(v,b,C,F)=>{const w=C&&F?lr(v[0],v[1],b[0],b[1],C[0],C[1],F[0],F[1],10):Math.sqrt((v[0]-b[0])**2+(v[1]-b[1])**2);c[o++]=w,u+=w});const a=n*u,h=t*u;let k=0,g=0;fe(s,f,l,!1,(v,b,C,F)=>{const w=c[g];if(!(k+w<=a||k>=h)){if(k>=a&&k+w<=h)g===0&&e.moveTo(v[0],v[1]),C&&F?e.bezierCurveTo(b[0],b[1],C[0],C[1],F[0],F[1]):e.lineTo(b[0],b[1]);else{const X=(a-k)/w,U=(h-k)/w;if(X>=U)return;let L=v[0],_=v[1],S=b[0],j=b[1];if(C&&F){let $=C[0],H=C[1],Q=F[0],Z=F[1];const O=[],I=[];X>0&&(B(L,S,$,Q,X,O),B(_,j,H,Z,X,I),L=O[4],_=I[4],S=O[5],j=I[5],$=O[6],H=I[6],Q=O[7],Z=I[7]),U<1&&(B(L,S,$,Q,U,O),B(_,j,H,Z,U,I),L=O[0],_=I[0],S=O[1],j=I[1],$=O[2],H=I[2],Q=O[3],Z=I[3]),e.moveTo(L,_),e.bezierCurveTo(S,j,$,H,Q,Z)}else X>0&&(L=(S-L)*X+L,_=(j-_)*X+_),U<1&&(S=(S-L)*U+L,j=(j-_)*U+_),e.moveTo(L,_),e.lineTo(S,j)}k+=w,g++}})}else{let c=!0;fe(s,f,l,r.close,(u,o,a,h)=>{c&&e.moveTo(u[0],u[1]),a&&h?e.bezierCurveTo(o[0],o[1],a[0],a[1],h[0],h[1]):e.lineTo(o[0],o[1]),c=!1}),r.close&&e.closePath()}}function ar(e){const r=e.graphic.extendShape({type:"lottie-shape-path",shape:R({in:[],out:[],v:[],close:!1,trimStart:0,trimEnd:100},te),buildPath(t,s){ie((f,l,c)=>{or.call(this,f,l,c)},s,t)}}),i=e.graphic.extendShape({type:"lottie-shape-ellipse",shape:R({cx:0,cy:0,rx:0,ry:0},te),buildPath(t,s){ie((f,l)=>{let c=l.cx,u=l.cy,o=l.rx,a=l.ry;if(o===a)f.arc(c,u,o,0,Math.PI*2),f.closePath();else{const h=.5522848,k=o*h,g=a*h;f.moveTo(c-o,u),f.bezierCurveTo(c-o,u-g,c-k,u-a,c,u-a),f.bezierCurveTo(c+k,u-a,c+o,u-g,c+o,u),f.bezierCurveTo(c+o,u+g,c+k,u+a,c,u+a),f.bezierCurveTo(c-k,u+a,c-o,u+g,c-o,u),f.closePath()}},s,t)}}),n=e.graphic.extendShape({type:"lottie-shape-rect",shape:R({r:0,x:0,y:0,width:0,height:0},te),buildPath(t,s){ie((f,l)=>{let c=l.width,u=l.height,o=l.x-c/2,a=l.y-u/2,h=l.r;h=Math.min(c/2,u/2,h),h?(c<0&&(o=o+c,c=-c),u<0&&(a=a+u,u=-u),f.moveTo(o+h,a),f.lineTo(o+c-h,a),f.arc(o+c-h,a+h,h,-Math.PI/2,0),f.lineTo(o+c,a+u-h),f.arc(o+c-h,a+u-h,h,0,Math.PI/2),f.lineTo(o+h,a+u),f.arc(o+h,a+u-h,h,Math.PI/2,Math.PI),f.lineTo(o,a+h),f.arc(o+h,a+h,h,Math.PI,Math.PI*1.5),f.closePath()):f.rect(o,a,c,u)},s,t)}});e.graphic.registerShape("lottie-shape-path",r),e.graphic.registerShape("lottie-shape-ellipse",i),e.graphic.registerShape("lottie-shape-rect",n)}function We(e,r){var i,n,t=e.length,s,f,l,c;for(n=0;n<t;n+=1)if(i=e[n],"ks"in i&&!i.completed){if(i.completed=!0,i.tt&&(e[n-1].td=i.tt),i.hasMask){var u=i.masksProperties;for(f=u.length,s=0;s<f;s+=1)if(u[s].pt.k.i)Y(u[s].pt.k);else for(c=u[s].pt.k.length,l=0;l<c;l+=1)u[s].pt.k[l].s&&Y(u[s].pt.k[l].s[0]),u[s].pt.k[l].e&&Y(u[s].pt.k[l].e[0])}i.ty===0?(i.layers=hr(i.refId,r),We(i.layers,r)):i.ty===4?Te(i.shapes):i.ty===5&&dr(i)}}function hr(e,r){for(var i=0,n=r.length;i<n;){if(r[i].id===e)return r[i].layers.__used?JSON.parse(JSON.stringify(r[i].layers)):(r[i].layers.__used=!0,r[i].layers);i+=1}return null}function Te(e){var r,i=e.length,n,t;for(r=i-1;r>=0;r-=1)if(e[r].ty==="sh")if(e[r].ks.k.i)Y(e[r].ks.k);else for(t=e[r].ks.k.length,n=0;n<t;n+=1)e[r].ks.k[n].s&&Y(e[r].ks.k[n].s[0]),e[r].ks.k[n].e&&Y(e[r].ks.k[n].e[0]);else e[r].ty==="gr"&&Te(e[r].it)}function Y(e){var r,i=e.i.length;for(r=0;r<i;r+=1)e.i[r][0]+=e.v[r][0],e.i[r][1]+=e.v[r][1],e.o[r][0]+=e.v[r][0],e.o[r][1]+=e.v[r][1]}function G(e,r){var i=r?r.split("."):[100,100,100];return e[0]>i[0]?!0:i[0]>e[0]?!1:e[1]>i[1]?!0:i[1]>e[1]?!1:e[2]>i[2]?!0:i[2]>e[2]?!1:null}var vr=function(){var e=[4,4,14];function r(n){var t=n.t.d;n.t.d={k:[{s:t,t:0}]}}function i(n){var t,s=n.length;for(t=0;t<s;t+=1)n[t].ty===5&&r(n[t])}return function(n){if(G(e,n.v)&&(i(n.layers),n.assets)){var t,s=n.assets.length;for(t=0;t<s;t+=1)n.assets[t].layers&&i(n.assets[t].layers)}}}(),kr=function(){var e=[4,7,99];return function(r){if(r.chars&&!G(e,r.v)){var i,n=r.chars.length,t,s,f,l;for(i=0;i<n;i+=1)if(r.chars[i].data&&r.chars[i].data.shapes)for(l=r.chars[i].data.shapes[0].it,s=l.length,t=0;t<s;t+=1)f=l[t].ks.k,f.__converted||(Y(l[t].ks.k),f.__converted=!0)}}}(),yr=function(){var e=[5,7,15];function r(n){var t=n.t.p;typeof t.a=="number"&&(t.a={a:0,k:t.a}),typeof t.p=="number"&&(t.p={a:0,k:t.p}),typeof t.r=="number"&&(t.r={a:0,k:t.r})}function i(n){var t,s=n.length;for(t=0;t<s;t+=1)n[t].ty===5&&r(n[t])}return function(n){if(G(e,n.v)&&(i(n.layers),n.assets)){var t,s=n.assets.length;for(t=0;t<s;t+=1)n.assets[t].layers&&i(n.assets[t].layers)}}}(),gr=function(){var e=[4,1,9];function r(n){var t,s=n.length,f,l;for(t=0;t<s;t+=1)if(n[t].ty==="gr")r(n[t].it);else if(n[t].ty==="fl"||n[t].ty==="st")if(n[t].c.k&&n[t].c.k[0].i)for(l=n[t].c.k.length,f=0;f<l;f+=1)n[t].c.k[f].s&&(n[t].c.k[f].s[0]/=255,n[t].c.k[f].s[1]/=255,n[t].c.k[f].s[2]/=255,n[t].c.k[f].s[3]/=255),n[t].c.k[f].e&&(n[t].c.k[f].e[0]/=255,n[t].c.k[f].e[1]/=255,n[t].c.k[f].e[2]/=255,n[t].c.k[f].e[3]/=255);else n[t].c.k[0]/=255,n[t].c.k[1]/=255,n[t].c.k[2]/=255,n[t].c.k[3]/=255}function i(n){var t,s=n.length;for(t=0;t<s;t+=1)n[t].ty===4&&r(n[t].shapes)}return function(n){if(G(e,n.v)&&(i(n.layers),n.assets)){var t,s=n.assets.length;for(t=0;t<s;t+=1)n.assets[t].layers&&i(n.assets[t].layers)}}}(),br=function(){var e=[4,4,18];function r(n){var t,s=n.length,f,l;for(t=s-1;t>=0;t-=1)if(n[t].ty==="sh")if(n[t].ks.k.i)n[t].ks.k.c=n[t].closed;else for(l=n[t].ks.k.length,f=0;f<l;f+=1)n[t].ks.k[f].s&&(n[t].ks.k[f].s[0].c=n[t].closed),n[t].ks.k[f].e&&(n[t].ks.k[f].e[0].c=n[t].closed);else n[t].ty==="gr"&&r(n[t].it)}function i(n){var t,s,f=n.length,l,c,u,o;for(s=0;s<f;s+=1){if(t=n[s],t.hasMask){var a=t.masksProperties;for(c=a.length,l=0;l<c;l+=1)if(a[l].pt.k.i)a[l].pt.k.c=a[l].cl;else for(o=a[l].pt.k.length,u=0;u<o;u+=1)a[l].pt.k[u].s&&(a[l].pt.k[u].s[0].c=a[l].cl),a[l].pt.k[u].e&&(a[l].pt.k[u].e[0].c=a[l].cl)}t.ty===4&&r(t.shapes)}}return function(n){if(G(e,n.v)&&(i(n.layers),n.assets)){var t,s=n.assets.length;for(t=0;t<s;t+=1)n.assets[t].layers&&i(n.assets[t].layers)}}}();function mr(e){e.__complete||(gr(e),vr(e),kr(e),yr(e),br(e),We(e.layers,e.assets),e.__complete=!0)}function dr(e){e.t.a.length===0&&!("m"in e.t.p)&&(e.singleShape=!0)}class Ar{constructor(){T(this,"frameTime",1e3/30);T(this,"startFrame",0);T(this,"endFrame");T(this,"assetsMap",new Map);T(this,"layerOffsetTime")}}function qe(e){return Array.isArray(e)&&typeof e[0]=="number"}function se(e){return qe(e==null?void 0:e.k)}function le(e){const r=e==null?void 0:e.k;return Array.isArray(r)&&r[0].t!==void 0&&qe(r[0].s)}function pr(e){return typeof(e==null?void 0:e.k)=="number"}function Cr(e){const r=e==null?void 0:e.k;return Array.isArray(r)&&r[0].t!==void 0&&typeof r[0].s=="number"}function ce(e){return`rgba(${[Math.round(E(e,0)*255),Math.round(E(e,1)*255),Math.round(E(e,2)*255),E(e,3)].join(",")})`}function E(e,r){return e!=null?typeof e=="number"?e:e[r||0]:NaN}function Fr(e,r,i){var t,s,f,l;let n=[];if(n.push(((t=e.o)==null?void 0:t.x)?E(e.o.x,i):0,((s=e.o)==null?void 0:s.y)?E(e.o.y,i):0,((f=r==null?void 0:r.o)==null?void 0:f.x)?E(r.o.x,i):1,((l=r==null?void 0:r.o)==null?void 0:l.y)?E(r.o.y,i):1),n[0]&&n[1]&&n[2]!==1&&n[3]!==1)return`cubic-bezier(${n.join(",")})`}function ue(e,r,i,n){const t=e.length,s=i.endFrame-i.startFrame,f={duration:0,delay:0,keyframes:[]};let l;for(let c=0;c<t;c++){const u=e[c],o=e[c+1],a=u.h===1,k={percent:(u.t+i.layerOffsetTime-i.startFrame)/s};a||(k.easing=Fr(u,o,r));const g=u.s||(l==null?void 0:l.e);if(g&&n(k,g),k.percent>0&&c===0){const v={percent:0};g&&n(v,g),f.keyframes.push(v)}if(f.keyframes.push(k),a&&o){const v={percent:Math.max((o.t+i.layerOffsetTime-i.startFrame)/s,0)};n(v,g),f.keyframes.push(v)}l=u}return t&&(f.duration=i.frameTime*s),f}function Ne(e,r,i,n,t,s){for(let f=0;f<i.length;f++){const l=i[f],c=ue(e,f,t,(u,o)=>{let a=E(o,f);s&&(a=s(a)),(r?u[r]={}:u)[l]=a});c.keyframes.length&&n.push(c)}}function ze(e,r,i,n,t){const s=ue(e,0,t,(f,l)=>{(r?f[r]={}:f)[i]=ce(l)});s.keyframes.length&&n.push(s)}function d(e,r,i,n,t,s,f){i&&(r[i]=r[i]||{});const l=i?r[i]:r;if(pr(e)){const c=e.k;l[n[0]]=f?f(c):c}else if(Cr(e))Ne(e.k,i,n,t,s,f);else if(se(e))for(let c=0;c<n.length;c++){const u=E(e.k,c);l[n[c]]=f?f(u):u}else le(e)&&Ne(e.k,i,n,t,s,f)}function oe(e,r,i,n,t="",s={x:"x",y:"y",rotation:"rotation",scaleX:"scaleX",scaleY:"scaleY",anchorX:"anchorX",anchorY:"anchorY"}){e.p.s?(d(e.p.x,r,t,[s.x],i,n),d(e.p.y,r,t,[s.y],i,n)):d(e.p,r,t,[s.x,s.y],i,n),d(e.s,r,t,[s.scaleX,s.scaleY],i,n,f=>f/100),d(e.r,r,t,[s.rotation],i,n,f=>-(f/180)*Math.PI),d(e.a,r,t,[s.anchorX,s.anchorY],i,n)}function Ge(e){return e.g&&e.s&&e.e}function Lr(e,r){const i=[];for(let n=0;n<r*4;){const t=e[n++],s=Math.round(e[n++]*255),f=Math.round(e[n++]*255),l=Math.round(e[n++]*255);i.push({offset:t,color:`rgb(${s}, ${f}, ${l})`})}return i}function $e(e){const r=e.g.k.k,i=Lr(r,e.g.p);return e.t===K.Linear?{type:"linear",colorStops:i,x:e.s.k[0],y:e.s.k[1],x2:e.e.k[0],y2:e.e.k[1],global:!0}:e.t===K.Radial?{type:"radial",colorStops:i,x:e.s.k[0],y:e.s.k[1],r:fr(e.e.k,e.s.k),global:!0}:"#000"}function _r(e,r,i,n){r.style=r.style||{},Ge(e)?r.style.fill=$e(e):se(e.c)?r.style.fill=ce(e.c.k):le(e.c)&&ze(e.c.k,"style","fill",i,n),d(e.o,r,"style",["fillOpacity"],i,n,t=>t/100)}function Er(e,r,i,n){switch(r.style=r.style||{},Ge(e)?r.style.stroke=$e(e):se(e.c)?r.style.stroke=ce(e.c.k):le(e.c)&&ze(e.c.k,"style","stroke",i,n),d(e.o,r,"style",["strokeOpacity"],i,n,f=>f/100),d(e.w,r,"style",["lineWidth"],i,n),e.lj){case q.Bevel:r.style.lineJoin="bevel";break;case q.Round:r.style.lineJoin="round";break;case q.Miter:r.style.lineJoin="miter";break}switch(e.lc){case N.Butt:r.style.lineCap="butt";break;case N.Round:r.style.lineCap="round";break;case N.Square:r.style.lineCap="square";break}const t=[];let s=0;e.d&&(e.d.forEach(f=>{f.n!=="o"?t.push(f.v.k):s=f.v.k}),r.style.lineDash=t,r.style.lineDashOffset=s)}function Or(e){return e&&e.i&&e.o&&e.v}function He(e,r,i){const n={type:"lottie-shape-path",style:{fill:"none",stroke:"none"}};if(Or(e.ks.k))n.shape={in:e.ks.k.i,out:e.ks.k.o,v:e.ks.k.v,close:e.ks.k.c};else if(Array.isArray(e.ks.k)){const t=ue(e.ks.k,0,i,(s,f)=>{s.shape={in:f[0].i,out:f[0].o,v:f[0].v,close:f[0].c}});t.keyframes.length&&r.push(t)}return n}function Ir(e,r,i){const n={type:"lottie-shape-rect",style:{fill:"none",stroke:"none"},shape:{}};return d(e.p,n,"shape",["x","y"],r,i),d(e.s,n,"shape",["width","height"],r,i),d(e.r,n,"shape",["r"],r,i),n}function Sr(e,r,i){const n={type:"lottie-shape-ellipse",style:{fill:"none",stroke:"none"},shape:{}};return d(e.p,n,"shape",["cx","cy"],r,i),d(e.s,n,"shape",["rx","ry"],r,i,t=>t/2),n}function jr(e,r){function i(s,f){let l;switch(s.ty){case p.Path:l=He(s,f,r);break;case p.Ellipse:l=Sr(s,f,r);break;case p.Rect:l=Ir(s,f,r);break}return l}function n(s,f){s.forEach(l=>{if(!l.hd)switch(l.ty){case p.Repeat:d(l.c,f.attrs,"shape",["repeat"],f.keyframeAnimations,r),oe(l.tr,f.attrs,f.keyframeAnimations,r,"shape",{x:"repeatX",y:"repeatY",rotation:"repeatRot",scaleX:"repeatScaleX",scaleY:"repeatScaleY",anchorX:"repeatAnchorX",anchorY:"repeatAnchorY"});break;case p.Trim:d(l.s,f.attrs,"shape",["trimStart"],f.keyframeAnimations,r),d(l.e,f.attrs,"shape",["trimEnd"],f.keyframeAnimations,r);break}})}function t(s,f){const l=[],c={},u=[];return s=s.slice().reverse(),n(s,f),s.forEach(o=>{if(o.hd)return;let a;switch(o.ty){case p.Group:a={type:"group",children:t(o.it,f)};break;case p.Fill:case p.GradientFill:_r(o,c,u,r);break;case p.Stroke:case p.GradientStroke:Er(o,c,u,r);break;case p.Transform:oe(o,c,u,r);break;default:a=i(o,u)}a&&(a.name=o.nm,l.push(a))}),l.forEach((o,a)=>{ee(o,f.attrs,!0),ee(o,c,!0),(u.length||f.keyframeAnimations.length)&&(o.keyframeAnimation=[...f.keyframeAnimations,...u]),l[a]=o}),l}return{type:"group",children:t(e.shapes,{attrs:{},keyframeAnimations:[]})}}function Qe(e,r){var i;r(e),e.type==="group"&&((i=e.children)==null||i.forEach(n=>{Qe(n,r)}))}function wr(e,r,i){var s,f;const n={},t=[];((s=e.ks)==null?void 0:s.o)&&(d(e.ks.o,n,"style",["opacity"],t,i,l=>l/100),(((f=n.style)==null?void 0:f.opacity)||t.length)&&Qe(r,l=>{l.type!=="group"&&l.style&&(Object.assign(l.style,n.style),t.length&&(l.keyframeAnimation=(l.keyframeAnimation||[]).concat(t)))}))}function Yr(e){return{type:"rect",shape:{x:0,y:0,width:e.sw,height:e.sh},style:{fill:e.sc}}}function Ze(e,r,i){let n=[];e=e.slice().reverse();const t=new Map,s=(i==null?void 0:i.st)||0;return e==null||e.forEach(f=>{var a,h;const l=s+f.ip,c=s+f.op,u=s+f.st;r.layerOffsetTime=s;let o;switch(f.ty){case W.shape:o=jr(f,r);break;case W.null:o={type:"group",children:[]};break;case W.solid:o={type:"group",children:[]},f.sc&&o.children.push(Yr(f));break;case W.precomp:o={type:"group",children:Ze(((a=r.assetsMap.get(f.refId))==null?void 0:a.layers)||[],r,{st:u})};break}if(o){const k=[],g={name:f.nm};if(f.ks&&oe(f.ks,g,k,r),Object.assign(o,g),f.ind!=null&&t.set(f.ind,o),o.extra={layerParent:f.parent},f.hasMask&&((h=f.masksProperties)==null?void 0:h.length)){const v=[],b=He({ks:f.masksProperties[0].pt},v,r);o.clipPath=R({type:"lottie-shape-path"},b),o.clipPath.style.fill="#000",v.length&&(o.clipPath.keyframeAnimation=v)}if(wr(f,o,r),l!=null&&c!=null&&(l>r.startFrame||c<r.endFrame)){const v=r.endFrame-r.startFrame,b={duration:v*r.frameTime,keyframes:[{ignore:!1,percent:(l-r.startFrame)/v}]};l>r.startFrame&&b.keyframes.unshift({ignore:!0,percent:0}),(c-r.startFrame)/v<1&&b.keyframes.push({ignore:!0,percent:(c-r.startFrame)/v}),k.push(b)}k.length&&(o.keyframeAnimation=k),n.push(o)}}),n.filter(f=>{var c,u;const l=t.get((c=f.extra)==null?void 0:c.layerParent);return l?((u=l.children)==null||u.push(f),!1):!0})}function Xr(e,r){var s;mr(e);const i=new Ar;r=r||{},i.frameTime=1e3/(e.fr||30),i.startFrame=e.ip,i.endFrame=e.op,(s=e.assets)==null||s.forEach(f=>{i.assetsMap.set(f.id,f)});const n=Ze(e.layers||[],i);function t(f,l){f.forEach(c=>{l(c),c.children&&t(c.children,l)})}return r.loop&&t(n,f=>{var l;(l=f.keyframeAnimation)==null||l.forEach(c=>{c.loop=!0})}),{width:e.w,height:e.h,elements:n,each:f=>{t(n,f)}}}m.install=ar,m.parse=Xr,Object.defineProperty(m,"__esModule",{value:!0}),m[Symbol.toStringTag]="Module"});