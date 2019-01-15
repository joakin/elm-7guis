!function(n){"use strict";function r(n,r,t){return t.a=n,t.f=r,t}function e(t){return r(2,t,function(r){return function(n){return t(r,n)}})}function t(e){return r(3,e,function(t){return function(r){return function(n){return e(t,r,n)}}})}function u(u){return r(4,u,function(e){return function(t){return function(r){return function(n){return u(e,t,r,n)}}}})}function i(i){return r(5,i,function(u){return function(e){return function(t){return function(r){return function(n){return i(u,e,t,r,n)}}}}})}function g(n,r,t){return 2===n.a?n.f(r,t):n(r)(t)}function $(n,r,t,e){return 3===n.a?n.f(r,t,e):n(r)(t)(e)}function s(n,r,t,e,u){return 4===n.a?n.f(r,t,e,u):n(r)(t)(e)(u)}function a(n,r,t,e,u,i){return 5===n.a?n.f(r,t,e,u,i):n(r)(t)(e)(u)(i)}var l={$:0};function b(n,r){return{$:1,a:n,b:r}}var f=e(b);function d(n){for(var r=l,t=n.length;t--;)r=b(n[t],r);return r}function m(n,r,t){if("object"!=typeof n)return n===r?0:n<r?-1:1;if(void 0===n.$)return(t=m(n.a,r.a))?t:(t=m(n.b,r.b))?t:m(n.c,r.c);for(;n.b&&r.b&&!(t=m(n.a,r.a));n=n.b,r=r.b);return t||(n.b?1:r.b?-1:0)}var o=0;function h(n,r){return{a:n,b:r}}function c(n){return n}function v(n,r){var t={};for(var e in n)t[e]=n[e];for(var e in r)t[e]=r[e];return t}function p(n,r){if("string"==typeof n)return n+r;if(!n.b)return r;var t=b(n.a,r);n=n.b;for(var e=t;n.b;n=n.b)e=e.b=b(n.a,r);return t}var y=t(function(n,r,t){for(var e=Array(n),u=0;u<n;u++)e[u]=t(r+u);return e}),A=e(function(n,r){for(var t=Array(n),e=0;e<n&&r.b;e++)t[e]=r.a,r=r.b;return t.length=e,h(t,r)});function w(n){throw Error("https://github.com/elm/core/blob/1.0.0/hints/"+n+".md")}var j=Math.ceil,k=Math.floor,_=Math.log,N=isNaN;var C=e(function(n,r){return n+r});var E=t(function(n,r,t){for(var e=t.length;e--;){var u=t[e],i=t.charCodeAt(e);i<56320||57343<i||(u=t[--e]+u),r=g(n,c(u),r)}return r}),x=e(function(n,r){return r.split(n)}),L=t(function(n,r,t){return t.slice(n,r)});var O=e(function(n,r){return 0==r.indexOf(n)});function T(n){return n+""}function B(n){return{$:2,b:n}}B(function(n){return"number"!=typeof n?G("an INT",n):-2147483647<n&&n<2147483647&&(0|n)===n?Nr(n):!isFinite(n)||n%1?G("an INT",n):Nr(n)}),B(function(n){return"boolean"==typeof n?Nr(n):G("a BOOL",n)}),B(function(n){return"number"==typeof n?Nr(n):G("a FLOAT",n)}),B(function(n){return Nr(Q(n))});var F=B(function(n){return"string"==typeof n?Nr(n):n instanceof String?Nr(n+""):G("a STRING",n)});var q=e(function(n,r){return{$:6,d:n,b:r}});function z(n,r){return{$:9,f:n,g:r}}var S=e(function(n,r){return z(n,[r])}),M=e(function(n,r){return R(n,W(r))});function R(n,r){switch(n.$){case 2:return n.b(r);case 5:return null===r?Nr(n.c):G("null",r);case 3:return I(r)?D(n.b,r,d):G("a LIST",r);case 4:return I(r)?D(n.b,r,P):G("an ARRAY",r);case 6:var t=n.d;if("object"!=typeof r||null===r||!(t in r))return G("an OBJECT with a field named `"+t+"`",r);var e=R(n.b,r[t]);return ur(e)?e:_r(g(Er,t,e.a));case 7:var u=n.e;if(!I(r))return G("an ARRAY",r);if(r.length<=u)return G("a LONGER array. Need index "+u+" but only see "+r.length+" entries",r);e=R(n.b,r[u]);return ur(e)?e:_r(g(xr,u,e.a));case 8:if("object"!=typeof r||null===r||I(r))return G("an OBJECT",r);var i=l;for(var a in r)if(r.hasOwnProperty(a)){e=R(n.b,r[a]);if(!ur(e))return _r(g(Er,a,e.a));i=b(h(a,e.a),i)}return Nr(br(i));case 9:for(var f=n.f,o=n.g,c=0;c<o.length;c++){e=R(o[c],r);if(!ur(e))return e;f=f(e.a)}return Nr(f);case 10:e=R(n.b,r);return ur(e)?R(n.h(e.a),r):e;case 11:for(var v=l,s=n.g;s.b;s=s.b){e=R(s.a,r);if(ur(e))return e;v=b(e.a,v)}return _r(Lr(br(v)));case 1:return _r(g(Cr,n.a,Q(r)));case 0:return Nr(n.a)}}function D(n,r,t){for(var e=r.length,u=Array(e),i=0;i<e;i++){var a=R(n,r[i]);if(!ur(a))return _r(g(xr,i,a.a));u[i]=a.a}return Nr(t(u))}function I(n){return Array.isArray(n)||"function"==typeof FileList&&n instanceof FileList}function P(r){return g(wr,r.length,function(n){return r[n]})}function G(n,r){return _r(g(Cr,"Expecting "+n,Q(r)))}function J(n,r){if(n===r)return!0;if(n.$!==r.$)return!1;switch(n.$){case 0:case 1:return n.a===r.a;case 2:return n.b===r.b;case 5:return n.c===r.c;case 3:case 4:case 8:return J(n.b,r.b);case 6:return n.d===r.d&&J(n.b,r.b);case 7:return n.e===r.e&&J(n.b,r.b);case 9:return n.f===r.f&&Y(n.g,r.g);case 10:return n.h===r.h&&J(n.b,r.b);case 11:return Y(n.g,r.g)}}function Y(n,r){var t=n.length;if(t!==r.length)return!1;for(var e=0;e<t;e++)if(!J(n[e],r[e]))return!1;return!0}function Q(n){return n}function W(n){return n}Q(null);function H(n){return{$:0,a:n}}function K(n){return{$:2,b:n,c:null}}var U=e(function(n,r){return{$:3,b:n,d:r}});var V=0;function X(n){var r={$:0,e:V++,f:n,g:null,h:[]};return un(r),r}function Z(r){return K(function(n){n(H(X(r)))})}function nn(n,r){n.h.push(r),un(n)}var rn=e(function(r,t){return K(function(n){nn(r,t),n(H(o))})});var tn=!1,en=[];function un(n){if(en.push(n),!tn){for(tn=!0;n=en.shift();)an(n);tn=!1}}function an(r){for(;r.f;){var n=r.f.$;if(0===n||1===n){for(;r.g&&r.g.$!==n;)r.g=r.g.i;if(!r.g)return;r.f=r.g.b(r.f.a),r.g=r.g.i}else{if(2===n)return void(r.f.c=r.f.b(function(n){r.f=n,un(r)}));if(5===n){if(0===r.h.length)return;r.f=r.f.b(r.h.shift())}else r.g={$:3===n?0:1,b:r.f.b,i:r.g},r.f=r.f.d}}}function fn(n,r,t,e,u,i){var a=g(M,n,Q(r?r.flags:void 0));ur(a)||w(2);var f={},o=(a=t(a.a)).a,c=i(s,o),v=function(n,r){var t;for(var e in on){var u=on[e];u.a&&((t=t||{})[e]=u.a(e,r)),n[e]=vn(u,r)}return t}(f,s);function s(n,r){c(o=(a=g(e,n,o)).a,r),hn(f,a.b,u(o))}return hn(f,a.b,u(o)),v?{ports:v}:{}}var on={};function cn(n,r,t,e,u){return{b:n,c:r,d:t,e:e,f:u}}function vn(n,r){var e={g:r,h:void 0},u=n.c,i=n.d,a=n.e,f=n.f;function o(t){return g(U,o,{$:5,b:function(n){var r=n.a;return 0===n.$?$(i,e,r,t):a&&f?s(u,e,r.i,r.j,t):$(u,e,a?r.i:r.j,t)}})}return e.h=X(g(U,o,n.b))}var sn=e(function(r,t){return K(function(n){r.g(t),n(H(o))})}),ln=e(function(n,r){return g(rn,n.h,{$:0,a:r})});function bn(r){return function(n){return{$:1,k:r,l:n}}}function dn(n){return{$:2,m:n}}function hn(n,r,t){var e={};for(var u in gn(!0,r,e,null),gn(!1,t,e,null),n)nn(n[u],{$:"fx",a:e[u]||{i:l,j:l}})}function gn(n,r,t,e){switch(r.$){case 1:var u=r.k,i=function(n,r,t,e){function u(n){for(var r=t;r;r=r.q)n=r.p(n);return n}return g(n?on[r].e:on[r].f,u,e)}(n,u,e,r.l);return void(t[u]=function(n,r,t){return t=t||{i:l,j:l},n?t.i=b(r,t.i):t.j=b(r,t.j),t}(n,i,t[u]));case 2:for(var a=r.m;a.b;a=a.b)gn(n,a.a,t,e);return;case 3:return void gn(n,r.o,t,{p:r.n,q:e})}}var $n;var mn="undefined"!=typeof document?document:{};function pn(n,r){n.appendChild(r)}function yn(n){return{$:0,a:n}}var An=e(function(i,a){return e(function(n,r){for(var t=[],e=0;r.b;r=r.b){var u=r.a;e+=u.b||0,t.push(u)}return e+=t.length,{$:1,c:a,d:Cn(n),e:t,f:i,b:e}})})(void 0);e(function(i,a){return e(function(n,r){for(var t=[],e=0;r.b;r=r.b){var u=r.a;e+=u.b.b||0,t.push(u)}return e+=t.length,{$:2,c:a,d:Cn(n),e:t,f:i,b:e}})})(void 0);var wn=e(function(n,r){return{$:"a0",n:n,o:r}}),jn=e(function(n,r){return{$:"a1",n:n,o:r}}),kn=e(function(n,r){return{$:"a2",n:n,o:r}}),_n=e(function(n,r){return{$:"a3",n:n,o:r}});var Nn;function Cn(n){for(var r={};n.b;n=n.b){var t=n.a,e=t.$,u=t.n,i=t.o;if("a2"!==e){var a=r[e]||(r[e]={});"a3"===e&&"class"===u?En(a,u,i):a[u]=i}else"className"===u?En(r,u,W(i)):r[u]=W(i)}return r}function En(n,r,t){var e=n[r];n[r]=e?e+" "+t:t}function xn(n,r){var t=n.$;if(5===t)return xn(n.k||(n.k=n.m()),r);if(0===t)return mn.createTextNode(n.a);if(4===t){for(var e=n.k,u=n.j;4===e.$;)"object"!=typeof u?u=[u,e.j]:u.push(e.j),e=e.k;var i={j:u,p:r};return(a=xn(e,i)).elm_event_node_ref=i,a}if(3===t)return Ln(a=n.h(n.g),r,n.d),a;var a=n.f?mn.createElementNS(n.f,n.c):mn.createElement(n.c);$n&&"a"==n.c&&a.addEventListener("click",$n(a)),Ln(a,r,n.d);for(var f=n.e,o=0;o<f.length;o++)pn(a,xn(1===t?f[o]:f[o].b,r));return a}function Ln(n,r,t){for(var e in t){var u=t[e];"a1"===e?On(n,u):"a0"===e?Fn(n,r,u):"a3"===e?Tn(n,u):"a4"===e?Bn(n,u):("value"!==e&&"checked"!==e||n[e]!==u)&&(n[e]=u)}}function On(n,r){var t=n.style;for(var e in r)t[e]=r[e]}function Tn(n,r){for(var t in r){var e=r[t];void 0!==e?n.setAttribute(t,e):n.removeAttribute(t)}}function Bn(n,r){for(var t in r){var e=r[t],u=e.f,i=e.o;void 0!==i?n.setAttributeNS(u,t,i):n.removeAttributeNS(u,t)}}function Fn(n,r,t){var e=n.elmFs||(n.elmFs={});for(var u in t){var i=t[u],a=e[u];if(i){if(a){if(a.q.$===i.$){a.q=i;continue}n.removeEventListener(u,a)}a=qn(r,i),n.addEventListener(u,a,Nn&&{passive:nt(i)<2}),e[u]=a}else n.removeEventListener(u,a),e[u]=void 0}}try{window.addEventListener("t",null,Object.defineProperty({},"passive",{get:function(){Nn=!0}}))}catch(n){}function qn(v,n){function s(n){var r=s.q,t=R(r.a,n);if(ur(t)){for(var e,u=nt(r),i=t.a,a=u?u<3?i.a:i.o:i,f=1==u?i.b:3==u&&i.Q,o=(f&&n.stopPropagation(),(2==u?i.b:3==u&&i.P)&&n.preventDefault(),v);e=o.j;){if("function"==typeof e)a=e(a);else for(var c=e.length;c--;)a=e[c](a);o=o.p}o(a,f)}}return s.q=n,s}function zn(n,r){return n.$==r.$&&J(n.a,r.a)}function Sn(n,r){var t=[];return Rn(n,r,t,0),t}function Mn(n,r,t,e){var u={$:r,r:t,s:e,t:void 0,u:void 0};return n.push(u),u}function Rn(n,r,t,e){if(n!==r){var u=n.$,i=r.$;if(u!==i){if(1!==u||2!==i)return void Mn(t,0,e,r);r=function(n){for(var r=n.e,t=r.length,e=Array(t),u=0;u<t;u++)e[u]=r[u].b;return{$:1,c:n.c,d:n.d,e:e,f:n.f,b:n.b}}(r),i=1}switch(i){case 5:for(var a=n.l,f=r.l,o=a.length,c=o===f.length;c&&o--;)c=a[o]===f[o];if(c)return void(r.k=n.k);r.k=r.m();var v=[];return Rn(n.k,r.k,v,0),void(0<v.length&&Mn(t,1,e,v));case 4:for(var s=n.j,l=r.j,b=!1,d=n.k;4===d.$;)b=!0,"object"!=typeof s?s=[s,d.j]:s.push(d.j),d=d.k;for(var h=r.k;4===h.$;)b=!0,"object"!=typeof l?l=[l,h.j]:l.push(h.j),h=h.k;return b&&s.length!==l.length?void Mn(t,0,e,r):((b?function(n,r){for(var t=0;t<n.length;t++)if(n[t]!==r[t])return!1;return!0}(s,l):s===l)||Mn(t,2,e,l),void Rn(d,h,t,e+1));case 0:return void(n.a!==r.a&&Mn(t,3,e,r.a));case 1:return void Dn(n,r,t,e,Pn);case 2:return void Dn(n,r,t,e,Gn);case 3:if(n.h!==r.h)return void Mn(t,0,e,r);var g=In(n.d,r.d);g&&Mn(t,4,e,g);var $=r.i(n.g,r.g);return void($&&Mn(t,5,e,$))}}}function Dn(n,r,t,e,u){if(n.c===r.c&&n.f===r.f){var i=In(n.d,r.d);i&&Mn(t,4,e,i),u(n,r,t,e)}else Mn(t,0,e,r)}function In(n,r,t){var e;for(var u in n)if("a1"!==u&&"a0"!==u&&"a3"!==u&&"a4"!==u)if(u in r){var i=n[u],a=r[u];i===a&&"value"!==u&&"checked"!==u||"a0"===t&&zn(i,a)||((e=e||{})[u]=a)}else(e=e||{})[u]=t?"a1"===t?"":"a0"===t||"a3"===t?void 0:{f:n[u].f,o:void 0}:"string"==typeof n[u]?"":null;else{var f=In(n[u],r[u]||{},u);f&&((e=e||{})[u]=f)}for(var o in r)o in n||((e=e||{})[o]=r[o]);return e}function Pn(n,r,t,e){var u=n.e,i=r.e,a=u.length,f=i.length;f<a?Mn(t,6,e,{v:f,i:a-f}):a<f&&Mn(t,7,e,{v:a,e:i});for(var o=a<f?a:f,c=0;c<o;c++){var v=u[c];Rn(v,i[c],t,++e),e+=v.b||0}}function Gn(n,r,t,e){for(var u=[],i={},a=[],f=n.e,o=r.e,c=f.length,v=o.length,s=0,l=0,b=e;s<c&&l<v;){var d=(N=f[s]).a,h=(C=o[l]).a,g=N.b,$=C.b,m=void 0,p=void 0;if(d!==h){var y=f[s+1],A=o[l+1];if(y){var w=y.a,j=y.b;p=h===w}if(A){var k=A.a,_=A.b;m=d===k}if(m&&p)Rn(g,_,u,++b),Yn(i,u,d,$,l,a),b+=g.b||0,Qn(i,u,d,j,++b),b+=j.b||0,s+=2,l+=2;else if(m)b++,Yn(i,u,h,$,l,a),Rn(g,_,u,b),b+=g.b||0,s+=1,l+=2;else if(p)Qn(i,u,d,g,++b),b+=g.b||0,Rn(j,$,u,++b),b+=j.b||0,s+=2,l+=1;else{if(!y||w!==k)break;Qn(i,u,d,g,++b),Yn(i,u,h,$,l,a),b+=g.b||0,Rn(j,_,u,++b),b+=j.b||0,s+=2,l+=2}}else Rn(g,$,u,++b),b+=g.b||0,s++,l++}for(;s<c;){var N;Qn(i,u,(N=f[s]).a,g=N.b,++b),b+=g.b||0,s++}for(;l<v;){var C,E=E||[];Yn(i,u,(C=o[l]).a,C.b,void 0,E),l++}(0<u.length||0<a.length||E)&&Mn(t,8,e,{w:u,x:a,y:E})}var Jn="_elmW6BL";function Yn(n,r,t,e,u,i){var a=n[t];if(!a)return i.push({r:u,A:a={c:0,z:e,r:u,s:void 0}}),void(n[t]=a);if(1===a.c){i.push({r:u,A:a}),a.c=2;var f=[];return Rn(a.z,e,f,a.r),a.r=u,void(a.s.s={w:f,A:a})}Yn(n,r,t+Jn,e,u,i)}function Qn(n,r,t,e,u){var i=n[t];if(i){if(0===i.c){i.c=2;var a=[];return Rn(e,i.z,a,u),void Mn(r,9,u,{w:a,A:i})}Qn(n,r,t+Jn,e,u)}else{var f=Mn(r,9,u,void 0);n[t]={c:1,z:e,r:u,s:f}}}function Wn(n,r,t,e){!function n(r,t,e,u,i,a,f){var o=e[u];var c=o.r;for(;c===i;){var v=o.$;if(1===v)Wn(r,t.k,o.s,f);else if(8===v){o.t=r,o.u=f;var s=o.s.w;0<s.length&&n(r,t,s,0,i,a,f)}else if(9===v){o.t=r,o.u=f;var l=o.s;if(l){l.A.s=r;var s=l.w;0<s.length&&n(r,t,s,0,i,a,f)}}else o.t=r,o.u=f;if(!(o=e[++u])||(c=o.r)>a)return u}var b=t.$;if(4===b){for(var d=t.k;4===d.$;)d=d.k;return n(r,d,e,u,i+1,a,r.elm_event_node_ref)}var h=t.e;var g=r.childNodes;for(var $=0;$<h.length;$++){var m=1===b?h[$]:h[$].b,p=++i+(m.b||0);if(i<=c&&c<=p&&(u=n(g[$],m,e,u,i,p,f),!(o=e[u])||(c=o.r)>a))return u;i=p}return u}(n,r,t,0,0,r.b,e)}function Hn(n,r,t,e){return 0===t.length?n:(Wn(n,r,t,e),Kn(n,t))}function Kn(n,r){for(var t=0;t<r.length;t++){var e=r[t],u=e.t,i=Un(u,e);u===n&&(n=i)}return n}function Un(n,r){switch(r.$){case 0:return function(n,r,t){var e=n.parentNode,u=xn(r,t);u.elm_event_node_ref||(u.elm_event_node_ref=n.elm_event_node_ref);e&&u!==n&&e.replaceChild(u,n);return u}(n,r.s,r.u);case 4:return Ln(n,r.u,r.s),n;case 3:return n.replaceData(0,n.length,r.s),n;case 1:return Kn(n,r.s);case 2:return n.elm_event_node_ref?n.elm_event_node_ref.j=r.s:n.elm_event_node_ref={j:r.s,p:r.u},n;case 6:for(var t=r.s,e=0;e<t.i;e++)n.removeChild(n.childNodes[t.v]);return n;case 7:for(var u=(t=r.s).e,i=n.childNodes[e=t.v];e<u.length;e++)n.insertBefore(xn(u[e],r.u),i);return n;case 9:if(!(t=r.s))return n.parentNode.removeChild(n),n;var a=t.A;return void 0!==a.r&&n.parentNode.removeChild(n),a.s=Kn(n,t.w),n;case 8:return function(n,r){var t=r.s,e=function(n,r){if(!n)return;for(var t=mn.createDocumentFragment(),e=0;e<n.length;e++){var u=n[e],i=u.A;pn(t,2===i.c?i.s:xn(i.z,r.u))}return t}(t.y,r);n=Kn(n,t.w);for(var u=t.x,i=0;i<u.length;i++){var a=u[i],f=a.A,o=2===f.c?f.s:xn(f.z,r.u);n.insertBefore(o,n.childNodes[a.r])}e&&pn(n,e);return n}(n,r);case 5:return r.s(n);default:w(10)}}function Vn(n){if(3===n.nodeType)return yn(n.textContent);if(1!==n.nodeType)return yn("");for(var r=l,t=n.attributes,e=t.length;e--;){var u=t[e];r=b(g(_n,u.name,u.value),r)}var i=n.tagName.toLowerCase(),a=l,f=n.childNodes;for(e=f.length;e--;)a=b(Vn(f[e]),a);return $(An,i,r,a)}var Xn=u(function(r,n,t,f){return fn(n,f,r.aO,r.a1,r.a$,function(e,n){var u=r.a3,i=f.node,a=Vn(i);return rr(n,function(n){var r=u(n),t=Sn(a,r);i=Hn(i,a,t,e),a=r})})}),Zn="undefined"!=typeof cancelAnimationFrame?cancelAnimationFrame:function(n){clearTimeout(n)},nr="undefined"!=typeof requestAnimationFrame?requestAnimationFrame:function(n){return setTimeout(n,1e3/60)};function rr(t,e){e(t);var u=0;function i(){u=1===u?0:(nr(i),e(t),1)}return function(n,r){t=n,r?(e(t),2===u&&(u=1)):(0===u&&nr(i),u=2)}}var tr={addEventListener:function(){},removeEventListener:function(){}};"undefined"!=typeof document&&document,"undefined"!=typeof window&&window;var er=f,ur=function(n){return!n.$},ir=u(function(n,r,t,e){return{$:0,a:n,b:r,c:t,d:e}}),ar=j,fr=e(function(n,r){return _(r)/_(n)}),or=ar(g(fr,2,32)),cr=[],vr=s(ir,0,or,cr,cr),sr=A,lr=t(function(n,r,t){for(;;){if(!t.b)return r;var e=t.b,u=n,i=g(n,t.a,r);n=u,r=i,t=e}}),br=function(n){return $(lr,er,l,n)},dr=e(function(n,r){for(;;){var t=g(sr,32,n),e=t.b,u=g(er,{$:0,a:t.a},r);if(!e.b)return br(u);n=e,r=u}}),hr=e(function(n,r){for(;;){var t=ar(r/32);if(1===t)return g(sr,32,n).a;n=g(dr,n,l),r=t}}),gr=k,$r=e(function(n,r){return 0<m(n,r)?n:r}),mr=function(n){return n.length},pr=e(function(n,r){if(r.e){var t=32*r.e,e=gr(g(fr,32,t-1)),u=n?br(r.h):r.h,i=g(hr,u,r.e);return s(ir,mr(r.g)+t,g($r,5,e*or),i,r.g)}return s(ir,mr(r.g),or,cr,r.g)}),yr=y,Ar=i(function(n,r,t,e,u){for(;;){if(r<0)return g(pr,!1,{h:e,e:t/32|0,g:u});var i={$:1,a:$(yr,32,r,n)};n=n,r=r-32,t=t,e=g(er,i,e),u=u}}),wr=e(function(n,r){if(0<n){var t=n%32;return a(Ar,r,n-t-32,n,l,$(yr,t,n-t,r))}return vr}),jr=function(n){return{$:0,a:n}},kr={$:1},_r=function(n){return{$:1,a:n}},Nr=function(n){return{$:0,a:n}},Cr=e(function(n,r){return{$:3,a:n,b:r}}),Er=e(function(n,r){return{$:0,a:n,b:r}}),xr=e(function(n,r){return{$:1,a:n,b:r}}),Lr=function(n){return{$:2,a:n}},Or=function(n){var r=n.charCodeAt(0);return r<55296||56319<r?r:1024*(r-55296)+n.charCodeAt(1)-56320+65536},Tr=function(n){var r=n.charCodeAt(0);return r?jr(r<55296||56319<r?h(c(n[0]),n.slice(1)):h(c(n[0]+n[1]),n.slice(2))):kr},Br=e(function(n,r){return d(g(x,n,r))}),Fr=dn(l),qr=function(){return h({r:0,B:15e3},Fr)},zr=function(n){return{$:0,a:n}},Sr=function(n){return{$:1,a:n}},Mr=t(function(n,r,t){return{O:t,at:r,az:n}}),Rr=H,Dr=Rr($(Mr,l,kr,0)),Ir=function(n){return n},Pr=Rr(0),Gr=u(function(n,r,t,e){if(e.b){var u=e.a,i=e.b;if(i.b){var a=i.a,f=i.b;if(f.b){var o=f.a,c=f.b;if(c.b){var v=c.b;return g(n,u,g(n,a,g(n,o,g(n,c.a,500<t?$(lr,n,r,br(v)):s(Gr,n,r,t+1,v)))))}return g(n,u,g(n,a,g(n,o,r)))}return g(n,u,g(n,a,r))}return g(n,u,r)}return r}),Jr=t(function(n,r,t){return s(Gr,n,r,0,t)}),Yr=e(function(t,n){return $(Jr,e(function(n,r){return g(er,t(n),r)}),l,n)}),Qr=U,Wr=e(function(r,n){return g(Qr,function(n){return Rr(r(n))},n)}),Hr=t(function(t,n,e){return g(Qr,function(r){return g(Qr,function(n){return Rr(g(t,r,n))},e)},n)}),Kr=function(n){return $(Jr,Hr(er),Rr(l),n)},Ur=sn,Vr=e(function(n,r){var t=r;return Z(g(Qr,Ur(n),t))});on.Task=cn(Pr,t(function(n,r){return g(Wr,function(){return 0},Kr(g(Yr,Vr(n),r)))}),t(function(){return Rr(0)}),e(function(n,r){return g(Wr,n,r)}));bn("Task");var Xr=S,Zr=function(n){return{$:0,a:n}},nt=function(n){switch(n.$){case 0:return 0;case 1:return 1;case 2:return 2;default:return 3}},rt=function(n){return n.length},tt=L,et=e(function(n,r){return n<1?r:$(tt,n,rt(r),r)}),ut=O,it=function(n){for(var r=0,t=n.charCodeAt(0),e=43==t||45==t?1:0,u=e;u<n.length;++u){var i=n.charCodeAt(u);if(i<48||57<i)return kr;r=10*r+i-48}return u==e?kr:jr(45==t?-r:r)},at=K(function(n){n(H(Date.now()))}),ft=K(function(n){var r=nr(function(){n(H(Date.now()))});return function(){Zn(r)}}),ot=ln,ct=function(t){return K(function(n){var r=t.f;2===r.$&&r.c&&r.c(),t.f=null,n(H(o))})},vt=Z,st=t(function(n,t,r){var e=r.at,u=r.O,i=h(e,t);if(1!==i.a.$)return i.b.b?Rr($(Mr,t,e,u)):g(Qr,function(){return Dr},ct(i.a.a));if(i.b.b){return g(Qr,function(r){return g(Qr,function(n){return Rr($(Mr,t,jr(r),n))},at)},vt(g(Qr,ot(n),ft)))}return Dr}),lt=Ir,bt=t(function(r,t,n){var e=n.az,u=n.O,i=function(n){return g(Ur,r,n.$?(0,n.a)(t-u):(0,n.a)(lt(t)))};return g(Qr,function(n){return g(Qr,function(){return Rr($(Mr,e,jr(n),t))},Kr(g(Yr,i,e)))},vt(g(Qr,ot(r),ft)))}),dt=t(function(n,r,t){return n(r(t))});on["Browser.AnimationManager"]=cn(Dr,st,bt,0,e(function(n,r){return r.$?Sr(g(dt,n,r.a)):{$:0,a:g(dt,n,r.a)}}));var ht,gt=bn("Browser.AnimationManager"),$t=function(n){return gt(Sr(n))},mt=dn(l),pt=e(function(n,r){switch(n.$){case 0:return h(v(r,{r:r.r+n.a}),Fr);case 1:return h(v(r,{B:n.a}),Fr);default:return qr()}}),yt={$:2},At=An("div"),wt=jn,jt=e(function(n,r){return g(At,g(er,g(wt,"padding","0.5rem"),n),r)}),kt=function(n){return{$:1,a:n}},_t=function(n){return n===1/0||n===-1/0},Nt=e(function(n,r){return r.$?n:r.a}),Ct=T,Et=function(n){if(0===n.length||/[\sxbo]/.test(n))return kr;var r=+n;return r==r?jr(r):kr},xt=An("input"),Lt=Q,Ot=e(function(n,r){return g(kn,n,Lt(r))}),Tt=Ot("max"),Bt=Ot("min"),Ft=Ot("type"),qt=Ot("value"),zt=function(n){return h(n,!0)},St=wn,Mt=e(function(n,r){return g(St,n,{$:1,a:r})}),Rt=q,Dt=F,It=g(e(function(n,r){return $(Jr,Rt,r,n)}),d(["target","value"]),Dt),Pt=e(function(n,r){return g(xt,p(d([Ft("range"),Bt("0"),Tt("30000"),qt(_t(r)?"30000":Ct(r)),(e="100",g(Ot,"step",e)),(t=g(dt,kt,function(n){return g(Nt,0,Et(n))}),g(Mt,"input",g(Xr,zt,g(Xr,t,It))))]),n),l);var t,e}),Gt=An("progress"),Jt=e(function(n,r){return g(Gt,p(d([Tt("100"),qt(_t(r)?"100":Ct(r))]),n),l)}),Yt=An("button"),Qt=An("span"),Wt=yn,Ht=e(function(n,r){return g(St,n,{$:0,a:r})}),Kt=function(n){return n<0?-n:n},Ut=N,Vt=e(function(n,r){return r.$?kr:jr(n(r.a))}),Xt=C,Zt=function(n){return g(Xt,n,"")},ne=t(function(n,r,t){return 0<n?$(ne,n>>1,p(r,r),1&n?p(t,r):t):t}),re=e(function(n,r){return $(ne,n,r,"")}),te=t(function(n,r,t){return p(t,g(re,n-rt(t),Zt(r)))}),ee=function(n){for(var r=n.length,t=Array(r),e=0;e<r;){var u=n.charCodeAt(e);u<55296||56319<u?t[r-e]=n[e]:(t[r-e]=n[e+1],t[r-++e]=n[e-1]),e++}return t.join("")},ue=e(function(n,r){for(;;){if(!r.b)return!1;var t=r.b;if(n(r.a))return!0;n=n,r=t}}),ie=E,ae=e(function(n,r){var t=g(ue,function(n){return"0"!==n&&"."!==n},$(ie,er,l,r));return p(n&&t?"-":"",r)}),fe=function(n){return c(n<0||1114111<n?"�":65535<n?String.fromCharCode(55296+Math.floor((n-=65536)/1024),n%1024+56320):String.fromCharCode(n))},oe=function(n){var r=n.a,t=n.b;if("9"===r){var e=Tr(t);return 1===e.$?"01":g(Xt,"0",oe(e.a))}var u=Or(r);return 48<=u&&u<57?g(Xt,fe(u+1),t):"0"},ce=function(n){var r=g(Br,".",n);return r.b?h(r.a,r.b.b?r.b.a:"0"):h("0","0")},ve=e(function(n,r){var t=r.b;return h(n(r.a),t)}),se=t(function(n,r,t){if(_t(t)||Ut(t))return Ct(t);var e=t<0,u=ce(function(n){var r=g(Br,"e",Ct(Kt(n)));if(r.b){if(r.b.b){var t=r.a,e=r.b.a,u=g(Nt,0,it(g(ut,"+",e)?g(et,1,e):e)),i=ce(t),a=p(i.a,i.b);return p(n<0?"-":"",u<0?g(Nt,"0",g(Vt,function(n){return n.a+"."+n.b},g(Vt,ve(Zt),Tr(p(g(re,Kt(u),"0"),a))))):$(te,u+1,"0",a))}return p(n<0?"-":"",t=r.a)}return""}(Kt(t))),i=u.a,a=u.b,f=rt(i)+r,o=p(g(re,1-f,"0"),$(te,f,"0",p(i,a))),c=rt(o),v=g($r,1,f),s=g(n,e,$(tt,v,c,o)),l=$(tt,0,v,o),b=s?ee(g(Nt,"1",g(Vt,oe,Tr(ee(l))))):l,d=rt(b),h="0"===b?b:0<r?m(r,rt(a))<0?$(tt,0,d-r,b)+"."+$(tt,d-r,d,b):p(i+".",$(te,r,"0",a)):p(b,g(re,Kt(r),"0"));return g(ae,e,h)})(e(function(n,r){var t=Tr(r);if(1===t.$)return!1;if("5"!==t.a.a)return 53<(e=Or(t.a.a))&&n||53<=e&&!n;if(""===t.a.b){return!n}var e;return!0})),le=Xn({aO:qr,a$:function(n){return m(n.r,n.B)<0?$t(zr):mt},a1:pt,a3:function(n){var r,t=n.r,e=n.B,u=100*t/e;return g(At,d([g(wt,"display","flex"),g(wt,"flex-direction","column")]),d([g(jt,d([g(wt,"display","flex"),g(wt,"align-items","center")]),d([g(Qt,d([g(wt,"margin-right","0.5rem")]),d([Wt("Elapsed time:")])),g(Jt,d([g(wt,"flex","1")]),u)])),g(jt,l,d([Wt(g(se,1,t/1e3)+"s")])),g(jt,d([g(wt,"display","flex"),g(wt,"align-items","center")]),d([g(Qt,d([g(wt,"margin-right","0.5rem")]),d([Wt("Duration:")])),g(Pt,d([g(wt,"flex","1")]),e)])),g(jt,l,d([g(Yt,d([g(wt,"width","100%"),(r=yt,g(Ht,"click",Zr(r)))]),d([Wt("Reset")]))]))]))}});ht={Main:{init:le(Zr(0))(0)}},n.Elm?function n(r,t){for(var e in t)e in r?"init"==e?w(6):n(r[e],t[e]):r[e]=t[e]}(n.Elm,ht):n.Elm=ht}(this);