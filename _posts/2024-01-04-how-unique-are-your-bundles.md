---
layout: post
title: "How unique are your bundles?"
date: '2024-01-04T14:01:24+10:00'
---

In the modern front-end development we are all used to package managers, transpilation and bundling.
These concepts are the biproduct of the tools which we hoped would simplify developer' burden and
speed up the development.

However, how confident are you these tools are actually doing good job?

Developers seem comfortable off-loading the processing power to users' machine (browser, predominantly).
We are not surprised by seeing slow websites anymore. A simple blog is downloading 55MB of JavaScript?
Seems fine nowadays.

I currently work on a fairly small tool ([MongoDB Relational Migrator](https://www.mongodb.com/products/relational-migrator)),
which also utilizes TypeScript, React and, of course, bundling.
We use Vite for that. Our bundles are split into chunks (but I have accounted for that, too).

I went ahead and wrote a rather [simple script](https://github.com/shybovycha/js-unique-function-analyzer)
which parses the bundles (using TypeScript compiler API, because why not),
extracting the function definitions (both arrow functions and plain old `function`) and counts how many times they
occur in the file. For this last bit, to make sure I am not counting `a => true` and `x => true` as different occurrences,
I am minimizing the function definition with `uglifyjs` and counting the SHA256 hashes of the minimized functions
(just to have a reasonable key in my hashmap instead of entire function code).

These are my findings.

Out of 54 chunks, 47 are not css-in-js chunks. Out of 47 remaining, 7 have any significant duplication (over 5%).
But when they do, they do it hard: duplication varies between 18% and a whopping 42% of sheer file size.
Absolute numbers are also astonishing: 33% to 59% functions are duplicates.

```
Found 15192 functions, 8963 are unique (59%)
Duplicates length: 1518418 bytes out of 3537579 bytes are duplicate code (42.92%)

Found 1202 functions, 494 are unique (41.1%)
Duplicates length: 130649 bytes out of 340227 bytes are duplicate code (38.4%)

Found 513 functions, 231 are unique (45.03%)
Duplicates length: 50160 bytes out of 136057 bytes are duplicate code (36.87%)

Found 598 functions, 267 are unique (44.65%)
Duplicates length: 57607 bytes out of 164737 bytes are duplicate code (34.97%)

Found 17 functions, 10 are unique (58.82%)
Duplicates length: 1932 bytes out of 6532 bytes are duplicate code (29.58%)

Found 154 functions, 98 are unique (63.64%)
Duplicates length: 11140 bytes out of 45135 bytes are duplicate code (24.68%)

Found 968 functions, 651 are unique (67.25%)
Duplicates length: 52616 bytes out of 281406 bytes are duplicate code (18.7%)
```

I thought my code might be wrong, so I looked into the bundle code itself. Here's a short excerpt:

```js
Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},yR.apply(this,arguments)}var Zce;function bR(){return bR=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},bR.apply(this,arguments)}var Wce;function wR(){return wR=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},wR.apply(this,arguments)}var Uce;function $R(){return $R=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},$R.apply(this,arguments)}var Gce;function OR(){return OR=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},OR.apply(this,arguments)}var Kce;function xR(){return xR=Object.assign?Object.assign.bind():function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e},xR.apply(this,arguments)}
```

See how the following fragment of code repeats multiple times:

```js
function(e){for(var t=1;t<arguments.length;t++){var n=arguments[t];for(var r in n)Object.prototype.hasOwnProperty.call(n,r)&&(e[r]=n[r])}return e}
```

In fact, this exact same fragment of code repeats 137 times in the same piece of bundle chunk (same file):

<img src="/images/how-unique-are-your-bundles/duplication1.png" loading="lazy" alt="Repeated function definition in a single chunk of code">

By the way, this is a production build of our front-end, built using Vite, with minification enabled.

The raw length of this function code is `146` characters. So in a single place, in a single file, you have `136 * 146 = 19_992` bytes of waste.
Meaning, browser has to load these 20KB of code, parse it and create 136 duplicating functions.

Looking at the overall size of 3.5MB of code in this chunk and its insane 41% duplicated code (in sheer bytes, not occurrences, so 1.5MB wasted),
imagine how much faster this single file _could_ have been loaded in a browser.

I was keen on seeing what functions get duplicated most often and ran my script on an entire build output directory.
Here are top 80-ish offenders:

<table id="vite-1" class="expandable">
    <thead>
        <th>Function code (minimized)</th>
        <th>Occurrences</th>
    </thead>
    <tbody>
        <tr><td><code>function(r){for(var t=1;t&lt;arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}</code></td><td>2205</td></tr>
        <tr><td><code>function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}</code></td><td>1197</td></tr>
        <tr><td><code>function n(){return Object.assign,n.apply(this,arguments)}</code></td><td>1008</td></tr>
        <tr><td><code>function(){}</code></td><td>753</td></tr>
        <tr><td><code>function(i){this.a=i}</code></td><td>250</td></tr>
        <tr><td><code>function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u&lt;f.length;u++)r=f[u],0&lt;=e.indexOf(r)||(t[r]=n[r]);return t}</code></td><td>191</td></tr>
        <tr><td><code>function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o&lt;l.length;o++)t=l[o],0&lt;=r.indexOf(t)||(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o&lt;l.length;o++)t=l[o],0&lt;=r.indexOf(t)||Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}</code></td><td>187</td></tr>
        <tr><td><code>function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}</code></td><td>159</td></tr>
        <tr><td><code>function(n){return this===n}</code></td><td>119</td></tr>
        <tr><td><code>function(r,t){if("object"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t||"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")}</code></td><td>113</td></tr>
        <tr><td><code>function(r,e,t){return i=function(r,e){if("object"!=typeof r||null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if("object"!=typeof t)return t;throw new TypeError("@@toPrimitive must return a primitive value.")}(e,"string"),(e="symbol"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}</code></td><td>111</td></tr>
        <tr><td><code>function(t){t=function(t,r){if("object"!=typeof t||null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if("object"!=typeof i)return i;throw new TypeError("@@toPrimitive must return a primitive value.")}(t,"string");return"symbol"==typeof t?t:String(t)}</code></td><td>111</td></tr>
        <tr><td><code>function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}</code></td><td>104</td></tr>
        <tr><td><code>function(c,i){He.call(this,c,i)}</code></td><td>94</td></tr>
        <tr><td><code>function(n,r){(null==r||r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e&lt;r;e++)l[e]=n[e];return l}</code></td><td>93</td></tr>
        <tr><td><code>function(){return!0}</code></td><td>92</td></tr>
        <tr><td><code>function(){return!1}</code></td><td>78</td></tr>
        <tr><td><code>function(){return new gt(this)}</code></td><td>77</td></tr>
        <tr><td><code>function(r){if(Array.isArray(r))return r}</code></td><td>77</td></tr>
        <tr><td><code>function(){throw new TypeError(`Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}</code></td><td>77</td></tr>
        <tr><td><code>function(t){return t&&"object"==typeof t&&"default"in t?t:{default:t}}</code></td><td>76</td></tr>
        <tr><td><code>function(i,t){this.a=i,this.b=t}</code></td><td>58</td></tr>
        <tr><td><code>function(){return this.a}</code></td><td>49</td></tr>
        <tr><td><code>function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}</code></td><td>49</td></tr>
        <tr><td><code>function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(c,e),Object.defineProperty(y,e,t.get?t:{enumerable:!0,get:function(){return c[e]}}))}</code></td><td>49</td></tr>
        <tr><td><code>function(){return c[b]}</code></td><td>49</td></tr>
        <tr><td><code>function(a,e,i){var r,t=i["aria-label"],n=i["aria-labelledby"],c=i.title;switch(a){case"img":return t||n||c?(l(r={},"aria-labelledby",n),l(r,"aria-label",t),l(r,"title",c),r):{"aria-label":"".concat(e.replace(/([a-z])([A-Z])/g,"$1 $2")," Icon")};case"presentation":return{"aria-hidden":!0,alt:""}}}</code></td><td>49</td></tr>
        <tr><td><code>function(i){Di(this,i)}</code></td><td>48</td></tr>
        <tr><td><code>function(r){return Object.getOwnPropertyDescriptor(e,r).enumerable}</code></td><td>48</td></tr>
        <tr><td><code>function(){throw M(new De)}</code></td><td>46</td></tr>
        <tr><td><code>function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]||l["@@iterator"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r||e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a||null==t.return||t.return()}finally{if(o)throw u}}return e}}</code></td><td>44</td></tr>
        <tr><td><code>function(n){throw M(new De)}</code></td><td>39</td></tr>
        <tr><td><code>function(r){var n;return r&&"object"==typeof r&&"default"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}</code></td><td>39</td></tr>
        <tr><td><code>function n(r){return n(r)}</code></td><td>38</td></tr>
        <tr><td><code>function(n){return typeof n}</code></td><td>38</td></tr>
        <tr><td><code>function(o){return o&&"function"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o}</code></td><td>38</td></tr>
        <tr><td><code>function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}</code></td><td>37</td></tr>
        <tr><td><code>function(){return this.b}</code></td><td>33</td></tr>
        <tr><td><code>function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]||l["@@iterator"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}</code></td><td>33</td></tr>
        <tr><td><code>function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}</code></td><td>32</td></tr>
        <tr><td><code>function(n){return Ei(n)}</code></td><td>30</td></tr>
        <tr><td><code>function(n){return L(fn,X,2,n,6,1)}</code></td><td>30</td></tr>
        <tr><td><code>function(n){}</code></td><td>29</td></tr>
        <tr><td><code>function(r){if(typeof Symbol<"u"&&null!=r[Symbol.iterator]||null!=r["@@iterator"])return Array.from(r)}</code></td><td>28</td></tr>
        <tr><td><code>function(){throw new TypeError(`Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}</code></td><td>28</td></tr>
        <tr><td><code>()=>{}</code></td><td>27</td></tr>
        <tr><td><code>function(){return null}</code></td><td>27</td></tr>
        <tr><td><code>function(n,o){e.exports=o(m,on(),dn)}</code></td><td>27</td></tr>
        <tr><td><code>function(i){Fi(this,i)}</code></td><td>23</td></tr>
        <tr><td><code>function(n,o){e.exports=o(dn,on(),m)}</code></td><td>22</td></tr>
        <tr><td><code>function(){throw M(new Fe(Re((xe(),Ds))))}</code></td><td>21</td></tr>
        <tr><td><code>function(){return this}</code></td><td>20</td></tr>
        <tr><td><code>function(i,t){this.b=i,this.a=t}</code></td><td>19</td></tr>
        <tr><td><code>function(n,e){throw M(new De)}</code></td><td>18</td></tr>
        <tr><td><code>function(){return 0}</code></td><td>17</td></tr>
        <tr><td><code>function(r){Object.defineProperty(e,r,Object.getOwnPropertyDescriptor(t,r))}</code></td><td>16</td></tr>
        <tr><td><code>()=>{var l;return null!=(l=e.options.debugAll)?l:e.options.debugHeaders}</code></td><td>16</td></tr>
        <tr><td><code>function(n){return n}</code></td><td>15</td></tr>
        <tr><td><code>function(c){Kr.call(this,c)}</code></td><td>15</td></tr>
        <tr><td><code>function(){return this.c}</code></td><td>15</td></tr>
        <tr><td><code>function(){return this.d}</code></td><td>15</td></tr>
        <tr><td><code>function(n,r){return n}</code></td><td>14</td></tr>
        <tr><td><code>d=>d.id</code></td><td>14</td></tr>
        <tr><td><code>function(){var r=function(t,o){return(r=Object.setPrototypeOf||({__proto__:[]}instanceof Array?function(t,o){t.__proto__=o}:function(t,o){for(var n in o)Object.prototype.hasOwnProperty.call(o,n)&&(t[n]=o[n])}))(t,o)};return function(t,o){if("function"!=typeof o&&null!==o)throw new TypeError("Class extends value "+String(o)+" is not a constructor or null");function n(){this.constructor=t}r(t,o),t.prototype=null===o?Object.create(o):(n.prototype=o.prototype,new n)}}</code></td><td>14</td></tr>
        <tr><td><code>function(_,o){_.__proto__=o}</code></td><td>14</td></tr>
        <tr><td><code>function(o,r){for(var t in r)Object.prototype.hasOwnProperty.call(r,t)&&(o[t]=r[t])}</code></td><td>14</td></tr>
        <tr><td><code>function(n){return!1}</code></td><td>13</td></tr>
        <tr><td><code>function(t){var l=-1,n=null==t?0:t.length;for(this.clear();++l&lt;n;){var r=t[l];this.set(r[0],r[1])}}</code></td><td>12</td></tr>
        <tr><td><code>function(a,e,l){var i,r=l["aria-label"],t=l["aria-labelledby"],n=l.title;switch(a){case"img":return r||t||n?(f(i={},"aria-labelledby",t),f(i,"aria-label",r),f(i,"title",n),i):{"aria-label":"".concat(e.replace(/([a-z])([A-Z])/g,"$1 $2")," Icon")};case"presentation":return{"aria-hidden":!0,alt:""}}}</code></td><td>12</td></tr>
        <tr><td><code>a=>a</code></td><td>12</td></tr>
        <tr><td><code>a=>a()</code></td><td>12</td></tr>
        <tr><td><code>function(n,a){n.a=a}</code></td><td>11</td></tr>
        <tr><td><code>function(){ia.call(this)}</code></td><td>11</td></tr>
        <tr><td><code>function(i,t,h){this.a=i,this.b=t,this.c=h}</code></td><td>11</td></tr>
        <tr><td><code>function(n,r){return r}</code></td><td>11</td></tr>
        <tr><td><code>function(){return this.a.gc()}</code></td><td>11</td></tr>
        <tr><td><code>function(e){return e&&e.__esModule?e:{default:e}}</code></td><td>11</td></tr>
        <tr><td><code>()=>n(!1)</code></td><td>11</td></tr>
        <tr><td><code>function(i){this.b=i}</code></td><td>10</td></tr>
        <tr><td><code>function(c){this.c=c}</code></td><td>10</td></tr>
        <tr><td><code>function(){return this.f}</code></td><td>10</td></tr>
        <tr><td><code>function(n){return n||"div"}</code></td><td>10</td></tr>
        <tr><td><code>function(n,r,i){var l;return i=null!=(l=i)?l:"div",n||("string"==typeof(null==r?void 0:r.href)?"a":i)}</code></td><td>10</td></tr>
    </tbody>
</table>
<div class="center">
    <button class="expand-table-toggle" data-table-id="vite-1">Show more</button>
</div>

Let's dive deeper, shall we?

Imagine for a second that we could define the duplicated functions once and then just reuse the short name instead
(sounds reasonable, does it not?).

But not all of those functions could be de-duplicated in such way (at least not so easily).
Some of these functions use the outer closure functions and variables (something defined outside of the function itself), so we can skip these.
For instance, `function(i){Di(this,i)}` and `function(){throw M(new De)}` can be ignored.

Then, there are function using `this`. These might be tricky (`this is not what you think` is a famous JavaScript mantra).

Lastly, some (if not most) of the functions could be either replaced with arrow functions or standard library function.
But that is uncertain - one must understand what a function does first.

With those points in mind, let's look at the offenders once again:

<table id="vite-2" class="expandable">
    <thead>
        <th>Function code (minimized)</th>
        <th>Occurrences</th>
        <th>Notes</th>
    </thead>
    <tbody>
        <tr class="green"><td><code>function(r){for(var t=1;t&lt;arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}</code></td><td>2205</td><td>spread operator?</td></tr>
        <tr class="green"><td><code>function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}</code></td><td>1197</td><td><code>Object.assign</code> methods?</td></tr>
        <tr class="green"><td><code>function n(){return Object.assign,n.apply(this,arguments)}</code></td><td>1008</td><td><code>Object.assign</code> properties?</td></tr>
        <tr class="green"><td><code>function(){}</code></td><td>753</td><td>no-op</td></tr>
        <tr><td><code>function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u&lt;f.length;u++)r=f[u],0&lt;=e.indexOf(r)||(t[r]=n[r]);return t}</code></td><td>191</td><td>?</td></tr>
        <tr><td><code>function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o&lt;l.length;o++)t=l[o],0&lt;=r.indexOf(t)||(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o&lt;l.length;o++)t=l[o],0&lt;=r.indexOf(t)||Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}</code></td><td>187</td><td>?</td></tr>
        <tr><td><code>function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}</code></td><td>159</td><td>?</td></tr>
        <tr><td><code>function(r,t){if("object"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t||"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")}</code></td><td>113</td><td>?</td></tr>
        <tr><td><code>function(r,e,t){return i=function(r,e){if("object"!=typeof r||null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if("object"!=typeof t)return t;throw new TypeError("@@toPrimitive must return a primitive value.")}(e,"string"),(e="symbol"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}</code></td><td>111</td><td>?</td></tr>
        <tr><td><code>function(t){t=function(t,r){if("object"!=typeof t||null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if("object"!=typeof i)return i;throw new TypeError("@@toPrimitive must return a primitive value.")}(t,"string");return"symbol"==typeof t?t:String(t)}</code></td><td>111</td><td>?</td></tr>
        <tr><td><code>function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}</code></td><td>104</td><td>?</td></tr>
        <tr class="green"><td><code>function(n,r){(null==r||r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e&lt;r;e++)l[e]=n[e];return l}</code></td><td>93</td><td>array spread?</td></tr>
        <tr class="green"><td><code>function(){return!0}</code></td><td>92</td><td>always-true</td></tr>
        <tr class="green"><td><code>function(){return!1}</code></td><td>78</td><td>always-false</td></tr>
        <tr class="green"><td><code>function(r){if(Array.isArray(r))return r}</code></td><td>77</td><td>self explanatory</td></tr>
        <tr class="green"><td><code>function(){throw new TypeError('Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.')}</code></td><td>77</td><td><code>isSymbol</code>?</td></tr>
        <tr class="green"><td><code>function(t){return t&&"object"==typeof t&&"default"in t?t:{default:t}}</code></td><td>76</td><td>?</td></tr>
        <tr><td><code>function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}</code></td><td>49</td><td>?</td></tr>
        <tr><td><code>function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(c,e),Object.defineProperty(y,e,t.get?t:{enumerable:!0,get:function(){return c[e]}}))}</code></td><td>49</td><td>?</td></tr>
        <tr><td><code>function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]||l["@@iterator"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r||e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a||null==t.return||t.return()}finally{if(o)throw u}}return e}}</code></td><td>44</td><td>?</td></tr>
        <tr><td><code>function(r){var n;return r&&"object"==typeof r&&"default"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}</code></td><td>39</td><td>?</td></tr>
        <tr><td><code>function n(r){return n(r)}</code></td><td>38</td><td><code>Function.apply</code>?</td></tr>
        <tr><td><code>function(n){return typeof n}</code></td><td>38</td><td><code>typeof</code></td></tr>
        <tr><td><code>function(o){return o&&"function"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o}</code></td><td>38</td><td>?</td></tr>
        <tr><td><code>function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}</code></td><td>37</td><td><code>import</code>?</td></tr>
        <tr><td><code>function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]||l["@@iterator"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}</code></td><td>33</td><td>?</td></tr>
        <tr class="green"><td><code>function(n){}</code></td><td>29</td><td>no-op</td></tr>
        <tr><td><code>function(r){if(typeof Symbol<"u"&&null!=r[Symbol.iterator]||null!=r["@@iterator"])return Array.from(r)}</code></td><td>28</td><td>?</td></tr>
        <tr><td><code>function(){throw new TypeError('Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.')}</code></td><td>28</td><td>?</td></tr>
        <tr class="green"><td><code>()=>{}</code></td><td>27</td><td>no-op</td></tr>
        <tr class="green"><td><code>function(){return null}</code></td><td>27</td><td>always-null</td></tr>
        <tr class="green"><td><code>function(){return this}</code></td><td>20</td><td>always-this</td></tr>
        <tr class="green"><td><code>function(){return 0}</code></td><td>17</td><td>always-zero</td></tr>
        <tr class="green"><td><code>function(n){return n}</code></td><td>15</td><td>identity</td></tr>
        <tr class="green"><td><code>function(n,r){return n}</code></td><td>14</td><td>always-first-argument</td></tr>
        <tr><td><code>d=>d.id</code></td><td>14</td><td><code>.id</code></td></tr>
        <tr><td><code>function(){var r=function(t,o){return(r=Object.setPrototypeOf||({__proto__:[]}instanceof Array?function(t,o){t.__proto__=o}:function(t,o){for(var n in o)Object.prototype.hasOwnProperty.call(o,n)&&(t[n]=o[n])}))(t,o)};return function(t,o){if("function"!=typeof o&&null!==o)throw new TypeError("Class extends value "+String(o)+" is not a constructor or null");function n(){this.constructor=t}r(t,o),t.prototype=null===o?Object.create(o):(n.prototype=o.prototype,new n)}}</code></td><td>14</td><td>?</td></tr>
        <tr class="green"><td><code>function(_,o){_.__proto__=o}</code></td><td>14</td><td><code>Object.is_a</code>?</td></tr>
        <tr class="green"><td><code>function(o,r){for(var t in r)Object.prototype.hasOwnProperty.call(r,t)&&(o[t]=r[t])}</code></td><td>14</td><td>?</td></tr>
        <tr class="green"><td><code>function(n){return!1}</code></td><td>13</td><td>always-false</td></tr>
        <tr class="green"><td><code>a=>a</code></td><td>12</td><td>identity</td></tr>
        <tr class="green"><td><code>a=>a()</code></td><td>12</td><td>call first argument</td></tr>
        <tr><td><code>function(n,a){n.a=a}</code></td><td>11</td><td>enum/const definition?</td></tr>
        <tr class="green"><td><code>function(n,r){return r}</code></td><td>11</td><td>always-second-argument</td></tr>
        <tr class="green"><td><code>function(e){return e&&e.__esModule?e:{default:e}}</code></td><td>11</td><td>import default</td></tr>
        <tr><td><code>function(c){this.c=c}</code></td><td>10</td><td>enum/const definition?</td></tr>
    </tbody>
</table>
<div class="center">
    <button class="expand-table-toggle" data-table-id="vite-2">Show more</button>
</div>

As a matter of fact, someone on the internet did a very similar research few years ago.
So I hoped to see the improvement in the build tools over the years.

As I mentioned above, our front-end is bundled with Vite. Let's see if using esbuild or bun
(since both are fairly new and stand out in terms of architecture and performance) do a better job.

With few small adjustments to make things fair (e.g. build the same thing in the same way),
like disabling the plugins for Vite, setting up svgr loader,
here are some build time stats:

`yarn install`:

```
➤ YN0000: Done with warnings in 17s 798ms
yarn  12.11s user 19.69s system 175% cpu 18.122 total
```

`bun install`:

```
warn: esbuild's postinstall script took 748.9ms

 1028 packages installed [1.82s]
  Removed: 2
bun install  0.22s user 0.65s system 47% cpu 1.849 total
```

<table>
    <thread>
        <tr>
            <td>Bundler</td>
            <td>Build time</td>
        </tr>
    </thread>
    <tbody>
        <tr>
            <td>bun</td>
            <td>0.43s</td>
        </tr>
        <tr>
            <td>esbuild</td>
            <td>2.57s</td>
        </tr>
        <tr>
            <td>vite</td>
            <td>85.04s</td>
        </tr>
        <tr>
            <td>webpack</td>
            <td>138.64s</td>
        </tr>
    </tbody>
</table>

And the analysis of the built bundles:

`vite`:

```
Found 968 functions, 651 are unique (67.25%)
Found 598 functions, 267 are unique (44.65%)
Found 154 functions, 98 are unique (63.64%)
Found 17 functions, 10 are unique (58.82%)
Found 15192 functions, 8963 are unique (59%)
Found 1202 functions, 494 are unique (41.1%)
Found 513 functions, 231 are unique (45.03%)
= Total 18644 functions, 10714 are unique (57.4%)

Duplicates length: 52616 bytes out of 281406 bytes are duplicate code (18.7%)
Duplicates length: 57607 bytes out of 164737 bytes are duplicate code (34.97%)
Duplicates length: 11140 bytes out of 45135 bytes are duplicate code (24.68%)
Duplicates length: 1932 bytes out of 6532 bytes are duplicate code (29.58%)
Duplicates length: 1518418 bytes out of 3537579 bytes are duplicate code (42.92%)
Duplicates length: 130649 bytes out of 340227 bytes are duplicate code (38.4%)
Duplicates length: 50160 bytes out of 136057 bytes are duplicate code (36.87%)
= Total 1822522 out of 4511673 bytes are duplicate code (40.3%)
```

`esbuild`:

```
Found 46654 functions, 28952 are unique (62.06%)
Duplicates length: 6905599 bytes out of 9645594 bytes are duplicate code (71.59%)
```

`bun`:

```
Found 31113 functions, 25755 are unique (82.78%)
Duplicates length: 446020 bytes out of 5696964 bytes are duplicate code (7.83%)
```

`webpack`:

```
Found 2898 functions, 1434 are unique (49.48%)
Duplicates length: 320940 bytes out of 4645589 bytes are duplicate code (6.91%)
```

And a deeper analysis of the duplicated functions:

`esbuild`:

<table id="esbuild-1" class="expandable">
    <thead>
        <tr>
            <th>Function</th>
            <th>Occurrences</th>
        </tr>
    </thead>
    <tbody>
        <tr class="green"><td><code>function(r){for(var t=1;t&lt;arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}</code></td><td>2216</td></tr>
        <tr class="green"><td><code>function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}</code></td><td>1204</td></tr>
        <tr class="green"><td><code>function n(){return Object.assign,n.apply(this,arguments)}</code></td><td>1010</td></tr>
        <tr class="green"><td><code>function(){}</code></td><td>844</td></tr>
        <tr class="green"><td><code>function(t){return t&&"object"==typeof t&&"default"in t?t:{default:t}}</code></td><td>260</td></tr>
        <tr><td><code>function(i){this.a=i}</code></td><td>250</td></tr>
        <tr><td><code>function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u&lt;f.length;u++)r=f[u],0&lt;=e.indexOf(r)||(t[r]=n[r]);return t}</code></td><td>203</td></tr>
        <tr><td><code>function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o&lt;l.length;o++)t=l[o],0&lt;=r.indexOf(t)||(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o&lt;l.length;o++)t=l[o],0&lt;=r.indexOf(t)||Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}</code></td><td>194</td></tr>
        <tr class="green"><td><code>function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}</code></td><td>160</td></tr>
        <tr class="green"><td><code>function(r,t){if("object"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t||"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")}</code></td><td>137</td></tr>
        <tr><td><code>function(r,e,t){return i=function(r,e){if("object"!=typeof r||null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if("object"!=typeof t)return t;throw new TypeError("@@toPrimitive must return a primitive value.")}(e,"string"),(e="symbol"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}</code></td><td>134</td></tr>
        <tr><td><code>function(t){t=function(t,r){if("object"!=typeof t||null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if("object"!=typeof i)return i;throw new TypeError("@@toPrimitive must return a primitive value.")}(t,"string");return"symbol"==typeof t?t:String(t)}</code></td><td>134</td></tr>
        <tr class="green"><td><code>()=>{}</code></td><td>129</td></tr>
        <tr class="green"><td><code>function(n){return this===n}</code></td><td>119</td></tr>
        <tr class="green"><td><code>function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}</code></td><td>115</td></tr>
        <tr class="green"><td><code>function(n,r){(null==r||r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e&lt;r;e++)l[e]=n[e];return l}</code></td><td>106</td></tr>
        <tr><td><code>function(c,i){Bs.call(this,c,i)}</code></td><td>94</td></tr>
        <tr class="green"><td><code>function(){return!0}</code></td><td>93</td></tr>
        <tr class="green"><td><code>function(r){if(Array.isArray(r))return r}</code></td><td>83</td></tr>
        <tr class="green"><td><code>function(){throw new TypeError(`Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}</code></td><td>83</td></tr>
        <tr class="green"><td><code>function(){return!1}</code></td><td>79</td></tr>
        <tr><td><code>function(){return new cu(this)}</code></td><td>77</td></tr>
        <tr><td><code>function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(b,e),Object.defineProperty(E,e,t.get?t:{enumerable:!0,get:function(){return b[e]}}))}</code></td><td>76</td></tr>
        <tr><td><code>function(){return b[P]}</code></td><td>76</td></tr>
        <tr><td><code>function(a,e,l){var i,r=l["aria-label"],t=l["aria-labelledby"],n=l.title;switch(a){case"img":return r||t||n?(s(i={},"aria-labelledby",t),s(i,"aria-label",r),s(i,"title",n),i):{"aria-label":"".concat(e.replace(/([a-z])([A-Z])/g,"$1 $2")," Icon")};case"presentation":return{"aria-hidden":!0,alt:""}}}</code></td><td>76</td></tr>
        <tr><td><code>function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}</code></td><td>67</td></tr>
        <tr class="green"><td><code>function(n){return typeof n}</code></td><td>64</td></tr>
        <tr class="green"><td><code>function(o){return o&&"function"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o}</code></td><td>64</td></tr>
        <tr><td><code>function n(r){return n(r)}</code></td><td>63</td></tr>
        <tr><td><code>function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}</code></td><td>61</td></tr>
        <tr><td><code>function(i,t){this.a=i,this.b=t}</code></td><td>58</td></tr>
        <tr><td><code>function(r){var n;return r&&"object"==typeof r&&"default"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}</code></td><td>50</td></tr>
        <tr><td><code>function(r){if(typeof Symbol<"u"&&null!=r[Symbol.iterator]||null!=r["@@iterator"])return Array.from(r)}</code></td><td>49</td></tr>
        <tr><td><code>function(){throw new TypeError(`Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}</code></td><td>49</td></tr>
        <tr><td><code>function(){return this.a}</code></td><td>49</td></tr>
        <tr><td><code>function(i){p0(this,i)}</code></td><td>48</td></tr>
        <tr><td><code>function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]||l["@@iterator"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r||e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a||null==t.return||t.return()}finally{if(o)throw u}}return e}}</code></td><td>46</td></tr>
        <tr><td><code>function(){throw St(new Ss)}</code></td><td>46</td></tr>
        <tr><td><code>function(n){throw St(new Ss)}</code></td><td>39</td></tr>
        <tr><td><code>()=>{"use strict";Vu(),Du()}</code></td><td>38</td></tr>
        <tr><td><code>function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]||l["@@iterator"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}</code></td><td>37</td></tr>
        <tr><td><code>function(){return this.b}</code></td><td>33</td></tr>
        <tr><td><code>function(){X(x)}</code></td><td>32</td></tr>
        <tr class="green"><td><code>function(n){}</code></td><td>31</td></tr>
        <tr><td><code>a=>a()</code></td><td>30</td></tr>
        <tr><td><code>function(n){return V1(n)}</code></td><td>30</td></tr>
        <tr><td><code>function(n){return dn(oi,mr,2,n,6,1)}</code></td><td>30</td></tr>
        <tr class="green"><td><code>function(){return null}</code></td><td>29</td></tr>
        <tr class="green"><td><code>function(){return this}</code></td><td>24</td></tr>
        <tr><td><code>()=>{"use strict";Du()}</code></td><td>23</td></tr>
        <tr><td><code>function(i){g0(this,i)}</code></td><td>23</td></tr>
        <tr><td><code>function(t,r){var e;if(t)return"string"==typeof t?k(t,r):"Map"===(e="Object"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)||"Set"===e?Array.from(t):"Arguments"===e||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(e)?k(t,r):void 0}</code></td><td>22</td></tr>
        <tr><td><code>function(t,r){var e;if(t)return"string"==typeof t?P(t,r):"Map"===(e="Object"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)||"Set"===e?Array.from(t):"Arguments"===e||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(e)?P(t,r):void 0}</code></td><td>22</td></tr>
        <tr class="green"><td><code>function(n){return null!=n&&n instanceof Array}</code></td><td>21</td></tr>
        <tr><td><code>function(r){if(Array.isArray(r))return P(r)}</code></td><td>21</td></tr>
        <tr class="green"><td><code>function(e){return e&&e.__esModule?e:{default:e}}</code></td><td>21</td></tr>
        <tr><td><code>function(t,r){var e;if(t)return"string"==typeof t?Q(t,r):"Map"===(e="Object"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)||"Set"===e?Array.from(t):"Arguments"===e||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(e)?Q(t,r):void 0}</code></td><td>21</td></tr>
        <tr><td><code>function(){throw St(new Os(il((qs(),_g))))}</code></td><td>21</td></tr>
        <tr class="green"><td><code>function(n){return n}</code></td><td>20</td></tr>
        <tr class="green"><td><code>function(n){return null!=n&&n.nodeType===Node.ELEMENT_NODE}</code></td><td>20</td></tr>
        <tr class="green"><td><code>function(e){throw Error("Received unhandled value: ".concat(e))}</code></td><td>20</td></tr>
        <tr class="green"><td><code>function(r,n){return Array.isArray(r)?r.concat(n):"string"==typeof r?r:void 0}</code></td><td>19</td></tr>
    </tbody>
</table>
<div class="center">
    <button class="expand-table-toggle" data-table-id="esbuild-1">Show more</button>
</div>

`bun`:

<table id="bun-1" class="expandable">
    <thead>
        <tr>
            <th>Function</th>
            <th>Occurrences</th>
        </tr>
    </thead>
    <tbody>
        <tr class="green"><td><code>function(){}</code></td><td>739</td></tr>
        <tr><td><code>function(i){this.a=i}</code></td><td>250</td></tr>
        <tr class="green"><td><code>function(r){for(var t=1;t&lt;arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}</code></td><td>197</td></tr>
        <tr class="green"><td><code>()=>{}</code></td><td>141</td></tr>
        <tr class="green"><td><code>function(n){return this===n}</code></td><td>119</td></tr>
        <tr><td><code>function(c,f){f7.call(this,c,f)}</code></td><td>94</td></tr>
        <tr class="green"><td><code>function(){return!0}</code></td><td>91</td></tr>
        <tr><td><code>function(){return new p9(this)}</code></td><td>77</td></tr>
        <tr class="green"><td><code>function(){return!1}</code></td><td>76</td></tr>
        <tr><td><code>function(i,t){this.a=i,this.b=t}</code></td><td>58</td></tr>
        <tr class="green"><td><code>function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u&lt;f.length;u++)r=f[u],0&lt;=e.indexOf(r)||(t[r]=n[r]);return t}</code></td><td>53</td></tr>
        <tr class="green"><td><code>function(r,t){if("object"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t||"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")}</code></td><td>51</td></tr>
        <tr><td><code>function(){return this.a}</code></td><td>49</td></tr>
        <tr class="green"><td><code>function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o&lt;l.length;o++)t=l[o],0&lt;=r.indexOf(t)||(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o&lt;l.length;o++)t=l[o],0&lt;=r.indexOf(t)||Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}</code></td><td>48</td></tr>
        <tr class="green"><td><code>function(r,e,t){return i=function(r,e){if("object"!=typeof r||null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if("object"!=typeof t)return t;throw new TypeError("@@toPrimitive must return a primitive value.")}(e,"string"),(e="symbol"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}</code></td><td>48</td></tr>
        <tr class="green"><td><code>function(t){t=function(t,r){if("object"!=typeof t||null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if("object"!=typeof i)return i;throw new TypeError("@@toPrimitive must return a primitive value.")}(t,"string");return"symbol"==typeof t?t:String(t)}</code></td><td>48</td></tr>
        <tr><td><code>function(i){T6(this,i)}</code></td><td>48</td></tr>
        <tr><td><code>()=>{R3(),G3()}</code></td><td>46</td></tr>
        <tr><td><code>function(){throw x0(new w7)}</code></td><td>46</td></tr>
        <tr><td><code>function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}</code></td><td>45</td></tr>
        <tr><td><code>function(n,r){(null==r||r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e&lt;r;e++)l[e]=n[e];return l}</code></td><td>41</td></tr>
        <tr><td><code>function(n){throw x0(new w7)}</code></td><td>39</td></tr>
        <tr class="green"><td><code>function(r){if(Array.isArray(r))return r}</code></td><td>36</td></tr>
        <tr class="green"><td><code>function(){throw new TypeError("Invalid attempt to destructure non-iterable instance.\\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}</code></td><td>36</td></tr>
        <tr><td><code>function(){return this.b}</code></td><td>33</td></tr>
        <tr><td><code>function(n){return n2(n)}</code></td><td>30</td></tr>
        <tr><td><code>function(n){return J1($5,p1,2,n,6,1)}</code></td><td>30</td></tr>
        <tr><td><code>function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}</code></td><td>29</td></tr>
        <tr><td><code>function(l,r){var t=null==l?null:"undefined"!=typeof Symbol&&l[Symbol.iterator]||l["@@iterator"];if(null!=t){var e,n,u,f,i=[],a=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;a=!1}else for(;!(a=(e=u.call(t)).done)&&(i.push(e.value),i.length!==r);a=!0);}catch(l){o=!0,n=l}finally{try{if(!a&&null!=t.return&&(f=t.return(),Object(f)!==f))return}finally{if(o)throw n}}return i}}</code></td><td>29</td></tr>
        <tr class="green"><td><code>function(n){}</code></td><td>29</td></tr>
        <tr class="green"><td><code>function(){return null}</code></td><td>28</td></tr>
        <tr><td><code>function(e){return Object.getOwnPropertyDescriptor(Z,e).enumerable}</code></td><td>25</td></tr>
        <tr><td><code>function(e){Object.defineProperty(Z,e,Object.getOwnPropertyDescriptor(W,e))}</code></td><td>25</td></tr>
        <tr class="green"><td><code>function(n){return n}</code></td><td>23</td></tr>
        <tr><td><code>function(i){C6(this,i)}</code></td><td>23</td></tr>
        <tr><td><code>()=>{G3()}</code></td><td>22</td></tr>
        <tr><td><code>function(){throw x0(new C7(n7((y7(),KG))))}</code></td><td>21</td></tr>
        <tr class="green"><td><code>function(){return this}</code></td><td>19</td></tr>
        <tr><td><code>function(i,t){this.b=i,this.a=t}</code></td><td>19</td></tr>
        <tr class="green"><td><code>function(n){return!1}</code></td><td>18</td></tr>
        <tr><td><code>function(n,w){throw x0(new w7)}</code></td><td>18</td></tr>
        <tr><td><code>function(){return 0}</code></td><td>17</td></tr>
        <tr class="green"><td><code>function(n){return typeof n}</code></td><td>17</td></tr>
        <tr><td><code>function(o){return o&&"function"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o}</code></td><td>17</td></tr>
        <tr><td><code>()=>{R3()}</code></td><td>16</td></tr>
        <tr><td><code>()=>{var e;return null!=(e=Z.options.debugAll)?e:Z.options.debugHeaders}</code></td><td>16</td></tr>
        <tr class="green"><td><code>function(n,r){return n}</code></td><td>15</td></tr>
        <tr><td><code>function(c){hX.call(this,c)}</code></td><td>15</td></tr>
        <tr><td><code>function(){return this.c}</code></td><td>15</td></tr>
        <tr><td><code>function(){return this.d}</code></td><td>15</td></tr>
        <tr><td><code>function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}</code></td><td>14</td></tr>
        <tr class="green"><td><code>function(r){if("undefined"!=typeof Symbol&&null!=r[Symbol.iterator]||null!=r["@@iterator"])return Array.from(r)}</code></td><td>14</td></tr>
        <tr><td><code>function(){throw new TypeError("Invalid attempt to spread non-iterable instance.\\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}</code></td><td>14</td></tr>
        <tr><td><code>d=>d.id</code></td><td>14</td></tr>
        <tr><td><code>()=>W(!1)</code></td><td>13</td></tr>
        <tr class="green"><td><code>a=>a</code></td><td>12</td></tr>
        <tr><td><code>function(t){var l=-1,n=null==t?0:t.length;for(this.clear();++l&lt;n;){var r=t[l];this.set(r[0],r[1])}}</code></td><td>12</td></tr>
        <tr class="green"><td><code>function(n,c){}</code></td><td>12</td></tr>
        <tr class="green"><td><code>function(n,r){return r}</code></td><td>11</td></tr>
        <tr class="green"><td><code>function(n,a){n.a=a}</code></td><td>11</td></tr>
        <tr><td><code>function(){_V.call(this)}</code></td><td>11</td></tr>
        <tr><td><code>function(i,t,h){this.a=i,this.b=t,this.c=h}</code></td><td>11</td></tr>
        <tr><td><code>function(){return this.a.gc()}</code></td><td>11</td></tr>
        <tr><td><code>function(i){this.b=i}</code></td><td>10</td></tr>
        <tr><td><code>function(c){this.c=c}</code></td><td>10</td></tr>
        <tr><td><code>function(){return this.f}</code></td><td>10</td></tr>
    </tbody>
</table>
<div class="center">
    <button class="expand-table-toggle" data-table-id="bun-1">Show more</button>
</div>

Interestingly enough, all three tools handled the job bad in different aspects:

* vite was the slowest and produced second biggest bundle
* esbuild was the fastest and produced the largest bundle
* bun was slower than esbuild by a split of hair, but produced smallest bundle with least duplicates

Bonus points to bun for installing node modules in a link of an eye.

In terms of duplicates, however, all three failed miserably (in my opinion), with the best result being the bundle
produced by bun with 18% duplicates and the rest having almost half the bundle wasted.

For the most part, bundlers seem to be doing a pretty bad job at tree shaking and keep _a lot_ of those utility functions' duplicates.
One can estimate how much of a wasted space these use, by multiplying the function code length by the number of duplicates minus one (for one definition).

Let's imagine some of the above functions could be de-duplicated. What are the benefit of that?
For the most part, the front-end can load faster for users - simply because there is less bytes to transfer.
On top of that, there are less functions to be created in memory. So _technically_, the front-end can _act_ faster.
Although, on the modern machines the difference between having one function and few thousand of the same function is negligible.

Here is a shortened list of top abusers from different bundlers for our tool:

<table id="deduplicatable-1" class="expandable">
    <thead>
        <tr>
            <th>Function</th>
            <th colspan="2">vite</th>
            <th colspan="2">esbuild</th>
            <th colspan="2">bun</th>
        </tr>
        <tr>
            <th></th>
            <th>#</th>
            <th>Bytes</th>
            <th>#</th>
            <th>Bytes</th>
            <th>#</th>
            <th>Bytes</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td><code>function(){}</code></td>
            <td>753</td>
            <td>9036</td>
            <td>844</td>
            <td>10128</td>
            <td>739</td>
            <td>8868</td>
        </tr>
        <tr>
            <td>
                <code>function(){return!0}</code>
            </td>
            <td>92</td>
            <td>1840</td>
            <td>93</td>
            <td>1860</td>
            <td>91</td>
            <td>1820</td>
        </tr>
        <tr>
            <td>
                <code>function(){return!1}</code>
            </td>
            <td>78</td>
            <td>1560</td>
            <td>79</td>
            <td>1580</td>
            <td>76</td>
            <td>1520</td>
        </tr>
        <tr>
            <td>
                <code>function(){return null}</code>
            </td>
            <td>27</td>
            <td>621</td>
            <td>29</td>
            <td>667</td>
            <td>28</td>
            <td>644</td>
        </tr>
        <tr>
            <td>
                <code>function(){return this}</code>
            </td>
            <td>20</td>
            <td>460</td>
            <td>24</td>
            <td>552</td>
            <td>19</td>
            <td>437</td>
        </tr>
        <tr>
            <td>
                <code>function(){return 0}</code>
            </td>
            <td>17</td>
            <td>340</td>
            <td>n/a</td>
            <td>0</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(n){return!1}</code>
            </td>
            <td>13</td>
            <td>273</td>
            <td>n/a</td>
            <td>0</td>
            <td>18</td>
            <td>378</td>
        </tr>
        <tr>
            <td><code>function(n){}</code></td>
            <td>29</td>
            <td>377</td>
            <td>31</td>
            <td>403</td>
            <td>29</td>
            <td>377</td>
        </tr>
        <tr>
            <td>
                <code>function(n){return n}</code>
            </td>
            <td>15</td>
            <td>315</td>
            <td>20</td>
            <td>420</td>
            <td>23</td>
            <td>483</td>
        </tr>
        <tr>
            <td>
                <code>function(n){return typeof n}</code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>64</td>
            <td>1792</td>
            <td>17</td>
            <td>476</td>
        </tr>
        <tr>
            <td>
                <code>function(n){return this===n}</code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>119</td>
            <td>3332</td>
            <td>119</td>
            <td>3332</td>
        </tr>
        <tr>
            <td>
                <code>function(n,r){return n}</code>
            </td>
            <td>14</td>
            <td>322</td>
            <td>n/a</td>
            <td>0</td>
            <td>15</td>
            <td>345</td>
        </tr>
        <tr>
            <td>
                <code>function(n,r){return r}</code>
            </td>
            <td>11</td>
            <td>253</td>
            <td>n/a</td>
            <td>0</td>
            <td>11</td>
            <td>253</td>
        </tr>
        <tr>
            <td><code>function(n,c){}</code></td>
            <td>n/a</td>
            <td>0</td>
            <td>n/a</td>
            <td>0</td>
            <td>12</td>
            <td>180</td>
        </tr>
        <tr>
            <td><code>()=&gt;{}</code></td>
            <td>27</td>
            <td>162</td>
            <td>129</td>
            <td>774</td>
            <td>141</td>
            <td>846</td>
        </tr>
        <tr>
            <td><code>a=&gt;a</code></td>
            <td>12</td>
            <td>48</td>
            <td>n/a</td>
            <td>0</td>
            <td>12</td>
            <td>48</td>
        </tr>
        <tr>
            <td><code>a=&gt;a()</code></td>
            <td>12</td>
            <td>72</td>
            <td>n/a</td>
            <td>0</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(r){for(var t=1;t&lt;arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&amp;&amp;(r[n]=o[n])}return r}</code>
            </td>
            <td>2205</td>
            <td>317520</td>
            <td>2216</td>
            <td>319104</td>
            <td>197</td>
            <td>28368</td>
        </tr>
        <tr>
            <td>
                <code>function n(){return Object.assign&amp;&amp;Object.assign.bind(),n.apply(this,arguments)}</code>
            </td>
            <td>1197</td>
            <td>95760</td>
            <td>1204</td>
            <td>96320</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function n(){return Object.assign,n.apply(this,arguments)}</code>
            </td>
            <td>1008</td>
            <td>58464</td>
            <td>1010</td>
            <td>58580</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code> function(n,r){(null==r||r&gt;n.length)&amp;&amp;(r=n.length);for(var e=0,l=new Array(r);e&lt;r;e++)l[e]=n[e];return l}</code>
            </td>
            <td>93</td>
            <td>9672</td>
            <td>106</td>
            <td>11024</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(r){if(Array.isArray(r))return r}</code>
            </td>
            <td>77</td>
            <td>3157</td>
            <td>83</td>
            <td>3403</td>
            <td>36</td>
            <td>1476</td>
        </tr>
        <tr>
            <td>
                <code>function(){throw new TypeError(&#39;Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.&#39;)}</code>
            </td>
            <td>77</td>
            <td>13244</td>
            <td>83</td>
            <td>14276</td>
            <td>36</td>
            <td>6192</td>
        </tr>
        <tr>
            <td>
                <code>function(t){return t&amp;&amp;"object"==typeof t&amp;&amp;"default"in t?t:{default:t}}</code>
            </td>
            <td>76</td>
            <td>5624</td>
            <td>260</td>
            <td>19240</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(_,o){_.__proto__=o}</code>
            </td>
            <td>14</td>
            <td>392</td>
            <td>n/a</td>
            <td>0</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(o,r){for(var t in r)Object.prototype.hasOwnProperty.call(r,t)&amp;&amp;(o[t]=r[t])}</code>
            </td>
            <td>14</td>
            <td>1176</td>
            <td>n/a</td>
            <td>0</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(e){return e&amp;&amp;e.__esModule?e:{default:e}}</code>
            </td>
            <td>11</td>
            <td>539</td>
            <td>21</td>
            <td>1029</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}</code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>115</td>
            <td>13570</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(r,n){return Array.isArray(r)?r.concat(n):"string"==typeof r?r:void 0}</code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>19</td>
            <td>1520</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(n){return null!=n&amp;&amp;n instanceof Array}</code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>21</td>
            <td>987</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u&lt;f.length;u++)r=f[u],0&lt;=e.indexOf(r)||(t[r]=n[r]);return t} </code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>n/a</td>
            <td>0</td>
            <td>53</td>
            <td>6890</td>
        </tr>
        <tr>
            <td>
                <code> function(r,t){if("object"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t||"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")} </code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>137</td>
            <td>36853</td>
            <td>51</td>
            <td>13719</td>
        </tr>
        <tr>
            <td>
                <code>function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}</code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>160</td>
            <td>17600</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(o){return o&amp;&amp;"function"==typeof Symbol&amp;&amp;o.constructor===Symbol&amp;&amp;o!==Symbol.prototype?"symbol":typeof o}</code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>64</td>
            <td>7424</td>
            <td>n/a</td>
            <td>0</td>
        </tr>
        <tr>
            <td>
                <code>function(r){if("undefined"!=typeof Symbol&amp;&amp;null!=r[Symbol.iterator]||null!=r["@@iterator"])return Array.from(r)}</code>
            </td>
            <td>n/a</td>
            <td>0</td>
            <td>n/a</td>
            <td>0</td>
            <td>14</td>
            <td>1624</td>
        </tr>
    </tbody>
</table>
<div class="center">
    <button class="expand-table-toggle" data-table-id="deduplicatable-1">Show more</button>
</div>

Interestingly enough, aside from a lot of `() => {}` and `(a, b) => a` and `() => true` (as I call them, utility) functions,
there are a lot of ES6 / TypeScript helpers such as class definition and spread operator variants, presumingly made to be compatible with ES5-only browsers.
Maybe if we had targeted only platforms supporting the latest ES features we would get better results?

Well, not quite much:

bundle sizes:

<table>
    <thead>
        <tr>
            <th>Bundler</th>
            <th>Bundle size</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>bun</td>
            <td>5.4M</td>
        </tr>
        <tr>
            <td>esbuild</td>
            <td>9.2M</td>
        </tr>
        <tr>
            <td>esbuild (tuned)</td>
            <td>8.0M</td>
        </tr>
        <tr>
            <td>vite</td>
            <td>7.1M</td>
        </tr>
        <tr>
            <td>vite (tuned)</td>
            <td>3.8M</td>
        </tr>
        <tr>
            <td>webpack</td>
            <td>4.4M</td>
        </tr>
    </tbody>
</table>

`vite`:

```
Found 15983 functions, 9581 are unique (59.94%)
Duplicates length: 1495985 bytes out of 4019326 bytes are duplicate code (37.22%)
```

`esbuild`:

```
Found 41736 functions, 29224 are unique (70.02%)
Duplicates length: 3406606 bytes out of 8347230 bytes are duplicate code (40.81%)
```

webpack is (should be) already using the `target` config option from `tsconfig.json` (which is set to `ESNext` in our case)
and bun does not really have a whole lot of customization in this regard.

One unlikely possibility when these duplicates can be faster than having just one function is that running the nearly-defined code
(in terms of a single block of code) might be slightly faster than making code jumps.
This is super far-fetched idea from low-level programming, when CPU does not have to jump thousands or millions of (assembly) instructions back and forth
but literally a few instead.
This won't justify using verbose ES5-compatible code on ESnext browser, however.
How about we run a very synthetic benchmark to check just this one theory?

```js
function f(){return!1}

console.time('1');
for(var i=0;i<100000;i++){var a=[];for(var t=0;t<20000;t++)a.push(Math.random()*1000000);var x=a.filter(f).length}
console.timeEnd('1');
```

```js
console.time('2');
for(var i=0;i<100000;i++){var a=[];for(var t=0;t<20000;t++)a.push(Math.random()*1000000);var f=function(){return!1},x=a.filter(f).length}
console.timeEnd('2');
```

The results are actually quite stable:

```
1: 17029ms
1: 16998ms
1: 16903ms

2: 21877ms
2: 21811ms
2: 21821ms
```

Having just one function instance is approx. `23%` faster. But what happens at consequitive runs?

```
1: 9194ms
1: 9159ms
1: 14044ms
1: 13882ms
1: 13975ms
1: 9205ms
1: 14026ms

2: 21821ms
2: 13843ms
2: 13866ms
2: 13854ms
2: 13961ms

2: 21718ms
2: 13952ms
2: 13925ms
2: 13923ms
```

Seems like CPU does indeed do a little bit of instruction caching and branch prediction (first run is visibly slower than the subsequent runs).
But the observation still holds: having one function definition instead of many copies (even "near" copies) has a much bigger impact.

With that being said, there is one interesting thing to try here: what if we actually replace some of those bulky duplicates with one-time declarations, within the same bundle?

Without performing an in-depth code analysis and optimization, I came up with the following naive implementation:

1. analyze the code and pick a few functions (biggest abusers) to fix them up
2. extract all function names, function parameter names (including potential destructuring objects in function params) and all variable and constant names
3. for each function to be de-duplicated, create a unique name (using the variable and function parameter and function names to avoid any clashes)
4. from the end of the file to the beginning, remove all occurrences of the function declaration and replace all function references with the unique name
5. add the function declarations to the beginning of the file

This approach is rather naive, since it does not account for a number of edge cases. For instance, if there are two functions to be replaced:

```js
function(e){for(var r=1;r<arguments.length;r++){var t,l=arguments[r];for(t in l)Object.prototype.hasOwnProperty.call(l,t)&&(e[t]=l[t])}return e}

function p(){return(p=Object.assign||function(e){for(var r=1;r<arguments.length;r++){var t,l=arguments[r];for(t in l)Object.prototype.hasOwnProperty.call(l,t)&&(e[t]=l[t])}return e}).apply(this,arguments)}
```

e.g. one includes the other, the algorithm could have potentially evicted cases like this.

Before proceeding further, it is a good idea to test if the cleaned up bundle can safely replace the original one.
Hence I just stuffed it in the static assets folder of our project and ran it with the modified bundle.

This way I figured few issues with the naive approach:

* two function definitions are causing stack overflow:
    * `$z=function n(){return Object.assign,n.apply(this,arguments)}`
    * `$q=function n(){return n=Object.assign&&Object.assign.bind(),n.apply(this,arguments)}`
* some of the empty functions are actually used as constructors (ES5-compatible OOP model) which is only discovered by finding the expressions like `$FnName.prototype.something = somethingElse;`
* some functions are named and then referenced later in the code
* some functions are not used at all: <img src="/images/how-unique-are-your-bundles/unused-deduplicated-functions.png" alt="Unused aliases">

For the shorthand functions I first tried manually fixing them up - had to replace them with `$z=function(){return $z=Object.assign.bind(),$z.apply(this,arguments)}` alikes. This worked, so I created an AST transformer to handle these one-line return-only functions, but it resulted in few extra whitespaces being added - using uglifyjs messes things up again and TypeScript compiler does not have an option for minimal output.

With the constructors I simply excluded them from being de-duplicated. This resulted in `155` fewer substitutions (in Vite mode), which is negligible on the overall scale of the problem.

As for the named functions which are in the global scope and are referenced later down the line, I had to create a list of "backwards-compatible aliases", mapping those old function names onto the new unique names.

Other than those, replacing the original bundle with the optimized one worked like a charm!

The results? With the threshold of `20` duplicates or more:

<table>
    <thead>
        <tr>
            <th rowspan="2">Bundler</th>
            <th colspan="5">Before optimization</th>
            <th colspan="5">After optimization</th>
        </tr>
        <tr>
            <th>Bundle size</th>
            <th>Total functions</th>
            <th>Unique functions</th>
            <th>Unique functions, %</th>
            <th>Duplicate code, %</th>
            <th>Bundle size</th>
            <th>Total functions</th>
            <th>Unique functions</th>
            <th>Unique functions, %</th>
            <th>Duplicate code, %</th>
        </tr>
    </thead>
    <tbody>
        <tr>
            <td>bun</td>
            <td>6.2M</td>
            <td>8903</td>
            <td>7443</td>
            <td>83.6%</td>
            <td>0.78%</td>
            <td>6.2M (no change)</td>
            <td>7865 (-1038)</td>
            <td>7355 (-88)</td>
            <td>93.52% (+9.92%)</td>
            <td>0.51% (-0.27%)</td>
        </tr>
        <tr>
            <td>esbuild</td>
            <td>8.7M</td>
            <td>13057</td>
            <td>10250</td>
            <td>78.5%</td>
            <td>3.9%</td>
            <td>8.5M (-0.2M)</td>
            <td>3265 (-9792)</td>
            <td>2990 (-7260)</td>
            <td>91.58% (+13.08%)</td>
            <td>0.62% (-3.28%)</td>
        </tr>
        <tr>
            <td>vite</td>
            <td>3.9M</td>
            <td>3502</td>
            <td>2365</td>
            <td>67.53%</td>
            <td>6.39%</td>
            <td>3.6M (-0.3M)</td>
            <td>2483 (-1019)</td>
            <td>2277 (-88)</td>
            <td>91.7% (+24.17%)</td>
            <td>1.68% (-4.71%)</td>
        </tr>
        <tr>
            <td>webpack</td>
            <td>4.4M</td>
            <td>2898</td>
            <td>1434</td>
            <td>49.48%</td>
            <td>6.91%</td>
            <td>4.1M (-0.3M)</td>
            <td>1484 (-1414)</td>
            <td>1375 (-59)</td>
            <td>92.65% (+43.17%)</td>
            <td>0.43% (-6.48%)</td>
        </tr>
    </tbody>
</table>

Conclusion? The bundlers do a pretty average job at optimizing the bundles, even in production mode with some extra tuning.
And if some brave soul is willing to invest even more time and effort than I did into developing a sophisticated solution
(potentially improving the existing tools, like uglifyjs or bundlers themselves), the numbers can be improved even further.
It would be really interesting to see what would the results be running this optimizer on a bigger bundle.

<style>
    .center {
        text-align: center;
    }
    table.expandable {
        display: block;
        max-height: 200px;
        overflow-y: hidden;
        border: none;
    }
    table.expandable.expand {
        display: table;
        overflow-y: visible;
    }
    table.expandable td, table.expandable th {
        padding: 0.5em;
    }
    table thead th {
        font-weight: bold;
    }
    /* table tr.red td {
        background: rgb(250, 88, 88);
    }
    table tr.green td {
        background: rgb(100, 232, 98);
    } */
</style>
<script>
[...document.querySelectorAll('.expand-table-toggle')].forEach(btn => {
    btn.onclick = () => {
        const tbl = document.querySelector(`table#${btn.dataset.tableId}`);
        tbl.classList.toggle('expand');
        if (tbl.classList.contains('expand')) {
            btn.innerText = 'Show less';
        } else {
            btn.innerText = 'Show more';
        }
    };
});
</script>
