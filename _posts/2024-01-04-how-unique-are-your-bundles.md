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

<img src="/images/how-unique-are-your-bundles/duplication1.webp" loading="lazy" alt="Repeated function definition in a single chunk of code">

By the way, this is a production build of our front-end, built using Vite, with minification enabled.

The raw length of this function code is `146` characters. So in a single place, in a single file, you have `136 * 146 = 19_992` bytes of waste.
Meaning, browser has to load these 20KB of code, parse it and create 136 duplicating functions.

Looking at the overall size of 3.5MB of code in this chunk and its insane 41% duplicated code (in sheer bytes, not occurrences, so 1.5MB wasted),
imagine how much faster this single file _could_ have been loaded in a browser.

I was keen on seeing what functions get duplicated most often and ran my script on an entire build output directory.
Here are top 80-ish offenders:

| Function code (minimized) | Copies |
| ------------------------- | ------ |
| `function(r){for(var t=1;t<arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}` | 2205 |
| `function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}` | 1197 |
| `function n(){return Object.assign,n.apply(this,arguments)}` | 1008 |
| `function(){}` | 753 |
| `function(i){this.a=i}` | 250 |
| ``function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)r=f[u],0<=e.indexOf(r)\|\|(t[r]=n[r]);return t}`` | 191 |
| `function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)\|\|(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)\|\|Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}` | 187 |
| `function(e,r){return r=r\|\|e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}` | 159 |
| `function(n){return this===n}` | 119 |
| `function(r,t){if("object"!=typeof r\|\|null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t\|\|"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")}` | 113 |
| `function(r,e,t){return i=function(r,e){if("object"!=typeof r\|\|null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if("object"!=typeof t)return t;throw new TypeError("@@toPrimitive must return a primitive value.")}(e,"string"),(e="symbol"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}` | 111 |
| `function(t){t=function(t,r){if("object"!=typeof t\|\|null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if("object"!=typeof i)return i;throw new TypeError("@@toPrimitive must return a primitive value.")}(t,"string");return"symbol"==typeof t?t:String(t)}` | 111 |
| `function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}` | 104 |
| `function(c,i){He.call(this,c,i)}` | 94 |
| `function(n,r){(null==r\|\|r&gt;n.length)&&(r=n.length);for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];return l}` | 93 |
| `function(){return!0}` | 92 |
| `function(){return!1}` | 78 |
| `function(){return new gt(this)}` | 77 |
| `function(r){if(Array.isArray(r))return r}` | 77 |
| ``function(){throw new TypeError(`Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}`` | 77 |
| `function(t){return t&&"object"==typeof t&&"default"in t?t:{default:t}}` | 76 |
| `function(i,t){this.a=i,this.b=t}` | 58 |
| `function(){return this.a}` | 49 |
| `function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}` | 49 |
| `function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(c,e),Object.defineProperty(y,e,t.get?t:{enumerable:!0,get:function(){return c[e]}}))}` | 49 |
| `function(){return c[b]}` | 49 |
| `function(a,e,i){var r,t=i["aria-label"],n=i["aria-labelledby"],c=i.title;switch(a){case"img":return t\|\|n\|\|c?(l(r={},"aria-labelledby",n),l(r,"aria-label",t),l(r,"title",c),r):{"aria-label":"".concat(e.replace(/([a-z])([A-Z])/g,"$1 $2")," Icon")};case"presentation":return{"aria-hidden":!0,alt:""}}}` | 49 |
| `function(i){Di(this,i)}` | 48 |
| `function(r){return Object.getOwnPropertyDescriptor(e,r).enumerable}` | 48 |
| `function(){throw M(new De)}` | 46 |
| `function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]\|\|l["@@iterator"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r\|\|e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a\|\|null==t.return\|\|t.return()}finally{if(o)throw u}}return e}}` | 44 |
| `function(n){throw M(new De)}` | 39 |
| `function(r){var n;return r&&"object"==typeof r&&"default"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 39 |
| `function n(r){return n(r)}` | 38 |
| `function(n){return typeof n}` | 38 |
| `function(o){return o&&"function"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o}` | 38 |
| `function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 37 |
| `function(){return this.b}` | 33 |
| `function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]\|\|l["@@iterator"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}` | 33 |
| `function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}` | 32 |
| `function(n){return Ei(n)}` | 30 |
| `function(n){return L(fn,X,2,n,6,1)}` | 30 |
| `function(n){}` | 29 |
| `function(r){if(typeof Symbol<"u"&&null!=r[Symbol.iterator]\|\|null!=r["@@iterator"])return Array.from(r)}` | 28 |
| ``function(){throw new TypeError(`Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}`` | 28 |
| `()=>{}` | 27 |
| `function(){return null}` | 27 |
| `function(n,o){e.exports=o(m,on(),dn)}` | 27 |
| `function(i){Fi(this,i)}` | 23 |
| `function(n,o){e.exports=o(dn,on(),m)}` | 22 |
| `function(){throw M(new Fe(Re((xe(),Ds))))}` | 21 |
| `function(){return this}` | 20 |
| `function(i,t){this.b=i,this.a=t}` | 19 |
| `function(n,e){throw M(new De)}` | 18 |
| `function(){return 0}` | 17 |
| `function(r){Object.defineProperty(e,r,Object.getOwnPropertyDescriptor(t,r))}` | 16 |
| `()=>{var l;return null!=(l=e.options.debugAll)?l:e.options.debugHeaders}` | 16 |
| `function(n){return n}` | 15 |
| `function(c){Kr.call(this,c)}` | 15 |
| `function(){return this.c}` | 15 |
| `function(){return this.d}` | 15 |
| `function(n,r){return n}` | 14 |
| `d=>d.id` | 14 |
| `function(){var r=function(t,o){return(r=Object.setPrototypeOf\|\|({__proto__:[]}instanceof Array?function(t,o){t.__proto__=o}:function(t,o){for(var n in o)Object.prototype.hasOwnProperty.call(o,n)&&(t[n]=o[n])}))(t,o)};return function(t,o){if("function"!=typeof o&&null!==o)throw new TypeError("Class extends value "+String(o)+" is not a constructor or null");function n(){this.constructor=t}r(t,o),t.prototype=null===o?Object.create(o):(n.prototype=o.prototype,new n)}}` | 14 |
| `function(_,o){_.__proto__=o}` | 14 |
| `function(o,r){for(var t in r)Object.prototype.hasOwnProperty.call(r,t)&&(o[t]=r[t])}` | 14 |
| `function(n){return!1}` | 13 |
| `function(t){var l=-1,n=null==t?0:t.length;for(this.clear();++l<n;){var r=t[l];this.set(r[0],r[1])}}` | 12 |
| `function(a,e,l){var i,r=l["aria-label"],t=l["aria-labelledby"],n=l.title;switch(a){case"img":return r\|\|t\|\|n?(f(i={},"aria-labelledby",t),f(i,"aria-label",r),f(i,"title",n),i):{"aria-label":"".concat(e.replace(/([a-z])([A-Z])/g,"$1 $2")," Icon")};case"presentation":return{"aria-hidden":!0,alt:""}}}` | 12 |
| `a=>a` | 12 |
| `a=>a()` | 12 |
| `function(n,a){n.a=a}` | 11 |
| `function(){ia.call(this)}` | 11 |
| `function(i,t,h){this.a=i,this.b=t,this.c=h}` | 11 |
| `function(n,r){return r}` | 11 |
| `function(){return this.a.gc()}` | 11 |
| `function(e){return e&&e.__esModule?e:{default:e}}` | 11 |
| `()=>n(!1)` | 11 |
| `function(i){this.b=i}` | 10 |
| `function(c){this.c=c}` | 10 |
| `function(){return this.f}` | 10 |
| `function(n){return n\|\|"div"}` | 10 |
| `function(n,r,i){var l;return i=null!=(l=i)?l:"div",n\|\|("string"==typeof(null==r?void 0:r.href)?"a":i)}` | 10 |

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

| Function code (minimized) | Copies | Notes |
| ------------------------- | ----------- | ----- |
| `function(r){for(var t=1;t<arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}` | 2205 | spread operator? |
| `function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}` | 1197 | `Object.assign` methods? |
| `function n(){return Object.assign,n.apply(this,arguments)}` | 1008 | `Object.assign` properties? |
| `function(){}` | 753 | no-op |
| `function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)r=f[u],0<=e.indexOf(r)\|\|(t[r]=n[r]);return t}` | 191 | ? |
| `function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)\|\|(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)\|\|Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}` | 187 | ? |
| `function(e,r){return r=r\|\|e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}` | 159 | ? |
| `function(r,t){if("object"!=typeof r\|\|null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t\|\|"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")}` | 113 | ? |
| `function(r,e,t){return i=function(r,e){if("object"!=typeof r\|\|null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if("object"!=typeof t)return t;throw new TypeError("@@toPrimitive must return a primitive value.")}(e,"string"),(e="symbol"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}` | 111 | ? |
| `function(t){t=function(t,r){if("object"!=typeof t\|\|null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if("object"!=typeof i)return i;throw new TypeError("@@toPrimitive must return a primitive value.")}(t,"string");return"symbol"==typeof t?t:String(t)}` | 111 | ? |
| `function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}` | 104 | ? |
| `function(n,r){(null==r\|\|r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];return l}` | 93 | array spread? |
| `function(){return!0}` | 92 | always-true |
| `function(){return!1}` | 78 | always-false |
| `function(r){if(Array.isArray(r))return r}` | 77 | self explanatory |
| `function(){throw new TypeError('Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.')}` | 77 | `isSymbol`? |
| `function(t){return t&&"object"==typeof t&&"default"in t?t:{default:t}}` | 76 | ? |
| `function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}` | 49 | ? |
| `function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(c,e),Object.defineProperty(y,e,t.get?t:{enumerable:!0,get:function(){return c[e]}}))}` | 49 | ? |
| `function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]\|\|l["@@iterator"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r\|\|e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a\|\|null==t.return\|\|t.return()}finally{if(o)throw u}}return e}}` | 44 | ? |
| `function(r){var n;return r&&"object"==typeof r&&"default"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 39 | ? |
| `function n(r){return n(r)}` | 38 | `Function.apply`? |
| `function(n){return typeof n}` | 38 | `typeof` |
| `function(o){return o&&"function"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o}` | 38 | ? |
| `function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 37 | `import`? |
| `function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]\|\|l["@@iterator"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}` | 33 | ? |
| `function(n){}` | 29 | no-op |
| `function(r){if(typeof Symbol<"u"&&null!=r[Symbol.iterator]\|\|null!=r["@@iterator"])return Array.from(r)}` | 28 | ? |
| `function(){throw new TypeError('Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.')}` | 28 | ? |
| `()=>{}` | 27 | no-op |
| `function(){return null}` | 27 | always-null |
| `function(){return this}` | 20 | always-this |
| `function(){return 0}` | 17 | always-zero |
| `function(n){return n}` | 15 | identity |
| `function(n,r){return n}` | 14 | always-first-argument |
| `d=>d.id` | 14 | `.id` |
| `function(){var r=function(t,o){return(r=Object.setPrototypeOf\|\|({__proto__:[]}instanceof Array?function(t,o){t.__proto__=o}:function(t,o){for(var n in o)Object.prototype.hasOwnProperty.call(o,n)&&(t[n]=o[n])}))(t,o)};return function(t,o){if("function"!=typeof o&&null!==o)throw new TypeError("Class extends value "+String(o)+" is not a constructor or null");function n(){this.constructor=t}r(t,o),t.prototype=null===o?Object.create(o):(n.prototype=o.prototype,new n)}}` | 14 | ? |
| `function(_,o){_.__proto__=o}` | 14 | `Object.is_a`? |
| `function(o,r){for(var t in r)Object.prototype.hasOwnProperty.call(r,t)&&(o[t]=r[t])}` | 14 | ? |
| `function(n){return!1}` | 13 | always-false |
| `a=>a` | 12 | identity |
| `a=>a()` | 12 | call first argument |
| `function(n,a){n.a=a}` | 11 | enum/const definition? |
| `function(n,r){return r}` | 11 | always-second-argument |
| `function(e){return e&&e.__esModule?e:{default:e}}` | 11 | import default |
| `function(c){this.c=c}` | 10 | enum/const definition? |

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

| Bundler | Build time |
| ------- | ---------- |
| bun     | 0.43s      |
| esbuild | 2.57s      |
| vite    | 85.04s     |
| webpack | 138.64s    |

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

| Function | Copies |
| -------- | ------ |
| `function(r){for(var t=1;t<arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}` | 2216 |
| `function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}` | 1204 |
| `function n(){return Object.assign,n.apply(this,arguments)}` | 1010 |
| `function(){}` | 844 |
| `function(t){return t&&"object"==typeof t&&"default"in t?t:{default:t}}` | 260 |
| `function(i){this.a=i}` | 250 |
| `function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)r=f[u],0<=e.indexOf(r)\|\|(t[r]=n[r]);return t}` | 203 |
| `function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)\|\|(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)\|\|Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}` | 194 |
| `function(e,r){return r=r\|\|e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}` | 160 |
| `function(r,t){if("object"!=typeof r\|\|null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t\|\|"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")}` | 137 |
| `function(r,e,t){return i=function(r,e){if("object"!=typeof r\|\|null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if("object"!=typeof t)return t;throw new TypeError("@@toPrimitive must return a primitive value.")}(e,"string"),(e="symbol"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}` | 134 |
| `function(t){t=function(t,r){if("object"!=typeof t\|\|null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if("object"!=typeof i)return i;throw new TypeError("@@toPrimitive must return a primitive value.")}(t,"string");return"symbol"==typeof t?t:String(t)}` | 134 |
| `()=>{}` | 129 |
| `function(n){return this===n}` | 119 |
| `function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}` | 115 |
| `function(n,r){(null==r\|\|r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];return l}` | 106 |
| `function(c,i){Bs.call(this,c,i)}` | 94 |
| `function(){return!0}` | 93 |
| `function(r){if(Array.isArray(r))return r}` | 83 |
| `function(){throw new TypeError(`Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}` | 83 |
| `function(){return!1}` | 79 |
| `function(){return new cu(this)}` | 77 |
| `function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(b,e),Object.defineProperty(E,e,t.get?t:{enumerable:!0,get:function(){return b[e]}}))}` | 76 |
| `function(){return b[P]}` | 76 |
| `function(a,e,l){var i,r=l["aria-label"],t=l["aria-labelledby"],n=l.title;switch(a){case"img":return r\|\|t\|\|n?(s(i={},"aria-labelledby",t),s(i,"aria-label",r),s(i,"title",n),i):{"aria-label":"".concat(e.replace(/([a-z])([A-Z])/g,"$1 $2")," Icon")};case"presentation":return{"aria-hidden":!0,alt:""}}}` | 76 |
| `function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 67 |
| `function(n){return typeof n}` | 64 |
| `function(o){return o&&"function"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o}` | 64 |
| `function n(r){return n(r)}` | 63 |
| `function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}` | 61 |
| `function(i,t){this.a=i,this.b=t}` | 58 |
| `function(r){var n;return r&&"object"==typeof r&&"default"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;"default"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 50 |
| `function(r){if(typeof Symbol<"u"&&null!=r[Symbol.iterator]\|\|null!=r["@@iterator"])return Array.from(r)}` | 49 |
| ``function(){throw new TypeError(`Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}`` | 49 |
| `function(){return this.a}` | 49 |
| `function(i){p0(this,i)}` | 48 |
| `function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]\|\|l["@@iterator"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r\|\|e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a\|\|null==t.return\|\|t.return()}finally{if(o)throw u}}return e}}` | 46 |
| `function(){throw St(new Ss)}` | 46 |
| `function(n){throw St(new Ss)}` | 39 |
| `()=>{"use strict";Vu(),Du()}` | 38 |
| `function(l,r){var t=null==l?null:typeof Symbol<"u"&&l[Symbol.iterator]\|\|l["@@iterator"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}` | 37 |
| `function(){return this.b}` | 33 |
| `function(){X(x)}` | 32 |
| `function(n){}` | 31 |
| `a=>a()` | 30 |
| `function(n){return V1(n)}` | 30 |
| `function(n){return dn(oi,mr,2,n,6,1)}` | 30 |
| `function(){return null}` | 29 |
| `function(){return this}` | 24 |
| `()=>{"use strict";Du()}` | 23 |
| `function(i){g0(this,i)}` | 23 |
| `function(t,r){var e;if(t)return"string"==typeof t?k(t,r):"Map"===(e="Object"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)\|\|"Set"===e?Array.from(t):"Arguments"===e\|\|/^(?:Ui\|I)nt(?:8\|16\|32)(?:Clamped)?Array$/.test(e)?k(t,r):void 0}` | 22 |
| `function(t,r){var e;if(t)return"string"==typeof t?P(t,r):"Map"===(e="Object"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)\|\|"Set"===e?Array.from(t):"Arguments"===e\|\|/^(?:Ui\|I)nt(?:8\|16\|32)(?:Clamped)?Array$/.test(e)?P(t,r):void 0}` | 22 |
| `function(n){return null!=n&&n instanceof Array}` | 21 |
| `function(r){if(Array.isArray(r))return P(r)}` | 21 |
| `function(e){return e&&e.__esModule?e:{default:e}}` | 21 |
| `function(t,r){var e;if(t)return"string"==typeof t?Q(t,r):"Map"===(e="Object"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)\|\|"Set"===e?Array.from(t):"Arguments"===e\|\|/^(?:Ui\|I)nt(?:8\|16\|32)(?:Clamped)?Array$/.test(e)?Q(t,r):void 0}` | 21 |
| `function(){throw St(new Os(il((qs(),_g))))}` | 21 |
| `function(n){return n}` | 20 |
| `function(n){return null!=n&&n.nodeType===Node.ELEMENT_NODE}` | 20 |
| `function(e){throw Error("Received unhandled value: ".concat(e))}` | 20 |
| `function(r,n){return Array.isArray(r)?r.concat(n):"string"==typeof r?r:void 0}` | 19 |

`bun`:

| Function | Copies |
| -------- | ------ |
| `function(){}` | 739 |
| `function(i){this.a=i}` | 250 |
| `function(r){for(var t=1;t<arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}` | 197 |
| `()=>{}` | 141 |
| `function(n){return this===n}` | 119 |
| `function(c,f){f7.call(this,c,f)}` | 94 |
| `function(){return!0}` | 91 |
| `function(){return new p9(this)}` | 77 |
| `function(){return!1}` | 76 |
| `function(i,t){this.a=i,this.b=t}` | 58 |
| `function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)r=f[u],0<=e.indexOf(r)\|\|(t[r]=n[r]);return t}` | 53 |
| `function(r,t){if("object"!=typeof r\|\|null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return("string"===t?String:Number)(r);e=e.call(r,t\|\|"default");if("object"!=typeof e)return e;throw new TypeError("@@toPrimitive must return a primitive value.")}` | 51 |
| `function(){return this.a}` | 49 |
| `function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)\|\|(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)\|\|Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}` | 48 |
| `function(r,e,t){return i=function(r,e){if("object"!=typeof r\|\|null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if("object"!=typeof t)return t;throw new TypeError("@@toPrimitive must return a primitive value.")}(e,"string"),(e="symbol"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}` | 48 |
| `function(t){t=function(t,r){if("object"!=typeof t\|\|null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if("object"!=typeof i)return i;throw new TypeError("@@toPrimitive must return a primitive value.")}(t,"string");return"symbol"==typeof t?t:String(t)}` | 48 |
| `function(i){T6(this,i)}` | 48 |
| `()=>{R3(),G3()}` | 46 |
| `function(){throw x0(new w7)}` | 46 |
| `function(e,r){return r=r\|\|e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}` | 45 |
| `function(n,r){(null==r\|\|r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];return l}` | 41 |
| `function(n){throw x0(new w7)}` | 39 |
| `function(r){if(Array.isArray(r))return r}` | 36 |
| `function(){throw new TypeError("Invalid attempt to destructure non-iterable instance.\\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}` | 36 |
| `function(){return this.b}` | 33 |
| `function(n){return n2(n)}` | 30 |
| `function(n){return J1($5,p1,2,n,6,1)}` | 30 |
| `function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}` | 29 |
| `function(l,r){var t=null==l?null:"undefined"!=typeof Symbol&&l[Symbol.iterator]\|\|l["@@iterator"];if(null!=t){var e,n,u,f,i=[],a=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;a=!1}else for(;!(a=(e=u.call(t)).done)&&(i.push(e.value),i.length!==r);a=!0);}catch(l){o=!0,n=l}finally{try{if(!a&&null!=t.return&&(f=t.return(),Object(f)!==f))return}finally{if(o)throw n}}return i}}` | 29 |
| `function(n){}` | 29 |
| `function(){return null}` | 28 |
| `function(e){return Object.getOwnPropertyDescriptor(Z,e).enumerable}` | 25 |
| `function(e){Object.defineProperty(Z,e,Object.getOwnPropertyDescriptor(W,e))}` | 25 |
| `function(n){return n}` | 23 |
| `function(i){C6(this,i)}` | 23 |
| `()=>{G3()}` | 22 |
| `function(){throw x0(new C7(n7((y7(),KG))))}` | 21 |
| `function(){return this}` | 19 |
| `function(i,t){this.b=i,this.a=t}` | 19 |
| `function(n){return!1}` | 18 |
| `function(n,w){throw x0(new w7)}` | 18 |
| `function(){return 0}` | 17 |
| `function(n){return typeof n}` | 17 |
| `function(o){return o&&"function"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o}` | 17 |
| `()=>{R3()}` | 16 |
| `()=>{var e;return null!=(e=Z.options.debugAll)?e:Z.options.debugHeaders}` | 16 |
| `function(n,r){return n}` | 15 |
| `function(c){hX.call(this,c)}` | 15 |
| `function(){return this.c}` | 15 |
| `function(){return this.d}` | 15 |
| `function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}` | 14 |
| `function(r){if("undefined"!=typeof Symbol&&null!=r[Symbol.iterator]\|\|null!=r["@@iterator"])return Array.from(r)}` | 14 |
| `function(){throw new TypeError("Invalid attempt to spread non-iterable instance.\\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.")}` | 14 |
| `d=>d.id` | 14 |
| `()=>W(!1)` | 13 |
| `a=>a` | 12 |
| `function(t){var l=-1,n=null==t?0:t.length;for(this.clear();++l<n;){var r=t[l];this.set(r[0],r[1])}}` | 12 |
| `function(n,c){}` | 12 |
| `function(n,r){return r}` | 11 |
| `function(n,a){n.a=a}` | 11 |
| `function(){_V.call(this)}` | 11 |
| `function(i,t,h){this.a=i,this.b=t,this.c=h}` | 11 |
| `function(){return this.a.gc()}` | 11 |
| `function(i){this.b=i}` | 10 |
| `function(c){this.c=c}` | 10 |
| `function(){return this.f}` | 10 |

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

<div class="content-read-marker" data-fraction="50"></div>

Here is a shortened list of top abusers from different bundlers for our tool:

+----------------------------------------------------------------------------+----------------+---------------+--------------+
| Function                                                                   | vite           | esbuild       | bun          |
|                                                                            +------+---------+------+--------+------+-------+
|                                                                            | \#   | Bytes   | \#   | Bytes  | \#   | Bytes |
+============================================================================+======+=========+======+========+======+=======+
| ```js                                                                      |      |         |      |        |      |       |
| function(){}                                                               |      |         |      |        |      |       |
| ```                                                                        | 753  | 9036    | 844  | 10128  | 739  | 8868  |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(){return!0}                                                       |      |         |      |        |      |       |
| ```                                                                        | 92   | 1840    | 93   | 1860   | 91   | 1820  |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(){return!1}                                                       |      |         |      |        |      |       |
| ```                                                                        | 78   | 1560    | 79   | 1580   | 76   | 1520  |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(){return null}                                                    |      |         |      |        |      |       |
| ```                                                                        | 27   | 621     | 29   | 667    | 28   | 644   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(){return this}                                                    |      |         |      |        |      |       |
| ```                                                                        | 20   | 460     | 24   | 552    | 19   | 437   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(){return 0}                                                       |      |         |      |        |      |       |
| ```                                                                        | 17   | 340     | n/a  | 0      | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n){return!1}                                                      |      |         |      |        |      |       |
| ```                                                                        | 13   | 273     | n/a  | 0      | 18   | 378   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n){}                                                              |      |         |      |        |      |       |
| ```                                                                        | 29   | 377     | 31   | 403    | 29   | 377   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n){return n}                                                      |      |         |      |        |      |       |
| ```                                                                        | 15   | 315     | 20   | 420    | 23   | 483   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n){return typeof n}                                               |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | 64   | 1792   | 17   | 476   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n){return this===n}                                               |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | 119  | 3332   | 119  | 3332  |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n,r){return n}                                                    |      |         |      |        |      |       |
| ```                                                                        | 14   | 322     | n/a  | 0      | 15   | 345   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n,r){return r}                                                    |      |         |      |        |      |       |
| ```                                                                        | 11   | 253     | n/a  | 0      | 11   | 253   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n,c){}                                                            |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | n/a  | 0      | 12   | 180   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| ()=>{}                                                                     |      |         |      |        |      |       |
| ```                                                                        | 27   | 162     | 129  | 774    | 141  | 846   |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| a=>a                                                                       |      |         |      |        |      |       |
| ```                                                                        | 12   | 48      | n/a  | 0      | 12   | 48    |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| a=>a()                                                                     |      |         |      |        |      |       |
| ```                                                                        | 12   | 72      | n/a  | 0      | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(r){                                                               |      |         |      |        |      |       |
| for(var t=1;t<arguments.length;t++){                                       |      |         |      |        |      |       |
| var n,o=arguments[t];                                                      |      |         |      |        |      |       |
| for(n in o)                                                                |      |         |      |        |      |       |
| Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])                     |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| return r                                                                   |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | 2205 | 317520  | 2216 | 319104 | 197  | 28368 |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function n(){                                                              |      |         |      |        |      |       |
| return Object.assign&&Object.assign.bind(),n.apply(this,arguments)         |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | 1197 | 95760   | 1204 | 96320  | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function n(){return Object.assign,n.apply(this,arguments)}                 |      |         |      |        |      |       |
| ```                                                                        | 1008 | 58464   | 1010 | 58580  | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n,r){(null==r\|\|r>n.length)&&(r=n.length);                       |      |         |      |        |      |       |
| for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];                              |      |         |      |        |      |       |
| return l}                                                                  |      |         |      |        |      |       |
| ```                                                                        | 93   | 9672    | 106  | 11024  | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(r){if(Array.isArray(r))return r}                                  |      |         |      |        |      |       |
| ```                                                                        | 77   | 3157    | 83   | 3403   | 36   | 1476  |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(){                                                                |      |         |      |        |      |       |
| throw new TypeError(&#39;Invalid attempt to destructure                    |      |         |      |        |      |       |
| non-iterable instance.\nIn order to be iterable,                           |      |         |      |        |      |       |
| non-array objects must have a [Symbol.iterator]() method.&#39;)}           |      |         |      |        |      |       |
| ```                                                                        | 77   | 13244   | 83   | 14276  | 36   | 6192  |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(t){                                                               |      |         |      |        |      |       |
| return t&&"object"==typeof t&&"default"in t?t:{default:t}                  |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | 76   | 5624    | 260  | 19240  | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(_,o){_.__proto__=o}                                               |      |         |      |        |      |       |
| ```                                                                        | 14   | 392     | n/a  | 0      | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(o,r){                                                             |      |         |      |        |      |       |
| for(var t in r)                                                            |      |         |      |        |      |       |
| Object.prototype.hasOwnProperty.call(r,t)&&(o[t]=r[t])                     |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | 14   | 1176    | n/a  | 0      | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(e){return e&&e.__esModule?e:{default:e}}                          |      |         |      |        |      |       |
| ```                                                                        | 11   | 539     | 21   | 1029   | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(e,n,r){                                                           |      |         |      |        |      |       |
| return n in e?Object.defineProperty(e,n,{                                  |      |         |      |        |      |       |
| value:r,enumerable:!0,configurable:!0,writable:!0                          |      |         |      |        |      |       |
| }):e[n]=r,e                                                                |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | 115  | 13570  | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(r,n){                                                             |      |         |      |        |      |       |
| return Array.isArray(r)?r.concat(n):"string"==typeof r?r:void 0            |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | 19   | 1520   | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n){return null!=n&&n instanceof Array}                            |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | 21   | 987    | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(n,e){                                                             |      |         |      |        |      |       |
| if(null==n)                                                                |      |         |      |        |      |       |
| return{};                                                                  |      |         |      |        |      |       |
| for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)                        |      |         |      |        |      |       |
| r=f[u],0<=e.indexOf(r)\|\|(t[r]=n[r]);                                     |      |         |      |        |      |       |
| return t                                                                   |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | n/a  | 0      | 53   | 6890  |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(r,t){                                                             |      |         |      |        |      |       |
| if("object"!=typeof r\|\|null===r)return r;                                |      |         |      |        |      |       |
| var e=r[Symbol.toPrimitive];                                               |      |         |      |        |      |       |
| if(void 0===e)return("string"===t?String:Number)(r);                       |      |         |      |        |      |       |
| e=e.call(r,t\|\|"default");                                                |      |         |      |        |      |       |
| if("object"!=typeof e)return e;                                            |      |         |      |        |      |       |
| throw new TypeError("@@toPrimitive must return a primitive value.")        |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | 137  | 36853  | 51   | 13719 |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(e,r){                                                             |      |         |      |        |      |       |
| return r=r\|\|e.slice(0),                                                  |      |         |      |        |      |       |
| Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))   |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | 160  | 17600  | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(o){                                                               |      |         |      |        |      |       |
| return o&&"function"==typeof Symbol&&                                      |      |         |      |        |      |       |
| o.constructor===Symbol&&o!==Symbol.prototype?"symbol":typeof o             |      |         |      |        |      |       | 
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | 64   | 7424   | n/a  | 0     |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+
| ```js                                                                      |      |         |      |        |      |       |
| function(r){                                                               |      |         |      |        |      |       |
| if("undefined"!=typeof Symbol&&                                            |      |         |      |        |      |       |
| null!=r[Symbol.iterator]\|\|null!=r["@@iterator"])                         |      |         |      |        |      |       |
| return Array.from(r)                                                       |      |         |      |        |      |       |
| }                                                                          |      |         |      |        |      |       |
| ```                                                                        | n/a  | 0       | n/a  | 0      | 14   | 1624  |
+----------------------------------------------------------------------------+------+---------+------+--------+------+-------+

Interestingly enough, aside from a lot of `() => {}` and `(a, b) => a` and `() => true` (as I call them, utility) functions,
there are a lot of ES6 / TypeScript helpers such as class definition and spread operator variants, presumingly made to be compatible with ES5-only browsers.
Maybe if we had targeted only platforms supporting the latest ES features we would get better results?

Well, not quite much:

bundle sizes:

| Bundler         | Bundle size |
| --------------- | ----------- |
| bun             | 5.4M        |
| esbuild         | 9.2M        |
| esbuild (tuned) | 8.0M        |
| vite            | 7.1M        |
| vite (tuned)    | 3.8M        |
| webpack         | 4.4M        |

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
* some functions are not used at all: <img src="/images/how-unique-are-your-bundles/unused-deduplicated-functions.webp" alt="Unused aliases">

For the shorthand functions I first tried manually fixing them up - had to replace them with `$z=function(){return $z=Object.assign.bind(),$z.apply(this,arguments)}` alikes. This worked, so I created an AST transformer to handle these one-line return-only functions:

```js
const simplifyFunction = (code, fname) => {
    const tmpFilename = '_tmp';

    fs.writeFileSync(tmpFilename, code, 'utf-8');

    const root = ts.createSourceFile(
        tmpFilename,
        code,
        ts.ScriptTarget.ESNext,
        /* setParentNodes */ true
    );

    let rootFnName = undefined;

    const parse = (node) => {
        if (ts.isFunctionDeclaration(node) && ts.isIdentifier(node.name) && node.name.escapedText !== '') {
            rootFnName = node.name.escapedText;
            return;
        }

        ts.forEachChild(node, child => {
            if (child) {
                parse(child);
            }
        });
    };

    parse(root);

    if (!rootFnName) {
        fs.rmSync(tmpFilename);
        return code;
    }

    const transformer = (ctx) => (sourceFile) => {
        const visit = (node) => {
            if (ts.isIdentifier(node) && node.escapedText === rootFnName) {
                return ts.factory.createIdentifier(fname);
            }

            if (
                ts.isFunctionDeclaration(node) &&
                ts.isBlock(node.body) &&
                node.body.statements.length === 1 &&
                ts.isReturnStatement(node.body.statements[0]) &&
                ts.isBinaryExpression(node.body.statements[0].expression)
            ) {
                const next = ts.factory.createFunctionDeclaration(
                    [],
                    undefined,
                    undefined,
                    [],
                    [],
                    undefined,

                    ts.factory.createBlock([
                        ts.factory.createReturnStatement(
                            ts.factory.createComma(
                                ts.factory.createAssignment(
                                    ts.factory.createIdentifier(fname),
                                    node.body.statements[0].expression.left
                                ),

                                node.body.statements[0].expression.right
                            )
                        )
                    ])
                );

                return ts.visitEachChild(next, visit, ctx);
            }

            return ts.visitEachChild(node, visit, ctx);
        };

        return ts.visitNode(sourceFile, visit);
    };

    const s = ts.createSourceFile(tmpFilename, code, ts.ScriptTarget.ESNext);
    const { transformed } = ts.transform(s, [ transformer ]);

    const newCode = ts.createPrinter({ omitTrailingSemicolon: true })
        .printFile(transformed.find(({ fileName }) => fileName === tmpFilename));

    fs.rmSync(tmpFilename);

    return newCode;
};
```

The transformer is essentially a two-pass processor: it first parses the `code` and identifies the first function declaration.
If none was found - it just returns the original code. If there was a so-called "root function" defined, it then replaces all
identifier with that "root function" name with the alias provided as `fname`.
It also replaces the return statements in form of a `return something && something()` with `return alias = something, alias()`.

This approach is different from simply using `uglifyjs` to just try and minimize the code - it is way more complex (compared to just one function call). It results in few extra whitespaces being added. But using `uglifyjs` messes things up again and TypeScript compiler does not have an option for minimizing the output. But few extra whitespaces are totally acceptable given the much bigger savings from this transformation.

With the constructors I simply excluded them from being de-duplicated. This resulted in `155` fewer substitutions (in Vite mode), which is negligible on the overall scale of the problem.

As for the named functions which are in the global scope and are referenced later down the line, I had to create a list of "backwards-compatible aliases", mapping those old function names onto the new unique names.

I ended up with a three-pass parser-transformer utility (not really "simple script" anymore). The passes being:

1. figuring out the duplicate function declarations and replacing them from the end to the start of the code (to minimize the chance of writing over the just-changed code)
2. removing the duplicate global-scope named functions and replacing them with shorthand aliases to the de-duplicated declarations
3. replace the usages of all known aliases and shorthand-named de-duplicated functions with their corresponding new (generated) names

Other than those, replacing the original bundle with the optimized one worked like a charm!

The results? With the threshold of `20` duplicates or more:

+---------+---------------------+-----------------+------------------+---------------------+-------------------+
| Bundler | Before optimization                                                                                |
|         +---------------------+-----------------+------------------+---------------------+-------------------+
|         | Bundle size         | Total functions | Unique functions | Unique functions, % | Duplicate code, % |
+=========+=====================+=================+==================+=====================+===================+
| bun     | 6.2M                | 8903            | 7443             | 83.6%               | 0.78%             |
+---------+---------------------+-----------------+------------------+---------------------+-------------------+
| esbuild | 8.7M                | 13057           | 10250            | 78.5%               | 3.9%              |
+---------+---------------------+-----------------+------------------+---------------------+-------------------+
| vite    | 3.9M                | 3502            | 2365             | 67.53%              | 6.39%             |
+---------+---------------------+-----------------+------------------+---------------------+-------------------+
| webpack | 4.4M                | 2898            | 1434             | 49.48%              | 6.91%             |
+:-------:+---------------------+-----------------+------------------+---------------------+-------------------+
| **After optimization**                                                                                       | 
+---------+---------------------+-----------------+------------------+---------------------+-------------------+
| bun     | 6.2M (same)         | 7865 (-1038)    | 7355 (-88)       | 93.52% (+9.92%)     | 0.51% (-0.27%)    |
+---------+---------------------+-----------------+------------------+---------------------+-------------------+
| esbuild | 8.5M (-0.2M)        | 3265 (-9792)    | 2990 (-7260)     | 91.58% (+13.08%)    | 0.62% (-3.28%)    |
+---------+---------------------+-----------------+------------------+---------------------+-------------------+
| vite    | 3.6M (-0.3M)        | 2483 (-1019)    | 2277 (-88)       | 91.7% (+24.17%)     | 1.68% (-4.71%)    |
+---------+---------------------+-----------------+------------------+---------------------+-------------------+
| webpack | 4.1M (-0.3M)        | 1484 (-1414)    | 1375 (-59)       | 92.65% (+43.17%)    | 0.43% (-6.48%)    |
+---------+---------------------+-----------------+------------------+---------------------+-------------------+

In conclusion, the bundlers do a pretty average job at optimizing the bundles, even in production mode with some extra tuning.
And if some brave soul is willing to invest even more time and effort than I did into developing a sophisticated solution
(potentially improving the existing tools, like uglifyjs or bundlers themselves), the numbers can be improved even further.
It would be really interesting to see what would the results be running this optimizer on a bigger bundle.

In my humble opinion, Bun does produce the cleanest bundle. It might be not the smallest one, but it has least unnecessary stuff.
On top of that, it is the fastest tool in JS world I have ever used.
By the way, this very blog is built using Bun and React SSR - on Github Actions it takes around a minute to build and publish:

<img src="/images/how-unique-are-your-bundles/gh-pages-bun-build.webp" loading="lazy" alt="Github Actions build and publish with Bun">

<img src="/images/how-unique-are-your-bundles/gh-pages-action-breakdown.webp" loading="lazy" alt="Github Actions build breakdown">

<div class="content-read-marker" data-fraction="100"></div>

<style>
    .center {
        text-align: center;
    }
    .center:has(button) {
        margin-top: 0.5em;
    }
</style>
<script>
[...document.querySelectorAll('table')].forEach((table, idx) => {
    if (table.querySelectorAll('tbody tr').length < 10) {
        return;
    }
    table.id = `table-${idx}`;
    table.classList.add('expandable');
    const div = document.createElement('div');
    div.classList.add('center');
    const btn = document.createElement('button');
    btn.innerText = 'Show more';
    div.appendChild(btn);
    table.after(div);
    btn.onclick = () => {
        const tbl = document.querySelector(`table#${table.id}`);
        tbl.classList.toggle('expand');
        if (tbl.classList.contains('expand')) {
            btn.innerText = 'Show less';
        } else {
            btn.innerText = 'Show more';
        }
    };
});
</script>
