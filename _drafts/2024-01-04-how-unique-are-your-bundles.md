---
---

In the modern front-end development we are all used to package managers, transpilation and bundling.
These concepts are the biproduct of the tools which we hoped would simplify developer' burden and
speed up the development.

However, how confident are you these tools are actually doing good job?

Developers seem comfortable off-loading the processing power to users' machine (browser, predominantly).
We are not surprised by seeing slow websites anymore. A simple blog is downloading 55MB of JavaScript?
Seems fine nowadays.

I currently work on a fairly small tool ([MongoDB Relational Migrator]()), which also utilizes TypeScript, React and,
of course, bundling. We use Vite for that. Our bundles are split into chunks (but I have accounted for that, too).

I went ahead and wrote a rather [simple script]() which parses the bundles (using TypeScript compiler API, because why not),
extracting the function definitions (both arrow functions and plain old `function`) and counts how many times they
occur in the file. For this last bit, to make sure I am not counting `a => true` and `x => true` as different occurrences,
I am minimizing the function definition with `uglifyjs` and counting the SHA256 hashes of the minimized functions
(just to have a reasonable key in my hashmap instead of entire function code).

These are my findings.

Out of 54 chunks, 47 are not css-in-js chunks. Out of 47 remaining, 7 have any significant duplication.
But when they do, they do it hard: duplication is varying between 18% and a whopping 42% of sheer file size.
Absolute numbers are also astonishing: 25% to 60% functions are duplicates.

```
Found 15192 functions, 8963 are unique (59%)
1518418 out of 3537579 bytes are duplicate code (42.92%)

Found 1202 functions, 494 are unique (41.1%)
130649 out of 340227 bytes are duplicate code (38.4%)

Found 598 functions, 267 are unique (44.65%)
57607 out of 164737 bytes are duplicate code (34.97%)

Found 154 functions, 98 are unique (63.64%)
11140 out of 45135 bytes are duplicate code (24.68%)

Found 17 functions, 10 are unique (58.82%)
1932 out of 6532 bytes are duplicate code (29.58%)

Found 968 functions, 651 are unique (67.25%)
52616 out of 281406 bytes are duplicate code (18.7%)

Found 11123 functions, 8398 are unique (75.5%)
76244 out of 1421129 bytes are duplicate code (5.37%)
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

| Function code (minimized) | Occurrences |
| ------------------------- | ----------- |
| `function(r){for(var t=1;t<arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}` | 2205 |
| `function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}` | 1197 |
| `function n(){return Object.assign,n.apply(this,arguments)}` | 1008 |
| `function(){}` | 753 |
| `function(i){this.a=i}` | 250 |
| `function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)r=f[u],0<=e.indexOf(r)||(t[r]=n[r]);return t}` | 191 |
| `function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)||(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)||Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}` | 187 |
| `function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}` | 159 |
| `function(n){return this===n}` | 119 |
| `function(r,t){if(\"object\"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return(\"string\"===t?String:Number)(r);e=e.call(r,t||\"default\");if(\"object\"!=typeof e)return e;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}` | 113 |
| `function(r,e,t){return i=function(r,e){if(\"object\"!=typeof r||null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if(\"object\"!=typeof t)return t;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}(e,\"string\"),(e=\"symbol\"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}` | 111 |
| `function(t){t=function(t,r){if(\"object\"!=typeof t||null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if(\"object\"!=typeof i)return i;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}(t,\"string\");return\"symbol\"==typeof t?t:String(t)}` | 111 |
| `function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}` | 104 |
| `function(c,i){He.call(this,c,i)}` | 94 |
| `function(n,r){(null==r||r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];return l}` | 93 |
| `function(){return!0}` | 92 |
| `function(){return!1}` | 78 |
| `function(){return new gt(this)}` | 77 |
| `function(r){if(Array.isArray(r))return r}` | 77 |
| `function(){throw new TypeError(`Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}` | 77 |
| `function(t){return t&&\"object\"==typeof t&&\"default\"in t?t:{default:t}}` | 76 |
| `function(i,t){this.a=i,this.b=t}` | 58 |
| `function(){return this.a}` | 49 |
| `function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}` | 49 |
| `function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(c,e),Object.defineProperty(y,e,t.get?t:{enumerable:!0,get:function(){return c[e]}}))}` | 49 |
| `function(){return c[b]}` | 49 |
| `function(a,e,i){var r,t=i[\"aria-label\"],n=i[\"aria-labelledby\"],c=i.title;switch(a){case\"img\":return t||n||c?(l(r={},\"aria-labelledby\",n),l(r,\"aria-label\",t),l(r,\"title\",c),r):{\"aria-label\":\"\".concat(e.replace(/([a-z])([A-Z])/g,\"$1 $2\"),\" Icon\")};case\"presentation\":return{\"aria-hidden\":!0,alt:\"\"}}}` | 49 |
| `function(i){Di(this,i)}` | 48 |
| `function(r){return Object.getOwnPropertyDescriptor(e,r).enumerable}` | 48 |
| `function(){throw M(new De)}` | 46 |
| `function(l,r){var t=null==l?null:typeof Symbol<\"u\"&&l[Symbol.iterator]||l[\"@@iterator\"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r||e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a||null==t.return||t.return()}finally{if(o)throw u}}return e}}` | 44 |
| `function(n){throw M(new De)}` | 39 |
| `function(r){var n;return r&&\"object\"==typeof r&&\"default\"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 39 |
| `function n(r){return n(r)}` | 38 |
| `function(n){return typeof n}` | 38 |
| `function(o){return o&&\"function\"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?\"symbol\":typeof o}` | 38 |
| `function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 37 |
| `function(){return this.b}` | 33 |
| `function(l,r){var t=null==l?null:typeof Symbol<\"u\"&&l[Symbol.iterator]||l[\"@@iterator\"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}` | 33 |
| `function(t){Object.defineProperty(e,t,Object.getOwnPropertyDescriptor(n,t))}` | 32 |
| `function(n){return Ei(n)}` | 30 |
| `function(n){return L(fn,X,2,n,6,1)}` | 30 |
| `function(n){}` | 29 |
| `function(r){if(typeof Symbol<\"u\"&&null!=r[Symbol.iterator]||null!=r[\"@@iterator\"])return Array.from(r)}` | 28 |
| `function(){throw new TypeError(`Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}` | 28 |
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
| `function(){var r=function(t,o){return(r=Object.setPrototypeOf||({__proto__:[]}instanceof Array?function(t,o){t.__proto__=o}:function(t,o){for(var n in o)Object.prototype.hasOwnProperty.call(o,n)&&(t[n]=o[n])}))(t,o)};return function(t,o){if(\"function\"!=typeof o&&null!==o)throw new TypeError(\"Class extends value \"+String(o)+\" is not a constructor or null\");function n(){this.constructor=t}r(t,o),t.prototype=null===o?Object.create(o):(n.prototype=o.prototype,new n)}}` | 14 |
| `function(_,o){_.__proto__=o}` | 14 |
| `function(o,r){for(var t in r)Object.prototype.hasOwnProperty.call(r,t)&&(o[t]=r[t])}` | 14 |
| `function(n){return!1}` | 13 |
| `function(t){var l=-1,n=null==t?0:t.length;for(this.clear();++l<n;){var r=t[l];this.set(r[0],r[1])}}` | 12 |
| `function(a,e,l){var i,r=l[\"aria-label\"],t=l[\"aria-labelledby\"],n=l.title;switch(a){case\"img\":return r||t||n?(f(i={},\"aria-labelledby\",t),f(i,\"aria-label\",r),f(i,\"title\",n),i):{\"aria-label\":\"\".concat(e.replace(/([a-z])([A-Z])/g,\"$1 $2\"),\" Icon\")};case\"presentation\":return{\"aria-hidden\":!0,alt:\"\"}}}` | 12 |
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
| `function(n){return n||\"div\"}` | 10 |
| `function(n,r,i){var l;return i=null!=(l=i)?l:\"div\",n||(\"string\"==typeof(null==r?void 0:r.href)?\"a\":i)}` | 10 |

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

| Function code (minimized) | Occurrences | Notes |
| ------------------------- | ----------- | ----- |
| `function(r){for(var t=1;t<arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}` | 2205 | spread operator? |
| `function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}` | 1197 | `Object.assign` methods? |
| `function n(){return Object.assign,n.apply(this,arguments)}` | 1008 | `Object.assign` properties? |
| `function(){}` | 753 | no-op |
| `function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)r=f[u],0<=e.indexOf(r)||(t[r]=n[r]);return t}` | 191 | ? |
| `function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)||(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)||Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}` | 187 | ? |
| `function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}` | 159 | ? |
| `function(r,t){if(\"object\"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return(\"string\"===t?String:Number)(r);e=e.call(r,t||\"default\");if(\"object\"!=typeof e)return e;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}` | 113 | ? |
| `function(r,e,t){return i=function(r,e){if(\"object\"!=typeof r||null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if(\"object\"!=typeof t)return t;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}(e,\"string\"),(e=\"symbol\"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}` | 111 | ? |
| `function(t){t=function(t,r){if(\"object\"!=typeof t||null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if(\"object\"!=typeof i)return i;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}(t,\"string\");return\"symbol\"==typeof t?t:String(t)}` | 111 | ? |
| `function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}` | 104 | ? |
| `function(n,r){(null==r||r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];return l}` | 93 | arrat spread? |
| `function(){return!0}` | 92 | always-true |
| `function(){return!1}` | 78 | always-false |
| `function(r){if(Array.isArray(r))return r}` | 77 | self explanatory |
| `function(){throw new TypeError('Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.')}` | 77 | `isSymbol`? |
| `function(t){return t&&\"object\"==typeof t&&\"default\"in t?t:{default:t}}` | 76 | ? |
| `function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}` | 49 | ? |
| `function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(c,e),Object.defineProperty(y,e,t.get?t:{enumerable:!0,get:function(){return c[e]}}))}` | 49 | ? |
| `function(l,r){var t=null==l?null:typeof Symbol<\"u\"&&l[Symbol.iterator]||l[\"@@iterator\"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r||e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a||null==t.return||t.return()}finally{if(o)throw u}}return e}}` | 44 | ? |
| `function(r){var n;return r&&\"object\"==typeof r&&\"default\"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 39 | ? |
| `function n(r){return n(r)}` | 38 | `Function.apply`? |
| `function(n){return typeof n}` | 38 | `typeof` |
| `function(o){return o&&\"function\"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?\"symbol\":typeof o}` | 38 | ? |
| `function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}` | 37 | `import`? |
| `function(l,r){var t=null==l?null:typeof Symbol<\"u\"&&l[Symbol.iterator]||l[\"@@iterator\"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}` | 33 | ? |
| `function(n){}` | 29 | no-op |
| `function(r){if(typeof Symbol<\"u\"&&null!=r[Symbol.iterator]||null!=r[\"@@iterator\"])return Array.from(r)}` | 28 | ? |
| `function(){throw new TypeError('Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.')}` | 28 | ? |
| `()=>{}` | 27 | no-op |
| `function(){return null}` | 27 | always-null |
| `function(){return this}` | 20 | always-this |
| `function(){return 0}` | 17 | always-zero |
| `function(n){return n}` | 15 | identity |
| `function(n,r){return n}` | 14 | always-first-argument |
| `d=>d.id` | 14 | `.id` |
| `function(){var r=function(t,o){return(r=Object.setPrototypeOf||({__proto__:[]}instanceof Array?function(t,o){t.__proto__=o}:function(t,o){for(var n in o)Object.prototype.hasOwnProperty.call(o,n)&&(t[n]=o[n])}))(t,o)};return function(t,o){if(\"function\"!=typeof o&&null!==o)throw new TypeError(\"Class extends value \"+String(o)+\" is not a constructor or null\");function n(){this.constructor=t}r(t,o),t.prototype=null===o?Object.create(o):(n.prototype=o.prototype,new n)}}` | 14 | ? |
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

`vite build`:

```
✓ built in 13.96s
yarn build  23.18s user 2.42s system 171% cpu 14.961 total
```

`esbuild build`:

```
yarn node esbuild.mjs  2.75s user 0.53s system 322% cpu 1.020 total
```

`bun build`:

```
[216ms] bundle 3089 modules
bun build src/app/index.tsx --outdir ./dist/bun --minify  0.41s user 0.39s system 256% cpu 0.311 total
```

And the analysis of the built bundles:

`vite`:

```
Found 15192 functions, 8963 are unique (59%)
Found 1202 functions, 494 are unique (41.1%)
Found 598 functions, 267 are unique (44.65%)
Found 154 functions, 98 are unique (63.64%)
Found 17 functions, 10 are unique (58.82%)
Found 968 functions, 651 are unique (67.25%)
Found 11123 functions, 8398 are unique (75.5%)
= Total 29254 functions, 18881 are unique (64.5%)

1518418 out of 3537579 bytes are duplicate code (42.92%)
130649 out of 340227 bytes are duplicate code (38.4%)
57607 out of 164737 bytes are duplicate code (34.97%)
11140 out of 45135 bytes are duplicate code (24.68%)
1932 out of 6532 bytes are duplicate code (29.58%)
52616 out of 281406 bytes are duplicate code (18.7%)
76244 out of 1421129 bytes are duplicate code (5.37%)
= Total 1848606 out of 5796745 bytes are duplicate code (31.8%)
```

`esbuild`:

```
Found 46654 functions
28952 are unique (62.06%)
Duplicates length: 6905599 bytes out of 9645594 bytes are duplicate code (71.59%)
```

`bun`:

```
Found 31113 functions
25755 are unique (82.78%)
Duplicates length: 446020 bytes out of 5696964 bytes are duplicate code (7.83%)
```

And a deeper analysis of the duplicated functions:

`esbuild`:

```json
[
  {
    "code": "function(r){for(var t=1;t<arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}",
    "duplicates": 2216,
    "length": 326705
  },
  {
    "code": "function n(){return Object.assign&&Object.assign.bind(),n.apply(this,arguments)}",
    "duplicates": 1204,
    "length": 280131
  },
  {
    "code": "function n(){return Object.assign,n.apply(this,arguments)}",
    "duplicates": 1010,
    "length": 213214
  },
  {
    "code": "function(){}",
    "duplicates": 844,
    "length": 12153
  },
  {
    "code": "function(t){return t&&\"object\"==typeof t&&\"default\"in t?t:{default:t}}",
    "duplicates": 260,
    "length": 19107
  },
  {
    "code": "function(i){this.a=i}",
    "duplicates": 250,
    "length": 6049
  },
  {
    "code": "function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)r=f[u],0<=e.indexOf(r)||(t[r]=n[r]);return t}",
    "duplicates": 203,
    "length": 28785
  },
  {
    "code": "function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)||(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)||Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}",
    "duplicates": 194,
    "length": 75876
  },
  {
    "code": "function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}",
    "duplicates": 160,
    "length": 18653
  },
  {
    "code": "function(r,t){if(\"object\"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return(\"string\"===t?String:Number)(r);e=e.call(r,t||\"default\");if(\"object\"!=typeof e)return e;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}",
    "duplicates": 137,
    "length": 36646
  },
  {
    "code": "function(r,e,t){return i=function(r,e){if(\"object\"!=typeof r||null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if(\"object\"!=typeof t)return t;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}(e,\"string\"),(e=\"symbol\"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}",
    "duplicates": 134,
    "length": 62258
  },
  {
    "code": "function(t){t=function(t,r){if(\"object\"!=typeof t||null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if(\"object\"!=typeof i)return i;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}(t,\"string\");return\"symbol\"==typeof t?t:String(t)}",
    "duplicates": 134,
    "length": 45263
  },
  {
    "code": "()=>{}",
    "duplicates": 129,
    "length": 1260
  },
  {
    "code": "function(n){return this===n}",
    "duplicates": 119,
    "length": 3304
  },
  {
    "code": "function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}",
    "duplicates": 115,
    "length": 14177
  },
  {
    "code": "function(n,r){(null==r||r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];return l}",
    "duplicates": 106,
    "length": 12061
  },
  {
    "code": "function(c,i){Bs.call(this,c,i)}",
    "duplicates": 94,
    "length": 3283
  },
  {
    "code": "function(){return!0}",
    "duplicates": 93,
    "length": 1850
  },
  {
    "code": "function(r){if(Array.isArray(r))return r}",
    "duplicates": 83,
    "length": 3594
  },
  {
    "code": "function(){throw new TypeError(`Invalid attempt to destructure non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}",
    "duplicates": 83,
    "length": 14022
  },
  {
    "code": "function(){return!1}",
    "duplicates": 79,
    "length": 1585
  },
  {
    "code": "function(){return new cu(this)}",
    "duplicates": 77,
    "length": 2356
  },
  {
    "code": "function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(b,e),Object.defineProperty(E,e,t.get?t:{enumerable:!0,get:function(){return b[e]}}))}",
    "duplicates": 76,
    "length": 11475
  },
  {
    "code": "function(){return b[P]}",
    "duplicates": 76,
    "length": 1725
  },
  {
    "code": "function(a,e,l){var i,r=l[\"aria-label\"],t=l[\"aria-labelledby\"],n=l.title;switch(a){case\"img\":return r||t||n?(s(i={},\"aria-labelledby\",t),s(i,\"aria-label\",r),s(i,\"title\",n),i):{\"aria-label\":\"\".concat(e.replace(/([a-z])([A-Z])/g,\"$1 $2\"),\" Icon\")};case\"presentation\":return{\"aria-hidden\":!0,alt:\"\"}}}",
    "duplicates": 76,
    "length": 24525
  },
  {
    "code": "function(r){var n;return r&&r.__esModule?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}",
    "duplicates": 67,
    "length": 18917
  },
  {
    "code": "function(n){return typeof n}",
    "duplicates": 64,
    "length": 1778
  },
  {
    "code": "function(o){return o&&\"function\"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?\"symbol\":typeof o}",
    "duplicates": 64,
    "length": 7091
  },
  {
    "code": "function n(r){return n(r)}",
    "duplicates": 63,
    "length": 14484
  },
  {
    "code": "function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}",
    "duplicates": 61,
    "length": 13765
  },
  {
    "code": "function(i,t){this.a=i,this.b=t}",
    "duplicates": 58,
    "length": 2027
  },
  {
    "code": "function(r){var n;return r&&\"object\"==typeof r&&\"default\"in r?r:(n=Object.create(null),r&&Object.keys(r).forEach(function(e){var t;\"default\"!==e&&(t=Object.getOwnPropertyDescriptor(r,e),Object.defineProperty(n,e,t.get?t:{enumerable:!0,get:function(){return r[e]}}))}),n.default=r,Object.freeze(n))}",
    "duplicates": 50,
    "length": 15038
  },
  {
    "code": "function(r){if(typeof Symbol<\"u\"&&null!=r[Symbol.iterator]||null!=r[\"@@iterator\"])return Array.from(r)}",
    "duplicates": 49,
    "length": 4988
  },
  {
    "code": "function(){throw new TypeError(`Invalid attempt to spread non-iterable instance.\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.`)}",
    "duplicates": 49,
    "length": 7968
  },
  {
    "code": "function(){return this.a}",
    "duplicates": 49,
    "length": 1200
  },
  {
    "code": "function(i){p0(this,i)}",
    "duplicates": 48,
    "length": 1081
  },
  {
    "code": "function(l,r){var t=null==l?null:typeof Symbol<\"u\"&&l[Symbol.iterator]||l[\"@@iterator\"];if(null!=t){var n,u,e=[],a=!0,o=!1;try{for(t=t.call(l);!(a=(n=t.next()).done)&&(e.push(n.value),!r||e.length!==r);a=!0);}catch(l){o=!0,u=l}finally{try{a||null==t.return||t.return()}finally{if(o)throw u}}return e}}",
    "duplicates": 46,
    "length": 14913
  },
  {
    "code": "function(){throw St(new Ss)}",
    "duplicates": 46,
    "length": 1344
  },
  {
    "code": "function(n){throw St(new Ss)}",
    "duplicates": 39,
    "length": 1102
  },
  {
    "code": "()=>{\"use strict\";Vu(),Du()}",
    "duplicates": 38,
    "length": 1036
  },
  {
    "code": "function(l,r){var t=null==l?null:typeof Symbol<\"u\"&&l[Symbol.iterator]||l[\"@@iterator\"];if(null!=t){var e,n,u,a,f=[],i=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;i=!1}else for(;!(i=(e=u.call(t)).done)&&(f.push(e.value),f.length!==r);i=!0);}catch(l){o=!0,n=l}finally{try{if(!i&&null!=t.return&&(a=t.return(),Object(a)!==a))return}finally{if(o)throw n}}return f}}",
    "duplicates": 37,
    "length": 14888
  },
  {
    "code": "function(){return this.b}",
    "duplicates": 33,
    "length": 800
  },
  {
    "code": "function(){X(x)}",
    "duplicates": 32,
    "length": 496
  },
  {
    "code": "function(n){}",
    "duplicates": 31,
    "length": 400
  },
  {
    "code": "a=>a()",
    "duplicates": 30,
    "length": 180
  },
  {
    "code": "function(n){return V1(n)}",
    "duplicates": 30,
    "length": 725
  },
  {
    "code": "function(n){return dn(oi,mr,2,n,6,1)}",
    "duplicates": 30,
    "length": 1073
  },
  {
    "code": "function(){return null}",
    "duplicates": 29,
    "length": 667
  },
  {
    "code": "function(){return this}",
    "duplicates": 24,
    "length": 537
  },
  {
    "code": "()=>{\"use strict\";Du()}",
    "duplicates": 23,
    "length": 506
  },
  {
    "code": "function(i){g0(this,i)}",
    "duplicates": 23,
    "length": 506
  },
  {
    "code": "function(t,r){var e;if(t)return\"string\"==typeof t?k(t,r):\"Map\"===(e=\"Object\"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)||\"Set\"===e?Array.from(t):\"Arguments\"===e||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(e)?k(t,r):void 0}",
    "duplicates": 22,
    "length": 6111
  },
  {
    "code": "function(t,r){var e;if(t)return\"string\"==typeof t?P(t,r):\"Map\"===(e=\"Object\"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)||\"Set\"===e?Array.from(t):\"Arguments\"===e||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(e)?P(t,r):void 0}",
    "duplicates": 22,
    "length": 6066
  },
  {
    "code": "function(n){return null!=n&&n instanceof Array}",
    "duplicates": 21,
    "length": 946
  },
  {
    "code": "function(r){if(Array.isArray(r))return P(r)}",
    "duplicates": 21,
    "length": 900
  },
  {
    "code": "function(e){return e&&e.__esModule?e:{default:e}}",
    "duplicates": 21,
    "length": 1059
  },
  {
    "code": "function(t,r){var e;if(t)return\"string\"==typeof t?Q(t,r):\"Map\"===(e=\"Object\"===(e=Object.prototype.toString.call(t).slice(8,-1))&&t.constructor?t.constructor.name:e)||\"Set\"===e?Array.from(t):\"Arguments\"===e||/^(?:Ui|I)nt(?:8|16|32)(?:Clamped)?Array$/.test(e)?Q(t,r):void 0}",
    "duplicates": 21,
    "length": 6200
  },
  {
    "code": "function(){throw St(new Os(il((qs(),_g))))}",
    "duplicates": 21,
    "length": 860
  },
  {
    "code": "function(n){return n}",
    "duplicates": 20,
    "length": 432
  },
  {
    "code": "function(n){return null!=n&&n.nodeType===Node.ELEMENT_NODE}",
    "duplicates": 20,
    "length": 1159
  },
  {
    "code": "function(e){throw Error(\"Received unhandled value: \".concat(e))}",
    "duplicates": 20,
    "length": 1216
  },
  {
    "code": "function(r,n){return Array.isArray(r)?r.concat(n):\"string\"==typeof r?r:void 0}",
    "duplicates": 19,
    "length": 1404
  },
  {
    "code": "function(e){return 1==new Set(e).size}",
    "duplicates": 19,
    "length": 684
  },
]
```

`bun`:

```json
[
    {
    "code": "function(){}",
    "duplicates": 739,
    "length": 10883
  },
  {
    "code": "function(i){this.a=i}",
    "duplicates": 250,
    "length": 6059
  },
  {
    "code": "function(r){for(var t=1;t<arguments.length;t++){var n,o=arguments[t];for(n in o)Object.prototype.hasOwnProperty.call(o,n)&&(r[n]=o[n])}return r}",
    "duplicates": 197,
    "length": 28616
  },
  {
    "code": "()=>{}",
    "duplicates": 141,
    "length": 882
  },
  {
    "code": "function(n){return this===n}",
    "duplicates": 119,
    "length": 3540
  },
  {
    "code": "function(c,f){f7.call(this,c,f)}",
    "duplicates": 94,
    "length": 3283
  },
  {
    "code": "function(){return!0}",
    "duplicates": 91,
    "length": 1948
  },
  {
    "code": "function(){return new p9(this)}",
    "duplicates": 77,
    "length": 2508
  },
  {
    "code": "function(){return!1}",
    "duplicates": 76,
    "length": 1647
  },
  {
    "code": "function(i,t){this.a=i,this.b=t}",
    "duplicates": 58,
    "length": 2027
  },
  {
    "code": "function(n,e){if(null==n)return{};for(var r,t={},f=Object.keys(n),u=0;u<f.length;u++)r=f[u],0<=e.indexOf(r)||(t[r]=n[r]);return t}",
    "duplicates": 53,
    "length": 6939
  },
  {
    "code": "function(r,t){if(\"object\"!=typeof r||null===r)return r;var e=r[Symbol.toPrimitive];if(void 0===e)return(\"string\"===t?String:Number)(r);e=e.call(r,t||\"default\");if(\"object\"!=typeof e)return e;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}",
    "duplicates": 51,
    "length": 13270
  },
  {
    "code": "function(){return this.a}",
    "duplicates": 49,
    "length": 1296
  },
  {
    "code": "function(e,r){if(null==e)return{};var t,n=function(e,r){if(null==e)return{};for(var t,n={},l=Object.keys(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)||(n[t]=e[t]);return n}(e,r);if(Object.getOwnPropertySymbols)for(var l=Object.getOwnPropertySymbols(e),o=0;o<l.length;o++)t=l[o],0<=r.indexOf(t)||Object.prototype.propertyIsEnumerable.call(e,t)&&(n[t]=e[t]);return n}",
    "duplicates": 48,
    "length": 17425
  },
  {
    "code": "function(r,e,t){return i=function(r,e){if(\"object\"!=typeof r||null===r)return r;var t=r[Symbol.toPrimitive];if(void 0===t)return String(r);t=t.call(r,e);if(\"object\"!=typeof t)return t;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}(e,\"string\"),(e=\"symbol\"==typeof i?i:String(i))in r?Object.defineProperty(r,e,{value:t,enumerable:!0,configurable:!0,writable:!0}):r[e]=t,r;var i}",
    "duplicates": 48,
    "length": 21550
  },
  {
    "code": "function(t){t=function(t,r){if(\"object\"!=typeof t||null===t)return t;var i=t[Symbol.toPrimitive];if(void 0===i)return String(t);i=i.call(t,r);if(\"object\"!=typeof i)return i;throw new TypeError(\"@@toPrimitive must return a primitive value.\")}(t,\"string\");return\"symbol\"==typeof t?t:String(t)}",
    "duplicates": 48,
    "length": 15739
  },
  {
    "code": "function(i){T6(this,i)}",
    "duplicates": 48,
    "length": 1175
  },
  {
    "code": "()=>{R3(),G3()}",
    "duplicates": 46,
    "length": 675
  },
  {
    "code": "function(){throw x0(new w7)}",
    "duplicates": 46,
    "length": 1390
  },
  {
    "code": "function(e,r){return r=r||e.slice(0),Object.freeze(Object.defineProperties(e,{raw:{value:Object.freeze(r)}}))}",
    "duplicates": 45,
    "length": 4943
  },
  {
    "code": "function(n,r){(null==r||r>n.length)&&(r=n.length);for(var e=0,l=new Array(r);e<r;e++)l[e]=n[e];return l}",
    "duplicates": 41,
    "length": 4158
  },
  {
    "code": "function(n){throw x0(new w7)}",
    "duplicates": 39,
    "length": 1178
  },
  {
    "code": "function(r){if(Array.isArray(r))return r}",
    "duplicates": 36,
    "length": 1469
  },
  {
    "code": "function(){throw new TypeError(\"Invalid attempt to destructure non-iterable instance.\\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.\")}",
    "duplicates": 36,
    "length": 6020
  },
  {
    "code": "function(){return this.b}",
    "duplicates": 33,
    "length": 864
  },
  {
    "code": "function(n){return n2(n)}",
    "duplicates": 30,
    "length": 783
  },
  {
    "code": "function(n){return J1($5,p1,2,n,6,1)}",
    "duplicates": 30,
    "length": 1131
  },
  {
    "code": "function(t,e){var r,n=Object.keys(t);return Object.getOwnPropertySymbols&&(r=Object.getOwnPropertySymbols(t),e&&(r=r.filter(function(e){return Object.getOwnPropertyDescriptor(t,e).enumerable})),n.push.apply(n,r)),n}",
    "duplicates": 29,
    "length": 6179
  },
  {
    "code": "function(l,r){var t=null==l?null:\"undefined\"!=typeof Symbol&&l[Symbol.iterator]||l[\"@@iterator\"];if(null!=t){var e,n,u,f,i=[],a=!0,o=!1;try{if(u=(t=t.call(l)).next,0===r){if(Object(t)!==t)return;a=!1}else for(;!(a=(e=u.call(t)).done)&&(i.push(e.value),i.length!==r);a=!0);}catch(l){o=!0,n=l}finally{try{if(!a&&null!=t.return&&(f=t.return(),Object(f)!==f))return}finally{if(o)throw n}}return i}}",
    "duplicates": 29,
    "length": 11032
  },
  {
    "code": "function(n){}",
    "duplicates": 29,
    "length": 422
  },
  {
    "code": "function(){return null}",
    "duplicates": 28,
    "length": 678
  },
  {
    "code": "function(e){return Object.getOwnPropertyDescriptor(Z,e).enumerable}",
    "duplicates": 25,
    "length": 1608
  },
  {
    "code": "function(e){Object.defineProperty(Z,e,Object.getOwnPropertyDescriptor(W,e))}",
    "duplicates": 25,
    "length": 1824
  },
  {
    "code": "function(n){return n}",
    "duplicates": 23,
    "length": 517
  },
  {
    "code": "function(i){C6(this,i)}",
    "duplicates": 23,
    "length": 550
  },
  {
    "code": "()=>{G3()}",
    "duplicates": 22,
    "length": 210
  },
  {
    "code": "function(){throw x0(new C7(n7((y7(),KG))))}",
    "duplicates": 21,
    "length": 900
  },
  {
    "code": "function(){return this}",
    "duplicates": 19,
    "length": 434
  },
  {
    "code": "function(i,t){this.b=i,this.a=t}",
    "duplicates": 19,
    "length": 639
  },
  {
    "code": "function(n){return!1}",
    "duplicates": 18,
    "length": 384
  },
  {
    "code": "function(n,w){throw x0(new w7)}",
    "duplicates": 18,
    "length": 561
  },
  {
    "code": "function(){return 0}",
    "duplicates": 17,
    "length": 348
  },
  {
    "code": "function(n){return typeof n}",
    "duplicates": 17,
    "length": 458
  },
  {
    "code": "function(o){return o&&\"function\"==typeof Symbol&&o.constructor===Symbol&&o!==Symbol.prototype?\"symbol\":typeof o}",
    "duplicates": 17,
    "length": 1810
  },
  {
    "code": "()=>{R3()}",
    "duplicates": 16,
    "length": 150
  },
  {
    "code": "()=>{var e;return null!=(e=Z.options.debugAll)?e:Z.options.debugHeaders}",
    "duplicates": 16,
    "length": 1065
  },
  {
    "code": "function(n,r){return n}",
    "duplicates": 15,
    "length": 378
  },
  {
    "code": "function(c){hX.call(this,c)}",
    "duplicates": 15,
    "length": 436
  },
  {
    "code": "function(){return this.c}",
    "duplicates": 15,
    "length": 378
  },
  {
    "code": "function(){return this.d}",
    "duplicates": 15,
    "length": 378
  },
  {
    "code": "function(e,n,r){return n in e?Object.defineProperty(e,n,{value:r,enumerable:!0,configurable:!0,writable:!0}):e[n]=r,e}",
    "duplicates": 14,
    "length": 1574
  },
  {
    "code": "function(r){if(\"undefined\"!=typeof Symbol&&null!=r[Symbol.iterator]||null!=r[\"@@iterator\"])return Array.from(r)}",
    "duplicates": 14,
    "length": 1458
  },
  {
    "code": "function(){throw new TypeError(\"Invalid attempt to spread non-iterable instance.\\nIn order to be iterable, non-array objects must have a [Symbol.iterator]() method.\")}",
    "duplicates": 14,
    "length": 2171
  },
  {
    "code": "d=>d.id",
    "duplicates": 14,
    "length": 127
  },
  {
    "code": "()=>W(!1)",
    "duplicates": 13,
    "length": 108
  },
  {
    "code": "a=>a",
    "duplicates": 12,
    "length": 66
  },
  {
    "code": "function(t){var l=-1,n=null==t?0:t.length;for(this.clear();++l<n;){var r=t[l];this.set(r[0],r[1])}}",
    "duplicates": 12,
    "length": 1124
  },
  {
    "code": "function(n,c){}",
    "duplicates": 12,
    "length": 185
  },
  {
    "code": "function(n,r){return r}",
    "duplicates": 11,
    "length": 253
  },
  {
    "code": "function(n,a){n.a=a}",
    "duplicates": 11,
    "length": 238
  },
  {
    "code": "function(){_V.call(this)}",
    "duplicates": 11,
    "length": 282
  },
  {
    "code": "function(i,t,h){this.a=i,this.b=t,this.c=h}",
    "duplicates": 11,
    "length": 470
  },
  {
    "code": "function(){return this.a.gc()}",
    "duplicates": 11,
    "length": 320
  },
  {
    "code": "function(i){this.b=i}",
    "duplicates": 10,
    "length": 214
  },
  {
    "code": "function(c){this.c=c}",
    "duplicates": 10,
    "length": 214
  },
  {
    "code": "function(){return this.f}",
    "duplicates": 10,
    "length": 243
  },
  {
    "code": "a=>!a",
    "duplicates": 10,
    "length": 65
  },
]
```

Interestingly enough, all three tools handled the job bad in different aspects:

* vite was the slowest and produced second biggest bundle
* esbuild was the fastest and produced the largest bundle
* bun was slower than esbuild by a split of hair, but produced smallest bundle with least duplicates

Bonus points to bun for installing node modules in a link of an eye.

In terms of duplicates, however, all three failed miserably (in my opinion), with the best result being the bundle
produced by bun with 18% duplicates and the rest having almost half the bundle wasted.
