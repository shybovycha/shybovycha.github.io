---
layout: post
title: SMD components cheatsheet
---

This is a quick compilation of handy cheatsheets I found while picking electronic components for my DIY project.

## Capacitor codes

The most common capacitor code uses a first digit, second digit, and multiplier scheme:

```
<digit1><digit2><power><tolerance>
```

To decode the capacitor value, you take a number defined by two digits, `<digit1><digit2>` and multiply it by `10` to power `<power>` and tat would give you capacitance in **pico Farads**.

For example:

```
223J = 22 * 10^3 pF = 22000 pF = 22 nF = 0.022 uF; +/-5%
151K = 15 * 10^1 pF = 150 pF = 0.15 nF = 0.00015 uF; +/- 10%
104J = 10 * 10^4 pF = 100000 pF = 100 nF = 0.1 uF; +/- 5%
```

The `<tolerance>` code is defined by a single letter:

| Code | Tolerance   |
| ---- | ----------- |
| F    | 1%          |
| G    | 2%          |
| J    | 5%          |
| K    | 10%         |
| M    | 20%         |
| Z    | +80% / -20% |

Sometimes the code might include a letter denoting the multiplier:

| Prefix | Abbreviation | Multiplier |
| ------ | ------------ | ---------- |
| pico   | p            | 10^-12     |
| nano   | n            | 10^-9      |
| micro  | μ            | 10^-6      |

Handy decoder:

<input id="capacitor_code" type="number" placeholder="Enter capacitor code" />
<button id="capacitor_calculate">calculate</button>
<div id="capacitor_value"></div>
<script>
(() => {
document.querySelector('#capacitor_calculate').addEventListener('click', () => {
const s = document.querySelector('#capacitor_code').value;
const pf = +(s.substr(0,2)) * Math.pow(10, s.substr(2,1));
const nf = pf / 1000;
const uf = nf / 1000;
document.querySelector('#capacitor_value').innerHTML = `${pf}pF = ${nf}nF = ${uf}uF`;
});
})();
</script>

## SMD package power rating

Different component packages have different power rating. For SMD components they are approximate:

| Package | Power rating |
| ------- | ------------ |
| 0201    | 1/20 W       |
| 0402    | 1/16 W       |
| 0603    | 1/16 W       |
| 0805    | 1/10 W       |
| 1206    | 1/8 W        |
| 1210    | 1/4 W        |
| 1812    | 1/3 W        |
| 2010    | 1/2 W        |
| 2512    | 1 W          |
