Array.from(document.querySelectorAll('link[rel="preload"][as="style"]'))
    .forEach(elt => elt.onload = () => elt.setAttribute('rel', 'stylesheet'));
