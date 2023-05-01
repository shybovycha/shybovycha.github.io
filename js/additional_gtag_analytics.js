window.addEventListener('load', () => {
    const seenMarkers = new Set();

    const onIntersect = (entries, observer1) => entries.forEach(entry => {
        if (!entry.isIntersecting) {
            return;
        }

        const elt = entry.target;
        const fraction = parseInt(elt.dataset.fraction);

        const previousMarkers = [25, 50, 75, 100].filter(p => p < fraction);
        const seenPreviousMarkers = previousMarkers.every(m => seenMarkers.has(m));
        const marker = seenPreviousMarkers ? `content_read_${fraction}` : `content_jumped_${fraction}`;

        seenMarkers.add(fraction);

        observer1.unobserve(elt);

        gtag && gtag('event', marker);
    });

    const observer = new IntersectionObserver(onIntersect, {
        margin: '40px 0',
        threshold: 0.01,
    });

    const markers = Array.from(document.querySelectorAll('.content-read-marker'));

    markers.forEach(marker => observer.observe(marker));
});
