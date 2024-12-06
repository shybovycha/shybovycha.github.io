window.addEventListener('load', () => {
    setTimeout(() => {
        let lightbox = document.querySelector('.lightbox');
        let lightboxImg;

        if (!lightbox) {
            lightbox = document.createElement('div');
            lightbox.className = 'lightbox';
            document.body.appendChild(lightbox);

            lightboxImg = document.createElement('img');
            lightbox.appendChild(lightboxImg);
        } else {
            lightboxImg = lightbox.querySelector('img');
        }

        lightbox.addEventListener('click', (e) => {
            lightbox.classList.remove('visible');
        });

        const images = Array.from(document.querySelectorAll('.carousel-item > img'));

        images.forEach((image) => {
            const src = image.getAttribute('data-photo-src');
            const previewSrc = src.replace(/^(.+)(\.\w{3,7})$/, '$1-preview$2');

            image.setAttribute('src', previewSrc);

            image.addEventListener('click', () => {
                const lightboxImg = lightbox.querySelector('img');

                lightboxImg.setAttribute('src', src);

                lightboxImg.addEventListener('load', function () {
                    lightbox.classList.add('visible');
                });
            });
        });
    }, 0);
});
