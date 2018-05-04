window.addEventListener('load', function () {
    setTimeout(function () {
        var lightbox = document.querySelector('.lightbox');

        lightbox.addEventListener('click', function () {
            lightbox.classList.remove('visible');
        });

        var images = [].slice.apply(document.querySelectorAll('.carousel-item > img'));

        images.forEach(function (image) {
            var src = image.getAttribute('data-photo-src');
            var previewSrc = src.replace(/^(.+)(\.\w{3,7})$/, '$1-preview$2');

            image.setAttribute('src', previewSrc);

            image.addEventListener('click', function () {
                var lightboxImg = lightbox.querySelector('img');

                lightboxImg.setAttribute('src', src);

                lightboxImg.addEventListener('load', function () {
                    lightbox.classList.add('visible');
                });
            });
        });
    }, 0);
});
