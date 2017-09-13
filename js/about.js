window.addEventListener('DOMContentLoaded', function() {
    var mymap = L.map('map').setView([ 50.0646501, 19.9449799 ], 3);

    L.tileLayer('http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png', {
        attribution: 'Map data &copy; <a href="http://openstreetmap.org">OpenStreetMap</a>',
        maxZoom: 18,
    }).addTo(mymap);

    /*
        const api_key = 'AIzaSyD0SRAAItWw5W3nU6sUOx45QOPguaXd3Cc';
        const addresses = [ "Paris, France", "Marsaxlokk, Malta", "Valletta, Malta", "Azure Window, Malta", "Bugibba, Malta", "St. Peter's Pool, Malta", "Las Palmas, Spain", "Maspalomas, Spain", "Rome, Italy", "Venice, Italy", "Florence, Italy", "Pisa, Italy", "Bologna, Italy", "Milan, Italy", "Lisbon, Portugal", "Sintra, Portugal", "Cascais, Portugal", "Cabo da Roca, Portugal", "Prague, Czech Republic", "Vienna, Austria", "Budapest, Hungary", "Zvolen, Slovakia", "Liptovsky Mikulas, Slovakia", "Banska Bystrica, Slovakia", "Brussells, Belgium", "Trolltunga, Norway", "Odda, Norway", "Oslo, Norway", "Latefossen, Norway", "Stockholm, Sweden", "Krakow, Poland", "Katowice, Poland", "Zakopane, Poland", "Gdansk, Poland", "Sopot, Poland", "Gdynia, Poland", "Gdynia, Poland", "Lviv, Ukraine", "Kamyanets-Podilski, Ukraine", "Sumy, Ukraine", "Vinnytsya, Uktaine", "Kharkiv, Ukraine", "Kyiv, Ukraine", "Zhytomyr, Ukraine" ];
        const data = Promise.all(
            addresses.map(addr =>
                fetch(`https://maps.googleapis.com/maps/api/geocode/json?address=${addr}&key=${api_key}`)
                    .then(res => res.json())
                    .then(json => json.results[0].geometry.location)
                    .then(loc => Object.assign({}, loc, { popup: addr })))
            )
            .then(results => console.log(JSON.stringify(results)))
        */

    var data = [
        {
            'lat': 48.856614,
            'lng': 2.3522219,
            'popup': 'Paris, France'
        },
        {
            'lat': 35.8411699,
            'lng': 14.5393097,
            'popup': 'Marsaxlokk, Malta'
        },
        {
            'lat': 35.8989085,
            'lng': 14.5145528,
            'popup': 'Valletta, Malta'
        },
        {
            'lat': 36.0524077,
            'lng': 14.1904596,
            'popup': 'Azure Window, Malta'
        },
        {
            'lat': 35.9490844,
            'lng': 14.4097459,
            'popup': 'Bugibba, Malta'
        },
        {
            'lat': 35.8331367,
            'lng': 14.5622043,
            'popup': 'St. Peter\'s Pool, Malta'
        },
        {
            'lat': 28.1235459,
            'lng': -15.4362574,
            'popup': 'Las Palmas, Spain'
        },
        {
            'lat': 27.7605619,
            'lng': -15.5860172,
            'popup': 'Maspalomas, Spain'
        },
        {
            'lat': 41.9027835,
            'lng': 12.4963655,
            'popup': 'Rome, Italy'
        },
        {
            'lat': 45.4408474,
            'lng': 12.3155151,
            'popup': 'Venice, Italy'
        },
        {
            'lat': 43.7695604,
            'lng': 11.2558136,
            'popup': 'Florence, Italy'
        },
        {
            'lat': 43.7228386,
            'lng': 10.4016888,
            'popup': 'Pisa, Italy'
        },
        {
            'lat': 44.494887,
            'lng': 11.3426162,
            'popup': 'Bologna, Italy'
        },
        {
            'lat': 45.4642035,
            'lng': 9.189982,
            'popup': 'Milan, Italy'
        },
        {
            'lat': 38.7222524,
            'lng': -9.1393366,
            'popup': 'Lisbon, Portugal'
        },
        {
            'lat': 38.8029127,
            'lng': -9.3816495,
            'popup': 'Sintra, Portugal'
        },
        {
            'lat': 38.6967571,
            'lng': -9.4207438,
            'popup': 'Cascais, Portugal'
        },
        {
            'lat': 38.780417,
            'lng': -9.498885,
            'popup': 'Cabo da Roca, Portugal'
        },
        {
            'lat': 50.0755381,
            'lng': 14.4378005,
            'popup': 'Prague, Czech Republic'
        },
        {
            'lat': 48.2081743,
            'lng': 16.3738189,
            'popup': 'Vienna, Austria'
        },
        {
            'lat': 47.497912,
            'lng': 19.040235,
            'popup': 'Budapest, Hungary'
        },
        {
            'lat': 48.5761806,
            'lng': 19.1371155,
            'popup': 'Zvolen, Slovakia'
        },
        {
            'lat': 49.0811487,
            'lng': 19.6192067,
            'popup': 'Liptovsky Mikulas, Slovakia'
        },
        {
            'lat': 48.736277,
            'lng': 19.1461917,
            'popup': 'Banska Bystrica, Slovakia'
        },
        {
            'lat': 50.8503463,
            'lng': 4.3517211,
            'popup': 'Brussells, Belgium'
        },
        {
            'lat': 60.124167,
            'lng': 6.74,
            'popup': 'Trolltunga, Norway'
        },
        {
            'lat': 60.06760360000001,
            'lng': 6.5473276,
            'popup': 'Odda, Norway'
        },
        {
            'lat': 59.9138688,
            'lng': 10.7522454,
            'popup': 'Oslo, Norway'
        },
        {
            'lat': 59.9478293,
            'lng': 6.584262,
            'popup': 'Latefossen, Norway'
        },
        {
            'lat': 59.32932349999999,
            'lng': 18.0685808,
            'popup': 'Stockholm, Sweden'
        },
        {
            'lat': 50.06465009999999,
            'lng': 19.9449799,
            'popup': 'Krakow, Poland'
        },
        {
            'lat': 50.26489189999999,
            'lng': 19.0237815,
            'popup': 'Katowice, Poland'
        },
        {
            'lat': 49.299181,
            'lng': 19.949562,
            'popup': 'Zakopane, Poland'
        },
        {
            'lat': 54.35202520000001,
            'lng': 18.6466384,
            'popup': 'Gdansk, Poland'
        },
        {
            'lat': 54.441581,
            'lng': 18.5600956,
            'popup': 'Sopot, Poland'
        },
        {
            'lat': 54.5188898,
            'lng': 18.5305409,
            'popup': 'Gdynia, Poland'
        },
        {
            'lat': 49.41919919999999,
            'lng': 20.3863415,
            'popup': 'Pieniny, Poland'
        },
        {
            'lat': 50.2070516,
            'lng': 19.8178762,
            'popup': 'Ojcowski Park Narodowy, Poland'
        },
        {
            'lat':49.839683,
            'lng':24.029717,
            'popup':'Lviv, Ukraine'
        },
        {
            'lat': 48.6967162,
            'lng': 26.5825364,
            'popup': 'Kamyanets-Podilski, Ukraine'
        },
        {
            'lat': 50.9077,
            'lng': 34.7981,
            'popup': 'Sumy, Ukraine'
        },
        {
            'lat': 49.233083,
            'lng': 28.4682169,
            'popup': 'Vinnytsya, Uktaine'
        },
        {
            'lat': 49.9935,
            'lng': 36.230383,
            'popup': 'Kharkiv, Ukraine'
        },
        {
            'lat': 50.4501,
            'lng': 30.5234,
            'popup': 'Kyiv, Ukraine'
        },
        {
            'lat': 50.25465,
            'lng': 28.6586669,
            'popup': 'Zhytomyr, Ukraine'
        },
        {
            'lat': 45.19045,
            'lng': 33.366867,
            'popup': 'Evpatoria, Crimea, Ukraine'
        },
        {
            'lat': 52.2296756,
            'lng': 21.0122287,
            'popup': 'Warszawa, Poland'
        },
        {
            'lat': 50.3172235,
            'lng': 29.0541431,
            'popup': 'Korostyshiv, Ukraine'
        },
        {
            'lat': 50.4965282,
            'lng': 29.2337369,
            'popup': 'Radomyshl, Ukraine'
        },
        {
            'lat': 51.7592485,
            'lng': 19.4559833,
            'popup': 'Lodz, Poland'
        }
    ];

    data.forEach(function (entry) {
        var marker = L.marker([entry.lat, entry.lng]).addTo(mymap);
        marker.bindPopup(entry.popup);
    });
});