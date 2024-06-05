---
layout: post
title: Eliminating promise hell
categories: []
tags: []
published: True

---

What I had:

```js
var mongoose = require('mongoose');

mongoose.connect('mongodb://localhost/lunch-ordering');

var Schema = mongoose.Schema,
    ObjectId = Schema.ObjectId;

var RestaurantSchema = new Schema({
    name: String,
    delivery_at: String,
    phone: String,
    delivery_days: [String],
    dishes: [{
        name: String,
        category: String
    }]
});

RestaurantSchema.static('forDay', function (day) {
    return this.findOne({ 'delivery_days': { '$in': [ day ] } });
});

RestaurantSchema.static('today', function () {
    var todayDay = 'tuesday';

    return this.forDay(todayDay);
});

var Restaurant = mongoose.model('Restaurant', RestaurantSchema);

var UserSchema = new Schema({
    name: String,
    orders: [{
        created_at: String,
        dishes: [{
            name: String,
            category: String,
            amount: Number
        }]
    }]
});

UserSchema.static('current', function () {
    return this.findOne();
});

UserSchema.method('createOrder', function () {
    var user = this;

    return Restaurant.today().exec()
        .then(function (restaurant) {
            var dishes = restaurant.dishes.map(function (dish) {
                var dish2 = {
                    name: dish['name'],
                    amount: 0,
                    category: dish['category']
                };

                return dish2;
            });

            return dishes;
        })
        .then(function (dishes) {
            var today = '2016-01-13';
            var newOrder = {
                created_at: today,
                dishes: dishes
            };

            return new Promise(function (resolve, reject) {
                user.update({ '$push': { 'orders': newOrder } }).then(function () {
                    resolve(newOrder);
                });
            });
        });
});

UserSchema.method('todayOrder', function () {
    var today = '2016-01-13';
    var user = this;

    return User.findOne({ '_id': this.id, 'orders.created_at': today }, { 'orders': 1 }).exec()
        .then(function (order) {
            return new Promise(function (resolve, reject) {
                if (!order) {
                    resolve(user.createOrder());
                } else {
                    resolve(order);
                }
            });
        });
});

var User = mongoose.model('User', UserSchema);
```

It's still better than callbacks, but with all those promises returning promises... Uggghhh...

So what I did is used ES7 `async/await` feature! Yeah, that requires a bit of configuration:

```bash
npm install --save-dev babel-cli babel-preset-es2015 babel-preset-stage-0 bluebird
```

`.babelrc` file:

```json
{
  "presets": [ "es2015", "stage-0" ],
  "plugins": [ "syntax-async-functions" ]
}
```

and running with:

```bash
./node_modules/.bin/babel-node app.js
```

The new code looks like this:

```js
var Promise = require('bluebird');

mongoose.Promise = Promise;

UserSchema.static('current', async function () {
    return await this.findOne().exec();
});

UserSchema.method('createOrder', async function () {
    let restaurant = await Restaurant.today().exec();

    let dishes = restaurant.dishes.map((dish) => {
                var dish2 = {
                    name: dish['name'],
                    amount: 0,
                    category: dish['category']
                };

                return dish2;
            });

    let today = '2016-01-13';
    let newOrder = {
        created_at: today,
        dishes: dishes
    };

    await (this.update({ '$push': { 'orders': newOrder } }));

    return newOrder;
});

UserSchema.method('todayOrder', async function () {
    let today = '2016-01-13';

    var order = await (User.findOne({ '_id': this.id, 'orders.created_at': today }, { 'orders': 1 }).exec());

    if (!order) {
        order = await (this.createOrder());
    }

    return order;
});
```