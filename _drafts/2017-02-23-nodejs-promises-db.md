---
layout: post
title: NodeJS promises + DB
---

{% highlight js %}
class ResultSet {
    operation() {
        console.log('ResultSet operation');
    }
}

class Connection {
    operation() {
        return new Promise(function (resolve, reject) {
            resolve(new ResultSet());
        });
    }

    close() {
        console.log('Closing DB connection');
    }
}

class DBClient {
    static connect() {
        return new Promise(function (resolve, reject) {
            resolve(new Connection());
        });
    }
}

DBClient.connect().then(function (connection) {
    return connection.operation();
}).then(function (results) {
    return results.operation();
}).then(function (connection) {
    connection.close(); // whoops!
});

DBClient.connect().then(function (connection) {
    return connection.operation()
        .then(function (results) {
            return results.operation();
        });
}).then(function (connection) {
    connection.close(); // whoops!
});

DBClient.connect().then(function (connection) {
    return new Promise(function (resolve, reject) {
        connection.operation()
            .then(function (results) {
                return results.operation();
            })
            .then(function () {
                resolve(connection);
            });
    });
}).then(function (connection) {
    connection.close(); // now it works
});
{% endhighlight %}
