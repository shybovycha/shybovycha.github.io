```js
function ClassA (name) {
    return {
        name: name,
        welcome: function () {
            console.log('Hello', this.name, '!');
        }
    };
}

function ClassB (name, age) {
    var instA = new ClassA(name);

    instA.age = age;

    instA.howOld = function () {
        return this.age;
    };

    instA.welcome = function () {
        return 'Hello, ' + this.name + '!';
    };

    return instA;
}

// the same with ES6

class ClassA {
    constructor (name) {
        this.name = name;
    }

    welcome () {
        console.log('Hello, ', this.name, '!');
    }
}

class ClassB extends ClassA {
    constructor (name, age) {
        this.age = age;

        super(name);
    }

    howOld () {
        return this.age;
    }
}
```