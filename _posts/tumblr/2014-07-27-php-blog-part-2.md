---
layout: post
title: "Чоткій блог. Часть друга"
date: '2014-07-27T23:20:00+02:00'
tags:
- php
- rtfm
- programming
tumblr_url: http://shybovycha.tumblr.com/post/92565628101
---

<div class="row">
    <div class="col-md-6 col-xs-12">
        {% include references/bro-blog.html %}
    </div>

    <div class="col-md-6 col-xs-12 text-xs-center text-md-right"></div>
</div>

## Моделі

Єсть така штука як **MVC, Model-View-Controller**. Це такий прінцип, по якому програма розділяється на три тіпа дєталєй:

* `Model` - отвічає за роботу чісто з базой даних
* `View` - отвічає чісто за отображеніє даних перед пользоватєльом (формочка, HTML, і так далєє)
* `Controller` - отвічає за обработку дєйствій пользоватєля, іспользує моделі і передає їх в’юхам

Так от, модель - це такій клас, який работає з рядочками одної таблички і другими табличками, шо прив’язані до нашої. Але тільки якшо та таблічка, з якою работає модель - главна в цій связі. Тоість, якшо у нас є модель <strong>Пост</strong> і модель <strong>Камєнтік</strong>, то модель <strong>Пост</strong> може вліять на <strong>Камєнтікі</strong>, а от модель <strong>Камєнтік</strong> вже нічо не може зробити з <strong>Постом</strong>. Тут таблічка <strong>Пост</strong> - главна, а таблічка <strong>Камєнтік</strong> - просто прив’язана до неї.

В общєм, модель - це такий удобний клас, в якому заникані всі запроси до бази даних. Ти визиваєш метод моделі, а получаєш - масив (або не масив, а тільки один його елємєнт) з запісями з бази даних. Або визиваєш другий мєтод і удаляєш/обновляєш/создаєш рядочки в базі.

<!--more-->

Шоб зробити цей клас унівєрсальним до невозможності, представим, шо <strong>класс</strong> отвічає за роботу з всьою табличкою (вибор рядочків з таблички), а <strong>об’єкт цього класа</strong> отвічає за роботу з конкретним рядочком з цеї таблички. Робота з цим класом буде виглядіть якось так:

```php
// всі пости
$postings = Posting::all();

// один конкретний пост
$posting = Posting::find($id);

// не до кінця обновлений пост (в базі ще не сохраньон)
$posting->title = "moo";

// сохранить пост в базі
$posting->save();

// удалить пост
$posting->destroy();
```

Сама проста реалізація моделі буде схожа на оце:

```php
class Posting {
    var $id, $title, $description;

    function __construct($title, $description) {
        $this->title = $title;
        $this->description = $description;
    }

    function __construct($id, $title, $description) {
        $this->id = $id;
        $this->title = $title;
        $this->description = $description;
    }

    public static function all() {
        $results = array();
        $rows = mysql_query("SELECT id, title, description FROM postings;");

        while ($posting = mysql_fetch_assoc($rows)) {
            $results[] = new Posting($posting['id'], $posting['title'], $posting['description']);
        }

        return $results;
    }

    public static function find($id) {
        $row = mysql_fetch_assoc(mysql_query("SELECT id, title, description FROM postings WHERE id = $id"));

        if (isset($row['id'])) {
          return new Posting($row['id'], $row['title'], $row['description']);
        } else {
            return null;
        }
    }

    public function save() {
        if (isset($this->id)) {
            mysql_query("UPDATE postings SET title = '$this->title', description = '$this->description' WHERE id = '$this->id'");
        } else {
            mysql_query("INSERT INTO postings (id, title, description) VALUES ('$this->id', '$this->title', '$this->description')");
        }
    }

    public function delete() {
        if (isset($this->id)) {
            mysql_query("DELETE FROM postings WHERE id = '$this->id'");
        }
    }
};
```

Але з таким ращотом, у нас для кожної таблички буде оддєльний клас і оддєльний набор запросов. І вспоминаєм, шо ми рішили не дублірувать код. Будем іспользувать мощ магічних методів PHP!

Нехай ми не знаєм, які поля єсть в табличці. Но всі запроси ці, `SELECT/DELETE` - вони в основном одінакові і відрізняються тільки для `INSERT/UPDATE`. Того нам нада так переписать ці `INSERT/UPDATE` і цей клас, шоб вони работали з любими табличками.

Воспользуємся масивами PHP і магічними методами `__set/__get`. Ці методи дають можливість описать свою логіку коли програміст записує чи читає неізвєсне поле класа. Напрімєр, якшо у нашого класа `Post` нема поля `moo`, то при попитці зробить `echo $posting->moo` буде визиватись мєтод `__get('moo')`, а при попитці записать шось туди `$posting->moo = 'MOO!'` буде визваний `__set('moo', 'MOO!')`. Приступаєм:

```php
class BaseModel {
    private $__data, $__tableName;

    private static function getTableName() {
        $class_name = get_called_class();
        return strtolower($class_name) . 's';
    }

    function __construct($data) {
        foreach ($data as $key => $value) {
            $this->__data[$key] = $value;
        }

        $__tableName = $this->getTableName();
    }

    function __get($field) {
        if (isset($this->__data[$field])) {
            return $this->__data[$field];
        } else {
            return null;
        }
    }

    function __set($field, $value) {
        $this->__data[$field] = $value;
    }

    public static function all() {
        $tableName = self::getTableName();
        $class = get_called_class();

        $results = array();

        $rows = mysql_query("SELECT * FROM $tableName;");

        while ($row = mysql_fetch_assoc($rows)) {
            $results[] = new $class($row);
        }

        return $results;
    }

    public static function find($id) {
        $row = mysql_fetch_assoc(mysql_query("SELECT * FROM $tableName WHERE id = $id"));

        if (isset($row['id'])) {
          return new BaseModel($row);
        } else {
            return null;
        }
    }

    public function save() {
        $query = "";

        if (isset($this->id)) {
            $fields = array();

            foreach ($this->__data as $field => $value) {
                if ($field == 'id') {
                    continue;
                }

                $fields[] = "`$field` = '$value'";
            }

            $query = "UPDATE $this->__tableName SET " . implode(', ', $fields) . " WHERE id = '$this->id'";
        } else {
            $values = array();

            foreach ($this->__data as $field => $value) {
                $values[$field] = "'$value'";
            }

            $query = "INSERT INTO $this->__tableName (" . implode(', ', array_keys($values)) . ") VALUES (" . implode(', ', array_values($values)) . ")";
        }

        mysql_query($query);
    }

    public function delete() {
        if (isset($this->id)) {
            mysql_query("DELETE FROM $this->__tableName WHERE id = '$this->id'");
        }
    }
};
```

Так шо ж за ад тут коїться?

По-перше, цей клас - базовий. Всі слєдующі модельки ми будем наслєдувать від нього. Тоість, весь цей ад у нас коїться тільки один раз - дальше всі моделі виглядять приблизно так:

```php
class Posting extends BaseModel {
    public function getViewUrl() {
        return "view.php?$this->id";
    }
};
```

А іспользовать такі моделі тепер - сплошне удовольствіє!

```php
<?php $postings = Posting::all(); ?>

...

<?php foreach ($postings as $posting) ?>
    <h2><?php echo $posting->title ?></h2>

    <div class="description">
        <?php echo $posting->description ?>
    </div>

    <div class="read-more">
        <?php echo $posting->getViewUrl() ?>
    </div>
<?php endforeach; ?>
```

По-друге, в конструктор ми передаєм (всігда) поля з таблички, а конструктор - записує їх в масив `__data`. Ну і в конструкторі ж ми дуже хітро достаєм імя таблички з імєні класа: якшо у нас, напрімєр, клас моделі `Posting`, то ім’я таблички буде `postings`.

Ще один ньюанс з приводу імені таблички і того шо ми вертаєм в мєтоді `all()`: тут іспользується така хітра функція `get_called_class()`, яка вертає ім’я класа, який визвав поточний мєтод. Воно працює і з наслєдованієм в ООП, тоість якшо запустити отакє:

```php
class Moo {
    function whoami() {
        echo get_called_class();
    }
}

class Foo extends Moo {
}

$a = new Moo();
$a->whoami();

$b = new Foo();
$b->whoami();
```

то воно виведе:

```
Moo
Foo
```

І якшо ми захочем зробить об’єкт дочєрнього класа внутрі батьківського, то ми можем спокойно визвать

```php
$class = get_called_class();
new $class(...);
```

Ну і всі запроси до бази даних строяться вже іспользуя поля `$__data` і `$__tableName`.

А тепер трошки поправим файлову структуру: у нас тепер є кілька файлів, які не відносяться конкретно до цього блога, а, так сказать, єсть каркасом, на якому можна наліпити ше шось. Це `BaseModel.php` і `connect.php`. Точніше, `connect.php` - в ньому задаються параметри подключєнія до бази даних конкретно цього блога. Шо я предлагаю - зробити три папки:

* `core`, в якому буде `BaseModel.php`
* `models`, куди положити `Posting.php` і всі прочі моделі, які наслєдують `BaseModel`
* `config`, куди положить `connect.php`

Не забудь поправити всі путі у `require()`!

Замєть, тепер файл `connect.php` нам треба тільки в одному місці - в `BaseModel.php`. Шо тоже кагбе упрощає код.

Тоість, тепер у нас має бути якась така файлова структура:

<img alt="чотка файлова структура" src="https://31.media.tumblr.com/a1800a7221cf2a424d042a2ae3349fdd/tumblr_inline_n94w90fOhS1qh5oee.png"/>

## Домашнє завдання

Зроби моделі поста і камєнтіка і застав їх работать внутрі `index.php` і `view.php`. Замєть, шо в бєзопасності `BaseModel` єсть дірка. А ще - і це <strong>дуже</strong> важно - подивись шо таке <strong>PDO</strong> і зроби замість всіх `mysql_query` і прочіх роботу з <strong>PDO</strong>.
