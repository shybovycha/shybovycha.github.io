---
layout: post
title: "Чоткій блог. Часть друга"
date: '2014-07-27T23:20:00+02:00'
tags:
- php
- tutorial
- programming
tumblr_url: http://shybovycha.tumblr.com/post/92565628101
---

<div class="row">
    <div class="col-md-6 col-xs-12">
        <div class="card">
            <div class="card-header">
                Содєржаніє
            </div>
            <ul class="list-group list-group-flush">
                <li class="list-group-item">
                    <a href="/tumblr/2014/07/22/php-blog-part-1.html">
                        Чоткій блог. Часть перша
                    </a>
                </li>
                <li class="list-group-item">
                    <a href="/tumblr/2014/07/27/php-blog-part-2.html">
                        Чоткій блог. Часть друга
                    </a>
                </li>
                <li class="list-group-item">
                    <a href="/tumblr/2014/07/28/php-blog-part-3.html">
                        Чоткій блог. Часть третя
                    </a>
                </li>
            </ul>
        </div>
    </div>
    <div class="col-md-6 col-xs-12 text-xs-center text-md-right"></div>
</div>

## Моделі

<p>Єсть така штука як <strong>MVC, Model-View-Controller</strong>. Це такий прінцип, по якому програма розділяється на три тіпа дєталєй:</p>

<ol><li><em>Model</em> - отвічає за роботу чісто з базой даних</li>
<li><em>View</em> - отвічає чісто за отображеніє даних перед пользоватєльом (формочка, HTML, і так далєє)</li>
<li><em>Controller</em> - отвічає за обработку дєйствій пользоватєля, іспользує моделі і передає їх в’юхам</li>
</ol><p>Так от, модель - це такій клас, який работає з рядочками одної таблички і другими табличками, шо прив’язані до нашої. Але тільки якшо та таблічка, з якою работає модель - главна в цій связі. Тоість, якшо у нас є модель <strong>Пост</strong> і модель <strong>Камєнтік</strong>, то модель <strong>Пост</strong> може вліять на <strong>Камєнтікі</strong>, а от модель <strong>Камєнтік</strong> вже нічо не може зробити з <strong>Постом</strong>. Тут таблічка <strong>Пост</strong> - главна, а таблічка <strong>Камєнтік</strong> - просто прив’язана до неї.</p>

<p>В общєм, модель - це такий удобний клас, в якому заникані всі запроси до бази даних. Ти визиваєш метод моделі, а получаєш - масив (або не масив, а тільки один його елємєнт) з запісями з бази даних. Або визиваєш другий мєтод і удаляєш/обновляєш/создаєш рядочки в базі.</p>

<!--more-->

<p>Шоб зробити цей клас унівєрсальним до невозможності, представим, шо <strong>класс</strong> отвічає за роботу з всьою табличкою (вибор рядочків з таблички), а <strong>об’єкт цього класа</strong> отвічає за роботу з конкретним рядочком з цеї таблички. Робота з цим класом буде виглядіть якось так:</p>

```php
// всі пости
$postings = Posting::all();

// один конкретний пост
$posting = Posting::find($id);

// не до кінця обновлений пост (в базі ще не сохраньон)
$posting-&gt;title = "moo";

// сохранить пост в базі
$posting-&gt;save();

// удалить пост
$posting-&gt;destroy();
```

<p>Сама проста реалізація моделі буде схожа на оце:</p>

```php
class Posting {
    var $id, $title, $description;

    function __construct($title, $description) {
        $this-&gt;title = $title;
        $this-&gt;description = $description;
    }

    function __construct($id, $title, $description) {
        $this-&gt;id = $id;
        $this-&gt;title = $title;
        $this-&gt;description = $description;
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
        if (isset($this-&gt;id)) {
            mysql_query("UPDATE postings SET title = '$this-&gt;title', description = '$this-&gt;description' WHERE id = '$this-&gt;id'");
        } else {
            mysql_query("INSERT INTO postings (id, title, description) VALUES ('$this-&gt;id', '$this-&gt;title', '$this-&gt;description')");
        }
    }

    public function delete() {
        if (isset($this-&gt;id)) {
            mysql_query("DELETE FROM postings WHERE id = '$this-&gt;id'");
        }
    }
};
```

<p>Але з таким ращотом, у нас для кожної таблички буде оддєльний клас і оддєльний набор запросов. І вспоминаєм, шо ми рішили не дублірувать код. Будем іспользувать мощ магічних методів PHP!</p>

<p>Нехай ми не знаєм, які поля єсть в табличці. Но всі запроси ці, <code>SELECT/DELETE</code> - вони в основном одінакові і відрізняються тільки для <code>INSERT/UPDATE</code>. Того нам нада так переписать ці <code>INSERT/UPDATE</code> і цей клас, шоб вони работали з любими табличками.</p>

<p>Воспользуємся масивами PHP і магічними методами <code>__set/__get</code>. Ці методи дають можливість описать свою логіку коли програміст записує чи читає неізвєсне поле класа. Напрімєр, якшо у нашого класа <code>Post</code> нема поля <code>moo</code>, то при попитці зробить <code>echo $posting-&gt;moo</code> буде визиватись мєтод <code>__get('moo')</code>, а при попитці записать шось туди <code>$posting-&gt;moo = 'MOO!'</code> буде визваний <code>__set('moo', 'MOO!')</code>. Приступаєм:</p>

```php
class BaseModel {
    private $__data, $__tableName;

    private static function getTableName() {
        $class_name = get_called_class();
        return strtolower($class_name) . 's';
    }

    function __construct($data) {
        foreach ($data as $key =&gt; $value) {
            $this-&gt;__data[$key] = $value;
        }

        $__tableName = $this-&gt;getTableName();
    }

    function __get($field) {
        if (isset($this-&gt;__data[$field])) {
            return $this-&gt;__data[$field];
        } else {
            return null;
        }
    }

    function __set($field, $value) {
        $this-&gt;__data[$field] = $value;
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

        if (isset($this-&gt;id)) {
            $fields = array();

            foreach ($this-&gt;__data as $field =&gt; $value) {
                if ($field == 'id') {
                    continue;
                }

                $fields[] = "`$field` = '$value'";
            }

            $query = "UPDATE $this-&gt;__tableName SET " . implode(', ', $fields) . " WHERE id = '$this-&gt;id'";
        } else {
            $values = array();

            foreach ($this-&gt;__data as $field =&gt; $value) {
                $values[$field] = "'$value'";
            }

            $query = "INSERT INTO $this-&gt;__tableName (" . implode(', ', array_keys($values)) . ") VALUES (" . implode(', ', array_values($values)) . ")";
        }

        mysql_query($query);
    }

    public function delete() {
        if (isset($this-&gt;id)) {
            mysql_query("DELETE FROM $this-&gt;__tableName WHERE id = '$this-&gt;id'");
        }
    }
};
```

<p>Так шо ж за ад тут коїться?</p>

<p>По-перше, цей клас - базовий. Всі слєдующі модельки ми будем наслєдувать від нього. Тоість, весь цей ад у нас коїться тільки один раз - дальше всі моделі виглядять приблизно так:</p>

```class Posting extends BaseModel {
    public function getViewUrl() {
        return "view.php?$this-&gt;id";
    }
};
```

<p>А іспользовать такі моделі тепер - сплошне удовольствіє!</p>

```php
<?php $postings = Posting::all(); ?>

...

<?php foreach ($postings as $posting) >
    <h2><?php echo $posting->title ?></h>

    <div class="description">
        <?php echo $posting->description ?>
    </div>

    <div class="read-more">
        <?php echo $posting->getViewUrl() ?>
    </div>
<?php endforeach; ?>
```

<p>По-друге, в конструктор ми передаєм (всігда) поля з таблички, а конструктор - записує їх в масив <code>__data</code>. Ну і в конструкторі ж ми дуже хітро достаєм імя таблички з імєні класа: якшо у нас, напрімєр, клас моделі <code>Posting</code>, то ім’я таблички буде <code>postings</code>.</p>

<p>Ще один ньюанс з приводу імені таблички і того шо ми вертаєм в мєтоді <code>all()</code>: тут іспользується така хітра функція <code>get_called_class()</code>, яка вертає ім’я класа, який визвав поточний мєтод. Воно працює і з наслєдованієм в ООП, тоість якшо запустити отакє:</p>

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

<p>то воно виведе:</p>

```
Moo
Foo
```

<p>І якшо ми захочем зробить об’єкт дочєрнього класа внутрі батьківського, то ми можем спокойно визвать</p>

```php
$class = get_called_class();
new $class(...);
```

<p>Ну і всі запроси до бази даних строяться вже іспользуя поля <code>$__data</code> і <code>$__tableName</code>.</p>

<p>А тепер трошки поправим файлову структуру: у нас тепер є кілька файлів, які не відносяться конкретно до цього блога, а, так сказать, єсть каркасом, на якому можна наліпити ше шось. Це <code>BaseModel.php</code> і <code>connect.php</code>. Точніше, <code>connect.php</code> - в ньому задаються параметри подключєнія до бази даних конкретно цього блога. Шо я предлагаю - зробити три папки:</p>

<ul><li><code>core</code>, в якому буде <code>BaseModel.php</code></li>
<li><code>models</code>, куди положити <code>Posting.php</code> і всі прочі моделі, які наслєдують <code>BaseModel</code></li>
<li><code>config</code>, куди положить <code>connect.php</code></li>
</ul><p>Не забудь поправити всі путі у <code>require()</code>!</p>

<p>Замєть, тепер файл <code>connect.php</code> нам треба тільки в одному місці - в <code>BaseModel.php</code>. Шо тоже кагбе упрощає код.</p>

<p>Тоість, тепер у нас має бути якась така файлова структура:</p>

<p><img alt="чотка файлова структура" src="/images/tumblr/php-blog-part2/tumblr_inline_pl2njcFk4W1qh5oee_400.webp"/></p>

<h2>Домашнє завдання</h2>

<p>Зроби моделі поста і камєнтіка і застав їх работать внутрі <code>index.php</code> і <code>view.php</code>. Замєть, шо в бєзопасності <code>BaseModel</code> єсть дірка. А ще - і це <strong>дуже</strong> важно - подивись шо таке <strong>PDO</strong> і зроби замість всіх <code>mysql_query</code> і прочіх роботу з <strong>PDO</strong>.</p>
