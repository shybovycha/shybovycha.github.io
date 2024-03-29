---
layout: post
title: "Чоткій блог на PHP. Часть перша"
date: '2014-07-22T12:45:00+02:00'
tags:
- programming
- php
- rtfm
tumblr_url: http://shybovycha.tumblr.com/post/92523360106/php
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

<h2>Прєдісловіє</h2>

<p>Дарагой друг! В цих статтях я розкажу тобі як зробить чоткій блог на чотком язику PHP. Розказувать буду просто, шоб всьо було понятно. Ну шо, паєхалі?</p>

<h2>Сістємні трєбованія</h2>

<p>Нехай у нас є який-небудь самий стандартний, простий, перший блог на PHP. Така собі кучка файлів для созданія постов, списка постов, перегляду оддєльного поста, реєстрації, логіна і логаута, камєнтіруванія… Просто купа файлів. В цих файлах у нас і HTML, і PHP код.</p>

<!--more-->

<p>Приблизно це виглядить отак:</p>

<p><img alt="нечотка файлова структура" src="/images/tumblr/php-blog-part1/tumblr_inline_pk8jsmKqWa1qh5oee_400.webp"/>.</p>

<p>І дуже-дуже нада знать CSS, HTML, ООП в PHP і потом, може, JavaScript і jQuery.</p>

<p>Перед началом роботи краще привести в порядок HTML і CSS - розбити всьо красіво по папочкам (CSS - оддєльно, рісунки - оддєльно, PHP-файли - оддєльно); убрать з HTML всі атрібути <code>style="..."</code>, всюди пороздававши класи елементам; пообертать атрібути в кавички (<code>height="15px"</code>, а не <code>height=15px</code>) і так далєє. Шоб ще прощє було потом читать код.</p>

<h2>Чистим HTML код</h2>

<p>Першим ділом ми будем умєньшать кількість кода, шо є в нашом блогє. В народі це називається <strong>принцип DRY, Don’t Repeat Yourself</strong>. Розшифровується воно як <strong>пацани не повторяють</strong>. В RubyOnRails і других крутих штуках шось подобне реалізовано так, шоб весь HTML-код і PHP-код (ну або там Ruby-код, Python-код, будь-який-язик-программіруванія-код) були оддєльно, ну або хотя би по мінімуму PHP і по максімуму HTML.</p>

<p>Тоість, єсть такі штуки, які називаються <strong>View</strong> і єсть такі штуки які називаються <strong>Controller</strong>. Так от, в View - у нас чисто виводиться інформація, форматірується з помощю HTML, а чучуть кода все ще є для всяких циклов, іфов або визовов методов <strong>Helper</strong>&lsquo;ов (якшо нада хітро вивести шось - ссилку, напрімєр, на пост або на удалєніє поста - шоб самому не писати весь адрєс ссилки - можна воспользоватись допоміжним мєтодом; од того і Helper). А в Controller - весь PHP код, який получає дані з бази даних, методи-Helpers, і вобщє нема HTML кода.</p>

<p>Виглядить це якось отако: якшо у нас було</p>

<p><img alt="нечоткий код index.php" src="/images/tumblr/php-blog-part1/tumblr_inline_pk8jsnkhj81qh5oee_500.webp"/></p>

<p>то має стати</p>

<p><img alt="чоткий код index.php" src="/images/tumblr/php-blog-part1/tumblr_inline_pk8jsnJby91qh5oee_500.webp"/></p>

<p>Так шо першим ділом ми будем убирать з нашого HTML весь лишній PHP код. Да, і желатєльно заранєє договоритись шо до цього момєнта HTML має бути подчищений от лишнього CSS - як мінімум всі <code>style="css-code"</code> замінить на <code>class="class-name"</code>. Шоб простіше було орієнтіруватись в коді.</p>

<h2>Перший кантроллєр</h2>

<p>Давай попробуєм зробить сначала контроллєр для странічки <code>index.php</code>, в якій отображаються всі пости. По старой, доброй традіції, ми зробим оддєльний файл з классом і подключим його зверху <code>index.php</code> через <code>require_once()</code>.</p>

<p>Якшо ти помниш ООП, то у классов є такій магічний мєтод як конструктор. Так вот, в ньому ми будем робити круту таку штуку як загрузку даних (із бази, із сесії, із куков) в пєрємєнні, які потом будем іспользовать.</p>

<p>Напрімєр, якшо ми перероблюємо список постінгов, то нам нада масив всіх наших постів з бази взять. Якшо нам там же треба пользоватєль, який залогінений - нам із сесії треба його взять. Або рішить, шо його нема.</p>

<p>Виглядить це буде отако:</p>

<pre><code>require('connect.php');

class PostingsController {
    var $postings, $user;

    function __construct() {
        // получаєм з бази пости
        $res = mysql_query("SELECT * FROM postings");

        $this-&gt;postings = array();

        while ($row = mysql_fetch_assoc($res)) {
            $this-&gt;postings[] = $row;
        }

        // берем із сесії пользоватєля
        if (isset($_SESSION['user'])) {
            $this-&gt;user = $_SESSION['user'];
        } else {
            $this-&gt;user = null;
        }
    }
};
</code></pre>

<p>Давай зразу робить чотку структуру файлов. Зробим папку <code>controllers</code> і туди будем сохранять контроллєри в файлах <code>***_controller.php</code>. Тоість, наш оцей клас ми положим в файл <code>controllers/PostingsController.php</code>.</p>

<p>Тепер в файлі <code>index.php</code> в самом верху, де у нас раньше подключалась база даних і прочі штуки, у нас буде тільки <code>require()</code> цього контроллєра. Но поки у нас архітєктура не дуже крута, то ще одну строчку прийдеться добавить:</p>

<pre><code>require_once('controllers/PostingsController.php');

$controller = new PostingsController();
</code></pre>

<p>І тепер всі оці <code>while ($posting = mysql_fetch_assoc($res)) { ... }</code> можна замінить на <code>foreach ($controller-&gt;postings as $posting) { ... }</code>, а <code>$_SESSION['user']</code> - на <code>$controller-&gt;user</code>. І це кручє, бо ми можем юзера і в базі тримать, і ще невідомо де. Ну а постінги - так вроді прощє іспользувать.</p>

<h1>Перший хелпєр</h1>

<p>А тепер давай ще додамо перший метод, який упростить жизнь і покаже мощь архітєктури. Він нам помогатиме формірувать URL до постінга. Це шось тіпа:</p>

<pre><code>&lt;a href="view.php?id=&lt;?php echo $posting['id'] ?&gt;"&gt;...&lt;/a&gt;
</code></pre>

<p>буде замінений на</p>

<pre><code>&lt;a href="&lt;?php echo $controller-&gt;viewPostingUrl($posting) ?&gt;"&gt;...&lt;/a&gt;
</code></pre>

<p>Согласись, якшо внезапно поміняється путь до файла, який буде отвічать за просмотр поста, то в першому случаї прийдеться багато де мінять цю строчку, а в другому случаї - тільки в кантроллєрі.</p>

<p>Отак, у нас цей метод буде принімать масів і вертать строчку… Зробим його пока шо простим:</p>

<pre><code>public function viewPostingUrl($posting) {
    $id = $posting['id'];
    return "view.php?id=$id";
}
</code></pre>

<h2>Домашнєє заданіє</h2>

<p>Попробуй зробити всякі другі хелпери - для ссилок на удалєніє постов, на редактіруваніє, і прочі.</p>
