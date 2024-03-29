---
layout: post
title: "Чоткій блог. Часть третя"
date: '2014-07-28T14:30:00+02:00'
tags:
- rtfm
- tutorial
- php
tumblr_url: http://shybovycha.tumblr.com/post/93106798026
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

<h2>Прєдупрєждєніє</h2>

<p>Будь готов! В цій главі ми полностью переробим блог! Він не буде працювати (як раньше) аж до кінця домашнього заданія; код буде магічним; придеться пару раз удалять <strong>весь</strong> код з деяких файлів! Але в сухом остаткє, потом має стати ну дуже прикольно розробляти його дальше.</p>

<!--more-->

<h2>RIP, блог!</h2>

<p>От коли-небудь ти точно міг замітить, шо на сайтах мало ссилок з адрєсами віда <code>some_file.php?...</code>. А більше ссилок віда <code>/moo/foo</code>. Так от, не зря ми чуть раньше вспоминали про <strong>MVC</strong>. У нас ще осталось цілих дві букви для расшифровки - <strong>Controller</strong> і <strong>View</strong>.</p>

<p>Раньше у нас один контроллєр отвічав чотко за одну странічку, за один php-файл. І ти помниш, як ми круто зробили з моделями, шоб можна було щитай пустий клас (ну і, канєшна, соотвєтствующу табличку в базє даних) создать - і модель у нас работала. От давай тепер зробим так, шоб і з кантроллєрами у нас було шось похоже - один клас отвічає за кучу разних странічок!</p>

<p>В всяких там Ruby On Rails, ASP.NET MVC, Django і прочіх модних мейнстрімових фреймворках штука “контроллєр” оброблює багато всяких странічок, об’єдіньонних похожой функциональностью. Каждий публічний метод контроллєра називається <strong>action</strong> і обрабатує виключно одну странічку. Так, напрімєр, якшо у нас є контроллєр “Пости”, він обрабатує всьо шо зв’язано <strong>тільки</strong> з постами:</p>

<ul><li>редактірованіє</li>
<li>удалєніє</li>
<li>созданіє</li>
<li>просмотр</li>
<li>просмотр списка всіх постів</li>
</ul><p>В цій часті ми будем убивать на корню весь наш блог! Чисто раді експерименту!</p>

<p>По суті, весь наш блог - це одна програма. В народі - <strong>application</strong>. Так давай зробим клас, який буде “запускать” наш сайт! Назвем цей клас <code>Application</code> і положим ми його рядом з <code>index.php</code>. В цьому класі нада зробить якийсь мєтод, який буде задавать настройки нашому сайту (<strong>конкрєтно нашому сайту</strong>) і мєтод, який буде собсно запускать наш сайт з цими настройками.</p>

<p>Якшо ти вніматєльно ізучав ООП, то поймеш, шо по феншую треба всякі пєрємєнні, які стосуються <strong>конкретно такого-то об’єкта</strong> задавать в конструкторі. А всі мєтоди шо визиватимуться у цього об’єкта будуть іспользовать ці пєрмєнні.</p>

<p>От так і предлагаю зробить:</p>

<pre><code>&lt;?php
    class Application {
        var $database;

        public function __construct() {
            $this-&gt;database = array(
                'user' =&gt; 'root',
                'password' =&gt; 'abc123',
                'host' =&gt; 'localhost',
                'database' =&gt; 'dbank'
            );
        }

        public function run() {
            // шось тут коїться!
        }
    }
</code></pre>

<p>а в файлі <code>index.php</code> тепер у нас буде тільки от така штука:</p>

<pre><code>&lt;?php
    require_once('application.php');

    $app = new Application();
    $app-&gt;run();
</code></pre>

<p>І якшо ти зараз запустиш свій блог - ти побачиш ровним щотом нічого. Бо в методі <code>run()</code> нічого не виводиться. Давай научимось розбирать адрєс странічки, яку хоче пользоватєль</p>

<h2>Закопуєм блог</h2>

<p>Тепер давай розберемся от з чим: у нас пользоватєль не хоче бачити таку бяку в адрєсной строкє браузера, як тіпа <code>moo.php?foo=bar&amp;baz=dao</code>. Йому удобніше, пріятніше читать шось тіпа <code>/moo/bar?baz=dao</code>. Тіпа вони будуть запоминать або тіпа вони зможуть записать на бумажці десь це. Не важно особо, нашо це пользоватєлю - главне зробить його щасливим.</p>

<p>Щас ми будем обільно рефлєксірувать. Єсть в PHP така одна чудєсна пєрємєнна, як <code>$_SERVER</code>. Це масів. В ньому лежить багато інтересних всяких настройок, але нам поки треба буде тільки одна - <code>$_SERVER['REQUEST_URI']</code>. Як не странно, воно слабо зв’язано з сервером. Це - адрєс, який ввів пользоватєль, тільки без домєна. Так шо, якшо у нас сайт називається <code><a href="http://google.com/search/">http://google.com/search/</a></code>, а пользоватєль введе в адрєсну строку браузєра <code><a href="http://google.com/search/moo/foo/bar">http://google.com/search/moo/foo/bar</a></code>, то в соотвєтствующому php-файлі, пєрємєнна оця <code>$_SERVER['REQUEST_URI']</code> буде мати значення <code>/moo/foo/bar</code>.</p>

<p>Але по умолчанію наш сервер перенаправляє пользоватєля на файл <code>index.php</code>, при цьому рісує цей <code>index.php</code> в адрєс странічки. Нада убрать його відти. Для цього ми візьмем настройки сєрвєра <strong>Apache</strong> (якшо у тебе ВНЕЗАПНО другий сервер - будем шось думать - пиши мені в лічку) і трошки їх поміняєм.</p>

<p>Першим ділом тобі треба включить расширєніє сервера під назвою <code>rewrite</code>. В лінуксах всяких це робиться в два етапа:</p>

<ol><li>ставиться пакєт <code>apache2-utils</code></li>
<li>включається це расширєніє командой <code>[sudo] a2enmod rewrite</code></li>
</ol><p>В віндовзах всяких прийдеться шукать відповідне меню з галочкою біля <code>mod-rewrite</code>.</p>

<p>Потом тобі треба в корні сайта, рядом з <code>index.php</code> положить файлік <code>.htaccess</code> (начинається ім’я його з крапки!) з таким вмістом:</p>

<pre><code>RewriteEngine on
RewriteCond %{REQUEST_FILENAME} !-d
RewriteCond %{REQUEST_FILENAME} !-f
RewriteRule . index.php [L]
</code></pre>

<p>Ця штука заставить всі адрєса, шо пользоватєль вводить для цього сайта перенаправлять в <code>index.php</code>. І у тебе в пєрємєнной <code>$_SERVER['REQUEST_URI']</code> всігда буде адрєс без <code>/index.php</code>.</p>

<p>Тепер давай розіб’єм все, шо нам буде присилать пользоватєль в строчці адрєса условно на три тіпа - <strong>контроллєр</strong>, <strong>екшн цього контроллєра</strong> і <strong>все остальне</strong> (в народі - <strong>парамєтри</strong>). Розбивать будем просто - по символу <code>/</code>. <strong>Все остальне</strong> - це по суті масив <code>$_GET</code>.</p>

<p><strong>ЯКШО НЕ СТРЕЛЬНУЛО</strong> то треба в настройках хоста (в лінуксах - <code>/var/www/apache2/sites-available/000-default.conf</code>) дописати</p>

<pre><code>&lt;Directory /var/www/&gt;
        AllowOverride all
        Options FollowSymlinks
        Order allow,deny
        Allow from all
&lt;/Directory&gt;
</code></pre>

<p>Шо по суті для папки <code>/var/www</code> (де лежить сайт і, в часності, <code>index.php</code>) дає можливість через <code>.htaccess</code> мінять настройки сервера.</p>

<p>Давай тепер в методі <code>Application::run()</code> впишем код шоб воно виводило нам наш адрєс:</p>

<pre><code>public function run() {
    echo $_SERVER['REQUEST_URI'];
}
</code></pre>

<p>Тепер розіб’єм цей адрєс на ім’я контроллєра і екшна. Розбиваєм по символу <code>/</code> і тупо берем перші два елємєнта получєного массіва. Якшо не хватає другого елємєнта - по умолчанію поставим <code>index</code>. Якшо не хватає ще й першого - по умолчанію поставим <code>application</code>.</p>

<pre><code>$pieces = array_filter(explode('/', $_SERVER['REQUEST_URI']));
$controllerName = isset($pieces[1]) ? $pieces[1] : 'application';
$actionName = isset($pieces[2]) ? $pieces[2] : 'index';
</code></pre>

<p>Така хороша функція <code>array_filter()</code> убирає із массіва пусті елементи. А тройним оператором <code>(condition) ? if_true : if_false</code> ми робим круте присвоєніє нужного значєнія в пєрємєнні. І як не странно, але в цьому випадку ключі у масива - <code>1</code> і <code>2</code>.</p>

<p>Тепер якшо ти виведеш ці пєрємєнні в методі <code>Application::run()</code> і зайдеш на сайт напрімєр по ссилці http://localhost/moo/foo , то ти побачиш як ця штука працює. Якшо передасиш один параметр через слешку: <code>http://localhost/moo</code> - побачиш шо <code>$controllerName = 'moo'</code> а <code>$actionName = 'index'</code>. Якшо два параметра - <code>http://localhost/moo/foo</code> - побачиш шо буде ще й <code>$actionName = 'foo'</code>. А якшо більше - то ніякої різниці вже не побачиш.</p>

<p>Цей момент важно запомнить: якшо наш сайт не знатиме якого контроллєра взять - буде намагатись найти контроллєр всього сайта. Якшо він не буде знати який йому екшн визвати - буде пробувать визвати екшн <code>index</code>.</p>

<h2>Почучуть оживляєм</h2>

<p>Тепер давай зробим якийсь конкрєтний контроллєр! Положим ми його в папочку <code>controllers</code>. Хай він поки особо нічого крутого не вміє - просто виводить <code>&lt;h1&gt;I am alive!&lt;/h1&gt;</code>. І це у нас буде екшн <code>index</code>:</p>

<pre><code>&lt;?php
    class PostingsController {
        public function index() {
            echo '&lt;h1&gt;I am alive!&lt;/h1&gt;';
        }
    }
</code></pre>

<p>Тепер у нас стоїть задача аутоматично найти цей класс і визвати нужний метод у нього, якшо пользоватєль попробує зайти по ссилці <code>http://localhost/postings/</code> або <code>http://localhost/postings/index</code>.</p>

<p>Ми скористаємось такою шикарною штукою PHP як авто-пошук нужних класів. В народі - <strong>autoloading</strong>. Якшо пишем ми на стареньком PHP 5.0, то у нас ще довго буде возможность пользуватись глобальною магічною функцією <code>__autoload($className)</code>.</p>

<p>Коли ми описуєм цю функцію, PHP буде іспользувать її шоб найти класс, який ми не опреділили, но питаємось іспользувать. В неї приходить один аргумєнт - ім’я класа, який нам нада найти. І ця функція должна шось хотя б попробувать зробити, шоб описати клас з таким іменем або подключити нужний файл, де цей клас описаний.</p>

<p>Ну от напрімєр, ми точно знаєм, шо наші кантроллєри лежать в папці <code>controllers</code>. Так давай научим наш <code>Application</code> находить нужний контроллєр і создавать його об’єкт в методі <code>run()</code>!</p>

<pre><code>&lt;?php
    function __autoload($className) {
        require("controllers/$className.php");
    }

    class Application {
        // ...

        public function run() {
            $pieces = array_filter(explode('/', $_SERVER['REQUEST_URI']));

            $controllerName = isset($pieces[1]) ? $pieces[1] : 'application';
            $actionName = isset($pieces[2]) ? $pieces[2] : 'index';

            $controllerName = ucfirst($controllerName) . 'Controller';

            $controller = new $controllerName();
            $controller-&gt;$actionName();
        }
    }
</code></pre>

<p>Трошки заумний код. Начнем з нового: функція <code>ucfirst($text)</code> бере першу букву з <code>$text</code> і перероблює її у верхній регістр. Тоість, якшо було <code>moo</code> стане <code>Moo</code>, якшо було <code>Moo</code> - нічо не поміняється. Функція <code>__autoload</code> просто бере файл з папки <code>controllers/</code> з імєнєм контроллєра і подключає його.</p>

<p>Тут важний момет нащот самой функції <code>require()</code>: коли ти її визиваєш вона по суті бере код із того файла і вставляє в місце, де ти визвав цю функцію. Точніше, не сам код файла, а виполняє цей файл і всьо шо вийшло в результаті - вставляє в це місце, де ти визвав <code>require</code>. Якщо там опрєдєлєні пєрємєнні чи класи - вони стають опрєдєлєні там де ти визвав <code>require</code>. Але якшо ти його визвав внутрі <code>__autoload</code> - то ці пєрємєнні чи класи стають доступні в усьому файлі, де сработав <code>__autoload</code>. Бо ще <code>__autoload</code> можна на каждий файл свій написать.</p>

<h2>Виводим HTML</h2>

<p>Якшо ти ще помниш, там де ми визиваєм <code>require</code> - виполняється код файла, який ми подключаєм і все що виполнилось вставляється в те місце.</p>

<p>Давай зробим таку інтересну штуку: зробим файлік який-небудь <code>views/index.html</code> з яким-небудь HTML і в екшні <code>PostingsController::index()</code> просто підключим його:</p>

<pre><code>&lt;?php
    class PostingsController {
        public function index() {
            require('index.html');
        }
    }
</code></pre>

<p>І якшо зараз зайти на <code>http://localhost/postings</code> - можна буде побачити содєржиме цього файла! І це прекрасно, ящітаю!</p>

<p>У каждого екшна є свій <strong>view</strong> - оддєльний файлік, який виводить HTML тільки основної частини страніци. Щитай це вміст тега <code>&lt;body&gt;</code>. А контроллєр збирає виведений цей HTML і виводить в свій оддєльний view, в якому і описане все вокруг тега <code>&lt;body&gt;</code> - щитай це весь HTML, кромє <code>&lt;body&gt;</code>. Для контроллєра цей view називається по-особєнному, <strong>layout</strong>. І от цей лейаут, він може бути один на всі-всі контроллєри, а може бути разний для нєкоторих або і вообщє - свій для каждого кантроллєра.</p>

<p>Так от, давай зробим слєдующім образом: у нас буде папочка <code>views/layouts</code>, де будуть лежать разні шаблончики. І ми зробим один шаблончик, який буде аутоматично подтягуватись для каждого контроллєра і назвем його <code>application.phtml</code>, як контроллєр “по умолчанію”. Замєть, файл має расширєніє <code>.phtml</code>. Це нормально. Це показує шо у нас в файлі є і PHP-код і HTML размєтка. В папочку <code>views/layouts</code> давай положим файлік <code>postings.phtml</code> куди наб’єм допустім такий HTML:</p>

<pre><code>&lt;!DOCTYPE html&gt;
&lt;html lang="en"&gt;
&lt;head&gt;
    &lt;meta charset="UTF-8"&gt;
    &lt;title&gt;Document&lt;/title&gt;
&lt;/head&gt;
&lt;body&gt;
    &lt;?php echo $body ?&gt;
&lt;/body&gt;
&lt;/html&gt;
</code></pre>

<p>Тут ми берем якусь пєрємєнну <code>$body</code> і виводим її. Простенький файлік, нічого сложного. Але нам нада звідкись достать цей самий <code>$body</code>. Для цього ми іспользуєм ще одну інтересну плюшку PHP, <strong>output buffer</strong>. Воно позволяє виполнить якийсь код якби в фоні, а все шо цей код вивів - сохранить в пєрємєнну. Виглядить це так:</p>

<pre><code>&lt;?php
    ob_start();

        echo 'I am alive!';

    ob_end_clean();
</code></pre>

<p>Якшо ти запустиш цей код - нічого не виведеться. Но зато оцей тєкст, <code>I am alive!</code> можна достать функцієй <code>ob_get_contents()</code>:</p>

<pre><code>&lt;?php
    ob_start();

        echo 'I am alive!';

    $content = ob_get_contents();

    ob_end_clean();

    echo "&lt;h1&gt;$content&lt;/h1&gt;";
</code></pre>

<p>А от цей код вже виведе <code>&lt;h1&gt;I am alive!&lt;/h1&gt;</code>. Замєть: <code>ob_end_clean()</code> очищає цей буфєр, куди ложиться вивод всього-всього між <code>ob_start()</code> і <code>ob_end_clean()</code>. І забрати звідки вивод треба до того, як визвать <code>ob_end_clean()</code>.</p>

<p>А якшо ми тепер поміняємо просте <code>echo</code> між <code>ob_start</code> і <code>ob_end</code> на, скажімо, <code>require()</code>?</p>

<pre><code>&lt;?php
    ob_start();

    require('views/index.html');

    $content = ob_get_contents();

    ob_end_clean();

    require('views/layouts/postings.phtml');
</code></pre>

<p>Виходить, що в <code>$content</code> у нас лежить вміст файла <code>views/index.html</code>, а якшо ми подключаєм лейаут - він виведе HTML з цього лейаута і всередині нього - пєрємєнну <code>$content</code>.</p>

<p>А тепер, якшо ти оцей код написав внутрі <code>PostingsController::index()</code>, ми його переробим. Шоб не писать в каждом екшні каждого кантроллєра багато всяких <code>ob_start</code>, <code>require</code> і прочіх. Опять вертаємось до класа <code>Application</code>, в якому ми описуєм запуск нашого сайта. В нашому чудєсном методі <code>run()</code> ми вже написали шо робить коли пользоватєль заходить на сайт. Давай тепер трошки перепишем цей код шоб обернуть його вивод в нужний нам HTML:</p>

<pre><code>&lt;?php
    class Application {
        // ...
        public function run() {
            $pieces = array_filter(explode('/', $_SERVER['REQUEST_URI']));

            $controllerName = isset($pieces[1]) ? $pieces[1] : 'application';
            $actionName = isset($pieces[2]) ? $pieces[2] : 'index';

            $controllerClass = ucfirst($controllerName) . 'Controller';

            // вичісляєм ім’я view
            $viewFile = "views/$controllerName/$actionName.phtml";

            // вичісляєм ім’я layout
            $layoutFile = "views/layouts/$controllerName.phtml";

            $controller = new $controllerClass();

            ob_start();

            // тут внутрі контроллєра задаються якісь пєрємєнні
            $controller-&gt;$actionName();

            // рісуєм відповідний view, в якому виводяться пєрємєнні, задані в екшні
            require($viewFile);

            // получаєм вивод екшна - наш body
            $body = ob_get_contents();

            ob_end_clean();

            // виводим лейаут, в якій виводиться body
            require($layoutFile);
        }
    }
</code></pre>

<p>При такому розкладі у нас цей мєтод робить сильно забагато всього. А всі пєрємєнні, які ми задаєм внутрі екшна недоступні всередині view - вони задаються внутрі контроллєра, а рісує в’юху у нас - application. Значить, вертаємось до ООП. Будем трошки усложнять зараз роботу шоб потом писать було проще.</p>

<p>Зробим основний клас для всіх контроллєрів, який буде рісувать свій лейаут і нужні в’юхи. А із application будем визивать якийсь один метод, який просто виведе все шо треба. Цей клас ми, по старой доброй традиції, положим в папку <code>core/</code>:</p>

<pre><code>&lt;?php
    class BaseController {
        public function __invoke($controllerName, $actionName) {
            // вичісляєм ім’я view
            $viewFile = "views/$controllerName/$actionName.phtml";

            // вичісляєм ім’я layout
            $layoutFile = "views/layouts/$controllerName.phtml";

            ob_start();

            $this-&gt;$actionName();

            // рісуєм відповідний view, в якому виводяться пєрємєнні, задані в екшні
            require($viewFile);

            $body = ob_get_contents();

            ob_end_clean();

            // виводим лейаут, в якій виводиться body
            require($layoutFile);
        }
    }
</code></pre>

<p>Ми просто щитай перенесли код з одного класа в другий. Замєть, ім’я мєтода називається з двох підкреслень. Ми тоже можем создавать <strong>магічні</strong> методи, муа-ха-ха! Тепер трошки переробим наш контроллєр <code>PostingsController</code>:</p>

<pre><code>&lt;?php
    require_once('core/BaseController.php');
    require_once('models/Posting.php');

    class PostingsController extends BaseController {
        function index() {
            $this-&gt;postings = Posting::all();
        }
    }
</code></pre>

<p>Але тепер дивись: у нас в каждій моделі подключається файл <code>BaseModel.php</code>. Тепер ще нам прийдеться в каждом контроллєрі подключать <code>BaseController.php</code>. Не комільфо. Щас будем рефакторить - всі ці строчки з моделей і з контроллерів можна перенести в <code>Application.php</code> - цей файл у нас по-любому подключається і всігда запускається самий перший.</p>

<pre><code>&lt;?php
    require_once('core/BaseModel.php');
    require_once('core/BaseController.php');

    // ...

    class Application {
        // ...
    }
</code></pre>

<p>Тепер нам треба перемістить <code>views/index.html</code> в <code>views/postings/index.phtml</code> шоб наш <code>Application::run()</code> знайшов його. І з <code>PostingsController::index()</code> убрать весь код - у нас тепер усьо аутоматично!</p>

<h2>Ітог</h2>

<p>Шо нам дає така сложна сістєма? По-перше, і це дуже важно, нам тепер не треба в усі файли пихать один і той же HTML. По-друге, у нас сайт може мати кучу совєршенно разних по дизайну і вигляду странічок тепер. І шоб їх помінять чи шоб в них встроїть, допустім, виведення всіх постінгів чи якоїсь формочки, нам треба просто вивести туди в’юху. І перероблювать, в случаї чого, треба дуже і дуже мало кода.</p>

<p>Наконєц, чисто для наглядності давай зробим простеньке упражнєніє: виведем в екшні <code>PostingsController::index()</code> всі постінги, іспользуя всі ці лейаути-в’юхи і моделі.</p>

<p>У нас уже є лейаут для контроллєра <code>PostingsController</code>. У нас уже є сам контроллєр. У нас є модель поста. У нас даже є в’юха (і байдуже шо дурна) для цього!</p>

<p>Значить, пробуєм? В екшні ми задамо перемєнну з усіма постінгами. Кстаті, конструктор контроллєра нам уже не треба - вже не актуально, не мейнстрім. А у в’юхє ми просто виведем всі постінги.</p>

<pre><code>&lt;?php
    require_once('models/Posting.php');

    class PostingsController {
        public function index() {
            $this-&gt;postings = Posting::all();
        }
    }
</code></pre>

<p>І в’юха:</p>

<pre><code>&lt;?php foreach ($this-&gt;postings as $posting): ?&gt;
    &lt;h1&gt;&lt;?php echo $posting-&gt;title ?&gt;&lt;/h1&gt;

    &lt;div class="description"&gt;
        &lt;?php echo $posting-&gt;description ?&gt;
    &lt;/div&gt;
&lt;?php endforeach; ?&gt;
</code></pre>

<h2>Домашнє заданіє</h2>

<p>Воно буде велике. <em>Prepare yourself!</em></p>

<ol><li>Треба прєдусмотрєть ситуацію, коли ми не найшли нужний класс контроллєра (щитай, не найшли файл) - в таком случаї нада показать ошибку <strong>404</strong>.</li>
<li>Переведи весь блог на контроллєри, в’юхи, лейаути і моделі. Попробуй, чи прощє тепер чи сложніше стало робить це.</li>
</ol>
