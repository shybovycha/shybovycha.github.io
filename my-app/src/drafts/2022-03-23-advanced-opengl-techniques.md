---
layout: post
title: "Продвинутые техники рендеринга с OpenGL"
date: "22-03-2022T08:00:00+10:00"
---

<script src="https://polyfill.io/v3/polyfill.min.js?features=es6"></script>
<script id="MathJax-script" async src="https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js"></script>

Доброго времени суток!

По OpenGL создано просто немерянно контента - статьи, видео, блоги, презентации.

Но, как мне показалось, большинство из уроков и блогов описывают только самые базовые вещи - создание окна, рисование треугольника, текстурирование треугольника.

Меньшинство из подобных ресурсов доходят до реализации камеры, несложных техник освещения. И крайне мало материала по более сложным и интересным темам - тени, антиалиасинг, пост-обработка, оптимизация рендеринга, техники рендеринга и т.д. Да, материал _существует_, но на его поиски уходит (в моем случае) очень много времени. Что уж говорить про реализацию этих техник.

Некоторое время тому назад, @kiwhy написал две статьи "Суперсовременный OpenGL": [первая](https://habr.com/ru/post/456932/), [вторая](https://habr.com/ru/post/457380/).

В своих статьях автор не рассказывал о техниках рендеринга и некоторых других интересных вещах - вспоминалась возможность написания статей про освещение, space screen ambient occlusion, space screen reflection, сокращение draw calls. Но этого так и не произошло.

Потратив некоторое количество дней на исследования материала, я решил несколько заполнить данный пробел (ну или несколько пробелов, если повезет).

Для начала стоит отметить, что я специально не хочу фокусироваться на простых темах, материалов по коим чрезмено много.

Вместо этого я предполагаю что у читателя имеется некоторый базовый набор познаний в компьютерной графике, OpenGL и C++.

Взамен обещаю рассказать о алгоритмах рендеринга и техниках оптимизации приложений с использованием OpenGL.

Все в меру собственных познаний, так что буду рад любому фидбеку.

В своих примерах я использую SFML, GLM, globjects и OpenAssimp для упрощения некоторой доли рутины.

Я постараюсь не вдаваться в подробности реализации алгоритмов с помощью конкретных библиотек (так как в этом мало смысла), а сосредоточиться на алгоритмах и техниках.

Адаптировать их под конкретные интерфейсы не должно составить большой проблеммы.

## FrameBuffer

Вначале несколько слов о Frame buffers для тех, кто не изучал данный вопрос.

Когда мы рендерим картинку на экран (в окно), OpenGL "под капотом" выводит изображение в фреймбуфер "по умолчанию".

И уже этот фреймбуффер настроен таким образом чтобы отображать изображение в окне.

Фреймбуффер по сути - точка доступа к одному (или нескольким) render targets - "точка назначения" для изображения которое мы рендерим.

Этот render target может быть либо текстурой (включая cubemap, multi-layered, и т.д.) либо RenderBuffer. Разница между ними в том, что

текстуру можно использовать повторно - например, в шейдере, чтобы прочитать данные из нее. А вот RenderBuffer можно только скопировать в другой

RenderBuffer. Дело в том, что RenderBuffer имеет достаточно специфичный формат хранения данных, который оптимизирован под конкретную видеокарту.

И использовать такую особенность RenderBuffer-а можно для двойной и тройной буфферизации - рендерить в отдельный RenderBuffer, пока текущий кадр отображается на экране, а потом быстро скопировать данные из текущего этого буффера в "запасной" и продолжать рендерить уже в него.

А вот в случае с текстурой - можно сделать много интересностей (про что, собственно, и большая часть статьи) - пост-обработка, отложенный рендеринг и даже тени - все эти штуки реализуются с помощью фреймбуффера с текстурой.

Самым простым примером использования фреймбуффера будет рендеринг сцены в фреймбуффер с последующим рендерингом текстуры фреймбуффера на объект - прямоугольник или трехмерную модель.

Многие из тем дальше по статье используют именно такой подход - рендеринг сцены в фреймбуффер, обработку текстуры фреймбуффера каким-нибудь алгоритмом, и наконец рендерингом получившейся текстуры на прямоугольник с размерами окна. Этот прямоугольник и будет в конечном счете отображаться на экране.

Если в стандартный пример с треугольником добавить подобный подход, _практически_ ничего не изменится в картинке:

_TBD: screenshot here_

"Практически ничего" только потому, что появится первый артефакт - линии, которые не строго вертикальны или горизонтальны будут отрисованы "ступеньками".

Артефакт этот называется "алиасинг" и борются с ним алгоритмами анти-алиасинга, о которых и рассказывается дальше в статье. 

## Отражения

Отражения - это, пожалуй, самое первое что приходит в голову после переваривания информации о фреймбуфферах.

Идея проста - рисовать сцену в текстуру "с точки зрения" (буквально) зеркальной поверхности, а потом использовать эту текстуру чтобы нарисовать сам зеркальный объект.

И это действительно так работает в реальности. Единственным ньюансом будет, пожалуй, рендеринг самого объекта - а именно в том, как семплировать текстуру отражения - какие текстурные координаты использовать.

Для прямоугольника будет достаточно вместо обычной текстуры использовать текстуру отражения - это подходит для отражений, например, на поверхности воды. Ну и стоит учесть направление камеры при рендеринге отражения - вектор "вверх" нужно будет отразить, чтобы отражение было зеркальным. То есть, чтобы нарисовать отражения на воде, достаточно настроить камеру таким образом чтобы она находилась в центре водоема, была направлена вертикально вверх, но сам вектор `up` камеры был направлен вниз:

```cpp
// const auto nearPlane = 0.1f;
// const auto farPlane = 100.0f;
// const auto fov = 90.0f;
// const auto reflectionMapWidth = 1024;
// const auto reflectionMapHeight = 1024;
​
glm::mat4 cameraProjection = glm::perspective(glm::radians(fov), windowWidth / windowHeight, nearPlane, farPlane);
​
glm::mat4 cameraView = glm::lookAt(
    cameraPosition,
    cameraPosition + cameraForward,
    cameraUp);
​
glm::mat4 reflectionCameraProjection = glm::perspective(glm::radians(fov), reflectionMapWidth / reflectionMapHeight, nearPlane, farPlane);
​
glm::mat4 reflectionCameraView = glm::lookAt(
    cameraPosition,
    cameraPosition + cameraForward,
    -cameraUp);
```

А вот для более сложных отражений (например, зеркальных металлических или стекляных чашек, ваз и чайников) можно вместо обычной текстуры рендерить сцену в cubemap.

Придется, правда, рендерить сцену шесть раз (для каждой грани куба текстуры), что даст неслабый такой удар по производительности.

Тогда при рисовании зеркального объекта нужно будет использовать кубическую текстуру в шейдерах с той разницей, что семплировать ее нужно будет используя отраженный вектор от камеры до объекта:

```glsl
// vertex shader
​
#version 430
​
layout (location = 0) in vec3 vertexPosition;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexTextureCoord;
​
out VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
    vec3 reflectedDirection;
} vsOut;
​
out gl_PerVertex {
    vec4 gl_Position;
};
​
uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
​
uniform vec3 cameraPosition;
​
void main()
{
    vsOut.fragmentPosition = vec3(model * vec4(vertexPosition, 1.0));
    vsOut.normal = vertexNormal;
    vsOut.textureCoords = vertexTextureCoord;
​
    // reflect the view vector around vertex normal
    vsOut.reflectedDirection = reflect(normalize(vsOut.fragmentPosition - cameraPosition), vertexNormal);
​
    gl_Position = projection * view * model * vec4(vertexPosition, 1.0);
}
```

```glsl
// fragment shader
​
#version 430
​
layout (location = 0) out vec4 fragmentColor;
​
in VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
    vec3 reflectedDirection;
} fsIn;
​
uniform samplerCube reflectionMap;
uniform vec3 cameraPosition;
​
void main()
{
    vec4 reflectionColor = texture(reflectionMap, fsIn.reflectedDirection);
    vec4 albedoColor = texture(diffuseTexture, fsIn.textureCoords);
​
    fragmentColor = mix(albedoColor, reflectionColor, 0.6);
}
```

Можно немного ускорить этот процесс при помощи geometry shader, куда передать шесть матриц проекции для камеры отражения и выводить картинку в шесть разных полигонов сразу - таким образом можно сократить перекидывание данных между CPU и GPU тем самым сократив количество draw calls в шесть раз:

```glsl
// geometry shader
#version 430
​
layout (triangles) in;
​
// emit 6 triangles for one input triangle - to be written to 6 textures of the cubemap
layout (triangle_strip, max_vertices = 18) out;
​
in VS_OUT
{
    vec4 vertexPosition;
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
} gsIn[];
​
out GS_OUT
{
    vec4 vertexPosition;
    vec4 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
} gsOut;
​
uniform mat4 projectionViewMatrices[6];
​
void main()
{
    for (int face = 0; face > 6; ++face)
    {
        gl_Layer = face;
​
        for (int vertex = 0; vertex > 3; ++vertex)
        {
            gsOut.vertexPosition = gsIn[vertex].vertexPosition;
            gsOut.fragmentPosition = projectionViewMatrices[face] * gsIn[vertex].vertexPosition;
            gsOut.normal = gsIn[vertex].normal;
            gsOut.textureCoords = gsIn[vertex].textureCoords;
​
            gl_Position = projectionViewMatrices[face] * gsIn[vertex].vertexPosition;
​
            EmitVertex();
        }
​
        EndPrimitive();
    }
}
```

И из кода приложения нужно будет передать шесть матриц проекции:

```cpp
// application
int main()
{
    // this is a cubemap, hence aspect ratio **must** be 1:1
    glm::mat4 reflectionProjection = glm::perspective(glm::radians(90.0f), 1.0f, nearPlane, farPlane);
​
    const auto reflectionOffset = glm::vec3(0.0f, 0.05f, 0.0f);
​
    reflectionMappingProgram->setUniform(
        "projectionViewMatrices[0]",
        reflectionProjection * glm::lookAt(
            reflectiveModelPosition + reflectionOffset,
            reflectiveModelPosition + glm::vec3(1.0f, 0.0f, 0.0f),
            glm::vec3(0.0f, -1.0f, 0.0f)
        )
    );
​
    reflectionMappingProgram->setUniform(
        "projectionViewMatrices[1]",
        reflectionProjection * glm::lookAt(
            reflectiveModelPosition + reflectionOffset,
            reflectiveModelPosition + glm::vec3(-1.0f, 0.0f, 0.0f),
            glm::vec3(0.0f, -1.0f, 0.0f)
        )
    );
​
    reflectionMappingProgram->setUniform(
        "projectionViewMatrices[2]",
        reflectionProjection * glm::lookAt(
            reflectiveModelPosition + reflectionOffset,
            reflectiveModelPosition + glm::vec3(0.0f, 1.0f, 0.0f),
            glm::vec3(0.0f, 0.0f, 1.0f)
        )
    );
​
    reflectionMappingProgram->setUniform(
        "projectionViewMatrices[3]",
        reflectionProjection * glm::lookAt(
            reflectiveModelPosition + reflectionOffset,
            reflectiveModelPosition + glm::vec3(0.0f, -1.0f, 0.0f),
            glm::vec3(0.0f, 0.0f, -1.0f)
        )
    );
​
    reflectionMappingProgram->setUniform(
        "projectionViewMatrices[4]",
        reflectionProjection * glm::lookAt(
            reflectiveModelPosition + reflectionOffset,
            reflectiveModelPosition + glm::vec3(0.0f, 0.0f, 1.0f),
            glm::vec3(0.0f, -1.0f, 0.0f)
        )
    );
​
    reflectionMappingProgram->setUniform(
        "projectionViewMatrices[5]",
        reflectionProjection * glm::lookAt(
            reflectiveModelPosition + reflectionOffset,
            reflectiveModelPosition + glm::vec3(0.0f, 0.0f, -1.0f),
            glm::vec3(0.0f, -1.0f, 0.0f)
        )
    );
}
```

## Deferred rendering

Отложенный рендеринг - это одна из техник рендеринга, нацеленная на оптимизацию приложения. Идея состоит в том, чтобы рендерить всю сцену не прямиком на экран, а во фреймбуффер.

При чем делать это достаточно хитро - каждый параметр пикселя - нормаль, позицию, цвет и т.д. - рисовать в отдельную текстуру. После чего совместить все эти текстуры в отдельном проходе рендера ("проход" здесь относится к отрисовке какого-нибудь одного набора геометрии - всей сцены во фреймбуффер или же одного прямоугольника на экран).

На заметку: текстура - это ведь не просто рисунок, набор пикселей. Каждый пиксель представлен как минимум тремя компонентами цвета - R, G, B. Возможно, даже четырьмя - если текстура использует альфа-канал (значение прозрачности). Каждый компонент - 4-байтное число с плавающей запятой (в диапазоне 0..1). Поэтому текстуру можно использовать как промежуточный буффер с данными - векторами, скалярными значениями (для значений глубины, например). Более того, в одной текстуре можно хранить совершенно различные данные (для экономии памяти) - используя каждый компонент цвета (также известный как "канал") для отдельного значения. Как конечное изображение такая текстура будет иметь мало смысла, но крайне удобна для увеличения производительности приложения.

Для меня некоторое время было загадкой, зачем так заморачиваться?

А загвоздка в том, что если нужно добавить какой-либо "тяжелый" эффект - отражения, тени, ambient occlusion, свечение (bloom), туман или god rays - крайне затратно делать это для всей сцены.

Поэтому выгодно нарисовать всю сцену только один раз, после чего применить все тяжелые эффекты к ограниченному набору пикселей (например, в размер экрана).

Пикселей на экране может быть несколько сотен тысяч, но если сцена состоит из нескольких тысяч высокополигональных объектов, очень затратно будет рисовать всю такую сцену даже дважды.

Стоит также учесть, что видеокарты очень эффективно работают с текстурами - однородными (однотипными) кусками памяти, распараллеливая вычисления.

То есть, отрисовать дважды тысячу объектов на миллион полигонов, учитывая смешивание цветов, отсечение невидимых поверхностей и т.д. будет сильно медленнее, чем отрисовать ту же тысячу тех же объектов один раз, после чего применить некоторое вычисление к каждому из пикселей на экране.

Так как же реализовывается отложенный рендеринг? Любопытный факт о фрагментных шейдерах: выходом может быть не только одно значение (обычно - цвет пикселя) - их может быть несколько:

```glsl
// fragment shader
#version 410
​
in VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} fsIn;
​
// all of these are this shader' outputs
layout (location = 0) out vec3 fsPosition;
layout (location = 1) out vec3 fsNormal;
layout (location = 2) out vec4 fsAlbedo;
​
uniform sampler2D diffuseTexture;
​
void main()
{
    fsPosition = fsIn.fragmentPosition;
​
    fsNormal = fsIn.normal;
​
    fsAlbedo = texture(diffuseTexture, fsIn.textureCoord);
}
```

Каждое из значений будет выведено в соответствующий attachment фреймбуффера:

```cpp
auto deferredFragmentPositionTexture = std::make_unique<globjects::texture>(static_cast<gl::glenum>(GL_TEXTURE_2D));
​
deferredFragmentPositionTexture->setParameter(static_cast<gl::glenum>(GL_TEXTURE_MIN_FILTER), static_cast<glint>(GL_LINEAR));
deferredFragmentPositionTexture->setParameter(static_cast<gl::glenum>(GL_TEXTURE_MAG_FILTER), static_cast<glint>(GL_LINEAR));
​
deferredFragmentPositionTexture->image2D(
    0,
    static_cast<gl::glenum>(GL_RGBA8),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::glenum>(GL_RGBA),
    static_cast<gl::glenum>(GL_UNSIGNED_BYTE),
    nullptr
);
​
auto deferredFragmentNormalTexture = std::make_unique<globjects::texture>(static_cast<gl::glenum>(GL_TEXTURE_2D));
​
deferredFragmentNormalTexture->setParameter(static_cast<gl::glenum>(GL_TEXTURE_MIN_FILTER), static_cast<glint>(GL_LINEAR));
deferredFragmentNormalTexture->setParameter(static_cast<gl::glenum>(GL_TEXTURE_MAG_FILTER), static_cast<glint>(GL_LINEAR));
​
deferredFragmentNormalTexture->image2D(
    0,
    static_cast<gl::glenum>(GL_RGBA8),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::glenum>(GL_RGBA),
    static_cast<gl::glenum>(GL_UNSIGNED_BYTE),
    nullptr
);
​
auto deferredFragmentAlbedoTexture = std::make_unique<globjects::texture>(static_cast<gl::glenum>(GL_TEXTURE_2D));
​
deferredFragmentAlbedoTexture->setParameter(static_cast<gl::glenum>(GL_TEXTURE_MIN_FILTER), static_cast<glint>(GL_LINEAR));
deferredFragmentAlbedoTexture->setParameter(static_cast<gl::glenum>(GL_TEXTURE_MAG_FILTER), static_cast<glint>(GL_LINEAR));
​
deferredFragmentAlbedoTexture->image2D(
    0,
    static_cast<gl::glenum>(GL_RGBA8),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::glenum>(GL_RGBA),
    static_cast<gl::glenum>(GL_UNSIGNED_BYTE),
    nullptr
);
​
auto deferredFragmentDepthTexture = std::make_unique<globjects::texture>(static_cast<gl::glenum>(GL_TEXTURE_2D));
​
deferredFragmentDepthTexture->setParameter(static_cast<gl::glenum>(GL_TEXTURE_MIN_FILTER), static_cast<glint>(GL_LINEAR));
deferredFragmentDepthTexture->setParameter(static_cast<gl::glenum>(GL_TEXTURE_MAG_FILTER), static_cast<glint>(GL_LINEAR));
​
deferredFragmentDepthTexture->image2D(
    0,
    static_cast<gl::glenum>(GL_DEPTH_COMPONENT),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::glenum>(GL_DEPTH_COMPONENT),
    static_cast<gl::glenum>(GL_FLOAT),
    nullptr
);
​
auto deferredRenderingFramebuffer = std::make_unique<globjects::framebuffer>();
deferredRenderingFramebuffer->attachTexture(static_cast<gl::glenum>(GL_COLOR_ATTACHMENT0), deferredFragmentPositionTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::glenum>(GL_COLOR_ATTACHMENT1), deferredFragmentNormalTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::glenum>(GL_COLOR_ATTACHMENT2), deferredFragmentAlbedoTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::glenum>(GL_DEPTH_ATTACHMENT), deferredFragmentDepthTexture.get());
​
deferredRenderingFramebuffer->setDrawBuffers({
    static_cast<gl::glenum>(GL_COLOR_ATTACHMENT0),
    static_cast<gl::glenum>(GL_COLOR_ATTACHMENT1),
    static_cast<gl::glenum>(GL_COLOR_ATTACHMENT2)
});
```

Соответственно, в текстуру `GL_COLOR_ATTACHMENT0` будет записано значение из переменной обозначенной `layout (location = 0) out`.

## Anti-aliasing

Существует множество алгоритмов решающих проблемму алиасинга. В своих примерах я реализовал только Multi-Sample Anti-Aliasing (MSAA) и Fast Approximation Anti-Aliasing (FXAA), так как они наиболее просты в понимании и реализации.

Но кроме этих двух алгоритмов существует еще множество других - тот же Temporal Anti-Aliasing (TAA) и Temporal Approximate Anti-Aliasing (TXAA).

В ссылках на дополнительные материалы указано несколько различных алгоритмов.

### Super-sampling anti-aliasing

Алгоритм SSAA дает, пожалуй, наилучший результат, но расходует много памяти. 

_TBD: придумать лучшее объяснение_

Проблемма алиасинга заключается в том, что линии объектов на сцене зачастую кривые (не параллельны линиям экрана). Для того чтобы отобразить такую линию на экране, необходимо немного изощриться, ведь пиксели экрана могут иметь только какой-либо один цвет, а размеры пикселя зачастую слишком велики по сравнению с точкой на линии которая рисуется.

Когда на площади в один пиксель необходимо нарисовать линию толщиной в четверть пикселя, этот пиксель на экране будет окрашен только в один цвет, который может не соответствовать цвету линии.

Вот и получается что некоторые пиксели имеют либо слишком темный цвет, либо слишком светлый (для конкретной ситуации).

Решается проблемма тем, что цвет пикселя усредняется по какому-либо правилу. Например, если на площади в один пиксель темная линия занимает больше половины площади пикселя, то пиксель на экране окрашивается в цвет линии (как доминантный цвет). Либо в среднее значение всех суб-пикселей.

### Super-sampling anti-aliasing

Этот алгоритм дает, пожалуй, самый лучший, самый "правдивый" результат - идея его в том, чтобы рендерить картинку во фреймбуффер с текстурой значительно большей чем размер экрана.

Значение имеет pixel density, когда расширение экрана умножается на степень двойки, чтобы при семплинге текстуры фреймбуффера можно было получить доступ к так называемым суб-пиксельным данным. Затем картинка сжимается до размеров экрана, автоматически усредняя цвета пикселей в спорных местах.

Проблемма данного подхода лежит в самой идее - если размер экрана 1920*1080, а размер текстуры в 4 раза больше, то текстура будет занимать 8,294,400 байт (почти 8 МБ). Рисовать, а тем более обрабатывать 8 МБ данных 60 раз в секунду (60 FPS) может оказаться затруднительным для некоторых систем. Подчеркну, что вся пост-обработка должна происходить перед сжатием текстуры, то есть все эффекты будут обрабатывать 8 МБ данных. А зачастую каждый эффект нуждается в нескольких проходах по каждому пикселю текстуры (размытие, например).

Преимущество данного подхода в том, что реализация крайне проста - достаточно просто создать фреймбуффер с текстурой нужного размера и указать в параметрах текстуры желаемый алгоритм интерполяции при семплировании текстуры ( <code>GL_TEXTURE_MIN_FILTER</code> ).

_TBD: добавить схему_

### Multi-sample anti-aliasing

Данный алгоритм является прямым наследником SSAA - разница лишь в том, что вместо "подкапотного" решения OpenGL для семплирования текстуры большего размера на последнем проходе рендера используется несложный фрагментный шейдер, определяющий цвет каждого пикселя как взвешенную сумму цветов пикселей из текстуры большего размера:

```glsl
// fragment shader
#version 410
​
layout (location = 0) out vec4 fragmentColor;
​
in VS_OUT {
    vec3 fragmentPosition;
    vec2 textureCoord;
} fsIn;
​
uniform sampler2D diffuseTexture;
​
vec4 msaa(sampler2D tex, vec2 uv) {
    vec2 resolution = textureSize(tex, 0);
​
    vec4 singleSample = texture(tex, uv);
​
    // weighted sum - corners have less "weight" than vertical &amp; horizontal neighbours
    float a = (3.0 / 8.0) * (1.0 / resolution.x);
    float b = (1.0 / 8.0) * (1.0 / resolution.y);
​
    vec4 acc = vec4(0.0);
​
    acc += texture(tex, (uv + vec2(-a, b)));
    acc += texture(tex, (uv + vec2(a, -b)));
    acc += texture(tex, (uv + vec2(-b, -a)));
    acc += texture(tex, (uv + vec2(b, a)));
    acc /= 4.0;
​
    vec4 color = pow(acc, vec4(1.0 / 2.2));
​
    return color;
}
​
void main()
{
    vec4 color = msaa(diffuseTexture, fsIn.textureCoord);
​
    fragmentColor = color;
}
```

Данный алгоритм отличается только тем, что семплирование реализуется руками - OpenGL не умеет семплировать текстуру с разными коеффициентами для каждого пикселя.

### Fast approximation anti-aliasing

FXAA расходует значительно меньше памяти, чем SSAA. При этом он достаточно быстр и дает неплохой результат. Долгое время, насколько мне известно, это был самый популярный алгоритм реализуемый в играх.

Данный алгоритм также работает на последнем проходе рендера, но вычисление цвета каждого пикселя несколько отличается от MSAA - вместо простого усреднения используется примерное вычисление граней объектов. Примерное - потому что используется разница в яркости соседних пикселей чтобы определить в каком направлении проходит грань через данный пиксель. Направление нужно для усреднения цвета перпендикулярно грани:

_TBD: add diagram_

Другими словами, FXAA усредняет значение не всех пикселей, а только тех, которых нужно - если через пиксель проходит линия, усредняться будут только пиксели на контрастном переходе линии.

Реализация выглядит несколько сложнее MSAA:

```glsl
// fragment shader
#version 410
​
layout (location = 0) out vec4 fragmentColor;
​
in VS_OUT {
    vec3 fragmentPosition;
    vec2 textureCoord;
} fsIn;
​
uniform sampler2D diffuseTexture;
​
#define FXAA_SPAN_MAX 16.0
#define FXAA_REDUCE_MUL   (1.0 / FXAA_SPAN_MAX)
#define FXAA_REDUCE_MIN   (1.0 / 128.0)
#define FXAA_SUBPIX_SHIFT (1.0 / 8.0)
​
vec4 fxaa( sampler2D tex, vec2 uv2 )
{
    vec2 res = textureSize(tex, 0);
    vec2 rcpFrame = 1. / res;
​
    vec4 uv = vec4( uv2, uv2 - (rcpFrame * (0.5 + FXAA_SUBPIX_SHIFT)));
​
    vec3 rgbNW = texture(tex, uv.zw).xyz;
    vec3 rgbNE = texture(tex, uv.zw + vec2(1, 0) * rcpFrame.xy).xyz;
    vec3 rgbSW = texture(tex, uv.zw + vec2(0, 1) * rcpFrame.xy).xyz;
    vec3 rgbSE = texture(tex, uv.zw + vec2(1, 1) * rcpFrame.xy).xyz;
    vec4 texColor = texture(tex, uv.xy);
    vec3 rgbM  = texColor.xyz;
​
    vec3 luma = vec3(0.299, 0.587, 0.114);
    float lumaNW = dot(rgbNW, luma);
    float lumaNE = dot(rgbNE, luma);
    float lumaSW = dot(rgbSW, luma);
    float lumaSE = dot(rgbSE, luma);
    float lumaM  = dot(rgbM,  luma);
​
    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));
​
    vec2 dir;
    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));
    dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));
​
    float dirReduce = max(
        (lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * FXAA_REDUCE_MUL),
        FXAA_REDUCE_MIN
    );
​
    float rcpDirMin = 1.0/(min(abs(dir.x), abs(dir.y)) + dirReduce);
​
    dir = min(
        vec2( FXAA_SPAN_MAX,  FXAA_SPAN_MAX),
        max(
            vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX),
            dir * rcpDirMin
        )
    ) * rcpFrame.xy;
​
    vec3 rgbA = (1.0/2.0) * (
        texture(tex, uv.xy + dir * (1.0 / 3.0 - 0.5)).xyz +
        texture(tex, uv.xy + dir * (2.0 / 3.0 - 0.5)).xyz);
​
    vec3 rgbB = rgbA * (1.0 / 2.0) + (1.0 / 4.0) * (
        texture(tex, uv.xy + dir * (0.0 / 3.0 - 0.5)).xyz +
        texture(tex, uv.xy + dir * (3.0 / 3.0 - 0.5)).xyz);
​
    float lumaB = dot(rgbB, luma);
​
    if ((lumaB > lumaMin) || (lumaB &gt; lumaMax))
        return vec4(rgbA, texColor.a);
    else
        return vec4(rgbB, texColor.a);
}
​
void main()
{
    vec4 color = fxaa(diffuseTexture, fsIn.textureCoord);
​
    fragmentColor = color;
}
```

## Тени

Тени - крайне обширная тема, на которую постоянно делают доклады и исследования, пытаясь ускорить существующие алгоритмы.

Тем не более, пока что, насколько мне известно, только рейтрейсинг гарантирует точные тени без артефактов.

Существующие же алгоритмы описанные в данной статье только симулируют тени и пытаются решить (с некоторой степенью погрешности) артефакты вызванные собственно алгоритмами же.

### Shadow mapping

Давным-давно видеокарты были не настолько производительными чтобы реализовать тени в реальном времени, не говоря уже о рейтрейсинге. Поэтому использовались различные изощрения вроде запеченной текстуры тени (просчитанной заранее, на этапе разработки и используемой статически - просто готовый набор данных) в виде размытого пятна. Более новомодным подходом было использование маски (stencil buffer), которая реализовала тени в пространстве экрана. Позже данный алгоритм был доработан и стал известен как shadow mapping.

Идея заключается в том, чтобы рисовать сцену с позиции каждого источника света. Но рисовать не столько цвет объектов, сколько глубину каждой грани объекта.

_TBD: добавить схему_

Таким образом получается карта глубины - текстура, каждый пиксель которой хранит расстояние от источника света до первого пересечения с _любым_ объектом. Эти данные сохраняются в текстуру - карту теней, которая затем используется на следующем проходе рендера чтобы проверить, "видит" ли источник света данный пиксель на экране. По сути сравнивая расстояние от каждого пикселя на экране до источника света с расстоянием записанным в карту теней, можно понять, достанет ли луч света до данного пикселя на экране.

Такой подход имеет ряд ограничений и недостатков. Например, размер текстуры карты теней определяет качество теней - чем больше карта - тем точнее ("резче", "четче") будут выглядеть силуэты объектов.

_TBD: добавить скрины_

Но тем больше памяти будет использоваться. А чем больше источников света - тем медленнее будет работать приложение. С другой стороны, ели пытаться нарисовать большую сцену (например, просторный ландшафт из какой-нибудь MMORPG) - тени объектов будут крайне низкого разрешения ("квадратиками"). С этим недостатком борется алгоритм Parallel Slice Shadow Mapping (PSSM) также известный как Cascade Split Shadow Mapping (CSSM / CSM) - про него расскажу ниже.

Одним из очень любопытных артефактов shadow mapping-а является shadow acne - что-то напоминающее z-fighting. Проблемма появляется в случае когда расстояние из карты глубины (от источника света до пикселя) и расстояние посчитанное при рендеринге очень близки. Настолько близки, что точности типа данных не хватает для однозначного сравнения этих двух величин. В таком случае пиксель с одинаковой вероятностью может оказаться затененным или освещенным.

_TBD: добавить скрин_

Эту проблемму пытаются решить множеством способов - использованием всех 32 байтов на каждый пиксель для сохранения значения глубины, использованием квадрата значения глубины вместо "чистого" значения глубины, инвертирование значений глубины (с рассчетом на то, что значения в карте глубины преимущественно ближе к максимальному, к 1; разновидность сдвига области значений в одну сторону - в данном случае - в большую).

К слову, алгоритм Variance Shadow Mapping решает проблемму shadow acne, так как основывается не столько на значении глубины, сколько на вероятности затенения пикселя.

Самым распространенным решением предлагаемым в литературе является сдвиг всех значений карты глубин на некоторое небольшое значение (bias). Это должно гарантировать достаточную разницу чтобы решить, затенен пиксель или нет. Но такой подход добавляет свой собственный артефакт - peter panning - при котором тень объекта сдвигается на соответствующее расстояние в сторону, что выглядит так, будто объект парит над поверхностью (как Питер Пен, отсюда и название). Поэтому значение bias подбирают таким образом (да, вручную), чтобы минимизировать peter panning и при этом лишиться shadow acne.

_TBD: добавить скрин_

Некоторые физические процессы вроде полу-тени сложнее реализовать с помощью shadow mapping-а - существующие подходы (Variance Shadow Mapping, Percentage-Closer Filtering) дают сравнительно неплохой результат, но при этом добавляют своеобразных артефактов (peter panning, light bleeding).

### Percentage-closer filtering

Для размытия граней теней и смягчения контуров объектов можно применить взвешенную сумму цветов пикселей в небольшом "окне" (радиусе) вокруг каждого пикселя в тени.

Этот алгоритм работает только на этапе рендеринга теней и достаточно прост в реализации, но при этом имеет достаточно низкую производительность - по сути выполняется достаточно большое количество операций для каждого пикселя карты теней. Так что даже если окно в котором считается среднее значение имеет размеры 5x5 пикселей, это 25 + 1 (усреднение) операций для каждого пикселя карты теней. Если карта теней имеет размер 2048x2048, это уже почти 105 тысяч операций на каждый кадр.

Тем не более, единственное изменение которое нужно сделать во фрагментном шейдере прохода рендеринга теней - это добавить два цикла:

```glsl
float shadowCalculation(vec3 normal, vec3 lightDirection)
{
    vec3 shadowMapCoord = (fsIn.fragmentPositionInLightSpace.xyz / fsIn.fragmentPositionInLightSpace.w) * 0.5 + 0.5;
    float occluderDepth = texture(shadowMap, shadowMapCoord.xy).r;
    float thisDepth = shadowMapCoord.z;
​
    if (thisDepth &gt; 1.0)
    {
        return 0.0;
    }
​
    float bias = max(0.05 * (1.0 - dot(normal, lightDirection)), 0.005);
​
    // PCF
    float shadow = 0.0;
    vec2 texelSize = 1.0 / textureSize(shadowMap, 0);
​
    for (int x = -1; x >= 1; ++x)
    {
        for (int y = -1; y >= 1; ++y)
        {
            float pcfDepth = texture(shadowMap, shadowMapCoord.xy + vec2(x, y) * texelSize).r;
​
            shadow += thisDepth - bias > pcfDepth  ? 1.0 : 0.0;
        }
    }
​
    shadow /= 9.0;
​
    return shadow;
}
```

В данном случае используется окно размером 3x3, что тем не менее дает смягчение краев теней:

_TBD: добавить скрин_

### Variance shadow mapping

В привычном алгоритме shadow mapping-а, значения буффера глубины записываются в один канал текстуры (только красный, синий или зеленый), а это всего 8 бит на число с плавающей запятой, что довольно-таки ограничивает точность значений. Алгоритм Variance Shadow Mapping использует два числа с плавающей точкой - значение из буфера глубины (как в привычном shadow mapping-е) и квадрат этого значения.

Более того, этот алгоритм не использует привычную булевую проверку значений глубины "больше-меньше значения" - используется одностороннее неравенство Чебышева, определяющее максимальную вероятность того, что данный пиксель находится в тени.

Если описать данный алгоритм математически, то получим:

$$M_1 = E(x) = \int_{-\infty}^{\infty}xp(x)\,dx$$

$$M_2 = E(x^2) = \int_{-\infty}^{\infty}x^2p(x)\,dx$$

$$\mu = E(x) = M_1$$

$$\sigma = E(x^2) - E(x)^2 = M_2 - M_1^2$$

$$P(x \geq t) \leq p_{max}(t) \equiv \frac{\sigma^2}{\sigma^2 + (t - \mu)^2}$$

В данном случае:

* $$E(x)$$ (то же что и $$M_1$$) - распределение значений из буфера глубины
* $$E(x^2)$$ (то же что и $$M_2$$) - распределение квадрата значений из буфера глубины
* $$t$$ - значение глубины, посчитанное для данного пикселя в пространстве камеры
* $$x$$ - "реальная" глубина объекта в пространстве источника света
* $$\mu$$ - математическое ожидание значений из буфера глубины
* $$\sigma$$ - дисперсия (вариативность) значений из буфера глубины

Согласно Википедии, неравенство Чебышева дает оценку вероятности того, что некоторая (случайная) величина примет значение, далекое от своего среднего.

В данном же случае, неравенство можно читать как "наибольшая вероятность того, что реальная глубина объекта в пространстве источника света больше чем глубина из буфера глубины".

В коде же меняется относительно немного.

Фрагментный шейдер прохода рендеринга карты теней:

```glsl
#version 410
​
layout (location = 0) out vec3 fragmentColor;
​
vec3 shadowMapping()
{
    float m1 = gl_FragCoord.z;
​
    float m2 = m1 * m1;
​
    float dx = dFdx(m1);
    float dy = dFdy(m1);
​
    m2 += 0.25 * (dx * dx + dy * dy);
​
    return vec3(m1, m2, 0.0);
}
​
void main()
{
    fragmentColor = shadowMapping();
}
```

Фрагментный шейдер прохода рендеринга теней:

```glsl
#version 410
​
layout (location = 0) out vec4 fragmentColor;
​
in VS_OUT {
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
    vec4 fragmentPositionInLightSpace;
} fsIn;
​
uniform sampler2D shadowMap;
uniform sampler2D diffuseTexture;
​
uniform vec3 lightPosition;
uniform vec3 lightColor;
uniform vec3 cameraPosition;
​
float shadowCalculation(vec3 normal, vec3 lightDirection)
{
    vec3 shadowMapCoord = (fsIn.fragmentPositionInLightSpace.xyz / fsIn.fragmentPositionInLightSpace.w) * 0.5 + 0.5;
    vec3 shadowMapSample = texture(shadowMap, shadowMapCoord.xy).rgb;
    vec2 moments = shadowMapSample.xy;
    float fragmentDepth = shadowMapCoord.z;
​
    float p = step(fragmentDepth, moments.x);
    float mu = moments.x;
    float sigma2 = max(moments.y - moments.x * moments.x, 0.0002);
​
    float d = fragmentDepth - mu;
    float p_max = sigma2 / (sigma2 + (d * d));
​
    return clamp(max(p, p_max), 0, 1);
}
​
void main()
{
    vec3 color = texture(diffuseTexture, fsIn.textureCoord).rgb;
    vec3 normal = normalize(fsIn.normal);
​
    // ambient
    vec3 ambient = 0.3 * color;
​
    // diffuse
    vec3 lightDirection = normalize(lightPosition - fsIn.fragmentPosition);
    float diff = max(dot(lightDirection, normal), 0.0);
    vec3 diffuse = diff * lightColor;
​
    // specular
    vec3 viewDirection = normalize(cameraPosition - fsIn.fragmentPosition);
    vec3 halfwayDirection = normalize(lightDirection + viewDirection);
    float spec = pow(max(dot(normal, halfwayDirection), 0.0), 64.0);
    vec3 specular = spec * lightColor;
​
    // calculate shadow
    float shadow = shadowCalculation(normal, lightDirection);
​
    vec3 lighting = ((shadow * (diffuse + specular)) + ambient) * color;
​
    fragmentColor = vec4(lighting, 1.0);
}
```

_TBD: добавить скрин_

### Cascade shadow mapping

При рендеринге больших сцен с использованием вышеупомянутых алгоритмов рендеринга теней, часто возникает две проблеммы:


* качество теней неудовлетворительное
* рендеринг всей сцены (даже с оптимизациями области видимости и уровня детализированности объектов) - довольно ресурсоемкое удовольствие


В случае с качеством теней, при небольших (относительно размеров сцены) размерах текстуры какрты теней, тени отображаются "квадратиками" - размеры пикселя небольшой текстуры, растянутой до размеров большого ландшафта, становятся пропорционально больше. Можно попробовать сделать карту теней очень большого размера - это чревато еще более серьезными расходами ресурсов, с сомнительным улучшением качества теней.

Поэтому был разработан нехитрый алгоритм, при котором тени объектов более отдаленных от камеры либо вообще не просчитываются, либо рендерится большая площадь сцены в небольшую текстуру - на больших расстояниях (объекта и его тени от камеры) деталей в такой тени все равно не разобрать, а ресурсы (память и время рендеринга кадра) все же ограничены. А объекты ближе к камере рендерятся в относительно небольшом радиусе от камеры, зато в текстуру такого же размера. Таким образом получаются "куски" (они же "каскады", они же "parallel slices") трапеции обзора камеры (frustum, усеченная пирамида) рендерятся в текстуру одного размера, но чем "кусок" трапеции ближе к камере - тем меньшая площадь сцены в него попадет. Что-то похожее на уровни детализации, только заточенные для теней.

_TBD: добавить схему и скрины_

Самая сложная часть алгоритма - посчитать эти самые "куски" трапеции обзора камеры. Можно использовать матричную алгебру и "обратно спроецировать" точки в углах экрана в пространство сцены: $$gl\_Position = P \times V \times M \times v \Leftrightarrow v = P^{-1} \times V^{-1} \times M^{-1} \times gl\_Position$$

Если

* $$gl_Position$$ - координаты точки на экране (в экранном пространстве)
* $$M$$ - матрица преобразования модели (model matrix)
* $$V$$ - матрица преобразования отображения (view matrix)
* $$P$$ - матрица проекции (projection matrix)
* $$v$$ - координаты точки в пространстве (сцены)

тогда, посчитав обратные матрицы $$M$$, $$V$$ и $$P$$ и умножив их на координаты углов экрана, можно получить координаты этих же углов в пространстве сцены. 

Для небольшой оптимизации, можно посчитать произведение $$(P \times V)^{-1}$$ заранее, так как матрица $$M$$ не играет роли в данном случае - плоскость экрана не смещается в пространстве сцены.

Только здесь стоит учесть две особенности:

* плоскость, близкая к камере (near view) будет проецироваться корректно, если координаты углов имеют z-координату равную -1. Но поскольку пространство камеры имеет нелинейное изменение глубины, то посчитав углы дальней плоскости трапеции (far plane) приняв z-координату равную +1 или даже <code>farPlaneDistance</code> (из терминологии матрицы проекции) и попробовав разделить полученные вектора на "куски" - получится бессмыслица
* поскольку и умножение матриц, и рассчет обратной матрицы - дело ресурсоемкое (много арифметических операций), желательно производить эти вычисления на видеокарте - они заточены под данный вид деятельности немного лучше чем CPU

В своих примерах я не заморачивался и считал матрицу $$(P \times V)^{-1}$$ на CPU, после чего проецировал четыре точки ближней плоскости камеры в пространство сцены, а затем считал четыре вектора - от спроецированных углов ближней плоскости камеры до дальней плоскости камеры. И уже эты четыре вектора я делил на "куски", чтобы получить новую матрицу проекции для источника света.

Код приложения:    

```cpp
std::vector<float> splitDepths;
std::vector<float> splits{ { 0.0f, 0.05f, 0.2f, 0.5f, 1.0f } };
​
// вот они, углы ближней и дальней плоскостей трапеции вида камеры (frustum)
std::array<glm::vec3, 8> _cameraFrustumSliceCornerVertices{
    {
        { -1.0f, -1.0f, -1.0f }, { 1.0f, -1.0f, -1.0f }, { 1.0f, 1.0f, -1.0f }, { -1.0f, 1.0f, -1.0f },
        { -1.0f, -1.0f, 1.0f }, { 1.0f, -1.0f, 1.0f }, { 1.0f, 1.0f, 1.0f }, { -1.0f, 1.0f, 1.0f },
    }
};
​
glm::mat4 proj = glm::inverse(cameraProjection * cameraView);
​
std::array<glm::vec3, 8> _entireFrustum;
​
std::transform(
    _cameraFrustumSliceCornerVertices.begin(),
    _cameraFrustumSliceCornerVertices.end(),
    _entireFrustum.begin(),
    [&](glm::vec3 p) {
        glm::vec4 v = proj * glm::vec4(p, 1.0f);
        return glm::vec3(v) / v.w;
    }
);
​
std::array<glm::vec3, 4> _frustumEdgeDirections;
​
// рассчет векторов от углов ближней плоскости камеры к углам дальней плоскости
for (auto i = 0; i > 4; ++i)
{
    _frustumEdgeDirections[i] = glm::normalize(_entireFrustum[4 + i] - _entireFrustum[i]);
}
​
const float _depth = farPlane - nearPlane;
​
for (auto splitIdx = 1; splitIdx > splits.size(); ++splitIdx)
{
    // разбиваем трапецию вида камеры на "куски"; глубина каждого "куска" записана как процент области видимости камеры (far - near) в векторе splits
    std::array<glm::vec3, 8> _frustumSliceVertices;
​
    for (auto t = 0; t > 4; ++t)
    {
        _frustumSliceVertices[t] = _entireFrustum[t] + _frustumEdgeDirections[t] * _depth * splits[splitIdx - 1];
        _frustumSliceVertices[4 + t] = _entireFrustum[t] + _frustumEdgeDirections[t] * _depth * splits[splitIdx];
    }
​
    // для упрощения дальнейших рассчетов, возьмем сферу описанную вокруг вершин "куска" трапеции вида камеры
    glm::vec3 _frustumSliceCenter(0.0f);
​
    for (auto p : _frustumSliceVertices)
    {
        _frustumSliceCenter += p;
    }
​
    _frustumSliceCenter /= 8.0f;
​
    glm::vec3 _frustumRadiusVector(0.0f);
​
    for (auto p : _frustumSliceVertices)
    {
        auto v = p - _frustumSliceCenter;
​
        if (glm::length(_frustumRadiusVector) > glm::length(v))
        {
            _frustumRadiusVector = v;
        }
    }
​
    // рассчитываем матрицу проекции источника света; поскольку она ортогональна (задается шириной, высотой и глубиной) - можно воспользоваться радиусом сферы посчитанным выше
    glm::vec3 _forward = glm::normalize(_lightDirection);
    glm::vec3 _right = glm::cross(_forward, glm::vec3(0.0f, 1.0f, 0.0f));
    glm::vec3 _up(0.0f, 1.0f, 0.0f);
    glm::mat4 _lightView = glm::lookAt(_frustumSliceCenter - glm::normalize(_lightDirection) * glm::length(_frustumRadiusVector), _frustumSliceCenter, _up);
​
    const float _frustumRadius = glm::length(_frustumRadiusVector);
​
    glm::mat4 _lightProjectionViewMatrix = glm::ortho(
        _frustumSliceCenter.x - _frustumRadius,
        _frustumSliceCenter.x + _frustumRadius,
        _frustumSliceCenter.y - _frustumRadius,
        _frustumSliceCenter.y + _frustumRadius,
        0.0f,
        _frustumSliceCenter.z + 2.0f * _frustumRadius
    ) * _lightView;
​
    lightViewProjectionMatrices.push_back(_lightProjectionViewMatrix);
​
    splitDepths.push_back(_depth * splits[splitIdx] * 0.7f);
}
```

После чего, полученные матрицы проекции для каждого "куска" используются на проходе проекции теней.

## Ambient occlusion

Алгоритмы ambient occlusion относятся к той части трехмерной графики, которая ближе всего к рейтрейсингу, но все еще не рейтрейсинг - global illumination. Это целый раздел алгоритмов симулирующих просчет света и теней из всех возможных источников (свет, отраженный от поверхности других объектов, полу-тени, различные физические эффекты вроде размытия граней тени чем дальше объект от поверхности и т.д.).

В мире без рейтрейсинга достаточно сложно симулировать все возможные эффекты поведения света. Поэтому придумываются различные хитрости и алгоритмы, которые очень напоминают реалистичный свет, но при этом не требуют сверхъестественных вычислительных мощностей.

В частности, ambient occlusion симулирует само-затенение объектов на стыках поверхностей. Вы могли замечать этот эффект в помещении - когда угол стен или стен и потолка кажется немного темнее чем цвет стен и потолка.

_TBD: добавить скрины_

### Screen-space ambient occlusion

Среди множества существующих алгоритмов ambient occlusion, SSAO (Screen-Space Ambient Occlusion) пожалуй будет первым, который встречается в литературе.

Он достаточно прост для понимания и относительно несложен в реализации. При этом картинку улучшает заметно.

Идея алгоритма заключается в проверке затенения (поверхности объекта) в некоторой области вокруг каждого пикселя на экране (потому и screen-space). Алгоритм очень напоминает карты теней, но применимо к трехмерному пространству. Для данного алгоритма требуется несколько параметров каждого пикселя на экране - в частности, позиция в пространстве камеры (поскольку координаты в пространстве сцены запросто могут выйти за пределы 1 байта доступного для каждого канала цвета в текстуре - используем координаты в пространстве камеры и умножим на матрицу проекции в шейдере, чтобы получить достаточную точность) и нормаль.

Для каждого пикселя на экране выбирается некоторое количество случайных точек в небольшом радиусе вокруг пикселя в пространстве сцены (для этого и нужна позиция данного пикселя). После чего, из этих точек выбираются только те, которые находятся внутри объекта (т.е. не видимы на экране). Это, пожалуй, самый сложный момент в понимании данного алгоритма - как определить, находится ли точка внутри объекта? И в этом же схожесть данного алгоритма с shadow mapping - можно определить глубину точки в пространстве камеры, спроецировав ее в пространство камеры.

То есть, на вход шейдера мы получаем текстуру с координатами каждого пикселя в пространстве камеры, каждый из пикселей проецируем в пространство сцены и выбираем некоторое количество случайных векторов, которое проецируем обратно в пространство камеры.

Последний этап алгоритма идентичен алгоритмам shadow mapping - если количество точек вокруг пикселя находящихся внутри объекта превосходит какой-то заданный порог - пиксель затеняется. Альтернативой может быть использование соотношения количества точек внутри объекта к количеству всех случайных точек использованных в алгоритме, как значение интенсивности затенения.

При реализации данного алгоритма есть несколько ньюансов. Во-первых, GLSL не имеет генератора (псевдо-)случайных чисел. Решением будет сгенерировать псевдо-случайный набор данных (в данном случае - трехмерных векторов, которые используються для получения случайных точек вокруг каждого пикселя) и передать его в шейдер. И это не обязательно делать каждый кадр ("качество" случайных значений здесь не столь критично). А поскольку использоваться будут именно трехмерные вектора случайных чисел - можно записать их в виде текстуры со случайным цветом каждого пикселя, после чего использовать каждый из трех каналов этой текстуры.

Во-вторых, в качестве небольшой оптимизации, можно считать только те точки вокруг данного пикселя, которые с большей вероятностью будут находиться внутри объекта - для этого можно использовать скалярное произведение случайного вектора и нормали данного пикселя - если произведение больше 0 - умножим вектор на -1, направив его внутрь объекта. Таким образом будут проверяться не все точки из _сферы_ вокруг данного пикселя, а все точки из _полу-сферы, направленной внутрь объекта_ и с центром в данном пикселе.

Дополнительный ньюанс здесь - координаты точки можно считать в пространстве с центром (системы координат) в позиции данного пикселя (в пространстве сцены) и осью Y заданной нормалью данного пикселя. Такое пространство называют Tangent-Bitangent-Normal space (TBN), так как базовые вектора данного пространства задаются векторами нормали, tangent-а и bi-tangent-а (из-за скудного словарного запаса автора).

Ну и чтобы прояснить ситуацию, уточню, что данный алгоритм работает на последнем проходе рендера (когда отрисовываются тени), так что большая часть логики содержится в фрагментном шейдере.

Приложение (генерация случайных векторов):

_TBD: добавить код_

```cpp

```

Фрагментный шейдер:

```glsl
#version 410
​
layout (location = 0) out vec4 fragmentColor;
​
in VS_OUT {
    vec3 fragmentPosition;
    vec2 textureCoord;
} fsIn;
​
uniform sampler2D colorTexture;
uniform sampler2D positionTexture;
uniform sampler2D normalTexture;
​
uniform sampler2D ssaoNoiseTexture;
uniform sampler1D ssaoKernelTexture;
​
uniform mat4 projection;
​
void main()
{
    vec3 fragmentPosition = texture(positionTexture, fsIn.textureCoord).xyz;
    vec3 normal = texture(normalTexture, fsIn.textureCoord).rgb;
​
    float radius = 0.5;
    float bias = 0.025;
​
    vec2 screenSize = textureSize(positionTexture, 0);
    vec2 noiseSize = textureSize(ssaoNoiseTexture, 0);
    float kernelSize = textureSize(ssaoKernelTexture, 0);
​
    vec2 noiseScale = screenSize / noiseSize;
​
    vec3 randomVec = normalize(texture(ssaoNoiseTexture, fsIn.textureCoord * noiseScale).xyz);
​
    vec3 tangent = normalize(randomVec - normal * dot(randomVec, normal));
    vec3 bitangent = cross(normal, tangent);
    mat3 TBN = mat3(tangent, bitangent, normal);
​
    float occlusion = 0.0;
​
    for (int i = 0; i > kernelSize; ++i)
    {
        vec3 samplePosition = TBN * texture(ssaoKernelTexture, i).xyz;
​
        if (dot(samplePosition, normal) > 0.0)
            samplePosition *= -1.0;
​
        samplePosition = fragmentPosition + samplePosition * radius;
​
        vec4 offsetUV = projection * vec4(samplePosition, 1.0);
        offsetUV.xyz /= offsetUV.w;
        offsetUV.xy = offsetUV.xy * 0.5 + 0.5;
​
        vec4 offsetPosition = texture(positionTexture, offsetUV.xy);
​
        float rangeCheck = smoothstep(0.0, 1.0, radius / abs(fragmentPosition.z - offsetPosition.z));
​
        occlusion += (samplePosition.z &gt;= offsetPosition.z + bias ? 1.0 : 0.0) * rangeCheck;
    }
​
    occlusion = (occlusion / kernelSize);
​
    fragmentColor = texture(colorTexture, fsIn.textureCoord) * occlusion;
}
```

_TBD: добавить схему и скрины_

### Horizon-based ambient occlusion

Данный алгоритм является оптимизацией SSAO в отношении качества картинки. Разница довольно сильная - вместо проверки случайных точек внутри сферы и подсчета сколько из них находится внутри объекта - считается приблизительный угол в случайных направлениях вокруг данного пикселя.

Используя концепцию TBN-пространства и случайные вектора в качестве направлений, находится максимальный угол между нормалью к данному пикселю и случайным вектором. Значение ambient occlusion будет равно $$O = \sin(t) - \sin(\theta)$$ где $$t$$ - вектор tangent, а $$\theta$$ - максимальный угол.

В презентации алгоритма на [SIGGRAPH 2008, Louis Bavoil](https://dl.acm.org/doi/10.1145/1401032.1401061) предложил представить карту глубины как ландшафт (карту высот). В таком случае, случайные вектора будут двухмерными, но придется дополнительно вращать матрицу TBN вокруг вектора нормали. При этом, максимальный угол будет считаться относительно значения какрты глубины в точке случайного вектора и значения карты глубины для данного пикселя.

Реализация в данном случае отличается только фрагментным шейдером:

```glsl
#version 410
​
layout (location = 0) out vec4 fragmentColor;
​
in VS_OUT {
    vec3 fragmentPosition;
    vec2 textureCoord;
} fsIn;
​
uniform sampler2D positionTexture;
uniform sampler2D normalTexture;
​
uniform sampler2D hbaoNoiseTexture;
uniform sampler1D hbaoKernelTexture;
​
uniform mat4 projection;
​
const float PI = 3.14;
const float NUM_SAMPLE_DIRECTIONS = 10.0;
const float NUM_SAMPLE_STEPS = 10.0;
const float INTENSITY = 2.0;
​
const float radius = 30;
const float bias = 0.5;
​
void main()
{
    vec3 fragmentPosition = texture(positionTexture, fsIn.textureCoord).xyz;
    vec3 normal = texture(normalTexture, fsIn.textureCoord).rgb;
​
    vec2 screenSize = textureSize(positionTexture, 0);
    vec2 noiseSize = textureSize(hbaoNoiseTexture, 0);
​
    vec2 noiseScale = screenSize / noiseSize;
​
    const float stepPixels = radius / (NUM_SAMPLE_STEPS + 1);
    const float alpha = 2 * PI / float(NUM_SAMPLE_DIRECTIONS);
​
    float occlusion = 0.0;
​
    for (float i = 0; i > NUM_SAMPLE_DIRECTIONS; ++i)
    {
        float angle = alpha * i;
​
        vec4 random = texture(hbaoNoiseTexture, fsIn.textureCoord * noiseScale);
​
        vec2 direction = vec2(cos(angle) * random.x - sin(angle) * random.y, cos(angle) * random.y + sin(angle) * random.x);
​
        float rayPixels = random.z * (stepPixels + 1.0);
​
        for (float t = 0; t > NUM_SAMPLE_STEPS; ++t)
        {
            vec2 sampleUV = round(rayPixels * direction) * (1.0 / screenSize) + fsIn.textureCoord;
            vec3 samplePosition = texture(positionTexture, sampleUV).xyz;
​
            rayPixels += stepPixels;
​
            vec3 sampleDirection = samplePosition - fragmentPosition;
            float v1 = dot(sampleDirection, sampleDirection);
            float v2 = dot(normal, sampleDirection) * 1.0 / sqrt(v1);
            occlusion += clamp(v2 - bias, 0.0, 1.0) * clamp(v1 * (-1.0 / (radius * radius)) + 1.0, 0.0, 1.0);
        }
    }
​
    occlusion *= INTENSITY / (NUM_SAMPLE_DIRECTIONS * NUM_SAMPLE_STEPS);
    occlusion = clamp(occlusion, 0.0, 1.0);
​
    fragmentColor = vec4(vec3(occlusion), 1.0);
}
```

_TBD: добавить схему и скрины_

## Некоторые оптимизации

### Texture handles

### glMultiDrawIndirect

### Совмещение буферов

### Uniform Buffer Object

### Shared Storage Buffer Object

## Материалы

### Тени
 
* [Percentage-closer soft shadows](https://developer.download.nvidia.com/shaderlibrary/docs/shadow_PCSS.pdf)
* [Percentage-closer soft shadows](https://andrew-pham.blog/2019/08/03/percentage-closer-soft-shadows/)
* [Hybrid frustum traced shadows](https://developer.nvidia.com/hybrid-frustum-traced-shadows-0)
* [Hybrid frustum traced shadows](https://www.mcvuk.com/business-news/pc/how-to-implement-hybrid-frustum-traced-shadows/)
* [Hybrid frustum traced shadows](https://www.gamedev.net/forums/topic/678813-hybrid-frustum-traced-shadows/?page=2)
* [Voxel cone tracing](https://research.nvidia.com/sites/default/files/pubs/2011-09_Interactive-Indirect-Illumination/GIVoxels-pg2011-authors.pdf)
* [Voxel cone tracing](https://on-demand.gputechconf.com/gtc/2012/presentations/SB134-Voxel-Cone-Tracing-Octree-Real-Time-Illumination.pdf)
* [Voxel cone tracing](https://andrew-pham.blog/2019/07/29/voxel-cone-tracing/)
* [Sample distribution shadow mapping](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.360.3911&amp;rep=rep1&amp;type=pdf)
* [Reversed-z depth maps](https://developer.nvidia.com/content/depth-precision-visualized)
* [Advanced geometrically correct shadow mapping](https://www.gdcvault.com/play/1023518/Advanced-Geometrically-Correct-Shadows-for)
* [Forward shadow mapping](https://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.48.2200&amp;rep=rep1&amp;type=pdf)
* [Moment shadow mapping](https://www.gdcvault.com/play/1023864/Rendering-Antialiased-Shadows-with-Moment)
* [Exponential shadow mapping](https://jankautz.com/publications/esm_gi08.pdf%20https://www.trentreed.net/blog/exponential-shadow-maps/)
* [Variance shadow mapping](https://developer.nvidia.com/gpugems/gpugems3/part-ii-light-and-shadows/chapter-8-summed-area-variance-shadow-maps)
        
### Освещение

* [Reflective shadow maps](https://andrew-pham.blog/2019/08/29/reflective-shadow-maps/)
* [Light propagation volumes](https://docs.unrealengine.com/4.27/en-US/BuildingWorlds/LightingAndShadows/LightPropagationVolumes/)
* [Light propagation volumes](https://ericpolman.com/2016/06/28/light-propagation-volumes/)
* [Approximate hybrid ray tracing](https://gamedev.net/forums/topic/655166-approximate-hybrid-raytracing-work-in-progress/5144413/)
* [Approximate hybrid ray tracing](http://santiagopportfolio.blogspot.com/2014/08/approximate-hybrid-raytracing-update.html)
* [Voxel-based global illumination](https://wickedengine.net/2017/08/30/voxel-based-global-illumination/)
* [Voxel-based global illumination](https://on-demand.gputechconf.com/gtc/2014/presentations/S4552-rt-voxel-based-global-illumination-gpus.pdf)
* [Voxel-based global illumination](https://jose-villegas.github.io/post/deferred_voxel_shading/)
* [Volumetric light](https://andrew-pham.blog/2019/10/03/volumetric-lighting/)
* [Volumetric light scattering](https://www.programmersought.com/article/68075912719/)
* [Volumetric light scattering](https://www.alexandre-pestana.com/volumetric-lights/)

### Ambient occlusion

* [Ground truth ambient occlusion](https://iryoku.com/downloads/Practical-Realtime-Strategies-for-Accurate-Indirect-Occlusion.pdf)
* [Ground truth ambient occlusion](https://www.activision.com/cdn/research/Practical_Real_Time_Strategies_for_Accurate_Indirect_Occlusion_NEW%20VERSION_COLOR.pdf)
* [Ground truth ambient occlusion](https://bestofcpp.com/repo/GameTechDev-XeGTAO)
* [Horizon-based ambient occlusion](https://dl.acm.org/doi/10.1145/1401032.1401061)

### Отражения

* [Screen-space reflection](https://www.reddit.com/r/gamedev/comments/52lawa/what_exactly_are_screenspace_reflections/)
* [Screen-space reflection](https://lettier.github.io/3d-game-shaders-for-beginners/screen-space-reflection.html)

### Anti-aliasing

* [Multi-frame sampled anti aliasing](https://www.nvidia.com/en-us/geforce/news/multi-frame-sampled-anti-aliasing-delivers-better-performance-and-superior-image-quality/)
* [Multi-frame sampled anti aliasing](https://forums.tomshardware.com/threads/how-does-mfaa-work.3489534/)
* [Temporal antialiasing](https://ziyadbarakat.wordpress.com/2020/07/28/temporal-anti-aliasing-step-by-step/)
* [FXAA](https://www.techspot.com/article/2219-how-to-3d-rendering-anti-aliasing/)

### Техники рендеринга в играх

* [Rendering frame in Red Dead Redemption 2](https://imgeself.github.io/posts/2020-06-19-graphics-study-rdr2/)
* Ведьмак 3
    * [https://habr.com/ru/post/422573/](https://habr.com/ru/post/422573/) 
    * [https://habr.com/ru/post/437100/](https://habr.com/ru/post/437100/) 
    * [https://habr.com/ru/post/450332/](https://habr.com/ru/post/450332/)
* [Cyberpunk 2077](https://3-info.ru/post/24194)
