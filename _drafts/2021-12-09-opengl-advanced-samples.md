---
layout: post
title: "OpenGL: advanced samples"
---

For a long time I was keen in learning low-level computer graphics APIs and algorithms, but only recently actually made any progress towards that goal.
I have spent few weeks if not months learning about shaders, graphics pipeline and rendering techniques, so this write-up was long forecoming.
There might be some mistakes, since I am not an expert in CG and OpenGL and there is a big chunk missing, namely rendering animated 3D models, compute shaders and tesselation, but I hope to fix those in the future.

In this blog I will try to condense the knowledge I have acquired. I will skip most basic parts such as initializing context, creating window and so on, since
most of the tutorials and articles on the Internet focus on those topics. Unlike most tutorials and articles out there, this blog will be all about _rendering techniques_.

This article was heavily inspired by few blogs on Russian website ([Habr](https://habr.com/)), namely "super-modern OpenGL" ([part 1](https://habr.com/ru/post/456932/) and [part 2](https://habr.com/ru/post/457380/)) - they are quite messy and lack a lot of material on really interesting topics (again, rendering techniques). This blog partially builds on top of those two and uses a lot more other materials (references provided).

## Setting up

Just to get this out of the way: I have used few available libraries to simplify my work.
You can totally use anything else in your projects with greater success, these were just my decisions which seemed right for me at that time.

* [SFML](https://github.com/SFML/SFML) to manage the window creation, input and loading images
* [globjects](https://github.com/cginternals/globjects) to simplify some of the OpenGL entity management (framebuffers, textures, shader programs, vertex attribute objects and buffers)
* [glm](https://github.com/g-truc/glm) for maths (algebra and matrices, predominantly)
* [assimp](https://github.com/assimp/assimp) to load 3D models

The project uses [CMake](https://cmake.org/) to handle the build process and [vcpkg](https://github.com/microsoft/vcpkg) to manage the dependencies. I have also tried [xmake]() but because it lacks IDE support (I have used Visual Studio), it did not quite fit me.

In these projects I have tried to utilize C++20 as much as possible, but I might be missing few great improvements it gives (mostly around vectors and memory ownership).

I also have implemented a sample of using [imgui](https://github.com/ocornut/imgui) for user interface way down the line as I thought it was not all that helpful for my projects, but apparently, I could have saved myself a lot of debugging time if I added few windows and buttons here and there. Hence I encourage you to look into that too.

## Basics I promised not to focus on

Again, to get this out of the way and not go into too much detail on these topics, few words on how I approached OpenGL from my very much outdated background (think OpenGL ver. 1).


### Getting started with SFML

Creating a window and handling user input pretty much remained the same:

```cpp
#include <SFML/Window.hpp>

int main()
{
#ifdef SYSTEM_DARWIN
    auto videoMode = sf::VideoMode(2048, 1536);
#else
    auto videoMode = sf::VideoMode(1024, 768);
#endif

    sf::Window window(videoMode, "Hello SFML Window!");

    while (window.isOpen())
    {
        sf::Event event {};

        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
            {
                window.close();
            }
        }
    }

    return 0;
}
```

### Getting started with globjects

The approach to rendering has changed drastically in past decades: from passing each vertex and its attributes to OpenGL it has grown to better suit modern graphics cards' architectures and now looks like passing chunks of data to be stored in the graphics memory and calling graphics driver to run pre-compiled programs on the GPU.

In order to run the program on GPU, it first needs to be compiled from the source. And just like any ordinary program for CPU, the program for GPU (called "shader program" or just "shader") has multiple stages when it runs - you can think about these stages as stages in a big data processing pipeline - you get some input and tell your executor (GPU in this case) how to process and transform the data. This pipeline, however, has few pre-defined stages with pre-defined (to some extent) input and output formats. The stages we will be using in all our samples are:

1. **vertex shader stage** - takes a bunch of vertices with bunch of attributes as flat integer or float numbers or packs of 2, 3 or 4 of those numbers, aka vectors; it transforms  those vertices (if needed, generally transforms vertices as if they were seen by the "camera") and returns each vertex with attributes assigned to each vertex (the attributes could be arbitrary - numbers or vectors)
2. **geometry shader stage** - takes each vertex and produces the geometry it will be part of (think lines, triangles or polygons); returns new vertices with vertex attributes assigned to each vertex (numbers or vectors)
3. **fragment shader stage** - takes each geometry (line or triangle) formed by multiple vertices with attributes assigned to each vertex and projects them onto screen (or texture, aka "render target"), returning a pixel data suitable for each render target (one target - one type of pixel data accepted)

There are more of those stages, but those are the most commonly used ones. Each of them is customizable by a separate shader. That is, OpenGL will throw an error if you try to return fragment shader data from vertex shader.

The way you render anything on the screen is: you take a bunch of vertices in 3D space, assign them some data (like the color or normal or texture coordinates), pack them into a buffer object (that is, just a buffer full of data). You then have to tell OpenGL what the data format is. This is done using vertex attribute object - this is essentially a descriptor, telling OpenGL how it should read and process the bytes stored in the buffer.

Here's a comprehensive example of a simple shader initialization and rendering:

```cpp
#include <fstream>
#include <iostream>

#include <glbinding/gl/gl.h>

#include <globjects/Buffer.h>
#include <globjects/Error.h>
#include <globjects/Program.h>
#include <globjects/Shader.h>
#include <globjects/VertexArray.h>
#include <globjects/VertexAttributeBinding.h>
#include <globjects/base/File.h>
#include <globjects/base/StaticStringSource.h>
#include <globjects/globjects.h>

#include <glm/vec2.hpp>

#include <SFML/OpenGL.hpp>
#include <SFML/Window.hpp>

#ifdef WIN32
using namespace gl;
#endif

int main()
{
    sf::ContextSettings settings;
    settings.depthBits = 24;
    settings.stencilBits = 8;
    settings.antialiasingLevel = 4;
    settings.majorVersion = 3;
    settings.minorVersion = 2;
    settings.attributeFlags = sf::ContextSettings::Attribute::Core;

#ifdef SYSTEM_DARWIN
    auto videoMode = sf::VideoMode(2048, 1536);
#else
    auto videoMode = sf::VideoMode(1024, 768);
#endif

    sf::Window window(videoMode, "Hello OpenGL!", sf::Style::Default, settings);

    globjects::init([](const char* name) {
        return sf::Context::getFunction(name);
    });

    globjects::DebugMessage::enable(); // enable automatic messages if KHR_debug is available

    globjects::DebugMessage::setCallback([](const globjects::DebugMessage& message) {
        std::cout << "[DEBUG] " << message.message() << std::endl;
    });

    std::cout << "[INFO] Initializing..." << std::endl;

    auto cornerBuffer = globjects::Buffer::create();

    std::cout << "[INFO] Creating shaders..." << std::endl;

    std::cout << "[INFO] Compiling vertex shader...";

    auto vertexShaderSource = globjects::Shader::sourceFromFile("media/vertex.glsl");
    auto vertexShaderTemplate = globjects::Shader::applyGlobalReplacements(vertexShaderSource.get());
    auto vertexShader = globjects::Shader::create(static_cast<gl::GLenum>(GL_VERTEX_SHADER), vertexShaderTemplate.get());

    std::cout << "done" << std::endl;

    std::cout << "[INFO] Compiling fragment shader...";

    auto fragmentShaderSource = globjects::Shader::sourceFromFile("media/fragment.glsl");
    auto fragmentShaderTemplate = globjects::Shader::applyGlobalReplacements(fragmentShaderSource.get());
    auto fragmentShader = globjects::Shader::create(static_cast<gl::GLenum>(GL_FRAGMENT_SHADER), fragmentShaderTemplate.get());

    std::cout << "done" << std::endl;

    std::cout << "[INFO] Linking shader programs...";

    auto renderingProgram = globjects::Program::create();
    renderingProgram->attach(vertexShader.get(), fragmentShader.get());

    std::cout << "done" << std::endl;

    std::cout << "[INFO] Creating VAO...";

    auto vao = globjects::VertexArray::create();

    cornerBuffer->setData(
        std::array<glm::vec2, 4> {
            { glm::vec2(0, 0), glm::vec2(1, 0), glm::vec2(0, 1), glm::vec2(1, 1) }
        },
        static_cast<gl::GLenum>(GL_STATIC_DRAW));

    vao->binding(0)->setAttribute(0);
    vao->binding(0)->setBuffer(cornerBuffer.get(), 0, sizeof(glm::vec2));
    vao->binding(0)->setFormat(2, static_cast<gl::GLenum>(GL_FLOAT));
    vao->enable(0);

    std::cout << "done" << std::endl;

    std::cout << "[INFO] Done initializing" << std::endl;

    while (window.isOpen())
    {
        sf::Event event {};

        while (window.pollEvent(event))
        {
            if (event.type == sf::Event::Closed)
            {
                window.close();
                break;
            }
        }

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        ::glViewport(0, 0, static_cast<GLsizei>(window.getSize().x), static_cast<GLsizei>(window.getSize().y));

        renderingProgram->use();

        vao->drawArrays(static_cast<gl::GLenum>(GL_TRIANGLE_STRIP), 0, 4);

        renderingProgram->release();

        window.display();
    }

    return 0;
}
```

vertex shader:

```glsl
#version 410

layout (location = 0) in vec2 corner;

out vec4 color;

out gl_PerVertex
{
    vec4 gl_Position;
};

void main()
{
    gl_Position = vec4(corner * 2.0 - 1.0, 0.0, 1.0);
    color = vec4(corner, 0.0, 1.0);
}
```

fragment shader:

```glsl
#version 410

layout (location = 0) out vec4 fragColor;

in vec4 color;

void main()
{
    fragColor = color;
}
```

### Camera and getting started with glm

A lot of operations are described as mathematical operations on bunch of vectors with the use of matrices.

Most of the transformations could be done on either CPU (think C++ code) or GPU (think shader code). But keep in mind: if you do a certain operation in vertex shader - it will be executed for each vertex you render with that shader. If you do it in fragment shader - it will be executed for each pixel rendered. Those numbers might be really big and vastly different (for a simple scene with just a plane - `4` vertices vs `1024 * 768 = 786432` pixels, for a big scene - say `2K` objects with `3M` vertices = `2*10^3 * 3*10^6 = 6*10^9` vertices vs same `786432` pixels).

GLSL comes packed with quite a lot of mathematical operations already, whilst for C++ we can use `glm` to handle all those nitty-gritty matrix multiplications and inverse matrix calculations.

In order to communicate some data from the CPU to the shader running on GPU, you can use uniform variables.

Here's a simple example of camera implementation with glm:

```cpp
#include <glm/ext/matrix_clip_space.hpp>
#include <glm/ext/matrix_transform.hpp>
#include <glm/gtx/rotate_vector.hpp>
#include <glm/mat4x4.hpp>
#include <glm/vec2.hpp>
#include <glm/vec3.hpp>

// ...

int main()
{
    // ...

    const float fov = 45.0f;

    const float cameraMoveSpeed = 1.0f;
    const float cameraRotateSpeed = 10.0f;

    glm::vec3 cameraPos = glm::vec3(0.0f, 0.0f, 3.0f);
    glm::vec3 cameraUp = glm::vec3(0.0f, 1.0f, 0.0f);
    glm::vec3 cameraRight = glm::vec3(1.0f, 0.0f, 0.0f);
    glm::vec3 cameraForward = glm::normalize(glm::cross(cameraUp, cameraRight));

    sf::Clock clock;

    // ...

    while (window.isOpen())
    {
        // measure time since last frame, in seconds
        float deltaTime = static_cast<float>(clock.restart().asSeconds());

        glm::vec2 currentMousePos = glm::vec2(sf::Mouse::getPosition(window).x, sf::Mouse::getPosition(window).y);
        glm::vec2 mouseDelta = currentMousePos - glm::vec2((window.getSize().x / 2), (window.getSize().y / 2));
        sf::Mouse::setPosition(sf::Vector2<int>(window.getSize().x / 2, window.getSize().y / 2), window);

        float horizontalAngle = (mouseDelta.x / static_cast<float>(window.getSize().x)) * -1 * deltaTime * cameraRotateSpeed * fov;
        float verticalAngle = (mouseDelta.y / static_cast<float>(window.getSize().y)) * -1 * deltaTime * cameraRotateSpeed * fov;

        cameraForward = glm::rotate(cameraForward, horizontalAngle, cameraUp);
        cameraForward = glm::rotate(cameraForward, verticalAngle, cameraRight);

        cameraRight = glm::normalize(glm::rotate(cameraRight, horizontalAngle, cameraUp));

        if (sf::Keyboard::isKeyPressed(sf::Keyboard::W))
        {
            cameraPos += cameraForward * cameraMoveSpeed * deltaTime;
        }

        if (sf::Keyboard::isKeyPressed(sf::Keyboard::S))
        {
            cameraPos -= cameraForward * cameraMoveSpeed * deltaTime;
        }

        if (sf::Keyboard::isKeyPressed(sf::Keyboard::A))
        {
            cameraPos -= glm::normalize(glm::cross(cameraForward, cameraUp)) * cameraMoveSpeed * deltaTime;
        }

        if (sf::Keyboard::isKeyPressed(sf::Keyboard::D))
        {
            cameraPos += glm::normalize(glm::cross(cameraForward, cameraUp)) * cameraMoveSpeed * deltaTime;
        }

        glm::mat4 projection = glm::perspective(glm::radians(fov), (float) window.getSize().x / (float) window.getSize().y, 0.1f, 100.0f);

        glm::mat4 view = glm::lookAt(
            cameraPos,
            cameraPos + cameraForward,
            cameraUp);

        renderProgram->setUniform("model", model);
        renderProgram->setUniform("view", view);
        renderProgram->setUniform("projection", projection);

        glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

        ::glViewport(0, 0, static_cast<GLsizei>(window.getSize().x), static_cast<GLsizei>(window.getSize().y));

        renderProgram->use();

        vao->drawArrays(static_cast<gl::GLenum>(GL_TRIANGLE_STRIP), 0, 4);

        renderProgram->release();

        window.display();
    }
}
```

vertex shader:

```glsl
#version 410

layout (location = 0) in vec3 vertexPos;

out vec4 color;

out gl_PerVertex
{
    vec4 gl_Position;
};

uniform mat4 model;
uniform mat4 view;
uniform mat4 projection;

void main()
{
    gl_Position = projection * view * model * vec4(vertexPos, 1.0);
    color = vec4(vertexPos, 1.0);
}
```

fragment shader:

```glsl
#version 410

layout (location = 0) out vec4 fragColor;

in vec4 color;

void main()
{
    fragColor = color;
}
```

## Optimization techniques

### Passing data to the shader

Bear in mind: if you want to pass just few bits of data - it is fine to use few uniforms, but if you want to pass a lot of data (think few matrices and lists of data) - you're much better off using buffers. That is, uniform buffer objects (UBO) or even better - shader shared buffer objects (SSBO) - the latter can store much larger amounts of data (according to OpenGL standard guarantees, UBO can store up to `16 kB` whilst SSBO - up to `128 MB`, but most drivers will let you store up to graphic card's memory limit) and you can have arrays of data in SSBOs. Coupled with user-defined data structures (think `struct` but in GLSL), you can pass, for example, list of light sources' descriptors to your shader with SSBO.

Here's how you can do it:

```cpp
struct alignas(16) PointLightDescriptor
{
    glm::vec3 position;
    float strength;
    glm::vec4 color;
};

int main()
{
    // ...

    std::vector<PointLightDescriptor> pointLights{ { glm::vec3(-1.75f, 3.85f, -0.75f), 0.5f, glm::vec4(1.0f, 1.0f, 1.0f, 1.0f) } };

    auto pointLightDataBuffer = std::make_unique<globjects::Buffer>();

    pointLightDataBuffer->setData(pointLights, static_cast<gl::GLenum>(GL_DYNAMIC_COPY));

    // ...

    while (window->isOpen())
    {
        pointLightDataBuffer->bindBase(GL_SHADER_STORAGE_BUFFER, 5);

        // bind a shader

        pointLightDataBuffer->unbind(GL_SHADER_STORAGE_BUFFER, 5);
    }
}
```

fragment shader:

```glsl
struct PointLight
{
    vec3 position;
    float strength;
    vec4 color;
};

layout (std430, binding = 5) buffer PointLightData
{
    PointLight pointLight[];
} pointLightData;

void main()
{
    for (int i = 0; i < pointLightData.pointLight.length(); ++i)
    {
        PointLight light = pointLightData.pointLight[i];

        vec3 lightDirection = normalize(light.position - fragmentPosition);
    }
}
```

### Deferred rendering

Rendering directly to the output (window), also known as "direct rendering", is not very optimal with most techniques (especially post-processing and special effects).
For the most part I will try to use technique called "deferred rendering", where you first render your entire scene into multiple textures with the same size as the output window and then you apply various techniques to those textures - this way you do not have to render entire scene multiple times.

Since you can have multiple render targets, you can render just one fragment (pixel) attribute to one render target and then use it in local post-processing calculations.
Trick is: you render scene once. In the geometry shader, you render each vertex' data to a different render target. Then you will not need to process each vertex, but each pixel instead. If you want to calculate shadows or reflections or bloom - you will only have to do it for each pixel, not for each vertex, which might save you heaps of GPU resources.

### Batch geometry rendering

You can pack the vertex data for your scene however you want. For example, you can have one buffer with just the vertices' positions and another buffer with just vertices' normals (and so on for each of the vertex attributes). But if the geometry you are trying to render is not changing with time, you can store all the data in one buffer - you just have to tell OpenGL how the bytes are packed in the buffer. For example, you can have position, followed by normal, followed by texture coordinates. You just tell OpenGL to have three attributes - one of the size of 3 floats, the next one of the size of three floats and the last one of the size of two floats.

Moreover, you can store multiple objects' vertices in one buffer! The reason for that would be to minimize the number of buffer and shader switching OpenGL has to make in order to render a certain amount of geometry.

You can combine rendering multiple different geometries with instanced rendering and use `glMultiDrawElementsIndirect` with a list of "drawing commands" to render a whole lot of geometry from buffers stored in GPU memory.

Top it up with the fact that you can store the draw commands in a buffer and you can generate this buffer using compute shaders (not covered yet) and you can have a rendering engine which works almost entirely on the GPU, saving you a lot of synchronization and communication between CPU and GPU.

## Rendering techniques

### Simple shadow mapping

The simplest idea of shadow mapping is: you render a scene depths (each pixel represents a distance from camera to the object) from the perspective of a light source to a separate render target. Then you render your scene normally from the perspective of a camera and for each pixel you compare its distance to the light source (take position of a pixel and subtract position of a light source from it) - if scene pixel is further from the light than the same pixel' depth in the light space - there's some other thing blocking the light, so this pixel is in the shadow.

### Percentage-close filtering

Percentage-close filtering, or PCF, is a technique of smoothing out those hard-edge shadows. The idea is that you do not calculate whether the pixel (say, on a surface) will be in shadow or not, but rather calculate the probability of that pixel being in shadow. If the probability is high enough - you treat it as if it was in the shadow. This allows you to get somewhat soft shadows.

### Cascaded shadow mapping

This is rather quality- and performance-optimization technique. It could also be used for not just shadow mapping, but for various objects rendering (like rendering grass, small terrain details, terrain itself, etc.). The idea is that there is no need to render high-detailed shadows further from the camera. Assume you have a large landscape. If you render detailed shadows for an entire landscape to rather limited texture - you will end up having each object have big pixelated shadow. Whilst you want objects closer to the camera look sharp and nice. And the trees in the distance - well, there is no need for them to have that nice shadows.

Thus, you "divide" your camera space into few sections (aka "cascades") and render shadows for each of those cascade separately, using the same size of the shadow map texture. The trick is that the cascade closest to the camera is the smallest, so the shadows have very high level of detail. The section of a camera space a bit further away is larger, but since the shadow map has the same size - the quality of shadows is lower. The furthest section of the camera space has the largest size, so the quality of the shadows in the same size of the shadow map will be the worst. But since the objects in that section are also furthest away from the camera - they don't have to have the most detailed shadows. In fact, you can go even further and only split half of camera space into three (or more, if you will) cascades, leaving the other half, the furthest one from the camera, completely out of the shadow mapping process. This way the objects far away won't even have any shadows at all.

### Multi-sampled anti-aliasing

_TBD_

### Fast-approximation anti-aliasing

_TBD_

### Screen-space ambient occlusion

_TBD_

### Horizon-based ambient occlusion

_TBD_

### Raymarching

_TBD_

## Common rendering techniques

### Bloom

_TBD_

### Particle systems

Particle simulation is not really a technique in itself - it is a very big topic (how you design it from the code perspective, how do you simulate particles optimally - concurrent programming, SIMD, calculations on the GPU, etc.). But the way you render bunch of particles could be qualified as a rendering technique. It is mostly about instanced rendering.

### Skybox

_TBD_

### Point lighting

_TBD_

### Reflections

_TBD_
