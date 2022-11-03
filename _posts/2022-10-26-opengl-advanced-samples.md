---
layout: post
title: "OpenGL: advanced samples"
date: "26-10-2022T00:00:00+10:00"
---

Damn this blog was long coming - I picked my interest in shaders over a decade ago, started to actually learn something in early 2021 and started writing this blog at the end of 2021. Now the time has finally come to share this with the world!

For a long time I was keen in learning low-level computer graphics APIs and algorithms, but only recently actually made any progress towards that goal.
I have spent few weeks if not months learning about shaders, graphics pipeline and rendering techniques, so this write-up was long forecoming.
There might be some mistakes, since I am not an expert in CG and OpenGL and there is a big chunk missing, namely rendering animated 3D models, compute shaders and tesselation, but I hope to fix those in the future.

One might ask: why OpenGL? Why not Vulkan (or DirectX 12)? Why even bother with these emulaiton techniques - why not jump straight to raytracing?
Well, it is the `end of October 2022` at the moment of publishing this blog, the most recent GPU on the market is GeForce RTX 4090, which is yet to arrive and its RRP (recommended retailer price) is `A$2959` (as advertised by nVidia):

<img data-src="/images/opengl-advanced-samples/geforce-rtx-4090-price-oct-2022.webp" alt="RTX4090 price as of October 2022" />

And most of those cards are gone in a blink of an eye and used for mining whatever-coins. My PC runs GTX1080, which is still more than enough for most of the games I play (being Genshin Impact at most).

I think it is still not a widely spread consumer market, and whilst raytracing, DLSS and all those fine things sound super cool and nice, it is still a niche market. That is not to say I won't be looking into that in the near future. I just think OpenGL still has place in this world, at least as a starting point to build a decent understanding of the modern graphics pipeline, some basic rendering techniques and justify switching to more recent APIs. OpenGL as well as DirectX 11 are still a very feasible fallback for those sytems which do not run top-notch hardware or are struggling running those massive open-world games in anything higher than 1080p@60fps.

In this blog I will try to condense the knowledge I have acquired. I will skip most basic parts such as initializing context, creating window and so on, since
most of the tutorials and articles on the Internet focus on those topics. Unlike most tutorials and articles out there, this blog will be all about _rendering techniques_.

This article was heavily inspired by few blogs on russian website ([Habr](https://habr.com/)), namely "super-modern OpenGL" ([part 1](https://habr.com/ru/post/456932/) and [part 2](https://habr.com/ru/post/457380/)) - they are quite messy and lack a lot of material on really interesting topics (again, rendering techniques). This blog partially builds on top of those two and uses a lot more other materials (references provided).

This blog is enormous, so blow is the table of contents for the topics covered. Feel free to jump straight to the topic that picks your interest, screenshots are provided:

* [Setting up](#setting-up)
* [Basics I promised not to focus on](#basics-i-promised-not-to-focus-on)
    * [Getting started with SFML](#getting-started-with-sfml)
    * [Getting started with globjects](#getting-started-with-globjects)
    * [Camera and getting started with glm](#camera-and-getting-started-with-glm)
* [Optimization techniques](#optimization-techniques)
    * [Passing data to the shader](#passing-data-to-the-shader)
    * [Deferred rendering](#deferred-rendering)
    * [Batch geometry rendering](#batch-geometry-rendering)
    * [Texture handles](#texture-handles)
    * [Rendering to different textures in geometry shader](#rendering-to-different-textures-in-geometry-shader)
* [Rendering techniques](#rendering-techniques)
    * [Shadow mapping](#shadow-mapping)
        * [Biasing](#biasing)
        * [Percentage-close filtering](#percentage-close-filtering)
        * [Variance shadow mapping](#variance-shadow-mapping)
        * [Cascaded shadow mapping](#cascaded-shadow-mapping)
    * [Anti-aliasing](#anti-aliasing)
        * [Multi-sampled anti-aliasing (MSAA)](#multi-sampled-anti-aliasing-msaa)
        * [Fast-approximation anti-aliasing (FXAA)](#fast-approximation-anti-aliasing-fxaa)
    * Ambient occlusion
        * [Screen-space ambient occlusion (SSAO)](#screen-space-ambient-occlusion-ssao)
        * [Horizon-based ambient occlusion (HBAO)](#horizon-based-ambient-occlusion-hbao)
    * [Volumetric lighting](#volumetric-lighting)
* [Common rendering techniques](#common-rendering-techniques)
    * [Bloom](#bloom)
    * [Particle systems](#particle-systems)
    * [Skybox](#skybox)
    * [Point lighting](#point-lighting)
    * [Reflections](#reflections)

The source code is available on [GitHub](https://github.com/shybovycha/opengl-samples/).

<!--more-->

## Setting up

Just to get this out of the way: I have used few available libraries to simplify my work.
You can totally use anything else in your projects with greater success, these were just my decisions which seemed right for me at that time.

* [SFML](https://github.com/SFML/SFML) to manage the window creation, input and loading images
* [globjects](https://github.com/cginternals/globjects) to simplify some of the OpenGL entity management (framebuffers, textures, shader programs, vertex attribute objects and buffers)
* [glm](https://github.com/g-truc/glm) for maths (algebra and matrices, predominantly)
* [assimp](https://github.com/assimp/assimp) to load 3D models

The project uses [CMake](https://cmake.org/) to handle the build process and [vcpkg](https://github.com/microsoft/vcpkg) to manage the dependencies. I have also tried [xmake](https://xmake.io/) but because it lacks IDE support _(I have used Visual Studio)_, it did not quite fit me.

In these projects I have tried to utilize the new C++ features _(C++17 onwards)_ as much as possible, but I might be missing few great improvements it gives _(mostly around containers and memory ownership)_.

I also have implemented a sample of using [imgui](https://github.com/ocornut/imgui) for user interface way down the line as I thought it was not all that helpful for my projects, but apparently, I could have saved myself a lot of debugging time if I added few windows and buttons here and there. Hence I encourage you to look into that too.

## Basics I promised not to focus on

Again, to get this out of the way and not go into too much detail on these topics, few words on how I approached OpenGL from my very much outdated background _(think OpenGL ver. 1)_.

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

        glm::mat4 projection = glm::perspective(
            glm::radians(fov),
            (float) window.getSize().x / (float) window.getSize().y,
            0.1f,
            100.0f);

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

    std::vector<PointLightDescriptor> pointLights{
        { glm::vec3(-1.75f, 3.85f, -0.75f), 0.5f, glm::vec4(1.0f, 1.0f, 1.0f, 1.0f) }
    };

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

To understand how the `glMultiDrawElementsIndirect` works, one must understand the concept of a draw command and how it works in DirectX 12 and Vulkan.
The idea is that you prepare all the geometry and the supporting data, store it in video memory and then only tell GPU to render all that mess in a specific order.
This way you don't need to send huge chunks of data from CPU to GPU every frame - this spares computer quite some work.

The draw command describes which chunks of data that are already stored in video memory to use for rendering and how to render them.

A typical draw command is represented as the following C++ structure:

```cpp
struct StaticGeometryDrawCommand
{
    unsigned int elementCount; // number of elements (triangles) to be rendered for this object
    unsigned int instanceCount; // number of object instances
    unsigned int firstIndex; // offset into GL_ELEMENT_ARRAY_BUFFER
    unsigned int baseVertex; // offset of the first object' vertex in the uber-static-object-buffer
    unsigned int baseInstance; // offset of the first instance' per-instance-vertex-attributes; attribute index is calculated as: (gl_InstanceID / glVertexAttribDivisor()) + baseInstance
};
```

The issue with this approach is: how to pass the per-instance data? Since all the data used by `glDrawMulti*` is per-vertex.
SSBOs to the rescue!

```cpp
struct alignas(16) StaticObjectData
{
    glm::vec2 albedoTextureSize;
    glm::vec2 normalTextureSize;
    glm::vec2 emissionTextureSize;
    unsigned int instanceDataOffset;
};

struct alignas(16) StaticObjectInstanceData
{
    glm::mat4 transformation;
};

// create two arrays: per-object data and per-instance data

std::vector<StaticObjectData> m_objectData;
std::vector<StaticObjectInstanceData> m_objectInstanceData;

// duck
m_objectData.push_back({
    // TODO: automate this
    .albedoTextureSize = glm::vec2(0, 0),
    .normalTextureSize = glm::vec2(0, 0),
    .emissionTextureSize = glm::vec2(0, 0),
    .instanceDataOffset = 0
});

m_objectInstanceData.push_back({ .transformation = glm::mat4() });

// lantern
m_objectData.push_back({ .albedoTextureSize = glm::vec2(1024, 1024),
                            .normalTextureSize = glm::vec2(1024, 1024),
                            .emissionTextureSize = glm::vec2(0, 0),
                            .instanceDataOffset = 1 });

m_objectInstanceData.push_back({ .transformation = glm::translate(glm::vec3(0.0f, 0.5f, 0.0f)) });

// scroll
m_objectData.push_back({ .albedoTextureSize = glm::vec2(1024, 1024),
                            .normalTextureSize = glm::vec2(0, 0),
                            .emissionTextureSize = glm::vec2(1024, 1024),
                            .instanceDataOffset = 2 });

m_objectInstanceData.push_back({ .transformation = glm::translate(glm::vec3(0.0f, 0.5f, -0.5f)) });

// pen
m_objectData.push_back({ .albedoTextureSize = glm::vec2(512, 512),
                            .normalTextureSize = glm::vec2(512, 512),
                            .emissionTextureSize = glm::vec2(0, 0),
                            .instanceDataOffset = 3 });

m_objectInstanceData.push_back({ .transformation = glm::translate(glm::vec3(0.5f, 0.5f, 0.5f)) });

// generate object data buffer
auto objectDataBuffer = std::make_unique<globjects::Buffer>();

objectDataBuffer->setData(m_objectData, static_cast<gl::GLenum>(GL_DYNAMIC_COPY));

// generate object **instance** data buffer
auto objectInstanceDataBuffer = std::make_unique<globjects::Buffer>();

objectInstanceDataBuffer->setData(m_objectInstanceData, static_cast<gl::GLenum>(GL_DYNAMIC_COPY));
```

Note how object data contains the texture sizes - this is done so that all the textures can be stored in one texture array (aka 3D texture).
This allows for easy texture indexing (using object index instead of having some arbitrary number of texture variables), making this approach extensible.
But texture arrays have a limitation: you can have as many textures as you would like in one array, but all textures must have same width and height.
However, that does not mean all the pixels matter - you can store smaller textures using their original size and just ignore the rest of the texture data.
But in order to do that, you would need to sample the texture with a scale factor. See the shader code below for details.

Then, for even more performance boost, you can put all objects' data into one big data buffer - since we will use drawing commands to render,
we can not worry about different buffers. That is, each buffer's element will contain all the necessary vertex data - position, normal and UV coordinates.
However, we will still need a separate buffer for indices:

```cpp
std::vector<unsigned int> m_indices;

std::vector<StaticGeometryDrawCommand> m_drawCommands;

struct NormalizedVertex
{
    glm::vec3 position;
    glm::vec3 normal;
    glm::vec2 uv;
};

std::vector<NormalizedVertex> normalizedVertexData;
unsigned int baseVertex = 0;

for (auto& scene : scenes)
{
    for (auto& mesh : scene->meshes)
    {
        const auto numVertices = mesh.vertexPositions.size();

        for (size_t i = 0; i < numVertices; ++i)
        {
            auto position = mesh.vertexPositions[i];
            auto normal = mesh.normals[i];
            auto uv = mesh.uvs[i];

            normalizedVertexData.push_back({ .position = position, .normal = normal, .uv = uv });
        }

        m_indices.insert(m_indices.end(), mesh.indices.begin(), mesh.indices.end());

        StaticGeometryDrawCommand drawCommand {
            .elementCount = static_cast<unsigned int>(mesh.indices.size()),
            .instanceCount = 1, // TODO: generate commands dynamically whenever the data is changed
            .firstIndex = 0,
            .baseVertex = baseVertex,
            .baseInstance = 0
        };

        m_drawCommands.push_back(drawCommand);

        baseVertex += numVertices;
    }
}

auto vao = std::make_unique<globjects::VertexArray>();

// generate draw command buffer
auto drawCommandBuffer = std::make_unique<globjects::Buffer>();
drawCommandBuffer->setData(m_drawCommands, static_cast<gl::GLenum>(GL_DYNAMIC_DRAW)); // draw commands can technically be changed

// generate vertex data buffer
auto geometryDataBuffer = std::make_unique<globjects::Buffer>();
geometryDataBuffer->setData(normalizedVertexData, static_cast<gl::GLenum>(GL_STATIC_DRAW));

vao->binding(0)->setAttribute(0);
vao->binding(0)->setBuffer(geometryDataBuffer.get(), offsetof(NormalizedVertex, position), sizeof(NormalizedVertex)); // number of elements in buffer, stride, size of buffer element
vao->binding(0)->setFormat(3, static_cast<gl::GLenum>(GL_FLOAT)); // number of data elements per buffer element (vertex), type of data
vao->enable(0);

vao->binding(1)->setAttribute(1);
vao->binding(1)->setBuffer(geometryDataBuffer.get(), offsetof(NormalizedVertex, normal), sizeof(NormalizedVertex)); // number of elements in buffer, stride, size of buffer element
vao->binding(1)->setFormat(3, static_cast<gl::GLenum>(GL_FLOAT)); // number of data elements per buffer element (vertex), type of data
vao->enable(1);

vao->binding(2)->setAttribute(2);
vao->binding(2)->setBuffer(geometryDataBuffer.get(), offsetof(NormalizedVertex, uv), sizeof(NormalizedVertex)); // number of elements in buffer, stride, size of buffer element
vao->binding(2)->setFormat(2, static_cast<gl::GLenum>(GL_FLOAT)); // number of data elements per buffer element (vertex), type of data
vao->enable(2);

// generate element buffer
auto elementBuffer = std::make_unique<globjects::Buffer>();
elementBuffer->setData(m_indices, static_cast<gl::GLenum>(GL_STATIC_DRAW));

vao->bindElementBuffer(elementBuffer.get());
```

Rendering is then as simple as binding those buffers and calling `glMultiDrawElementsIndirect`:

```cpp
drawCommandBuffer->bind(static_cast<gl::GLenum>(GL_DRAW_INDIRECT_BUFFER));
objectDataBuffer->bindBase(static_cast<gl::GLenum>(GL_SHADER_STORAGE_BUFFER), 4);
objectInstanceDataBuffer->bindBase(static_cast<gl::GLenum>(GL_SHADER_STORAGE_BUFFER), 5);

vao->bind();

vao->multiDrawElementsIndirect(
    static_cast<gl::GLenum>(GL_TRIANGLES),
    static_cast<gl::GLenum>(GL_UNSIGNED_INT),
    0,
    m_drawCommands.size(),
    0);

simpleProgram->release();

drawCommandBuffer->unbind(static_cast<gl::GLenum>(GL_DRAW_INDIRECT_BUFFER));
objectDataBuffer->unbind(static_cast<gl::GLenum>(GL_SHADER_STORAGE_BUFFER), 4);
objectInstanceDataBuffer->unbind(static_cast<gl::GLenum>(GL_SHADER_STORAGE_BUFFER), 5);

vao->unbind();
```

Vertex shader:

```glsl
#version 460

layout (location = 0) in vec3 vertexPosition;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexTextureCoord;

out VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
    flat uint objectID;
    flat uint instanceID;
} vsOut;

struct ObjectData
{
    vec2 albedoTextureSize;
    vec2 normalTextureSize;
    vec2 emissionTextureSize;
    uint instanceDataOffset; // use this field to get instance data: StaticObjectInstanceData.transformation[StaticObjectData.objectData[gl_DrawID].instanceDataOffset + gl_InstanceID]
};

layout (std430, binding = 4) buffer StaticObjectData
{
    ObjectData[] objectData;
};

layout (std430, binding = 5) buffer StaticObjectInstanceData
{
    mat4[] transformations;
};

uniform mat4 projection;
uniform mat4 view;

void main()
{
    vsOut.fragmentPosition = vertexPosition;
    vsOut.normal = vertexNormal;
    vsOut.textureCoord = vec2(vertexTextureCoord.x, vertexTextureCoord.y);
    vsOut.objectID = gl_DrawID;
    vsOut.instanceID = gl_InstanceID;

    uint objectInstanceIndex = objectData[gl_DrawID].instanceDataOffset + gl_InstanceID;

    mat4 model = transformations[objectInstanceIndex];

    gl_Position = projection * view * model * vec4(vertexPosition, 1.0);
}
```

Fragment shader:

```glsl
#version 460

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
    flat uint objectID;
    flat uint instanceID;
} fsIn;

struct ObjectData
{
    vec2 albedoTextureSize;
    vec2 normalTextureSize;
    vec2 emissionTextureSize;
    uint instanceDataOffset;
};

layout (std430, binding = 4) buffer StaticObjectData
{
    ObjectData[] objectData;
};

uniform sampler2DArray albedoTextures;
uniform sampler2DArray normalTextures;
uniform sampler2DArray emissionTextures;

void main()
{
    vec2 maxTextureSize = textureSize(albedoTextures, 0).xy;
    vec2 uvFactor = objectData[fsIn.objectID].albedoTextureSize / maxTextureSize;
    vec2 uv = vec2(fsIn.textureCoord.x, fsIn.textureCoord.y) * uvFactor;

    vec4 albedo = texture(albedoTextures, vec3(uv.x, uv.y, fsIn.objectID));

    fragmentColor = albedo;
}
```

Read more:

* [[blog] Opengl 4.4 scene rendering techniques](https://on-demand.gputechconf.com/gtc/2014/video/S4379-opengl-44-scene-rendering-techniques.mp4)

### Texture handles

This optimization allows you to spare those `glActivateTexture` and referring textures by some magical numbers. You create a texture once and freeze its params by using the handles (so you can not change texture params once you have started using its handle), but what you gain is one handle, stored as a 64-bit number (roughly: texture address in GPU memory) which you can then pass to multiple shaders without bothering with those texture IDs.

```cpp
// create texture - nothing changes here
auto shadowMapTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));

shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_BORDER_COLOR), glm::vec4(1.0f, 1.0f, 1.0f, 1.0f));

shadowMapTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB10),
    glm::vec2(shadowMapSize, shadowMapSize),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr);

// copy texture to GPU memory; this could be done once (not on each frame render) to save some time
shadowMapTexture->textureHandle().makeResident();

// at this stage, texture parameters (dimensions, filtering, wrapping, etc.) can not be changed anymore - you would have to create a new texture

// remove texture from GPU memory
shadowMapTexture->textureHandle().makeNonResident();
```

### Rendering to different textures in geometry shader

A neat trick to render scene multiple times to different textures (for instance, rendering into cubemap) is to use geometry shader - you simply bind multi-layer texture (cubemap or a texture array) and use the `gl_Layer` output variable in the geometry shader to write to a specific texture layer:

C++ program:

```cpp
struct alignas(16) PointLightData
{
    glm::vec3 lightPosition;
    float farPlane;
    std::array<glm::mat4, 6> projectionViewMatrices;
};

// prepare the shader program with vertex, geometry and fragment shaders
auto pointShadowMappingVertexSource = globjects::Shader::sourceFromFile("media/shadow-mapping-point.vert");
auto pointShadowMappingVertexShaderTemplate = globjects::Shader::applyGlobalReplacements(pointShadowMappingVertexSource.get());
auto pointShadowMappingVertexShader = std::make_unique<globjects::Shader>(static_cast<gl::GLenum>(GL_VERTEX_SHADER), pointShadowMappingVertexShaderTemplate.get());

if (!pointShadowMappingVertexShader->compile())
{
    std::cerr << "[ERROR] Can not compile point shadow mapping vertex shader" << std::endl;
    return 1;
}

auto pointShadowMappingGeometrySource = globjects::Shader::sourceFromFile("media/shadow-mapping-point.geom");
auto pointShadowMappingGeometryShaderTemplate = globjects::Shader::applyGlobalReplacements(pointShadowMappingGeometrySource.get());
auto pointShadowMappingGeometryShader = std::make_unique<globjects::Shader>(static_cast<gl::GLenum>(GL_GEOMETRY_SHADER), pointShadowMappingGeometryShaderTemplate.get());

if (!pointShadowMappingGeometryShader->compile())
{
    std::cerr << "[ERROR] Can not compile point shadow mapping fragment shader" << std::endl;
    return 1;
}

auto pointShadowMappingFragmentSource = globjects::Shader::sourceFromFile("media/shadow-mapping-point.frag");
auto pointShadowMappingFragmentShaderTemplate = globjects::Shader::applyGlobalReplacements(pointShadowMappingFragmentSource.get());
auto pointShadowMappingFragmentShader = std::make_unique<globjects::Shader>(static_cast<gl::GLenum>(GL_FRAGMENT_SHADER), pointShadowMappingFragmentShaderTemplate.get());

if (!pointShadowMappingFragmentShader->compile())
{
    std::cerr << "[ERROR] Can not compile point shadow mapping fragment shader" << std::endl;
    return 1;
}

auto pointShadowMappingProgram = std::make_unique<globjects::Program>();
pointShadowMappingProgram->attach(pointShadowMappingVertexShader.get(), pointShadowMappingGeometryShader.get(), pointShadowMappingFragmentShader.get());

// prepare the cubemap texture

auto pointShadowMapTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP));

pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_R), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));

pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_BORDER_COLOR), glm::vec4(1.0f, 1.0f, 1.0f, 1.0f));

pointShadowMapTexture->bind();

const auto shadowMapSize = 2048;

for (auto i = 0; i < 6; ++i)
{
    ::glTexImage2D(
        static_cast<::GLenum>(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i),
        0,
        GL_DEPTH_COMPONENT,
        shadowMapSize,
        shadowMapSize,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr);
}

pointShadowMapTexture->unbind();

// prepare the framebuffer

auto pointShadowMappingFramebuffer = std::make_unique<globjects::Framebuffer>();
pointShadowMappingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_DEPTH_ATTACHMENT), pointShadowMapTexture.get());

// prepare a list of six view projection matrices

glm::mat4 cameraProjection = glm::perspective(glm::radians(fov), (float)window.getSize().x / (float)window.getSize().y, 0.1f, 100.0f);

glm::mat4 cameraView = glm::lookAt(
    cameraPos,
    cameraPos + cameraForward,
    cameraUp);

const float nearPlane = 0.1f;
const float farPlane = 10.0f;

glm::mat4 pointLightProjection = glm::perspective(glm::radians(90.0f), static_cast<float>(shadowMapSize / shadowMapSize), nearPlane, farPlane);

std::array<glm::mat4, 6> pointLightProjectionViewMatrices{
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)),
};

PointLightData pointLightData{ pointLightPosition, farPlane, pointLightProjectionViewMatrices };

// store the view projection matrices in SSBO

auto pointLightDataBuffer = std::make_unique<globjects::Buffer>();

pointLightDataBuffer->setData(pointLightData, static_cast<gl::GLenum>(GL_DYNAMIC_COPY));

// ...

// bind framebuffer and shared stored buffer object with parameters

pointLightDataBuffer->bindBase(GL_SHADER_STORAGE_BUFFER, 5);

pointShadowMappingFramebuffer->bind();

pointShadowMappingProgram->use();

// render entire scene

pointShadowMappingProgram->release();

pointShadowMappingFramebuffer->unbind();
```

geometry shader:

```glsl
void main()
{
    for (int face = 0; face < 6; ++face)
    {
        gl_Layer = face;

        for (int vertex = 0; vertex < 3; ++vertex)
        {
            fragmentPosition = gl_in[vertex].gl_Position;
            gl_Position = pointLight.projectionViewMatrices[face] * fragmentPosition;
            EmitVertex();
        }

        EndPrimitive();
    }
}
```

## Rendering techniques

### Shadow mapping

The most straightforward idea of implementing shadow mapping is: you render a scene depths (each pixel represents a distance from camera to the object) from the perspective of a light source to a separate render target. Then you render your scene normally from the perspective of a camera and for each pixel you compare its distance to the light source (take position of a pixel and subtract position of a light source from it) - if scene pixel is further from the light than the same pixel' depth in the light space - there's some other thing blocking the light, so this pixel is in the shadow.

For this algorithm, you would need a shadow map texture and a framebuffer it is attached to:

```cpp
auto shadowMapTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));

shadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_BORDER_COLOR), glm::vec4(1.0f, 1.0f, 1.0f, 1.0f));

shadowMapTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB10),
    glm::vec2(2048, 2048),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr);

auto framebuffer = std::make_unique<globjects::Framebuffer>();
framebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), shadowMapTexture.get());
framebuffer->setDrawBuffers({ static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), static_cast<gl::GLenum>(GL_NONE) });

auto renderBuffer = std::make_unique<globjects::Renderbuffer>();
renderBuffer->storage(static_cast<gl::GLenum>(GL_DEPTH24_STENCIL8), 2048, 2048);
framebuffer->attachRenderBuffer(static_cast<gl::GLenum>(GL_DEPTH_STENCIL_ATTACHMENT), renderBuffer.get());
```

Then you render the entire scene to that framebuffer from the position of the light using the orthographic projection and setting the viewport to the shadow map' size:

```cpp
glm::vec3 lightPosition = glm::vec3(0.0f, 3.0f, 4.0f); // cameraPos;

const float nearPlane = 0.1f;
const float farPlane = 10.0f;
glm::mat4 lightProjection = glm::ortho(-10.0f, 10.0f, -10.0f, 10.0f, nearPlane, farPlane);

glm::mat4 lightView = glm::lookAt(lightPosition, glm::vec3(0.0f, 0.0f, 0.0f), glm::vec3(0.0f, 1.0f, 0.0f));

glm::mat4 lightSpaceMatrix = lightProjection * lightView;

::glViewport(0, 0, 2048, 2048);

framebuffer->bind();

::glClearColor(static_cast<gl::GLfloat>(1.0f), static_cast<gl::GLfloat>(1.0f), static_cast<gl::GLfloat>(1.0f), static_cast<gl::GLfloat>(1.0f));
::glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);
// same as
// framebuffer->clearBuffer(static_cast<gl::GLenum>(GL_COLOR_BUFFER_BIT | GL_DEPTH), 0, glm::vec4(1.0f));

glEnable(GL_DEPTH_TEST);
glEnable(GL_CULL_FACE);

// cull front faces to prevent peter panning the generated shadow map
glCullFace(GL_FRONT);

shadowMappingProgram->use();

shadowMappingProgram->setUniform("lightSpaceMatrix", lightSpaceMatrix);
shadowMappingProgram->setUniform("model", chickenModel->getTransformation());

// render scene

framebuffer->unbind();

shadowMappingProgram->release();

glEnable(GL_CULL_FACE);
glCullFace(GL_BACK);
```

Finally, you restore the viewport to the window (screen) size and render the scene, taking data from shadow map into consideration:

```cpp
::glViewport(0, 0, static_cast<GLsizei>(window.getSize().x), static_cast<GLsizei>(window.getSize().y));
::glClearColor(static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(1.0f));
glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

shadowRenderingProgram->use();

shadowRenderingProgram->setUniform("lightPosition", lightPosition);
shadowRenderingProgram->setUniform("lightColor", glm::vec3(1.0, 1.0, 1.0));
// ambientColorUniform->set(glm::vec3(1.0f, 1.0f, 1.0f));
// materialSpecularUniform->set(12.0f);
shadowRenderingProgram->setUniform("cameraPosition", cameraPos);

shadowRenderingProgram->setUniform("projection", cameraProjection);
shadowRenderingProgram->setUniform("view", cameraView);
shadowRenderingProgram->setUniform("lightSpaceMatrix", lightSpaceMatrix);

// render the scene again

shadowMapTexture->unbindActive(0);

shadowRenderingProgram->release();
```

Shadow mapping vertex shader:

```glsl
#version 410

layout (location = 0) in vec3 vertexPosition;

out gl_PerVertex
{
    vec4 gl_Position;
};

uniform mat4 lightSpaceMatrix;
uniform mat4 model;

void main()
{
    gl_Position = lightSpaceMatrix * model * vec4(vertexPosition, 1.0);
}
```

Shadow mapping fragment shader:

```glsl
#version 410

void main()
{
    gl_FragDepth = gl_FragCoord.z;
}
```

Final rendering pass vertex shader:

```glsl
#version 410

layout (location = 0) in vec3 vertexPosition;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexTextureCoord;

out VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
    vec4 fragmentPositionInLightSpace;
} vsOut;

out gl_PerVertex {
    vec4 gl_Position;
};

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
uniform mat4 lightSpaceMatrix;

void main()
{
    vsOut.fragmentPosition = vec3(model * vec4(vertexPosition, 1.0));
    vsOut.normal = vertexNormal;
    vsOut.textureCoord = vertexTextureCoord;
    vsOut.fragmentPositionInLightSpace = lightSpaceMatrix * model * vec4(vertexPosition, 1.0);

    gl_Position = projection * view * model * vec4(vertexPosition, 1.0);
}
```

Final rendering fragment shader:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
    vec4 fragmentPositionInLightSpace;
} fsIn;

uniform sampler2D shadowMap;
uniform sampler2D diffuseTexture;

uniform vec3 lightPosition;
uniform vec3 lightColor;
uniform vec3 cameraPosition;

float shadowCalculation(vec3 normal, vec3 lightDirection)
{
    vec2 shadowMapCoord = (fsIn.fragmentPositionInLightSpace.xy / fsIn.fragmentPositionInLightSpace.w) * 0.5 + 0.5;
    float occluderDepth = texture(shadowMap, shadowMapCoord.xy).r;
    float thisDepth = fsIn.fragmentPositionInLightSpace.z / fsIn.fragmentPositionInLightSpace.w;

    if (thisDepth > 1.0)
    {
        return 0.0;
    }

    return 1.0;
}

void main()
{
    vec3 color = texture(diffuseTexture, fsIn.textureCoord).rgb;
    vec3 normal = normalize(fsIn.normal);

    // ambient
    vec3 ambient = 0.3 * color;

    // diffuse
    vec3 lightDirection = normalize(lightPosition - fsIn.fragmentPosition);
    float diff = max(dot(lightDirection, normal), 0.0);
    vec3 diffuse = diff * lightColor;

    // specular
    vec3 viewDirection = normalize(cameraPosition - fsIn.fragmentPosition);
    vec3 halfwayDirection = normalize(lightDirection + viewDirection);
    float spec = pow(max(dot(normal, halfwayDirection), 0.0), 64.0);
    vec3 specular = spec * lightColor;

    // calculate shadow
    float shadow = shadowCalculation(normal, lightDirection);

    vec3 lighting = ((shadow * (diffuse + specular)) + ambient) * color;

    fragmentColor = vec4(lighting, 1.0);
}
```

This technique, however, introduces a number of artifacts.

The simplest and easily solved issue is the rough edge of the shadow where there should be none, caused by the bounds of the light projection matrix. One can simply add a check in the shader to see if the pixel is out of the shadow map bounds and light those pixels by default.

Shadow aliasing - when you are trying to render a whole lot of objects on a small texture, shadow edges have stair-like shape instead of smooth lines. The bigger the shadow map, the more cube-like shadows will become. These artifacts are addressed by a number of different techniques, like cascaded shadow maps (aka parallel-slice aka parallel-split shadow mapping).

<img data-src="/images/opengl-advanced-samples/sample-09-shadow-mapping-issue-aliasing.webp">

Sometimes you might see lines of shadows where there should be none. This is caused by the lack of precision of the shadow map - the depth values simply do not have enough data to represent that half-of-a-pixel depth. This could be somewhat mitigated by adding a small factor to the depth sampled from a shadow map. But such a solution presents a yet new artifact - peter-panning, when the objects rendered "fly" over a surface beneath them. Sometimes light might "bleed" through the objects, lighting the surfaces where they should be in the shadow. This artifact is also known as "light bleeding".

There are other algorithms addressing the above artifacts - PCF and variance shadow mapping.

### Biasing

This technique fixes the aliasing issue by "offsetting" the surface the shadow is cast upon by some small number
(realistically you do not offset anything - you just adjust the calculations).

```glsl
float shadowCalculation(vec3 normal, vec3 lightDirection)
{
    vec2 shadowMapCoord = (fsIn.fragmentPositionInLightSpace.xy / fsIn.fragmentPositionInLightSpace.w) * 0.5 + 0.5;
    float occluderDepth = texture(shadowMap, shadowMapCoord.xy).r;
    float thisDepth = fsIn.fragmentPositionInLightSpace.z / fsIn.fragmentPositionInLightSpace.w;

    if (thisDepth > 1.0)
    {
        return 0.0;
    }

    // this biasing fixes aliasing artifact, but introduces peter-panning
    float bias = max(0.05 * (1.0 - dot(normal, lightDirection)), 0.005);

    return thisDepth - bias < occluderDepth ? 1.0 : 0.0;
}
```

This fixes the aliasing, indeed, but introduces the peter-panning effect, making object appear flying or hovering over the surface upon close inspection.

### Percentage-close filtering

Percentage-close filtering, or PCF, is a technique of smoothing out those hard-edge shadows. Instead of calculating whether the pixel (say, on a surface) is in shadow or not you calculate the median of all the neighbours of an are around that pixel (say, 5x5 pixels).

<img data-src="/images/opengl-advanced-samples/sample-09-shadow-mapping-pcf.webp">

PCF is implemented in the final shadow rendering fragment shader:

```glsl
float shadowCalculation(vec3 normal, vec3 lightDirection)
{
    vec2 shadowMapCoord = (fsIn.fragmentPositionInLightSpace.xy / fsIn.fragmentPositionInLightSpace.w) * 0.5 + 0.5;
    float occluderDepth = texture(shadowMap, shadowMapCoord.xy).r;
    float thisDepth = fsIn.fragmentPositionInLightSpace.z / fsIn.fragmentPositionInLightSpace.w;

    if (thisDepth > 1.0)
    {
        return 0.0;
    }

    // this biasing fixes aliasing artifact, but introduces peter-panning
    float bias = max(0.05 * (1.0 - dot(normal, lightDirection)), 0.005);

    // PCF
    float shadow = 0.0;
    vec2 texelSize = 1.0 / textureSize(shadowMap, 0);

    for (int x = -1; x <= 1; ++x)
    {
        for (int y = -1; y <= 1; ++y)
        {
            float pcfDepth = texture(shadowMap, shadowMapCoord.xy + vec2(x, y) * texelSize).r;

            shadow += thisDepth - bias < pcfDepth  ? 1.0 : 0.0;
        }
    }

    shadow /= 9.0;

    return shadow;
}
```

### Variance shadow mapping

[Variance shadow mapping](https://developer.nvidia.com/gpugems/gpugems3/part-ii-light-and-shadows/chapter-8-summed-area-variance-shadow-maps) is yet another technique of smoothing hard shadows. Unlike PCF, it has constant computation time. The idea is that you calculate the probability of a pixel being in shadow. If the probability is high enough - you treat it as if it was in the shadow. For the calculations, the two numbers (called "moments") are calculated during the shadow mapping, essentially being a function of the pixel depth (in light space) and a function of the same depth, squared. During the final scene rendering, you substitute the values in Chebyshev's inequality to get the probability.

<img data-src="/images/opengl-advanced-samples/sample-09-shadow-mapping-vsm.webp">

The implementation is as simple as having the following shadow mapping fragment shader:

```glsl
float linearizeDepth(float depth)
{
    float nearPlane = 1.0;
    float farPlane = 100.0;

    return (2.0 * nearPlane) / (farPlane + nearPlane - depth * (farPlane - nearPlane));
}

float linstep(float _min, float _max, float v)
{
    return clamp((v - _min) / (_max - _min), 0, 1);
}

float reduceLightBleeding(float p_max, float Amount)
{
    // Remove the [0, Amount] tail and linearly rescale (Amount, 1].
    return linstep(Amount, 1, p_max);
}

// compute an upper bound on the probability that the currently shaded surface (at depth t) is occluded
float ChebyshevUpperBound(vec2 moments, float t)
{
    // One-tailed inequality valid if t > Moments.x
    if (t <= moments.x)
    {
        return 1.0;
    }

    // Compute variance
    float variance = moments.y - (moments.x * moments.x);
    variance = max(variance, 0.001);

    // Compute probabilistic upper bound
    float d = t - moments.x;
    return variance / (variance + d * d);
}

float shadowCalculation(vec3 normal, vec3 lightDirection)
{
    vec3 shadowMapCoord = (fsIn.fragmentPositionInLightSpace.xyz / fsIn.fragmentPositionInLightSpace.w) * 0.5 + 0.5;
    vec3 shadowMapSample = texture(shadowMap, shadowMapCoord.xy).rgb;
    vec2 moments = shadowMapSample.xy;
    float fragmentDepth = shadowMapCoord.z; // alternatively, use linearizeDepth(shadowMapCoord.z);

    // Compute the Chebyshev upper bound.
    float p = step(fragmentDepth, moments.x);
    float mu = moments.x;
    float sigma2 = max(moments.y - moments.x * moments.x, 0.0002);

    float d = fragmentDepth - mu;
    float p_max = sigma2 / (sigma2 + (d * d));
    p_max = clamp(max(p, p_max), 0, 1);

    return reduceLightBleeding(p_max, 0.5);
}
```

### Cascaded shadow mapping

This is rather quality- and performance-optimization technique. It could also be used for not just shadow mapping, but for various objects rendering (like rendering grass, small terrain details, terrain itself, etc.). The idea is that there is no need to render high-detailed shadows further from the camera. Assume you have a large landscape. If you render detailed shadows for an entire landscape to rather limited texture - you will end up having each object have big pixelated shadow. Whilst you want objects closer to the camera look sharp and nice. And the trees in the distance - well, there is no need for them to have that nice shadows.

<img data-src="/images/opengl-advanced-samples/sample-12-cascade-shadow-mapping-1.webp">
<img data-src="/images/opengl-advanced-samples/sample-12-cascade-shadow-mapping-2.webp">
<img data-src="/images/opengl-advanced-samples/sample-12-cascade-shadow-mapping-3.webp">

Thus, you "divide" your camera space into few sections (aka "cascades") and render shadows for each of those cascade separately, using the same size of the shadow map texture. The trick is that the cascade closest to the camera is the smallest, so the shadows have very high level of detail. The section of a camera space a bit further away is larger, but since the shadow map has the same size - the quality of shadows is lower. The furthest section of the camera space has the largest size, so the quality of the shadows in the same size of the shadow map will be the worst. But since the objects in that section are also furthest away from the camera - they don't have to have the most detailed shadows. In fact, you can go even further and only split half of camera space into three (or more, if you will) cascades, leaving the other half, the furthest one from the camera, completely out of the shadow mapping process. This way the objects far away won't even have any shadows at all.

<img data-src="/images/opengl-advanced-samples/Screen Shot 2021-07-05 at 1.28.47 pm.webp">

Read more:

* [[nVidia] Parallel split shadow maps on programmable GPUs](https://developer.nvidia.com/gpugems/gpugems3/part-ii-light-and-shadows/chapter-10-parallel-split-shadow-maps-programmable-gpus)
* [[blog] Shadow Rendering Using Parallel-Split Shadow Mapping](https://sudonull.com/post/110380-Shadow-Rendering-Using-Parallel-Split-Shadow-Mapping)
* [[MSDN] Cascaded shadow maps](https://docs.microsoft.com/en-us/windows/win32/dxtecharts/cascaded-shadow-maps)
* [[OGLdev] Cascaded shadow mapping](https://ogldev.org/www/tutorial49/tutorial49.html)

For the most part, the logic is implemented in the main application, where you have to set up the projection matrices for the shadow mapping and run the shadow mapping rendering stage multiple times (for each of the projection matrix).

Alternatively, this could be used in conjunction with other two optimization techniques - SSBO and geometry shader - you send the projection matrices to the shader via SSBO and then instead of issuing rendering commands multiple times from the application side (CPU), everything is done on the GPU, without the need to switch back and forth between shader context and application context (roughly put - without the need to formally finish the rendering, let the CPU / application know about this and make it issue a new bunch of rendering commands, which essentially will be the same - just the projection matrix will be different).

Setting up shadow mapping shaders:

```cpp
auto shadowMappingVertexSource = globjects::Shader::sourceFromFile("media/shadow-mapping.vert");
auto shadowMappingVertexShaderTemplate = globjects::Shader::applyGlobalReplacements(shadowMappingVertexSource.get());
auto shadowMappingVertexShader = std::make_unique<globjects::Shader>(static_cast<gl::GLenum>(GL_VERTEX_SHADER), shadowMappingVertexShaderTemplate.get());

if (!shadowMappingVertexShader->compile())
{
    std::cerr << "[ERROR] Can not compile vertex shader" << std::endl;
    return 1;
}

auto shadowMappingGeometrySource = globjects::Shader::sourceFromFile("media/shadow-mapping.geom");
auto shadowMappingGeometryShaderTemplate = globjects::Shader::applyGlobalReplacements(shadowMappingGeometrySource.get());
auto shadowMappingGeometryShader = std::make_unique<globjects::Shader>(gl::GL_GEOMETRY_SHADER, shadowMappingGeometryShaderTemplate.get());

if (!shadowMappingGeometryShader->compile())
{
    std::cerr << "[ERROR] Can not compile geometry shader" << std::endl;
    return 1;
}

auto shadowMappingFragmentSource = globjects::Shader::sourceFromFile("media/shadow-mapping.frag");
auto shadowMappingFragmentShaderTemplate = globjects::Shader::applyGlobalReplacements(shadowMappingFragmentSource.get());
auto shadowMappingFragmentShader = std::make_unique<globjects::Shader>(static_cast<gl::GLenum>(GL_FRAGMENT_SHADER), shadowMappingFragmentShaderTemplate.get());

if (!shadowMappingFragmentShader->compile())
{
    std::cerr << "[ERROR] Can not compile fragment shader" << std::endl;
    return 1;
}

auto shadowMappingProgram = std::make_unique<globjects::Program>();

shadowMappingProgram->attach(shadowMappingVertexShader.get(), shadowMappingGeometryShader.get(), shadowMappingFragmentShader.get());

auto shadowMappingModelTransformationUniform = shadowMappingProgram->getUniform<glm::mat4>("modelTransformation");
auto shadowMappingLightViewProjectionMatrices = shadowMappingProgram->getUniform<std::vector<glm::mat4>>("lightViewProjectionMatrix");
auto lightViewProjectionMatricesUniform = shadowMappingProgram->getUniform<std::vector<glm::mat4>>("lightViewProjectionMatrix");
```

And a few constants:

```cpp
std::vector<glm::mat4> lightViewProjectionMatrices;
std::vector<float> splitDepths;
const std::vector<float> splits{ { 0.0f, 0.05f, 0.2f, 0.5f, 1.0f } };

// these vertices define view frustum in screen space coordinates
constexpr std::array<glm::vec3, 8> _cameraFrustumSliceCornerVertices{
    {
        { -1.0f, -1.0f, -1.0f }, { 1.0f, -1.0f, -1.0f }, { 1.0f, 1.0f, -1.0f }, { -1.0f, 1.0f, -1.0f },
        { -1.0f, -1.0f, 1.0f }, { 1.0f, -1.0f, 1.0f }, { 1.0f, 1.0f, 1.0f }, { -1.0f, 1.0f, 1.0f },
    }
};
```

Then, the shadow mapping stage:

```cpp
{
    lightViewProjectionMatrices.clear();
    splitDepths.clear();

    glm::mat4 proj = glm::inverse(cameraProjection * cameraView);

    std::array<glm::vec3, 8> _entireFrustum{};

    std::transform(
        _cameraFrustumSliceCornerVertices.begin(),
        _cameraFrustumSliceCornerVertices.end(),
        _entireFrustum.begin(),
        [proj](glm::vec3 p) {
            glm::vec4 v = proj * glm::vec4(p, 1.0f);
            return glm::vec3(v) / v.w;
        }
    );

    std::array<glm::vec3, 4> _frustumEdgeDirections {};

    for (auto i = 0; i < 4; ++i)
    {
        _frustumEdgeDirections[i] = glm::normalize(_entireFrustum[4 + i] - _entireFrustum[i]);
    }

    const float _depth = farPlane - nearPlane;

    for (auto splitIdx = 1; splitIdx < splits.size(); ++splitIdx)
    {
        // frustum slice vertices in world space
        std::array<glm::vec3, 8> _frustumSliceVertices;

        for (auto t = 0; t < 4; ++t)
        {
            _frustumSliceVertices[t] = _entireFrustum[t] + _frustumEdgeDirections[t] * _depth * splits[splitIdx - 1];
            _frustumSliceVertices[4 + t] = _entireFrustum[t] + _frustumEdgeDirections[t] * _depth * splits[splitIdx];
        }

        // TODO: also check if camera is looking towards the light

        glm::vec3 _frustumSliceCenter(0.0f);

        for (auto p : _frustumSliceVertices)
        {
            _frustumSliceCenter += p;
        }

        _frustumSliceCenter /= 8.0f;

        glm::vec3 _frustumRadiusVector(0.0f);

        for (auto p : _frustumSliceVertices)
        {
            auto v = p - _frustumSliceCenter;

            if (glm::length(_frustumRadiusVector) < glm::length(v))
            {
                _frustumRadiusVector = v;
            }
        }

        // calculate new light projection
        glm::vec3 _forward = glm::normalize(_lightDirection);
        glm::vec3 _right = glm::cross(_forward, glm::vec3(0.0f, 1.0f, 0.0f));
        glm::vec3 _up(0.0f, 1.0f, 0.0f);
        glm::mat4 _lightView = glm::lookAt(_frustumSliceCenter - glm::normalize(_lightDirection) * glm::length(_frustumRadiusVector), _frustumSliceCenter, _up);

        const float _frustumRadius = glm::length(_frustumRadiusVector);

        glm::mat4 _lightProjectionViewMatrix = glm::ortho(
            _frustumSliceCenter.x - _frustumRadius,
            _frustumSliceCenter.x + _frustumRadius,
            _frustumSliceCenter.y - _frustumRadius,
            _frustumSliceCenter.y + _frustumRadius,
            0.0f,
            _frustumSliceCenter.z + 2.0f * _frustumRadius
        ) * _lightView;

        lightViewProjectionMatrices.push_back(_lightProjectionViewMatrix);

        splitDepths.push_back(_depth * splits[splitIdx] * 0.7f);
    }

    shadowMappingLightViewProjectionMatrices->set(lightViewProjectionMatrices);
    shadowRenderingLightViewProjectionsUniform->set(lightViewProjectionMatrices);
    shadowRenderingSplitsUniform->set(splitDepths);
}
```

Then geometry shader would be:

```glsl
#version 410

layout(triangles) in;

/*
 this shader will be emitting data to 4 shadow map layers as per 4 frustum splits;
 each triangle fed into this geometry shader will be projected into each shadow map layer;
 hence this shader will emit 4 layers * 3 vertices (per each input primitive / geometry) = 12 vertices;
 note how the input vertices are in the world space, so we need to project them to the clip space in here
*/
layout(triangle_strip, max_vertices = 12) out;

out gl_PerVertex
{
  vec4 gl_Position;
};

// out int gl_Layer;

uniform mat4 lightViewProjectionMatrix[4]; // as per 4 frustum splits

void main()
{
    for (int split = 0; split < 4; ++split)
    {
        // input primitive index
        for (int i = 0; i < gl_in.length(); ++i)
        {
            // in a 3D texture, which layer do we project our input primitive to
            gl_Layer = split;

            // project an input primitive using the corresponding projection matrix from the uniform
            gl_Position = lightViewProjectionMatrix[split] * gl_in[i].gl_Position;

            EmitVertex();
        }

        EndPrimitive();
    }
}
```

The rendering stage would use the projection matrices too. So setting up the rendering shader:

```cpp
auto shadowRenderingFragmentShaderSource = globjects::Shader::sourceFromFile("media/shadow-rendering.frag");
auto shadowRenderingFragmentShaderTemplate = globjects::Shader::applyGlobalReplacements(shadowRenderingFragmentShaderSource.get());
auto shadowRenderingFragmentShader = std::make_unique<globjects::Shader>(static_cast<gl::GLenum>(GL_FRAGMENT_SHADER), shadowRenderingFragmentShaderTemplate.get());

if (!shadowRenderingFragmentShader->compile())
{
    std::cerr << "[ERROR] Can not compile chicken fragment shader" << std::endl;
    return 1;
}

auto shadowRenderingProgram = std::make_unique<globjects::Program>();
shadowRenderingProgram->attach(shadowRenderingVertexShader.get(), shadowRenderingFragmentShader.get());

auto shadowRenderingModelTransformationUniform = shadowRenderingProgram->getUniform<glm::mat4>("model");
auto shadowRenderingViewTransformationUniform = shadowRenderingProgram->getUniform<glm::mat4>("view");
auto shadowRenderingProjectionTransformationUniform = shadowRenderingProgram->getUniform<glm::mat4>("projection");

auto shadowRenderingLightPositionUniform = shadowRenderingProgram->getUniform<glm::vec3>("lightPosition");
auto shadowRenderingLightColorUniform = shadowRenderingProgram->getUniform<glm::vec3>("lightColor");
auto shadowRenderingCameraPositionUniform = shadowRenderingProgram->getUniform<glm::vec3>("cameraPosition");

auto shadowRenderingLightViewProjectionsUniform = shadowRenderingProgram->getUniform<std::vector<glm::mat4>>("lightViewProjections");
auto shadowRenderingSplitsUniform = shadowRenderingProgram->getUniform<std::vector<float>>("splits"); // distance from camera to zFar, e.g. (far - cameraPosition.z) * splitFraction
```

And finally, the fragment shader for the final rendering stage:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec4 viewPosition;
    vec4 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} fsIn;

uniform mat4 lightViewProjections[4];
uniform float splits[4];

uniform sampler2DArray shadowMaps;
uniform sampler2D diffuseTexture;

uniform vec3 lightPosition;
uniform vec3 lightColor;
uniform vec3 cameraPosition;

float shadowCalculation(vec3 normal, vec3 lightDirection)
{
    float cameraViewDepth = fsIn.viewPosition.z;

    for (int i = 0; i < 4; ++i)
    {
        if (cameraViewDepth < splits[i])
        {
            vec4 fragmentPositionInLightSpace = lightViewProjections[i] * fsIn.fragmentPosition;
            vec3 shadowPos1 = fragmentPositionInLightSpace.xyz / fragmentPositionInLightSpace.w;
            vec3 shadowMapCoord = shadowPos1 * 0.5 + 0.5;
            float thisDepth = shadowMapCoord.z;

            if (thisDepth > 1.0)
            {
                continue;
            }

            float occluderDepth = texture(shadowMaps, vec3(shadowMapCoord.xy, i)).r;

            float bias = max(0.05 * (1.0 - dot(normal, lightDirection)), 0.005);

            return thisDepth - bias > occluderDepth ? 0.25 : 1.0;
        }
    }

    return 0.0;
}

void main()
{
    vec3 color = texture(diffuseTexture, fsIn.textureCoord).rgb;
    vec3 normal = normalize(fsIn.normal);

    // ambient
    vec3 ambient = 0.3 * color;

    // diffuse
    vec3 lightDirection = normalize(lightPosition - vec3(fsIn.fragmentPosition) / fsIn.fragmentPosition.w);
    float diff = max(dot(lightDirection, normal), 0.0);
    vec3 diffuse = diff * lightColor;

    // specular
    vec3 viewDirection = normalize(cameraPosition - vec3(fsIn.fragmentPosition) / fsIn.fragmentPosition.w);
    vec3 halfwayDirection = normalize(lightDirection + viewDirection);
    float spec = pow(max(dot(normal, halfwayDirection), 0.0), 64.0);
    vec3 specular = spec * lightColor;

    // calculate shadow
    float shadow = shadowCalculation(normal, lightDirection);

    vec3 lighting = ((shadow * (diffuse + specular)) + ambient) * color;

    fragmentColor = vec4(lighting, 1.0);
}
```

Note how now the shadow calculation function makes use of the projection matrices, so instead of rendering the scene multiple times again, we simply pick the shadow data from a corresponding shadow map buffer.

### Anti-aliasing

You might have noticed that rendering scene from the framebuffer results in most lines looking like stairs, with very distinct hard edges around every shape.
This is most obvious during the deferred rendering.

<img data-src="/images/opengl-advanced-samples/sample-16-anti-aliasing-2.webp">
<img data-src="/images/opengl-advanced-samples/sample-16-anti-aliasing-3.webp">

In order to prevent (or rather somewhat mitigate) this issue, the technique called "anti-aliasing" is used. It softens the pixels around those hard edges so they gradually change color instead of hard jump.

### Multi-sampled anti-aliasing (MSAA)

I have never understood the concept of multi-resolution pyramid (even when working on my second M.Sc. thesis) until I got myself into all this OpenGL stuff.
The way you can soften the rapid change of a color in neighbour pixels is to upscale the image image few times and then downscale it back. This way you will loose a lot of detail, getting an average color value for each scaling level.
OpenGL actually comes bundled with the multi-sampled rendering feature - when you create a framebuffer you can specify the number of samples for texture.

But you can implement one by yourself, explicitly, using one rather simple shader program and a deferred rendering technique.

First rendering pass would be rendering a scene to a frame buffer. The last (or second, in this overly-simplified example) rendering pass would run on top of that frame buffer and compute the new one using the simple shader program:

vertex shader for the MSAA last rendering pass:

```glsl
#version 410

layout (location = 0) in vec3 vertexPosition;
layout (location = 2) in vec2 vertexTextureCoord;

out VS_OUT
{
    vec3 fragmentPosition;
    vec2 textureCoord;
} vsOut;

out gl_PerVertex {
    vec4 gl_Position;
};

void main()
{
    vsOut.fragmentPosition = vertexPosition;
    vsOut.textureCoord = vec2(1.0 - vertexTextureCoord.x, vertexTextureCoord.y);

    gl_Position = vec4(vertexPosition, 1.0);
}
```

fragment shader for the last rendering pass:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec2 textureCoord;
} fsIn;

uniform sampler2D diffuseTexture;

vec4 msaa( sampler2D tex, vec2 uv ) {
    vec2 resolution = textureSize(tex, 0);

    vec4 singleSample = texture(tex, uv);

    float a = (3.0 / 8.0) * (1.0 / resolution.x);
    float b = (1.0 / 8.0) * (1.0 / resolution.y);

    vec4 acc = vec4(0.0);

    acc += texture(tex, (uv + vec2(-a, b)));
    acc += texture(tex, (uv + vec2(a, -b)));
    acc += texture(tex, (uv + vec2(-b, -a)));
    acc += texture(tex, (uv + vec2(b, a)));
    acc /= 4.0;

    // vec4 color = pow(acc, vec4(1.0 / 2.2));

    return acc;
}

void main()
{
    vec4 color = msaa(diffuseTexture, fsIn.textureCoord);

    fragmentColor = color;
}
```

### Fast-approximation anti-aliasing (FXAA)

Multi-sampled rendering is fine, but usually it comes at a cost of higher memory consumption - for each multi-sampled texture you have to allocate memory.
The more multi-sampled textures you have - the more memory your application consumes.

Fast approximation works at constant time and memory and thus is one of the most popular anti-aliasing algorithms used.
It is also quite easy to implement, which makes it all more appealing.

The algorithm actually detects the edges on the image and then blends the colors in the direction perpendicular to the edge.

That sounds easy on paper, but how does it actually detect the edges? The algorithm calculates the luminance of the pixels surrounding a given central pixel by calculating a dot product of each pixel' color and a constant luminance vector. It then compares the luminance in two directions vertically and two directions horizontally to find the edge' direction.

<img data-src="/images/opengl-advanced-samples/sample-22-fxaa-0.webp">
<img data-src="/images/opengl-advanced-samples/sample-22-fxaa-1.webp">

The only substantial difference in the code for FXAA is in the fragment shader for the last rendering pass:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec2 textureCoord;
} fsIn;

uniform sampler2D diffuseTexture;

#define FXAA_SPAN_MAX 16.0
#define FXAA_REDUCE_MUL   (1.0 / FXAA_SPAN_MAX)
#define FXAA_REDUCE_MIN   (1.0 / 128.0)
#define FXAA_SUBPIX_SHIFT (1.0 / 8.0)

vec4 fxaa( sampler2D tex, vec2 uv2 )
{
    vec2 res = textureSize(tex, 0);
    vec2 rcpFrame = 1. / res;

    vec4 uv = vec4( uv2, uv2 - (rcpFrame * (0.5 + FXAA_SUBPIX_SHIFT)));

    vec3 rgbNW = texture(tex, uv.zw).xyz;
    vec3 rgbNE = texture(tex, uv.zw + vec2(1, 0) * rcpFrame.xy).xyz;
    vec3 rgbSW = texture(tex, uv.zw + vec2(0, 1) * rcpFrame.xy).xyz;
    vec3 rgbSE = texture(tex, uv.zw + vec2(1, 1) * rcpFrame.xy).xyz;
    vec4 texColor = texture(tex, uv.xy);
    vec3 rgbM  = texColor.xyz;

    vec3 luma = vec3(0.299, 0.587, 0.114);
    float lumaNW = dot(rgbNW, luma);
    float lumaNE = dot(rgbNE, luma);
    float lumaSW = dot(rgbSW, luma);
    float lumaSE = dot(rgbSE, luma);
    float lumaM  = dot(rgbM,  luma);

    float lumaMin = min(lumaM, min(min(lumaNW, lumaNE), min(lumaSW, lumaSE)));
    float lumaMax = max(lumaM, max(max(lumaNW, lumaNE), max(lumaSW, lumaSE)));

    vec2 dir;
    dir.x = -((lumaNW + lumaNE) - (lumaSW + lumaSE));
    dir.y =  ((lumaNW + lumaSW) - (lumaNE + lumaSE));

    float dirReduce = max(
        (lumaNW + lumaNE + lumaSW + lumaSE) * (0.25 * FXAA_REDUCE_MUL),
        FXAA_REDUCE_MIN
    );

    float rcpDirMin = 1.0/(min(abs(dir.x), abs(dir.y)) + dirReduce);

    dir = min(
        vec2( FXAA_SPAN_MAX,  FXAA_SPAN_MAX),
        max(
            vec2(-FXAA_SPAN_MAX, -FXAA_SPAN_MAX),
            dir * rcpDirMin
        )
    ) * rcpFrame.xy;

    vec3 rgbA = (1.0/2.0) * (
        texture(tex, uv.xy + dir * (1.0 / 3.0 - 0.5)).xyz +
        texture(tex, uv.xy + dir * (2.0 / 3.0 - 0.5)).xyz);

    vec3 rgbB = rgbA * (1.0 / 2.0) + (1.0 / 4.0) * (
        texture(tex, uv.xy + dir * (0.0 / 3.0 - 0.5)).xyz +
        texture(tex, uv.xy + dir * (3.0 / 3.0 - 0.5)).xyz);

    float lumaB = dot(rgbB, luma);

    if ((lumaB < lumaMin) || (lumaB > lumaMax))
        return vec4(rgbA, texColor.a);
    else
        return vec4(rgbB, texColor.a);
}

void main()
{
    vec4 color = fxaa(diffuseTexture, fsIn.textureCoord);

    fragmentColor = color;
}
```

### Screen-space ambient occlusion (SSAO)

As you might know, rendering big and complex scenes is expensive in both time, memory and computational resources. It is not always desirable or even possible to render all effects like reflections and shadows for each and every polygon of the scene.

To address this issue, techniques like ambient occlusion (specifically, screen-space ambient occlusion, in that they only work in screen space, combined with deferred rendering, as a post-processing step) exist. This technique in particular allows simulating the shadow cast by the objects by darkening the pixels which are occluded by other objects from the perspective of light.

Sounds very much like shadow mapping, doesn't it? But the big difference here is that in shadow mapping you need to render an entire scene from the perspective of each light source. Even if you trim the scene to only the objects visible by the light (which is a quite nice optimization, but for the most part it is also expensive), you will still have to render every polygon.

In contrast, ambient occlusion is calculated for a fixed number of samples around a given pixel. On top of that, the pixels we analyze are only in screen space. On top of that, we do not need to render all the geometry around a given pixel - we re-use the position information rendered as one of the attributes in first pass of a deferred rendering. To reduce the cost of this algorithm even further, you do not need to render anything from the light perspective - you just calculate the sample rays of light for each light, knowing its position and direction.

You will need to store the position of each fragment in camera space (yes, camera space, not light space). Then, for each pixel, you iterate each source of light to calculate the direction of light and samples for a given pixel' position in camera space. You assume you have a sphere of a given constant radius around the pixel' position and you generate a certain constant number of random samples within that sphere. Since the samples are also in the camera space, you can use these positions as-is. Given the information about positions of neighbour pixels in camera space, you then compare those positions to the samples within the sphere - if the sample's position is further from the camera than the neighbour pixel's position - this sample is occluded by some polygon. You then calculate the ratio of occluded samples to the non-occluded ones and based on that give the original pixel its shadow value.

<img data-src="/images/opengl-advanced-samples/sample-24-ssao-1.webp">
<img data-src="/images/opengl-advanced-samples/sample-24-ssao-6.webp">

Read more:

* [[learnopengl] SSAO](https://learnopengl.com/Advanced-Lighting/SSAO)

Setting up the initial render pass framebuffer is a bit tricky - you will need multiple attachments for each of the data component:

```cpp
auto deferredFragmentPositionTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentPositionTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentPositionTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentPositionTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB32F),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr
);

auto deferredFragmentNormalTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentNormalTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentNormalTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentNormalTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB32F),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr
);

auto deferredFragmentAlbedoTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentAlbedoTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentAlbedoTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentAlbedoTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGBA8),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGBA),
    static_cast<gl::GLenum>(GL_UNSIGNED_BYTE),
    nullptr
);

auto deferredFragmentDepthTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentDepthTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_DEPTH_COMPONENT),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_DEPTH_COMPONENT),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr
);

auto deferredRenderingFramebuffer = std::make_unique<globjects::Framebuffer>();
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), deferredFragmentPositionTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT1), deferredFragmentNormalTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT2), deferredFragmentAlbedoTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_DEPTH_ATTACHMENT), deferredFragmentDepthTexture.get());

// tell framebuffer it actually needs to render to **BOTH** textures, but does not have to output anywhere (last NONE argument, iirc)
deferredRenderingFramebuffer->setDrawBuffers({
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0),
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT1),
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT2),
    static_cast<gl::GLenum>(GL_NONE)
});

deferredRenderingFramebuffer->printStatus(true);
```

You will also need two buffers for blur:

```cpp
auto temporaryTexture1 = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

temporaryTexture1->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
temporaryTexture1->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

temporaryTexture1->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGBA8),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGBA),
    static_cast<gl::GLenum>(GL_UNSIGNED_BYTE),
    nullptr
);

auto temporaryFramebuffer = std::make_unique<globjects::Framebuffer>();

temporaryFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), temporaryTexture1.get());
temporaryFramebuffer->setDrawBuffers({ static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), static_cast<gl::GLenum>(GL_NONE) });

temporaryFramebuffer->printStatus(true);

auto temporaryTexture2 = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

temporaryTexture2->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
temporaryTexture2->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

temporaryTexture2->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGBA8),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGBA),
    static_cast<gl::GLenum>(GL_UNSIGNED_BYTE),
    nullptr
);

auto temporaryFramebuffer2 = std::make_unique<globjects::Framebuffer>();

temporaryFramebuffer2->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), temporaryTexture2.get());
temporaryFramebuffer2->setDrawBuffers({ static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), static_cast<gl::GLenum>(GL_NONE) });

temporaryFramebuffer2->printStatus(true);
```

And the random data (vector offsets) for the SSAO algorithm, which we will generate in the application and store in a small texture:

```cpp
std::uniform_real_distribution<float> randomFloats(0.0, 1.0); // random floats between [0.0, 1.0]
std::default_random_engine generator;

const auto ssaoKernelSamples = 64;

std::vector<glm::vec3> ssaoKernel;

for (unsigned int i = 0; i < ssaoKernelSamples; ++i)
{
    glm::vec3 sample(
        randomFloats(generator) * 2.0f - 1.0f,
        randomFloats(generator) * 2.0f - 1.0f,
        randomFloats(generator)
    );

    sample = glm::normalize(sample);
    sample *= randomFloats(generator);

    float scale = static_cast<float>(i) / static_cast<float>(ssaoKernelSamples);

    // lerp(a, b, f) = a + f * (b - a);
    // scale = lerp(0.1f, 1.0f, scale * scale);
    scale = 0.1f + (scale * scale * (1.0f - 0.1f));

    sample *= scale;

    ssaoKernel.push_back(sample);
}

std::vector<glm::vec3> ssaoNoise;

for (unsigned int i = 0; i < 16; i++)
{
    glm::vec3 noise(
        randomFloats(generator) * 2.0f - 1.0f,
        randomFloats(generator) * 2.0f - 1.0f,
        0.0f
    );

    ssaoNoise.push_back(noise);
}

auto ssaoNoiseTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

ssaoNoiseTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_NEAREST));
ssaoNoiseTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_NEAREST));
ssaoNoiseTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<GLint>(GL_REPEAT));
ssaoNoiseTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<GLint>(GL_REPEAT));

ssaoNoiseTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB32F),
    glm::vec2(4.0f, 4.0f),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    &ssaoNoise[0]
);

auto ssaoKernelTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_1D));

ssaoKernelTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_NEAREST));
ssaoKernelTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_NEAREST));

ssaoKernelTexture->image1D(
    0,
    static_cast<gl::GLenum>(GL_RGBA16F),
    static_cast<float>(ssaoKernelSamples),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    &ssaoKernel[0]
);
```

With the rendering itself, there are no tricks - you have three passes.

On the first pass you bind the deferred rendering buffer (with a few attachments, that is) and render scene as normal.
The textures attached to the deferred rendering buffer will be populated with the data.

This data is then used on the second rendering pass to calculate the dark edges (amboent occlusion) on the frame rendered (aka in the screen space) and blur them.

On the last rendering pass you combine the framebuffers to obtain the final image:

```cpp
// first render pass - prepare for deferred rendering by rendering to the entire scene to a deferred rendering framebuffer's attachments
{
    deferredRenderingFramebuffer->bind();

    deferredRenderingPrePassProgram->use();

    // render scene

    deferredRenderingPrePassProgram->release();

    deferredRenderingFramebuffer->unbind();
}

// second render pass - calculate & blur the ambient occlusion
{
    temporaryFramebuffer->bind();

    ::glViewport(0, 0, static_cast<GLsizei>(window.getSize().x), static_cast<GLsizei>(window.getSize().y));
    ::glClearColor(static_cast<gl::GLfloat>(1.0f), static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(1.0f));
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    ssaoProgram->use();

    deferredFragmentPositionTexture->bindActive(2);
    deferredFragmentNormalTexture->bindActive(3);

    ssaoNoiseTexture->bindActive(5);
    ssaoKernelTexture->bindActive(6);

    ssaoProgram->setUniform("positionTexture", 2);
    ssaoProgram->setUniform("normalTexture", 3);
    ssaoProgram->setUniform("albedoTexture", 4);

    ssaoProgram->setUniform("ssaoNoiseTexture", 5);
    ssaoProgram->setUniform("ssaoKernelTexture", 6);

    ssaoProgram->setUniform("cameraPosition", cameraPos);
    ssaoProgram->setUniform("projection", cameraProjection);
    ssaoProgram->setUniform("view", cameraView);

    quadModel->bind();
    quadModel->draw();
    quadModel->unbind();

    deferredFragmentPositionTexture->unbindActive(2);
    deferredFragmentNormalTexture->unbindActive(3);

    ssaoNoiseTexture->unbindActive(5);
    ssaoKernelTexture->unbindActive(6);

    ssaoProgram->release();

    temporaryFramebuffer->unbind();

    // blur
    temporaryFramebuffer->bind(static_cast<gl::GLenum>(GL_READ_FRAMEBUFFER));
    temporaryFramebuffer2->bind(static_cast<gl::GLenum>(GL_DRAW_FRAMEBUFFER));

    temporaryFramebuffer->blit(
        static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0),
        std::array<gl::GLint, 4>{ 0, 0, static_cast<int>(window.getSize().x), static_cast<int>(window.getSize().y) },
        temporaryFramebuffer2.get(),
        std::vector<gl::GLenum>{ static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0) },
        std::array<gl::GLint, 4>{ 0, 0, static_cast<int>(window.getSize().x), static_cast<int>(window.getSize().y) },
        static_cast<gl::ClearBufferMask>(GL_COLOR_BUFFER_BIT),
        static_cast<gl::GLenum>(GL_NEAREST));

    // same as
    // glReadBuffer(GL_COLOR_ATTACHMENT0);
    // glDrawBuffer(GL_COLOR_ATTACHMENT0);
    // glBlitFramebuffer(0, 0, window.getSize().x, window.getSize().y, 0, 0, window.getSize().x, window.getSize().y, static_cast<gl::ClearBufferMask>(GL_COLOR_BUFFER_BIT), static_cast<gl::GLenum>(GL_NEAREST));

    temporaryFramebuffer->unbind();
    temporaryFramebuffer2->unbind();

    blurProgram->use();

    const auto blurPasses = 10;

    // for the initial blur pass, use the texture from the bloomFramebuffer as an input

    // we do not need anything extra here, since the bloomBlurFramebuffer2 (which we read from) will already contain the data from the bloomBrightnessTexture

    blurProgram->setUniform("blurInput", 0);

    for (auto i = 0; i < blurPasses; ++i)
    {
        // bind one framebuffer to write blur results to and bind the texture from another framebuffer to read input data from (for this blur stage)
        if (i % 2 == 0)
        {
            // bind the new target framebuffer to write blur results to
            temporaryFramebuffer->bind();
            // bind the texture from the previous blur pass to read input data for this stage from
            temporaryTexture2->bindActive(0);
            // tell shader that we want to use horizontal blur
            blurProgram->setUniform("isHorizontalBlur", true);
        }
        else
        {
            // bind the new target framebuffer to write blur results to
            temporaryFramebuffer2->bind();
            // bind the texture from the previous blur pass to read input data for this stage from
            if (i > 0)
                temporaryTexture1->bindActive(0);
            // tell shader that we want to use vertical blur
            blurProgram->setUniform("isHorizontalBlur", false);
        }

        // render quad with the texture from the active texture
        quadModel->bind();
        quadModel->draw();
        quadModel->unbind();

        if (i % 2 == 0)
        {
            // unbind the active framebuffer
            temporaryFramebuffer->unbind();
            // unbind the active texture
            temporaryTexture2->unbindActive(0);
        }
        else
        {
            temporaryFramebuffer2->unbind();
            temporaryTexture1->unbindActive(0);
        }
    }

    blurProgram->release();
}

// third render pass - merge textures from the deferred rendering pre-pass into a final frame
{
    ::glViewport(0, 0, static_cast<GLsizei>(window.getSize().x), static_cast<GLsizei>(window.getSize().y));
    ::glClearColor(static_cast<gl::GLfloat>(1.0f), static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(1.0f));
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    deferredRenderingFinalPassProgram->use();

    deferredFragmentPositionTexture->bindActive(2);
    deferredFragmentNormalTexture->bindActive(3);
    deferredFragmentAlbedoTexture->bindActive(4);

    temporaryTexture1->bindActive(5);

    pointLightDataBuffer->bindBase(GL_SHADER_STORAGE_BUFFER, 5);

    deferredRenderingFinalPassProgram->setUniform("positionTexture", 2);
    deferredRenderingFinalPassProgram->setUniform("normalTexture", 3);
    deferredRenderingFinalPassProgram->setUniform("albedoTexture", 4);

    deferredRenderingFinalPassProgram->setUniform("ssaoTexture", 5);

    deferredRenderingFinalPassProgram->setUniform("cameraPosition", cameraPos);
    deferredRenderingFinalPassProgram->setUniform("projection", cameraProjection);
    deferredRenderingFinalPassProgram->setUniform("view", cameraView);

    quadModel->bind();
    quadModel->draw();
    quadModel->unbind();

    pointLightDataBuffer->unbind(GL_SHADER_STORAGE_BUFFER, 5);

    deferredFragmentPositionTexture->unbindActive(2);
    deferredFragmentNormalTexture->unbindActive(3);
    deferredFragmentAlbedoTexture->unbindActive(4);

    temporaryTexture1->unbindActive(5);

    deferredRenderingFinalPassProgram->release();
}
```

The fragment shader for the first render pass:

```glsl
#version 410

in VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} fsIn;

layout (location = 0) out vec3 fsPosition;
layout (location = 1) out vec3 fsNormal;
layout (location = 2) out vec4 fsAlbedo;

uniform sampler2D diffuseTexture;
uniform sampler2D normalMapTexture;

void main()
{
    fsPosition = fsIn.fragmentPosition;

    fsNormal = texture(normalMapTexture, fsIn.textureCoord).rgb;

    if (length(fsNormal) == 0.0)
    {
        fsNormal = fsIn.normal;
    }

    fsAlbedo = texture(diffuseTexture, fsIn.textureCoord);
}
```

Note how it has three output **vectors**, each of which will be bound to the corresponding framebuffer attachment.

The fragment shader for SSAO _(second render pass)_:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec2 textureCoord;
} fsIn;

uniform sampler2D positionTexture;
uniform sampler2D normalTexture;

uniform sampler2D ssaoNoiseTexture;
uniform sampler1D ssaoKernelTexture;

uniform mat4 projection;

void main()
{
    vec3 fragmentPosition = texture(positionTexture, fsIn.textureCoord).xyz;
    vec3 normal = texture(normalTexture, fsIn.textureCoord).rgb;

    float radius = 0.5;
    float bias = 0.025;

    vec2 screenSize = textureSize(positionTexture, 0);
    vec2 noiseSize = textureSize(ssaoNoiseTexture, 0);
    float kernelSize = textureSize(ssaoKernelTexture, 0);

    vec2 noiseScale = screenSize / noiseSize;

    vec3 randomVec = normalize(texture(ssaoNoiseTexture, fsIn.textureCoord * noiseScale).xyz);

    vec3 tangent = normalize(randomVec - normal * dot(randomVec, normal));
    vec3 bitangent = cross(normal, tangent);
    mat3 TBN = mat3(tangent, bitangent, normal);

    float occlusion = 0.0;

    for (int i = 0; i < kernelSize; ++i)
    {
        vec3 samplePosition = TBN * texture(ssaoKernelTexture, i).xyz;

        if (dot(samplePosition, normal) < 0.0)
            samplePosition *= -1.0;

        samplePosition = fragmentPosition + samplePosition * radius;

        vec4 offsetUV = projection * vec4(samplePosition, 1.0);
        offsetUV.xyz /= offsetUV.w;
        offsetUV.xy = offsetUV.xy * 0.5 + 0.5;

        vec4 offsetPosition = texture(positionTexture, offsetUV.xy);

        float rangeCheck = smoothstep(0.0, 1.0, radius / abs(fragmentPosition.z - offsetPosition.z));

        occlusion += (samplePosition.z >= offsetPosition.z + bias ? 1.0 : 0.0) * rangeCheck;
    }

    occlusion = (occlusion / kernelSize);

    fragmentColor = vec4(vec3(occlusion), 1.0);
}
```

The blur fragment shader:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} fsIn;

uniform sampler2D blurInput;

uniform bool isHorizontalBlur;

float weight[5] = float[] (0.2270270270, 0.1945945946, 0.1216216216, 0.0540540541, 0.0162162162);

void main()
{
    vec2 textureOffset = 1.0 / textureSize(blurInput, 0);
    vec3 result = texture(blurInput, fsIn.textureCoord).rgb * weight[0];

    if (isHorizontalBlur) {
        for (int i = 1; i < 5; ++i) {
            result += texture(blurInput, fsIn.textureCoord + vec2(textureOffset.x * i, 0.0)).rgb * weight[i];
            result += texture(blurInput, fsIn.textureCoord - vec2(textureOffset.x * i, 0.0)).rgb * weight[i];
        }
    } else {
        for (int i = 1; i < 5; ++i) {
            result += texture(blurInput, fsIn.textureCoord + vec2(0.0, textureOffset.y * i)).rgb * weight[i];
            result += texture(blurInput, fsIn.textureCoord - vec2(0.0, textureOffset.y * i)).rgb * weight[i];
        }
    }

    fragmentColor = vec4(result, 1.0);
}
```

The final render pass fragment shader:

```glsl
#version 430

in VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} fsIn;

layout (location = 0) out vec4 fragmentColor;

struct PointLight
{
    vec3 position;
    float strength;
    vec4 color;
    // float farPlane;
    // mat4 projectionViewMatrices[6];
};

layout (std430, binding = 5) buffer PointLightData
{
    PointLight pointLight[];
} pointLightData;

uniform sampler2D positionTexture;
uniform sampler2D normalTexture;
uniform sampler2D albedoTexture;
uniform sampler2D ssaoTexture;

uniform vec3 cameraPosition;

uniform mat4 view;
uniform mat4 projection;

float attenuation_constant = 1.0;
float attenuation_linear = 0.09;
float attenuation_quadratic = 0.032;

void main()
{
    vec3 fragmentPosition = texture(positionTexture, fsIn.textureCoord).xyz;
    vec3 normal = texture(normalTexture, fsIn.textureCoord).rgb;
    vec4 albedoColor = texture(albedoTexture, fsIn.textureCoord);

    vec3 viewDirection = normalize(cameraPosition - fragmentPosition);

    float ambientOcclusion = texture(ssaoTexture, fsIn.textureCoord).r;

    vec4 lighting = albedoColor * 0.3 * ambientOcclusion;

    for (int i = 0; i < pointLightData.pointLight.length(); ++i)
    {
        PointLight light = pointLightData.pointLight[i];

        vec3 lightDirection = normalize(light.position - fragmentPosition);

        float lightDistance = length(light.position - fragmentPosition);
        float attenuation = 1.0 / (attenuation_constant + (attenuation_linear * lightDistance) + (attenuation_quadratic * lightDistance * lightDistance));

        vec4 diffuse = max(dot(normal, lightDirection), 0.0) * albedoColor * light.color;

        lighting += diffuse * attenuation;
    }

    fragmentColor = lighting;
}
```

Since all of the rendering is happening in passes on the frame buffers, the vertex shaders are very simple - just passing the parameters through to the next stage.

### Horizon-based ambient occlusion (HBAO)

Screen-space ambient occlusion algorithm (aka SSAO) is fine, but it can use even further improvement. Instead of calculating random samples inside imaginary sphere, you can be a little smarter about those samples and only generate them in a hemisphere, oriented towards the camera. Then, instead of checking the camera-space positions of samples and pixels, you can use the "horizon" of a current pixel and calculate the "horizion" for each of the samples. You can then use the angle difference between those two "horizons" to see if pixels are potentially occluded by some other polygon.

This technique gives better results for corners and edges than conventional SSAO technique.

<img data-src="/images/opengl-advanced-samples/sample-25-hbao-1.webp">
<img data-src="/images/opengl-advanced-samples/sample-25-hbao-2.webp">

Read more:

* [[nVidia] Image-Space Horizon-Based Ambient Occlusion](https://developer.download.nvidia.com/presentations/2008/SIGGRAPH/HBAO_SIG08b.pdf)

The difference between SSAO and HBAO implementations lays in literally one fragment shader, calculating the ambient occlusion values _(the second pass, from the SSAO paragraph)_:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec2 textureCoord;
} fsIn;

uniform sampler2D positionTexture;
uniform sampler2D normalTexture;

uniform sampler2D hbaoNoiseTexture;
uniform sampler1D hbaoKernelTexture;

uniform mat4 projection;

const float PI = 3.14;
const float NUM_SAMPLE_DIRECTIONS = 10.0;
const float NUM_SAMPLE_STEPS = 10.0;
const float INTENSITY = 2.0;

const float radius = 30;
const float bias = 0.5;

void main()
{
    vec3 fragmentPosition = texture(positionTexture, fsIn.textureCoord).xyz;
    vec3 normal = texture(normalTexture, fsIn.textureCoord).rgb;

    vec2 screenSize = textureSize(positionTexture, 0);
    vec2 noiseSize = textureSize(hbaoNoiseTexture, 0);

    vec2 noiseScale = screenSize / noiseSize;

    const float stepPixels = radius / (NUM_SAMPLE_STEPS + 1);
    const float alpha = 2 * PI / float(NUM_SAMPLE_DIRECTIONS);

    float occlusion = 0.0;

    for (float i = 0; i < NUM_SAMPLE_DIRECTIONS; ++i)
    {
        float angle = alpha * i;

        vec4 random = texture(hbaoNoiseTexture, fsIn.textureCoord * noiseScale);

        // vec2 dir = vec2(cos(angle), sin(angle));
        // vec2 cosSin = random.xy;
        // vec2 direction = vec2(dir.x * cosSin.x - dir.y * cosSin.y, dir.x * cosSin.y + dir.y * cosSin.x);
        // vec2 direction = rotateDirection(vec2(cos(angle), sin(angle)), random.xy);

        vec2 direction = vec2(cos(angle) * random.x - sin(angle) * random.y, cos(angle) * random.y + sin(angle) * random.x);

        float rayPixels = random.z * (stepPixels + 1.0);

        for (float t = 0; t < NUM_SAMPLE_STEPS; ++t)
        {
            vec2 sampleUV = round(rayPixels * direction) * (1.0 / screenSize) + fsIn.textureCoord;
            vec3 samplePosition = texture(positionTexture, sampleUV).xyz;

            rayPixels += stepPixels;

            vec3 sampleDirection = samplePosition - fragmentPosition;
            float v1 = dot(sampleDirection, sampleDirection);
            float v2 = dot(normal, sampleDirection) * 1.0 / sqrt(v1);
            occlusion += clamp(v2 - bias, 0.0, 1.0) * clamp(v1 * (-1.0 / (radius * radius)) + 1.0, 0.0, 1.0);
        }
    }

    occlusion *= INTENSITY / (NUM_SAMPLE_DIRECTIONS * NUM_SAMPLE_STEPS);
    occlusion = clamp(occlusion, 0.0, 1.0);

    fragmentColor = vec4(vec3(occlusion), 1.0);
}
```

### Volumetric lighting

Have you ever seen those beautiful rays of light passing through the clouds and becoming actually visible _rays_? There are lots of these in games too. They are also known as "god rays". They are actually tiny particles (presumingly dust) hanging in the air and reflecting the light.

<img data-src="/images/opengl-advanced-samples/sample-26-raymarching-2.webp">

The technique which allows to achieve such an effect (and tons of others) is called "raymarching".

The idea is that you store the pixels' depth and position information in both light space and camera space and then you cast an imaginary ray from the camera towards each pixel rendered. You then split this ray into a constant number of parts and project the resulting points into light space. You then compare the position of the point on the ray in light space to the depth value stored in the light space, and if the depth value stored is greater than the position on the ray - the point on the ray is visible from the perspective of a light, so you make the pixel on the screen a bit brighter. The more parts of the ray are visible by the light - the brighter the pixel will be.

Read more:

* [[nVidia] Volumetric lighting](https://developer.nvidia.com/volumetriclighting)
* [[blog] Volumetric lights](https://www.alexandre-pestana.com/volumetric-lights/)
* [[nVidia] Volumetric light scattering post-processing](https://developer.nvidia.com/gpugems/gpugems3/part-ii-light-and-shadows/chapter-13-volumetric-light-scattering-post-process)

The implementation for this technique is actually pretty straightforward: this is a two-pass deferred rendering application.

First, you set up a framebuffer with multiple attachments, for each of the scene parameters:

```cpp
auto deferredFragmentPositionTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentPositionTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentPositionTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentPositionTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB32F),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr
);

auto deferredFragmentNormalTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentNormalTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentNormalTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentNormalTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB32F),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr
);

auto deferredFragmentAlbedoTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentAlbedoTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentAlbedoTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentAlbedoTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGBA8),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGBA),
    static_cast<gl::GLenum>(GL_UNSIGNED_BYTE),
    nullptr
);

auto deferredFragmentLightSpacePositionTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentLightSpacePositionTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentLightSpacePositionTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentLightSpacePositionTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB32F),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr
);

auto deferredFragmentDepthTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

deferredFragmentDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
deferredFragmentDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

deferredFragmentDepthTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_DEPTH_COMPONENT),
    glm::vec2(static_cast<float>(window.getSize().x), static_cast<float>(window.getSize().y)),
    0,
    static_cast<gl::GLenum>(GL_DEPTH_COMPONENT),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr
);

auto deferredRenderingFramebuffer = std::make_unique<globjects::Framebuffer>();
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), deferredFragmentPositionTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT1), deferredFragmentNormalTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT2), deferredFragmentAlbedoTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT3), deferredFragmentLightSpacePositionTexture.get());
deferredRenderingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_DEPTH_ATTACHMENT), deferredFragmentDepthTexture.get());

// tell framebuffer it actually needs to render to **BOTH** textures, but does not have to output anywhere (last NONE argument, iirc)
deferredRenderingFramebuffer->setDrawBuffers({
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0),
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT1),
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT2),
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT3),
    static_cast<gl::GLenum>(GL_NONE)
});

deferredRenderingFramebuffer->printStatus(true);
```

Then, you render scene to that framebuffer:

```cpp
deferredRenderingFramebuffer->bind();

deferredRenderingPrePassProgram->use();

// render scene

deferredRenderingPrePassProgram->release();

deferredRenderingFramebuffer->unbind();
```

The vertex shader for this pass is:

```glsl
#version 410

layout (location = 0) in vec3 vertexPosition;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexTextureCoord;
// layout (location = 3) in vec3 vertexTangent;
// layout (location = 4) in vec3 vertexBitangent;

out VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
    vec3 lightSpaceCoord;
} vsOut;

out gl_PerVertex {
    vec4 gl_Position;
};

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;
uniform mat4 lightSpaceMatrix;

void main()
{
    vec4 position = model * vec4(vertexPosition, 1.0);
    vsOut.fragmentPosition = position.xyz / position.w;
    vsOut.normal = transpose(inverse(mat3(view * model))) * vertexNormal;
    vsOut.textureCoord = vertexTextureCoord;

    vec4 lightSpaceCoord = lightSpaceMatrix * model * vec4(vertexPosition, 1.0);
    vsOut.lightSpaceCoord = lightSpaceCoord.xyz / lightSpaceCoord.w;

    gl_Position = projection * view * model * vec4(vertexPosition, 1.0);
}
```

And the fragment shader is:

```glsl
#version 410

in VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
    vec3 lightSpaceCoord;
} fsIn;

layout (location = 0) out vec3 fsPosition;
layout (location = 1) out vec3 fsNormal;
layout (location = 2) out vec4 fsAlbedo;
layout (location = 3) out vec3 fsLightSpaceCoord;

uniform sampler2D diffuseTexture;
uniform sampler2D normalMapTexture;

void main()
{
    fsPosition = fsIn.fragmentPosition * 0.5 + 0.5;

    fsNormal = texture(normalMapTexture, fsIn.textureCoord).rgb;

    if (length(fsNormal) == 0.0)
    {
        fsNormal = fsIn.normal;
    }

    fsNormal = normalize(fsNormal.xyz) * 0.5 + 0.5;

    fsAlbedo = texture(diffuseTexture, fsIn.textureCoord);

    fsLightSpaceCoord = fsIn.lightSpaceCoord.xyz * 0.5 + 0.5;
}
```

In the final render pass you just combine the textures of the previous render pass(-es) into the final frame:

```cpp
// final render pass - merge textures from the deferred rendering pre-pass into a final frame
{
    ::glViewport(0, 0, static_cast<GLsizei>(window.getSize().x), static_cast<GLsizei>(window.getSize().y));
    ::glClearColor(static_cast<gl::GLfloat>(1.0f), static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(0.0f), static_cast<gl::GLfloat>(1.0f));
    glClear(GL_COLOR_BUFFER_BIT | GL_DEPTH_BUFFER_BIT);

    deferredRenderingFinalPassProgram->use();

    deferredFragmentPositionTexture->textureHandle().makeResident();
    deferredFragmentNormalTexture->textureHandle().makeResident();
    deferredFragmentAlbedoTexture->textureHandle().makeResident();

    deferredFragmentLightSpacePositionTexture->textureHandle().makeResident();
    shadowMapTexture->textureHandle().makeResident();

    pointLightDataBuffer->bindBase(GL_SHADER_STORAGE_BUFFER, 5);

    deferredRenderingFinalPassProgram->setUniform("positionTexture", deferredFragmentPositionTexture->textureHandle().handle());
    deferredRenderingFinalPassProgram->setUniform("normalTexture", deferredFragmentNormalTexture->textureHandle().handle());
    deferredRenderingFinalPassProgram->setUniform("albedoTexture", deferredFragmentAlbedoTexture->textureHandle().handle());

    deferredRenderingFinalPassProgram->setUniform("lightSpaceCoord", deferredFragmentLightSpacePositionTexture->textureHandle().handle());
    deferredRenderingFinalPassProgram->setUniform("shadowMap", shadowMapTexture->textureHandle().handle());

    deferredRenderingFinalPassProgram->setUniform("cameraPosition", cameraPos);
    deferredRenderingFinalPassProgram->setUniform("projection", cameraProjection);
    deferredRenderingFinalPassProgram->setUniform("view", cameraView);

    deferredRenderingFinalPassProgram->setUniform("lightSpaceMatrix", lightSpaceMatrix);
    deferredRenderingFinalPassProgram->setUniform("sunDirection", -lightPosition);
    deferredRenderingFinalPassProgram->setUniform("sunColor", glm::vec4(1.0f, 1.0f, 1.0f, 1.0f));

    quadModel->bind();
    quadModel->draw();
    quadModel->unbind();

    pointLightDataBuffer->unbind(GL_SHADER_STORAGE_BUFFER, 5);

    deferredFragmentPositionTexture->textureHandle().makeNonResident();
    deferredFragmentNormalTexture->textureHandle().makeNonResident();
    deferredFragmentAlbedoTexture->textureHandle().makeNonResident();

    deferredFragmentLightSpacePositionTexture->textureHandle().makeNonResident();
    shadowMapTexture->textureHandle().makeNonResident();

    deferredRenderingFinalPassProgram->release();
}
```

Using the fragment shader, which increases the brightness of the pixels based on the information from the light projection buffer. This is done using counting _(or the  "accumulating")_ the number of intersections of the ray from the viewing position towards the light source _(by checking the light projection buffer - the position of each pixel in the light space)_:

```glsl
#version 460

#extension GL_ARB_bindless_texture : require

in VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} fsIn;

layout (location = 0) out vec4 fragmentColor;

struct PointLight
{
    vec3 position;
    float strength;
    vec4 color;
    // float farPlane;
    // mat4 projectionViewMatrices[6];
};

layout (std430, binding = 5) buffer PointLightData
{
    PointLight pointLight[];
} pointLightData;

layout(bindless_sampler) uniform sampler2D positionTexture;
layout(bindless_sampler) uniform sampler2D normalTexture;
layout(bindless_sampler) uniform sampler2D albedoTexture;

layout(bindless_sampler) uniform sampler2D lightSpaceCoord;
layout(bindless_sampler) uniform sampler2D shadowMap;

uniform vec3 cameraPosition;
uniform vec3 sunDirection;
uniform vec4 sunColor;

uniform mat4 view;
uniform mat4 projection;
uniform mat4 lightSpaceMatrix;

float attenuation_constant = 1.0;
float attenuation_linear = 0.09;
float attenuation_quadratic = 0.032;

const float NB_STEPS = 30;
const float G_SCATTERING = 0.858;

#define M_PI 3.1415926535897932384626433832795

void main()
{
    vec3 fragmentPosition = texture(positionTexture, fsIn.textureCoord).xyz * 2 - 1;
    vec3 normal = texture(normalTexture, fsIn.textureCoord).rgb * 2 - 1;
    vec4 albedoColor = texture(albedoTexture, fsIn.textureCoord);

    vec3 viewDirection = normalize(cameraPosition - fragmentPosition);

    vec3 startPosition = cameraPosition;
    vec3 endRayPosition = fragmentPosition;

    vec3 rayVector = endRayPosition.xyz- startPosition;

    float rayLength = length(rayVector);
    vec3 rayDirection = normalize(rayVector);

    float stepLength = rayLength / NB_STEPS;

    vec3 raymarchingStep = rayDirection * stepLength;

    vec3 currentPosition = startPosition;

    vec4 accumFog = vec4(0.0);

    for (int i = 0; i < NB_STEPS; i++)
    {
        // basically perform shadow mapping
        vec4 worldInLightSpace = lightSpaceMatrix * vec4(currentPosition, 1.0f);
        worldInLightSpace /= worldInLightSpace.w;

        vec2 lightSpaceTextureCoord1 = (worldInLightSpace.xy * 0.5) + 0.5; // [-1..1] -> [0..1]
        float shadowMapValue1 = texture(shadowMap, lightSpaceTextureCoord1.xy).r;

        if (shadowMapValue1 > worldInLightSpace.z)
        {
            // Mie scaterring approximated with Henyey-Greenstein phase function
            float lightDotView = dot(normalize(rayDirection), normalize(-sunDirection));

            float scattering = 1.0 - G_SCATTERING * G_SCATTERING;
            scattering /= (4.0f * M_PI * pow(1.0f + G_SCATTERING * G_SCATTERING - (2.0f * G_SCATTERING) * lightDotView, 1.5f));

            accumFog += scattering * sunColor;
        }

        currentPosition += raymarchingStep;
    }

    accumFog /= NB_STEPS;

    // fade rays away
    // accumFog *= currentPosition / NB_STEPS);

    // lighting *= accumFog;

    vec4 fragmentPositionInLightSpace1 = lightSpaceMatrix * vec4(fragmentPosition, 1.0);
    fragmentPositionInLightSpace1 /= fragmentPositionInLightSpace1.w;

    vec2 shadowMapCoord1 = fragmentPositionInLightSpace1.xy * 0.5 + 0.5;

    // vec3 fragmentPositionInLightSpace2 = texture(lightSpaceCoord, fsIn.textureCoord).xyz;

    // vec2 shadowMapCoord2 = fragmentPositionInLightSpace2.xy;

    float thisDepth = fragmentPositionInLightSpace1.z;
    float occluderDepth = texture(shadowMap, shadowMapCoord1).r;

    float shadowFactor = 0.0;

    if (thisDepth > 0.0 && occluderDepth < 1.0 && thisDepth < occluderDepth)
    {
        shadowFactor = 1.0;
    }

    fragmentColor = ((albedoColor * shadowFactor * 0.3) + (accumFog * 0.7));
}
```

## Common rendering techniques

There are few simpler effects and techniques, which are more widely known (in that more tutorials on the Internet discuss these to some degree), but I feel like describing them as well.

### Bloom

Bloom is a relatively simple effect, but it looks really nice.

The idea is that you render the scene to a framebuffer, storing the light emitting value per pixel. You then blur the resulting image (using Gaussian blur, for instance) and blend the original scene image with the blurred one.

The only complex part here is Gaussian blur.

<img data-src="/images/opengl-advanced-samples/sample-15-bloom-2.webp">

```cpp
const auto antiAliasingSamples = 4;

auto bloomColorTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D_MULTISAMPLE));

bloomColorTexture->image2DMultisample(
    antiAliasingSamples,
    static_cast<gl::GLenum>(GL_RGB16F),
    glm::vec2(window.getSize().x, window.getSize().y),
    static_cast<gl::GLboolean>(true));

auto bloomBrightnessTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D_MULTISAMPLE));

bloomBrightnessTexture->image2DMultisample(
    antiAliasingSamples,
    static_cast<gl::GLenum>(GL_RGB16F),
    glm::vec2(window.getSize().x, window.getSize().y),
    static_cast<gl::GLboolean>(true));

auto renderBuffer = std::make_unique<globjects::Renderbuffer>();

renderBuffer->storageMultisample(antiAliasingSamples, static_cast<gl::GLenum>(GL_DEPTH24_STENCIL8), window.getSize().x, window.getSize().y);

auto bloomFramebuffer = std::make_unique<globjects::Framebuffer>();

bloomFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), bloomColorTexture.get());
bloomFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT1), bloomBrightnessTexture.get());

bloomFramebuffer->attachRenderBuffer(static_cast<gl::GLenum>(GL_DEPTH_STENCIL_ATTACHMENT), renderBuffer.get());

// tell framebuffer it actually needs to render to **BOTH** textures, but does not have to output anywhere (last NONE argument, iirc)
bloomFramebuffer->setDrawBuffers({ static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT1), static_cast<gl::GLenum>(GL_NONE) });

auto temporaryOutputTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

temporaryOutputTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
temporaryOutputTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

temporaryOutputTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_EDGE));
temporaryOutputTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_EDGE));

temporaryOutputTexture->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB16F),
    glm::vec2(window.getSize().x, window.getSize().y),
    0,
    static_cast<gl::GLenum>(GL_RGB),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr);

auto temporaryOutputFramebuffer = std::make_unique<globjects::Framebuffer>();

temporaryOutputFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), temporaryOutputTexture.get());

temporaryOutputFramebuffer->printStatus(true);

auto bloomBlurTexture1 = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

bloomBlurTexture1->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
bloomBlurTexture1->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

bloomBlurTexture1->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_EDGE));
bloomBlurTexture1->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_EDGE));

bloomBlurTexture1->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB16F),
    glm::vec2(window.getSize().x, window.getSize().y),
    0,
    static_cast<gl::GLenum>(GL_RGBA),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr);

auto bloomBlurFramebuffer1 = std::make_unique<globjects::Framebuffer>();

bloomBlurFramebuffer1->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), bloomBlurTexture1.get());

bloomBlurFramebuffer1->printStatus(true);

auto bloomBlurTexture2 = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_2D));

bloomBlurTexture2->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
bloomBlurTexture2->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

bloomBlurTexture2->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_EDGE));
bloomBlurTexture2->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_EDGE));

bloomBlurTexture2->image2D(
    0,
    static_cast<gl::GLenum>(GL_RGB16F),
    glm::vec2(window.getSize().x, window.getSize().y),
    0,
    static_cast<gl::GLenum>(GL_RGBA),
    static_cast<gl::GLenum>(GL_FLOAT),
    nullptr);

auto bloomBlurFramebuffer2 = std::make_unique<globjects::Framebuffer>();

bloomBlurFramebuffer2->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), bloomBlurTexture2.get());
```

With all that preparatory work, the rendering is a multi-step (multi-pass) process - first pass is to render scene to the `bloomFramebuffer`,
filling the `bloomColorTexture` and `bloomBrightnessTexture`:

```cpp
bloomFramebuffer->bind();

// render scene

bloomFramebuffer->unbind();
```

The shader for the first rendering pass is only different in fragment phase:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;
layout (location = 1) out vec4 brightColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
    vec4 fragmentPositionInLightSpace;
} fsIn;

uniform sampler2D shadowMap;
uniform sampler2D diffuseTexture;
uniform sampler2D specularMapTexture;
uniform sampler2D emissionMapTexture;

uniform vec3 lightPosition;
uniform vec3 lightColor;
uniform vec3 cameraPosition;

uniform vec3 emissionColor;

float attenuation_constant = 1.0;
float attenuation_linear = 0.09;
float attenuation_quadratic = 0.032;

float shadowCalculation(vec3 normal, vec3 lightDirection)
{
    vec3 shadowMapCoord = (fsIn.fragmentPositionInLightSpace.xyz / fsIn.fragmentPositionInLightSpace.w) * 0.5 + 0.5;
    float occluderDepth = texture(shadowMap, shadowMapCoord.xy).r;
    float thisDepth = shadowMapCoord.z;

    if (thisDepth > 1.0)
    {
        return 0.0;
    }

    float bias = max(0.05 * (1.0 - dot(normal, lightDirection)), 0.005);

    // PCF
    float shadow = 0.0;
    vec2 texelSize = 1.0 / textureSize(shadowMap, 0);

    for (int x = -1; x <= 1; ++x)
    {
        for (int y = -1; y <= 1; ++y)
        {
            float pcfDepth = texture(shadowMap, shadowMapCoord.xy + vec2(x, y) * texelSize).r;

            shadow += thisDepth - bias < pcfDepth  ? 1.0 : 0.0;
        }
    }

    shadow /= 9.0;

    return shadow;
}

void main()
{
    vec3 color = texture(diffuseTexture, fsIn.textureCoord).rgb;
    vec3 normal = normalize(fsIn.normal);

    // ambient
    vec3 ambient = 0.3 * color;

    // diffuse
    vec3 lightDirection = normalize(lightPosition - fsIn.fragmentPosition);
    float diff = max(dot(lightDirection, normal), 0.0);
    vec3 diffuse = diff * lightColor;

    // specular
    vec3 viewDirection = normalize(cameraPosition - fsIn.fragmentPosition);
    vec3 halfwayDirection = normalize(lightDirection + viewDirection);
    float spec = pow(max(dot(normal, halfwayDirection), 0.0), 64.0);
    vec3 specular = spec * lightColor;

    // attenuation
    float distance = length(lightPosition - fsIn.fragmentPosition);
    float attenuation = 1.0 / (attenuation_constant + attenuation_linear * distance + attenuation_quadratic * (distance * distance));

    // calculate shadow; this represents a global directional light, like Sun
    float shadow = shadowCalculation(normal, lightDirection);

    // these are the multipliers from different light maps (read from corresponding textures)
    float specularCoefficient = texture(specularMapTexture, fsIn.textureCoord).r;
    float emissionCoefficient = texture(emissionMapTexture, fsIn.textureCoord).r;

    vec3 lighting = (1 - emissionCoefficient) * ((shadow * ((diffuse * attenuation) + (specular * specularCoefficient * attenuation))) + (ambient * attenuation)) * color + (emissionColor * emissionCoefficient * 4.0);

    float brightness = dot(lighting, vec3(0.2126, 0.7152, 0.0722));

    if (brightness > 1.0) {
        brightColor = vec4(lighting, 1.0);
    } else {
        brightColor = vec4(0.0, 0.0, 0.0, 1.0);
    }

    fragmentColor = vec4(lighting, 1.0);
}
```

The second scene rendering blurs the framebuffer's data by rendering the texture to a quad (that is, a rectangle with same dimensions as the screen; this makes a deferred rendering)
and making each pixel an average of the neighbour pixels from the previous step:

```cpp
// copy data from the multisampled framebuffer to non-multisampled textures
// since the bloomFramebuffer has two color attachments, we need to blit twice
// but doing so removes the need in some additional conditions when running the blur

bloomFramebuffer->bind(static_cast<gl::GLenum>(GL_READ_FRAMEBUFFER));
temporaryOutputFramebuffer->bind(static_cast<gl::GLenum>(GL_DRAW_FRAMEBUFFER));

bloomFramebuffer->blit(
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0),
    std::array<gl::GLint, 4>{ 0, 0, static_cast<int>(window.getSize().x), static_cast<int>(window.getSize().y) },
    temporaryOutputFramebuffer.get(),
    std::vector<gl::GLenum>{ static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0) },
    std::array<gl::GLint, 4>{ 0, 0, static_cast<int>(window.getSize().x), static_cast<int>(window.getSize().y) },
    static_cast<gl::ClearBufferMask>(GL_COLOR_BUFFER_BIT),
    static_cast<gl::GLenum>(GL_NEAREST));

// same as
// glReadBuffer(GL_COLOR_ATTACHMENT0);
// glDrawBuffer(GL_COLOR_ATTACHMENT0);
// glBlitFramebuffer(0, 0, window.getSize().x, window.getSize().y, 0, 0, window.getSize().x, window.getSize().y, static_cast<gl::ClearBufferMask>(GL_COLOR_BUFFER_BIT), static_cast<gl::GLenum>(GL_NEAREST));

temporaryOutputFramebuffer->unbind();
bloomFramebuffer->unbind();

bloomFramebuffer->bind(static_cast<gl::GLenum>(GL_READ_FRAMEBUFFER));
bloomBlurFramebuffer2->bind(static_cast<gl::GLenum>(GL_DRAW_FRAMEBUFFER));

bloomFramebuffer->blit(
    static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0),
    std::array<gl::GLint, 4>{ 0, 0, static_cast<int>(window.getSize().x), static_cast<int>(window.getSize().y) },
    bloomBlurFramebuffer2.get(),
    std::vector<gl::GLenum>{ static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0) },
    std::array<gl::GLint, 4>{ 0, 0, static_cast<int>(window.getSize().x), static_cast<int>(window.getSize().y) },
    static_cast<gl::ClearBufferMask>(GL_COLOR_BUFFER_BIT),
    static_cast<gl::GLenum>(GL_NEAREST));

bloomFramebuffer->unbind();
bloomBlurFramebuffer2->unbind();

// third pass - blur the data stored in the bloom framebuffer with two-pass Gauss blur

bloomBlurProgram->use();

const auto blurPasses = 10;

// for the initial blur pass, use the texture from the bloomFramebuffer as an input

// we do not need anything extra here, since the bloomBlurFramebuffer2 (which we read from) will already contain the data from the bloomBrightnessTexture

bloomBlurProgram->setUniform("blurInput", 0);

for (auto i = 0; i < blurPasses; ++i)
{
    // bind one framebuffer to write blur results to and bind the texture from another framebuffer to read input data from (for this blur stage)
    if (i % 2 == 0)
    {
        // bind the new target framebuffer to write blur results to
        bloomBlurFramebuffer1->bind();
        // bind the texture from the previous blur pass to read input data for this stage from
        bloomBlurTexture2->bindActive(0);
        // tell shader that we want to use horizontal blur
        bloomBlurProgram->setUniform("isHorizontalBlur", true);
    }
    else
    {
        // bind the new target framebuffer to write blur results to
        bloomBlurFramebuffer2->bind();
        // bind the texture from the previous blur pass to read input data for this stage from
        if (i > 0)
            bloomBlurTexture1->bindActive(0);
        // tell shader that we want to use vertical blur
        bloomBlurProgram->setUniform("isHorizontalBlur", false);
    }

    // render quad with the texture from the active texture
    quadModel->bind();
    quadModel->draw();
    quadModel->unbind();

    if (i % 2 == 0)
    {
        // unbind the active framebuffer
        bloomBlurFramebuffer1->unbind();
        // unbind the active texture
        bloomBlurTexture2->unbindActive(0);
    }
    else
    {
        bloomBlurFramebuffer2->unbind();
        bloomBlurTexture1->unbindActive(0);
    }
}

bloomBlurProgram->release();
```

The fragment shader for blur render pass:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} fsIn;

uniform sampler2D blurInput;

uniform bool isHorizontalBlur;

float weight[5] = float[] (0.2270270270, 0.1945945946, 0.1216216216, 0.0540540541, 0.0162162162);

void main()
{
    vec2 textureOffset = 1.0 / textureSize(blurInput, 0);
    vec3 result = texture(blurInput, fsIn.textureCoord).rgb * weight[0];

    if (isHorizontalBlur) {
        for (int i = 1; i < 5; ++i) {
            result += texture(blurInput, fsIn.textureCoord + vec2(textureOffset.x * i, 0.0)).rgb * weight[i];
            result += texture(blurInput, fsIn.textureCoord - vec2(textureOffset.x * i, 0.0)).rgb * weight[i];
        }
    } else {
        for (int i = 1; i < 5; ++i) {
            result += texture(blurInput, fsIn.textureCoord + vec2(0.0, textureOffset.y * i)).rgb * weight[i];
            result += texture(blurInput, fsIn.textureCoord - vec2(0.0, textureOffset.y * i)).rgb * weight[i];
        }
    }

    fragmentColor = vec4(result, 1.0);
}
```

As with most cases described in this blog, the vertex shader for both stages is just the same old projection, nothing fancy:

```glsl
#version 410

layout (location = 0) in vec3 vertexPosition;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexTextureCoord;

out VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} vsOut;

out gl_PerVertex {
    vec4 gl_Position;
};

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;

void main()
{
    vsOut.fragmentPosition = vec3(model * vec4(vertexPosition, 1.0));
    vsOut.normal = vertexNormal;
    vsOut.textureCoord = vertexTextureCoord;

    gl_Position = projection * view * model * vec4(vertexPosition, 1.0);
}
```

Except there is no need to project anything when rendering to the quad, so the difference is only in the last line for the blur shader:

```glsl
gl_Position = vec4(vertexPosition, 1.0);
```

Now, the last rendering pass is quite simple, but that's where the magic becomes reality: we mix the original picture, rendered to the `temporaryOutputTexture` (first render pass) with
the blurred bright pixels from `bloomBlurTexture2` (the second render pass) to get the glow effect:

```cpp
bloomOutputProgram->use();

// here we use our temporary non-multisampled texture as one of the inputs for merge bloom effect stages, since OUR shader would only work with one sample layer
temporaryOutputTexture->bindActive(0);
bloomBlurTexture2->bindActive(1);

bloomOutputProgram->setUniform("colorOutput", 0);
bloomOutputProgram->setUniform("blurOutput", 1);

quadModel->bind();
quadModel->draw();
quadModel->unbind();

temporaryOutputTexture->unbindActive(0);
bloomBlurTexture2->unbindActive(1);

bloomOutputProgram->release();
```

Fragment shader:

```glsl
#version 410

layout (location = 0) out vec4 fragmentColor;

in VS_OUT {
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoord;
} fsIn;

uniform sampler2D blurInput;

uniform bool isHorizontalBlur;

float weight[5] = float[] (0.2270270270, 0.1945945946, 0.1216216216, 0.0540540541, 0.0162162162);

void main()
{
    vec2 textureOffset = 1.0 / textureSize(blurInput, 0);
    vec3 result = texture(blurInput, fsIn.textureCoord).rgb * weight[0];

    if (isHorizontalBlur) {
        for (int i = 1; i < 5; ++i) {
            result += texture(blurInput, fsIn.textureCoord + vec2(textureOffset.x * i, 0.0)).rgb * weight[i];
            result += texture(blurInput, fsIn.textureCoord - vec2(textureOffset.x * i, 0.0)).rgb * weight[i];
        }
    } else {
        for (int i = 1; i < 5; ++i) {
            result += texture(blurInput, fsIn.textureCoord + vec2(0.0, textureOffset.y * i)).rgb * weight[i];
            result += texture(blurInput, fsIn.textureCoord - vec2(0.0, textureOffset.y * i)).rgb * weight[i];
        }
    }

    fragmentColor = vec4(result, 1.0);
}
```

Vertex shader is same deferred rendering vertex shader - without any projection going on.

### Particle systems

Particle simulation is not really a technique in itself - it is a very big topic (how you design it from the code perspective, how do you simulate particles optimally - concurrent programming, SIMD, calculations on the GPU, etc.). But the way you render bunch of particles could be qualified as a rendering technique. It is mostly about instanced rendering.

The standard implementations include calculating every particle's params (position, rotation, texture animation frame etc.) on CPU and passing that data to the GPU to be rendered. More interesting approach is to do all the calculations on the GPU itself, only passing a handful of parameters from CPU, like the number of particles and their origin - essentially, the particle emitter parameters and then using the computing shader to do the rest.

Read more:

* [[learnopengl] Particles](https://learnopengl.com/In-Practice/2D-Game/Particles)
* [[opengl-tutorial] Particles / instancing](http://www.opengl-tutorial.org/intermediate-tutorials/billboards-particles/particles-instancing/)

<!-- TODO: compute shaders -->

### Skybox

Sky can really add atmosphere to your game. Rendering it might be as simple as rendering an inverted cube with texture on the insides of its faces and moving it along with the camera (or rather rendering it in camera space, not world space).

The skybox can use normal textures or it could use cubemaps - literally just a cube with textures on each of its six sides. Cubemaps allow you to calculate the texture coordinates by only having a vector from the center of the cube in the direction of view.

<img data-src="/images/opengl-advanced-samples/sample-17-skybox.webp">

Read more:

* [[learnopengl] Cubemaps](https://learnopengl.com/Advanced-OpenGL/Cubemaps)

In order to implement skybox, one first needs a cubemap texture:

```cpp
sf::Image m_top, m_bottom, m_left, m_right, m_front, m_back;

// load textures with SFML

std::map<gl::GLenum, sf::Image> skyboxTextures{
    { static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP_POSITIVE_X), m_right },
    { static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP_NEGATIVE_X), m_left },
    { static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP_POSITIVE_Y), m_top },
    { static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP_NEGATIVE_Y), m_bottom },
    { static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP_POSITIVE_Z), m_back },
    { static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP_NEGATIVE_Z), m_front },
};

auto skyboxTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP));

skyboxTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<GLint>(GL_LINEAR));
skyboxTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<GLint>(GL_LINEAR));

skyboxTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<GLint>(GL_CLAMP_TO_EDGE));
skyboxTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<GLint>(GL_CLAMP_TO_EDGE));
skyboxTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_R), static_cast<GLint>(GL_CLAMP_TO_EDGE));

skyboxTexture->bind();

for (auto& kv : skyboxTextures)
{
    const auto target = kv.first;
    auto& image = kv.second;

    if (target == gl::GL_TEXTURE_CUBE_MAP_POSITIVE_Y || target == gl::GL_TEXTURE_CUBE_MAP_NEGATIVE_Y)
    {
        image.flipVertically();
    }
    else
    {
        image.flipHorizontally();
    }

    ::glTexImage2D(
        static_cast<::GLenum>(target),
        0,
        static_cast<::GLenum>(GL_RGBA8),
        static_cast<::GLsizei>(image.getSize().x),
        static_cast<::GLsizei>(image.getSize().y),
        0,
        static_cast<::GLenum>(GL_RGBA),
        static_cast<::GLenum>(GL_UNSIGNED_BYTE),
        reinterpret_cast<const ::GLvoid*>(image.getPixelsPtr()));
}

skyboxTexture->unbind();
```

In my sample I manually create a box mesh with inverted normals:

```cpp
std::vector<glm::vec3> vertices{
    { -1.0f, 1.0f, 1.0f },
    { -1.0f, -1.0f, 1.0f },
    { 1.0f, -1.0f, 1.0f },
    { 1.0f, 1.0f, 1.0f },
    { -1.0f, 1.0f, 1.0f },
    { -1.0f, -1.0f, 1.0f },
    { -1.0f, -1.0f, -1.0f },
    { -1.0f, 1.0f, -1.0f },
    { 1.0f, 1.0f, 1.0f },
    { 1.0f, -1.0f, 1.0f },
    { 1.0f, -1.0f, -1.0f },
    { 1.0f, 1.0f, -1.0f },
    { -1.0f, 1.0f, -1.0f },
    { -1.0f, -1.0f, -1.0f },
    { 1.0f, -1.0f, -1.0f },
    { 1.0f, 1.0f, -1.0f },
    { -1.0f, 1.0f, -1.0f },
    { -1.0f, 1.0f, 1.0f },
    { 1.0f, 1.0f, 1.0f },
    { 1.0f, 1.0f, -1.0f },
    { -1.0f, -1.0f, -1.0f },
    { -1.0f, -1.0f, 1.0f },
    { 1.0f, -1.0f, 1.0f },
    { 1.0f, -1.0f, -1.0f },
};

std::vector<glm::vec3> normals{
    { 0.0f, 1.0f, 0.0f },
    { 0.0f, 1.0f, 0.0f },
    { 0.0f, 1.0f, 0.0f },

    { 0.0f, -1.0f, 0.0f },
    { 0.0f, -1.0f, 0.0f },
    { 0.0f, -1.0f, 0.0f },

    { -1.0f, 0.0f, 0.0f },
    { -1.0f, 0.0f, 0.0f },
    { -1.0f, 0.0f, 0.0f },

    { 0.0f, 0.0f, -1.0f },
    { 0.0f, 0.0f, -1.0f },
    { 0.0f, 0.0f, -1.0f },

    { 1.0f, 0.0f, 0.0f },
    { 1.0f, 0.0f, 0.0f },
    { 1.0f, 0.0f, 0.0f },

    { 0.0f, 0.0f, 1.0f },
    { 0.0f, 0.0f, 1.0f },
    { 0.0f, 0.0f, 1.0f },

    { 0.0f, 1.0f, 0.0f },
    { 0.0f, 1.0f, 0.0f },
    { 0.0f, 1.0f, 0.0f },

    { 0.0f, -1.0f, 0.0f },
    { 0.0f, -1.0f, 0.0f },
    { 0.0f, -1.0f, 0.0f },

    { -1.0f, 0.0f, 0.0f },
    { -1.0f, 0.0f, 0.0f },
    { -1.0f, 0.0f, 0.0f },

    { 0.0f, 0.0f, -1.0f },
    { 0.0f, 0.0f, -1.0f },
    { 0.0f, 0.0f, -1.0f },

    { 1.0f, 0.0f, 0.0f },
    { 1.0f, 0.0f, 0.0f },
    { 1.0f, 0.0f, 0.0f },

    { 0.0f, 0.0f, 1.0f },
    { 0.0f, 0.0f, 1.0f },
    { 0.0f, 0.0f, 1.0f },
};

std::for_each(vertices.begin(), vertices.end(), [this](glm::vec3 p) { return p * m_size; });

std::vector<unsigned int> indices{
    2, 1, 0,
    2, 0, 3,
    4, 5, 6,
    7, 4, 6,
    10, 9, 8,
    10, 8, 11,
    12, 13, 14,
    15, 12, 14,
    18, 17, 16,
    18, 16, 19,
    20, 21, 22,
    23, 20, 22,
};

std::vector<glm::vec2> uvs{
    { 0.333333f, 0.500000f },
    { 0.333333f, 0.000000f },
    { 0.000000f, 0.000000f },
    { 0.000000f, 0.500000f },
    { 0.000000f, 1.000000f },
    { 0.000000f, 0.500000f },
    { 0.333333f, 0.500000f },
    { 0.333333f, 1.000000f },
    { 1.000000f, 1.000000f },
    { 1.000000f, 0.500000f },
    { 0.666666f, 0.500000f },
    { 0.666666f, 1.000000f },
    { 0.333333f, 1.000000f },
    { 0.333333f, 0.500000f },
    { 0.666666f, 0.500000f },
    { 0.666666f, 1.000000f },
    { 0.340000f, 0.500000f },
    { 0.666666f, 0.500000f },
    { 0.666666f, 0.000000f },
    { 0.340000f, 0.000000f },
    { 0.666666f, 0.500000f },
    { 0.666666f, 0.000000f },
    { 1.000000f, 0.000000f },
    { 1.000000f, 0.500000f },
};

// use AbstractMesh to render this
```

The only difference in rendering skybox is the use of `samplerCube` for texture in the fragment shader:

```glsl
#version 410

in VS_OUT
{
    vec3 textureCoords;
} fsIn;

out vec4 fragmentColor;

uniform samplerCube cubeMap;

void main() {
    fragmentColor = texture(cubeMap, fsIn.textureCoords);
}
```

In vertex shader, however, the `textureCoords` variable is set using the camera view matrix passed as a uniform variable:

```glsl
#version 410

layout (location = 0) in vec3 vertexPosition;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexUV;

out VS_OUT
{
    vec3 textureCoords;
} vsOut;

out gl_PerVertex {
    vec4 gl_Position;
};

uniform mat4 projection;
uniform mat4 view;

void main()
{
    vec4 pos = projection * view * vec4(vertexPosition, 1.0);
    gl_Position = pos; //.xyww;
    vsOut.textureCoords = vertexPosition;
}
```

And passing the matrix to the uniform variable in the application _(thanks globjects for a beautiful API)_:

```cpp
skyboxRenderingProjectionTransformationUniform->set(cameraProjection);
skyboxRenderingViewTransformationUniform->set(glm::mat4(glm::mat3(cameraView)));
```

### Point lighting

When people are talking about point light sources, they usually mean light attenuation (light intensity fading with the distance from the light source) and object shading calculation for each light source around a given polygon (you calculate the Blinn-Phong shading considering each light source and light attenuation).

A bit more interesting here is the shadow casting technique. Instead of rendering shadow map as a simple 2D texture, you will need to utilize the cubemap, rendering an entire scene for each light source six times. But bear in mind: this is a very calculation-intensive technique, so refrain from doing so on the large landscapes. Alternatively, you should limit the light sources to the ones that are visible (or whose shadows are visible) in the camera space.

The implementation is quite interesting - it is an inversion of a cubemap rendering technique - instead of sampling the cubemap texture we now render to the cubemap texture.
Initializing this texture:

```cpp
auto pointShadowMapTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP));

pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_R), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));

pointShadowMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_BORDER_COLOR), glm::vec4(1.0f, 1.0f, 1.0f, 1.0f));

pointShadowMapTexture->bind();

const auto shadowMapSize = 2048;

for (auto i = 0; i < 6; ++i)
{
    ::glTexImage2D(
        static_cast<::GLenum>(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i),
        0,
        GL_DEPTH_COMPONENT,
        shadowMapSize,
        shadowMapSize,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr);
}

pointShadowMapTexture->unbind();
```

This texture will be attached to a framebuffer:

```cpp
auto pointShadowMappingFramebuffer = std::make_unique<globjects::Framebuffer>();
pointShadowMappingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_DEPTH_ATTACHMENT), pointShadowMapTexture.get());

pointShadowMappingFramebuffer->printStatus(true);
```

Since the cubemap is effectively six different textures, we will need to render to all of them, using different light projection matrices. We can somewhat optimize this by using geometry shader. Passing the matrices could be optimized using the SSBO:

```cpp
struct alignas(16) PointLightData
{
    glm::vec3 lightPosition;
    float farPlane;
    std::array<glm::mat4, 6> projectionViewMatrices;
};

auto pointLightDataBuffer = std::make_unique<globjects::Buffer>();

// calculate the 6 light projection matrices

glm::mat4 pointLightProjection = glm::perspective(glm::radians(90.0f), static_cast<float>(shadowMapSize / shadowMapSize), nearPlane, farPlane);

std::array<glm::mat4, 6> pointLightProjectionViewMatrices{
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)),
    pointLightProjection * glm::lookAt(pointLightPosition, pointLightPosition + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)),
};

PointLightData pointLightData{ pointLightPosition, farPlane, pointLightProjectionViewMatrices };

pointLightDataBuffer->setData(pointLightData, static_cast<gl::GLenum>(GL_DYNAMIC_COPY));
```

Then, on each frame, bind the SSBO and render scene to a framebuffer with cubemap texture attached to it:

```cpp
// bind the buffer on each frame

pointShadowMappingFramebuffer->bind();

pointLightDataBuffer->bindBase(GL_SHADER_STORAGE_BUFFER, 5);

pointShadowMappingProgram->use();

// render frame

pointLightDataBuffer->unbind(GL_SHADER_STORAGE_BUFFER, 5);

pointShadowMappingFramebuffer->unbind();

pointShadowMappingProgram->release();
```

For shadow mapping pass, the shaders are as following:

**geometry shader:**

```glsl
#version 430

layout (triangles) in;

// we emit 6 triangles for one input triangle - to be written to 6 textures of the cubemap
layout (triangle_strip, max_vertices = 18) out;

struct PointLight
{
   vec3 lightPosition;
   float farPlane;
   mat4 projectionViewMatrices[6];
};

layout (std430, binding = 5) buffer pointLightData
{
   PointLight pointLight;
};

out vec4 fragmentPosition;

void main()
{
    for (int face = 0; face < 6; ++face)
    {
        gl_Layer = face;

        for (int vertex = 0; vertex < 3; ++vertex)
        {
            fragmentPosition = gl_in[vertex].gl_Position;
            gl_Position = pointLight.projectionViewMatrices[face] * fragmentPosition;
            EmitVertex();
        }

        EndPrimitive();
    }
}
```

**fragment shader:**

```glsl
#version 430

in vec4 fragmentPosition;

struct PointLight
{
    vec3 lightPosition;
    float farPlane;
    mat4 projectionViewMatrices[6];
};

layout (std430, binding = 5) buffer pointLightData
{
    PointLight pointLight;
};

void main()
{
    float distance = length(fragmentPosition.xyz - pointLight.lightPosition) / pointLight.farPlane;

    gl_FragDepth = distance;
}
```

And for the final render pass _(sampling the shadow cubemap)_:

**vertex shader:**

```glsl
#version 430

in vec4 fragmentPosition;

struct PointLight
{
    vec3 lightPosition;
    float farPlane;
    mat4 projectionViewMatrices[6];
};

layout (std430, binding = 5) buffer pointLightData
{
    PointLight pointLight;
};

void main()
{
    float distance = length(fragmentPosition.xyz - pointLight.lightPosition) / pointLight.farPlane;

    gl_FragDepth = distance;
}
```

**fragment shader:**

```glsl
#version 430

layout (location = 0) out vec4 fragmentColor;

in VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
} fsIn;

uniform samplerCube shadowMap;
uniform sampler2D diffuseTexture;
uniform sampler2D specularMapTexture;
uniform sampler2D emissionMapTexture;

struct PointLight
{
    vec3 lightPosition;
    float farPlane;
    mat4 projectionViewMatrices[6];
};

layout (std430, binding = 5) buffer pointLightData
{
    PointLight pointLight;
};

uniform vec3 lightColor;
uniform vec3 cameraPosition;

// TODO: make these params uniforms
// uniform vec3 ambientColor;
// uniform vec3 diffuseColor;
// uniform float materialSpecular;
uniform vec3 emissionColor;

float attenuation_constant = 1.0;
float attenuation_linear = 0.09;
float attenuation_quadratic = 0.032;

const int pcfSamples = 20;
vec3 pcfSampleOffsetDirections[pcfSamples] = vec3[]
(
   vec3(1, 1, 1), vec3(1, -1, 1), vec3(-1, -1, 1), vec3(-1, 1, 1),
   vec3(1, 1, -1), vec3(1, -1, -1), vec3(-1, -1, -1), vec3(-1, 1, -1),
   vec3(1, 1, 0), vec3(1, -1, 0), vec3(-1, -1, 0), vec3(-1, 1, 0),
   vec3(1, 0, 1), vec3(-1, 0, 1), vec3(1, 0, -1), vec3(-1, 0, -1),
   vec3(0, 1, 1), vec3(0, -1, 1), vec3(0, -1, -1), vec3(0, 1, -1)
);

float shadowCalculation(vec3 normal)
{
    vec3 fragmentToLight = fsIn.fragmentPosition - pointLight.lightPosition;
    float occluderDepth = texture(shadowMap, fragmentToLight).r * pointLight.farPlane;
    float thisDepth = length(fragmentToLight);

    float bias = 0.15;

    // PCF
    /*float shadow = 0.0;
    float viewDistance = length(fsIn.fragmentPosition - cameraPosition);
    float diskRadius = (1.0 + (viewDistance / farPlane)) / 50.0;

    for (int i = 0; i < pcfSamples; ++i)
    {
        float closestDepth = texture(shadowMap, fragmentToLight + pcfSampleOffsetDirections[i] * diskRadius).r * farPlane;

        if (thisDepth - bias < closestDepth)
            shadow += 1.0;
    }

    shadow /= float(pcfSamples);*/

    float shadow = (thisDepth - bias) < occluderDepth ? 1.0 : 0.0;

    return shadow;
}

void main()
{
    vec3 color = texture(diffuseTexture, fsIn.textureCoords).rgb;
    vec3 normal = normalize(fsIn.normal);

    // ambient
    vec3 ambient = 0.3 * color;

    // diffuse
    vec3 lightDirection = normalize(pointLight.lightPosition - fsIn.fragmentPosition);
    float diff = max(dot(lightDirection, normal), 0.0);
    vec3 diffuse = diff * lightColor;

    // specular
    vec3 viewDirection = normalize(cameraPosition - fsIn.fragmentPosition);
    vec3 halfwayDirection = normalize(lightDirection + viewDirection);
    float spec = pow(max(dot(normal, halfwayDirection), 0.0), 64.0);
    vec3 specular = spec * lightColor;

    // attenuation
    float lightRadius = 10.0;
    float distance = length(pointLight.lightPosition - fsIn.fragmentPosition) / lightRadius;
    float attenuation = 1.0 / (attenuation_constant + attenuation_linear * distance + attenuation_quadratic * (distance * distance));

    // calculate shadow; this represents a global directional light, like Sun
    float shadow = shadowCalculation(normal);

    // these are the multipliers from different light maps (read from corresponding textures)
    float specularCoefficient = texture(specularMapTexture, fsIn.textureCoords).r;
    float emissionCoefficient = texture(emissionMapTexture, fsIn.textureCoords).r;

    // vec3 lighting = ((shadow * ((diffuse * attenuation) + (specular * specularCoefficient * attenuation))) + (ambient * attenuation)) * color + (emissionColor * emissionCoefficient);
    vec3 lighting = ((shadow * ((diffuse) + (specular * specularCoefficient))) + (ambient)) * color + (emissionColor * emissionCoefficient);

    fragmentColor = vec4(lighting, 1.0);
}
```

Note how the shadow map is `samplerCube` and sampling it is using the `vec3` instead of `vec2` as with planar (2D) textures.
Also note how lighting has something called "attenuation" now - this is the fading-out factor - the further the pixel is from the light source - the more bleak the lighting effect would be.

### Reflections

Reflections on the water surface are relatively simple to implement - for each pixel you calculate the direction to the camera upside-down (simply create a new view matrix with "up" vector pointing down) and then use the GLSL function `reflect()` to calculate the reflected vector. You then render take the color at the reflected position and add it (multiplied by some transparency factor) to the original pixel' color.

<img data-src="/images/opengl-advanced-samples/sample-18-reflection-3.webp">
<img data-src="/images/opengl-advanced-samples/sample-18-reflection-4.webp">

The concept of cubemaps is not only useful for skyboxes, but could also be used to calculate reflections on the objects. For each reflective object you render the scene from its position to the cubemap texture (so you will have to render the scene six times, which is huge overhead in itself) and when rendering the object, you reflect the camera view vector (vector from the pixel in world space to camera position) and sample the cubemap with that vector. You then add the sampled color to the source pixel color to receive a nice reflective surface effect.

This technique is very similar to both point light shadow mapping and skybox rendering combined.

We will need the cubemap texture to render to:

```cpp
auto reflectionMapTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP));

reflectionMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
reflectionMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

reflectionMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
reflectionMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
reflectionMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_R), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));

reflectionMapTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_BORDER_COLOR), glm::vec4(1.0f, 1.0f, 1.0f, 1.0f));

reflectionMapTexture->bind();

const auto reflectionMapSize = 2048;

for (auto i = 0; i < 6; ++i)
{
    ::glTexImage2D(
        static_cast<::GLenum>(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i),
        0,
        GL_RGBA8,
        reflectionMapSize,
        reflectionMapSize,
        0,
        GL_RGBA,
        GL_UNSIGNED_INT,
        nullptr);
}

reflectionMapTexture->unbind();
```

And on top of that, for things to not look mangled, we will need a depth texture. Which also has to be a cubemap:

```cpp
auto reflectionMapDepthTexture = std::make_unique<globjects::Texture>(static_cast<gl::GLenum>(GL_TEXTURE_CUBE_MAP));

reflectionMapDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MIN_FILTER), static_cast<gl::GLenum>(GL_LINEAR));
reflectionMapDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_MAG_FILTER), static_cast<gl::GLenum>(GL_LINEAR));

reflectionMapDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_S), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
reflectionMapDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_T), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));
reflectionMapDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_WRAP_R), static_cast<gl::GLenum>(GL_CLAMP_TO_BORDER));

reflectionMapDepthTexture->setParameter(static_cast<gl::GLenum>(GL_TEXTURE_BORDER_COLOR), glm::vec4(0.0f, 0.0f, 0.0f, 1.0f));

reflectionMapDepthTexture->bind();

for (auto i = 0; i < 6; ++i)
{
    ::glTexImage2D(
        static_cast<::GLenum>(GL_TEXTURE_CUBE_MAP_POSITIVE_X + i),
        0,
        GL_DEPTH_COMPONENT,
        reflectionMapSize,
        reflectionMapSize,
        0,
        GL_DEPTH_COMPONENT,
        GL_FLOAT,
        nullptr);
}

reflectionMapDepthTexture->unbind();
```

They both are then attached to a framebuffer:

```cpp
auto reflectionMappingFramebuffer = std::make_unique<globjects::Framebuffer>();
reflectionMappingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), reflectionMapTexture.get());
reflectionMappingFramebuffer->attachTexture(static_cast<gl::GLenum>(GL_DEPTH_ATTACHMENT), reflectionMapDepthTexture.get());

// tell framebuffer it actually needs to render to **BOTH** textures, but does not have to output anywhere (last NONE argument, iirc)
reflectionMappingFramebuffer->setDrawBuffers({ static_cast<gl::GLenum>(GL_COLOR_ATTACHMENT0), static_cast<gl::GLenum>(GL_NONE) });

reflectionMappingFramebuffer->printStatus(true);
```

Then, we have to render scene... well, 6 times - once for each side of the reflection cubemap:

```cpp
// this is a cubemap, hence aspect ratio **must** be 1:1
glm::mat4 reflectionProjection = glm::perspective(glm::radians(90.0f), 1.0f, nearPlane, farPlane);

// TODO: technically, this should be calculated as AABB / 2
const auto reflectionOffset = glm::vec3(0.0f, 0.05f, 0.0f);

reflectionMappingProgram->setUniform("projectionViewMatrices[0]", reflectionProjection * glm::lookAt(reflectiveModelPosition + reflectionOffset, reflectiveModelPosition + glm::vec3(1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
reflectionMappingProgram->setUniform("projectionViewMatrices[1]", reflectionProjection * glm::lookAt(reflectiveModelPosition + reflectionOffset, reflectiveModelPosition + glm::vec3(-1.0f, 0.0f, 0.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
reflectionMappingProgram->setUniform("projectionViewMatrices[2]", reflectionProjection * glm::lookAt(reflectiveModelPosition + reflectionOffset, reflectiveModelPosition + glm::vec3(0.0f, 1.0f, 0.0f), glm::vec3(0.0f, 0.0f, 1.0f)));
reflectionMappingProgram->setUniform("projectionViewMatrices[3]", reflectionProjection * glm::lookAt(reflectiveModelPosition + reflectionOffset, reflectiveModelPosition + glm::vec3(0.0f, -1.0f, 0.0f), glm::vec3(0.0f, 0.0f, -1.0f)));
reflectionMappingProgram->setUniform("projectionViewMatrices[4]", reflectionProjection * glm::lookAt(reflectiveModelPosition + reflectionOffset, reflectiveModelPosition + glm::vec3(0.0f, 0.0f, 1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));
reflectionMappingProgram->setUniform("projectionViewMatrices[5]", reflectionProjection * glm::lookAt(reflectiveModelPosition + reflectionOffset, reflectiveModelPosition + glm::vec3(0.0f, 0.0f, -1.0f), glm::vec3(0.0f, -1.0f, 0.0f)));

reflectionMappingFramebuffer->bind();

reflectionMappingProgram->use();

// render scene EXCEPT the mirror object

reflectionMappingFramebuffer->unbind();

reflectionMappingProgram->release();
```

For this we use geometry shader:

```glsl
#version 430

layout (triangles) in;

// we emit 6 triangles for one input triangle - to be written to 6 textures of the cubemap
layout (triangle_strip, max_vertices = 18) out;

in VS_OUT
{
    vec4 vertexPosition;
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
} gsIn[];

out GS_OUT
{
    vec4 vertexPosition;
    vec4 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
} gsOut;

uniform mat4 projectionViewMatrices[6];

// out vec4 fragmentPosition;

void main()
{
    for (int face = 0; face < 6; ++face)
    {
        gl_Layer = face;

        for (int vertex = 0; vertex < 3; ++vertex)
        {
            gsOut.vertexPosition = gsIn[vertex].vertexPosition;
            gsOut.fragmentPosition = projectionViewMatrices[face] * gsIn[vertex].vertexPosition;
            gsOut.normal = gsIn[vertex].normal;
            gsOut.textureCoords = gsIn[vertex].textureCoords;

            gl_Position = projectionViewMatrices[face] * gsIn[vertex].vertexPosition;

            EmitVertex();
        }

        EndPrimitive();
    }
}
```

Vertex shader for reflection mapping:

```glsl
#version 430

layout (location = 0) in vec3 vertexPosition;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexTextureCoords;

out VS_OUT
{
    vec4 vertexPosition;
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
} vsOut;

out gl_PerVertex {
    vec4 gl_Position;
};

uniform mat4 modelTransformation;

void main()
{
    vsOut.vertexPosition = modelTransformation * vec4(vertexPosition, 1.0);
    vsOut.fragmentPosition = vec3(modelTransformation * vec4(vertexPosition, 1.0));
    vsOut.normal = vertexNormal;
    vsOut.textureCoords = vertexTextureCoords;

    gl_Position = vsOut.vertexPosition;
}
```

And fragment shader:

```glsl
#version 430

in GS_OUT
{
    vec4 vertexPosition;
    vec4 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
} fsIn;

uniform sampler2D diffuseTexture;

void main()
{
    vec4 color = texture(diffuseTexture, fsIn.textureCoords);

    // TODO: add lighting component here
    gl_FragColor = color;
}
```

Note how the output is a 4-component vector instead of a regular 3-component one.

When rendering the reflective object on the final render pass, we sample the reflection cubemap texture and use the `reflect` function in **vertex shader** to calculate the sampling vector from the camera view vector:

```glsl
#version 430

layout (location = 0) in vec3 vertexPosition;
layout (location = 1) in vec3 vertexNormal;
layout (location = 2) in vec2 vertexTextureCoord;

out VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
    vec3 reflectedDirection;
} vsOut;

out gl_PerVertex {
    vec4 gl_Position;
};

uniform mat4 projection;
uniform mat4 view;
uniform mat4 model;

uniform vec3 cameraPosition;

void main()
{
    vsOut.fragmentPosition = vec3(model * vec4(vertexPosition, 1.0));
    vsOut.normal = vertexNormal;
    vsOut.textureCoords = vertexTextureCoord;
    vsOut.reflectedDirection = reflect(normalize(vsOut.fragmentPosition - cameraPosition), vertexNormal);

    gl_Position = projection * view * model * vec4(vertexPosition, 1.0);
}
```

In the fragment shader we then combine the color sampled from reflection texture and the object' own _(albedo)_ color:

```glsl
#version 430

layout (location = 0) out vec4 fragmentColor;

in VS_OUT
{
    vec3 fragmentPosition;
    vec3 normal;
    vec2 textureCoords;
    vec3 reflectedDirection;
} fsIn;

uniform samplerCube reflectionMap;
uniform sampler2D diffuseTexture;

// uniform vec3 lightColor;
uniform vec3 cameraPosition;

uniform mat4 model;

void main()
{
    vec4 reflectionColor = texture(reflectionMap, fsIn.reflectedDirection);
    vec4 albedoColor = texture(diffuseTexture, fsIn.textureCoords);

    fragmentColor = mix(albedoColor, reflectionColor, 0.6);
}
```
