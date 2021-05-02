---
title: "Irrlicht application template"
date: 2021-05-02T11:00:00+0700
---

Often when I start revising my old applicaitons with Irrlicht engine, I do few things very similarly.
Especially when the application contains GUI and uses Irrlicht tools for it.

This is mostly due to the **extremely** outdated nature of Irrlicht itself.
There are all sorts of things in it like the lack of 4K monitors support, the use of a very obscure font,
sample applications being a simple yet messy single CPP files, etc.

The common things I do include:

* using new C++ standard features such as:
  * shared pointers
  * automatic type inference
  * standard containers
  * C++-style string operations
* setting the new font for GUI, adopted to higher screen resolutions
* using CMake to build the project and vcpkg to manage dependencies
* utilizing object-oriented approach
* moving the classes to separate header and CPP files

In this blog I describe in bloody detail what I do and why.

<!--more-->

## Application structure

First of all, applicaiton structure is:

1. `main.cpp` - only instantiates the `Application` class and calls its `run()` method (the only public API of the `Application` class)
2. `Application` class - has two purposes:
  1. initialize Irrlicht sub-systems (device, GUI, renderer, scene manager, etc.)
  2. runs the main application loop, but delegate all the business logic to the `ApplicationDelegate` class
3. `ApplicaitonDelegate` - does a bit more than `Application`:
  - loads resources
  - resets default GUI settings
  - creates the camera
  - defines (user) events callbacks (handling business logic of the application)
  - handles every frame update (rendering, third-party systems' updates such as physics, AI, math, etc.)
4. `IrrlichtEventReceiver` - intercepts Irrlicht-specific events and delegates the execution to a corresponding event handler from `ApplicationDelegate`

For my [ShootThem! game](https://github.com/shybovycha/shoot-them/) I have also defined game state in a somewhat interesting manner, uncommon for game development - utilizing the principles
of Redux from front-end, since it made sense to me back in the day - user and application trigger events, event receiver delegates them to the application delegate,
which returns a new state and passes the state back to the store, application uses the state to render things on the screen.

For a more complicated application it might be easier to use the state machine approach instead, which is a common approach in gamedev.

## Build system

Back to the topic of the application template, it is, again, a common practice in a modern (C++) world to use some sort of a build system and dependency manager.
The reason being the simplicity of setup and deploy, abstraction from dependencies' build process, ease of distribution.
Makefiles do not really play well with Windows-based systems, for example. Whilst vcproj / mssys build is hard to get right on Unix-based systems.
And handling dependencies is a pain in the neck when dependencies themselves have different build processes and transient dependencies.

Hence I stick with CMake for the overall application build process (although I honestly hate it, since it is so bloated and cumbersome)
and vcpkg to get dependencies for my applications.

### CMake configuration

The main entry point of an application build process is the `CMakeLists.txt` file, which defines how the application is built.

Following the best practices collected all over the world (conference talks, *recent* blogs, etc. - the existing documentation for CMake is horrible in that regard it 
shows off the **bad practices** and utilizes the outdated approaches), her is what I came up with:

```cmake
cmake_minimum_required(VERSION 3.16 FATAL_ERROR)

project(myapplication VERSION 1.0.1 LANGUAGES CXX)

set(EXECUTABLE_NAME myapplication)

set(SOURCES
    "src/main.cpp"
    "src/ApplicationDelegate.h"
    "src/ApplicationDelegate.cpp"
    "src/IrrlichtEventReceiver.h"
    "src/IrrlichtEventReceiver.cpp"
    "src/Application.h"
    "src/Application.cpp"
)

add_executable(${EXECUTABLE_NAME} ${SOURCES})
```

## VCPKG configuration

VCPKG _can_ use the configuration file when invoked with params swithing it to the _manifest mode_. I am unsure why this is not the default mode until now, but it is what it is.

Anyways, the `vcpkg.json` file looks like this:

```json
{
    "$schema": "https://raw.githubusercontent.com/microsoft/vcpkg/master/scripts/vcpkg.schema.json",
    "name": "myapplication",
    "version-string": "0.1.0",
    "dependencies": [
        "irrlicht"
    ]
}
```

When invoked from a command line, vcpkg will tell you what you should add to your `CMakeLists.txt` file to find the package you've just installed:

```bash
$ vcpkg install irrlicht
Computing installation plan...
The following packages are already installed:
    irrlicht[core]:x86-windows -> 1.8.4-10
Package irrlicht:x86-windows is already installed

Total elapsed time: 214.7 us

The package irrlicht:x86-windows provides CMake targets:

    find_package(irrlicht CONFIG REQUIRED)
    target_link_libraries(main PRIVATE Irrlicht)
```

Just follow these instructions and add those lines to the `CMakeLists.txt` file:

```cmake
cmake_minimum_required(VERSION 3.16 FATAL_ERROR)

project(myapplication VERSION 1.0.1 LANGUAGES CXX)

set(EXECUTABLE_NAME myapplication)

set(SOURCES
    "src/main.cpp"
    "src/ApplicationDelegate.h"
    "src/ApplicationDelegate.cpp"
    "src/IrrlichtEventReceiver.h"
    "src/IrrlichtEventReceiver.cpp"
    "src/Application.h"
    "src/Application.cpp"
)

add_executable(${EXECUTABLE_NAME} ${SOURCES})

find_package(irrlicht CONFIG REQUIRED)
target_link_libraries(${EXECUTABLE_NAME} PRIVATE Irrlicht)
```

### Building

The only trick to building the project with vcpkg is the invocation of CMake:

```bash
cmake -B build -S .  -DCMAKE_TOOLCHAIN_FILE=C:\Users\myuser\vcpkg\vcpkg\scripts\buildsystems\vcpkg.cmake
```

This will download all the dependencies specified in the `vcpkg.json` file and generate the project for the corresponding build system.

The build process then is straightforward:

```bash
cmake --build build
```

## Source

The sources for the application units mentioned above are:

### `main.cpp`

```cpp
#include "Application.h"

#include <memory>

int main() {
    std::unique_ptr<Application> app = std::make_unique<Application>();

    app->run();

    return 0;
}
```

### `Application.h`

```cpp
#pragma once

#include "ApplicationDelegate.h"
#include "IrrlichtEventReceiver.h"

#include <irrlicht.h>

#include <iostream>
#include <string>

class Application {
public:
    Application();

    void run();

private:
    void initialize();

    irr::IrrlichtDevice* device;
    irr::video::IVideoDriver* driver;
    irr::scene::ISceneManager* smgr;
    irr::gui::IGUIEnvironment* guienv;

    std::shared_ptr<ApplicationDelegate> applicationDelegate;
    std::unique_ptr<IrrlichtEventReceiver> eventReceiver;
};
```

### `Application.cpp`

```cpp
#include "Application.h"

Application::Application() {}

void Application::initialize() {
    device = irr::createDevice(
        irr::video::EDT_OPENGL,
        irr::core::dimension2d<irr::u32>(1024, 768),
        32,
        false,
        false,
        true,
        0
    );

    if (!device) {
        std::cerr << "Could not initialize video device\n";

        return;
    }

    device->setWindowCaption(L"irrPaint3D");

    driver = device->getVideoDriver();
    smgr = device->getSceneManager();
    guienv = device->getGUIEnvironment();

    applicationDelegate = std::make_shared<ApplicationDelegate>(device);

    applicationDelegate->initialize();

    eventReceiver = std::make_unique<IrrlichtEventReceiver>(applicationDelegate);

    device->setEventReceiver(eventReceiver.get());
}

void Application::run() {
    initialize();

    while (device->run()) {
        if (!device->isWindowActive() || !device->isWindowFocused() || device->isWindowMinimized()) {
            continue;
        }

        applicationDelegate->update();
    }

    device->drop();
}
```

### `ApplicationDelegate.h`

```cpp
#pragma once

#include <fstream>
#include <iostream>
#include <memory>
#include <queue>
#include <sstream>
#include <string>

#include <irrlicht.h>

class ApplicationDelegate
{
public:
    ApplicationDelegate(irr::IrrlichtDevice* _device);

    void initialize();

    void update();

    // event callbacks

    void quit();

private:
    void initGUI();

    void loadGUI();

    void resetFont();

    irr::gui::IGUIElement* getElementByName(const std::string& name);
    irr::gui::IGUIElement* getElementByName(const std::string& name, irr::gui::IGUIElement* parent);

    irr::IrrlichtDevice* device;

    irr::video::IVideoDriver* driver;
    irr::scene::ISceneManager* smgr;
    irr::gui::IGUIEnvironment* guienv;
    irr::scene::ICameraSceneNode* camera;
};
```

### `ApplicationDelegate.cpp`

```cpp
#include "ApplicationDelegate.h"

ApplicationDelegate::ApplicationDelegate(irr::IrrlichtDevice* _device) :
    device(_device),
    smgr(device->getSceneManager()),
    guienv(device->getGUIEnvironment()),
    driver(device->getVideoDriver()),
    camera(nullptr)
{
}

void ApplicationDelegate::initialize()
{
    camera = smgr->addCameraSceneNode();

    initGUI();
}

void ApplicationDelegate::initGUI()
{
    loadGUI();

    resetFont();

    createToolbar();
}

void ApplicationDelegate::loadGUI()
{
    guienv->loadGUI("media/gui.xml", nullptr);
}

irr::gui::IGUIElement* ApplicationDelegate::getElementByName(const std::string& name)
{
    return getElementByName(name, guienv->getRootGUIElement());
}

irr::gui::IGUIElement* ApplicationDelegate::getElementByName(const std::string& name, irr::gui::IGUIElement* parent)
{
    std::queue<irr::gui::IGUIElement*> queue;

    queue.push(parent);

    while (!queue.empty())
    {
        auto currentElement = queue.front();

        queue.pop();

        auto currentElementName = std::string(currentElement->getName());

        if (name == currentElementName) {
            return currentElement;
        }

        for (auto child : currentElement->getChildren())
        {
            queue.push(child);
        }
    }

    return nullptr;
}

void ApplicationDelegate::resetFont()
{
    irr::gui::IGUIFont* font = guienv->getFont("media/calibri.xml");
    guienv->getSkin()->setFont(font);
}

void ApplicationDelegate::quit()
{
    device->closeDevice();
}

void ApplicationDelegate::update()
{
    driver->beginScene(true, true, irr::video::SColor(0, 200, 200, 200));

    smgr->drawAll();

    guienv->drawAll();

    driver->endScene();
}
```

Worth mentioning the two helpers I have added to this class:

* `getElementByName(std::string name)`
* `getElementByName(std::string name, irr::gui::IGUIElement* parent)`,

They are really a big help when working with GUI designed with out-of-the-box GUIEditor and has to do with Irrlicht specifics: any given
GUI element in Irrlicht can have both ID (integer) or name (string). In the GUI editor you _can_ specify both, but what you will end up with
is trying to remember all those integers when both designing a GUI in the editor and later, when you write code for that GUI.

One way to do it properly would be to automatically generate a mapping between GUI elements' IDs and names and then use it in the application,
but out-of-the-box GUI editor is not that sophisticated and you will have to do it by hand by analyzing the XML. That's where the issue lies:
if your GUI element does not have a name - good luch figuring out a name for it - was it a button X or a button Z?

Hence I operate on elements' names. They are nothing more than strings. This is a suboptimal solution, since recursively traversing the whole GUI
and comparing each element's string name to a given string is somewhat expensive, when done on every GUI event (think MouseMove event or something)
or every frame (for whatever reason). But it gets the job done without the need to modify the GUIEditor itself.

One other tricky thing with the out-of-the-box GUIEditor is the texture for GUI elements - the default value for `texture` attribute in the editor is `-1`,
whatever that means so one must specify a relative path to a texture by hand and hope editor won't crash
(since it will try to load the file, if it is not present in the working directory of editor - it will throw an unhandled exception and die).
Hence I manually edit the GUI XML file and specify the paths to the textures after I'm done designing the GUI.

Also, since there is no out-of-the-box `SaveFileDialog` component (which is weird, since there is a `IGUIFileOpenDialog`,
which could be easily extended to support opening files functionality), I often have to implement it by hand like
[described](http://irrlicht.sourceforge.net/forum/viewtopic.php?p=166255#p166255) by someone on the forums years ago (`2008` to be exact).

The other detail is that the default font provided by Irrlicht is extremely old and looks ugly on modern high-resolution screens.
Irrlicht does not work with TTF or other font types out-of-the-box, but uses a bitmap with all the supported characters instead.
I converted one of the fonts available in my Windows system, [Calibri](https://docs.microsoft.com/en-us/typography/font-list/calibri),
to a font map (PNG + XML file combo, consumable by Irrlicht) with the out-of-the-box `FontTool` provided by Irrlicht, by selecting
the maximum available file dimensions, reasonable font size (`18px` in my case) and **enabling the alpha** for the font map (otherwise
all the GUI elements will have ugly black background):

<img data-src="/images/irrlicht-template/fonttool.png" alt="FontTool configuration">

Then the `resetFont` method of the `ApplicationDelegate` uses the new font for the whole application:

```cpp
irr::gui::IGUIFont* font = guienv->getFont("media/calibri.xml");
guienv->getSkin()->setFont(font);
```

Note that you must store **both** PNG and XML files under the same directory (or specify the relative path to the font map (PNG) in the XML file by hand).

It makes a significant difference in the looks of an application:

Before:

<img data-src="/images/irrlicht-template/old-font-look.png" alt="FontTool configuration">

After:

<img data-src="/images/irrlicht-template/new-font-look.png" alt="FontTool configuration">

### `IrrlichtEventReceiver.h`

```cpp
#pragma once

#include "ApplicationDelegate.h"

#include <memory>

#include <irrlicht.h>

class IrrlichtEventReceiver : public irr::IEventReceiver
{
public:
    IrrlichtEventReceiver(std::shared_ptr<ApplicationDelegate> applicationDelegate);

    bool OnEvent(const irr::SEvent& event) override;

private:
    std::shared_ptr<ApplicationDelegate> applicationDelegate;
};
```

### `IrrlichtEventReceiver.cpp`

```cpp
#include "IrrlichtEventReceiver.h"

IrrlichtEventReceiver::IrrlichtEventReceiver(std::shared_ptr<ApplicationDelegate> _applicationDelegate) : applicationDelegate(std::move(_applicationDelegate))
{
}

bool IrrlichtEventReceiver::OnEvent(const irr::SEvent& event)
{
    if (event.EventType == irr::EET_KEY_INPUT_EVENT)
    {
        // handling CTRL+S
        if (event.KeyInput.Key == irr::KEY_KEY_S && event.KeyInput.Control)
        {
            // applicationDelegate->saveSomething();
        }

        return false;
    }

    if (event.EventType == irr::EET_MOUSE_INPUT_EVENT)
    {
        // handling mouse movement
        if (event.MouseInput.Event == irr::EMIE_MOUSE_MOVED)
        {
            // applicationDelegate->onMouseMoveSomething();
        }
    }

    return false;
}
```

## Copying resources

One more tricky thing with these applications is how to copy the resources (fonts, 3D assets, etc.) to the executable directory.
In CMake this could be done by using this trick (so that it copies files at **build time** as opposed to **generation time**):

```cmake
add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory  ${CMAKE_CURRENT_LIST_DIR}/media $<TARGET_FILE_DIR:${EXECUTABLE_NAME}>/media)
```

## Outro

This is about everything I wanted to share. This template should set you up in few-ish minutes so that you can start implementing the actual application logic
instead of wondering why there are some missing references for linker or some nuisance like those.
