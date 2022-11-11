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

<div class="content-read-marker" data-fraction="25"></div>

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
See the details of its implementation [below](#bonussavefiledialog).

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

<div class="content-read-marker" data-fraction="50"></div>

## Copying resources

One more tricky thing with these applications is how to copy the resources (fonts, 3D assets, etc.) to the executable directory.
In CMake this could be done by using this trick (so that it copies files at **build time** as opposed to **generation time**):

```cmake
add_custom_command(TARGET ${EXECUTABLE_NAME} POST_BUILD COMMAND ${CMAKE_COMMAND} -E copy_directory  ${CMAKE_CURRENT_LIST_DIR}/media $<TARGET_FILE_DIR:${EXECUTABLE_NAME}>/media)
```

## Bonus: `SaveFileDialog`

This is something I implement in my own editors (like the [level editor for ShootThem!](https://github.com/shybovycha/shoot-them/tree/master/Editor)).
As mentioned above, this implementation takes its roots at [this forum post by @pera](http://irrlicht.sourceforge.net/forum/viewtopic.php?p=166255#p166255) from 2008, which,
is a fork itself of a [code by @MolokoTheMole from 2007](http://irrlicht.sourceforge.net/forum/viewtopic.php?p=126280#p126280), apparently.

### `SaveFileDialog.h`

```cpp
#pragma once

#include <codecvt> // for converting wstring to string
#include <xlocbuf> // for converting wstring to string

#include <memory>
#include <string>

#include <irrlicht.h>

const irr::s32 SAVE_FILE_DIALOG_WIDTH = 350;
const irr::s32 SAVE_FILE_DIALOG_HEIGHT = 250;

class SaveFileDialog : public irr::gui::IGUIFileOpenDialog
{
public:
    //! constructor
    SaveFileDialog(const wchar_t* title,
            irr::gui::IGUIEnvironment* environment,
            irr::gui::IGUIElement* parent,
            irr::s32 id,
            bool restoreCWD = false,
            irr::io::path::char_type* startDir = 0);

    //! destructor
    virtual ~SaveFileDialog();

    //! returns the filename of the selected file. Returns NULL, if no file was selected.
    virtual const wchar_t* getFileName() const override;

    //! Returns the filename of the selected file. Is empty if no file was selected.
    virtual const irr::io::path& getFileNameP() const;

    //! Returns the directory of the selected file. Returns NULL, if no directory was selected.
    virtual const irr::io::path& getDirectoryName();

    //! Returns the directory of the selected file converted to wide characters. Returns NULL if no directory was selected.
    virtual const wchar_t* getDirectoryNameW() const;

    //! called if an event happened.
    virtual bool OnEvent(const irr::SEvent& event) override;

    //! draws the element and its children
    virtual void draw() override;

protected:

    void setFileName(const irr::io::path& name);

    void setDirectoryName(const irr::io::path& name);

    //! Ensure filenames are converted correct depending on wide-char settings
    void pathToStringW(irr::core::stringw& result, const irr::io::path& p);

    //! fills the listbox with files.
    void fillListBox();

    //! sends the event that the file has been selected.
    void sendSelectedEvent(irr::gui::EGUI_EVENT_TYPE type);

    //! sends the event that the file choose process has been canceled
    void sendCancelEvent();

    irr::core::position2d<irr::s32> DragStart;
    irr::io::path FileName;
    irr::core::stringw FileNameW;
    irr::io::path FileDirectory;
    irr::io::path FileDirectoryFlat;
    irr::core::stringw FileDirectoryFlatW;
    irr::io::path RestoreDirectory;
    irr::io::path StartDirectory;

    irr::gui::IGUIButton* CloseButton;
    irr::gui::IGUIButton* OKButton;
    irr::gui::IGUIButton* CancelButton;
    irr::gui::IGUIListBox* FileBox;
    irr::gui::IGUIEditBox* FileNameText;
    irr::gui::IGUIElement* EventParent;

    irr::io::IFileSystem* FileSystem;
    irr::io::IFileList* FileList;

    bool Dragging;
};
```

### `SaveFileDialog.cpp`

```cpp
#include "SaveFileDialog.h"

//! constructor
SaveFileDialog::SaveFileDialog(const wchar_t* title,
        irr::gui::IGUIEnvironment* environment,
        irr::gui::IGUIElement* parent,
        irr::s32 id,
        bool restoreCWD,
        irr::io::path::char_type* startDir)
        : IGUIFileOpenDialog(
        environment,
        parent,
        id,
        irr::core::rect<irr::s32>(
                ((parent ? parent : environment->getRootGUIElement())->getAbsolutePosition().getWidth() -
                 SAVE_FILE_DIALOG_WIDTH) / 2,
                ((parent ? parent : environment->getRootGUIElement())->getAbsolutePosition().getHeight() -
                 SAVE_FILE_DIALOG_HEIGHT) / 2,
                ((parent ? parent : environment->getRootGUIElement())->getAbsolutePosition().getWidth() -
                 SAVE_FILE_DIALOG_WIDTH) / 2 + SAVE_FILE_DIALOG_WIDTH,
                ((parent ? parent : environment->getRootGUIElement())->getAbsolutePosition().getHeight() -
                 SAVE_FILE_DIALOG_HEIGHT) / 2 + SAVE_FILE_DIALOG_HEIGHT)
),
          FileNameText(nullptr),
          FileList(nullptr),
          Dragging(false)
{
#ifdef _DEBUG
    IGUIElement::setDebugName("SaveFileDialog");
#endif

    Text = title;

    FileSystem = Environment ? Environment->getFileSystem() : nullptr;

    if (FileSystem)
    {
        FileSystem->grab();

        if (restoreCWD)
        {
            RestoreDirectory = FileSystem->getWorkingDirectory();
        }

        if (startDir)
        {
            StartDirectory = startDir;
            FileSystem->changeWorkingDirectoryTo(startDir);
        }
    }
    else
    {
        return;
    }

    irr::gui::IGUISpriteBank* sprites = nullptr;
    irr::video::SColor color(255, 255, 255, 255);
    irr::gui::IGUISkin* skin = Environment->getSkin();

    if (skin)
    {
        sprites = skin->getSpriteBank();
        color = skin->getColor(irr::gui::EGDC_WINDOW_SYMBOL);
    }

    const irr::s32 buttonw = skin ? skin->getSize(irr::gui::EGDS_WINDOW_BUTTON_WIDTH) : 2;
    const irr::s32 posx = RelativeRect.getWidth() - buttonw - 4;

    CloseButton = Environment->addButton(irr::core::rect<irr::s32>(posx, 3, posx + buttonw, 3 + buttonw),
            this,
            -1,
            L"",
            skin ? skin->getDefaultText(irr::gui::EGDT_WINDOW_CLOSE) : L"Close");

    CloseButton->setSubElement(true);
    CloseButton->setTabStop(false);

    if (sprites)
    {
        CloseButton->setSpriteBank(sprites);
        CloseButton->setSprite(irr::gui::EGBS_BUTTON_UP, skin->getIcon(irr::gui::EGDI_WINDOW_CLOSE), color);
        CloseButton->setSprite(irr::gui::EGBS_BUTTON_DOWN, skin->getIcon(irr::gui::EGDI_WINDOW_CLOSE), color);
    }

    CloseButton->setAlignment(irr::gui::EGUIA_LOWERRIGHT, irr::gui::EGUIA_LOWERRIGHT, irr::gui::EGUIA_UPPERLEFT,
            irr::gui::EGUIA_UPPERLEFT);
    CloseButton->grab();

    OKButton = Environment->addButton(
            irr::core::rect<irr::s32>(RelativeRect.getWidth() - 80, 30, RelativeRect.getWidth() - 10, 50),
            this, -1, skin ? skin->getDefaultText(irr::gui::EGDT_MSG_BOX_OK) : L"OK");
    OKButton->setSubElement(true);
    OKButton->setAlignment(irr::gui::EGUIA_LOWERRIGHT, irr::gui::EGUIA_LOWERRIGHT, irr::gui::EGUIA_UPPERLEFT,
            irr::gui::EGUIA_UPPERLEFT);
    OKButton->grab();

    CancelButton = Environment->addButton(
            irr::core::rect<irr::s32>(RelativeRect.getWidth() - 80, 55, RelativeRect.getWidth() - 10, 75),
            this, -1, skin ? skin->getDefaultText(irr::gui::EGDT_MSG_BOX_CANCEL) : L"Cancel");
    CancelButton->setSubElement(true);
    CancelButton->setAlignment(irr::gui::EGUIA_LOWERRIGHT, irr::gui::EGUIA_LOWERRIGHT, irr::gui::EGUIA_UPPERLEFT,
            irr::gui::EGUIA_UPPERLEFT);
    CancelButton->grab();

    FileBox = Environment->addListBox(irr::core::rect<irr::s32>(10, 55, RelativeRect.getWidth() - 90, 230), this, -1,
            true);
    FileBox->setSubElement(true);
    FileBox->setAlignment(irr::gui::EGUIA_UPPERLEFT, irr::gui::EGUIA_LOWERRIGHT, irr::gui::EGUIA_UPPERLEFT,
            irr::gui::EGUIA_LOWERRIGHT);
    FileBox->grab();

    FileNameText = Environment->addEditBox(nullptr, irr::core::rect<irr::s32>(10, 30, RelativeRect.getWidth() - 90, 50),
            true,
            this);
    FileNameText->setSubElement(true);
    FileNameText->setAlignment(irr::gui::EGUIA_UPPERLEFT, irr::gui::EGUIA_LOWERRIGHT, irr::gui::EGUIA_UPPERLEFT,
            irr::gui::EGUIA_UPPERLEFT);
    FileNameText->grab();

    setTabGroup(true);

    fillListBox();
}


//! destructor
SaveFileDialog::~SaveFileDialog()
{
    if (CloseButton)
        CloseButton->drop();

    if (OKButton)
        OKButton->drop();

    if (CancelButton)
        CancelButton->drop();

    if (FileBox)
        FileBox->drop();

    if (FileNameText)
        FileNameText->drop();

    if (FileSystem)
    {
        // revert to original CWD if path was set in constructor
        if (!RestoreDirectory.empty())
            FileSystem->changeWorkingDirectoryTo(RestoreDirectory);

        FileSystem->drop();
    }

    if (FileList)
        FileList->drop();
}

//! returns the filename of the selected file. Returns NULL, if no file was selected.
const wchar_t* SaveFileDialog::getFileName() const
{
    return FileNameW.c_str();
}

const irr::io::path& SaveFileDialog::getFileNameP() const
{
    return FileName;
}

//! Returns the directory of the selected file. Returns NULL, if no directory was selected.
const irr::io::path& SaveFileDialog::getDirectoryName()
{
    return FileDirectoryFlat;
}

const wchar_t* SaveFileDialog::getDirectoryNameW() const
{
    return FileDirectoryFlatW.c_str();
}

void SaveFileDialog::setFileName(const irr::io::path& name)
{
    FileName = name;
    pathToStringW(FileNameW, FileName);
}

void SaveFileDialog::setDirectoryName(const irr::io::path& name)
{
    FileDirectory = name;
    FileDirectoryFlat = name;
    FileSystem->flattenFilename(FileDirectoryFlat);
    pathToStringW(FileDirectoryFlatW, FileDirectoryFlat);
}

//! called if an event happened.
bool SaveFileDialog::OnEvent(const irr::SEvent& event)
{
    if (isEnabled())
    {
        switch (event.EventType)
        {
        case irr::EET_GUI_EVENT:
            switch (event.GUIEvent.EventType)
            {
            case irr::gui::EGET_ELEMENT_FOCUS_LOST:
                Dragging = false;
                break;

            case irr::gui::EGET_BUTTON_CLICKED:
                if (event.GUIEvent.Caller == CloseButton || event.GUIEvent.Caller == CancelButton)
                {
                    sendCancelEvent();
                    remove();

                    return true;
                }
                else if (event.GUIEvent.Caller == OKButton)
                {
                    if (!FileName.empty())
                    {
                        sendSelectedEvent(irr::gui::EGET_FILE_SELECTED);
                        remove();

                        return true;
                    }

                    if (!FileDirectory.empty())
                    {
                        sendSelectedEvent(irr::gui::EGET_DIRECTORY_SELECTED);
                    }
                }
                break;

            case irr::gui::EGET_LISTBOX_CHANGED:
            {
                irr::s32 selected = FileBox->getSelected();

                if (FileList && FileSystem)
                {
                    if (FileList->isDirectory(selected))
                    {
                        setFileName("");
                        setDirectoryName(FileList->getFullFileName(selected));
                    }
                    else
                    {
                        setDirectoryName("");
                        setFileName(FileList->getFullFileName(selected));
                    }

                    return true;
                }
            }
                break;

            case irr::gui::EGET_LISTBOX_SELECTED_AGAIN:
            {
                const irr::s32 selected = FileBox->getSelected();

                if (FileList && FileSystem)
                {
                    if (FileList->isDirectory(selected))
                    {
                        setDirectoryName(FileList->getFullFileName(selected));
                        FileSystem->changeWorkingDirectoryTo(FileDirectory);
                        fillListBox();
                        setFileName("");
                    }
                    else
                    {
                        setFileName(FileList->getFullFileName(selected));
                    }

                    return true;
                }
            }
                break;

            case irr::gui::EGET_EDITBOX_CHANGED:
                if (event.GUIEvent.Caller == FileNameText)
                {
                    setFileName(FileNameText->getText());

                    return true;
                }
                break;

            case irr::gui::EGET_EDITBOX_ENTER:
                if (event.GUIEvent.Caller == FileNameText)
                {
                    irr::io::path dir(FileNameText->getText());

                    if (FileSystem->changeWorkingDirectoryTo(dir))
                    {
                        fillListBox();
                        setFileName("");
                    }
                    else
                    {
                        setFileName(FileNameText->getText());
                    }

                    return true;
                }
                break;

            default:
                break;
            }
            break;

        case irr::EET_MOUSE_INPUT_EVENT:
            switch (event.MouseInput.Event)
            {
            case irr::EMIE_MOUSE_WHEEL:
                return FileBox->OnEvent(event);

            case irr::EMIE_LMOUSE_PRESSED_DOWN:
                DragStart.X = event.MouseInput.X;
                DragStart.Y = event.MouseInput.Y;
                Dragging = true;
                return true;

            case irr::EMIE_LMOUSE_LEFT_UP:
                Dragging = false;
                return true;

            case irr::EMIE_MOUSE_MOVED:
                if (!event.MouseInput.isLeftPressed())
                    Dragging = false;

                if (Dragging)
                {
                    // gui window should not be dragged outside its parent
                    if (Parent)
                        if (event.MouseInput.X < Parent->getAbsolutePosition().UpperLeftCorner.X + 1 ||
                            event.MouseInput.Y < Parent->getAbsolutePosition().UpperLeftCorner.Y + 1 ||
                            event.MouseInput.X > Parent->getAbsolutePosition().LowerRightCorner.X - 1 ||
                            event.MouseInput.Y > Parent->getAbsolutePosition().LowerRightCorner.Y - 1)

                            return true;

                    move(irr::core::position2d<irr::s32>(event.MouseInput.X - DragStart.X,
                            event.MouseInput.Y - DragStart.Y));
                    DragStart.X = event.MouseInput.X;
                    DragStart.Y = event.MouseInput.Y;
                    return true;
                }
                break;

            default:
                break;
            }
        }
    }

    return IGUIElement::OnEvent(event);
}


//! draws the element and its children
void SaveFileDialog::draw()
{
    if (!IsVisible)
        return;

    irr::gui::IGUISkin* skin = Environment->getSkin();

    irr::core::rect<irr::s32> rect = AbsoluteRect;

    rect = skin->draw3DWindowBackground(this, true, skin->getColor(irr::gui::EGDC_ACTIVE_BORDER),
            rect, &AbsoluteClippingRect);

    if (!Text.empty())
    {
        rect.UpperLeftCorner.X += 2;
        rect.LowerRightCorner.X -= skin->getSize(irr::gui::EGDS_WINDOW_BUTTON_WIDTH) + 5;

        irr::gui::IGUIFont* font = skin->getFont(irr::gui::EGDF_WINDOW);

        if (font)
            font->draw(Text.c_str(), rect,
                    skin->getColor(irr::gui::EGDC_ACTIVE_CAPTION),
                    false, true, &AbsoluteClippingRect);
    }

    IGUIElement::draw();
}

void SaveFileDialog::pathToStringW(irr::core::stringw& result, const irr::io::path& p)
{
#ifndef _IRR_WCHAR_FILESYSTEM
    std::wstring_convert<std::codecvt_utf8_utf16<wchar_t>> wstringConverter;
    result = wstringConverter.from_bytes(p.c_str()).c_str();
#else
    result = p.c_str();
#endif
}

//! fills the listbox with files.
void SaveFileDialog::fillListBox()
{
    irr::gui::IGUISkin* skin = Environment->getSkin();

    if (!FileSystem || !FileBox || !skin)
        return;

    if (FileList)
        FileList->drop();

    FileBox->clear();

    FileList = FileSystem->createFileList();
    irr::core::stringw s;

    if (FileList)
    {
        for (irr::u32 i = 0; i < FileList->getFileCount(); ++i)
        {
            pathToStringW(s, FileList->getFileName(i));
            FileBox->addItem(s.c_str(),
                    skin->getIcon(FileList->isDirectory(i) ? irr::gui::EGDI_DIRECTORY : irr::gui::EGDI_FILE));
        }
    }

    if (FileNameText)
    {
        setDirectoryName(FileSystem->getWorkingDirectory());
        pathToStringW(s, FileDirectory);
        FileNameText->setText(s.c_str());
    }
}

//! sends the event that the file has been selected.
void SaveFileDialog::sendSelectedEvent(irr::gui::EGUI_EVENT_TYPE type)
{
    irr::SEvent event;
    event.EventType = irr::EET_GUI_EVENT;
    event.GUIEvent.Caller = this;
    event.GUIEvent.Element = nullptr;
    event.GUIEvent.EventType = type;
    Parent->OnEvent(event);
}


//! sends the event that the file choose process has been cancelled
void SaveFileDialog::sendCancelEvent()
{
    irr::SEvent event;
    event.EventType = irr::EET_GUI_EVENT;
    event.GUIEvent.Caller = this;
    event.GUIEvent.Element = nullptr;
    event.GUIEvent.EventType = irr::gui::EGET_FILE_CHOOSE_DIALOG_CANCELLED;
    Parent->OnEvent(event);
}
```

## Outro

This is about everything I wanted to share. This template should set you up in few-ish minutes so that you can start implementing the actual application logic
instead of wondering why there are some missing references for linker or some nuisance like those.

<div class="content-read-marker" data-fraction="100"></div>
