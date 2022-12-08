---
layout: post
title: "ShootThem! revival"
date: '2020-11-13T13:46:24+11:00'
---

Quite some time ago I [dag out]({% post_url tumblr/2015-04-06-shootthem %}) the sources of an old game of mine, ShootThem! made back when I was at high-school, around 2006.

It has been over a decade ever since I made that game and I had enough inspiration to revisit the code once again.

This is a short update blog about what it used to be and what it became as of now.

<div><img src="/images/shootthem-revival/ShootThem 15_09_2020 8_19_43 AM.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/ShootThem 26_09_2020 8_19_43 PM.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/square1_render2.webp" loading="lazy"></div>

<!--more-->

## 2006

Back in 2006 I have barely learned some OOP and C / C++, just enough to be able to use it in school programming competitions.

The game looked awful - there were no textures in the whole game, the models were mostly hand-made by myself in some dodgy 3D editor
(Calligra TrueSpace or something alike).

<div><img src="/images/shootthem-revival/ShootThem 13_09_2020 10_18_45 PM.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/ShootThem 13_09_2020 10_18_52 PM.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/ShootThem 15_09_2020 8_19_36 AM.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/ShootThem 15_09_2020 8_19_43 AM.webp" loading="lazy"></div>

The code was terrible as well - static variables, huge functions, not to mention the whole game was a huge hard-coded, non-configurable mess in one `main.cpp` file.

<div><img src="/images/shootthem-revival/Screen Shot 2020-11-13 at 2.02.28 pm.webp" loading="lazy"></div>

But there was also a map editor, which allowed for some customization - it was basically a first-person camera and few magical buttons that allowed to define target positions for a specific level. The level mesh and output file were both provided via command-line arguments.

## 2015

I have dag out the sources and decided to put them on GitHub for historical reference. I have also managed to set up CMake for the thing
so that it could be built on _any_ computer without bothering too much about magical compiler options and everything.

## 2020

I have started the rework. First things first, I reworked the CMake configuration to a modern style and replaced the dependency "management" (essentially an instruction to manually unzip Irrlich and IrrKlang files and put them in specific directories) with CMake's `Fetch` module.

Then I thought about reshaping the code, so I have split one big `main.cpp` file with static variables and a handful of functions into classes, encapsulating the logic - `Level`, `Score`, `PlayerState`, `GameState`. This allowed me to maintain the working state of the game whilst actually improving the state of the code ([commit](https://github.com/shybovycha/shoot-them/commit/b49b478e76c33fd59c5b1960127b21a1039fce9a)).

I have also extracted the settings into a separate file, `settings.xml` so that the application can be actually somewhat configured. Also, the magical data format for the levels was replaced with much more readable XML, `levels.xml`:

```xml
<levels>
  <level>
    <model>room1.dae</model>
    <entities>
      <light>
        <position x="-717.96405029296875" y="129.1875" z="200"/>
      </light>
      <light>
          <position x="0" y="125.34170532226563" z="258.18295288085938"/>
      </light>
      <target>
          <position x="-765.06158447265625" y="271.0589599609375" z="670.49334716796875"/>
      </target>
      <!-- ... -->
    </entities>
  </level>
</levels>
```

Next step was to reduce the coupling of code components and introduce some sort of state management. I am still unhappy with what I ended up with, but it is still infinitely better than one single `main()` function.

I have ended up with something similar to Redux from front-end world (at least that was an initial plan) - there is a _Store_, where the state is stored; there are _Actions_, which define a change to the state; there is a _Reducer_, which modifies the state of the app; and finally, there are _Subscribers_ which react to state changes and render the state of the app.

But the issue was that there are many _effects_ to handle like playing sound, loading and unloading the data, etc.

Along the way I have also kept maintaining a list of things I was thinking about - bugs found, improvements, refactoring ideas, etc ([README.md history](https://github.com/shybovycha/shoot-them/commits/master/README.md)).

Few improvements included adding a main menu, replacing the "UI" with something more user-friendly (previously it was just an ugly line of text), improving the "drunk shooter" effect (ended up replacing it with shaders, which is not a trivial task in Irrlicht - more on that later).

<div><img src="/images/shootthem-revival/ShootThem 20_09_2020 2_48_59 PM.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/ShootThem 26_09_2020 8_19_43 PM.webp" loading="lazy"></div>

I have also started reworking the 3D models for the game and decided it was a good point to just show how I _meant_ the original art to be in the game:

<div><img src="/images/shootthem-revival/egypt1_render.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/forest1_render.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/square1_render2.webp" loading="lazy"></div>

The art is still far from being ready, but here's a preview of the new training level:

<div><img src="/images/shootthem-revival/blender_shooting_range3.webp" loading="lazy"></div>

The editor was also reworked... Or better being said, made from scratch - now it actually has a GUI, it allows to manage all the levels without restarting the app, it allows to place targets and move them around, it allows to place light sources on the scene and it is not a first-person camera anymore.

<div><img src="/images/shootthem-revival/Shoot Them Editor 26_09_2020 1_09_03 PM.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/Shoot Them Editor 26_09_2020 1_12_56 PM.webp" loading="lazy"></div>

<div class="content-read-marker" data-fraction="25"></div>

## Lessons learned

There are quite a few lessons that I have learned.

### CMake sucks. A lot.

I freaking hate CMake for a few very good reasons:

- awful syntax, it is still same good old Make, just with syntax being wrecked
- lack of any structure to the projects - everyone does it in their own preferred way, so no one will actually tell you _the way_ to set up your project
- the lack of dependency managemen - which, in 2020, seems like a huge drawback, especially if you don't develop every single bit of your application from scratch
- performance - yes, performance - by default it performs a clean build every time - it compiles _everything_ from scratch every time you change anything
- documentation - no one will tell you _the way to do it_; most manuals and articles still rely on CMake 2.6 or 3.0 at best; a lot of information online is so dispersed, inconsistent and out-of-date - you will be surprised!

The only reason I use CMake is because all the dependencies I use _could_ be used with CMake. Well, except Irrlicht and IrrKlang.

I thought Bazel or Buck will be better, but they still require one to create custom build instructions for those dependencies and it is especially hard for a cross-platform libraries such as Irrlicht. Well, at least they do solve most of CMake issues I have listed above.

### Irrlicht is old

It is _extremely_ old - last _bugfix_ version, `1.8.4`, was released on `9th July 2016` (which is only 4.5 years ago as of now) and the corresponding minor version, `1.8.0` being released on `8th November 2012` (8 years ago as of now).

It might not seem like a _huge_ deal, but here are few things that I have noticed:

- shaders are hard to get working
- only GLSL and HSL are supported
- no frame buffer support
- renderers are OpenGL 3 and DirectX 9
- out-of-the-box tools are quite limited:
  - camera movement is either Maya-style, First-Person-Shooter-style or manual control, through matrices and vectors
  - the GUI out of the box is not suited for high-resolution monitors with fonts being pixelated
  - overall a lot of bugs with GUI (random events missed or fired)
- ray-picking is buggy
- textures / lighting is buggy as heck (sometimes textures are disappearing, separate mesh triangles being shaded differently from others, etc.)
- anti-aliasing not working properly for sprites (which makes my HUD look ugly most of the time, even though it is high-res images)

Check these screenshots for example:

<div><img src="/images/shootthem-revival/ShootThem 26_09_2020 8_19_30 PM.webp" loading="lazy"></div>
<div><img src="/images/shootthem-revival/ShootThem 26_09_2020 8_19_43 PM.webp" loading="lazy"></div>

It is the same scene, with lighting being messed up for _all target models_ on the latter screenshot.

With the subset of APIs that are needed for my game, I am quite seriously considering just sticking to lower-level graphics APIs (OpenGL / DirectX) and stepping away from Irrlicht.

### C++ is not easy

With the idea of having redux-like state management and the strong type system of C++ I thought it would be a good idea to leverage the compiler and language features to do some compile-time checks for the actions that are being dispatched. I started with templates, but then figured out (the most painful way possible) that templates are merely hints for compiler.

Also, one can not define method overrides for the base class of the hierarchy by specifying child class as method param.

So my initial design like below fell apart very quickly:

```cpp
class BaseAction {};

class PlaySoundAction : public BaseAction {
  public:
    std::string filename;
};

class DestroyEntityAction : public BaseAction {
  public:
    std::shared_ptr<Entity> entity;
};

// ---

class BaseReducer {
  public:
    virtual void processAction(BaseAction* action) = 0;
};

class Reducer : public BaseReducer {
  public:
    // error: 'processAction' marked 'override' but does not override any member functions
    virtual void processAction(DestroyEntityAction* action) override {
      // ...
    }

    // error: 'processAction' marked 'override' but does not override any member functions
    virtual void processAction(PlaySoundAction* action) override {
      // ...
    }
};

// ---

class Store {
  private:
    std::unique_ptr<BaseReducer> reducer;
    std::queue<BaseAction*> actionQueue;

    void processQueue() {
      while (!actionQueue.empty()) {
        auto action = actionQueue.front();

        actionQueue.pop();

        // error: no matching member function for call to 'processAction'
        reducer->processAction(action);
      }
    }
};
```
<div class="content-read-marker" data-fraction="50"></div>

Templates won't solve the issue either:

```cpp
class BaseReducer {
  public:
    // only this method will be called every time
    template <class T>
    void processAction(T action) {
      std::cout << "processing unknown action" << std::endl;
    }
};

class Reducer : public BaseReducer {
  public:
    void processAction(DestroyEntityAction* action) {
      BaseReducer::processAction(action);
      std::cout << "destroying an object" << std::endl;
    }

    void processAction(PlaySoundAction* action) {
      BaseReducer::processAction(action);
      std::cout << "playing a sound" << std::endl;
    }
};
```

Also, a template method can't be virtual.

So here comes another trick front-end developers use, the `type` field for actions and `switch..case` statement in reducer:

```cpp
class BaseReducer {
  public:
    virtual void processAction(BaseAction* action) = 0;
};

class Reducer : public BaseReducer {
  public:
    virtual void processAction(BaseAction* action) override {
      switch (action->type) {
        case ActionType::PLAY_SOUND:
          std::cout << "playing a sound" << std::endl;
        break;

        case ActionType::DESTROY_ENTITY:
          std::cout << "destroying an object" << std::endl;
        break;

        default:
          std::cout << "unknown action" << std::endl;
      }
    }
};
```

### Editor is really important

My game has multiple levels. Each level requires its own lighting and target placement. It is quite hard to do properly without the editor.
And the first version of my "editor" has proven that.

Even the new version of editor lacks lots of features - one still can't rotate the targets or see the actual targets, there are no particle emitters available, there are no scene-specific events that would have allowed some neat mechanics.

And finally, it is hard to see the result in the editor itself - there will be differences with the game.

However, I did consider using GoDot engine for the game.

There are features that I really liked about it.

#### I18n support

<div><img src="/images/shootthem-revival/Screen Shot 2020-08-26 at 9.58.10 am.webp" loading="lazy"></div>

#### Configurable user actions

This way you can have same handler for gamepad buttons' and keyboard/mouse input events:

<div><img src="/images/shootthem-revival/Screen Shot 2020-08-26 at 9.57.58 am.webp" loading="lazy"></div>

#### Attaching scripts to any object on the scene

This allows for a magnitude of interesting mechanics.

<div><img src="/images/shootthem-revival/Screen Shot 2020-08-26 at 9.55.19 am.webp" loading="lazy"></div>

For a number of reasons I didn't choose it - there were some user experience issues significantly reducing my productivity.

<div class="content-read-marker" data-fraction="75"></div>

#### Asset management

Sspecifically the neccesity to specify every single texture for every single object on a scene. Check out this chicken model:

<div><img src="/images/shootthem-revival/Screen Shot 2020-08-26 at 9.55.34 am.webp" loading="lazy"></div>

It is just an OBJ file with every material specified:

```
mtllib chicken.mtl

# 262 vertex positions
v  18.556948 41.779099 -12.354659
v  20.53804 50.283123 -13.547608

# ...

# Mesh '0' with 68 faces
g 0
usemtl Material #1
f  1/1/1 2/2/2 3/3/3
f  3/3/3 2/2/2 4/4/4

# ...
```

Yet in the editor you'll have to manually assign every single material to every single sub-mesh:

<div><img src="/images/shootthem-revival/Screen Shot 2020-08-26 at 9.56.56 am.webp" loading="lazy"></div>

#### Not really clear scene node types

Now which node do I use for kinematic physical body?

<div><img src="/images/shootthem-revival/Screen Shot 2020-08-26 at 10.10.15 am.webp" loading="lazy"></div>

Just what the heck is "spatial" and why does it have both "camera" and "collision shape" next to "audio stream"?

<div><img src="/images/shootthem-revival/Screen Shot 2020-08-26 at 10.10.20 am.webp" loading="lazy"></div>

#### Lack of any sort of pre-made solutions

So that every time you need an FPS camera - you have to define it from zero, using the matrix transformations. This increases the amount of boilerplate code and actually reduces the productivity even compared to Irrlicht:

<div><img src="/images/shootthem-revival/Screen Shot 2020-08-26 at 9.55.19 am.webp" loading="lazy"></div>

## This is not the end

I am not done with this project revival just yet - I still want it to build under OSX and I still have few assets to put into the game.
Also, the plot is still not very clear, so I'd better come up with something rather interesting.
Stay tuned for more updates!

<div class="content-read-marker" data-fraction="100"></div>
