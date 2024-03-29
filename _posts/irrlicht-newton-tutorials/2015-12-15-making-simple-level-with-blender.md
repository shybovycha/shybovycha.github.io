---
layout: post
title: "Irrlicht Newton GD tutorial: making simple level with Blender"
date: '2015-12-15T18:06:00+01:00'
---

In this section we will have short but powerful introduction to Blender. We will cover
just enough of model creation basics, you will need to create most of simple projects.

No, we will not cover animation, shaders or modificators here, but just enough minimum
to create this ramp floor for our tutorial:

<img src="/images/irrlicht-newton-tutorials/blender/ramp_render.webp" loading="lazy" alt="The desired result" class="img-responsive">

You will find lot of keyboard shortcuts here. And this is one of the most awesome
features of Blender - you can work without menus or panels! Everything you need
can be done with keyboard!

So let's dive in Blender now!

<!--more-->

## Welcome to Belnder

When you open Blender, you will see some pretty image, made with Belnder, version information,
some useful links and recent files

<img src="/images/irrlicht-newton-tutorials/blender/0.webp" loading="lazy" alt="Blender" class="img-responsive">

To close this window, simply click outside it. You will then see your workspace with the
`Default` window layout _(we will learn about them later)_. Workspace contains a few kickstarting
items:

* camera
* light
* cube

<img src="/images/irrlicht-newton-tutorials/blender/1.webp" loading="lazy" alt="Workspace" class="img-responsive">

You may be confused of the last one, but you will see shortly that so many cool things could
be done starting with plain cube and modifying it. Oh, and about modifying: let's switch to
the **Edit mode**, hitting the <kbd>Tab</kbd> key:

<img src="/images/irrlicht-newton-tutorials/blender/2.webp" loading="lazy" alt="Edit mode" class="img-responsive">

You can exit it hitting <kbd>Tab</kbd> again. In the **edit mode** you can manipulate mesh'
**edges**, **vertices** or **faces**. To switch between these, use three buttons on the screen's
bottom:

<img src="/images/irrlicht-newton-tutorials/blender/3.webp" loading="lazy" alt="Switching between edges, faces and vertices in edit mode" class="img-responsive">

Let's choose the **faces** editing mode. Unlike other 3D editors, in Blender selection is done
with the **Right mouse button**. Select one face of the cube:

<img src="/images/irrlicht-newton-tutorials/blender/4.webp" loading="lazy" alt="Selecting items in blender" class="img-responsive">

You may have noticed that the axis arrows have moved in the selected face's place.
These are used to manipulate selected elements. Also, they show the orientation of the selected
element. You can move the selected element by simply dragging one of the arrows. Selected
element will be moved along the selected axis only:

<img src="/images/irrlicht-newton-tutorials/blender/5.webp" loading="lazy" alt="Moving selected elements" class="img-responsive">

The same operation, movement, could be performed hitting the <kbd>G</kbd> key. You can move other
elements, too - this will change the form of our cube:

<img src="/images/irrlicht-newton-tutorials/blender/6.webp" loading="lazy" alt="Moving edges" class="img-responsive">

Now let's try something more complex. See the **Tools** panel on your left?

<img src="/images/irrlicht-newton-tutorials/blender/7.webp" loading="lazy" alt="Tools panel" class="img-responsive">

Select a face in a face editing mode and click **Extrude** (or hit the <kbd>E</kbd> key). Your face
will be extruded and you will be able to move it freely. But usually, designers move elements along
some axis - this makes models more accurate. To fix the movement axis, just hit its letter while being
in the extruding mode - <kbd>X</kbd>, <kbd>Y</kbd> or <kbd>Z</kbd>:

<img src="/images/irrlicht-newton-tutorials/blender/8.webp" loading="lazy" alt="Extruding faces" class="img-responsive">

Interesting fact: you may extrude both vertices and edges too.

Now, let's use even more advanced operation, which is oftenly described later in tutorials on
3D modelling. Choose the **Loop cut and slice** operation from the **Tools** panel - you will
see nothing. *Until* you move your cursor over your model. Depending on the edge, cursor is closer
to, you will see purple rectangle, looping through your model:

<img src="/images/irrlicht-newton-tutorials/blender/9.webp" loading="lazy" alt="Loop cut" class="img-responsive">

When you click the **Left mouse button**, you will move to the next part of this operation -
*slicing*. Just place the new edges where you want:

<img src="/images/irrlicht-newton-tutorials/blender/10.webp" loading="lazy" alt="Slicing the loop cut" class="img-responsive">

Now let's create walls for our "ramp". Create a few loop cuts alongside the ramp and we will start
extruding:

<img src="/images/irrlicht-newton-tutorials/blender/12.webp" loading="lazy" alt="Extruding one wall" class="img-responsive">

Or maybe just moving faces?..

<img src="/images/irrlicht-newton-tutorials/blender/13.webp" loading="lazy" alt="Moving vs extruding" class="img-responsive">

No, that's definitely not what we want! We want walls, not a new ramp! Hmmm... But if we will
extrude walls one-by-one, it will be inaccurate... Hold the <kbd>Shift</kbd> key and
**right-click** the two neighbour walls:

<img src="/images/irrlicht-newton-tutorials/blender/14.webp" loading="lazy" alt="Multiple selection" class="img-responsive">

Now we will work with three elements in the same way. Hit the <kbd>E</kbd> key and then - <kbd>Z</kbd>
and extrude all three walls at the same time up:

<img src="/images/irrlicht-newton-tutorials/blender/15.webp" loading="lazy" alt="Simultaneous extrusion" class="img-responsive">

Now we need two more walls to prevent our hero _(the ball, if you recall from the previous part)_
from falling aside. Select two edges at the corner of our ramp and hit the <kbd>W</kbd> key. You
will see the context menu like this:

<img src="/images/irrlicht-newton-tutorials/blender/16.webp" loading="lazy" alt="Editing context menu" class="img-responsive">

Click the **Subdivide** item, and the selected edges will be connected right in the middle:

<img src="/images/irrlicht-newton-tutorials/blender/17.webp" loading="lazy" alt="Subdivision for two edges" class="img-responsive">

You can perform that operation on faces - that is oftenly handy. Now, if you undo your changes with
usual <kbd>Ctrl</kbd>+<kbd>Z</kbd> (or <kbd>Command</kbd>+<kbd>Z</kbd> on Mac) and try to perform
the same operation on four opposite edges, you will see there is a redundant _(in our case)_
edge:

<img src="/images/irrlicht-newton-tutorials/blender/18.webp" loading="lazy" alt="4-th subdivision" class="img-responsive">

You can remove it by selecting that edge, hitting <kbd>X</kbd> and selecting **Dissolve edges**.
If you choose the **Delete edge** - you will loose the neighbour faces, which were made of that
edge.

<img src="/images/irrlicht-newton-tutorials/blender/19.webp" loading="lazy" alt="Delete or dissolve?" class="img-responsive">

So in the end we need to have two edges on the same line:

<img src="/images/irrlicht-newton-tutorials/blender/20.webp" loading="lazy" alt="The needed edges" class="img-responsive">

Now, switch to the **Ortho View**, choosing one from the **View** menu at the bottom of the
screen, or hitting the <kbd>Num 5</kbd> key:

<img src="/images/irrlicht-newton-tutorials/blender/21.webp" loading="lazy" alt="View menu" class="img-responsive">

Your workspace now should look different:

<img src="/images/irrlicht-newton-tutorials/blender/22.webp" loading="lazy" alt="Ortho view" class="img-responsive">

Using the **View** menu, you may switch between different views, perpendicular to your model.

<img src="/images/irrlicht-newton-tutorials/blender/23.webp" loading="lazy" alt="Top view" class="img-responsive">

<img src="/images/irrlicht-newton-tutorials/blender/24.webp" loading="lazy" alt="Right view" class="img-responsive">

Switching between different views will not clear the selection. And this is awesome!
So if you try to move the selected edges in the **Right Ortho View**, you will move
both of them:

<img src="/images/irrlicht-newton-tutorials/blender/25.webp" loading="lazy" alt="Selection persistence" class="img-responsive">

Yeeks... They move just along the Y axis, but not along the edge. But Blender easily handles
that - you need to switch between coordinate system using the corresponding menu
at the bottom of your screen:

<img src="/images/irrlicht-newton-tutorials/blender/26.webp" loading="lazy" alt="Coordinate system" class="img-responsive">

Use the **Normal** one and you will see the arrows at the selected edges changed:

<img src="/images/irrlicht-newton-tutorials/blender/27.webp" loading="lazy" alt="Normal coordinate system" class="img-responsive">

Now movement is done along the edge, just as we need:

<img src="/images/irrlicht-newton-tutorials/blender/28.webp" loading="lazy" alt="Moving with Normal coordinate system" class="img-responsive">

Try **moving** _(yes, moving, not extruding)_ our edges up - they will move along the normal:

<img src="/images/irrlicht-newton-tutorials/blender/30.webp" loading="lazy" alt="Moving edges in Right Ortho view" class="img-responsive">

But if you **click the mouse wheel** and **rotate camera**, or even if you switch to the
**Top Ortho** view, you will notice that our walls have different width:

<img src="/images/irrlicht-newton-tutorials/blender/31.webp" loading="lazy" alt="Whoops..." class="img-responsive">

So we need to make one wall thinner. But we should not forget about other edges - ones,
which will make another wall for us. Undoing now is not an option... We need to move the edges.
But if you move only those visible at the **Top Ortho** view, you will forget about the ones
at the bottom and screw the model. And selecting all those edges one-by-one is not an option
too...

<img src="/images/irrlicht-newton-tutorials/blender/32.webp" loading="lazy" alt="Selecting many edges manually is a pain..." class="img-responsive">

Moreover, we do not see those edges at the bottom! This is easy to fix, though: see the small
button with rectangles near the **vertex/edge/face** switcher?

<img src="/images/irrlicht-newton-tutorials/blender/33.webp" loading="lazy" alt="'Limit selection to visible' switcher" class="img-responsive">

Click it and you will be able to select bottom edges without the need to rotate the camera.
And now we will try the circle-selection tool, which will come to help you when you need to
select many elements at a time. Hit the <kbd>C</kbd> key and you will see the circle in a
workspace. Try dragging it (**left-click the mouse and drag**) over the edges we need:

<img src="/images/irrlicht-newton-tutorials/blender/35.webp" loading="lazy" alt="Circle selection" class="img-responsive">

Hmmm... It's way too much... Now, hold the <kbd>Shift</kbd> key and drag the circle over
the neighbour, redundant ones:

<img src="/images/irrlicht-newton-tutorials/blender/36.webp" loading="lazy" alt="Unselecting elements" class="img-responsive">

Now we can switch back to the **Top Ortho View** and successfully move our edges:

<img src="/images/irrlicht-newton-tutorials/blender/37.webp" loading="lazy" alt="Making walls thinner" class="img-responsive">

Now that we have our walls precisely set up, we can extrude the last two walls.
Select the **Normal coordinate system** and perform the extrusion along the Z axis:

<img src="/images/irrlicht-newton-tutorials/blender/40.webp" loading="lazy" alt="Extruding last two walls" class="img-responsive">

Now we will scale our model a few times. Staying in the **Edit mode**, select all the faces with
the <kbd>A</kbd> key:

<img src="/images/irrlicht-newton-tutorials/blender/42.webp" loading="lazy" alt="Selecting everything" class="img-responsive">

And hit the <kbd>S</kbd> key and start entering scale factor number. That's right, just press,
say, <kbd>5</kbd>:

<img src="/images/irrlicht-newton-tutorials/blender/41.webp" loading="lazy" alt="Entering factor while scaling" class="img-responsive">

You can correct what you entered using the <kbd>Backspace</kbd> key. You can do the same
thing while moving or rotating elements. This is useful when you need to
make operation really precise. But you still can use your mouse, of course.

**Hint:** if you scaled your model outside the **Edit mode**, you may find your scale, translation
or rotation different from identity values (`1, 1, 1` for scale or `0, 0, 0` for position/rotation).
This may cause different bugs while exporting models. To fix this, you need to select your
model in the **Object mode**, hit the <kbd>Ctrl</kbd>+<kbd>A</kbd> and select **Apply Scale**
_(or whatever you need to fix)_ from the pop-up menu.

<img src="/images/irrlicht-newton-tutorials/blender/44.webp" loading="lazy" alt="Applying scale" class="img-responsive">

## Texturing our model

Now we need to paint our model to have something more beautiful in our application than just
pitch black... stuff...

Adding textures to a model in Blender is extremely easy - you just select your model, switch to
the **Texture** tab on the right panel and click **New** button:

<img src="/images/irrlicht-newton-tutorials/blender/47.webp" loading="lazy" alt="Creating a texture" class="img-responsive">

Then you pass in some params like texture size, the background color and image name - and you are done!

<img src="/images/irrlicht-newton-tutorials/blender/48.webp" loading="lazy" alt="New texture params" class="img-responsive">

But that will only add a **blank** texture. And then you will need to paint it as you wish.
But painting a texture requires your model to have vertices, synchronized with your texture.
So each vertex will know where it lays both in 3D space and on the texture image. This
assignment process is called **Texture unwrapping** or **UV mapping** _(because texture
coordinates are usually called `u` and `v` instead of `x` and `y`, since those are already
involved to describe vertex' position)_. And this process requires one thing from you: you
need to specify, where Blender should "cut" your model. This is quite simple task, but this
will result on how the texture will look like and how easy it will be to paint.

So, go to the **Edit mode** and select a few loops of edges:

<img src="/images/irrlicht-newton-tutorials/blender/50.webp" loading="lazy" alt="Selecting seam edges" class="img-responsive">

<img src="/images/irrlicht-newton-tutorials/blender/51.webp" loading="lazy" alt="Selecting seam edges" class="img-responsive">

Now, on the left panel, switch to the **Shading/UVs** tab and click the **Mark Seam** button:

<img src="/images/irrlicht-newton-tutorials/blender/49.webp" loading="lazy" alt="Shading/UVs tab" class="img-responsive">

This will mark the selected edges as seams to "cut" your model along. Have no fear, your model
will not be actually cut - it will be used for maths only.

Then, on the same panel click the **Unwrap** button and select the first unwrapping method on
the list:

<img src="/images/irrlicht-newton-tutorials/blender/53.webp" loading="lazy" alt="Unwrapping method" class="img-responsive">

Again, no effect you will see now. To see something, switch the window layout at the top menu to
**UV Editing**:

<img src="/images/irrlicht-newton-tutorials/blender/54.webp" loading="lazy" alt="Layout switcher" class="img-responsive">

<img src="/images/irrlicht-newton-tutorials/blender/55.webp" loading="lazy" alt="Layouts available" class="img-responsive">

You will see two windows - on your left there will be **UV/image editor** and on your right
there will be the **3D view**. And again, nothing interesting here... But I am not fooling
with you - it's only how Blender works... To see something marvelous, select everything on
the **3D view**:

<img src="/images/irrlicht-newton-tutorials/blender/58.webp" loading="lazy" alt="UV-Mapped model" class="img-responsive">

You will see some lines on your left. That's what you have selected, mapped onto image plane.
But there is no actual image in the **UV/Image editor** for now. To add one, just click the
**New** button on the bottom menu of the **UV/Image editor** or select an existing one:

<img src="/images/irrlicht-newton-tutorials/blender/60.webp" loading="lazy" alt="Selecting background image for UV mapping" class="img-responsive">

This will not change the image itself. The image will be the background for our image editor
window, nothing more. To start making miracles, go to the **Texture Paint mode** in the
**3D view**:

<img src="/images/irrlicht-newton-tutorials/blender/61.webp" loading="lazy" alt="Texture Paint mode" class="img-responsive">

And your model will change its look...

<img src="/images/irrlicht-newton-tutorials/blender/62.webp" loading="lazy" alt="Pinky!" class="img-responsive">

What is this pink monster?! Well, on the left panel of our **3D View** there's a message,
saying the texture slot is missing and proposing to create one... Let's do this...

<img src="/images/irrlicht-newton-tutorials/blender/63.webp" loading="lazy" alt="Texture slot creation" class="img-responsive">

Now we are able to paint our model! See, how awesome it is: you have a brush tool activated.
Brush has three params:

1. **Color** - this could be changed with the color circle below
2. **Radius**
3. **Pressure**, or **Alpha**

Radius could be changed by pressing the <kbd>F</kbd> key and moving mouse cursor:

<img src="/images/irrlicht-newton-tutorials/blender/64.webp" loading="lazy" alt="Brush radius changing" class="img-responsive">

Pressure could be changed by pressing <kbd>Shift</kbd>+<kbd>F</kbd> and doing the same:

<img src="/images/irrlicht-newton-tutorials/blender/65.webp" loading="lazy" alt="Brush pressure changing" class="img-responsive">

And you can just pain like in... Microsoft Paint!

<img src="/images/irrlicht-newton-tutorials/blender/66.webp" loading="lazy" alt="Just paint!!!" class="img-responsive">

But if you look into the **UV/Image editor**, you will see... nothing! Again! 'the hell?!

<img src="/images/irrlicht-newton-tutorials/blender/67.webp" loading="lazy" alt="WTF?!" class="img-responsive">

That is just misunderstanging - you were painting on the other image instead of the selected one:

<img src="/images/irrlicht-newton-tutorials/blender/68.webp" loading="lazy" alt="Choosing image for UV/Image editor" class="img-responsive">

We created a new one, when created a texture slot...

To start drawing in the **UV/Image editor** instead of **3D View**, you just need to switch
its mode to **Paint** at the bottom menu:

<img src="/images/irrlicht-newton-tutorials/blender/71.webp" loading="lazy" alt="Painting in the UV/Image editor" class="img-responsive">

Okay, so far so good. We are able to paint our model. But there's one interesting thing: if
you try to draw a straight line - you may face situation, when line is straight in the image
but is curved on the model:

<img src="/images/irrlicht-newton-tutorials/blender/72.webp" loading="lazy" alt="UV mapping mistakes" class="img-responsive">

<img src="/images/irrlicht-newton-tutorials/blender/74.webp" loading="lazy" alt="UV mapping mistakes" class="img-responsive">

But that's happening not everywhere - only on certain faces/edges:

<img src="/images/irrlicht-newton-tutorials/blender/75.webp" loading="lazy" alt="Mistakes are only on certain faces" class="img-responsive">

Well, that's because of UV mapping is not precise enough. If you switch to the **View mode**
in the **UV/Image editor** and to the **Edit mode** in the **3D View**, and select all the model,
you will see the points in the image editor, you may drag:

<img src="/images/irrlicht-newton-tutorials/blender/76.webp" loading="lazy" alt="Control points in the image editor" class="img-responsive">

Try selecting them with **Right mouse button** and moving them with <kbd>G</kbd>:

<img src="/images/irrlicht-newton-tutorials/blender/77.webp" loading="lazy" alt="Selecting control points" class="img-responsive">

<img src="/images/irrlicht-newton-tutorials/blender/78.webp" loading="lazy" alt="Moving control points" class="img-responsive">

Yes, now texture looks creepy, but lines are almost straight:

<img src="/images/irrlicht-newton-tutorials/blender/79.webp" loading="lazy" alt="Fixing UV mapping errors manually" class="img-responsive">

<img src="/images/irrlicht-newton-tutorials/blender/80.webp" loading="lazy" alt="Fixing UV mapping errors manually" class="img-responsive">

## Exporting our model

When you finish painting your texture, the last thing we need to do is to export our model
to the format, understandable by Irrlicht. For good, both Blender and Irrlicht support
many different formats:

<img src="/images/irrlicht-newton-tutorials/blender/83.webp" loading="lazy" alt="Blender exporting" class="img-responsive">

Blender's file dialogs look differently, but have very intuitive interface:

<img src="/images/irrlicht-newton-tutorials/blender/90.webp" loading="lazy" alt="Blender file dialog" class="img-responsive">

If you do not see the needed format in Blender - you just need to turn on a corresponding plugin:

<img src="/images/irrlicht-newton-tutorials/blender/95.webp" loading="lazy" alt="Blender settings menu" class="img-responsive">

<img src="/images/irrlicht-newton-tutorials/blender/96.webp" loading="lazy" alt="Blender settings menu" class="img-responsive">

After exporting our model to, say, **3DS** format, take a look at the directory you have exported
your model to:

<img src="/images/irrlicht-newton-tutorials/blender/88.webp" loading="lazy" alt="No textures!" class="img-responsive">

Where are the textures? Relax, they are in the **UV/Image editor**, yet unsaved. You can
save the modified image with the **Image -> Save** menu at the bottom of **UV/Image Editor**:

<img src="/images/irrlicht-newton-tutorials/blender/89.webp" loading="lazy" alt="Saving image from UV/Image Editor" class="img-responsive">

Now we have everything we need for our Newtonian sample!

<a href="/irrlicht-newton-tutorials/2015/12/16/finishing-the-first-scene.html" class="btn btn-success">Next chapter</a>
