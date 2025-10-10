# vrc-sigmar

An implementation of Sigmar's Garden (from [Zachtronics's Opus Magnum](https://store.steampowered.com/app/558990/Opus_Magnum/)) as an asset for VRChat worlds.

(A proper demo world for this asset is pending a lot of work that may not happen.)

...Ok, so, this project wasn't intended to take as long as it did and be as complicated as it is. This was intended to be a silly weekend project. Then, it wasn't.

Among other things, it accidentally dug up a field of Unity and VRChat SDK bugs and summoned a spaghetti graph code monster.

As it is, I think I've managed to create a reasonably self-contained game asset you can just drop into a world. Sync appears to work though there's probably some way to break it. Good enough. Don't do that.

I will say, as a postmortem on this project: This made me feel like I am simultaneously fighting a bloated, buggy, memory-hog of an engine, a messy and painful sandboxing system on top of it, and _on top of that,_ three languages broken in different but equally crippling ways (broken constants, 'actually Unity's fault because domain reloading crash', broken compiler and literally can't write `|` in strings without breaking things)

The only reason I touch this engine is because it's the only way to make stuff for one of the unfortunately few platforms that care _slightly more_ about reasonable hardware.

## Licensing etc.

Firstly, there is obviously the matter of the rules being implemented.

However, copyrighting the _mechanics_ of a game isn't _exactly_ a thing, except when it's a video game _and_ patents are involved. I do not believe this is the case here.

Secondly, there's the board database, sourced by having <https://bits.ondrovo.com/sigmar/> generate 2048 different boards. (It's all client-side, don't worry.)

I'm assuming that there are no issues with this due to it being kind of in the realm of 'particular random numbers'.

Thirdly, there's the original work in this repository.

All visual assets for this implementation were either custom-designed/rendered or are derived from fonts (falling under the classification of 'documents').

Where possible, the contents of this repository are released as so:

```
This is free and unencumbered software released into the public domain.

Anyone is free to copy, modify, publish, use, compile, sell, or
distribute this software, either in source code form or as a compiled
binary, for any purpose, commercial or non-commercial, and by any
means.

In jurisdictions that recognize copyright laws, the author or authors
of this software dedicate any and all copyright interest in the
software to the public domain. We make this dedication for the benefit
of the public at large and to the detriment of our heirs and
successors. We intend this dedication to be an overt act of
relinquishment in perpetuity of all present and future rights to this
software under copyright law.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.
IN NO EVENT SHALL THE AUTHORS BE LIABLE FOR ANY CLAIM, DAMAGES OR
OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
OTHER DEALINGS IN THE SOFTWARE.

For more information, please refer to <http://unlicense.org>
```

Regardless of this notice, it is likely for the best that copies of this repository continue to acknowledge that:

1. None of Zachtronics, MightyPork, or VRChat have endorsed this implementation
2. The board database was generated using MightyPork's work at <https://bits.ondrovo.com/sigmar/>
