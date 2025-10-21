# 'Science32' (name massively subject to change): RV32I to Udon transpiler

Ok, so, basically, I remembered something I heard about static recompilation.

And then I realized, most of the issues don't apply if the user trusts their own code not to do JIT or anything weird!

**Therefore, this project statically recompiles RV32I instructions into Udon Assembly.**

## Why would you want to do that?

Writing complex logic in C/Rust/etc. targetting VRChat.

## Why not use WebAssembly/LLVM bitcode/etc.?

All of these options make various mechanisms _the implementor's problem._

As the complexity of this project shows, the implementor really can't afford to have any _more_ complexity if they want to get whatever their actual goal is completed in a timely manner.

Exception handling, for instance, would be a total mess. It's better implemented in-VM.

Firmly not-helping is that the WebAssembly 3.0 core spec includes things like _vector types._ This is not viable.

Meanwhile, RV32I has a clear minimal set of instructions a compiler can be told to target. Is it optimal? No, but it also avoids some nasty pitfalls.

* Large register count is good for performance of resulting Udon code.
	* Since the compiler is managing spilling and saving registers in as optimal a way as possible, we don't have to try doing it ourselves (but worse).

## Future extensions?

* Testing the _exact_ details of multiplication semantics is a potential nightmare, and as System.Convert has decided to be troublesome, some very awkward questions arise.
	* This'll probably need to be handled eventually for performance reasons, but any time something that might have a sign bit has to be coerced into Int32 is painful, and a lot of those sorts of instructions lurk in the multiplication extension.
* Floating-point support would be nice, and may be added in future, but spec compliance is guaranteed to fail.
	* Instruction RM flag will be ignored, and `fcsr` simply won't exist. However, the performance boost will be worth it for some applications, and the compiler can be told not to use it.

## Why Udon Assembly, despite its issues re: constants?

The issues with constants don't actually severely interfere with RISC-V dynamic recompilation because we don't need constants of the types affected by those issues.

In fact, as it turns out, there are so many flaws in Udon's handling of numeric types that you basically should use as high-precision a type as you dare _whenever possible,_ just so you don't have to AND off bits, just so that `System.Convert` won't come knocking with an exception.

RV32I only performs type conversions during loads and stores. It is no coincidence that the load/store code is the most painful part of the recompiler.

For the purposes of this project, the benefits of not having to deal with the Domain Reloading crash (which is part of why this subproject was even started) outweigh the loss from using Udon Assembly.

## Notes

Udon Assembly doesn't play as well as it could with import on the no-auto-import configuration. For this reason, you may have to manually delete the SerializedUdonProgram file to get it to recompile.

CURRENT BIG SCARY BUG: **Type signedness messes everything up because System.Convert is really, really picky.**
