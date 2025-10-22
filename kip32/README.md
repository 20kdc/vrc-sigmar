# KIP32: RV32I to Udon transpiler

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
* ISA has good tooling support even in minimal configurations (soft-float, etc.)

## How does it work from the behaviour programmer's view?

1. Continue to write code that interacts with Unity in Udon Graph or UdonSharp.
2. Write _complex_ code in C/Rust/etc.
	* Write this code as if you're writing it for a microcontroller you don't happen to have on your desk. \
	  In other words, you should have a clear method of testing as a native executable.
3. Compile to what is essentially a RV32I microcontroller. Link with `sdk/kip32.ld` linker script.
	* Symbols starting with `Udon` are exported as custom events (or regular events) with the prefix removed.
		* The expected workflow for other behaviours is to set the `a0`-`a7` registers, execute a custom event, and retrieve results.
		* Custom events called `_sym_` are created for any reasonably valid symbol. These events return the address of the symbol, for easy DMA.
	* Other behaviours may directly access microcontroller memory, but there is no way to 'map' microcontroller memory to other devices.
	* Microcontroller memory isn't synced; do your sync efficiently in another behaviour and then copy into VM memory on deserialization.
	* No code is executed on behaviour start. The microcontroller is automatically initialized _on first use._
	* If you want to make your own linker script (or linker):
		* The transpiler expects an ELF file with _section headers_ (it will ignore program headers).
		* Section names are arbitrary.
		* The symbol table is used for various tasks.
		* Relocations are completely ignored, so if you're relying on them you're going to have a bad time.
		* For efficiency reasons, executable sections should should start at 0 and end as early as possible to minimize the size of the indirect jump table.
4. Tighter integration may be achieved using various flags, particularly `--inc` and `--ecall` ; see transpiler help for details.
	* `sdk/log_syscall.uasm` is a simple example.
5. Udon Assembly doesn't play as well as it could with import on the no-auto-import configuration.
	* For this reason, you may have to manually delete the SerializedUdonProgram file to get it to recompile.

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
