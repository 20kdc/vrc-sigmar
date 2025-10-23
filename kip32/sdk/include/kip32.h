/* KIP-32 SDK header */

#pragma once

/* Exported symbols are put into a specific section. */
#define KIP32_EXPORT __attribute__((section(".kip32_export")))

/* We're subtly trying to tell the compiler to remove these memory accesses. */
#define KIP32_SYSCALL_CORE(name, e0, v0, e1, v1, e2, v2, e3, v3, e4, v4, e5, v5, e6, v6, e7, v7) \
static inline __attribute__((always_inline)) void name { \
	register int a0 __asm__("a0") e0 v0; \
	register int a1 __asm__("a1") e1 v1; \
	register int a2 __asm__("a2") e2 v2; \
	register int a3 __asm__("a3") e3 v3; \
	register int a4 __asm__("a4") e4 v4; \
	register int a5 __asm__("a5") e5 v5; \
	register int a6 __asm__("a6") e6 v6; \
	register int a7 __asm__("a7") e7 v7; \
	__asm__ volatile ( \
		"ecall" \
		: "=r" (a0), "=r" (a1), "=r" (a2), "=r" (a3), "=r" (a4), "=r" (a5), "=r" (a6), "=r" (a7) \
		: "r" (a0), "r" (a1), "r" (a2), "r" (a3), "r" (a4), "r" (a5), "r" (a6), "r" (a7) \
		: \
	); \
	v0 e0 a0; \
	v1 e1 a1; \
	v2 e2 a2; \
	v3 e3 a3; \
	v4 e4 a4; \
	v5 e5 a5; \
	v6 e6 a6; \
	v7 e7 a7; \
}

KIP32_SYSCALL_CORE(kip32_syscall0(),,,,,,,,,,,,,,,,);
KIP32_SYSCALL_CORE(kip32_syscall1(int * c0), =, *c0,,,,,,,,,,,,,,);
KIP32_SYSCALL_CORE(kip32_syscall2(int * c0, int * c1), =, *c0, =, *c1,,,,,,,,,,,,);
KIP32_SYSCALL_CORE(kip32_syscall3(int * c0, int * c1, int * c2), =, *c0, =, *c1, =, *c2,,,,,,,,,,);
KIP32_SYSCALL_CORE(kip32_syscall4(int * c0, int * c1, int * c2, int * c3), =, *c0, =, *c1, =, *c2, =, *c3,,,,,,,,);
KIP32_SYSCALL_CORE(kip32_syscall5(int * c0, int * c1, int * c2, int * c3, int * c4), =, *c0, =, *c1, =, *c2, =, *c3, =, *c4,,,,,,);
KIP32_SYSCALL_CORE(kip32_syscall6(int * c0, int * c1, int * c2, int * c3, int * c4, int * c5), =, *c0, =, *c1, =, *c2, =, *c3, =, *c4, =, *c5,,,,);
KIP32_SYSCALL_CORE(kip32_syscall7(int * c0, int * c1, int * c2, int * c3, int * c4, int * c5, int * c6), =, *c0, =, *c1, =, *c2, =, *c3, =, *c4, =, *c5, =, *c6,,);
KIP32_SYSCALL_CORE(kip32_syscall8(int * c0, int * c1, int * c2, int * c3, int * c4, int * c5, int * c6, int * c7), =, *c0, =, *c1, =, *c2, =, *c3, =, *c4, =, *c5, =, *c6, =, *c7);
