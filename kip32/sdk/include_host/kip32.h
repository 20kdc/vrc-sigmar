/* KIP-32 semihosting header */

#pragma once

#ifdef WINDOWS
#define KIP32_EXPORT __attribute__((dllexport))
#else
#define KIP32_EXPORT __attribute__((visibility("default")))
#endif

/* */
void kip32_syscall8(int * c0, int * c1, int * c2, int * c3, int * c4, int * c5, int * c6, int * c7);

#define KIP32_SYSCALL_DUMMIES int a0 = 0xF00FF00F, a1 = 0xF00FF00F, a2 = 0xF00FF00F, a3 = 0xF00FF00F, a4 = 0xF00FF00F, a5 = 0xF00FF00F, a6 = 0xF00FF00F, a7 = 0xF00FF00F;

static inline void kip32_syscall0() {
    KIP32_SYSCALL_DUMMIES
    kip32_syscall8(&a0, &a1, &a2, &a3, &a4, &a5, &a6, &a7);
}

static inline void kip32_syscall1(int * c0) {
    KIP32_SYSCALL_DUMMIES
    kip32_syscall8(c0, &a1, &a2, &a3, &a4, &a5, &a6, &a7);
}

static inline void kip32_syscall2(int * c0, int * c1) {
    KIP32_SYSCALL_DUMMIES
    kip32_syscall8(c0, c1, &a2, &a3, &a4, &a5, &a6, &a7);
}

static inline void kip32_syscall3(int * c0, int * c1, int * c2) {
    KIP32_SYSCALL_DUMMIES
    kip32_syscall8(c0, c1, c2, &a3, &a4, &a5, &a6, &a7);
}

static inline void kip32_syscall4(int * c0, int * c1, int * c2, int * c3) {
    KIP32_SYSCALL_DUMMIES
    kip32_syscall8(c0, c1, c2, c3, &a4, &a5, &a6, &a7);
}

static inline void kip32_syscall5(int * c0, int * c1, int * c2, int * c3, int * c4) {
    KIP32_SYSCALL_DUMMIES
    kip32_syscall8(c0, c1, c2, c3, c4, &a5, &a6, &a7);
}

static inline void kip32_syscall6(int * c0, int * c1, int * c2, int * c3, int * c4, int * c5) {
    KIP32_SYSCALL_DUMMIES
    kip32_syscall8(c0, c1, c2, c3, c4, c5, &a6, &a7);
}

static inline void kip32_syscall7(int * c0, int * c1, int * c2, int * c3, int * c4, int * c5, int * c6) {
    KIP32_SYSCALL_DUMMIES
    kip32_syscall8(c0, c1, c2, c3, c4, c5, c6, &a7);
}
