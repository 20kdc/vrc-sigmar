#include <kip32.h>

void putchar(int c) {
	kip32_syscall1(&c);
}

void puts(const char * s) {
	while (*s)
		putchar(*(s++));
	putchar(10);
}

int tmp = 0;

KIP32_EXPORT int _interact() {
	puts("Hello, Udon World!");
	tmp++;
	return tmp * tmp;
}

KIP32_EXPORT int increment() {
	return ++tmp;
}
KIP32_EXPORT int decrement() {
	return --tmp;
}

// SLT checks. Test with a0 = ? and a1 = -1 for best effect.
KIP32_EXPORT int SLT(int a, int b) {
	return a < b;
}
KIP32_EXPORT int SLTU(unsigned int a, unsigned int b) {
	return a < b;
}

// memory tests
char arr_s8[] = "Temporary";
short arr_s16[] = {0, 1, 2, 3};
KIP32_EXPORT void write_s8() {
	arr_s8[tmp] = 0x80;
}
KIP32_EXPORT int read_s8() {
	// So here's a fun one; char seems to be unsigned on this compiler.
	// Not a bug, though, since specifying the type better compiles LB as expected.
	// return arr_s8[tmp];
	return ((signed char *) arr_s8)[tmp];
}
KIP32_EXPORT int read_u8() {
	return ((unsigned char *) arr_s8)[tmp];
}
KIP32_EXPORT void write_s16() {
	arr_s16[tmp] = 0x8234;
}
KIP32_EXPORT int read_s16() {
	return arr_s16[tmp];
}
KIP32_EXPORT int read_u16() {
	return ((unsigned short * ) arr_s16)[tmp];
}
