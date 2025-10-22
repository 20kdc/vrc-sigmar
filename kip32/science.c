void putchar(int c) {
	register int cx __asm__("a0") = c;
	__asm__ volatile (
		"ecall"
		: "=r" (cx)
		: "r" (cx)
		:
	);
}

void puts(const char * s) {
	while (*s)
		putchar(*(s++));
	putchar(10);
}

int tmp = 0;

int Udon_interact() {
	puts("Hello, Udon World!");
	tmp++;
	return tmp * tmp;
}

int Udonincrement() {
	return ++tmp;
}
int Udondecrement() {
	return --tmp;
}

// SLT checks. Test with a0 = ? and a1 = -1 for best effect.
int UdonSLT(int a, int b) {
	return a < b;
}
int UdonSLTU(unsigned int a, unsigned int b) {
	return a < b;
}

// memory tests
char arr_s8[] = "Temporary";
short arr_s16[] = {0, 1, 2, 3};
void Udonwrite_s8() {
	arr_s8[tmp] = 0x80;
}
int Udonread_s8() {
	// So here's a fun one; char seems to be unsigned on this compiler.
	// Not a bug, though, since specifying the type better compiles LB as expected.
	// return arr_s8[tmp];
	return ((signed char *) arr_s8)[tmp];
}
int Udonread_u8() {
	return ((unsigned char *) arr_s8)[tmp];
}
void Udonwrite_s16() {
	arr_s16[tmp] = 0x8234;
}
int Udonread_s16() {
	return arr_s16[tmp];
}
int Udonread_u16() {
	return ((unsigned short * ) arr_s16)[tmp];
}
