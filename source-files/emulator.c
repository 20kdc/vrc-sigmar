#include <stdio.h>
#include <string.h>

#undef PHYTOLAB
#define PHYTOLAB
#include "phytocore.h"
#include "board.h"

static const char atomtypechars[] = "_feawsqvm012345Q";

// -- Test Board --

#define _ ATOM_NONE,
#define f ATOM_E0_FIRE,
#define e ATOM_E1_EARTH,
#define a ATOM_E2_AIR,
#define w ATOM_E3_WATER,
#define s ATOM_SALT,
#define q ATOM_QUICKSILVER,
#define v ATOM_VITAE,
#define m ATOM_MORS,
#define A ATOM_M0,
#define B ATOM_M1,
#define C ATOM_M2,
#define D ATOM_M3,
#define E ATOM_M4,
#define F ATOM_M5,
#define Q ATOM_QUINTESSENCE,

static int boardinit[MARBLE_COUNT] = {
     q v a s w f
    _ e _ a s _ _
   _ B _ _ a _ _ _
  _ q C _ q E s f _
 _ _ e v e _ a w f _
a _ _ v _ F e _ _ w e
 w e m e f _ f _ _ q
  e A _ _ q v m f s
   w _ _ a D _ m f
    w m f a _ _ w
     a _ _ _ _ w
};
// Quintessence test board
static int boardQ[MARBLE_COUNT] = {
     _ f _ e _ a
    _ _ _ _ _ _ _
   w _ s _ q _ v _
  _ _ _ _ _ _ _ _ _
 m _ A _ B _ C _ D _
_ _ _ _ _ _ _ _ _ _ _
 E _ F _ Q _ _ _ _ _
  _ _ _ _ _ _ _ _ _
   f _ e _ a _ w _
    _ _ _ _ _ _ _
     _ _ _ _ _ _
};

#undef _
#undef f
#undef e
#undef a
#undef w
#undef s
#undef q
#undef v
#undef m
#undef A
#undef B
#undef C
#undef D
#undef E
#undef F
#undef Q

static int cursor = 0;

static void display_board() {
	// display board
	printf("\x1b[2J");
	printf("\x1b[0m"); // reset draw mode
	int marbleIdx = 0;
	const char * magic = board_layout_str;
	int skipNext = 0;
	while (*magic) {
		if (*magic == '*') {
			int state = game_state[marbleIdx++];
			if (state & ATOM_FLAG_SELECTED)
				printf("\x1b[4m");
			if (!(state & ATOM_FLAG_CALC_DIMMED))
				printf("\x1b[1m");
			putchar(atomtypechars[ATOM_TYPE(state)]);
			printf("\x1b[0m");
			if ((marbleIdx - 1) == cursor) {
				putchar('<');
				skipNext = 1;
			}
		} else {
			// newlines don't count since they don't disturb the existing content
			if ((*magic) == 10)
				skipNext = 0;
			if (!skipNext)
				putchar(*magic);
			skipNext = 0;
		}
		magic++;
	}
}

static void travel(int dir) {
	int res = board_layout[cursor].n[dir];
	if (res != -1)
		cursor = res;
}

int main() {
	// reinit the board
	memcpy(game_state, boardinit, MARBLE_COUNT * sizeof(int));
	Recalculate();
	while (1) {
		display_board();
		int chr = getchar();
		// navigation
		if (chr == 'q')
			travel(DIR_NW);
		if (chr == 'w') {
			travel(DIR_NE);
			travel(DIR_NW);
		}
		if (chr == 'x') {
			travel(DIR_SE);
			travel(DIR_SW);
		}
		if (chr == 'e')
			travel(DIR_NE);
		if (chr == 'd')
			travel(DIR_E);
		if (chr == 'c')
			travel(DIR_SE);
		if (chr == 'z')
			travel(DIR_SW);
		if (chr == 'a')
			travel(DIR_W);
		// gameplay
		if (chr == 's')
			SelectMarble(cursor);
		if (chr == '9') {
			memcpy(game_state, boardinit, MARBLE_COUNT * sizeof(int));
			Recalculate();
		}
		if (chr == '0') {
			memcpy(game_state, boardQ, MARBLE_COUNT * sizeof(int));
			Recalculate();
		}
	}
	return 0;
}
