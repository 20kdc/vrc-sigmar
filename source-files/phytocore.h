#include <kip32.h>

enum {
	ATOM_NONE = 0,
	ATOM_E0_FIRE = 1,
	ATOM_E1_EARTH = 2,
	ATOM_E2_AIR = 3,
	ATOM_E3_WATER = 4,
	ATOM_SALT = 5,
	ATOM_QUICKSILVER = 6,
	ATOM_VITAE = 7,
	ATOM_MORS = 8,
	ATOM_M0 = 9,
	ATOM_M1 = 10,
	ATOM_M2 = 11,
	ATOM_M3 = 12,
	ATOM_M4 = 13,
	ATOM_M5 = 14,
	ATOM_QUINTESSENCE = 15,
	ATOM_TYPE_MASK = 15,
	// Multiple atoms can be selected during Quintessence's states, and we also want to keep sync very simple.
	// So, we represent this using flags.
	ATOM_FLAG_SELECTED = 256,
	ATOM_FLAG_CALC_DIMMED = 512,
};
#define ATOM_TYPE(atom) ((atom) & ATOM_TYPE_MASK)
#define ATOM_TYPE_IS_UNLOCKABLE_METAL(atom_type) (((atom_type) >= ATOM_M1) && ((atom_type) <= ATOM_M5))

// Returns 1 if the given marble index is surrounded.
// This is 'immediate' and doesn't require a calcflags update.
int phyto_surrounded(int marble);

// Selection state tracker.
extern int phyto_calc_qstate;
#define PHYTO_QSTATE_FLAG(atype) (1 << (atype))
#define PHYTO_QSTATE_COMPLETE (PHYTO_QSTATE_FLAG(ATOM_QUINTESSENCE) | PHYTO_QSTATE_FLAG(ATOM_E0_FIRE) | PHYTO_QSTATE_FLAG(ATOM_E1_EARTH) | PHYTO_QSTATE_FLAG(ATOM_E2_AIR) | PHYTO_QSTATE_FLAG(ATOM_E3_WATER))

// Updates calculated flags on the board.
// Run after *any* board change!
void phyto_update_calc();

// This function reports what selecting the given marble would do.
// Checks the index is valid, also.
// It does not make any changes to the board.
// Make sure calcflags are up to date before you run this.
int phyto_select_marble_response(int marble);

// Same idea as phyto_select_marble_response, but this is the guts of the logic.
// Calcflags do not need to be up to date (the *passed in* qstate obviously does need to be correct)
// This function does not explicitly know which metals are unlocked, but figures it out via ATOM_FLAG_CALC_DIMMED.
int phyto_qstate_update_response(int qstate, int atom);

// -- Udon Interface --

// Clears the board. "Effectively" recalculates.
KIP32_EXPORT void _ClearBoard();

// Gets marble data for the given marble.
KIP32_EXPORT int _GetMarble(int marble);

// Sets marble data for the given marble.
// This does NOT recalculate.
KIP32_EXPORT void _SetMarble(int marble, int value);

KIP32_EXPORT void _Recalculate();

// Implements the actual process of selecting a marble and processing the response.
// Called externally, so recalculates before and after just to be safe.
KIP32_EXPORT void _SelectMarble(int marble);

enum {
	// Not allowed.
	PHYTO_SELECT_MARBLE_RESPONSE_FAIL = 0,
	// All selected marbles are deleted and the board is re-evaluated.
	PHYTO_SELECT_MARBLE_RESPONSE_DELETE_ALL = 1,
	// The marble is added to the selection.
	PHYTO_SELECT_MARBLE_RESPONSE_SELECT = 2,
	// The selection is cleared.
	PHYTO_SELECT_MARBLE_RESPONSE_CLEAR_SELECTION = 3,
};
