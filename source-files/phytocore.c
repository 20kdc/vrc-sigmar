/* 'Phytocore' */
#define PHYTO_IMPL
#include "board.h"
#include "phytocore.h"

int phyto_surrounded(int marble) {
	int set = 0;
	// Go over each neighbour position and mark 1 where NOT empty.
	// This turns bit masks into a "are any of these occupied" check.
	// In turn, logical NOT gives what we want (are all of these free).
	for (int i = 0; i < 6; i++) {
		int nid = board_layout[marble].n[i];
		if ((nid != -1) && (game_state[nid] != ATOM_NONE)) {
			set |= 1 << i;
		}
	}
	if (!(set & 0b111000))
		return 0;
	if (!(set & 0b011100))
		return 0;
	if (!(set & 0b001110))
		return 0;
	if (!(set & 0b000111))
		return 0;
	if (!(set & 0b100011))
		return 0;
	if (!(set & 0b110001))
		return 0;
	return 1;
}

int phyto_calc_qstate;

void phyto_update_calc() {
	phyto_calc_qstate = 0;
	// used for metal calculations
	int istate = 0;
	for (int i = 0; i < MARBLE_COUNT; i++) {
		int atom = game_state[i];
		int atom_type = ATOM_TYPE(atom);
		istate |= PHYTO_QSTATE_FLAG(atom_type);
	}
	for (int i = 0; i < MARBLE_COUNT; i++) {
		int atom = game_state[i];
		int atom_type = ATOM_TYPE(atom);
		if (atom & ATOM_FLAG_SELECTED)
			phyto_calc_qstate |= PHYTO_QSTATE_FLAG(atom_type);

		// Empty slots are considered dimmed so we don't have to be explicit about not touching them.
		int dimmed = atom_type == ATOM_NONE;

		// Otherwise, check if it's surrounded.
		if (!dimmed)
			dimmed = phyto_surrounded(i);

		// If not already dimmed, and an unlockable metal, if the previous metal exists, dim.
		if (!dimmed)
			if (ATOM_TYPE_IS_UNLOCKABLE_METAL(atom_type))
				if (istate & PHYTO_QSTATE_FLAG(atom_type - 1))
					dimmed = 1;
		if (dimmed) {
			atom |= ATOM_FLAG_CALC_DIMMED;
		} else {
			atom &= ~ATOM_FLAG_CALC_DIMMED;
		}
		game_state[i] = atom;
	}
}

int phyto_select_marble_response(int marble) {
	if ((marble < 0) || (marble >= MARBLE_COUNT))
		return PHYTO_SELECT_MARBLE_RESPONSE_FAIL;
	return phyto_qstate_update_response(phyto_calc_qstate, game_state[marble]);
}

int phyto_qstate_update_response(int qstate, int atom) {
	int atom_type = ATOM_TYPE(atom);

	// Empty slots cannot be selected.
	if (atom_type == ATOM_NONE)
		return PHYTO_SELECT_MARBLE_RESPONSE_FAIL;

	// The atom is dimmed (non-interactible).
	if (atom & ATOM_FLAG_CALC_DIMMED)
		return PHYTO_SELECT_MARBLE_RESPONSE_FAIL;

	// The atom is already selected.
	if (atom & ATOM_FLAG_SELECTED)
		return PHYTO_SELECT_MARBLE_RESPONSE_CLEAR_SELECTION;

	// The atom isn't air or dimmed. That means it can be selected from a blank state by default.
	if (qstate == 0) {
		// Gold deletes by itself.
		if (atom_type == ATOM_M5)
			return PHYTO_SELECT_MARBLE_RESPONSE_DELETE_ALL;
		return PHYTO_SELECT_MARBLE_RESPONSE_SELECT;
	}

	// This is the exact list of atoms that match with themselves.
	// If our qstate solely contains one of these atoms, and we are selecting another atom, they match.
	const static int atoms_that_match_with_themselves[5] = {ATOM_E0_FIRE, ATOM_E1_EARTH, ATOM_E2_AIR, ATOM_E3_WATER, ATOM_SALT};
	for (int i = 0; i < 5; i++) {
		int at = atoms_that_match_with_themselves[i];
		if ((qstate == PHYTO_QSTATE_FLAG(at)) && (atom_type == at))
			return PHYTO_SELECT_MARBLE_RESPONSE_DELETE_ALL;
	}

	// All other cases do not allow selection when an atom of a contained type is already selected.
	if (qstate & PHYTO_QSTATE_FLAG(atom_type))
		return PHYTO_SELECT_MARBLE_RESPONSE_CLEAR_SELECTION;

	// This allows seeing the group in an order-independent manner.
	int new_qstate = qstate | PHYTO_QSTATE_FLAG(atom_type);

	// The cardinal elements match with salt.
	for (int i = ATOM_E0_FIRE; i <= ATOM_E3_WATER; i++)
		if (new_qstate == (PHYTO_QSTATE_FLAG(i) | PHYTO_QSTATE_FLAG(ATOM_SALT)))
			return PHYTO_SELECT_MARBLE_RESPONSE_DELETE_ALL;

	// Vitae matches with Mors.
	if (new_qstate == (PHYTO_QSTATE_FLAG(ATOM_VITAE) | PHYTO_QSTATE_FLAG(ATOM_MORS)))
		return PHYTO_SELECT_MARBLE_RESPONSE_DELETE_ALL;

	// Each metal (except Gold) matches with quicksilver. (Locked metals are dimmed, so don't reach here.)
	for (int i = ATOM_M0; i <= ATOM_M4; i++)
		if (new_qstate == (PHYTO_QSTATE_FLAG(i) | PHYTO_QSTATE_FLAG(ATOM_QUICKSILVER)))
			return PHYTO_SELECT_MARBLE_RESPONSE_DELETE_ALL;

	// Quintessence must be in the old qstate to be considered.
	// (This is really just a UX thing, not a real rule.)
	if (qstate & PHYTO_QSTATE_FLAG(ATOM_QUINTESSENCE)) {
		// If the quintessence would be complete, then it's complete; we're done here.
		if (new_qstate == PHYTO_QSTATE_COMPLETE)
			return PHYTO_SELECT_MARBLE_RESPONSE_DELETE_ALL;
		// The above "do not allow selection if already selected" rule prevents matching >1 of the same element (including quintessence itself).
		// For this reason, we can safely assume that if the new qstate both contains quintessence and only contains a valid quintessence qstate, we can keep selecting.
		if ((new_qstate & PHYTO_QSTATE_FLAG(ATOM_QUINTESSENCE)) && ((new_qstate & PHYTO_QSTATE_COMPLETE) == new_qstate))
			return PHYTO_SELECT_MARBLE_RESPONSE_SELECT;
	}

	// Failed; clear selection.
	return PHYTO_SELECT_MARBLE_RESPONSE_CLEAR_SELECTION;
}

void UdonClearBoard() {
	for (int i = 0; i < MARBLE_COUNT; i++)
		game_state[i] = ATOM_NONE | ATOM_FLAG_CALC_DIMMED;
	phyto_calc_qstate = 0;
}

int UdonGetMarble(int marble) {
	if (marble < 0 || marble >= MARBLE_COUNT)
		return 0;
	return game_state[marble];
}

void UdonSetMarble(int marble, int value) {
	if (marble < 0 || marble >= MARBLE_COUNT)
		return;
	game_state[marble] = value;
}

void UdonRecalculate() {
	phyto_update_calc();
}

void UdonSelectMarble(int marble) {
	// Just in case.
	phyto_update_calc();
	switch (phyto_select_marble_response(marble)) {
		case PHYTO_SELECT_MARBLE_RESPONSE_FAIL:
			break;
		case PHYTO_SELECT_MARBLE_RESPONSE_SELECT:
			game_state[marble] |= ATOM_FLAG_SELECTED;
			phyto_update_calc();
			break;
		case PHYTO_SELECT_MARBLE_RESPONSE_CLEAR_SELECTION:
			for (int i = 0; i < MARBLE_COUNT; i++)
				game_state[i] &= ~ATOM_FLAG_SELECTED;
			phyto_update_calc();
			break;
		case PHYTO_SELECT_MARBLE_RESPONSE_DELETE_ALL:
			game_state[marble] = ATOM_NONE | ATOM_FLAG_CALC_DIMMED;
			for (int i = 0; i < MARBLE_COUNT; i++)
				if (game_state[i] & ATOM_FLAG_SELECTED)
					game_state[i] = ATOM_NONE | ATOM_FLAG_CALC_DIMMED;
			phyto_update_calc();
			break;
	}
}
