// For use in https://bits.ondrovo.com/sigmar/
(() => {
	// cut out key functionality for speed
	// console also has to be cleared often
	window.game.renderPreparedBoard = () => {};
	window.game.updateGameGui = () => {};
	window.game.get_opts.url_seed = false;
	let data = "";
	let indexMap = [
		0, 1, 2, 3, 4, 5,
		11, 12, 13, 14, 15, 16, 17,
		22, 23, 24, 25, 26, 27, 28, 29,
		33, 34, 35, 36, 37, 38, 39, 40, 41,
		44, 45, 46, 47, 48, 49, 50, 51, 52, 53,
		55, 56, 57, 58, 59, 60, 61, 62, 63, 64, 65,
		67, 68, 69, 70, 71, 72, 73, 74, 75, 76,
		79, 80, 81, 82, 83, 84, 85, 86, 87,
		91, 92, 93, 94, 95, 96, 97, 98,
		103, 104, 105, 106, 107, 108, 109,
		115, 116, 117, 118, 119, 120
	];
	let otMap = {
		mercury: "q",
		salt: "s",
		fire: "f",
		earth: "e",
		water: "w",
		air: "a",
		vitae: "v",
		mors: "m",
		lead: "0",
		tin: "1",
		iron: "2",
		copper: "3",
		silver: "4",
		gold: "5"
	};
	const PASS_SIZE = 64;
	let passes = 2048 / PASS_SIZE;
	let seed = 0;
	function runPass() {
		console.clear();
		let subData = "";
		for (let i = 0; i < PASS_SIZE; i++) {
			window.game.newGame(seed);
			seed++;
			for (let j = 0; j < 91; j++) {
				let orb = window.game.board.getOrbByIndex(indexMap[j]);
				if (orb) {
					// Since conversion is turned off, the board doesn't remap its internal state the way it should.
					// That's fine.
					let mapped = orb.symbol ? otMap[orb.symbol] : otMap[orb];
					if (!mapped)
						throw new Error("mapping: " + orb);
					subData += mapped;
				} else {
					subData += "_";
				}
			}
			subData += "|";
		}
		data += subData;
	}
	let passThenPass = () => {
		if (passes > 0) {
			passes--;
			runPass();
			setTimeout(passThenPass, 1);
		} else {
			console.log(data);
		}
	};
	passThenPass();
})();
