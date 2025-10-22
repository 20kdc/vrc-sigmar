. = 0;
SECTIONS {
	/DISCARD/ : {
		*(.comment)
		*(.symtab)
		*(.strtab)
		*(.shstrtab)
		*(.riscv.attributes)
	}
	.text : {
		. = ALIGN(4);
		*(.text)
		*(.text.*)
	}
	.other : {
		. = ALIGN(4);
		*(*)
	}
}
