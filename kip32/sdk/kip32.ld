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
	.init : {
		. = ALIGN(4);
		*(.rodata)
		*(.rodata.*)
		*(.data)
		*(.data.*)
		*(.sdata)
		*(.sdata.*)
	}
	.end : {
		. = ALIGN(4);
		/* uninitialized data */
		*(.bss)
		*(.bss.*)
		*(.sbss)
		*(.sbss.*)
	}
	/* If this section is marked ALLOC, check your linker map with -M and adjust this file as necessary */
	.checklinkermap : {
		. = ALIGN(4);
		*(*)
	}
}
