use anyhow::*;
use std::collections::BTreeMap;

pub const STT_NOTYPE: u8 = 0;
pub const STT_OBJECT: u8 = 1;
pub const STT_FUNC: u8 = 2;
pub const STT_SECTION: u8 = 3;
pub const STT_FILE: u8 = 4;

#[derive(Clone)]
pub struct Sci32SymInfo {
    pub st_name: String,
    pub st_addr: u32,
    pub st_type: u8,
    /// If the symbol is in the .kip32export section (and thus is expected to be exported)
    pub export_section: bool,
}

/// This contains the data read in from the ELF.
#[derive(Clone, Default)]
pub struct Sci32Image {
    /// Array of words (treat as little-endian).
    pub data: Vec<u32>,
    /// Instructions are concentrated in the first half of the image.
    /// This is to reduce the amount of redundant compilation, but it can also act as a sort of memory permissions system.
    pub instructions: usize,
    /// Symbol table.
    /// A BTreeMap is used due to determinism issues with HashMap.
    pub symbols: BTreeMap<String, Sci32SymInfo>,
}

fn get<const LEN: usize>(bytes: &[u8], at: usize) -> Result<[u8; LEN]> {
    let mut targ: [u8; LEN] = [0; LEN];
    match bytes.get(at..(at + LEN)) {
        None => Err(anyhow!("Out of range access")),
        Some(v) => {
            targ.copy_from_slice(v);
            Ok(targ)
        }
    }
}

fn get_u32(bytes: &[u8], at: usize) -> Result<u32> {
    Ok(u32::from_le_bytes(get::<4>(bytes, at)?))
}

fn get_u16(bytes: &[u8], at: usize) -> Result<u16> {
    Ok(u16::from_le_bytes(get::<2>(bytes, at)?))
}

fn get_u8(bytes: &[u8], at: usize) -> Result<u8> {
    Ok(get::<1>(bytes, at)?[0])
}

fn get_string(bytes: &[u8], at: usize) -> Result<String> {
    let mut zero_at: usize = at;
    while get_u8(bytes, zero_at)? != 0 {
        zero_at += 1;
    }
    return Ok(String::from_utf8_lossy(&bytes[at..zero_at]).into_owned());
}

/// We need to care about this section type because we use it to find symbols.
const SHT_SYMTAB: u32 = 2;
/// We need to care about this section type because we skip the blit proper (pad only).
const SHT_NOBITS: u32 = 8;
/// We blit anything with this section flag.
const SHF_ALLOC: u32 = 2;
/// When doing the pre-blit padding, we control the code flag with this.
const SHF_EXECINSTR: u32 = 4;

struct TmpSectionInfo {
    // This is a u32 because we need the section info to resolve section names
    sh_name: u32,
    sh_type: u32,
    sh_flags: u32,
    sh_addr: u32,
    sh_offset: u32,
    sh_size: u32,
    sh_link: u32,
}

impl Sci32Image {
    /// Ensures that everything *before* the given byte address exists.
    /// (The given byte address itself is not so protected.)
    /// If 'code' is set, the instruction fold is placed accordingly.
    pub fn pad(&mut self, extent: usize, code: bool) {
        let mut word = extent >> 2;
        if (extent & 3) != 0 {
            word = word + 1;
        }
        if word > self.data.len() {
            self.data.resize(word, 0);
        }
        if code && self.instructions < word {
            self.instructions = word;
        }
    }
    /// Writes a value into the image memory at the given byte address.
    /// Slow, but reliable.
    pub fn write8(&mut self, at: usize, val: u8) {
        let word = at >> 2;
        let subword = (at & 3) as u32;
        if word >= self.data.len() {
            self.data.resize(word + 1, 0);
        }
        let shift = subword * 8;
        self.data[word] &= !(0xFFu32 << shift);
        self.data[word] |= (val as u32) << shift;
    }

    /// Reads an ELF into a Sci32Image. Multiple ELFs may be loaded, but you almost certainly never want to do this as it requires very careful linking.
    /// Overlaps are *not* detected.
    /// An error may result in a partially loaded image.
    pub fn from_elf(&mut self, bytes: &[u8]) -> Result<()> {
        // read header
        let shoff = get_u32(bytes, 0x20)?;
        let shnum = get_u16(bytes, 0x30)?;
        let shstrndx = get_u16(bytes, 0x32)?;
        if shstrndx >= shnum {
            return Err(anyhow!("Out of range section string table"));
        }
        // mine out useful info from the section headers
        let mut section_info: Vec<TmpSectionInfo> = Vec::new();
        for i in 0..shnum {
            let base = (shoff as usize) + ((i as usize) * 40);
            section_info.push(TmpSectionInfo {
                sh_name: get_u32(bytes, base)?,
                sh_type: get_u32(bytes, base + 4)?,
                sh_flags: get_u32(bytes, base + 8)?,
                sh_addr: get_u32(bytes, base + 12)?,
                sh_offset: get_u32(bytes, base + 16)?,
                sh_size: get_u32(bytes, base + 20)?,
                sh_link: get_u32(bytes, base + 24)?,
            });
        }
        // process sections
        for section in &section_info {
            if (section.sh_flags & SHF_ALLOC) != 0 {
                // This section is blittable.
                let end_addr = (section.sh_addr as usize) + (section.sh_size as usize);
                // Always pad; this is important for marking as code, and if NOBITS is involved the 'blit proper' isn't done.
                self.pad(end_addr, (section.sh_flags & SHF_EXECINSTR) != 0);
                if section.sh_type != SHT_NOBITS {
                    for i in 0..section.sh_size {
                        let ofs = (section.sh_offset as usize) + (i as usize);
                        let addr = (section.sh_addr as usize) + (i as usize);
                        self.write8(addr, get_u8(bytes, ofs)?);
                    }
                }
            }
            if section.sh_type == SHT_SYMTAB {
                // sh_link is 'operating system specific' for some reason.
                // Of course, we (and ImHex) knows it's the string table.
                let strtab_idx = section.sh_link as usize;
                if strtab_idx >= section_info.len() {
                    return Err(anyhow!("Out of range symtab string table"));
                }
                let strtab = &section_info[strtab_idx];
                let sym_count = section.sh_size / 16;
                for sym in 0..sym_count {
                    let base = (section.sh_offset as usize) + ((sym as usize) * 16);
                    let name = (strtab.sh_offset as usize) + (get_u32(bytes, base)? as usize);
                    let value = get_u32(bytes, base + 4)?;
                    // st_size between
                    let info = get_u8(bytes, base + 12)?;
                    let st_bind = info >> 4;
                    let st_type = info & 0xF;
                    let st_shndx = get_u16(bytes, base + 14)? as usize;
                    let name_str = get_string(bytes, name)?;
                    // eprintln!("Symbol @ {:08X} : {}, {}", base, name_str, st_bind);
                    if name_str.eq("") || st_bind == 0 {
                        // This is an invisible symbol and thus not exported.
                    } else {
                        let mut export_section = false;
                        if st_shndx < section_info.len() {
                            let strofs = (section_info[st_shndx].sh_name
                                + section_info[shstrndx as usize].sh_offset)
                                as usize;
                            if get_string(bytes, strofs)?.eq(".kip32_export") {
                                export_section = true;
                            }
                        }
                        // Exported.
                        self.symbols.insert(
                            name_str.clone(),
                            Sci32SymInfo {
                                st_name: name_str.clone(),
                                st_addr: value,
                                export_section,
                                st_type,
                            },
                        );
                    }
                }
            }
        }
        Ok(())
    }
    /// Returns the initialized (non-zero) start of memory.
    pub fn initialized_bytes(&self) -> Vec<u8> {
        let mut res = Vec::new();
        for v in &self.data {
            res.extend_from_slice(&v.to_le_bytes());
        }
        while res.len() > 0 && res[res.len() - 1] == 0 {
            res.truncate(res.len() - 1);
        }
        res
    }
    /// Determines if an instruction ought to be at this address.
    pub fn is_instruction_at(&self, x: u32) -> bool {
        if (x & 3) != 0 {
            false
        } else {
            x < ((self.instructions as u32) * 4)
        }
    }
}
