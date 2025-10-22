#![allow(dead_code)]

use crate::UdonAsm;

macro_rules! udon_ext {
    ($($id:ident ($($arg:ident)*) = $value:literal)*) => {
        pub struct UdonExterns {
            $(
                pub $id: String,
            )*
        }
        impl UdonExterns {
            pub fn new(asm: &UdonAsm) -> UdonExterns {
                UdonExterns {
                    $(
                        $id: asm.ensure_extern($value),
                    )*
                }
            }
            $(
                pub fn $id(&self, asm: &UdonAsm$(, $arg: impl std::fmt::Display)*) {
                    $(
                        asm.push($arg);
                    )*
                    asm.ext(&self.$id);
                }
            )*
        }
    };
}
udon_ext!(
    obj_equality(a b r) =
        "SystemObject.__op_Equality__SystemObject_SystemObject__SystemBoolean"
    bytearray_create(size r) =
        "SystemByteArray.__ctor__SystemInt32__SystemByteArray"
    i32_eq(a b r) =
        "SystemInt32.__op_Equality__SystemInt32_SystemInt32__SystemBoolean"
    i32_neq(a b r) =
        "SystemInt32.__op_Inequality__SystemInt32_SystemInt32__SystemBoolean"
    i32_ge(a b r) =
        "SystemInt32.__op_GreaterThanOrEqual__SystemInt32_SystemInt32__SystemBoolean"

    i32_lt(a b r) =
        "SystemInt32.__op_LessThan__SystemInt32_SystemInt32__SystemBoolean"
    i32_add(a b r) =
        "SystemInt32.__op_Addition__SystemInt32_SystemInt32__SystemInt32"
    i32_sub(a b r) =
        "SystemInt32.__op_Subtraction__SystemInt32_SystemInt32__SystemInt32"
    i32_xor(a b r) =
        "SystemInt32.__op_LogicalXor__SystemInt32_SystemInt32__SystemInt32"
    i32_or(a b r) =
        "SystemInt32.__op_LogicalOr__SystemInt32_SystemInt32__SystemInt32"
    i32_and(a b r) =
        "SystemInt32.__op_LogicalAnd__SystemInt32_SystemInt32__SystemInt32"
    i32_shl(a b r) =
        "SystemInt32.__op_LeftShift__SystemInt32_SystemInt32__SystemInt32"
    i32_shr(a b r) =
        "SystemInt32.__op_RightShift__SystemInt32_SystemInt32__SystemInt32"
    i32_mul(a b r) =
        "SystemInt32.__op_Multiplication__SystemInt32_SystemInt32__SystemInt32"
    // This extern is risky because it will error on negative numbers.
    // For this reason, it's only used in the indirect jump code.
    u32_fromi32(i r) = "SystemConvert.__ToUInt32__SystemInt32__SystemUInt32"
    u32_add(a b r) =
        "SystemUInt32.__op_Addition__SystemUInt32_SystemUInt32__SystemUInt32"
    // init, etc.
    base64_decode(i r) =
        "SystemConvert.__FromBase64String__SystemString__SystemByteArray"
    read_byte(i o r) = "SystemByteArray.__Get__SystemInt32__SystemByte"
    bytearray_copy(i a o) =
        "SystemByteArray.__CopyTo__SystemArray_SystemInt32__SystemVoid"
    // readers (byte-array, offset)
    read_i32(a o r) =
        "SystemBitConverter.__ToInt32__SystemByteArray_SystemInt32__SystemInt32"
    read_u16(a o r) =
        "SystemBitConverter.__ToUInt16__SystemByteArray_SystemInt32__SystemUInt16"
    // reader unsigned conversions
    i32_fromu8(i r) = "SystemConvert.__ToInt32__SystemByte__SystemInt32"
    i32_fromu16(i r) = "SystemConvert.__ToInt32__SystemUInt16__SystemInt32"
    // This thing
    i32_frombool(i r) = "SystemConvert.__ToInt32__SystemBoolean__SystemInt32"

    intarray_create(size r) =
        "SystemInt32Array.__ctor__SystemInt32__SystemInt32Array"
    intarray_get(i o r) =
        "SystemInt32Array.__Get__SystemInt32__SystemInt32"
    intarray_set(i o v) =
        "SystemInt32Array.__Set__SystemInt32_SystemInt32__SystemVoid"

    // Reader and writer are crazy code caused by SystemConvert getting way too stabby. You heard it here first...
    // BlockCopy kinda saved the project's performance.
    bcopy(source source_ofs dest dest_ofs bytes) =
        "SystemBuffer.__BlockCopy__SystemArray_SystemInt32_SystemArray_SystemInt32_SystemInt32__SystemVoid"
);
