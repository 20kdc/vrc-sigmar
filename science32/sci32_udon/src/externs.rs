use crate::UdonAsm;

macro_rules! udon_ext {
    ($($id:ident = $value:literal)*) => {
        pub struct UdonExterns {
            $(
                pub $id: String,
            )*
        }
        impl UdonExterns {
            pub fn new(asm: &mut UdonAsm) -> UdonExterns {
                UdonExterns {
                    $(
                        $id: asm.ensure_extern($value),
                    )*
                }
            }
        }
    };
}
udon_ext!(
    obj_equality =
        "SystemObject.__op_Equality__SystemObject_SystemObject__SystemBoolean"
    bytearray_create =
        "SystemByteArray.__ctor__SystemInt32__SystemByteArray"
    i32_eq =
        "SystemInt32.__op_Equality__SystemInt32_SystemInt32__SystemBoolean"
    i32_neq =
        "SystemInt32.__op_Inequality__SystemInt32_SystemInt32__SystemBoolean"
    i32_ge =
        "SystemInt32.__op_GreaterThanOrEqual__SystemInt32_SystemInt32__SystemBoolean"

    i32_lt =
        "SystemInt32.__op_LessThan__SystemInt32_SystemInt32__SystemBoolean"
    i32_add =
        "SystemInt32.__op_Addition__SystemInt32_SystemInt32__SystemInt32"
    i32_sub =
        "SystemInt32.__op_Subtraction__SystemInt32_SystemInt32__SystemInt32"
    i32_xor =
        "SystemInt32.__op_LogicalXor__SystemInt32_SystemInt32__SystemInt32"
    i32_or =
        "SystemInt32.__op_LogicalOr__SystemInt32_SystemInt32__SystemInt32"
    i32_and =
        "SystemInt32.__op_LogicalAnd__SystemInt32_SystemInt32__SystemInt32"
    i32_shl =
        "SystemInt32.__op_LeftShift__SystemInt32_SystemInt32__SystemInt32"
    i32_shr =
        "SystemInt32.__op_RightShift__SystemInt32_SystemInt32__SystemInt32"
    i32_mul =
        "SystemInt32.__op_Multiplication__SystemInt32_SystemInt32__SystemInt32"
    // Not a typo.
    u32_fromi32 = "SystemConvert.__ToUInt32__SystemInt32__SystemUInt32"
    i32_fromu32 = "SystemConvert.__ToInt32__SystemUInt32__SystemInt32"
    u32_shr =
        "SystemUInt32.__op_RightShift__SystemUInt32_SystemInt32__SystemUInt32"
    u32_add =
        "SystemUInt32.__op_Addition__SystemUInt32_SystemUInt32__SystemUInt32"
    // init, etc.
    base64_decode =
        "SystemConvert.__FromBase64String__SystemString__SystemByteArray"
    read_byte = "SystemByteArray.__Get__SystemInt32__SystemByte"
    write_byte =
        "SystemByteArray.__Set__SystemInt32_SystemByte__SystemVoid"
    bytearray_copy =
        "SystemByteArray.__CopyTo__SystemArray_SystemInt32__SystemVoid"
    // readers (byte-array, offset)
    read_i32 =
        "SystemBitConverter.__ToInt32__SystemByteArray_SystemInt32__SystemInt32"
    read_u16 =
        "SystemBitConverter.__ToUInt16__SystemByteArray_SystemInt32__SystemUInt16"
    // reader unsigned conversions
    i32_fromu8 = "SystemConvert.__ToInt32__SystemByte__SystemInt32"
    i32_fromu16 = "SystemConvert.__ToInt32__SystemUInt16__SystemInt32"
    // Writer is crazy code caused by SystemConvert getting way too stabby. You heard it here first...
    tobytes_i32 =
        "SystemBitConverter.__GetBytes__SystemInt32__SystemByteArray"
    tobytes_u16 =
        "SystemBitConverter.__GetBytes__SystemUInt16__SystemByteArray"
    u8_fromi32 = "SystemConvert.__ToByte__SystemInt32__SystemByte"
    u16_fromi32 = "SystemConvert.__ToUInt16__SystemInt32__SystemUInt16"
);
