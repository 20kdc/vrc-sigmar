macro_rules! bitenum {
    ($name:ident $mask:literal $(
        $val:literal => $id:ident
    )*) => {
        #[derive(Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Debug)]
        #[repr(u32)]
        pub enum $name {
            $($id = $val),*
        }
        impl From<u32> for $name {
            fn from(n: u32) -> Self {
                match n & $mask {
                    $(
                        $val => Self::$id,
                    )*
                    _ => unreachable!()
                }
            }
        }
    }
}
bitenum! {
    Kip32Reg 31
    0 => Zero
    1 => RA
    2 => SP
    3 => X3
    4 => X4
    5 => T0
    6 => T1
    7 => T2
    8 => FP
    9 => S1
    10 => A0
    11 => A1
    12 => A2
    13 => A3
    14 => A4
    15 => A5
    16 => A6
    17 => A7
    18 => S2
    19 => S3
    20 => S4
    21 => S5
    22 => S6
    23 => S7
    24 => S8
    25 => S9
    26 => S10
    27 => S11
    28 => T3
    29 => T4
    30 => T5
    31 => T6
}
