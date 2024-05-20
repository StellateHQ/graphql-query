// TODO: Replace with print comparison (See FloatValue's write_to_buffer)
// This is here *just* so we can have PartialEq on other nodes, but we shouldn't ever actually compare
// floats

// See: https://github.com/reem/rust-ordered-float/blob/8962ffc/src/lib.rs
use super::ast::FloatValue;
use core::hash::{Hash, Hasher};

// masks for the parts of the IEEE 754 float
const SIGN_MASK: u64 = 0x8000_0000_0000_0000_u64;
const EXP_MASK: u64 = 0x7ff0_0000_0000_0000_u64;
const MAN_MASK: u64 = 0x000f_ffff_ffff_ffff_u64;

// canonical raw bit patterns (for hashing)
const CANONICAL_NAN_BITS: u64 = 0x7ff8_0000_0000_0000_u64;
const CANONICAL_ZERO_BITS: u64 = 0x0u64;

#[inline]
fn raw_double_bits(f: &f64) -> u64 {
    if f.is_nan() {
        return CANONICAL_NAN_BITS;
    }

    // See: https://github.com/rust-num/num-traits/blob/edb4821/src/float.rs#L2027-L2041
    let bits: u64 = f.to_bits();
    let sign: i8 = if bits >> 31 == 0 { 1 } else { -1 };
    let mut exp: i16 = ((bits >> 52) & 0x7ff) as i16;
    let man = if exp == 0 {
        (bits & 0x000f_ffff_ffff_ffff) << 1
    } else {
        (bits & 0x000f_ffff_ffff_ffff) | 0x0010_0000_0000_0000
    };
    // Exponent bias + mantissa shift
    exp -= 1023 + 52;

    if man == 0 {
        return CANONICAL_ZERO_BITS;
    }

    let man_u64 = man;
    let exp_u64 = u64::from(exp as u16);
    let sign_u64 = if sign > 0 { 1u64 } else { 0u64 };
    (man_u64 & MAN_MASK) | ((exp_u64 << 52) & EXP_MASK) | ((sign_u64 << 63) & SIGN_MASK)
}

impl<'a> Hash for FloatValue<'a> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        let float = self.value.parse::<f64>().unwrap_or(0.0);
        if float.is_nan() {
            raw_double_bits(&f64::NAN).hash(state);
        } else {
            raw_double_bits(&float).hash(state);
        }
    }
}

impl<'a> PartialEq for FloatValue<'a> {
    fn eq(&self, other: &Self) -> bool {
        self.value == other.value
    }
}

impl<'a> Eq for FloatValue<'a> {}
