pub fn bit_is_set(val: u8, bit: u8) -> bool {
    val & (1u8 << bit) != 0
}

pub fn set_bit(val: u8, bit: u8, enable: bool) -> u8 {
    let uval = enable as u8;

    (val & !(1u8 << bit)) | (uval << bit)
}
