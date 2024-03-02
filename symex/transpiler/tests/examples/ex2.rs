use transpiler::pseudo;
fn main() {
    
    pseudo!(
        ret.extend[
            // Backup carry bit
            old_carry = carry;
            // Set carry  bit to 1
            carry = one;

            intermediate = !rn;
            // add with carry
            rd = intermediate adc imm;
            LocalAddress("address",32) = rd;

            if w {
                rn = rn - (4u32* n).local_into();
            }
        ]
    );
}
