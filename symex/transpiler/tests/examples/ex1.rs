use transpiler::pseudo;
fn main() {
    pseudo!(
        ret.extend[
            let a = a.local_into() + ADC(1+2).local_into();
            let b = 2.local_into();
            let b = 2;
            let b = !b;
        ]
    );
}
