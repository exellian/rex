use structx::*;

pub fn add(left: usize, right: usize) -> usize {
    left + right
}

pub fn struct_name(mut field_names: Vec<&str>) -> String {
    field_names.sort();
    "".to_string()
}

//pub fn stype(input: TokenStream) -> Token

#[cfg(test)]
mod tests {
    use super::*;
    use structx::*;

    #[test]
    fn it_works() {
        let result = add(2, 2);
        let x = structx! { name: 0 };
        assert_eq!(result, 4);
    }
}
