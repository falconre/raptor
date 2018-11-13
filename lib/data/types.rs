pub struct Struct {}


pub enum Type {
    U8,
    U16,
    U32,
    U64,
    Struct(Struct),
    Pointer(Box<Type>),
}