#[derive(Debug)]
pub struct Program {
    pub items: Vec<Item>,
}

#[derive(Debug)]
pub enum Item {
    Require(String),
    Class(Class),
    Function(Function),
}

#[derive(Debug)]
pub struct Class {
    pub name: String,
    pub fields: Vec<Field>,
    pub methods: Vec<Function>,
    pub public: bool,
    pub dynamic: bool,
}

#[derive(Debug)]
pub struct Field {
    pub name: String,
    pub ty: Type,
    pub public: bool,
    pub mutable: bool,
    pub dynamic: bool,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub public: bool,
    pub dynamic: bool, // dynamic=false => static
}

#[derive(Debug)]
pub enum Type {
    Ident(String),
    Pointer(Box<Type>),
}