#[derive(Debug)]
pub struct Program {
    pub items: Vec<Statement>,
}

#[derive(Debug)]
pub enum Statement {
    Require(Require),
    ClassDeclaration(Class),
    FunctionDeclaration(Function),
    Expression(Expression),
}

#[derive(Debug)]
pub enum Require {
    Module(String, Box<Require>),
    EndingModule(String),
    Identifier(String),
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
    pub field_type: Type,
    pub public: bool,
    pub mutable: bool,
    pub dynamic: bool,
}

#[derive(Debug)]
pub struct Function {
    pub name: String,
    pub public: bool,
    pub dynamic: bool, // dynamic=false => static
    pub return_type: Type,
}

#[derive(Debug)]
pub enum Operator {
    Addition,
    Subtraction,
    Multiplication,
    Division,
}

#[derive(Debug)]
pub enum Expression {
    Binary {
        left: Box<Expression>,
        right: Box<Expression>,
        op: Operator,
        ty: Option<Type>,
    },
    Unary {
        right: Box<Expression>,
        op: Operator,
        ty: Option<Type>,
    },
    Literal {
        val: Literal,
        ty: Option<Type>,
    },
}

#[derive(Debug)]
pub enum Literal {
    Boolean(bool),
    Integer(i64),
    Float(f64),
    String(String),
    Variable(String),
}

impl Literal {
    pub fn to_integer(&self) -> i64 {
        match self {
            Literal::Integer(i) => *i,
            _ => panic!("Literal::to_integer")
        }
    }

    pub fn to_usize(&self) -> usize {
        match self {
            Literal::Integer(i) => *i as usize,
            _ => panic!("Literal::to_usize")
        }
    }
}

#[derive(Debug)]
pub enum Type {
    Ident(String),
    SizeIdent(String, usize),
    Pointer(Box<Type>),
}