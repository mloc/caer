#[derive(Debug)]
pub enum Statement {
    Decl(String),
    Assignment(String, Expr),
    Put(String),
}

#[derive(Debug)]
pub enum Expr {
    Term(Term),
    Add(Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum Term {
    Literal(i32),
    Ref(String),
}
