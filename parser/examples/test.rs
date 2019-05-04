use parser::parser;
/*parser! {
    S{
        S[L,Symbol(Equal),R],
        S[R],
        L[Symbol(Star),R],
        L[Ident(_)],
        R[L]
    }
}*/
/*
parser! {
    E{
        E[E,Symbol(Add),T],
        E[T],
        T[T,Symbol(Star),F],
        T[F],
        F[Symbol(LP),E,Symbol(RP)],
        L[Ident(_)]
    }
}*//*
parser! {
    S{
        S[C,C];
        C[Symbol(c),C];
        C[Symbol(d)];
    }
}*/
parser! {
    E{
        E->[E,Symbol(Star),E];
        E->[E,Symbol(Add),E];
        //E->[Symbol(LP),E,Symbol(RP)];
        E->[Ident(_)];
    }
}
enum Symbol {
    Add,
    Star,
    LP,
    RP,
}
struct Ident(String);
fn main() {}
