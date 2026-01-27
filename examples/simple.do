# comment 1
# comment 3
my_var i32 = 3 + 1.0
my_var += 1
is_true bool = ((true || false) && false )

a_string string = "a \"string\" literal\n"

a_char char = 'a'

Number : union { # a union
    INT i64
    FLT f64
}

Operator : enum {
    SUM
    SUB
    MUL
    DIV
    PWR
}

Operation : kind {
    Left number
    Op operator
    Right number
}