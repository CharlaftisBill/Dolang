
Number : union {
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

NewOperation : (left number, op operator, right number) owned<Operation> {
    op = Operation {
        op
        right
    }
    success op.move() 
} 

owned<Operation>.Eval : () number {
    if self.Left.TYPE != self.Right.TYPE {
        left
        failure NotMatchingTypes 0
    }

    result number = number(0)
    match self.OP {
        SUM: result = self.Left + self.Right
        SUB: result = self.Left - self.Right
        MUL: result = self.Left * self.Right
        DIV: result = self.Left / self.Right
        PWR: result = self.Left ^ self.Right
    }

    success result
}