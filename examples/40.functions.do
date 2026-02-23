# functions
say_hello () : {
    print("Hello!" )
}

sum (lh i32, rh i32) i32 : {
    print("hey")
    success (lh + rh)
}

div (lh i32, rh i32) f32, f32 : {
    if rh == 0 {
        failure DivByZero 0, 0
    }
    success lh / rh, lh % rh
}