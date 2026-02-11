# functions
#great () : {
#    print("Hello!" )
#}

sum (lh i32, rh i32) i32 : {
    success (lh + rh)
    print("hey")
}

#div (lh i32, rh i32) f32 : {
#    if rh == 0 {
#        failure DivByZero math.inf
#    }
#    success lh / rh
#}