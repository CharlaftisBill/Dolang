# arithmetical
arith0 f32 : 1 + 2
arith1 f32 = 3 - 4
arith2 f32 = 5 / 6
arith3 f32 = 7 % 8
arith4 f32 = 9 * 10

arith5 f32 = (1 + (2 - 3)) / 4 * 5 % 6
arith6 f32 = (1 + (arith1 - arith2)) / arith3 * arith0 % arith4
arith7 f32 : sum(1, 2)
arith8 f32 = (sum(1, 2) + (arith1 - arith2)) / arith3 * arith0 % arith4
