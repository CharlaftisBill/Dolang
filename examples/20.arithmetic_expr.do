# arithmetical
arith0 f32 : 1 + 2
arith1 f32 = 3 - 4
arith2 f32 = 5 / 6
arith3 f32 = 7 % 8
arith4 f32 = 9 * 10

arith5 f32 = (1 + (2 - 3)) / 4 * 5 % 6
arith6 f32 = (1 + (arith1 - arith2)) / arith3 * arith0 % arith4
arith7 f32 : sum(3 - 2, 2)
arith8 f32 = (sum(3 - 2, 2) + (arith1 - arith2)) / arith3 * arith0 % arith4

indices [5 i32] = [i32: 0, 1, 2, 3, 4]
indices[1 + 1] = 3
indices[2] = (3 - 2) * 4

d3 [2, 2, 2 i32] = [i32: [[1, 2], [3, 4]], [[5, 6], [7, 8]]]
d3[0, 2 / 2, 0] = 1
d3[0, 1, 0] = (3 - 2) * 4 / 4

odd_elem_mean _ = (indices[d3[0, 1, 1]] + indices[3]) / 2

result _ = get_matrix()[0][ x + 1][indices[0] * 2 ]

arith9 f32 = (sum(indices[d3[0, 1, 1]] - 2, indices[3]) + (arith1 - arith2)) / arith3 * arith0 % arith4
