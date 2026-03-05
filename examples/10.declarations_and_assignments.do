# integers
int0 i8 = 1
int0 += 2
int0 *= 3
int0 /= 4
int0 -= 5
int0 %= 6

int1 u16 : 1
Int2 i32 = -1

# floats
float0 f32 = 3.14
float0 = - 6.28

# bools
bool0 bool = true
bool1 bool : false
bool0 = false

# characters
char0 char = 'a'
char0 = 'b'

# strings
str0 str = "The quick \"brown fox\" jumps over the lazy dog.\n"
str0 = "An other \"string\""

# Arrays
arr_int0 [3 i32] = [i32: 0,1,2]
arr_int0[1] = 3

arr_str0 [2 str] = [str: "0.1", "0.2"]
arr_str0[3 - 2] = "3"

arr_3d [2, 2, 2 i32] = [i32: [[0, 1], [2, 3]], [[4, 5], [6, 7]]]
arr_3d[0, 2 / 2, 0] = (3 - 2) * 4 / 4

