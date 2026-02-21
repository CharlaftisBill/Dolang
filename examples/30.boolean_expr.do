# boolean
bool0 bool = true && false
bool1 bool = true || false
bool2 bool = !true

bool3 bool = 1 < 2
bool4 bool = 3 > 4
bool5 bool = 5 <= 6
bool6 bool = 7 >= 8
bool7 bool = 9 != 10
bool8 bool = 11 == 12

bool9  bool = (true && (false || true)) && !false

bool10 bool = (1 < 2) && (3 >= 4)
bool11 bool = (5 <= 6) || (7 > 8)
bool12 bool = !(9 == 10)
bool13 bool = ((1 < 2) && (3 != 4)) || (5 >= 6)
bool14 bool = ((7 <= 8) || (9 > 10)) && !(11 < 12)

bool15 bool = (bool1 && (bool3 || bool8)) || bool0
bool16 bool = (bool10 && (bool5 || bool7)) && !bool4
bool17 bool = (bool6 || (bool4 && !bool5)) && bool3

bool18 bool : and(true, false)
bool19 bool : and(1 < 2, 3 >= 4)

bool20 bool = (and(true, false) || (bool3 && !bool8)) && bool1
bool21 bool = (and(1 < 2, 3 != 4) || (5 <= 6 && 7 > 8)) && bool5
