# user defined types

race enum : {
    ELF,
    ORC,
    HUMAN,
    GOBLIN,
}

second_race union : {
    race    Race,
    none    _,
}

location type : {
    x   i32,
    y   i32,
    z   i32,
}

direction enum : {
    UP,
    RIGHT,
    DOWN,
    LEFT,
}

stat type : u32
Player type : {
    Speed       stat,
    Charisma    stat,
    Race        race,
    SecondRace  Second_race,
    Location    location,
}

todo: interfaces