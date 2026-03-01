main () : {
    x u8 = 5

    # if, else block
    if x == 5 {
        print("x is 5")
    } else x < 5 {
        print("x is less than 5")
    } else {
        print("x is more than 5")
    }

    # Match
    user_input str : io.ask("Do you want to continue? [y/yes, n/no, a/aboard]")
    should_continue bool = false
    match user_input {
        "y", "yes", "Y", "YES" { should_continue = true },
        "n", "no", "N", "NO" { should_continue = false },
        "a", "aboard", "A", "ABOARD" {
            print("See you!")
            os.exit(1)
        },
        * { print("Invalid option!") },
    }

    # catch
    file_as_string str : fs.Read_file("/path/to/file.txt") catch {
        fileNotExists {
 			panic("File is not existing!")	
		},
    }

    # for
    arr [4]i32 : [i32: 1,4,2,6]
    for i, elem in arr {
        print(i)
    }

    for i, elem in [i32: 3,6,4,8] {
        print(i)
    }

    # while
    while_limit size : 123
    current_index size = 0
    while current_index <= while_limit {
        if current_index % 14 == 0 {
            print(current_index)
        }else current_index % 14 == 0 && current_index % 8 == 0 {
            continue
        }
        current_index += 1
    }
}