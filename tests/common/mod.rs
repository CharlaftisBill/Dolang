use serde_json::{Number, Value};
use std::env;
use std::fs;
use std::path::Path;

pub fn assert_json_snapshot(snapshot_name: &str, actual_ast: &impl serde::Serialize) {
    let manifest_dir = env::var("CARGO_MANIFEST_DIR").expect("CARGO_MANIFEST_DIR not found");
    let snapshot_path = Path::new(&manifest_dir)
        .join("tests/snapshots")
        .join(format!("{}.json", snapshot_name));

    let actual_json_val: Value = serde_json::to_value(actual_ast).expect("Failed to serialize AST");
    let actual_json_str =
        serde_json::to_string_pretty(actual_ast).expect("Failed to stringify AST");


    if let Some(parent) = snapshot_path.parent() {
        let _ = fs::create_dir_all(parent);
    }

    let expected_json_val: Value = if snapshot_path.exists() {
        let content = fs::read_to_string(&snapshot_path).expect("Failed to read snapshot");
        if content.trim().is_empty() {
            Value::Null
        } else {
            serde_json::from_str(&content).expect("Invalid JSON")
        }
    } else {
        Value::Null
    };

    let mut has_errors = false;
    compare_values(
        "root",
        &expected_json_val,
        &actual_json_val,
        &mut has_errors,
    );

    if has_errors {
        eprintln!("\nAST Mismatch for '{}'", snapshot_name);
        eprintln!("File: {:?}", snapshot_path);

        std::fs::write("/tmp/ast.json", actual_json_str).expect("Unable to write file");
        eprintln!("\n--- FULL ACTUAL OUTPUT: /tmp/ast.json");

        panic!("Snapshot mismatch");
    }
}


fn compare_values(path: &str, expected: &Value, actual: &Value, has_errors: &mut bool) {
    use Value::*;

    match (expected, actual) {
        (Object(e_map), Object(a_map)) => {
            for (key, val) in e_map {
                let new_path = format!("{}.{}", path, key);
                match a_map.get(key) {
                    Some(a_val) => compare_values(&new_path, val, a_val, has_errors),
                    None => {
                        eprintln!("[-] Missing Key: {} (Expected: {:?})", new_path, val);
                        *has_errors = true;
                    }
                }
            }
            for (key, val) in a_map {
                if !e_map.contains_key(key) {
                    eprintln!("[+] Extra Key: {}.{} (Value: {:?})", path, key, val);
                    *has_errors = true;
                }
            }
        }
        (Array(e_arr), Array(a_arr)) => {
            let max_len = std::cmp::max(e_arr.len(), a_arr.len());
            for i in 0..max_len {
                let new_path = format!("{}[{}]", path, i);
                let e_item = e_arr.get(i);
                let a_item = a_arr.get(i);
                match (e_item, a_item) {
                    (Some(e), Some(a)) => compare_values(&new_path, e, a, has_errors),
                    (Some(e), None) => {
                        eprintln!("[-] Missing Item at {}: Expected {:?}", new_path, e);
                        *has_errors = true;
                    }
                    (None, Some(a)) => {
                        eprintln!("[+] Extra Item at {}: Got {:?}", new_path, a);
                        *has_errors = true;
                    }
                    (None, None) => unreachable!(),
                }
            }
        }
        (Number(n1), Number(n2)) => {
            if !numbers_approx_equal(n1, n2) {
                eprintln!("(!) Value Mismatch at {}", path);
                eprintln!("    Expected: {}", n1);
                eprintln!("    Actual:   {}", n2);
                *has_errors = true;
            }
        }
        (e, a) => {
            if e != a {
                eprintln!("(!) Value Mismatch at {}", path);
                eprintln!("    Expected: {}", e);
                eprintln!("    Actual:   {}", a);
                *has_errors = true;
            }
        }
    }
}

fn numbers_approx_equal(n1: &Number, n2: &Number) -> bool {
    if let (Some(f1), Some(f2)) = (n1.as_f64(), n2.as_f64()) {

        let epsilon = 0.000001;
        return (f1 - f2).abs() < epsilon;
    }

    n1.to_string() == n2.to_string()
}
