use std::fs::File;
use std::io::Write;
use std::process::{Command, Stdio};

const ASM_PATH: &str = "test_output.S";

fn setup(ret_val: i32) {
    let output = Command::new("cargo")
        .args(["run", "--", &ret_val.to_string()])
        .stdout(Stdio::piped())
        .output()
        .expect("Failed to run compiler");

    assert!(output.status.success(), "Compiler execution failed");

    let asm_content = String::from_utf8(output.stdout).expect("Invalid UTF-8 in assembly output");
    let mut file = File::create(ASM_PATH).expect("Failed to create assembly file");
    file.write_all(asm_content.as_bytes())
        .expect("Failed to write assembly to file");

    let compile_status = Command::new("cc")
        .args(["-static", "-o", "test_executable", ASM_PATH])
        .status()
        .expect("Failed to compile assembly");

    assert!(
        compile_status.success(),
        "Failed to compile assembly with cc"
    );
}

fn cleanup() {
    std::fs::remove_file(ASM_PATH).ok();
    std::fs::remove_file("test_executable").ok();
}

#[test]
fn test_compile_32() {
    let input: i32 = 32;
    setup(input);

    // Run the compiled executable and check exit code
    let run_output = Command::new("./test_executable")
        .status()
        .expect("Failed to run executable");

    let exit_code = run_output.code().expect("No exit code available");
    assert_eq!(
        exit_code, input,
        "Exit code does not match expected value ({input})"
    );

    cleanup();
}
