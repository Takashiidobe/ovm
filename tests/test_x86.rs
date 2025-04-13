use std::fs::File;
use std::io::Write;
use std::process::{Command, Stdio};

struct TestCompiler {
    asm_path: String,
    executable_path: String,
}

impl TestCompiler {
    fn new(test_name: &str, expression: &str) -> Self {
        let asm_path = format!("test_{}.S", test_name);
        let executable_path = format!("test_{}_executable", test_name);

        // Run the compiler
        let output = Command::new("cargo")
            .args(["run", "--", expression])
            .stdout(Stdio::piped())
            .output()
            .expect("Failed to run compiler");

        assert!(output.status.success(), "Compiler execution failed");

        // Write assembly to file
        let asm_content =
            String::from_utf8(output.stdout).expect("Invalid UTF-8 in assembly output");
        let mut file = File::create(&asm_path).expect("Failed to create assembly file");
        file.write_all(asm_content.as_bytes())
            .expect("Failed to write assembly to file");

        // Compile with cc
        let compile_status = Command::new("cc")
            .args(["-static", "-o", &executable_path, &asm_path])
            .status()
            .expect("Failed to compile assembly");

        assert!(
            compile_status.success(),
            "Failed to compile assembly with cc"
        );

        Self {
            asm_path,
            executable_path,
        }
    }

    fn run_and_check_exit_code(&self, expected: i32) {
        // Run the compiled executable and check exit code
        let run_output = Command::new(format!("./{}", self.executable_path))
            .status()
            .expect("Failed to run executable");

        let exit_code = run_output.code().expect("No exit code available");
        assert_eq!(
            exit_code, expected,
            "Exit code does not match expected value ({})",
            expected
        );
    }
}

impl Drop for TestCompiler {
    fn drop(&mut self) {
        std::fs::remove_file(&self.asm_path).ok();
        std::fs::remove_file(&self.executable_path).ok();
    }
}

#[test]
fn test_simple_number() {
    let input = 32;
    let compiler = TestCompiler::new("simple_number", &input.to_string());
    compiler.run_and_check_exit_code(input);
}

#[test]
fn test_expression() {
    let expression = "5+20-4";
    let expected_result = 21;

    let compiler = TestCompiler::new("expression", expression);
    compiler.run_and_check_exit_code(expected_result);
}

