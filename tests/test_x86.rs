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
            .args(["-o", &executable_path, &asm_path])
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

    fn stdout(&self, expected: &str) {
        // Run the compiled executable and check exit code
        let output = Command::new(format!("./{}", self.executable_path))
            .output()
            .unwrap()
            .stdout;

        let stdout = String::from_utf8_lossy(&output);

        let trimmed = stdout.trim();

        assert_eq!(
            trimmed, expected,
            "Stdout does not match expected value ({})",
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
    let input = "print 21;";
    let expected_result = "21";
    let compiler = TestCompiler::new("simple_number", input);
    compiler.stdout(expected_result);
}

#[test]
fn test_expression() {
    let expression = "print 5+20-4;";
    let expected_result = "21";

    let compiler = TestCompiler::new("expression", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_multiplication() {
    let expression = "print 5*4;";
    let expected_result = "20";

    let compiler = TestCompiler::new("multiplication", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_division() {
    let expression = "print 20/4;";
    let expected_result = "5";

    let compiler = TestCompiler::new("division", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_mixed_operations() {
    let expression = "print 2+10*3;";
    let expected_result = "32"; // 2+(10*3) = 32 with proper operator precedence

    let compiler = TestCompiler::new("mixed_operations", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_complex_expression() {
    let expression = "print 20/4*2+3;";
    let expected_result = "13"; // (20/4)*2+3 = 5*2+3 = 10+3 = 13

    let compiler = TestCompiler::new("complex_expression", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_parentheses_basic() {
    let expression = "print (2+3)*4;";
    let expected_result = "20"; // (2+3)*4 = 5*4 = 20 with parentheses

    let compiler = TestCompiler::new("parentheses_basic", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_parentheses_precedence_override() {
    let expression = "print 2+3*4;";
    let expected_result = "14"; // 2+3*4 = 2+12 = 14 without parentheses

    let compiler = TestCompiler::new("precedence_without_parens", expression);
    compiler.stdout(expected_result);

    let expression = "print (2+3)*4;";
    let expected_result = "20"; // (2+3)*4 = 5*4 = 20 with parentheses

    let compiler = TestCompiler::new("precedence_with_parens", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_nested_parentheses() {
    let expression = "print 2*(3+(4-1));";
    let expected_result = "12"; // 2*(3+(4-1)) = 2*(3+3) = 2*6 = 12

    let compiler = TestCompiler::new("nested_parentheses", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_complex_parentheses() {
    let expression = "print (10-5)*(2+2)/2;";
    let expected_result = "10"; // (10-5)*(2+2)/2 = 5*4/2 = 20/2 = 10

    let compiler = TestCompiler::new("complex_parentheses", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_multiple_statements_simple() {
    let expression = "1;2;print 3;";
    let expected_result = "3"; // Last statement value is returned

    let compiler = TestCompiler::new("multiple_statements_simple", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_multiple_statements_complex() {
    let expression = "(1+2);3;(20*30*(20));print 4;";
    let expected_result = "4"; // Last statement value is returned

    let compiler = TestCompiler::new("multiple_statements_complex", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_multiple_statements_with_operations() {
    let expression = "6/2;20;1;print 1+2;";
    let expected_result = "3"; // Last statement value is returned

    let compiler = TestCompiler::new("multiple_statements_operations", expression);
    compiler.stdout(expected_result);
}

#[test]
fn test_relational_ops() {
    let expression = "print 1 < 2; print 1 > 2;";
    let expected_result = ["1", "0"].join("\n"); // Last statement value is returned

    let compiler = TestCompiler::new("multiple_statements_operations", expression);
    compiler.stdout(&expected_result);
}
