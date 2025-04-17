use std::fs::File;
use std::io::Write;
use std::process::{Command, Stdio};

test_codegen!(simple_number, "print 21;", ["21"]);
test_codegen!(program, "print 5+20-4;", ["21"]);
test_codegen!(multiplication, "print 5*4;", ["20"]);
test_codegen!(division, "print 20/4;", ["5"]);
test_codegen!(mixed_operations, "print 2+10*3;", ["32"]);
test_codegen!(complex_program, "print 20/4*2+3;", ["13"]);
test_codegen!(parens1, "print (2+3)*4;", ["20"]);
test_codegen!(parens2, "print 2+(3*4);", ["14"]);
test_codegen!(nested_parentheses, "print 2*(3+(4-1));", ["12"]);
test_codegen!(complex_parentheses, "print (10-5)*(2+2)/2;", ["10"]);
test_codegen!(multiple_statements_simple, "1;2;print 3;", ["3"]);
test_codegen!(
    multiple_statements_complex,
    "(1+2);3;(20*30*(20));print 4;",
    ["4"]
);
test_codegen!(
    multiple_statements_with_operations,
    "6/2;20;1;print 1+2;",
    ["3"]
);
test_codegen!(relational_ops, "print 1 < 2; print 1 > 2;", ["1", "0"]);
test_codegen!(
    test_conds,
    "var x = 5; if (x > 10) { print x; } else { print 20; } print x;",
    ["20", "5"]
);

struct TestCompiler {
    asm_path: String,
    executable_path: String,
}

impl TestCompiler {
    fn new(test_name: &str, program: &str) -> Self {
        let asm_path = format!("test_{}.S", test_name);
        let executable_path = format!("test_{}_executable", test_name);

        // Run the compiler
        let output = Command::new("cargo")
            .args(["run", "--", program])
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

    fn stdout(&self, expected: &[&str]) {
        let output = Command::new(format!("./{}", self.executable_path))
            .output()
            .unwrap()
            .stdout;

        let stdout = String::from_utf8_lossy(&output);
        let trimmed = stdout.trim();
        let fixed = expected.join("\n");

        assert_eq!(
            trimmed, fixed,
            "Stdout does not match expected value ({fixed})",
        );
    }
}

impl Drop for TestCompiler {
    fn drop(&mut self) {
        std::fs::remove_file(&self.asm_path).ok();
        std::fs::remove_file(&self.executable_path).ok();
    }
}

macro_rules! test_codegen {
    ($name:ident, $input:expr, [$($expected:expr),* $(,)?]) => {
        #[test]
        fn $name() {
            let input = $input;
            let expected_result = [$($expected),*];
            let compiler = TestCompiler::new(stringify!($name), input);
            compiler.stdout(&expected_result);
        }
    };
}
