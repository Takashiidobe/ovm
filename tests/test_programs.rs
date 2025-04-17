use assert_cmd::cargo::CommandCargoExt;
use insta::{assert_yaml_snapshot, glob};
use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize)]
struct TestOutput {
    status: i32,
    stdout: Vec<String>,
    stderr: Vec<String>,
}

#[test]
fn reference_files() {
    use std::fs::{read_to_string, write};
    use std::path::PathBuf;
    use std::process::{Command, Stdio};
    use tempfile::tempdir;

    glob!("./programs", "**/*.ovm", |path| {
        let file_stem = path.file_stem().unwrap().to_string_lossy();
        let test_name = file_stem.to_string();
        let input = read_to_string(&path).expect("Path doesn't exist");

        // Create a temporary directory for the test
        let temp_dir = tempdir().expect("Failed to create temp dir");
        let temp_path = temp_dir.path();

        // File paths inside the temp directory
        let asm_path: PathBuf = temp_path.join(format!("{test_name}.S"));
        let bin_path: PathBuf = temp_path.join(format!("{test_name}.out"));

        // Run compiler to get assembly
        let compiler_output = Command::cargo_bin(env!("CARGO_PKG_NAME"))
            .unwrap()
            .arg(&input)
            .output()
            .expect("Failed to run compiler");

        assert!(
            compiler_output.status.success(),
            "Compiler error: {}",
            String::from_utf8_lossy(&compiler_output.stderr)
        );

        // Write emitted assembly to file
        write(&asm_path, &compiler_output.stdout).expect("Failed to write .S file");

        // Compile .S to binary using cc
        let compile_status = Command::new("cc")
            .arg(&asm_path)
            .arg("-o")
            .arg(&bin_path)
            .status()
            .expect("Failed to compile assembly");

        assert!(
            compile_status.success(),
            "Assembly compilation failed for {:?}",
            asm_path
        );

        // Run the compiled binary and capture output
        let run_output = Command::new(&bin_path)
            .current_dir(&temp_path)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .output()
            .expect("Failed to run compiled output");

        let test_output = TestOutput {
            status: run_output.status.code().unwrap_or(-1),
            stdout: String::from_utf8_lossy(&run_output.stdout)
                .lines()
                .map(|x| x.to_owned())
                .collect(),
            stderr: String::from_utf8_lossy(&run_output.stderr)
                .lines()
                .map(|x| x.to_owned())
                .collect(),
        };

        assert_yaml_snapshot!(test_output);
    });
}
