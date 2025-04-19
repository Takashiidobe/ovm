# dev.nix
{ pkgs }:

pkgs.mkShell {
  packages = with pkgs; [
    # Rust Toolchain
    rustc
    cargo
    rust-analyzer

    # Add other necessary packages here
    # e.g., clang for C linking if needed
    # gcc
    # libc # On Linux
  ];
}
