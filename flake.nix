{
  inputs = {
    fenix = {
      url = "github:nix-community/fenix/monthly";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    nixpkgs.url = "nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, fenix, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        f = fenix.packages.${system};
        tc = f.complete.toolchain;
        llvm = pkgs.llvmPackages_12;
      in {
        devShells.default = pkgs.mkShell {
          buildInputs = [ tc f.rust-analyzer llvm.llvm llvm.clang llvm.libclang pkgs.libffi pkgs.libxml2 ];
          LIBCLANG_PATH = "${llvm.libclang.lib}/lib/";
          LD_LIBRARY_PATH = "${with pkgs; lib.makeLibraryPath [stdenv.cc.cc.lib libffi libz ncurses]}";
        };
      });
}
