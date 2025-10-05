{
  description = "NSKK.el - Next-generation SKK for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};

        emacsWithPackages = pkgs.emacs.pkgs.withPackages (epkgs: with epkgs; [
          transient
        ]);

        buildInputs = [
          emacsWithPackages
          pkgs.gnumake
          pkgs.git
        ];

        nskkPackage = pkgs.emacsPackages.trivialBuild {
          pname = "nskk";
          version = "1.0.0";

          src = ./.;

          packageRequires = with pkgs.emacsPackages; [
            transient
          ];

          meta = with pkgs.lib; {
            description = "Next-generation SKK for Emacs";
            homepage = "https://github.com/takeokunn/nskk.el";
            license = licenses.gpl3Plus;
            maintainers = [ ];
            platforms = platforms.all;
          };
        };

        emacsWithNskk = pkgs.emacs.pkgs.withPackages (epkgs: with epkgs; [
          transient
          nskkPackage
        ]);

      in
      {
        devShells.default = pkgs.mkShell {
          inherit buildInputs;

          shellHook = ''
            echo "NSKK.el development environment"
            echo "Emacs version: ${pkgs.emacs.version}"
            echo ""
            echo "Available commands:"
            echo "  make test            - Run tests"
            echo "  make coverage        - Generate coverage report"
            echo "  make coverage-report - Open coverage report in browser"
            echo "  make clean           - Clean coverage data"
            echo ""
          '';
        };

        packages.default = nskkPackage;

        apps.default = {
          type = "app";
          program = "${pkgs.writeShellScript "nskk" ''
            exec ${emacsWithNskk}/bin/emacs -q -nw --eval "(require 'nskk)" "$@"
          ''}";
        };
      }
    );
}
