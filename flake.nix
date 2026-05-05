{
  description = "NSKK - Next-generation SKK for Emacs";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, nixpkgs }:
    let
      supportedSystems = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
      forAllSystems = nixpkgs.lib.genAttrs supportedSystems;
      pkgsFor = system: nixpkgs.legacyPackages.${system};
    in
    {
      checks = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          emacs = pkgs.emacsPackages.emacsWithPackages (ps: [ ]);
        in
        {
          default = pkgs.runCommand "nskk-check" {
            nativeBuildInputs = [ emacs pkgs.gnumake ];
            src = self;
          } ''
            cp -r $src/. .
            chmod -R u+w .
            make EMACS=${emacs}/bin/emacs compile
            make EMACS=${emacs}/bin/emacs test
            make EMACS=${emacs}/bin/emacs lint
            touch $out
          '';
        });

      devShells = forAllSystems (system:
        let
          pkgs = pkgsFor system;
          emacs = pkgs.emacsPackages.emacsWithPackages (ps: [ ]);
          skkDict = pkgs.skkDictionaries.l;
        in
        {
          default = pkgs.mkShell {
            buildInputs = [
              emacs
              skkDict
            ];
            shellHook = ''
              cat <<'USAGE_EOF'

=== NSKK Development Shell (Debug Mode Enabled) ===

Automated checks:
  nix flake check   # Run all checks (compile + test + lint) in sandbox
  make compile      # Byte-compile all .el files
  make test         # Run ERT test suite
  make lint         # Run checkdoc
  make clean        # Remove *.elc files

Manual testing:
  emacs -Q -L src \
    --eval "(setq nskk-dict-system-dictionary-files '(\"${skkDict}/share/skk/SKK-JISYO.L\"))" \
    --eval "(setq nskk-debug-enabled t)" \
    --eval "(require 'nskk-debug)" \
    --eval "(require 'nskk)" \
    --eval "(nskk-global-mode 1)"

Debug commands in Emacs:
  M-x nskk-debug-toggle     Toggle debug mode
  M-x nskk-debug-show       View *NSKK Debug* buffer
  M-x nskk-debug-clear      Clear debug buffer

AZIK usage:
  (setq nskk-converter-romaji-style 'azik)  ; Enable AZIK input

Key bindings (after nskk-global-mode is enabled):
  C-x C-j          Toggle NSKK mode
  C-j              Commit conversion

Useful commands in Emacs:
  M-x nskk-set-mode-hiragana    Switch to hiragana mode
  M-x nskk-set-mode-katakana    Switch to katakana mode
  M-x nskk-set-mode-latin       Switch to ASCII/latin mode
  M-x nskk-set-mode-jisx0208-latin  Switch to full-width latin mode

USAGE_EOF
            '';
          };
        });
    };
}
