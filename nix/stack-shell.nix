# This is the derivation used by "stack --nix".
# It provides the system dependencies required for a stack build.
with import ./. {};

haskell.lib.buildStackProject {
  name = "stack-env";
  ghc = (import ../shell.nix { inherit pkgs; }).ghc.baseGhc;

  buildInputs =
    # Development libraries which may be necessary for the build.
    # TODO: Add remove libraries as necessary
    [ zlib gmp ncurses lzma openssl git systemd.dev ] ++
    # MacOS-specific librararies which may be necessary for the build.
    (lib.optionals stdenv.isDarwin (with darwin.apple_sdk.frameworks; [ Cocoa CoreServices libcxx libiconv ])) ++
    # Add any programs/tools which are required for e.g. running tests.
    (with haskellNixPackages; [ /* TODO: Put programs here */ ]);

  phases = ["nobuildPhase"];
  nobuildPhase = "mkdir -p $out";
}
