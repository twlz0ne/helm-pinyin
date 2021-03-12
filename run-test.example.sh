#!/usr/bin/env bash
#
# Run tests on multiple versions of emacs
#
# Usage:
#
#     cp ./run-test.{example,}.sh # And edit ‘emacs_bins’
#     ./run-test.sh <ert-testcase-name>
#
set -e

emacs_bins=(
    ~/.local/bin/emacs
    /Applications/Emacs-27.1.app/Contents/MacOS/Emacs
    /Applications/Emacs-26.3.app/Contents/MacOS/Emacs
    /Applications/Emacs-26.1.app/Contents/MacOS/Emacs
    /Applications/Emacs-25.3.app/Contents/MacOS/Emacs
    /Applications/Emacs-25.2.app/Contents/MacOS/Emacs
    /Applications/Emacs-25.1.app/Contents/MacOS/Emacs
    /Applications/Emacs-24.5.app/Contents/MacOS/Emacs
    /Applications/Emacs-24.4.app/Contents/MacOS/Emacs
)

for bin in "${emacs_bins[@]}"; do
    ver=$($bin --batch --eval '(princ (format "%s.%s" emacs-major-version emacs-minor-version))')
    echo "==> $ver: $bin"
    export EMACS=$bin
    test -d .cask/$ver || cask install
    cask build
    cask exec ert-runner --reporter ert+duration
    cask exec ecukes --win
done

cask clean-elc
