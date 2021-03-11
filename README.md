[![CI](https://github.com/twlz0ne/helm-pinyin/workflows/CI/badge.svg)](https://github.com/twlz0ne/helm-pinyin/actions?query=workflow%3ACI)

# helm-pinyin

Pinyin support for `helm-find-files` and `helm-mini`.

## Installation

``` elisp
(quelpa '(helm-pinyin :repo "twlz0ne/helm-pinyin" :fetcher github))
```

## Usage

``` elisp
(require 'helm-pinyin)
(turn-on-helm-pinyin)
```

## ⚠️ Caution

The find files/buffers process has been changed quite a bit to optimize the execution speed.

Original:

    (loop ...
          (loop ...
                (generate-and-apply-patterns)))

Replaced with:

    (loop ...
          (generate-patterns)
          (loop ...
                (apply-patterns)))

A numbers of related functions have also been modifield.  This may cause some unexpected problems.
