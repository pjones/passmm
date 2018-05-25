# Revision History for passmm

## 0.3.0 (May 25, 2018)

  * Added an optional Helm interface.

    This is different than the `helm-pass` package since it uses Emacs
    Lisp to read pass entries and therefore will use EPA.  This is
    very helpful if you set `epa-pinentry-mode` to loopback (for
    example, if you are using `exwm` and don't want to block Emacs).

## 0.2.0 (January 12, 2017)

  * Added prefix argument to `passmm-kill-password` to show the
    password file after killing the password.

  * Fixed all issues identified by `package-lint`.

## 0.1.0 (January 11, 2017)

  * Initial release.
