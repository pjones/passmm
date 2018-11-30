Release History for `passmm`
============================

0.4.1 (November 30, 2018)
-------------------------

  * New function `passmm-get-password` so you can read passwords from
    lisp and respect `epa-pinentry-mode`.

0.4.0 (November 28, 2018)
-------------------------

  * Correctly insert sub-directories when using RET in a dired buffer

  * Add an interface for Ivy

0.3.1 (May 25, 2018)
--------------------

  * Properly load dependencies.

0.3.0 (May 25, 2018)
--------------------

  * Added an optional Helm interface.

    This is different than the `helm-pass` package since it uses Emacs
    Lisp to read pass entries and therefore will use EPA.  This is
    very helpful if you set `epa-pinentry-mode` to loopback (for
    example, if you are using `exwm` and don't want to block Emacs).

0.2.0 (January 12, 2017)
------------------------

  * Added prefix argument to `passmm-kill-password` to show the
    password file after killing the password.

  * Fixed all issues identified by `package-lint`.

0.1.0 (January 11, 2017)
------------------------

  * Initial release.
