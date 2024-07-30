# Unmaintained

I recommend using the [pass](https://github.com/NicolasPetton/pass) package instead.

# Passmm: A minor mode for pass (Password Store).

This is a minor mode that uses ‘dired’ to display all password
files from the password store.  It supports the following features:

  * Generate new passwords, storing them in the current ‘dired’
    subdir (or optionally prompting for a directory).  (See:
    ‘passmm-generate-password’.)

  * Store the password of a file into the Emacs kill ring and the
    system clipboard for N seconds.  (See:
    ‘passmm-kill-password’.)

  * Edit a password file with narrowing so the password isn’t
    show.  (See: ‘passmm-dired-action’.)

Typically you’ll want to start passmm by calling
‘passmm-list-passwords’.
