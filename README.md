# .emacs.d
> My Emacs config

## Running

Run Emacs in daemon mode:

    $ emacs -daemon

Afterwards I connect to the server with either a terminal or a GUI client like this:

    $ emacsclient -t
    $ emacsclient -c

You'd probably do well to put a few aliases in your .zshrc (or .bashrc):

    alias e='emacsclient -t'
    alias ec='emacsclient -c'

Also you can open a file with cursor on choosen line:

    $ emacsclient somefile:1234

## Stop emacs daemon

The simplest way to stop the `emacs daemon` from within emacs is to use the `kill-emacs` commands.
From outside of emacs this can be achieved using emacsclient:

    $ emacsclient -e '(kill-emacs)'

This will shutdown the daemon immediately with out prompting or saving files


### Customized Keymap

  * `M-j    `  join line
  * `M-\    `  M-x delete-horizontal-space
  * `C-c .  `  toggle neotree
  * `C-c f J`  reveal-in-osx-finder
  * `C-j    `  avy-goto-char-in-line
  * `C-c j j`  avy-goto-char
  * `C-c j w`  avy-goto-word-1
  * `C-c j l`  avy-goto-line
  * `C-c j b`  avy-pop-mark
  * `C-c j k`  avy-goto-char-2
  * `C-c j p`  goto-last-change
  * `C-c j n`  goto-last-change-reverse
  * `C-c g g`  counsel-git-grep
  * `C-c C-<`  mc/mark-all-like-this
  * `C-c C-,`  mc/mark-all-like-this

### Emacs as SQL client

To create a MySQL connection start by running `M-x sql-mysql`

You'll be prompted for `username`, `database`, `password` and `host` and then you'll be dropped in a buffer dedicated to the connection you've specified.

While in some `.sql` file execute `M-x sql-set-product` and type `mysql`.

Afterwards do `M-x sql-set-sqli-buffer` and select the name of the connection buffer you want to use (it's probably called `*SQL*` if you have only one connection buffer).

Now you'll be able to use the following commands like from the `.sql` buffer and the code from the region will be executed in the associated connection buffer.

  * `C-r C-n` sql-send-line-and-next
  * `C-c C-r` sql-send-region
  * `C-c C-b` sql-send-buffer
  * `C-c C-s` sql-send-string
  * `C-c C-c` sql-send-paragraph
