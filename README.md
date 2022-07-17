# .emacs.d
> My Emacs config

## Running

Run Emacs in daemon mode:

    $ emacs -daemon

Afterwards I connect to the server with either a terminal or a GUI client like this:

    $ emacsclient -t
    $ emacsclient -c

You'd probably do well to put a few aliases in your .zshrc (or .bashrc):

    alias e='TERM=xterm-256color emacsclient -t'
    alias ec='emacsclient -c'

Also you can open a file with cursor on choosen line:

    $ emacsclient somefile:1234

## Stop emacs daemon

The simplest way to stop the `emacs daemon` from within emacs is to use the `kill-emacs` commands.
From outside of emacs this can be achieved using emacsclient:

    $ emacsclient -e '(kill-emacs)'

I also added an alias in my .bashrc

    alias ek="emacsclient -e '(kill-emacs)'"

This will shutdown the daemon immediately with out prompting or saving files

### Keymap

  * `M-j    `  join line
  * `M-\    `  M-x delete-horizontal-space
  * `<F2>   `  toggle neotree
  * `C-c f J`  reveal-in-osx-finder
  * `C-c j i`  avy-goto-char-in-line
  * `C-c j j`  avy-goto-char
  * `C-c j w`  avy-goto-word-1
  * `C-c j l`  avy-goto-line
  * `C-c j b`  avy-pop-mark
  * `C-c j k`  avy-goto-char-2
  * `C-c j p`  goto-last-change
  * `C-c j n`  goto-last-change-reverse
  * `C-c g g`  counsel-git-grep
  * `C-x p f`  [projectile](https://docs.projectile.mx/projectile/usage.html) find-file
  * `C-c C-<`  mc/mark-all-like-this
  * `C-c C-,`  mc/mark-all-like-this
  * `C-x v g`  vc-annotate, [git annotate](https://stackoverflow.com/questions/15460550/git-blame-with-commit-details-in-emacs)

#### Reformat Buffer

`C-x h C-M-\`

```
C-x h runs the command mark-whole-buffer
C-M-\ runs the command indent-region
```
