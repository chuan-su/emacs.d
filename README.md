# .emacs.d
> My Emacs config

# Running

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


