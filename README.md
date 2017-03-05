# .emacs.d
My Emacs config

# Running

Run Emacs in daemon mode:
    
    $ emacs -daemon
    
Afterwards I connect to the server with either a terminal or a GUI client like this:

    $ emacs -t
    $ emacs -c

You'd probably do well to put a few aliases in your .zshrc (or .bashrc):

    alias e='emacsclient -t'
    alias ec='emacsclient -c'

