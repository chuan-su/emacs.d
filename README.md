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
    alias vim='emacsclient -t'
    alias vi='emacsclient -t'
    
The last two aliases are helpful if you're used to editing files from the command line using vi(m)

Also you can open a file with cursor on choosen line:

    emacsclient somefile:1234

