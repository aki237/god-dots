# My precious .emacs.d

Seriously make your own.... You know ... You may end up cursing my typing methods....

# Setup
Usual git clone will do the trick....
```shell
git clone https://github.com/aki237/gods-dots.git ~/.emacs.d/
cd .emacs.d
git submodule init
git submodule update
```

After this in your .emacs file add the following :

```elisp
;; NOTHING HAPPENS HERE....    <- this is a comment.... No need to add this.....
(load-file "~/.emacs.d/config.el")
;; SEE THIS ^^^^^^^^^^^^^^^^^^^^ file <- This too.
```
or
Run
``` shell
$ ~/.emacs.d/install.sh
```


# PS
After setup if have any problems... delete both .emacs and .emacs.d and
**start from scratch....**
