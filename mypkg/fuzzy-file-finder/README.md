# fuzzy-file-finder
+ *This is my first package.*
+ *This is a piggy back package, based on ido and ido-better-flex*
+ *Do not install it. Not that it is unstable. This is utter shit*


Well, the package name says it all. But it'll index only the files in the `default-directory`


## Installation
**(I really don't know why in hell you would do this.)**

You need ido-mode (not a problem in new emacs), and ido-better-flex. You can get them from marmalade or melpa.
If you really want this, In your `~/.emacs` or wherever your config file is in, add the following code. 

```lisp
(require 'fuzzy-file-finder)
```

You can map a key board shortcut to access this crappy functionality very fast.
```lisp
(global-set-key (kbd "C-c C-f") 'fff:fuzzy-find)
```

### PS
Sorry, that's my bad. I should have named this file as DONT_README.md or shouldn't have published this package.