# template-overlays

Emacs [overlays](https://www.gnu.org/software/emacs/manual/html_node/elisp/Overlays.html) for template systems

![Screenshot](https://bitbucket.org/mmontone/template-overlays/raw/8b62abafa48cf72b54d11884717f9969a95b07ce/template-overlays.gif "Screenshot")

## Install

```
(require 'template-overlays)

(add-hook 'web-mode-hook 'template-overlays-mode)
```

## Custom delimiters

Add delimiters to `template-overlays-delimiters`. Syntax is `(delim-from delim-to &rest options)`.

```
;; Custom template overlay delimiters
(setcdr (last template-overlays-delimiters)
        '(("<%=" "%>" face (:box t))
          ("<%" "%>" face (:weight bold))
          ("{_" "_}" face (:slant italic))))
```
