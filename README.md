# `sticky-scroll-mode`

Sticky scroll mode is a minor mode to enable sticky scrolling in any buffer that has an active `treesit` parser.
If you'd prefer not to have a live sticky scroll window, and would rather have a temporary popup, you can use
`sticky-toggle` to briefly show the outer, offscreen context.

## Language support

#### This section is subject to frequent change, and I'm considering nuking it all together and going with an indentation-based approach.

### Approach 
The `sticky-scroll--language-alist` controls which braces are associated with which parser, allowing you 
to customize the level of sticky scrolling you'd like.

The alist is `'(lang . (<BRACE ALIST>)`, where `<BRACE ALIST>` is a cons cell of opening braces to closing braces.

The out of the box configuration for `go`, for instance, is represented by 

``` emacs-lisp
'(go . (("{" . "}") ("(" . ")")))
```
which means that offscreen contexts beginning with `{` or `(` are shown in the `sticky-window`, presuming that the cursor is not past its corresponding closing brace
(`}` and `)` respectively).

You can specify more or fewer contexts by customizing this alist.

### Sticky scroll window

The package exposes the `sticky-scroll--max-window-height` which can be thought of as "the max number of outer contexts shown". If
the sticky scroll buffer has more lines than `sticky-scroll--max-window-height`, only the last `sticky-scroll--max-window-height` are shown.
Otherwise, the sticky window shows all lines.