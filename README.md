# `sticky-scroll-mode`

Sticky scroll mode is a minor mode to enable sticky scrolling in any buffer, language agnostic.
If you'd prefer not to have a live sticky scroll window, and would rather have a temporary popup, you can use
`sticky-scroll-popup` to briefly show the outer, offscreen context.


## Example
![sticky-mode-scroll](https://github.com/user-attachments/assets/43bf0c34-e7e5-4c64-b35d-6dc7eed98eab)

### Sticky scroll window

The package exposes the `sticky-scroll--max-window-height` which can be thought of as "the max number of outer contexts shown". If
the sticky scroll buffer has more lines than `sticky-scroll--max-window-height`, only the last `sticky-scroll--max-window-height` are shown.
Otherwise, the sticky window shows all lines.

#### Example
An outer context of:
```
A {
  B {
    C {
```
with a `sticky-scroll--max-window-height` of 2 will show:

```
  B {
    C {
```

### `sticky-scroll-popup`

Pop up the sticky window until you do another command (moving, typing, etc.)

Compatible with `C-u N`, it will invoke the window as if `sticky-scroll--max-window-height` is set to `N`.


## Approach 

Uses an indentation approach, to find offscreen lines that are levels of indentation lower than the current point.

This works surprising well and quickly.


## Known bugs

The sticky window disappears if you move to a 0-indent line that is within some indentation scope. Working on it.
