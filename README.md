# lentil
A pulsing line highlight minor mode for GNU Emacs.

## What?

It's a simple minor mode for GNU Emacs, intended primarily as an alternative to 'hl-line'. By default it gives a quick pulse to the *logical* line point is on. The region that is pulsed can be customized, as can whether the pulse happens on a particular line at all. See the 'Lentil' customize group for more details, or read the source code.

## How?

Using the ```pulse.el``` library that ships with Emacs.

### Deciding when the pulse happens

As an example of a function you could add to ```lentil-inhibit-pulse-functions```, the following function detects whether point is on an Outline mode heading, including whether it is on text hidden by Outline mode visibility cycling.

```
(defun my/on-outline-heading-p ()
  "An example function for `lentil-inhibit-pulse-functions'.
This function returns non-nil if point is on an Outline mode
heading."
  (and (fboundp #'outline-on-heading-p)
       (or (outline-on-heading-p)
           (save-excursion
             (and (not (or (bobp)(backward-char)))
                  (outline-invisible-p))))))
```
If you add this to ```lentil-inhibit-pulse-functions``` as follows, the highlight will be static whenever point is on an Outline heading line:
```
(add-to-list 'lentil-inhibit-pulse-functions
             '(my/on-outline-heading-p . freeze))
```


## Why?

There are other ways, perhaps simpler, and arguably better, to achieve the effect that this minor mode provides. Nor can Lentil be said to be at all necessary, profoundly interesting, or in any way innovative. It was written purely with an auto-didactic purpose, in other words, to teach myself some Emacs Lisp beyond the basics I already knew. I was not even put off when I found that ```pulse.el``` already contains a ```pulse-momentary-highlight-one-line``` function, which has a hook to fire it. Undoubtedly using that function or its hook would be a far simpler way to do more-or-less what Lentil is primarily designed to do. However, that function highlights the current *screen* line, and I decided, arbitrarily, that I wanted to highlight the current *logical* line, and, if that was split over more than a single screen line, I wanted the pulse to trigger once only. I had a vague memory of seeing a pulse.el based minor-mode for Emacs (which I later realised must have been Prot's Pulsar package) and I thought that Emacs having a built-in pulse library was quite interesting. The pulse.el library is pretty small and easy enough to understand and seemed like a good choice as the basis for writing my own basic minor mode.

The first version of what would become Lentil, unplanned and hastily written, was pretty bad. I intend to put a heavily commented version of that earlier code into a branch of this repo, so that others might be able to gain some benefit and/or amusement from it. It may be that those more experienced in Emacs Lisp would say that the code as it is now remains pretty bad, but, whether-or-no, I've tried to make it readable and understandable. Again, this is in the hope that it can provide a helpful example to others who are trying to get to grips with Emacs as a system, and Emacs Lisp as a language. Also, having written it, I find I actually really do quite like the effect, and I do like that it highlights the logical rather than the visual lines. In that sense at least, it was worth the effort.

## Who?

Just some guy. I used Emacs for years. Then stopped, and switched to VS Code for a while. Then I came back to Emacs last year. Primarily to use Org mode again, but also because I'd heard about the amazing native compilation idea, and the already significant gains in performance that had been made even in its early stages. Also, Emacs seemed to have gained some development impetus, which it had badly needed, I think. I decided that, with more general programming experience under my belt, I would like to increase my knowledge of Emacs Lisp and learn more about how Emacs works at a code level.

## When?

Earlier.

## Why Lentil, though?

A lentil is a kind of pulse.
