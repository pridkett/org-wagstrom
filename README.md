org-wagstrom
============

Patrick Wagstrom &lt;patrick@wagstrom.net&gt;

August 2021

Overview
--------

About two years ago I moved to using org-mode for my daily note taking tasks
instead of Quiver. This was done for a variety of reasons, but I think the
biggest reason was because I was interested in org-roam. As I did more work, I
developed my own flavor of working with org-mode, which probably isn't right,
but works for me.

Useful Functions
----------------

- `org-insert-person`: (**C-c i p**) When taking notes in a meeting, I like to
  add all the participants. This function allows you to add people to a list of
participants and creates a skeleton file for facts about that person if it
doesn't already exist.

- `org-create-meeting`: Creates a new meeting document and adds it to the list
  of all current meetings in `meetings.org`. Meetings are named according to
the timestamp of the meeting.

- `org-insert-image`: (**C-c i i**) Shells out to `pngpaste` to save the
  contents of the current clipboard to a PNG and then inserts a reference to
that image in the current document. Then you can use **C-c C-x C-v** to toggle
whether or not all of the images in the document should be shown.

Installation
------------

Right now, copy the file into your `~/.emacs.d` directory and then add the
following line to your `init.el`:

```elisp
(require 'org-wagstrom)
```

Caveats
-------

This code probably looks like what it would look like if a python programmer
who knew some Lisp from 25 years ago tried to program Emacs Lisp in 2021.

License
-------

Copyright (c) 2021 Patrick Wagstrom - Licensed under terms of the MIT License

