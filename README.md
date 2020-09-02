# Handwave

A NES game where you play in a digital handbell choir

Copyright 2020 Mark T. Tomczak, all rights reserved

Source and game released under the MIT license (see attached LICENSE file).

# Introduction

Welcome to the Digital Choir of Our Lady Of The Squared Wave. This choir
supports up to sixteen individuals (though we generally have eight players, each
holding two waves, or even four holding four waves each, if they're comfortable
with so many!).

![Handwave example game, showing notes on a staff to be played](/handwave.png)

To play, pick up your controller (one of the four plugged in) and press one of
two pairs of buttons, either the A or B button, or, the Left or Right
button. Due to budget cutbacks, I'm afraid we only have four controllers; two
players will have to share one controller (or four to a controller if we're each
holding one wave). Pressing the button will sound your wave so you know it's
working. Take note of which color and symbol corresponds to your two waves;
there are four symbols (for the A, B, Left and Right buttons) and four colors
(red for controller 1, green for controller 2, blue for controller 3, and yellow
for controller 4).

Once everyone has sounded the waves they're holding, the choir director (that's
controller 1) can start the song by hitting "Start" on their controller.  Choir
director, you can also reset the held waves by hitting "Select" if someone
accidentally bumps a button, and you can press "down" on your controller to
switch to "network mode" if you're playing in a high-latency environment, such
as a shared game on http://app.kosmi.io (more on that later).

Once the director presses "Start," notes will scroll from right to left. When
a note crosses the symbol corresponding to one of your handwaves, press that button
to sound the wave! If your timing is right, you'll make sweet music.

## network mode

To address the unfortunate reality of latency, our choir also supports "network mode."
In this mode, a dotted-white staff will appear. Sound your notes when they cross
the staff instead of when they cross your handwave, and they'll "lock in" and play
correctly when they reach your handwave. This way, everyone can stay in sync even
in different rooms.

# Development

Handwave was developed using [co2](https://github.com/dustmop/co2) (commit
b407129355496e346ecf35a943eeacd1841199ee), which requires
[racket](https://racket-lang.org/) and
[asm6](http://3dscapture.com/NES/asm6.zip). Once the dependencies are installed,
you'll need to edit the [Makefile](/Makefile) to set the path to your co2
install and the name of the asm6 binary. *Note*: Windows was used as the
development OS, but nothing should be OS-specific (though you'll need to find a
build of asm6 for your OS of choice; [asm6f](https://github.com/freem/asm6f)
might be a decent substitute if you need to build from source, but I have not
tested it). For convenience, you'll likely want an install of `make` to use the
Makefile.

The song is an arrangement of "Korobeiniki," a traditional Russian song that may
be slightly familiar to some old game fans. ;) The music is stored in the
`[first.sng](/first.sng)` file, which is compiled through a small Racket program
in `[song.scm](/song.scm)`. The song language is Racket-derived and described in
`[song-language.md](/song-language.md)`.

You are very much encouraged to try writing your own songs! Changing the
contents of `first.sng` and running `make` should kick out a new ROM with your
song encoded in it (unless it's so long you blow past RAM limits). Play around!

# Acknowledgments

Special thanks to Amanda Leight for sharing her expertise in handbell choirs and
ideas for laying out notes, and for trying out my first prototypes.

Thanks to my regular game group for their patience on my goofy NES hack projects
and excellent feedback on initial drafts.
