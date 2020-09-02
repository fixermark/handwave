# Song format, loose description

Songs are lists of the following form:

`(song (initial-assignments) cmd-a cmd-b cmd-c ...)`

`(initial-assignments)` is a starting (assign...) command that lists all 16 notes. It is an error to list less than 16

cmd-a, cmd-b, cmd-c etc. are commands as follows:

* (assign 0 c4 1 c#4 2 d4 3 d#4 ...)

Assignment of notes to handwaves. Assignments are pairs of the form idx,value
where idx is a handwave from 0 to 15 (0 is the "highest" handwave in the list) and
value is a note representation (name-SharpOrFlat-octave, with "middle C" as c4).

* [note id]

A bare note name, such as `c4`, or a handwave index, such as `3`, plays that
note by itself. It is an error to reference a note name that is not currently
assigned a handwave index.

* (chord note-id-1, note-id-2, note-id-3)

Play note-id-1, note-id-2, note-id-3 at the same time as a chord

* rest N

Play nothing for N beats
