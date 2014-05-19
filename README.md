stuttering
==========

Stuttering is a simple language which attempts to parse how I (fail to) express ideas.

An example:

    basically
        umm,
        this is a
        multiline comment.
        yeah...

        a is like 3 plus 17 over 2?
        b is like negative 4 times 5?
        c is like a plus b?
        if 5 minus 1 equals 4
        or c equals 21 over 3
        and 12 is bigger than 2 then basically
            print 1
        so yeah otherwise basically
            print negative 0
        so yeah?

        i is like 10?
        while i is bigger than 0 basically
            print i?
            i is like i minus 1
        so yeah

        um, this is a single line comment.
    so yeah

[Stream of consciousness](http://en.wikipedia.org/wiki/Stream_of_consciousness_(narrative_mode)) style:

    basically a is like 3 plus 17 over 2? b is like negative 4 times 5? c is like a plus b? if 5 minus 1 equals 4 or c equals 21 over 3 and 12 is bigger than 2 then basically print 1 so yeah otherwise basically print negative 0 so yeah? i is like 10? while i is bigger than 0 basically print i? i is like i minus 1 so yeah so yeah

Right now it only deals with numbers, since it was just a quick hack.
I'll hopefully get round to adding in more stuff at some point.

Also, it doesn't actually compile to Java yet, it's literally just a pretty-printer.
I'd like to make the process more involved such that it actually compiles to Java.
This will involve little annoyances like marking up the AST with information
such as types and whether an assignment is the first to that variable.
