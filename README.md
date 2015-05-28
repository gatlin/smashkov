smashkov
===

(c) 2015 Gatlin Johnson <gatlin@niltag.net>

0. What?
---

This is a little tool that takes a text file, generates a super simple Markov
model, and generates sentences from the model.

It is very rudimentary but its sole purpose was to generate Markov chains from
Smashmouth lyrics so this isn't my best work.

1. Usage
---

At your shell, assuming you've built it and it's on your path:

    $> smashkov "sample.txt"
    hey i spare some change 
    ---
    smashin' the sharpest tool in the world to arrive and clappin' man what the ground 
    ---
    we could i like it gets colder you're bundled up now you're an 
    ---
    never know if you might as well be walkin' on the crack puts you don't go you'll 
    ---
    close up mr. wizard 
    ---
    alive six to differ judging by the world get bored. somebody once asked could i said yep what the hell 
    ---
    you're still alive six to stay in the with her baby's old enough 
    ---
    broke up so what's 
    ---
    change for gas i 
    ---
    your brain gets dumb so don't sit back kick back and we skate is a 
    ---

2. How to build it
---

1. Install the [Haskell Platform][hp] for your platform. Should be
   straightforward.

2. Clone this repository to, eg, `smashkov/` and `cd smashkov/`

3. Run `sh prep.sh`

4. Run `cabal configure && cabal build`

5. `cabal run` will run the program and, by default, feed it the `sample.txt`
   file.

If you want, you can copy the `smashkov` program out ouf
`dist/build/smashkov/smashkov` and put it in `/usr/bin` or something idgaf.

3. Licensing
---

See `LICENSE` for details.

4. Issues? Questions? Suggestions?
---

Please use the GitHub issue tracker or email me at <gatlin@niltag.net>.

TODO

[hp]: https://www.haskell.org/platform/
