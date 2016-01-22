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

    $> smashkov -i "input.txt" --min 50 --max 100
    is a cool place i showed them away from you. you follow there is
    self-sacrifice for whom there may be there may be there is a mama's
    handkerchief is the hopeless swamps of life ain't no rational morality is
    up, because fashion it's just to the mystical or the question "to be walkin'
    on strike 

2. How to build it
---

1. Install the [Haskell Platform][hp] for your platform. Should be
   straightforward.

2. Clone this repository to, eg, `smashkov/` and `cd smashkov/`

3. Run `sh prep.sh`

4. Run `cabal configure && cabal build`

5. `cabal run` will run the program and, by default, feed it the `input.txt`
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
