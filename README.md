<p align="center">
	<img src="./doc/img/spade_logo.svg" alt="Spade logo"/>
</p>

Ever wanted to do something crazy with command blocks but the notation seemed too horrendous?
Ever wanted to make an adventure map which reacts to events?
Ever wanted to write code which uses both Minecraft commands and a server plugin?

If so, Spade may help!
Spade is an approachable programming language which can compile down to both mcfunctions, and Java, making it much easier to take total and unquestionable command of all parts of your Minecraft world.

## Features

- A simple, unified syntax with similarities to other common languages
- Syntactic sugaring to allow useful tasks to be used by the programmer with minimal effort
- A strict type-system to catch errors before runtime and _without_ affecting efficiency (better than Java)
- A single language which can be run both on the server and client sides
- A compiler that can run on any common operating system (Windows, MacOS and Linux)

## Installation

### Download (_Super Easy_)

We maintain [installers][releases] for Windows, MacOS and Linux on our releases page.

### Build from Scratch (_Hard, just for development_)

If you'd like to build directly from the source code, you will need a number of smaller tools.
Whip up the terminal, and make sure the following are installed:

- [`git`][git] distributed version-control system
- [`stack`][stack] build tool for Haskell
- [`make`][make] GNU generic build system
- [`alex`][alex] Haskell lexical analyser generator
- [`happy`][happy] Haskell parser generator
- [`m4`][m4] GNU macro language

Then, download our code and compile it:

```bash
git clone https://github.com/jf908/spade
cd spade
make
```

Once this succeeds, the compiler can be run:

```bash
./dig
```

Use `./dig --help` to get more help.

To install, run:
```bash
make install
```

Et voilà!
One (custom) version of `dig`.

### License

Spade and Dig are developed under the MIT license.

### Authors

Ed and Josh have been playing Minecraft since 2012 and probably need to go outside more.
We are both Computer Scientists whose specialisms include formal languages and systems development, compiler design and programming language principles.

_We are not affiliated with Mojang AB._

[releases]: https://github.com/jf908/Spade/releases
[git]: https://git-scm.com
[stack]: https://docs.haskellstack.org/en/stable/README/
[make]: https://www.gnu.org/software/make/
[alex]: https://www.haskell.org/alex/
[happy]: https://www.haskell.org/happy/
[m4]: https://www.gnu.org/software/m4/
