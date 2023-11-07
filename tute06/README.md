---
title: Week 6 Exercises
author: FIT2102 Programming Paradigms
margin: 1inch
---

- [Requirements](#requirements)
- [Setting up Haskell](#setting-up-haskell)
  - [Install GHCup](#install-ghcup)
  - [Install tools using GHCup](#install-tools-using-ghcup)
  - [Test](#test)
  - [Essential VSCode extensions](#essential-vscode-extensions)
  - [Optional VSCode Extensions](#optional-vscode-extensions)
- [Using the code bundle](#using-the-code-bundle)

## Requirements

Complete all the exercises as per the instructions found in the code files. All tests must pass. There must be no compilation warnings or errors.

Recommended order:
1. `IntPair.hs`
2. `BinTree.hs`
3. `Examples.hs`

## Setting up Haskell

### Install GHCup

We will use [GHCup](https://www.haskell.org/ghcup/) to install and manage our tooling.

Start by installing GHCup using the [official installation instructions](https://www.haskell.org/ghcup/install).

- [Windows Instructions](#windows)
- [Non-Windows Instructions](#not-windows)

#### Windows

You may use Chocolatey to install haskell if you wish via `haskell-dev`.

We will use PowerShell and the provided GHCup script for installation.

In the installation process, select the following options:

```
Chocolatey was detected on your system. It is capable of installing the Haskell toolchain as well.
If you want to rather use that instead of ghcup, abort the installation and run the following at an
elevated command prompt:
    choco install haskell-dev
    refreshenv

Continue with GHCup installation?
[C] Continue  [A] Abort  [?] Help (default is "C"): C



Picked C:\ as default Install prefix!
Welcome to Haskell!

This script can download and install the following programs:
  * ghcup - The Haskell toolchain installer
  * ghc   - The Glasgow Haskell Compiler
  * msys2 - A linux-style toolchain environment required for many operations
  * cabal - The Cabal build tool for managing Haskell software
  * stack - (optional) A cross-platform program for developing Haskell projects
  * hls   - (optional) A language server for developers to integrate with their editor/IDE

Please note that ANTIVIRUS may interfere with the installation. If you experience problems, consider
disabling it temporarily.

Where to install to (this should be a short Path, preferably a Drive like 'C:\')?
If you accept this path, binaries will be installed into 'C:\ghcup\bin' and msys2 into 'C:\ghcup\msys64'.
Press enter to accept the default [C:\]: (Enter) or choose your own path. This should be one without special characters or spaces.



Specify Cabal directory (this is where haskell packages end up)
Press enter to accept the default [C:\\cabal]: (Enter) or choose your own path. This should be one without special characters or spaces.



Install HLS
Do you want to install the haskell-language-server (HLS) for development purposes as
well?
[Y] Yes  [N] No  [A] Abort  [?] Help (default is "N"): Y



Install stack
Do you want to install stack as well?
[Y] Yes  [N] No  [A] Abort  [?] Help (default is "N"): Y



Install MSys2
Do you want GHCup to install a default MSys2 toolchain (recommended)?
[Y] Yes  [N] No  [?] Help (default is "Y"): Y
```

#### Not Windows

In the installation process, select the following options:

```
Do you want ghcup to automatically add the required PATH variable to "..."?

[P] Yes, prepend  [A] Yes, append  [N] No  [?] Help (default is "P").

A

Do you want to install haskell-language-server (HLS)?
HLS is a language-server that provides IDE-like functionality
and can integrate with different editors, such as Vim, Emacs, VS Code, Atom, ...
Also see https://haskell-language-server.readthedocs.io/en/stable/

[Y] Yes  [N] No  [?] Help (default is "N").

N

Do you want to enable better integration of stack with GHCup?
This means that stack won't install its own GHC versions, but uses GHCup's.
For more information see:
  https://docs.haskellstack.org/en/stable/yaml_configuration/#ghc-installation-customisation-experimental
If you want to keep stacks vanilla behavior, answer 'No'.

[Y] Yes  [N] No  [?] Help (default is "Y").

Y
```

Run
```bash
$ ghcup --version
```
to check that the installation has succeeded.

### Install tools using GHCup

The tools should be installed automatically, but make sure they are the correct versions.

The versions installed should be as follows:
- Stack: 2.9.3 (recommended)
- HLS: 2.1.0.0 (latest)
- cabal: 3.6.2.0 (recommended)
- GHC: 9.2.8 (recommended)

**If they are different, install the correct versions.**

We will be using ghc-9.2.8 for this unit as it has LTS [^3] in [stack](https://www.stackage.org/) and is supported by [HLS](https://haskell-language-server.readthedocs.io/en/latest/support/ghc-version-support.html)[^4].

[^3]: Long Term Support.

[^4]: Haskell Language Server.

You may have to restart your terminal for PATH changes to take place.

- [Windows Instructions](#windows-1)
- [Non-Windows Instructions](#not-windows-1)

#### Windows

Run
```bash
$ ghcup list
```
and check that Stack, HLS, and cabal have been installed with the correct versions.

Run
```bash
$ ghcup install <tool> <version>
```
to install any missing versions. For example,
```bash
$ ghcup install hls 2.1.0.0
```

#### Not Windows

Run
```bash
$ ghcup tui
```
and check that Stack, HLS, and cabal have been installed with the correct versions.

Follow the instructions of the GUI to install any missing versions.

### Test

Open the code bundle and ensure stack works by running
```bash
$ stack ghc -- --version
The Glorious Glasgow Haskell Compilation System, version 9.2.8
```

This may download any missing dependencies.

### Essential VSCode extensions

[Haskell](https://marketplace.visualstudio.com/items?itemName=haskell.haskell)
- Must have
- Haskell language support including language server, syntax highlighting, and linting

[Haskell Syntax Highlighting](https://marketplace.visualstudio.com/items?itemName=justusadam.language-haskell)
- Useful
- Adds syntax highlighting for Haskell associated languages
- Adds automatic indentation

### Optional VSCode Extensions

These may take a while to setup, so it is worth getting started on your applied session work!

[Ormolu](https://marketplace.visualstudio.com/items?itemName=sjurmillidahl.ormolu-vscode)
- Very Useful
- Haskell formatter
- Requires installing [ormolu](https://github.com/tweag/ormolu) separately
  - This can be done via `stack install ormolu`

[hlint](https://marketplace.visualstudio.com/items?itemName=lunaryorn.hlint)
- Useful
- Additional linting options and configuration
- Requires building [hlint](https://github.com/ndmitchell/hlint) separately
  - This can be done via `stack build hlint`

## Using the code bundle

Once you have downloaded and extracted the code, set it up by running:
```bash
$ stack setup
```
- You should only have to do this once throughout the semester

Build your code files by executing
```bash
$ stack build
```

Run all the tests by executing
```bash
$ stack test
```
- This may take a while to run the first time

To run, debug, or test the code, enter Haskell's interactive environment,
GHCi[^6]:
```bash
$ stack ghci
```

It should load all the source files in the current project. If you
modify your code and want to test it, you need to `:load/:l` or `:reload/:r`
your code, the former will load the file you indicate, while the latter will
reload the whole project:

[^6]: GHCi is a REPL (Read-Eval-Print-Loop), which means that whatever code you
    type in it is interpreted as if it were source code; loading a file is
    equivalent to copy/pasting it.

```bash
*Main Lib> :l src/BinTree.hs
[1 of 1] Compiling BinTree           ( src/BinTree.hs, interpreted )
Ok, modules loaded: BinTree
*BinTree> :r
[1 of 1] Compiling BinTree           ( src/BinTree.hs, interpreted )
Ok, modules loaded: BinTree.
```

The interactive environment gives you a number of tools to explore the code,
typing `:type/:t` will give you the type of a function; to have more
information, you can use `:info/:i`.

```bash
*Maybes> :t mapTree
mapTree :: (Int -> Int) -> BinTree -> BinTree
*Maybes> :i mapTree
mapTree :: (Int -> Int) -> BinTree -> BinTree
  	-- Defined at src/BinTree.hs:82:1
```

Using GHCi makes debugging faster as each time you reload your file(s) it will
inform you of any syntax or type error.

You can also type code directly within GHCi and use its results either by
copy/pasting into your code, or using `it` to recall the last results.

```bash
Prelude> map (+1) [1..10]
[2,3,4,5,6,7,8,9,10,11]
Prelude> map (*2) it
[4,6,8,10,12,14,16,18,20,22]
```

Once you have finished writing the code for a file, you can test it using:

```bash
$ stack exec doctest src/<file>.hs
```
