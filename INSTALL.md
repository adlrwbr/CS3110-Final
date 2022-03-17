1. If you do not have OCaml and/or Dune, install it on your machine using the CS 3110 textbook installation instructions
   here: https://cs3110.github.io/textbook/chapters/preface/install.html

2. Install the OCaml graphics library on your machine by using the tutorial listed below, which a copied portion from the
   tutorial "Installing OCaml Graphics" under the GitHub respository "OCaml Graphics Demo" by the
   Yale-NUS College module on Data Structures and Algorithms. Link: https://github.com/ysc2229/ocaml-graphics-demo.

---

## Installing OCaml Graphics

If you followed the initially provided instructions for installing the
OCaml infrastructure on your operating system, the project should be
working. Otherwise, follow the instructions below.

This project relies on OCaml's `graphics` package, which is somewhat
non-trivial to install, due to native system dependencies. Some
instructions on how make it work are provided below:

### Mac OS X

1. Install [XQuartz](https://www.xquartz.org/).

2. Log-out from the system and log-in again.

3. After that, you will have to **re-install all packages** you have previously
   installed via opam. The following commands will all be done in the terminal within the project directory.
   For instance, first try

   ```
   opam switch
   ```

   If your version is, for instance `4.10.0`, next run

   ```
   opam switch reinstall 4.10.0
   ```

   It will take a while, as it re-builds all packages from scratch.

   The `graphics` package has been checked to work with OCaml
   `4.06.1`, `4.07.1`, and `4.10.0`.

4. Next, execute

   ```
   opam update; opam upgrade
   opam install graphics
   ```

   After that you should be able to build the project.

5. You might need to install `core`, and `batteries` via `opam` (using
   `opam install .`).

### Linux

1. Make sure that x11 window manager is installed (it comes as a part
   of most of the standard distributions).

2. Proceed to Step 3 for Mac OS X (above).

### Windows (Cygwin)

1. As it seems to be the case, x11 graphic interface is included into
   Cygwin (https://x.cygwin.com/).

2. Proceed to Step 3 for Mac OS X (above).

---

3. Run the system using "make play" in the terminal in the directory of the project.
