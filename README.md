[![Build Status](https://travis-ci.org/luismbo/cl-travis.svg?branch=master)](https://travis-ci.org/luismbo/cl-travis)

Overview and Setup
------------------

CL-TRAVIS helps you test your Common Lisp projects with [Travis][1]
and many different Lisp implementations: ABCL, Allegro CL, SBCL,
CMUCL, CCL and ECL.

In addition, it installs Quicklisp as well as [CIM][4], which
means you can conveniently run Lisp code using the `cl` command
and grab dependencies via `ql:quickload`. ASDF is set up to look
for system definitions recursively within your project repository
and within the `~/lisp` directory.

Using it is simple, you don't even have to clone this repository:

1. Grab the the sample [`.travis.yml`][2] file.
2. Modify it to test your own project (the base version tests
   `cl-travis` itself.)
3. Push it to the root of your project's repository and
   [enable Travis][3] from your GitHub account.

Examples
--------

Here's a list of `.travis.yml` files from various projects using CL-TRAVIS:
[CFFI](https://github.com/cffi/cffi/blob/master/.travis.yml),
[SLIME](https://github.com/slime/slime/blob/master/.travis.yml),
[Osicat](https://github.com/osicat/osicat/blob/master/.travis.yml),
[stumpwm](https://github.com/stumpwm/stumpwm/blob/master/.travis.yml),
[Babel](https://github.com/cl-babel/babel/blob/master/.travis.yml),
[trivial-garbage](https://github.com/trivial-garbage/trivial-garbage/blob/master/.travis.yml),
[trivial-features](https://github.com/trivial-features/trivial-features/blob/master/.travis.yml).

[1]: https://travis-ci.org
[2]: https://raw.githubusercontent.com/luismbo/cl-travis/master/.travis.yml
[3]: http://docs.travis-ci.com/user/getting-started/
[4]: https://github.com/KeenS/CIM
