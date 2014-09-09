[![Build Status](https://travis-ci.org/luismbo/cl-travis.png?branch=master)](https://travis-ci.org/luismbo/cl-travis)

Overview and Setup
------------------

CL-TRAVIS helps you test your Common Lisp projects with [Travis][1]
and many different Lisp implementations, such as SBCL, CMUCL, CCL and
ECL.

Using it is simple, you don't even have to clone this repository:

1. Grab the the sample [`.travis.yml`][2] file.
2. Modify it to test your own project (the base version tests
   `cl-travis` itself.)
3. Push it to the root of your project's repository and
   [enable Travis][3] from your GitHub account.

[1]: https://travis-ci.org
[2]: https://raw.githubusercontent.com/luismbo/cl-travis/master/.travis.yml
[3]: http://docs.travis-ci.com/user/getting-started/
