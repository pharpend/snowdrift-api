snowdrift-api
=============

[Snowdrift.coop][1] is a cooperative platform to fund free/libre/open
creative work.

This program runs an API server to access data on and/or about
Snowdrift.coop.

This program is experimental and should not be used for any purpose
whatsoever by anybody.

To be consistent with Snowdrift.coop, this project is written in
[Haskell][3], and licensed under the
[GNU Affero General Public License][4].

Installation
------------

At the moment, you have to compile it. I develop on Arch Linux, and test
somewhat regularly on FreeBSD as well.

1.  [Install Haskell][2]

2.  Enter these commands in a terminal:

        git clone git://github.com/pharpend/snowdrift-api.git
        cd snowdrift-api
        cabal sandbox init

3.  Run `ghc --version`. If it is 7.8.something, run

        ln -s cabal.config.7.8 cabal.config

    If it's 7.10.something, run

        ln -s cabal.config.7.10 cabal.config

    For me, it's

        The Glorious Glasgow Haskell Compilation System, version 7.10.1

4.  Once that's done:

        cabal install -j -fdev

Usage
-----

    snowdrift-api v.0.1.0.0
    Written by Peter Harpending.
    Copyright (c) 2015 Snowdrift.coop. See `snowdrift-api --license` for the full license.
    
    OPTIONS
        -h,--help                         Show this page.
        --license                         Print out the license (AGPLv3+).
        --version                         Print out the version.
        -p,--port PORT                    Port on which to run the server (default: 8778).
        -q,--quiet                        Don't output anything to stdout.


Contact
-------

* Email: Peter Harpending <peter@harpending.org>
* IRC: `pharpend` on FreeNode and OFTC

[1]: https://snowdrift.coop
[2]: https://github.com/bitemyapp/learnhaskell/blob/master/install.md
[3]: https://www.haskell.org
[4]: https://www.gnu.org/licenses/
