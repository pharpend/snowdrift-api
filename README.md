snowdrift-api
=============

Snowdrift.coop <https://snowdrift.coop> is a cooperative platform to
fund free/libre/open creative work.

This program runs an API server to access data on and/or about
Snowdrift.coop.

This program is experimental and should not be used for any purpose
whatsoever by anybody.

Installation
------------

*   First, install Haskell:
    <https://github.com/bitemyapp/learnhaskell/blob/master/install.md>

*   Next, enter these commands in a terminal:

        git clone git://github.com/pharpend/snowdrift-api.git
        cd snowdrift-api
        cabal sandbox init

*   Run `ghc --version`. If it is 7.8.something, run

        ln -s cabal.config.7.8 cabal.config

    If it's 7.10.something, run

        ln -s cabal.config.7.10 cabal.config

*   Once that's done:

        cabal install -j

Usage
-----

The program doesn't do anything yet, so there is no usage!

Licensing
---------

To be consistent with Snowdrift.coop, this project is written in Haskell
<https://www.haskell.org>, and licensed under the GNU Affero General
Public License <https://www.gnu.org/licenses/>

Contact
-------

* Email: Peter Harpending <peter@harpending.org>
* IRC: `pharpend` on FreeNode and OFTC
