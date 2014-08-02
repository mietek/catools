--------------------------------------------------------------------------------

CA Tools
========

This is a set of tools for interfacing with Cater Allen.

To build:

    cabal sandbox init
    cabal install --dependencies-only
    cabal build

To run:

    export CA_USER_NAME=...
    export CA_SECRET_ACCESS_CODE=...
    export CA_SECRET_PASSWORD=...
    cabal run cascrape -- 2014-07-01 2014-07-31 >2014-07.csv
    cabal run caparse <2014-07.csv | sed 's/,,/, ,/g;s/,,/, ,/g' | column -s, -t

Meta
----

Written by [Miëtek Bak](http://mietek.io).  Say hello@mietek.io

Available under the MIT License.

--------------------------------------------------------------------------------
