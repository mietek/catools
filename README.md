--------------------------------------------------------------------------------

CA Tools
========

To build:

    $ cabal sandbox init
    $ cabal install --dependencies-only
    $ cabal build

To run:

    $ export CA_USERNAME=...
    $ export CA_ACCESS_CODE=...
    $ export CA_PASSWORD=...
    $ cabal run
    Account number:   55555555
    Account name:     EXAMPLE LTD
    Account balance:  £9,999.99

Meta
----

Written by [Miëtek Bak](http://mietek.io).  Say hello@mietek.io

Available under the MIT License.

--------------------------------------------------------------------------------
