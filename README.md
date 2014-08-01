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
    $ cabal run -- 2014-07-01 2014-07-31
    Account number:  55555555
    Account name:    EXAMPLE LTD
    Account balance: 9999.99
    31Jul2014,"Example reference","Example detail",0.00,9999.99
    ...
    01Jul2014,"Example reference","Example detail",0.00,9999.99


Meta
----

Written by [Miëtek Bak](http://mietek.io).  Say hello@mietek.io

Available under the MIT License.

--------------------------------------------------------------------------------
