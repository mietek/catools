-------------------------------------------------------------------------------

This project is no longer maintained.

-------------------------------------------------------------------------------


# catools

This is a set of tools for interfacing with [Cater Allen][], written in [Haskell][].

* [cascrape](#cascrape) downloads transactions.
* [caparse](#caparse) processes transactions.


### Example session

    $ export CA_USER_NAME=...
    $ export CA_SECRET_ACCESS_CODE=...
    $ export CA_SECRET_PASSWORD=...

    $ cascrape 2014-07-01 2014-07-31 >before.csv
    $ head -n 1 <before.csv
    31Jul2014,"Visa Sales Gh *github.Com    Ih3e    5045 4154486673    Ca 25.00  Us 13/07/14 Fx 1.710   Fee 0.29","",-14.91,9999.99

    $ caparse <before.csv >after.csv
    $ head -n 1 <after.csv
    2014-07-31,2014-07-13,"Visa debit","Gh *github.Com    Ih3e",4154486673,California,-25.00,USD,1.710,0.29,-14.91,9999.99


### Installation

To build using a recent version of [GHC][] and [Cabal][]:

    cabal sandbox init
    cabal install --dependencies-only
    cabal build

To build and run:

    cabal run ${BINARY} -- ${ARGS}

To run:

    ./dist/build/${BINARY}/${BINARY} ${ARGS}

To install:

    cabal install


## cascrape

This tool downloads Cater Allen transactions, in CSV format.


### Usage

    cascrape FROM_DATE TO_DATE

Argument            | Description                           | Format
--------------------|---------------------------------------|-------------------
`FROM_DATE`         | First day of transactions             | `YYYY-MM-DD`
`TO_DATE`           | Last day of transactions              | `YYYY-MM-DD`

Environment variable                    | Description
----------------------------------------|---------------------------------------
`CA_USER_NAME`                          | Cater Allen user name
`CA_SECRET_ACCESS_CODE`                 | Cater Allen personal access code
`CA_SECRET_PASSWORD`                    | Cater Allen internet password


#### Output format

Output field        | Example value
--------------------|-----------------------------------------------------------
Date                | `31Jul2014`
Detail              | `Visa Sales Gh *github.Com    Ih3e    5045 4154486673    Ca 25.00  Us 13/07/14 Fx 1.710   Fee 0.29`
Sub-reference       |
Amount              | `-14.91`
Balance             | `9999.99`


## caparse

This tool processes Cater Allen transactions, in CSV format.


### Usage

    caparse


#### Input format

Same as [cascrape](#cascrape) output format.


#### Output format

Output field        | Example value
--------------------|-----------------------------------------------------------
Date                | `2014-07-31`
Original date       | `2014-07-13`
Type                | `Visa debit`
Party               | `Gh *github.Com    Ih3e`
Reference           | `4154486673`
Territory           | `California`
Original amount     | `-25.00`
Original currency   | `USD`
Conversion rate     | `1.710`
Conversion fee      | `0.29`
Amount              | `-14.91`
Balance             | `9999.99`


## Meta

Written by [Miëtek Bak](http://mietek.io).  Say hello@mietek.io

Available under the BSD License.

--------------------------------------------------------------------------------

[Cater Allen]:      http://www.caterallen.co.uk
[Haskell]:          http://www.haskell.org
[GHC]:              http://www.haskell.org/ghc/
[Cabal]:            http://www.haskell.org/cabal/
[Miëtek Bak]:       http://mietek.io
