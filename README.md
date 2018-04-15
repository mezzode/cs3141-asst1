Firstly, ensure that GHC has been setup for this project by typing, in the directory that
contains the assignment code:

    stack setup

If stack reports that it has already set up GHC, you should be able to build the assignment with:

    stack build

This build command will, on first run, download and build the library dependencies
as well, so be sure to have an internet connection active. To run the program from
‘Main.hs’, which saves an image tortoise.png, type:

    stack exec Tortoise

To run the program from ‘Tests.hs’, which contains all the QuickCheck properties,
type:

    stack exec TortoiseTests

To start a ghci session for the Tortoise program, type:

    stack repl Tortoise:exe:Tortoise

Similarly, stack repl can be used with TortoiseTests:

    stack repl Tortoise:exe:TortoiseTests

Lastly, if for whatever reason you want to remove all build artefacts, type:

    stack clean
