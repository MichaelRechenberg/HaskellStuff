# coin-change

This is a project to deal with the following problem:
Given an initial amount of money and denominations of coins, find how many
  possible combinations there are to make change for that initial amount.
Note: Order does not matter...[1,1,5,1] == [5,1,1,1] and is only counted once

This is also a project to get used to more Haskell logistics
  * testing with Hspec
  * modifying .cabal files



Stack/Haskell Review:

  Write tests in coin-change/test
  Write application code in src/CoinChange.hs

  Building/Running
      
      stack build
      stack exec <project-name>-exe
      stack exec coin-change-exe



Setup:

  Install HSpec for test suite
  
      stack install hspec


  Modify .cabal file to expose modules we write

      
      library
        hs-source-dirs:      src
        >>>>>>>>>>
        exposed-modules:     Lib
        <<<<<<<<<<<
        exposed-modules:     Lib, CoinChange
        build-depends:       base >= 4.7 && < 5
        default-language:    Haskell2010

  Modify .cabal file to specify the new file we'll be using as the "main" of the test suite

      test-suite coin-change-test
        type:                exitcode-stdio-1.0
        hs-source-dirs:      test
        exposed-modules:     Lib
        >>>>>>>>>>
        main-is:             Spec.hs
        <<<<<<<<<<<
        main-is:             SomeNewSpecFile.hs
        build-depends:       base
                           , coin-change
        ghc-options:         -threaded -rtsopts -with-rtsopts=-N
        default-language:    Haskell2010


    otherwise, Stack will complain that it can't find the interface for "CoinChange"


  Modify .cabal file to include Hspec in build-depends for test-suite

      test-suite coin-change-test
        type:                exitcode-stdio-1.0
        hs-source-dirs:      test
        main-is:             CoinChangeSpec.hs
        build-depends:       base
                           , coin-change
        >>>>>>>>>>
        --This is the added line
                           , hspec == 2.*
        ghc-options:         -threaded -rtsopts -with-rtsopts=-N

HSpec:
  Designed to be similar to RSpec from Ruby
  Feels similar to ScalaTest's FunSpec style

Debugging:
  https://en.wikibooks.org/wiki/Haskell/Debugging 
  Use `trace` of the Debug.Trace module


Random Ramblings:
  Remember `show` will make data into a String (if the typeclass extends Show)
  Variables seem to have to be lowercase (ALL CAPS means compiler is seeing that symbol as a "data constructor")
  When dealing with lists try to think of the head/tail method of recursion (made my code a lot cleaner
    after looking at the Scala StackOverflow question...no longer having to use that coinChangeHelper shenanigans and
    explicitly selecting the coin, resulting in massive double counting)
