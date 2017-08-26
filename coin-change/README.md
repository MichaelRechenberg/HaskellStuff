# coin-change

This is a project to solve the following problem in Haskell
Given an initial amount of money and denominations of coins, find how many
  possible combinations there are to make change for that initial amount.
Note: Order does not matter...[1,1,5,1] == [5,1,1,1] and is only counted once

This is also a project to get used to more Haskell logistics
  * testing with Hspec
  * modifying .cabal files
  * using third-party packages


## Main Lessons Learned
 -----------------------------------------------------------------------------------------------------
 When dealing with lists try to think of the head/tail method of recursion (made my code a lot cleaner
  after looking at the Scala StackOverflow question...no longer having to use that coinChangeHelper shenanigans and
  explicitly selecting the coin, resulting in massive double counting)
 -----------------------------------------------------------------------------------------------------

 -----------------------------------------------------------------------------------------------------
  Recall that Haskell is lazy...you can define how variables will look after various transformations
    are done or functions are called, but the functions won't actually be called until they have to be.
  This is how I was able to write coinChangeMemo where I define all of the work done for solving
    a coinChange subproblem before I check the result of the cache lookup...the work I defined never
    occurs unless the lookup fails (which results in us referring to workedResult in the case statement,
    and for workedResult to be computed totalCombs needs to be computed, and totalCombs requires
    coinsUsedCombs to be computed...and so on and so on)
  -----------------------------------------------------------------------------------------------------




## Stack/Haskell Review

* Write tests in coin-change/test
*  Write application code in src/CoinChange.hs

* Building/Running
      
      stack build
      stack exec <project-name>-exe
      stack exec coin-change-exe

*  Testing

      stack test



## Setup

* Install HSpec for test suite
  
      stack install hspec


* Modify .cabal file to expose modules we write

      
      library
        hs-source-dirs:      src
        >>>>>>>>>>
        exposed-modules:     Lib
        <<<<<<<<<<<
        exposed-modules:     Lib, CoinChange
        build-depends:       base >= 4.7 && < 5
        default-language:    Haskell2010

* Modify .cabal file to specify the new file we'll be using as the "main" of the test suite

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


* Modify .cabal file to include Hspec in build-depends for test-suite

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

  Similar changes to .cabal file for containers package (Map.Data.Strict)

## HSpec
  Designed to be similar to RSpec from Ruby
  Feels similar to ScalaTest's FunSpec style

  `describe` a functionality you want to test, then 
    supply that describe section with unit tests using it/shouldBe constructs

## Debugging
  https://en.wikibooks.org/wiki/Haskell/Debugging 
  Use `trace` of the Debug.Trace module to print out variable status 
    (make sure you `show` it if the var is not a String)


## Random Ramblings
* Remember `show` will make data into a String (if the typeclass extends Show)
* Variables seem to have to be lowercase (ALL CAPS means compiler is seeing that symbol as a "data constructor")

