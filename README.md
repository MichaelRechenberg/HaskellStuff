# HaskellStuff
Notes for functional Programming in Haskell...Inspired by Learn You a Haskell

# Installation Issues
For some reason the server hosting GHC for `stack setup`
  was not working on June 1st 2017 so I had to install GHC 
  through apt-get and modify the stack.yaml for the 
  hello-world project with the following settings:

    resolver:  ghc-7.10.3
    system-ghc: true


If I wanted to have these changes be global, I'd have to update
  ~/.stack/global-project/stack.yaml with the same settings


