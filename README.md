PkgBrowser
==========

PkgBrowser is a local browser for your haskell packages. It uses gtk and
webkit, and just read haddock generated page. It's useful when you cannot access
hackage documentation (e.g. no internet, the horror).

For more usefulness, you should turn on documentation generation in your cabal config.

  ~/.cabal/config
  ...
  documentation: True
  ...


