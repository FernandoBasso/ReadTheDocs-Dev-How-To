# Haskell How To

This repository intends to be a wiki of sorts about Haskell (and functional
programming in general).

**NOTE**: The [Gitlab repo](https://gitlab.com/devhowto/haskell-how-to) is the
*main one where all the action happens and people can collaborate. The [Github
*repo](https://github.com/FernandoBasso/Haskell-How-To) is just a mirror.


```text
##
# This recipe allows us to run the `develop` recipe from the docs/Makefile
# from the root directory of the project.
#
# Make sure you install sphinx-autobuild. Something like:
#
#   $ pip install sphinx-autobuild
#
develop:
	$(MAKE) develop -C docs
```


```text
##
# The Dev How To project is setup to run locally on port 2001.
#
# • http://local.fpjshowto.dev:4001
#
127.0.0.1 local.devhowto.dev
```
