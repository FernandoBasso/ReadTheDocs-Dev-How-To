# Haskell How To

This repository intends to be a wiki of sorts about Haskell (and functional programming in general).

**NOTE**: The [Gitlab repo](https://gitlab.com/devhowto/haskell-how-to) is the main one where all the action happens and people can collaborate. The [Github repo](https://github.com/FernandoBasso/Haskell-How-To) is just a mirror.


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


##
# These are used for Sphinx Doc. Remember to use the proper port
# number as per your projects' configuration. For example, my local
# cmdline project runs on port 4000 and the haskell one on port
# 4001, thus, I access these projects with the following URLs:
#
# • http://local.cmdline.dev:4000
# • http://local.haskellhowto.dev:4001
#
127.0.0.1 local.cmdlinehowto.dev
127.0.0.1 local.haskellhowto.dev

