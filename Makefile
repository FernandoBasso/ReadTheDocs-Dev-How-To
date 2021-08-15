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

