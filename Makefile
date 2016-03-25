# Project.
PROJECT = mnesia_utile
PROJECT_DESCRIPTION = A collection of mnesai utile functions
PROJECT_VERSION = 0.0.1

# Options.
CT_OPTS += -pa test -ct_hooks mnesia_utile_ct_hook []

# Depandancies.
TEST_DEPS = ct_helper
dep_ct_helper = git https://github.com/extend/ct_helper.git master

include erlang.mk
