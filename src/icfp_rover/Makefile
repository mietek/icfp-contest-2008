##
## Template Makefile for build system
## Author: Martin Carlson <martin@martinc.eu>
##

# Versioning and Configuration
# (use ?= when defining variables to retain override effect)
sinclude $(TOP_DIR)/config.local.mk # (optional)
sinclude config.local.mk # (optional)
include config.mk
include vsn.mk

## Code layout
APPSRC = $(patsubst src/%.app.src,%.app.src,$(wildcard src/*.app.src))
ERLS = $(patsubst src/%.erl,%.erl,$(wildcard src/*.erl))
BEAMS = $(ERLS:.erl=.beam)
MODS = $(BEAMS:.beam=)
APP = $(APPSRC:.app.src=.app)

## Dependecy Search Paths
VPATH = src:include:ebin

all: $(APP) $(BEAMS) c_src

.PHONY: all clean c_src
.SUFFIXES: .erl .beam .app.src .app

clean: 
	@for i in $(wildcard ebin/*); do \
		echo [RM] $$i; \
		$(RM) $$i; \
	done
	@if test -d c_src ; then \
		$(MAKE) -C c_src clean; \
	fi

%.beam: %.erl
	@echo [ERLC] $<
	@$(ERLC) -o ebin $(EFLAGS) \
		-I include \
		-DREV=$(REV) \
		+debug_info \
		$(patsubst %,-pa $(TOP_DIR)/apps/%/ebin -pa $(TOP_DIR)/external/%/ebin, $(EDEPS)) $<

$(APP): $(APPSRC) 
	@echo [SED] $<
	@$(SED) "s|%MODULES%|`echo $(MODS) | tr '[:blank:]' ','`|g" $< | \
	$(SED) "s|%VSN%|$(VSN)|g" > ebin/$@

c_src:
	@if test -d c_src ; then \
		$(MAKE) -C c_src; \
	fi

