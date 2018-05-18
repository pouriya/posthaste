REBAR = $(CURDIR)/rebar3
BUILD_PATH := _build
LIB_PATH := $(BUILD_PATH)/default/lib

.PHONY: all compile shell test clean distclean

all: compile

compile:
	@ $(REBAR) compile

shell: compile
	@ erl -pa $(LIB_PATH)/posthaste/ebin

test:
	$(REBAR) ct && $(REBAR) dialyzer

clean:
	@ $(REBAR) clean

distclean: clean
	@ rm -rf $(BUILD_PATH)
