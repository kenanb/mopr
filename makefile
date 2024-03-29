MAKE_UTIL_DIR ::= $(CURDIR)/src/make

include $(MAKE_UTIL_DIR)/header.mk

.PHONY: all
all: test_run

.PRECIOUS: .%.d .%.o

include $(MAKE_UTIL_DIR)/depend.mk

#
## Initialization
#

$(info ========= INITIALIZE BEGIN =========)

MOPR_OUT_DIR ?= out

$(info MOPR_OUT_DIR - Value  : $(MOPR_OUT_DIR))
$(info MOPR_OUT_DIR - Origin : $(origin MOPR_OUT_DIR))

ifeq ($(MOPR_OUT_DIR),)
$(error MOPR_OUT_DIR is not assigned!)
endif

ifeq ($(MOPR_USD_ROOT),)
$(info MOPR_USD_ROOT is not assigned! Defaulting to system locations.)
else
$(info Setting build environment to use lib and include from MOPR_USD_ROOT.)
USD_INC_ROOT ::= -isystem$(MOPR_USD_ROOT)/include
USD_LIB_ROOT ::= -L$(MOPR_USD_ROOT)/lib
endif

$(info ========== INITIALIZE END ==========)

#
## Shared variables
#

CC ::= gcc

CXX ::= g++

CSTD ::= -std=c99

CXXSTD ::= -std=c++17

COMMON_CFLAGS ::= -pedantic -Wall -g3 -O3 -Wno-deprecated \
	-fstrict-aliasing -Wstrict-aliasing=2 -DMOPR_EXPORTS \
	-I$(CURDIR)/src -fvisibility=hidden

COMMON_LFLAGS ::= -Wl,-rpath,\$$ORIGIN -fuse-ld=mold

# The reason we use embed is because --libs python3 doesn't return a link flag.
USD_CFLAGS ::= $(USD_INC_ROOT) -DTBB_SUPPRESS_DEPRECATED_MESSAGES \
	`pkg-config --cflags python3-embed`

USD_LFLAGS ::= $(USD_LIB_ROOT) \
	`pkg-config --libs python3-embed`

#
#
####################
#### MOPR BUILD ####
##
#
#

MOPR_LIB_DIR ::= $(MOPR_OUT_DIR)/lib

#
## Target: mopr_core
#

MOPR_CORE_CF ::= $(COMMON_CFLAGS) $(USD_CFLAGS) -fPIC

MOPR_CORE_C_OBJ ::= src/core/._test.c.o

MOPR_CORE_C_DEP ::= $(MOPR_CORE_C_OBJ:.o=.d)

$(MOPR_CORE_C_DEP): CFLAGS = $(MOPR_CORE_CF) $(CSTD)
include $(MOPR_CORE_C_DEP)

mopr_core ::= $(MOPR_LIB_DIR)/libmopr_core.so

$(mopr_core): CFLAGS   ::= $(MOPR_CORE_CF) $(CSTD)
$(mopr_core): LDFLAGS  ::= $(COMMON_LFLAGS)
$(mopr_core): LDLIBS   ::=

$(mopr_core): $(MOPR_CORE_C_OBJ)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CC) -shared -o $@ $(MOPR_CORE_C_OBJ) $(LDFLAGS) $(LDLIBS)

# This is currently only for native testing of lisp module.
# It is not tied to the wider task dependency graph.
.PHONY: core
core: $(mopr_core)

#
## Target: wrap_std
#

WRAP_STD_CF ::= $(COMMON_CFLAGS) -fPIC

WRAP_STD_CPP_OBJ ::= src/wrap/std/ext/._test.cpp.o \
	src/wrap/std/ext/.string.cpp.o

WRAP_STD_CPP_DEP ::= $(WRAP_STD_CPP_OBJ:.o=.d)

$(WRAP_STD_CPP_DEP): CXXFLAGS = $(WRAP_STD_CF) $(CXXSTD)
include $(WRAP_STD_CPP_DEP)

wrap_std ::= $(MOPR_LIB_DIR)/libmopr_wrap_std.so

$(wrap_std): CFLAGS   ::= $(WRAP_STD_CF) $(CSTD)
$(wrap_std): CXXFLAGS ::= $(WRAP_STD_CF) $(CXXSTD)
$(wrap_std): LDFLAGS  ::= $(COMMON_LFLAGS)
$(wrap_std): LDLIBS   ::=

$(wrap_std): $(WRAP_STD_CPP_OBJ)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CXX) -shared -o $@ $(WRAP_STD_CPP_OBJ) $(LDFLAGS) $(LDLIBS)

# This is currently only for native testing of lisp module.
# It is not tied to the wider task dependency graph.
.PHONY: wrap
wrap: $(wrap_std)

#
## Target: wrap_usd
#

WRAP_USD_CF ::= $(COMMON_CFLAGS) $(USD_CFLAGS) -fPIC

WRAP_USD_CPP_OBJ ::= src/wrap/usd/ext/._test.cpp.o \
	src/wrap/usd/ext/.attribute.cpp.o \
	src/wrap/usd/ext/.valueTypeName.cpp.o \
	src/wrap/usd/ext/.timecode.cpp.o \
	src/wrap/usd/ext/.token.cpp.o \
	src/wrap/usd/ext/.array.cpp.o \
	src/wrap/usd/ext/.datum.cpp.o \
	src/wrap/usd/ext/.value.cpp.o \
	src/wrap/usd/ext/.path.cpp.o \
	src/wrap/usd/ext/.layer.cpp.o \
	src/wrap/usd/ext/.prim.cpp.o \
	src/wrap/usd/ext/.primDefinition.cpp.o \
	src/wrap/usd/ext/.propertyDefinition.cpp.o \
	src/wrap/usd/ext/.schemaInfo.cpp.o \
	src/wrap/usd/ext/.schemaTypeSet.cpp.o \
	src/wrap/usd/ext/.stage.cpp.o

WRAP_USD_CPP_DEP ::= $(WRAP_USD_CPP_OBJ:.o=.d)

$(WRAP_USD_CPP_DEP): CXXFLAGS = $(WRAP_USD_CF) $(CXXSTD)
include $(WRAP_USD_CPP_DEP)

wrap_usd ::= $(MOPR_LIB_DIR)/libmopr_wrap_usd.so

$(wrap_usd): CFLAGS   ::= $(WRAP_USD_CF) $(CSTD)
$(wrap_usd): CXXFLAGS ::= $(WRAP_USD_CF) $(CXXSTD)
$(wrap_usd): LDFLAGS  ::= $(COMMON_LFLAGS) $(USD_LFLAGS) -L$(MOPR_LIB_DIR)
$(wrap_usd): LDLIBS   ::= -lmopr_wrap_std

ifeq ($(MOPR_MONOLITHIC_USD),1)
$(wrap_usd): LDLIBS += -lusd_ms
else
$(wrap_usd): LDLIBS += \
	-lusd_tf -lusd_sdf -lusd_vt -lusd_usd -lusd_usdGeom
endif


$(wrap_usd): $(WRAP_USD_CPP_OBJ) $(wrap_std)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CXX) -shared -o $@ $(WRAP_USD_CPP_OBJ) $(LDFLAGS) $(LDLIBS)

# This is currently only for native testing of lisp module.
# It is not tied to the wider task dependency graph.
.PHONY: wrap
wrap: $(wrap_std) $(wrap_usd)

#
## Target: mopr-user--all-systems.so
#

MOPR_LISP_CF ::= $(COMMON_CFLAGS)

MOPR_LISP_FILE ::= mopr-user--all-systems.so

mopr_lisp ::= $(MOPR_LIB_DIR)/$(MOPR_LISP_FILE)

MOPR_LISP_DEP ::= src/lisp/.mopr-user.asd.d
$(MOPR_LISP_DEP): CFLAGS = $(MOPR_LISP_CF) $(CSTD)
include $(MOPR_LISP_DEP)

# $(wrap_usd) is dynamically loaded via CFFI. So no linker declaration.
$(mopr_lisp): $(mopr_core) $(wrap_usd) $(MOPR_LISP_DEP)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	ecl -norc --shell ./src/make/make.lisp
	mv ./$(@F) $(mopr_lisp)

#
## Target: boot_lisp
#

BOOT_LISP_CF ::= $(COMMON_CFLAGS) $(USD_CFLAGS) -fPIC \
	`ecl-config --cflags`

BOOT_LISP_CPP_OBJ ::= \
	src/boot/.bootLisp.cpp.o

BOOT_LISP_CPP_DEP ::= $(BOOT_LISP_CPP_OBJ:.o=.d)

$(BOOT_LISP_CPP_DEP): CXXFLAGS = $(BOOT_LISP_CF) $(CXXSTD)
include $(BOOT_LISP_CPP_DEP)

boot_lisp ::= $(MOPR_LIB_DIR)/libmopr_boot_lisp.so

$(boot_lisp): CFLAGS   ::= $(BOOT_LISP_CF) $(CSTD)
$(boot_lisp): CXXFLAGS ::= $(BOOT_LISP_CF) $(CXXSTD)
$(boot_lisp): LDFLAGS  ::= $(COMMON_LFLAGS) # No need for linking USD.
$(boot_lisp): LDECLOPT ::= `ecl-config --ldflags` \
	-L$(MOPR_LIB_DIR) -l:$(MOPR_LISP_FILE)

$(boot_lisp): $(BOOT_LISP_CPP_OBJ) $(mopr_lisp)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CXX) -shared -o $@ $(BOOT_LISP_CPP_OBJ) $(LDFLAGS) $(LDECLOPT)

#
## Target: plug_usdx
#

USDX_CF ::= $(COMMON_CFLAGS) $(USD_CFLAGS) -fPIC \
	`ecl-config --cflags`

USDX_CPP_OBJ ::= \
	src/plug/usdx/.usdx.cpp.o

USDX_CPP_DEP ::= $(USDX_CPP_OBJ:.o=.d)

$(USDX_CPP_DEP): CXXFLAGS = $(USDX_CF) $(CXXSTD)
include $(USDX_CPP_DEP)

plug_usdx ::= $(MOPR_OUT_DIR)/usdx/libmopr_usdx.so

$(plug_usdx): CFLAGS   ::= $(USDX_CF) $(CSTD)
$(plug_usdx): CXXFLAGS ::= $(USDX_CF) $(CXXSTD)
$(plug_usdx): LDFLAGS  ::= $(COMMON_LFLAGS) $(USD_LFLAGS)
$(plug_usdx): LDLIBS   ::=

ifeq ($(MOPR_MONOLITHIC_USD),1)
$(plug_usdx): LDLIBS += -lusd_ms
else
$(plug_usdx): LDLIBS += \
	-lusd_tf -lusd_sdf -lusd_vt -lusd_gf \
	-lusd_plug -lusd_arch -lusd_work -ltbb \
	-lusd_usd
endif

# NOTE: The order is important here.
# If we need to link ecl directly, mopr_boot_lisp should be linked
# before that. Otherwise, we get a "symbols are not initialized yet" error.
# Of course, we don't really need to link ecl again, if we link mopr_boot_lisp.
$(plug_usdx): LDECLOPT ::= -L$(MOPR_LIB_DIR) -lmopr_wrap_usd -lmopr_boot_lisp \
	`ecl-config --ldflags`

$(plug_usdx): $(USDX_CPP_OBJ) $(boot_lisp) $(wrap_usd)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CXX) -shared -o $@ $(USDX_CPP_OBJ) $(LDFLAGS) $(LDLIBS) $(LDECLOPT)

#
## Target: meta_usdx
#

meta_usdx ::= $(MOPR_OUT_DIR)/usdx/plugInfo.json

$(meta_usdx): src/plug/usdx.json
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	@cp $< $@

#
## Target: usdx_build
#

.PHONY: usdx_build
usdx_build: $(plug_usdx) $(meta_usdx)

#
#
#################
#### TESTING ####
##
#
#

#
## Target: trtn_build
#

TRTN_CF ::= $(COMMON_CFLAGS) $(USD_CFLAGS) -fPIC \
	`ecl-config --cflags`

TRTN_CPP_OBJ ::= \
	src/test/routines/.routines.cpp.o

TRTN_CPP_DEP ::= $(TRTN_CPP_OBJ:.o=.d)

$(TRTN_CPP_DEP): CXXFLAGS = $(TRTN_CF) $(CXXSTD)
include $(TRTN_CPP_DEP)

trtn_build ::= $(MOPR_OUT_DIR)/test/libroutines.so

$(trtn_build): CFLAGS   ::= $(TRTN_CF) $(CSTD)
$(trtn_build): CXXFLAGS ::= $(TRTN_CF) $(CXXSTD)
$(trtn_build): LDFLAGS  ::= $(COMMON_LFLAGS) $(USD_LFLAGS) -L$(MOPR_LIB_DIR)
$(trtn_build): LDLIBS   ::= -lmopr_wrap_std -lmopr_wrap_usd

ifeq ($(MOPR_MONOLITHIC_USD),1)
$(trtn_build): LDLIBS += -lusd_ms
else
$(trtn_build): LDLIBS += \
	-lusd_usd
endif

$(trtn_build): LDECLOPT ::= `ecl-config --ldflags`

$(trtn_build): $(TRTN_CPP_OBJ) $(wrap_usd)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CXX) -shared -o $@ $(TRTN_CPP_OBJ) $(LDFLAGS) $(LDLIBS) $(LDECLOPT)

#
## Target: test_build
#

TEST_PKGS ::= catch2
TEST_CF ::= $(COMMON_CFLAGS) -fPIE -Isrc/test \
	`pkg-config --cflags $(TEST_PKGS)`

TEST_CPP_OBJ ::= \
	src/test/.config.cpp.o \
	src/test/.main.cpp.o \
	src/test/.test_plug.cpp.o \
	src/test/.test_wrap.cpp.o

TEST_CPP_DEP ::= $(TEST_CPP_OBJ:.o=.d)

$(TEST_CPP_DEP): CXXFLAGS = $(TEST_CF) $(CXXSTD)
include $(TEST_CPP_DEP)

test_build ::= $(MOPR_OUT_DIR)/test/test

$(test_build): CFLAGS   ::= $(TEST_CF) $(CSTD)
$(test_build): CXXFLAGS ::= $(TEST_CF) $(CXXSTD)
$(test_build): LDFLAGS  = -L$(@D) $(COMMON_LFLAGS) \
	`pkg-config --libs $(TEST_PKGS)`

$(test_build): $(trtn_build)
$(test_build): $(TEST_CPP_OBJ)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CXX) -o $@ $(TEST_CPP_OBJ) $(LDFLAGS) -lroutines

#
## Target: test_run
#

run_tests_sh ::= $(MOPR_OUT_DIR)/run_tests

$(run_tests_sh): src/make/run_tests
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	@cp $< $@

with_plugin_sh ::= $(MOPR_OUT_DIR)/with_plugin

$(with_plugin_sh): src/make/with_plugin
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	@cp $< $@

TEST_ARGS ::= -R $(CURDIR) # -s

# The ending slash of plugin path is important.
.PHONY: test_run
test_run: usdx_build
test_run: $(test_build)
test_run: $(with_plugin_sh)
test_run: $(run_tests_sh)
test_run:
	$(call ECHO_RULE)
	(export LD_LIBRARY_PATH=$(CURDIR)/${MOPR_LIB_DIR}:$$LD_LIBRARY_PATH; \
	 export PXR_PLUGINPATH_NAME=$(CURDIR)/$(MOPR_OUT_DIR)/usdx/:$${PXR_PLUGINPATH_NAME}; \
	 ./$(test_build) $(TEST_ARGS))
