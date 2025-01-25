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

# NOTE : Link step segfaults with mold-2.35.x, but works again with mold-2.36.0.
COMMON_LFLAGS ::= -Wl,-rpath,\$$ORIGIN -fuse-ld=mold

# The reason we use embed is because --libs python3 doesn't return a link flag.
USD_CFLAGS ::= $(USD_INC_ROOT) -DTBB_SUPPRESS_DEPRECATED_MESSAGES \
	`pkg-config --cflags python3-embed`

# WORKAROUND: When the framework is built with Clang, but linked USD is built with GCC,
# usage of some classes like UsdGeomBBoxCache triggers a segfault in unordered_map dtor.
# Because TfHashMap derives from __gnu_cxx::hash_map when ARCH_HAS_GNU_STL_EXTENSIONS
# is defined, which is the case when the compiler is GCC. Otherwise, it derives from
# std::unordered_map. Therefore, not using the same compiler causes incompatibility.
#
# In order to workaround this issue, we manually define the macro here.
# Relevant info:
# https://github.com/PixarAnimationStudios/OpenUSD/issues/1291
# https://github.com/PixarAnimationStudios/OpenUSD/issues/1057
USD_CFLAGS += -DARCH_HAS_GNU_STL_EXTENSIONS

USD_LFLAGS ::= $(USD_LIB_ROOT) \
	`pkg-config --libs python3-embed`

# NOTE: It is assumed that the correct python environment was activated, if needed.
PYTHON_VER_MAJ ::= `python -c 'import sys; print(sys.version_info[0])'`
PYTHON_VER_MIN ::= `python -c 'import sys; print(sys.version_info[1])'`
BOOST_PYTHON_LIB ::= boost_python$(PYTHON_VER_MAJ)$(PYTHON_VER_MIN)

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
## Target: mopr_repr
#

MOPR_REPR_CF ::= $(COMMON_CFLAGS) $(USD_CFLAGS) -fPIC

MOPR_REPR_C_OBJ ::= src/repr/.command.c.o

MOPR_REPR_C_DEP ::= $(MOPR_REPR_C_OBJ:.o=.d)

$(MOPR_REPR_C_DEP): CFLAGS = $(MOPR_REPR_CF) $(CSTD)
include $(MOPR_REPR_C_DEP)

mopr_repr ::= $(MOPR_LIB_DIR)/libmopr_repr.so

$(mopr_repr): CFLAGS   ::= $(MOPR_REPR_CF) $(CSTD)
$(mopr_repr): LDFLAGS  ::= $(COMMON_LFLAGS)
$(mopr_repr): LDLIBS   ::=

$(mopr_repr): $(MOPR_REPR_C_OBJ)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CC) -shared -o $@ $(MOPR_REPR_C_OBJ) $(LDFLAGS) $(LDLIBS)

# This is currently only for native testing of lisp module.
# It is not tied to the wider task dependency graph.
.PHONY: repr
repr: $(mopr_repr)

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
$(MOPR_LISP_DEP): CFLAGS = $(MOPR_LISP_CF) $(CSTD) -I$(MOPR_YOGA_INC_DIR)
include $(MOPR_LISP_DEP)

# $(wrap_usd) is dynamically loaded via CFFI. So no linker declaration.
$(mopr_lisp): $(mopr_core) $(mopr_repr) $(wrap_usd) $(MOPR_LISP_DEP)
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

# NOTE: The order is important here.
# If we need to link ecl directly, mopr_boot_lisp should be linked
# before that. Otherwise, we get a "symbols are not initialized yet" error.
# Of course, we don't really need to link ecl again, if we link mopr_boot_lisp.
#
# NOTE: Some linker versions seem to ignore the shared-lib ctor dtor hooks and ignore
# mopr_boot_lisp dependency declaration. So we add --no-as-needed beforehand.
BOOT_LISP_LDECLOPT ::= -L$(MOPR_LIB_DIR) -lmopr_wrap_usd \
	-Wl,--no-as-needed -lmopr_boot_lisp `ecl-config --ldflags`

#
## Target: yoga_core
#

YOGA_CORE_CF ::= $(COMMON_CFLAGS) -fPIC

YOGA_CORE_C_OBJ ::=

YOGA_CORE_C_DEP ::= $(YOGA_CORE_C_OBJ:.o=.d)

$(YOGA_CORE_C_DEP): CFLAGS = $(YOGA_CORE_CF) $(CSTD)
include $(YOGA_CORE_C_DEP)

yoga_core ::= $(MOPR_LIB_DIR)/libyoga_core.so

$(yoga_core): CFLAGS   ::= $(YOGA_CORE_CF) $(CSTD)
$(yoga_core): LDFLAGS  ::= $(COMMON_LFLAGS) -L$(MOPR_YOGA_LIB_DIR)
$(yoga_core): LDLIBS   ::= -Wl,--whole-archive -lyogacore -Wl,--no-whole-archive

$(yoga_core): $(YOGA_CORE_C_OBJ)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	$(CC) -shared -o $@ $(YOGA_CORE_C_OBJ) $(LDFLAGS) $(LDLIBS)

# This is currently only for native testing of lisp module.
# It is not tied to the wider task dependency graph.
.PHONY: yoga
yoga: $(yoga_core)

#
## Target: mopr_edit
#

CURL_CF ::= `curl-config --cflags`

CURL_LDLIBS ::= `curl-config --libs`

MOPR_EDIT_CF ::= $(COMMON_CFLAGS) $(USD_CFLAGS) -fPIC

MOPR_EDIT_LIBS = gl glew sdl2 SDL2_image pugixml

MOPR_EDIT_CPP_OBJ ::= src/edit/.main.cpp.o \
	src/edit/.clientInProcessECL.cpp.o \
	src/edit/.clientLoopbackHTTP.cpp.o \
	src/edit/.common.cpp.o \
	src/edit/.glUtil.cpp.o \
	src/edit/.editor.cpp.o \
	src/edit/.scene.cpp.o \
	src/edit/.overlayLayer.cpp.o \
	src/edit/.overlayProgram.cpp.o \
	src/edit/.appConfig.cpp.o \
	src/edit/.appDelegate.cpp.o \
	src/edit/.appEnvironment.cpp.o \
	src/edit/.appState.cpp.o \
	src/edit/.messaging.cpp.o \
	src/edit/.procedureViz.cpp.o \
	src/edit/.viewportNavigation.cpp.o \
	src/edit/ext/imgui/.imgui.cpp.o \
	src/edit/ext/imgui/.imgui_draw.cpp.o \
	src/edit/ext/imgui/.imgui_tables.cpp.o \
	src/edit/ext/imgui/.imgui_widgets.cpp.o \
	src/edit/ext/imgui/backends/.imgui_impl_sdl2.cpp.o \
	src/edit/ext/imgui/backends/.imgui_impl_opengl3.cpp.o \
	src/edit/ext/imgui/misc/cpp/.imgui_stdlib.cpp.o

MOPR_EDIT_CPP_DEP ::= $(MOPR_EDIT_CPP_OBJ:.o=.d)

$(MOPR_EDIT_CPP_DEP): CXXFLAGS = `pkg-config --cflags $(MOPR_EDIT_LIBS)` \
	$(MOPR_EDIT_CF) $(CURL_CF) $(CXXSTD) -I$(MOPR_YOGA_INC_DIR)
$(MOPR_EDIT_CPP_DEP): CXXFLAGS += \
	-Isrc/edit/ext/imgui \
	-Isrc/edit/ext/imgui/backends \
	-Isrc/edit/ext/imgui/misc/cpp

include $(MOPR_EDIT_CPP_DEP)

mopr_edit ::= $(MOPR_OUT_DIR)/mopr_editor

$(mopr_edit): CXXFLAGS ::= `pkg-config --cflags $(MOPR_EDIT_LIBS)` \
	$(MOPR_EDIT_CF) $(CURL_CF) $(CXXSTD) -I$(MOPR_YOGA_INC_DIR)
$(mopr_edit): CXXFLAGS += \
	-Isrc/edit/ext/imgui \
	-Isrc/edit/ext/imgui/backends \
	-Isrc/edit/ext/imgui/misc/cpp
$(mopr_edit): LDFLAGS  ::= $(COMMON_LFLAGS) $(USD_LFLAGS) -L$(MOPR_LIB_DIR)
$(mopr_edit): LDLIBS   ::= `pkg-config --libs $(MOPR_EDIT_LIBS)` \
	 $(CURL_LDLIBS) -lmopr_core -lmopr_repr -lyoga_core

# boost-python is required by usdGeom dependency.
$(mopr_edit): LDLIBS += -lboost_regex -l$(BOOST_PYTHON_LIB)

ifeq ($(MOPR_MONOLITHIC_USD),1)
$(mopr_edit): LDLIBS += -lusd_ms
else
$(mopr_edit): LDLIBS += \
	-lusd_arch -lusd_gf -lusd_js -lusd_tf -lusd_vt -lusd_work \
	-lusd_sdf -lusd_usd -lusd_usdGeom -lusd_python \
	-lusd_garch -lusd_cameraUtil -lusd_glf -lusd_hd \
	-lusd_usdImaging -lusd_usdImagingGL
endif

$(mopr_edit): LDECLOPT ::= $(BOOT_LISP_LDECLOPT)

$(mopr_edit): $(MOPR_EDIT_CPP_OBJ) $(mopr_core) $(mopr_repr) $(yoga_core) $(boot_lisp) $(wrap_usd)
	$(call ECHO_RULE)
	@mkdir -p $(@D)
	@cp -r src/edit/res $(MOPR_OUT_DIR)
	$(CXX) -o $@ $(MOPR_EDIT_CPP_OBJ) $(LDFLAGS) $(LDLIBS) $(LDECLOPT)

# This is currently only for native testing of lisp module.
# It is not tied to the wider task dependency graph.
.PHONY: mopr_edit
mopr_edit: $(mopr_edit)

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

$(plug_usdx): LDLIBS += -ltbb

ifeq ($(MOPR_MONOLITHIC_USD),1)
$(plug_usdx): LDLIBS += -lusd_ms
else
$(plug_usdx): LDLIBS += \
	-lusd_tf -lusd_sdf -lusd_vt -lusd_gf \
	-lusd_plug -lusd_arch -lusd_work \
	-lusd_usd
endif

$(plug_usdx): LDECLOPT ::= $(BOOT_LISP_LDECLOPT)

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
	-lusd_plug -lusd_sdf -lusd_usd
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
test_run: mopr_edit
test_run: usdx_build
test_run: $(test_build)
test_run: $(with_plugin_sh)
test_run: $(run_tests_sh)
test_run:
	$(call ECHO_RULE)
	(export LD_LIBRARY_PATH=$(CURDIR)/${MOPR_LIB_DIR}:$$LD_LIBRARY_PATH; \
	 export PXR_PLUGINPATH_NAME=$(CURDIR)/$(MOPR_OUT_DIR)/usdx/:$${PXR_PLUGINPATH_NAME}; \
	 ./$(test_build) $(TEST_ARGS))
