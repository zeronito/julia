JULIAHOME := $(abspath $(dir $(lastword $(MAKEFILE_LIST))))
include $(JULIAHOME)/Make.inc

VERSDIR := v`cut -d. -f1-2 < $(JULIAHOME)/VERSION`

default: $(JULIA_BUILD_MODE) # contains either "debug" or "release"
all: debug release

# sort is used to remove potential duplicates
DIRS := $(sort $(build_bindir) $(build_depsbindir) $(build_libdir) $(build_private_libdir) $(build_libexecdir) $(build_includedir) $(build_includedir)/julia $(build_sysconfdir)/julia $(build_datarootdir)/julia $(build_datarootdir)/julia/stdlib $(build_man1dir))
ifneq ($(BUILDROOT),$(JULIAHOME))
BUILDDIRS := $(BUILDROOT) $(addprefix $(BUILDROOT)/,base src ui doc deps stdlib test test/embedding test/llvmpasses)
BUILDDIRMAKE := $(addsuffix /Makefile,$(BUILDDIRS)) $(BUILDROOT)/sysimage.mk
DIRS := $(DIRS) $(BUILDDIRS)
$(BUILDDIRMAKE): | $(BUILDDIRS)
	@# add Makefiles to the build directories for convenience (pointing back to the source location of each)
	@echo '# -- This file is automatically generated in julia/Makefile -- #' > $@
	@echo 'BUILDROOT=$(BUILDROOT)' >> $@
	@echo 'include $(JULIAHOME)$(patsubst $(BUILDROOT)%,%,$@)' >> $@
julia-deps: | $(BUILDDIRMAKE)
configure-y: | $(BUILDDIRMAKE)
configure:
ifeq ("$(origin O)", "command line")
	@if [ "$$(ls '$(BUILDROOT)' 2> /dev/null)" ]; then \
		echo 'WARNING: configure called on non-empty directory $(BUILDROOT)'; \
		read -p "Proceed [y/n]? " answer; \
	else \
		answer=y;\
	fi; \
	[ $$answer = 'y' ] && $(MAKE) configure-$$answer
else
	$(error "cannot rerun configure from within a build directory")
endif
else
configure:
	$(error "must specify O=builddir to run the Julia `make configure` target")
endif

$(foreach dir,$(DIRS),$(eval $(call dir_target,$(dir))))
$(foreach link,base $(JULIAHOME)/test,$(eval $(call symlink_target,$(link),$$(build_datarootdir)/julia,$(notdir $(link)))))

julia_flisp.boot.inc.phony: julia-deps
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/src julia_flisp.boot.inc.phony

# Build the HTML docs (skipped if already exists, notably in tarballs)
$(BUILDROOT)/doc/_build/html/en/index.html: $(shell find $(BUILDROOT)/base $(BUILDROOT)/doc \( -path $(BUILDROOT)/doc/_build -o -path $(BUILDROOT)/doc/deps -o -name *_constants.jl -o -name *_h.jl -o -name version_git.jl \) -prune -o -type f -print)
	@$(MAKE) docs

julia-symlink: julia-ui-$(JULIA_BUILD_MODE)
ifeq ($(OS),WINNT)
	@echo '@"%~dp0"\'"$(shell $(JULIAHOME)/contrib/relative_path.sh "$(BUILDROOT)" "$(JULIA_EXECUTABLE)" | tr / '\\')" '%*' > $(BUILDROOT)/julia.bat
	chmod a+x $(BUILDROOT)/julia.bat
else
ifndef JULIA_VAGRANT_BUILD
	@ln -sf "$(shell $(JULIAHOME)/contrib/relative_path.sh "$(BUILDROOT)" "$(JULIA_EXECUTABLE)")" $(BUILDROOT)/julia
endif
endif

julia-deps: | $(DIRS) $(build_datarootdir)/julia/base $(build_datarootdir)/julia/test
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/deps

julia-stdlib: | $(DIRS)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/stdlib

julia-base: julia-deps $(build_sysconfdir)/julia/startup.jl $(build_man1dir)/julia.1 $(build_datarootdir)/julia/julia-config.jl
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/base

julia-libccalltest: julia-deps
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/src libccalltest

julia-libllvmcalltest: julia-deps
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/src libllvmcalltest

julia-src-release julia-src-debug : julia-src-% : julia-deps julia_flisp.boot.inc.phony
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/src libjulia-$*

julia-ui-release julia-ui-debug : julia-ui-% : julia-src-%
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/ui julia-$*

julia-sysimg-ji : julia-stdlib julia-base julia-ui-$(JULIA_BUILD_MODE) | $(build_private_libdir)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT) -f sysimage.mk sysimg-ji JULIA_EXECUTABLE='$(JULIA_EXECUTABLE)'

julia-sysimg-release julia-sysimg-debug : julia-sysimg-% : julia-sysimg-ji julia-ui-%
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT) -f sysimage.mk sysimg-$*

julia-debug julia-release : julia-% : julia-sysimg-% julia-symlink julia-libccalltest julia-libllvmcalltest julia-base-cache

debug release : % : julia-%

docs: julia-sysimg-$(JULIA_BUILD_MODE)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/doc JULIA_EXECUTABLE='$(call spawn,$(JULIA_EXECUTABLE_$(JULIA_BUILD_MODE)))'

check-whitespace:
ifneq ($(NO_GIT), 1)
	@$(JULIAHOME)/contrib/check-whitespace.sh
else
	$(warn "Skipping whitespace check because git is unavailable")
endif

release-candidate: release testall
	@$(JULIA_EXECUTABLE) $(JULIAHOME)/contrib/add_license_to_files.jl #add license headers
	@#Check documentation
	@$(JULIA_EXECUTABLE) $(JULIAHOME)/doc/NEWS-update.jl #Add missing cross-references to NEWS.md
	@$(MAKE) -C $(BUILDROOT)/doc html doctest=true linkcheck=true
	@$(MAKE) -C $(BUILDROOT)/doc pdf

	@# Check to see if the above make invocations changed anything important
	@if [ -n "$$(git status --porcelain)" ]; then \
		echo "Git repository dirty; Verify and commit changes to the repository, then retry"; \
		exit 1; \
	fi

	@#Check that netload tests work
	@#for test in test/netload/*.jl; do julia $$test; if [ $$? -ne 0 ]; then exit 1; fi; done
	@echo
	@echo To complete the release candidate checklist:
	@echo

	@echo 1. Remove deprecations in base/deprecated.jl
	@echo 2. Update references to the julia version in the source directories, such as in README.md
	@echo 3. Bump VERSION
	@echo 4. Increase SOMAJOR and SOMINOR if needed.
	@echo 5. Create tag, push to github "\(git tag v\`cat VERSION\` && git push --tags\)"		#"` # These comments deal with incompetent syntax highlighting rules
	@echo 6. Clean out old .tar.gz files living in deps/, "\`git clean -fdx\`" seems to work	#"`
	@echo 7. Replace github release tarball with tarballs created from make light-source-dist and make full-source-dist
	@echo 8. Check that 'make && make install && make test' succeed with unpacked tarballs even without Internet access.
	@echo 9. Follow packaging instructions in DISTRIBUTING.md to create binary packages for all platforms
	@echo 10. Upload to AWS, update https://julialang.org/downloads and http://status.julialang.org/stable links
	@echo 11. Update checksums on AWS for tarball and packaged binaries
	@echo 12. Announce on mailing lists
	@echo 13. Change master to release-0.X in base/version.jl and base/version_git.sh as in 4cb1e20
	@echo

$(build_man1dir)/julia.1: $(JULIAHOME)/doc/man/julia.1 | $(build_man1dir)
	@echo Copying in usr/share/man/man1/julia.1
	@mkdir -p $(build_man1dir)
	@cp $< $@

$(build_sysconfdir)/julia/startup.jl: $(JULIAHOME)/etc/startup.jl | $(build_sysconfdir)/julia
	@echo Creating usr/etc/julia/startup.jl
	@cp $< $@

$(build_datarootdir)/julia/julia-config.jl: $(JULIAHOME)/contrib/julia-config.jl | $(build_datarootdir)/julia
	$(INSTALL_M) $< $(dir $@)

$(build_depsbindir)/stringreplace: $(JULIAHOME)/contrib/stringreplace.c | $(build_depsbindir)
	@$(call PRINT_CC, $(HOSTCC) -o $(build_depsbindir)/stringreplace $(JULIAHOME)/contrib/stringreplace.c)

julia-base-cache: julia-sysimg-$(JULIA_BUILD_MODE) | $(DIRS) $(build_datarootdir)/julia
	@$(call spawn,$(JULIA_EXECUTABLE) --startup-file=no $(call cygpath_w,$(JULIAHOME)/etc/write_base_cache.jl) \
		$(call cygpath_w,$(build_datarootdir)/julia/base.cache))

# public libraries, that are installed in $(prefix)/lib
JL_TARGETS := julia
ifeq ($(BUNDLE_DEBUG_LIBS),1)
JL_TARGETS += julia-debug
endif

# private libraries, that are installed in $(prefix)/lib/julia
JL_PRIVATE_LIBS-0 := libccalltest libllvmcalltest
ifeq ($(USE_GPL_LIBS), 1)
JL_PRIVATE_LIBS-0 += libsuitesparse_wrapper
JL_PRIVATE_LIBS-$(USE_SYSTEM_SUITESPARSE) += libamd libcamd libccolamd libcholmod libcolamd libumfpack libspqr libsuitesparseconfig
endif
JL_PRIVATE_LIBS-$(USE_SYSTEM_PCRE) += libpcre2-8
JL_PRIVATE_LIBS-$(USE_SYSTEM_DSFMT) += libdSFMT
JL_PRIVATE_LIBS-$(USE_SYSTEM_GMP) += libgmp
JL_PRIVATE_LIBS-$(USE_SYSTEM_MPFR) += libmpfr
JL_PRIVATE_LIBS-$(USE_SYSTEM_LIBSSH2) += libssh2
JL_PRIVATE_LIBS-$(USE_SYSTEM_MBEDTLS) += libmbedtls libmbedcrypto libmbedx509
JL_PRIVATE_LIBS-$(USE_SYSTEM_CURL) += libcurl
JL_PRIVATE_LIBS-$(USE_SYSTEM_LIBGIT2) += libgit2
ifeq ($(USE_LLVM_SHLIB),1)
JL_PRIVATE_LIBS-$(USE_SYSTEM_LLVM) += libLLVM libLLVM-6
endif

ifeq ($(USE_SYSTEM_LIBM),0)
JL_PRIVATE_LIBS-$(USE_SYSTEM_OPENLIBM) += libopenlibm
endif

JL_PRIVATE_LIBS-$(USE_SYSTEM_BLAS) += $(LIBBLASNAME)
ifneq ($(LIBLAPACKNAME),$(LIBBLASNAME))
JL_PRIVATE_LIBS-$(USE_SYSTEM_LAPACK) += $(LIBLAPACKNAME)
endif

ifeq ($(OS),Darwin)
ifeq ($(USE_SYSTEM_BLAS),1)
ifeq ($(USE_SYSTEM_LAPACK),0)
JL_PRIVATE_LIBS-0 += libgfortblas
endif
endif
endif

# On FreeBSD, /lib/libgcc_s.so.1 is incompatible with Fortran; to use Fortran on FreeBSD,
# we need to link to the libgcc_s that ships with the same GCC version used by libgfortran.
# To work around this, we copy the GCC libraries we need, namely libgfortran, libgcc_s,
# and libquadmath, into our build library directory, $(build_libdir). We also add them to
# JL_PRIVATE_LIBS-0 so that they know where they need to live at install time.
ifeq ($(OS),FreeBSD)
define std_so
julia-deps: | $$(build_libdir)/$(1).so
$$(build_libdir)/$(1).so: | $$(build_libdir)
	$$(INSTALL_M) $$(GCCPATH)/$(1).so* $$(build_libdir)
JL_PRIVATE_LIBS-0 += $(1)
endef

$(eval $(call std_so,libgfortran))
$(eval $(call std_so,libgcc_s))
$(eval $(call std_so,libquadmath))
endif # FreeBSD

ifeq ($(OS),WINNT)
# find the standard .dll folders
ifeq ($(XC_HOST),)
STD_LIB_PATH ?= $(PATH)
else
STD_LIB_PATH := $(shell LANG=C $(CC) -print-search-dirs | grep programs | sed -e "s/^programs: =//")
STD_LIB_PATH += :$(shell LANG=C $(CC) -print-search-dirs | grep libraries | sed -e "s/^libraries: =//")
ifneq (,$(findstring CYGWIN,$(BUILD_OS))) # the cygwin-mingw32 compiler lies about it search directory paths
STD_LIB_PATH := $(shell echo '$(STD_LIB_PATH)' | sed -e "s!/lib/!/bin/!g")
endif
endif

pathsearch = $(firstword $(wildcard $(addsuffix /$(1),$(subst :, ,$(2)))))

define std_dll
julia-deps-libs: | $$(build_bindir)/lib$(1).dll $$(build_depsbindir)/lib$(1).dll
$$(build_bindir)/lib$(1).dll: | $$(build_bindir)
	cp $$(or $$(call pathsearch,lib$(1).dll,$$(STD_LIB_PATH)),$$(error can't find lib$1.dll)) $$(build_bindir)
$$(build_depsbindir)/lib$(1).dll: | $$(build_depsbindir)
	cp $$(or $$(call pathsearch,lib$(1).dll,$$(STD_LIB_PATH)),$$(error can't find lib$1.dll)) $$(build_depsbindir)
JL_TARGETS += $(1)
endef
julia-deps: julia-deps-libs

# Given a list of space-separated libraries, return the first library name that is
# correctly found through `pathsearch`.
define select_std_dll
$(firstword $(foreach name,$(1),$(if $(call pathsearch,lib$(name).dll,$(STD_LIB_PATH)),$(name),)))
endef

$(eval $(call std_dll,$(call select_std_dll,gfortran-3 gfortran-4 gfortran-5)))
$(eval $(call std_dll,quadmath-0))
$(eval $(call std_dll,stdc++-6))
ifeq ($(ARCH),i686)
$(eval $(call std_dll,gcc_s_sjlj-1))
else
$(eval $(call std_dll,gcc_s_seh-1))
endif
$(eval $(call std_dll,ssp-0))
$(eval $(call std_dll,winpthread-1))
$(eval $(call std_dll,atomic-1))
endif


define stringreplace
	$(build_depsbindir)/stringreplace $$(strings -t x - $1 | grep '$2' | awk '{print $$1;}') '$3' 255 "$(call cygpath_w,$1)"
endef

# Run fixup-libgfortran on all platforms but Windows and FreeBSD. On FreeBSD we
# pull in the GCC libraries earlier and use them for the build to make sure we
# don't inadvertently link to /lib/libgcc_s.so.1, which is incompatible with
# libgfortran, and on Windows we copy them in earlier as well.
ifeq (,$(findstring $(OS),FreeBSD WINNT))
julia-base: $(build_libdir)/libgfortran*.$(SHLIB_EXT)*
$(build_libdir)/libgfortran*.$(SHLIB_EXT)*: | $(build_libdir) julia-deps
	-$(CUSTOM_LD_LIBRARY_PATH) PATH=$(PATH):$(build_depsbindir) $(JULIAHOME)/contrib/fixup-libgfortran.sh --verbose $(build_libdir)
JL_PRIVATE_LIBS-0 += libgfortran libgcc_s libquadmath
endif


install: $(build_depsbindir)/stringreplace $(BUILDROOT)/doc/_build/html/en/index.html
	@$(MAKE) $(QUIET_MAKE) all
	@for subdir in $(bindir) $(datarootdir)/julia/stdlib/$(VERSDIR) $(docdir) $(man1dir) $(includedir)/julia $(libdir) $(private_libdir) $(sysconfdir); do \
		mkdir -p $(DESTDIR)$$subdir; \
	done

	$(INSTALL_M) $(build_bindir)/julia $(DESTDIR)$(bindir)/
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	$(INSTALL_M) $(build_bindir)/julia-debug $(DESTDIR)$(bindir)/
endif
ifeq ($(OS),WINNT)
	-$(INSTALL_M) $(filter-out $(build_bindir)/libjulia-debug.dll,$(wildcard $(build_bindir)/*.dll)) $(DESTDIR)$(bindir)/
	-$(INSTALL_M) $(build_libdir)/libjulia.dll.a $(DESTDIR)$(libdir)/
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	-$(INSTALL_M) $(build_bindir)/libjulia-debug.dll $(DESTDIR)$(bindir)/
	-$(INSTALL_M) $(build_libdir)/libjulia-debug.dll.a $(DESTDIR)$(libdir)/
endif
	-$(INSTALL_M) $(build_bindir)/libopenlibm.dll.a $(DESTDIR)$(libdir)/
else

# Copy over .dSYM directories directly for Darwin
ifeq ($(OS),Darwin)
	-cp -a $(build_libdir)/libjulia.*.dSYM $(DESTDIR)$(libdir)
	-cp -a $(build_private_libdir)/sys.dylib.dSYM $(DESTDIR)$(private_libdir)
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	-cp -a $(build_libdir)/libjulia-debug.*.dSYM $(DESTDIR)$(libdir)
	-cp -a $(build_private_libdir)/sys-debug.dylib.dSYM $(DESTDIR)$(private_libdir)
endif
endif

	for suffix in $(JL_TARGETS) ; do \
		for lib in $(build_libdir)/lib$${suffix}.*$(SHLIB_EXT)*; do \
			if [ "$${lib##*.}" != "dSYM" ]; then \
				$(INSTALL_M) $$lib $(DESTDIR)$(libdir) ; \
			fi \
		done \
	done
	for suffix in $(JL_PRIVATE_LIBS-0) ; do \
		for lib in $(build_libdir)/$${suffix}.*$(SHLIB_EXT)*; do \
			if [ "$${lib##*.}" != "dSYM" ]; then \
				$(INSTALL_M) $$lib $(DESTDIR)$(private_libdir) ; \
			fi \
		done \
	done
	for suffix in $(JL_PRIVATE_LIBS-1) ; do \
		lib=$(build_private_libdir)/$${suffix}.$(SHLIB_EXT); \
		$(INSTALL_M) $$lib $(DESTDIR)$(private_libdir) ; \
	done
endif

	# Copy public headers
	cp -R -L $(build_includedir)/julia/* $(DESTDIR)$(includedir)/julia
	# Copy system image
	$(INSTALL_M) $(build_private_libdir)/sys.$(SHLIB_EXT) $(DESTDIR)$(private_libdir)
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	$(INSTALL_M) $(build_private_libdir)/sys-debug.$(SHLIB_EXT) $(DESTDIR)$(private_libdir)
endif

	# Copy in all .jl sources as well
	cp -R -L $(build_datarootdir)/julia $(DESTDIR)$(datarootdir)/
	# Copy documentation
	cp -R -L $(BUILDROOT)/doc/_build/html $(DESTDIR)$(docdir)/
	# Remove various files which should not be installed
	-rm -f $(DESTDIR)$(datarootdir)/julia/base/version_git.sh
	-rm -f $(DESTDIR)$(datarootdir)/julia/test/Makefile
	-rm -f $(DESTDIR)$(datarootdir)/julia/stdlib/$(VERSDIR)/*/source-extracted
	-rm -f $(DESTDIR)$(datarootdir)/julia/stdlib/$(VERSDIR)/*/build-configured
	-rm -f $(DESTDIR)$(datarootdir)/julia/stdlib/$(VERSDIR)/*/build-compiled
	-rm -f $(DESTDIR)$(datarootdir)/julia/stdlib/$(VERSDIR)/*/build-checked
	# Copy in beautiful new man page
	$(INSTALL_F) $(build_man1dir)/julia.1 $(DESTDIR)$(man1dir)/
	# Copy icon and .desktop file
	mkdir -p $(DESTDIR)$(datarootdir)/icons/hicolor/scalable/apps/
	$(INSTALL_F) $(JULIAHOME)/contrib/julia.svg $(DESTDIR)$(datarootdir)/icons/hicolor/scalable/apps/
	-touch -c $(DESTDIR)$(datarootdir)/icons/hicolor/
	-gtk-update-icon-cache --ignore-theme-index $(DESTDIR)$(datarootdir)/icons/hicolor/
	mkdir -p $(DESTDIR)$(datarootdir)/applications/
	$(INSTALL_F) $(JULIAHOME)/contrib/julia.desktop $(DESTDIR)$(datarootdir)/applications/
	# Install appdata file
	mkdir -p $(DESTDIR)$(datarootdir)/appdata/
	$(INSTALL_F) $(JULIAHOME)/contrib/julia.appdata.xml $(DESTDIR)$(datarootdir)/appdata/

	# Update RPATH entries and JL_SYSTEM_IMAGE_PATH if $(private_libdir_rel) != $(build_private_libdir_rel)
ifneq ($(private_libdir_rel),$(build_private_libdir_rel))
ifeq ($(OS), Darwin)
	for j in $(JL_TARGETS) ; do \
		install_name_tool -rpath @executable_path/$(build_private_libdir_rel) @executable_path/$(private_libdir_rel) $(DESTDIR)$(bindir)/$$j; \
		install_name_tool -add_rpath @executable_path/$(build_libdir_rel) @executable_path/$(libdir_rel) $(DESTDIR)$(bindir)/$$j; \
	done
else ifneq (,$(findstring $(OS),Linux FreeBSD))
	for j in $(JL_TARGETS) ; do \
		patchelf --set-rpath '$$ORIGIN/$(private_libdir_rel):$$ORIGIN/$(libdir_rel)' $(DESTDIR)$(bindir)/$$j; \
	done
endif

	# Overwrite JL_SYSTEM_IMAGE_PATH in julia library
	$(call stringreplace,$(DESTDIR)$(libdir)/libjulia.$(SHLIB_EXT),sys.$(SHLIB_EXT)$$,$(private_libdir_rel)/sys.$(SHLIB_EXT))
ifeq ($(BUNDLE_DEBUG_LIBS),1)
	$(call stringreplace,$(DESTDIR)$(libdir)/libjulia-debug.$(SHLIB_EXT),sys-debug.$(SHLIB_EXT)$$,$(private_libdir_rel)/sys-debug.$(SHLIB_EXT))
endif

endif
	# On FreeBSD, remove the build's libdir from each library's RPATH
ifeq ($(OS),FreeBSD)
	$(JULIAHOME)/contrib/fixup-rpath.sh $(build_depsbindir)/patchelf $(DESTDIR)$(libdir) $(build_libdir)
	$(JULIAHOME)/contrib/fixup-rpath.sh $(build_depsbindir)/patchelf $(DESTDIR)$(private_libdir) $(build_libdir)
	$(JULIAHOME)/contrib/fixup-rpath.sh $(build_depsbindir)/patchelf $(DESTDIR)$(bindir) $(build_libdir)
	# Set libgfortran's RPATH to ORIGIN instead of GCCPATH. It's only libgfortran that
	# needs to be fixed here, as libgcc_s and libquadmath don't have RPATHs set. If we
	# don't set libgfortran's RPATH, it won't be able to find its friends on systems
	# that don't have the exact GCC port installed used for the build.
	for lib in $(DESTDIR)$(private_libdir)/libgfortran*$(SHLIB_EXT)*; do \
		$(build_depsbindir)/patchelf --set-rpath '$$ORIGIN' $$lib; \
	done
endif

	mkdir -p $(DESTDIR)$(sysconfdir)
	cp -R $(build_sysconfdir)/julia $(DESTDIR)$(sysconfdir)/

distclean:
	-rm -fr $(BUILDROOT)/julia-*.tar.gz $(BUILDROOT)/julia*.exe $(BUILDROOT)/julia-*.7z $(BUILDROOT)/julia-$(JULIA_COMMIT)

binary-dist: distclean
ifeq ($(USE_SYSTEM_BLAS),0)
ifeq ($(ISX86),1)
ifneq ($(OPENBLAS_DYNAMIC_ARCH),1)
	@echo OpenBLAS must be rebuilt with OPENBLAS_DYNAMIC_ARCH=1 to use binary-dist target
	@false
endif
endif
endif
ifneq ($(prefix),$(abspath julia-$(JULIA_COMMIT)))
	$(error prefix must not be set for make binary-dist)
endif
ifneq ($(DESTDIR),)
	$(error DESTDIR must not be set for make binary-dist)
endif
	@$(MAKE) -C $(BUILDROOT) -f $(JULIAHOME)/Makefile install
	cp $(JULIAHOME)/LICENSE.md $(BUILDROOT)/julia-$(JULIA_COMMIT)
ifeq ($(OS), Linux)
	-$(JULIAHOME)/contrib/fixup-libstdc++.sh $(DESTDIR)$(libdir) $(DESTDIR)$(private_libdir)

	# Copy over any bundled ca certs we picked up from the system during buildi
	-cp $(build_datarootdir)/julia/cert.pem $(DESTDIR)$(datarootdir)/julia/
endif
	# Copy in startup.jl files per-platform for binary distributions as well
	# Note that we don't install to sysconfdir: we always install to $(DESTDIR)$(prefix)/etc.
	# If you want to make a distribution with a hardcoded path, you take care of installation
ifeq ($(OS), Darwin)
	-cat $(JULIAHOME)/contrib/mac/startup.jl >> $(DESTDIR)$(prefix)/etc/julia/startup.jl
endif

ifeq ($(OS), WINNT)
	[ ! -d $(JULIAHOME)/dist-extras ] || ( cd $(JULIAHOME)/dist-extras && \
		cp 7z.exe 7z.dll libexpat-1.dll zlib1.dll $(BUILDROOT)/julia-$(JULIA_COMMIT)/bin )
	cd $(BUILDROOT)/julia-$(JULIA_COMMIT)/bin && rm -f llvm* llc.exe lli.exe opt.exe LTO.dll bugpoint.exe macho-dump.exe

	# create file listing for uninstall. note: must have Windows path separators and line endings.
	cd $(BUILDROOT)/julia-$(JULIA_COMMIT) && find * | sed -e 's/\//\\/g' -e 's/$$/\r/g' > etc/uninstall.log

	# build nsis package
	cd $(BUILDROOT) && $(call spawn,$(JULIAHOME)/dist-extras/nsis/makensis.exe) -NOCD -DVersion=$(JULIA_VERSION) -DArch=$(ARCH) -DCommit=$(JULIA_COMMIT) -DMUI_ICON="$(call cygpath_w,$(JULIAHOME)/contrib/windows/julia.ico)" $(call cygpath_w,$(JULIAHOME)/contrib/windows/build-installer.nsi) | iconv -f latin1

	# compress nsis installer and combine with 7zip self-extracting header
	cd $(BUILDROOT) && $(JULIAHOME)/dist-extras/7z a -mx9 "julia-install-$(JULIA_COMMIT)-$(ARCH).7z" julia-installer.exe
	cd $(BUILDROOT) && cat $(JULIAHOME)/contrib/windows/7zS.sfx $(JULIAHOME)/contrib/windows/7zSFX-config.txt "julia-install-$(JULIA_COMMIT)-$(ARCH).7z" > "$(JULIA_BINARYDIST_FILENAME).exe"
	chmod a+x "$(BUILDROOT)/$(JULIA_BINARYDIST_FILENAME).exe"
	-rm -f $(BUILDROOT)/julia-install-$(JULIA_COMMIT)-$(ARCH).7z
	-rm -f $(BUILDROOT)/julia-installer.exe
else
	cd $(BUILDROOT) && $(TAR) zcvf $(JULIA_BINARYDIST_FILENAME).tar.gz julia-$(JULIA_COMMIT)
endif
	rm -fr $(BUILDROOT)/julia-$(JULIA_COMMIT)

app:
	$(MAKE) -C contrib/mac/app
	@mv contrib/mac/app/$(JULIA_BINARYDIST_FILENAME).dmg $(BUILDROOT)

light-source-dist.tmp: $(BUILDROOT)/doc/_build/html/en/index.html
ifneq ($(BUILDROOT),$(JULIAHOME))
	$(error make light-source-dist does not work in out-of-tree builds)
endif
	# Save git information
	-@$(MAKE) -C $(JULIAHOME)/base version_git.jl.phony

	# Create file light-source-dist.tmp to hold all the filenames that go into the tarball
	echo "base/version_git.jl" > light-source-dist.tmp

	# Download all stdlibs and include the tarball filenames in light-source-dist.tmp
	@$(MAKE) -C stdlib getall NO_GIT=1
	-ls stdlib/srccache/*.tar.gz >> light-source-dist.tmp

	# Exclude git, github and CI config files
	git ls-files | sed -E -e '/^\..+/d' -e '/\/\..+/d' -e '/appveyor.yml/d' >> light-source-dist.tmp
	find doc/_build/html >> light-source-dist.tmp

# Make tarball with only Julia code + stdlib tarballs
light-source-dist: light-source-dist.tmp
	# Prefix everything with "julia-$(commit-sha)/" or "julia-$(version)/" and then create tarball
	# To achieve prefixing, we temporarily create a symlink in the source directory that points back
	# to the source directory.
	sed -e "s_.*_julia-${JULIA_COMMIT}/&_" light-source-dist.tmp > light-source-dist.tmp1
	ln -s . julia-${JULIA_COMMIT}
	tar -cz --no-recursion -T light-source-dist.tmp1 -f julia-$(JULIA_VERSION)_$(JULIA_COMMIT).tar.gz
	rm julia-${JULIA_COMMIT}

source-dist:
	@echo \'source-dist\' target is deprecated: use \'full-source-dist\' instead.

# Make tarball with Julia code plus all dependencies
full-source-dist: light-source-dist.tmp
	# Get all the dependencies downloaded
	@$(MAKE) -C deps getall NO_GIT=1

	# Create file full-source-dist.tmp to hold all the filenames that go into the tarball
	cp light-source-dist.tmp full-source-dist.tmp
	-ls deps/srccache/*.tar.gz deps/srccache/*.tar.bz2 deps/srccache/*.tar.xz deps/srccache/*.tgz deps/srccache/*.zip deps/srccache/*.pem >> full-source-dist.tmp

	# Prefix everything with "julia-$(commit-sha)/" or "julia-$(version)/" and then create tarball
	# To achieve prefixing, we temporarily create a symlink in the source directory that points back
	# to the source directory.
	sed -e "s_.*_julia-${JULIA_COMMIT}/&_" full-source-dist.tmp > full-source-dist.tmp1
	ln -s . julia-${JULIA_COMMIT}
	tar -cz --no-recursion -T full-source-dist.tmp1 -f julia-$(JULIA_VERSION)_$(JULIA_COMMIT)-full.tar.gz
	rm julia-${JULIA_COMMIT}

clean: | $(CLEAN_TARGETS)
	@-$(MAKE) -C $(BUILDROOT)/base clean
	@-$(MAKE) -C $(BUILDROOT)/doc clean
	@-$(MAKE) -C $(BUILDROOT)/src clean
	@-$(MAKE) -C $(BUILDROOT)/ui clean
	@-$(MAKE) -C $(BUILDROOT)/test clean
	@-$(MAKE) -C $(BUILDROOT)/stdlib clean
	-rm -f $(BUILDROOT)/julia
	-rm -f $(BUILDROOT)/*.tar.gz
	-rm -f $(build_depsbindir)/stringreplace \
	   $(BUILDROOT)/light-source-dist.tmp $(BUILDROOT)/light-source-dist.tmp1 \
	   $(BUILDROOT)/full-source-dist.tmp $(BUILDROOT)/full-source-dist.tmp1
	-rm -fr $(build_private_libdir)
# Teporarily add this line to the Makefile to remove extras
	-rm -fr $(build_datarootdir)/julia/extras

cleanall: clean
	@-$(MAKE) -C $(BUILDROOT)/src clean-flisp clean-support
	@-$(MAKE) -C $(BUILDROOT)/deps clean-libuv
	-rm -fr $(build_prefix) $(build_staging)

distcleanall: cleanall
	@-$(MAKE) -C $(BUILDROOT)/stdlib distclean
	@-$(MAKE) -C $(BUILDROOT)/deps distcleanall
	@-$(MAKE) -C $(BUILDROOT)/doc cleanall

.PHONY: default debug release check-whitespace release-candidate \
	julia-debug julia-release julia-stdlib julia-deps julia-deps-libs \
	julia-ui-release julia-ui-debug julia-src-release julia-src-debug \
	julia-symlink julia-base julia-sysimg julia-sysimg-ji julia-sysimg-release julia-sysimg-debug \
	test testall testall1 test clean distcleanall cleanall clean-* \
	run-julia run-julia-debug run-julia-release run \
	install binary-dist light-source-dist.tmp light-source-dist \
	dist full-source-dist source-dist

test: check-whitespace $(JULIA_BUILD_MODE)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test default JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

JULIA_SYSIMG=$(build_private_libdir)/sys$(JULIA_LIBSUFFIX).$(SHLIB_EXT)

testall: check-whitespace $(JULIA_BUILD_MODE)
	cp $(JULIA_SYSIMG) $(BUILDROOT)/local.$(SHLIB_EXT)
	$(call spawn,$(JULIA_EXECUTABLE) -J $(call cygpath_w,$(BUILDROOT)/local.$(SHLIB_EXT)) -e 'true')
	rm $(BUILDROOT)/local.$(SHLIB_EXT)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test all JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

testall1: check-whitespace $(JULIA_BUILD_MODE)
	@env JULIA_CPU_THREADS=1 $(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test all JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

test-%: check-whitespace $(JULIA_BUILD_MODE)
	@$(MAKE) $(QUIET_MAKE) -C $(BUILDROOT)/test $* JULIA_BUILD_MODE=$(JULIA_BUILD_MODE)

# download target for some hardcoded windows dependencies
.PHONY: win-extras wine_path
win-extras:
	[ -d $(JULIAHOME)/dist-extras ] || mkdir $(JULIAHOME)/dist-extras
ifneq ($(BUILD_OS),WINNT)
ifeq (,$(findstring CYGWIN,$(BUILD_OS)))
	cp /usr/lib/p7zip/7z /usr/lib/p7zip/7z.so $(JULIAHOME)/dist-extras
endif
endif
ifneq (,$(filter $(ARCH), i386 i486 i586 i686))
	cd $(JULIAHOME)/dist-extras && \
	$(JLDOWNLOAD) http://downloads.sourceforge.net/sevenzip/7z1805.exe && \
	7z x -y 7z1805.exe 7z.exe 7z.dll && \
	../contrib/windows/winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win32/openSUSE_Leap_42.2 \
		"mingw32-libexpat1 mingw32-zlib1" && \
	cp usr/i686-w64-mingw32/sys-root/mingw/bin/*.dll .
else ifeq ($(ARCH),x86_64)
	cd $(JULIAHOME)/dist-extras && \
	$(JLDOWNLOAD) 7z1805-x64.msi http://downloads.sourceforge.net/sevenzip/7z1805-x64.msi && \
	7z x -y 7z1805-x64.msi _7z.exe _7z.dll && \
	mv _7z.dll 7z.dll && \
	mv _7z.exe 7z.exe && \
	../contrib/windows/winrpm.sh http://download.opensuse.org/repositories/windows:/mingw:/win64/openSUSE_Leap_42.2 \
		"mingw64-libexpat1 mingw64-zlib1" && \
	cp usr/x86_64-w64-mingw32/sys-root/mingw/bin/*.dll .
else
	$(error no win-extras target for ARCH=$(ARCH))
endif
	cd $(JULIAHOME)/dist-extras && \
	$(JLDOWNLOAD) http://downloads.sourceforge.net/sevenzip/7z1805-extra.7z && \
	$(JLDOWNLOAD) https://storage.googleapis.com/google-code-archive-downloads/v2/code.google.com/unsis/nsis-2.46.5-Unicode-setup.exe && \
	chmod a+x 7z.exe && \
	chmod a+x 7z.dll && \
	$(call spawn,./7z.exe) x -y -onsis nsis-2.46.5-Unicode-setup.exe && \
	chmod a+x ./nsis/makensis.exe

# various statistics about the build that may interest the user
ifeq ($(USE_SYSTEM_LLVM), 1)
LLVM_SIZE := llvm-size$(EXE)
else
LLVM_SIZE := $(build_depsbindir)/llvm-size$(EXE)
endif
build-stats:
	@echo $(JULCOLOR)' ==> ./julia binary sizes'$(ENDCOLOR)
	$(call spawn,$(LLVM_SIZE) -A $(call cygpath_w,$(build_private_libdir)/sys.$(SHLIB_EXT)) \
		$(call cygpath_w,$(build_shlibdir)/libjulia.$(SHLIB_EXT)) \
		$(call cygpath_w,$(build_bindir)/julia$(EXE)))
	@echo $(JULCOLOR)' ==> ./julia launch speedtest'$(ENDCOLOR)
	@time $(call spawn,$(build_bindir)/julia$(EXE) -e '')
	@time $(call spawn,$(build_bindir)/julia$(EXE) -e '')
	@time $(call spawn,$(build_bindir)/julia$(EXE) -e '')
