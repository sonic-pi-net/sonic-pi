# -*- makefile -*-

include ${srcdir}/libffi.gnu.mk

$(LIBFFI):		
	@mkdir -p "$(LIBFFI_BUILD_DIR)" "$@(D)"
	@if [ ! -f "$(LIBFFI_BUILD_DIR)"/Makefile ]; then \
	    echo "Configuring libffi"; \
	    cd "$(LIBFFI_BUILD_DIR)" && \
		/usr/bin/env CFLAGS="$(LIBFFI_CFLAGS)" GREP_OPTIONS="" \
		/bin/sh $(LIBFFI_CONFIGURE) $(LIBFFI_HOST) > /dev/null; \
	fi
	$(MAKE) -C "$(LIBFFI_BUILD_DIR)"
