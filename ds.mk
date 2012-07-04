GOS2GO = cmd/gos2go/gos2go

FILES = \
	ds_builtin_gos.go \
	ds_complex_gos.go \
	ds_error_gos.go \
	ds_inexact_gos.go \
	ds_list_gos.go \
	ds_string_gos.go \
	ds_vector_gos.go

.PHONY: all
all: $(FILES)

%_gos.go: %.gos
	$(GOS2GO) $< > $@
