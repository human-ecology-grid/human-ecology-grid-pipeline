.PHONY: all

all: ivea-grid

ifneq ($(shell uname),Linux)
$(error Only Linux builds are supported)
endif

EC := extern/eC
EC_LIB := $(EC)/obj/linux/lib/libecrtStatic.a
DGGAL := extern/dggal
DGGAL_LIB := $(DGGAL)/obj/linux/lib/libdggalStatic.a

extern:
	@mkdir -p extern

$(EC):
	@echo "Fetching eC"
	@git clone -b main --single-branch https://github.com/ecere/eC.git $(EC)

$(DGGAL): dggal-patch.patch
	@[ -d $(DGGAL) ] || git clone -b eC-core --single-branch https://github.com/ecere/dggal.git $(DGGAL)
	@echo "Patching dggal"
	@cd $(DGGAL) && git reset --hard
	@cd $(DGGAL) && git apply ../../dggal-patch.patch

$(EC_LIB): $(EC)
	@cd $(EC) && $(MAKE)

$(DGGAL_LIB): $(DGGAL) $(EC_LIB)
	@cd $(DGGAL) && $(MAKE) -f Makefile.dggal.static

ivea-grid: $(DGGAL_LIB) *.c *.h
	@gcc -std=c17 -static -DEC_STATIC -O3 ivea-grid.c dggal/dggal.c dggal/ecrt.c $(EC_LIB) $(DGGAL_LIB) -lz -o ivea-grid
	@strip ivea-grid
