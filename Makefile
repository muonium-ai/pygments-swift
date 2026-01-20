.PHONY: help clean build test clean-samples

SWIFT_PACKAGE_DIR := pygments-swift
CODEVIEWER_PACKAGE_DIR := codeviewer
CONFIG ?= debug

# codeviewer binary for the chosen config
CODEVIEWER_BIN := ./$(CODEVIEWER_PACKAGE_DIR)/.build/$(CONFIG)/codeviewer

# Default theme for render-samples
THEME ?= github-dark

# Optional fixed width for renders. If empty, images auto-size to content.
WIDTH ?=

# Custom theme file (checked in)
CUSTOM_THEME_FILE ?= themes/custom-theme.json

.PHONY: help clean build test clean-samples codeviewer-build render-samples render-samples-all render-samples-custom

# OCR tooling (system python3 + tesseract)
OCR_SCRIPT ?= python3 tools/ocr_compare_png.py
OCR_DIR ?= out/samples
SHOW_TAILS ?= 0

.PHONY: ocr-check ocr-check-pdf

help:
	@echo "Targets:"
	@echo "  make build   - Build the Swift package"
	@echo "  make test    - Run the Swift test suite"
	@echo "  make codeviewer-build - Build the codeviewer CLI"
	@echo "  make render-samples   - Render sample-code/*.ext to PNG/PDF"
	@echo "  make ocr-check        - OCR-check rendered PNGs for cropping (quiet unless failures)"
	@echo "  make ocr-check-pdf    - Also OCR-check matching PDFs via sips (baseline; set FAIL=1 to fail on CROPPED?)"
	@echo "    Options: SHOW_TAILS=1 (print OCR/source tails on CROPPED?)"
	@echo "  make clean-samples    - Remove generated sample renders"
	@echo "  make clean   - Clean Swift build artifacts"

build:
	swift build --package-path "$(SWIFT_PACKAGE_DIR)"

test:
	swift test --disable-swift-testing --package-path "$(SWIFT_PACKAGE_DIR)"

clean:
	swift package clean --package-path "$(SWIFT_PACKAGE_DIR)"

clean-samples:
	@rm -rf out/samples
	@echo "Removed out/samples"

codeviewer-build:
	swift build -c $(CONFIG) --package-path "$(CODEVIEWER_PACKAGE_DIR)"

render-samples: codeviewer-build
	@mkdir -p out/samples/$(THEME)
	@for f in sample-code/*; do \
		if [ -f "$$f" ]; then \
			if [ -n "$(WIDTH)" ]; then \
				$(CODEVIEWER_BIN) "$$f" --outdir out/samples/$(THEME) --theme $(THEME) --width $(WIDTH); \
			else \
				$(CODEVIEWER_BIN) "$$f" --outdir out/samples/$(THEME) --theme $(THEME); \
			fi; \
		fi; \
	done
	@echo "Rendered outputs in out/samples/$(THEME)"

render-samples-custom: codeviewer-build
	@mkdir -p out/samples/custom
	@test -f "$(CUSTOM_THEME_FILE)" || (echo "Missing custom theme: $(CUSTOM_THEME_FILE)"; exit 2)
	@for f in sample-code/*; do \
		if [ -f "$$f" ]; then \
			if [ -n "$(WIDTH)" ]; then \
				$(CODEVIEWER_BIN) "$$f" --outdir out/samples/custom --theme-file "$(CUSTOM_THEME_FILE)" --width $(WIDTH); \
			else \
				$(CODEVIEWER_BIN) "$$f" --outdir out/samples/custom --theme-file "$(CUSTOM_THEME_FILE)"; \
			fi; \
		fi; \
	done
	@echo "Rendered outputs in out/samples/custom (theme-file: $(CUSTOM_THEME_FILE))"

render-samples-all: codeviewer-build
	@echo "Rendering all built-in themes (config=$(CONFIG))..."
	@themes="$$($(CODEVIEWER_BIN) --list-themes)"; \
	for t in $$themes; do \
		$(MAKE) render-samples THEME="$$t" CONFIG="$(CONFIG)" WIDTH="$(WIDTH)"; \
	done
	@$(MAKE) render-samples-custom CONFIG="$(CONFIG)" WIDTH="$(WIDTH)" CUSTOM_THEME_FILE="$(CUSTOM_THEME_FILE)"
	@echo "Done. Rendered outputs in out/samples/<theme> and out/samples/custom"

ocr-check:
	@test -d "$(OCR_DIR)" || (echo "Missing $(OCR_DIR). Run 'make render-samples-all' first."; exit 2)
	@set -e; \
	any=0; \
	for d in "$(OCR_DIR)"/*; do \
		[ -d "$$d" ] || continue; \
		echo ""; \
		echo "=== OCR check: $$d ==="; \
		tails=""; \
		if [ "$(SHOW_TAILS)" = "1" ]; then tails="--show-tails"; fi; \
		out="$$($(OCR_SCRIPT) --dir "$$d" --only-cropped $$tails)"; \
		echo "$$out"; \
		echo "$$out" | grep -Fq "CROPPED?" && any=1 || true; \
	done; \
	if [ $$any -ne 0 ]; then \
		echo ""; \
		echo "OCR check FAILED (cropped PNGs detected)."; \
		exit 1; \
	fi; \
	echo ""; \
	echo "OCR check passed (no cropped PNGs detected)."

ocr-check-pdf:
	@test -d "$(OCR_DIR)" || (echo "Missing $(OCR_DIR). Run 'make render-samples-all' first."; exit 2)
	@set -e; \
	any=0; \
	for d in "$(OCR_DIR)"/*; do \
		[ -d "$$d" ] || continue; \
		echo ""; \
		echo "=== OCR check (PNG+PDF): $$d ==="; \
		tails=""; \
		if [ "$(SHOW_TAILS)" = "1" ]; then tails="--show-tails"; fi; \
		out="$$($(OCR_SCRIPT) --dir "$$d" --only-cropped $$tails --also-pdf)"; \
		echo "$$out"; \
		echo "$$out" | grep -Fq "CROPPED?" && any=1 || true; \
	done; \
	if [ $$any -ne 0 ]; then \
		echo ""; \
		echo "OCR check found CROPPED? results."; \
		if [ "$(FAIL)" = "1" ]; then \
			echo "OCR check FAILED (FAIL=1)."; \
			exit 1; \
		else \
			echo "Not failing because this is a baseline check. Re-run with FAIL=1 to fail."; \
		fi; \
	fi; \
	echo ""; \
	echo "OCR baseline check finished."
