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

help:
	@echo "Targets:"
	@echo "  make build   - Build the Swift package"
	@echo "  make test    - Run the Swift test suite"
	@echo "  make codeviewer-build - Build the codeviewer CLI"
	@echo "  make render-samples   - Render sample-code/*.ext to PNG/PDF"
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
