.PHONY: help clean build test clean-samples

SWIFT_PACKAGE_DIR := pygments-swift
CODEVIEWER_PACKAGE_DIR := codeviewer

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
	swift build --package-path "$(CODEVIEWER_PACKAGE_DIR)"

render-samples: codeviewer-build
	@mkdir -p out/samples/$(THEME)
	@for f in sample-code/*; do \
		if [ -f "$$f" ]; then \
			if [ -n "$(WIDTH)" ]; then \
				./codeviewer/.build/debug/codeviewer "$$f" --outdir out/samples/$(THEME) --theme $(THEME) --width $(WIDTH); \
			else \
				./codeviewer/.build/debug/codeviewer "$$f" --outdir out/samples/$(THEME) --theme $(THEME); \
			fi; \
		fi; \
	done
	@echo "Rendered outputs in out/samples/$(THEME)"

# Default theme for render-samples
THEME ?= github-dark

# Optional fixed width for renders. If empty, images auto-size to content.
WIDTH ?=
