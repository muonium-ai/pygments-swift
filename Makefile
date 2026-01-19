.PHONY: help clean build test

SWIFT_PACKAGE_DIR := pygments-swift
CODEVIEWER_PACKAGE_DIR := codeviewer

help:
	@echo "Targets:"
	@echo "  make build   - Build the Swift package"
	@echo "  make test    - Run the Swift test suite"
	@echo "  make codeviewer-build - Build the codeviewer CLI"
	@echo "  make clean   - Clean Swift build artifacts"

build:
	swift build --package-path "$(SWIFT_PACKAGE_DIR)"

test:
	swift test --disable-swift-testing --package-path "$(SWIFT_PACKAGE_DIR)"

clean:
	swift package clean --package-path "$(SWIFT_PACKAGE_DIR)"

codeviewer-build:
	swift build --package-path "$(CODEVIEWER_PACKAGE_DIR)"
