.PHONY: help clean build test

SWIFT_PACKAGE_DIR := pygments-swift

help:
	@echo "Targets:"
	@echo "  make build   - Build the Swift package"
	@echo "  make test    - Run the Swift test suite"
	@echo "  make clean   - Clean Swift build artifacts"

build:
	swift build --package-path "$(SWIFT_PACKAGE_DIR)"

test:
	swift test --disable-swift-testing --package-path "$(SWIFT_PACKAGE_DIR)"

clean:
	swift package clean --package-path "$(SWIFT_PACKAGE_DIR)"
