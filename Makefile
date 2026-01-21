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

.PHONY: help clean build test clean-samples codeviewer-build render-samples render-samples-all render-samples-custom code-stats code-stats-full

# OCR tooling (system python3 + tesseract)
OCR_SCRIPT ?= python3 tools/ocr_compare_png.py
OCR_DIR ?= out/samples
SHOW_TAILS ?= 0
JOBS ?= 1
THEMES ?=

# Themebook tooling (concatenate rendered PDFs per theme)
THEMEBOOK_SCRIPT ?= python3 tools/make_themebook.py
THEMEBOOK_DIR ?= out/themebooks
THEMEBOOK_LABEL ?= filename

.PHONY: ocr-check ocr-check-pdf themebook themebooks themebooks-all

help:
	@echo "Targets:"
	@echo "  make build   - Build the Swift package"
	@echo "  make test    - Run the Swift test suite"
	@echo "  make codeviewer-build - Build the codeviewer CLI"
	@echo "  make render-samples   - Render sample-code/*.ext to PNG/PDF"
	@echo "  make ocr-check        - OCR-check rendered PNGs for cropping (quiet unless failures)"
	@echo "  make ocr-check-pdf    - Also OCR-check matching PDFs via sips (baseline; set FAIL=1 to fail on CROPPED?)"
	@echo "    Options: SHOW_TAILS=1 (print OCR/source tails on CROPPED?)"
	@echo "  make themebook        - Build a single PDF themebook for THEME (out/themebooks/<theme>.pdf)"
	@echo "    Options: THEMEBOOK_LABEL=filename|filename-language|original-ext|filename+original-ext"
	@echo "  make themebooks       - Build themebooks for all themes (use THEMES=... to filter)"
	@echo "  make themebooks-all   - Build themebooks for all built-in themes + custom"
	@echo "  make code-stats       - Print first/last commit dates"
	@echo "  make code-stats-full  - Print repo stats (commits + total lines)"
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

themebook: codeviewer-build
	@test -d "$(OCR_DIR)/$(THEME)" || (echo "Missing $(OCR_DIR)/$(THEME). Run 'make render-samples THEME=$(THEME)' (or render-samples-all) first."; exit 2)
	@mkdir -p "$(THEMEBOOK_DIR)"
	@extra=""; \
	if [ "$(THEME)" = "custom" ]; then \
	  extra="--theme-file $(CUSTOM_THEME_FILE)"; \
	else \
	  extra="--theme $(THEME)"; \
	fi; \
	$(THEMEBOOK_SCRIPT) --dir "$(OCR_DIR)/$(THEME)" --out "$(THEMEBOOK_DIR)/$(THEME).pdf" --label "$(THEMEBOOK_LABEL)" --codeviewer "$(CODEVIEWER_BIN)" $$extra
	@echo "Wrote $(THEMEBOOK_DIR)/$(THEME).pdf"

themebooks:
	@set -e; \
	if [ -z "$(THEMES)" ]; then \
	  $(MAKE) themebooks-all THEMEBOOK_LABEL="$(THEMEBOOK_LABEL)"; \
	else \
	  for t in $(THEMES); do \
	    $(MAKE) themebook THEME="$$t" THEMEBOOK_LABEL="$(THEMEBOOK_LABEL)"; \
	  done; \
	fi

themebooks-all: codeviewer-build
	@themes="$$($(CODEVIEWER_BIN) --list-themes)"; \
	for t in $$themes custom; do \
		$(MAKE) themebook THEME="$$t" THEMEBOOK_LABEL="$(THEMEBOOK_LABEL)"; \
	done

code-stats:
	@command -v git >/dev/null 2>&1 || (echo "Error: git not found"; exit 2)
	@git rev-parse --is-inside-work-tree >/dev/null 2>&1 || (echo "Error: not a git repo"; exit 2)
	@first_commit=$$(git log --reverse --format='%H' | head -n 1); \
	last_commit=$$(git rev-parse HEAD); \
	first_date=$$(git show -s --format='%ad' --date=short $$first_commit); \
	last_date=$$(git show -s --format='%ad' --date=short $$last_commit); \
	echo "first_commit_date=$$first_date"; \
	echo "last_commit_date=$$last_date"

code-stats-full:
	@command -v git >/dev/null 2>&1 || (echo "Error: git not found"; exit 2)
	@git rev-parse --is-inside-work-tree >/dev/null 2>&1 || (echo "Error: not a git repo"; exit 2)
	@echo "Repo: $$(git rev-parse --show-toplevel)"
	@echo "Branch: $$(git branch --show-current 2>/dev/null || true)"
	@echo ""
	@echo "Commits:"
	@echo "  total: $$(git rev-list --count HEAD)"
	@first_commit=$$(git log --reverse --format='%H' | head -n 1); \
	last_commit=$$(git rev-parse HEAD); \
	first_date=$$(git show -s --format='%ad' --date=short $$first_commit); \
	last_date=$$(git show -s --format='%ad' --date=short $$last_commit); \
	echo "  first commit date: $$first_date"; \
	echo "  last commit date:  $$last_date"
	@echo ""
	@echo "Lines (tracked files):"
	@total_lines=$$(git ls-files -z | xargs -0 wc -l 2>/dev/null | tail -n 1 | awk '{print $$1}'); \
	echo "  total: $$total_lines"; \
	echo "  note:  counts lines across all git-tracked files"

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
	start=$$(python3 -c 'import time; print(time.time())'); \
	if [ -n "$(THEMES)" ]; then \
	  dirs=""; \
	  for t in $(THEMES); do dirs="$$dirs $(OCR_DIR)/$$t"; done; \
	else \
	  dirs="$(OCR_DIR)/*"; \
	fi; \
	if [ "$(JOBS)" -gt 1 ]; then \
		echo "Running OCR check in parallel (JOBS=$(JOBS))..."; \
		tails=""; \
		if [ "$(SHOW_TAILS)" = "1" ]; then tails="--show-tails"; fi; \
		TAILS="$$tails"; export TAILS; \
		status=0; \
		for d in $$dirs; do \
			[ -d "$$d" ] || continue; \
			echo "$$d"; \
		done | xargs -n 1 -P $(JOBS) sh -c 'd="$$1"; echo ""; echo "=== OCR check: $$d ==="; $(OCR_SCRIPT) --dir "$$d" --only-cropped $$TAILS --fail-if-cropped' sh || status=$$?; \
		end=$$(python3 -c 'import time; print(time.time())'); \
		total=$$(python3 -c 'import sys; print(f"{float(sys.argv[1]) - float(sys.argv[2]):.2f}s")' "$$end" "$$start"); \
		echo ""; \
		echo "Total time: $$total"; \
		if [ $$status -ne 0 ]; then \
			echo ""; \
			echo "OCR check FAILED (cropped PNGs detected)."; \
			exit 1; \
		fi; \
		echo ""; \
		echo "OCR check passed (no cropped PNGs detected)."; \
	else \
		any=0; \
		for d in $$dirs; do \
			[ -d "$$d" ] || continue; \
			echo ""; \
			echo "=== OCR check: $$d ==="; \
			tails=""; \
			if [ "$(SHOW_TAILS)" = "1" ]; then tails="--show-tails"; fi; \
			out="$$($(OCR_SCRIPT) --dir "$$d" --only-cropped $$tails)"; \
			echo "$$out"; \
			echo "$$out" | grep -Fq "CROPPED?" && any=1 || true; \
		done; \
		end=$$(python3 -c 'import time; print(time.time())'); \
		total=$$(python3 -c 'import sys; print(f"{float(sys.argv[1]) - float(sys.argv[2]):.2f}s")' "$$end" "$$start"); \
		echo ""; \
		echo "Total time: $$total"; \
		if [ $$any -ne 0 ]; then \
			echo ""; \
			echo "OCR check FAILED (cropped PNGs detected)."; \
			exit 1; \
		fi; \
		echo ""; \
		echo "OCR check passed (no cropped PNGs detected)."; \
	fi

ocr-check-pdf:
	@test -d "$(OCR_DIR)" || (echo "Missing $(OCR_DIR). Run 'make render-samples-all' first."; exit 2)
	@set -e; \
	start=$$(python3 -c 'import time; print(time.time())'); \
	any=0; \
	if [ -n "$(THEMES)" ]; then \
	  dirs=""; \
	  for t in $(THEMES); do dirs="$$dirs $(OCR_DIR)/$$t"; done; \
	else \
	  dirs="$(OCR_DIR)/*"; \
	fi; \
	if [ "$(JOBS)" -gt 1 ]; then \
		echo "Running OCR check (PNG+PDF) in parallel (JOBS=$(JOBS))..."; \
		tails=""; \
		if [ "$(SHOW_TAILS)" = "1" ]; then tails="--show-tails"; fi; \
		TAILS="$$tails"; export TAILS; \
		status=0; \
		for d in $$dirs; do \
			[ -d "$$d" ] || continue; \
			echo "$$d"; \
		done | xargs -n 1 -P $(JOBS) sh -c 'd="$$1"; echo ""; echo "=== OCR check (PNG+PDF): $$d ==="; $(OCR_SCRIPT) --dir "$$d" --only-cropped $$TAILS --also-pdf --fail-if-cropped' sh || status=$$?; \
		[ $$status -ne 0 ] && any=1 || true; \
	else \
		for d in $$dirs; do \
			[ -d "$$d" ] || continue; \
			echo ""; \
			echo "=== OCR check (PNG+PDF): $$d ==="; \
			tails=""; \
			if [ "$(SHOW_TAILS)" = "1" ]; then tails="--show-tails"; fi; \
			out="$$($(OCR_SCRIPT) --dir "$$d" --only-cropped $$tails --also-pdf)"; \
			echo "$$out"; \
			echo "$$out" | grep -Fq "CROPPED?" && any=1 || true; \
		done; \
	fi; \
	end=$$(python3 -c 'import time; print(time.time())'); \
	total=$$(python3 -c 'import sys; print(f"{float(sys.argv[1]) - float(sys.argv[2]):.2f}s")' "$$end" "$$start"); \
	echo ""; \
	echo "Total time: $$total"; \
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
