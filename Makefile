.PHONY: all papers haskell python clean clean-latex help

# Default target
all: papers haskell python

# Build all LaTeX papers
papers:
	@echo "Building LaTeX papers..."
	@find papers -name "*.tex" -type f | while read tex; do \
		dir=$$(dirname "$$tex"); \
		cd "$$dir" && pdflatex -interaction=nonstopmode -output-directory=. "$$(basename $$tex)" || true; \
		cd - > /dev/null; \
	done
	@echo "Papers built successfully."

# Build Haskell project
haskell:
	@echo "Building Haskell project..."
	cd src/haskell && stack build

# Install Python dependencies
python:
	@echo "Installing Python dependencies..."
	pip install -r src/python/requirements.txt

# Clean LaTeX build artifacts
clean-latex:
	@echo "Cleaning LaTeX artifacts..."
	@find . -name "*.aux" -o -name "*.log" -o -name "*.out" -o -name "*.toc" \
		-o -name "*.lof" -o -name "*.lot" -o -name "*.fls" -o -name "*.fdb_latexmk" \
		-o -name "*.synctex.gz" -o -name "*.bbl" -o -name "*.blg" -o -name "*.bcf" \
		-o -name "*.xml" -o -name "*.run.xml" -o -name "*.nav" -o -name "*.snm" \
		-o -name "*.vrb" | xargs rm -f

# Clean all build artifacts
clean: clean-latex
	@echo "Cleaning Haskell build artifacts..."
	cd src/haskell && stack clean
	@echo "Cleaning Python artifacts..."
	find . -type d -name "__pycache__" -exec rm -rf {} + 2>/dev/null || true
	find . -name "*.pyc" -delete

# Help target
help:
	@echo "Functorial Physics Build System"
	@echo "==============================="
	@echo ""
	@echo "Available targets:"
	@echo "  all         - Build everything (papers, Haskell, Python)"
	@echo "  papers      - Build all LaTeX papers"
	@echo "  haskell     - Build Haskell project"
	@echo "  python      - Install Python dependencies"
	@echo "  clean       - Remove all build artifacts"
	@echo "  clean-latex - Remove only LaTeX build artifacts"
	@echo "  help        - Show this help message"