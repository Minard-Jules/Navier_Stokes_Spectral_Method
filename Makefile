# Compiler
FC = gfortran
FCFLAGS = -O3 -Wall -Wextra
LDFLAGS = 

# Directories
SRC_DIR = src
APP_DIR = app
BUILD_DIR = build
DEPS_DIR = $(BUILD_DIR)/dependencies
OBJ_DIR = $(BUILD_DIR)/obj
BIN_DIR = $(BUILD_DIR)/bin

# Dependencies
GTK_FORTRAN_REPO = https://github.com/vmagnin/gtkfortran.git
NAFPACK_REPO = https://github.com/Minard-Jules/NAFPack.git

# Find Fortran source files
SRCS = $(wildcard $(SRC_DIR)/*.f90) $(APP_DIR)/main.f90
OBJS = $(SRCS:%.f90=$(OBJ_DIR)/%.o)

# Executable
TARGET = $(BIN_DIR)/solver

# Default target
all: $(TARGET)

# Compile the Fortran sources
$(OBJ_DIR)/%.o: %.f90 | $(OBJ_DIR)
	$(FC) $(FCFLAGS) -c $< -o $@

# Link the executable
$(TARGET): $(OBJS) | $(BIN_DIR)
	$(FC) $(FCFLAGS) $(OBJS) -o $(TARGET) $(LDFLAGS)

# Create necessary directories
$(OBJ_DIR) $(BIN_DIR):
	mkdir -p $@

# Install dependencies
$(DEPS_DIR)/gtk-fortran:
	git clone $(GTK_FORTRAN_REPO) $(DEPS_DIR)/gtk-fortran
	cd $(DEPS_DIR)/gtk-fortran && mkdir build && cd build && cmake .. && make && make install

$(DEPS_DIR)/NAFPack:
	git clone $(NAFPACK_REPO) $(DEPS_DIR)/NAFPack
	cd $(DEPS_DIR)/NAFPack && mkdir build && cd build && cmake .. && make && make install

install-deps: $(DEPS_DIR)/gtk-fortran $(DEPS_DIR)/NAFPack

# Clean
clean:
	rm -rf $(BUILD_DIR)

.PHONY: all install-deps clean
