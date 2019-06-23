
# set the compiler and proper flags
FC     = gfortran
FC_FLAGS = -std=f95 -Wall
STANDARD_FLAGS = -O3
DEBUG_FLAGS = -Og
FC_INC = 

#set object
OBJ = duschinsky

#set the directory for object files, module files and sources
BIN_DIR   = .
BUILD_DIR = build
MOD_DIR = $(BUILD_DIR)/mod
OBJ_DIR = $(BUILD_DIR)/obj
SRC_DIR = src

SCRIPT_DIR = script

# set sources files. The order is mandatory: if a file is dependent
# from another, simple put it after the dependance in the list

FILES =  various/kinds.F90      		 \
		   various/parameters.F90 		 \
			tools/string_tools.F90      \
			in_out/file_info.F90        \
			in_out/input_file.F90       \
			array_info.F90              \
			external/gaussian_input.F90 \
			external/external_files.F90 \
		   $(OBJ).F90

SRC_FILES := $(addprefix $(SRC_DIR)/,$(FILES))
SRC_OBJS := $(patsubst %.F90,$(OBJ_DIR)/%.o,$(FILES))

OBJS_DIR := $(dir $(SRC_OBJS))

.PHONY: all directories debug clean

all: FC_FLAGS += $(STANDARD_FLAGS)
all: directories $(BIN_DIR)/$(OBJ)

debug: FC_FLAGS += $(DEBUG_FLAGS)
debug: directories $(BIN_DIR)/$(OBJ)

test:


$(BIN_DIR)/$(OBJ): $(SRC_OBJS)
	$(FC) $(FC_FLAGS) -J$(MOD_DIR) -o $@ $^

$(SRC_OBJS):$(OBJ_DIR)/%.o: $(SRC_DIR)/%.F90
	$(FC) $(FC_FLAGS) -J$(MOD_DIR) -c -o $@ $^

# make the directories for necessary build
directories:
	@mkdir -p $(BUILD_DIR)
	@mkdir -p $(OBJS_DIR)
	@mkdir -p $(MOD_DIR)
	@mkdir -p $(BIN_DIR)

clean:
	rm -rf $(OBJ_DIR) $(MOD_DIR) $(BIN_DIR)/$(OBJ)