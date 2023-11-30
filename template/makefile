# Define the Erlang compiler
ERLC = erlc

# Define the escript to run
MAIN_SCRIPT = main.erl

# Find all .erl files in the current directory except main.erl
ERL_SOURCES = $(filter-out main.erl, $(wildcard *.erl))

# Define the .beam files from .erl sources
BEAM_FILES = $(ERL_SOURCES:.erl=.beam)

# Default target
all: compile run

# Compile all .erl files to .beam
compile: $(BEAM_FILES)

# Rule to compile .erl to .beam
%.beam: %.erl
	$(ERLC) $<

# Run the main script
run:
	./$(MAIN_SCRIPT) input.txt

# Clean up beam files
clean:
	rm -f $(BEAM_FILES)

