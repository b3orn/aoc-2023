src_dir := src
ebin_dir := ebin


src_files := $(wildcard $(src_dir)/*.erl)
beam_files := $(patsubst $(src_dir)/%.erl,$(ebin_dir)/%.beam,$(src_files))


.PHONY: all clean

all: $(beam_files)

clean:
	-rm -r $(ebin_dir)

$(ebin_dir)/%.beam: $(src_dir)/%.erl
	mkdir -p $(dir $@)
	erlc -o $(ebin_dir) $<
