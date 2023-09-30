all0: all Makefile.smlsharp

Makefile.smlsharp: $(shell find . | grep '.smi$$')
	smlsharp -MMm main.smi > $@

include Makefile.smlsharp


