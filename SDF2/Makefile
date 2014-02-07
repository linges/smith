PACK_SDF = java -cp ../lib/strategoxt.jar run org.strategoxt.tools.main-pack-sdf 
SDF2TABLE = sdf2table


SDFINCLUDE = .

SDFSOURCES = \
	BinderLang/Module.sdf \
	BinderLang/Term.sdf \
	BinderLang/Type.sdf \
	BinderLang/Kind.sdf \
	BinderLang/Lexical.sdf \
	BinderLang/SyntaxExtension.sdf

TARGETS = BinderLang.def BinderLang.tbl

all: $(TARGETS)

BinderLang.def: $(SDFSOURCES)
	$(PACK_SDF) -I $(SDFINCLUDE) -of txt -i BinderLang/Module.sdf -o $@

BinderLang.tbl: BinderLang.def
	$(SDF2TABLE) -i $^ -o $@ -m BinderLang/Module

clean :
	rm -f $(TARGETS)
