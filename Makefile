CABAL_FILES =					\
     flatparse-util/flatparse-util.cabal	\
     org-data/org-data.cabal			\
     org-filetags/org-filetags.cabal		\
     org-cbor/org-cbor.cabal			\
     org-json/org-json.cabal			\
     org-lint/org-lint.cabal			\
     org-main/org-main.cabal			\
     org-parse/org-parse.cabal			\
     org-print/org-print.cabal			\
     org-site/org-site.cabal			\
     org-types/org-types.cabal

all: $(CABAL_FILES)
	cabal build all
	find -L ~/org/ -name '*.org' -type f		\
	    | time cabal run org-main:exe:org --	\
		-c ~/org/org.dot			\
		lint					\
		--check-dir ~/.local/share/org-jw       \
		-l INFO					\
		-F -					\
		+RTS -p

flatparse-util/flatparse-util.cabal: flatparse-util/package.yaml
	(cd flatparse-util; hpack -f)

org-data/org-data.cabal: org-data/package.yaml
	(cd org-data; hpack -f)

org-filetags/org-filetags.cabal: org-filetags/package.yaml
	(cd org-filetags; hpack -f)

org-cbor/org-cbor.cabal: org-cbor/package.yaml
	(cd org-cbor; hpack -f)

org-json/org-json.cabal: org-json/package.yaml
	(cd org-json; hpack -f)

org-lint/org-lint.cabal: org-lint/package.yaml
	(cd org-lint; hpack -f)

org-main/org-main.cabal: org-main/package.yaml
	(cd org-main; hpack -f)

org-parse/org-parse.cabal: org-parse/package.yaml
	(cd org-parse; hpack -f)

org-print/org-print.cabal: org-print/package.yaml
	(cd org-print; hpack -f)

org-site/org-site.cabal: org-site/package.yaml
	(cd org-site; hpack -f)

org-types/org-types.cabal: org-types/package.yaml
	(cd org-types; hpack -f)
