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
		+RTS -N

json: $(CABAL_FILES)
	cabal build all
	find -L ~/org/ -name '*.org' -type f	\
	    | cabal run org-main:exe:org --	\
		-c ~/org/org.dot		\
		json				\
		--output ~/.cache/org-jw-json	\
		-F -				\
		+RTS -N

trip: $(CABAL_FILES)
	cabal build all
	cabal run org-main:exe:org -- -c ~/org/org.dot \
		trip ~/org/journal/202409101058-meeting-leads-offsite-q3-2024.org

stats: $(CABAL_FILES)
	cabal build all
	cabal run org-main:exe:org -- -c ~/org/org.dot \
		stats ~/org/todo.org

meeting-stats: $(CABAL_FILES)
	cabal build all
	find -L ~/org/journal/ -name '*.org' -type f -print0	\
	    | xargs -0 egrep -l '^#\+filetags.*:kadena:'	\
	    | cabal run org-main:exe:org --			\
		-c ~/org/org.dot				\
		stats						\
		-F -

stats-all: $(CABAL_FILES)
	cabal build all
	find -L ~/org/ -name '*.org' -type f	\
	    | cabal run org-main:exe:org --	\
		-c ~/org/org.dot		\
		stats				\
		-F -

round-trip: $(CABAL_FILES)
	cabal build all
	find -L ~/org/ -name '*.org' -type f	\
	    | cabal run org-main:exe:org --	\
		-c ~/org/org.dot		\
		trip				\
		--change-in-place		\
		-F -				\
		+RTS -N

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
