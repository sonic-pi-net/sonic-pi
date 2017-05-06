LANGUAGES := $$(find * -maxdepth 0 -type d |cut -d/ -f2|grep -v '^en$$')

all: generate-translations

status:
	@for f in sonic-pi-tutorial-??.po; do \
		printf "$${f}: "; \
		msgfmt --statistics --output /dev/null $${f}; \
	done

po4a.cfg: en/*.md Makefile
	( \
	  printf "[po_directory] .\n[po4a_paths] sonic-pi-tutorial.pot\n\n"; \
	  for f in en/*.md; do printf '[type: text] %s\n' $$f; done | LC_ALL=C sort \
	) > po4a.cfg


sonic-pi-tutorial.pot: en/*.md po4a.cfg
	po4a --no-translations po4a.cfg

# This build rule generate the translated markdown files from .po
# files.  Note that fuzzy translations are ignored and at least 80% of
# the file must be translated for the file to be generated.
generate-translations: *.po
	for pofile in sonic-pi-tutorial-??.po; do \
		lang=$$(basename $$pofile .po | sed 's/sonic-pi-tutorial-//'); \
		printf "\n******** Generating $${lang} ********\n\n"; \
		if [ -e sonic-pi-tutorial-$${lang}.po ]; then \
		    for f in $$(cd en; ls *.md); do \
			po4a-translate \
				-k 1 \
				-f text \
				-m en/$${f} \
				-p sonic-pi-tutorial-$${lang}.po \
				-l $${lang}/$${f}; \
		    done; \
		else echo "Missing $${lang}.po"; fi; \
	done

sonic-pi-tutorial-%.po: sonic-pi-tutorial.pot
	msgmerge $@ $^ -o $@

# This build rule should only be used once per language, when the translated
# markdown files are converted to .po files.  The conversion is a bit
# shaky and might fail.  Check the result carefully before commiting.
#
# To migrate 'de' translation, run 'make LANGUAGES=de import-translations'
# and check the content of sonic-pi-tutorial-de.po and
# sonic-pi-tutorial-de-failed.po.  The migration require the file structure of
# the English original and the translated file to match.  If the migration
# have problems, try modifying the translated file structure to match the
# English version.
import-translations:
	for lang in $(LANGUAGES); do \
		for f in $$(cd en; ls *.md); do \
			po4a-gettextize \
				--localized-charset UTF-8 \
				--master-charset UTF-8 \
				-f text \
				-m en/$${f} \
				-l $${lang}/$${f} \
				-p $${f}-$${lang}.po; \
			if [ -e gettextization.failed.po ] ; then \
				mv gettextization.failed.po \
					$${f}-$${lang}-failed.po; \
			fi; \
		done ; \
		msgcat -o sonic-pi-tutorial-$${lang}.po *.md-$${lang}.po; \
		for f in *.md-$${lang}-failed.po ; do \
			msgcat -o sonic-pi-tutorial-$${lang}-failed.po $${f}; \
			rm -f $${f}; \
		done; \
		rm -f *.md-$${lang}.po; \
	done
