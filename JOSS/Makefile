FILE = covid19analytics_JOSS

SOURCE = ${FILE}.md
TARGET = ${FILE}.pdf

OPTS = --filter pandoc-citeproc

${TARGET} : ${SOURCE}
	pandoc  ${OPTS}  -o $@ $^

clean :
	rm -v ${TARGET}
