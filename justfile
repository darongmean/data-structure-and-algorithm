# use fish instead of sh:
set shell := ["fish", "-c"]

_default:
	@just -l

# run test-refresh
do-it:
    clojure -M:env/dev:env/test:test-refresh