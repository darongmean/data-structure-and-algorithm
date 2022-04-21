# use fish instead of sh:
set shell := ["fish", "-c"]

_default:
	@just -l

# run test-refresh
do-it:
    clojure -M:env/dev:env/test:test-refresh

# see https://github.com/jgpc42/jmh-clojure-task
benchmark:
    mkdir -p classes
    mkdir -p benchmarks
    clojure -X:env/test:jmh :format :table, :status true > benchmarks/README.txt

# see https://github.com/jgpc42/jmh-clojure-task
quick-bench:
    mkdir -p classes
    mkdir -p benchmarks
    clojure -X:env/test:jmh :format :table, :status false
