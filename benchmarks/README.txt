# JMH version: 1.32
# VM version: JDK 17.0.2, OpenJDK 64-Bit Server VM, 17.0.2+8
# VM invoker: /Users/darong/.sdkman/candidates/java/17.0.2-tem/bin/java
# VM options: -Dclojure.basis=.cpcache/4253354531.basis
# Blackhole mode: full + dont-inline hint
# Warmup: 5 iterations, 10 us each
# Measurement: 10 iterations, 10 us each, 10 calls per op
# Timeout: 10 s per iteration
# Threads: 10 threads, will synchronize iterations
# Benchmark mode: Average time, time/op
# Benchmark: jmh_1650531552105.bench._000_steep

# Run progress: 0.00% complete, ETA 00:00:00
# Fork: 1 of 1
# Warmup Iteration   1: 62.115 ±(99.9%) 285.059 us/op
# Warmup Iteration   2: 6.623 ±(99.9%) 28.475 us/op
# Warmup Iteration   3: 1.411 ±(99.9%) 3.133 us/op
# Warmup Iteration   4: 9.256 ±(99.9%) 21.358 us/op
# Warmup Iteration   5: 0.564 ±(99.9%) 0.252 us/op
Iteration   1: 11.050 ±(99.9%) 22.927 us/op
Iteration   2: 79.537 ±(99.9%) 281.118 us/op
Iteration   3: 28.989 ±(99.9%) 72.772 us/op
Iteration   4: 47.999 ±(99.9%) 170.653 us/op
Iteration   5: 345.166 ±(99.9%) 1614.842 us/op
Iteration   6: 24.946 ±(99.9%) 50.536 us/op
Iteration   7: 8.740 ±(99.9%) 11.458 us/op
Iteration   8: 78.885 ±(99.9%) 220.489 us/op
Iteration   9: 2.799 ±(99.9%) 6.086 us/op
Iteration  10: 93.607 ±(99.9%) 353.387 us/op


Result "jmh_1650531552105.bench._000_steep":
  72.172 ±(99.9%) 153.135 us/op [Average]
  (min, avg, max) = (2.799, 72.172, 345.166), stdev = 101.289
  CI (99.9%): [≈ 0, 225.307] (assumes normal distribution)


# JMH version: 1.32
# VM version: JDK 17.0.2, OpenJDK 64-Bit Server VM, 17.0.2+8
# VM invoker: /Users/darong/.sdkman/candidates/java/17.0.2-tem/bin/java
# VM options: -Dclojure.basis=.cpcache/4253354531.basis
# Blackhole mode: full + dont-inline hint
# Warmup: 5 iterations, 10 us each
# Measurement: 10 iterations, 10 us each, 10 calls per op
# Timeout: 10 s per iteration
# Threads: 10 threads, will synchronize iterations
# Benchmark mode: Average time, time/op
# Benchmark: jmh_1650531552105.bench._001_steep_02

# Run progress: 50.00% complete, ETA 00:00:01
# Fork: 1 of 1
# Warmup Iteration   1: 1.403 ±(99.9%) 1.947 us/op
# Warmup Iteration   2: 2.412 ±(99.9%) 5.248 us/op
# Warmup Iteration   3: 0.742 ±(99.9%) 1.303 us/op
# Warmup Iteration   4: 3.815 ±(99.9%) 15.819 us/op
# Warmup Iteration   5: 0.526 ±(99.9%) 0.316 us/op
Iteration   1: 102.569 ±(99.9%) 465.420 us/op
Iteration   2: 21.759 ±(99.9%) 19.145 us/op
Iteration   3: 17.726 ±(99.9%) 21.565 us/op
Iteration   4: 16.306 ±(99.9%) 16.672 us/op
Iteration   5: 12.227 ±(99.9%) 8.906 us/op
Iteration   6: 6.458 ±(99.9%) 3.942 us/op
Iteration   7: 52.329 ±(99.9%) 179.149 us/op
Iteration   8: 19.614 ±(99.9%) 43.579 us/op
Iteration   9: 108.463 ±(99.9%) 455.544 us/op
Iteration  10: 17.508 ±(99.9%) 50.357 us/op


Result "jmh_1650531552105.bench._001_steep_02":
  37.496 ±(99.9%) 57.237 us/op [Average]
  (min, avg, max) = (6.458, 37.496, 108.463), stdev = 37.859
  CI (99.9%): [≈ 0, 94.733] (assumes normal distribution)


# Run complete. Total time: 00:00:01

REMEMBER: The numbers below are just data. To gain reusable insights, you need to follow up on
why the numbers are the way they are. Use profilers (see -prof, -lprof), design factorial
experiments, perform baseline and negative tests that provide experimental control, make sure
the benchmarking environment is safe on JVM/OS/HW level, ask for reviews from the domain experts.
Do not assume the numbers tell you what you want them to tell.

Benchmark          Mode  Cnt   Score     Error  Units
j.b._000_steep     avgt   10  72.172 ± 153.135  us/op
j.b._001_steep_02  avgt   10  37.496 ±  57.237  us/op

:benchmark                                    :mode     :samples  :score         :score-error
--------------------------------------------  --------  --------  -------------  ------------
adwh.basic.accumulating-and-tupling/steep     :average  10        72.172  us/op  153.135     
adwh.basic.accumulating-and-tupling/steep-02  :average  10        37.496  us/op  57.237      
