# Church Encodings

Code demonstrating a reactive stream implementation with and without Church encoded representation.

Code:
 - `baseline`: the baseline non-Church encoded implementation that contains a flaw
 - `termination`: the baseline implementation with the flaw fixed
 - `partial`: partially Church encoded and partially transformed to continuation passing style (CPS)
 - `church`: fully Church encoded and CPSed

Benchmark Results:
[info] Benchmark                              Mode  Cnt    Score   Error  Units
[info] church.StreamBenchmark.zipAndAdd       avgt  200  517.525 ± 3.252  ms/op
[info] partial.StreamBenchmark.zipAndAdd      avgt  200  387.252 ± 2.165  ms/op
[info] termination.StreamBenchmark.zipAndAdd  avgt  200  532.190 ± 2.724  ms/op
