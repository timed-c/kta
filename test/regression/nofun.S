

# TEST 1 - no pipeline, no cache
# Command: kta wcet nofun.S main
# Expected output: WCET: 1 cycle

# TEST 2 - 5 stage pipeline, no cache
# Command: kta wcet nofun.S main -pipeline
# Expected output: WCET: 5 cycles

# TEST 2 - 5 stage pipeline, instruction cache. Cache miss penalty = 100
# Command: kta wcet nofun.S main -pipeline -cache
# Expected output: WCET: 105 cycles


main:
        jr      $ra
