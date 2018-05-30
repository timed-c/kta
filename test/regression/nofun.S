

# TEST 1 - no pipeline, no cache
# Command: kta wcet nofun.S main
# Expected output: WCET: 2 cycle

# TEST 2 - 5 stage pipeline, no cache
# Command: kta wcet nofun.S main -pipeline
# Expected output: WCET: 6 cycles

# TEST 2 - 5 stage pipeline, instruction cache. Cache miss penalty = 100
# Command: kta wcet nofun.S main -pipeline -cache
# Expected output: WCET: 16 cycles


main:
        jr      $ra
