

let counter = ref 0

let config_max_batch_size = ref 4

let config_max_cycles = ref 1000000
                            
let set_max_batch_size size =
  config_max_batch_size := size

let set_max_cycles cycles =
  config_max_cycles := cycles
                         
let count() = counter := !counter + 1;
  
