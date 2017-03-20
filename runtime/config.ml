

let counter = ref 0

let config_max_batch_size = ref 4

let set_max_batch_size size =
  config_max_batch_size := size

let count() = counter := !counter + 1;
  
