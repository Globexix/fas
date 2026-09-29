type failure = {
  argv : string array;
  status : Unix.process_status;
  stdout : string;
  stderr : string;
}

val run : string array -> (string * string, failure) result
val run_to_file : string array -> string -> (string, failure) result
