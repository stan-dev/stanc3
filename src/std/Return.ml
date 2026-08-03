let with_return : 'a 'b. (('a -> 'b) -> 'a) -> 'a =
 fun (type a) f ->
  let live = ref true in
  let exception Return of a in
  let return r =
    if !live then raise_notrace (Return r)
    else failwith "Called return outside of with_return scope!" in
  try
    let res = f return in
    live := false;
    res
  with e -> (
    live := false;
    match e with Return a -> a | _ -> raise e)
