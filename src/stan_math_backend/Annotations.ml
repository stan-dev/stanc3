open Core

let recognized_annotation a = List.mem ["extern"] a ~equal:String.equal
