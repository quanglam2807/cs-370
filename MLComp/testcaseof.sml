let val x = "hello"
in
  println
    (case x of 
      "hello1" => 8
    | "hello2" => 5
    | "hello" => 1
    | "hello4" => 3)
end