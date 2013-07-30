val terminaleB :
  'a -> ('b -> bool) -> ('b -> 'b) -> ('b -> 'a -> 'a) -> 'b -> 'a
val iterativeBWhile :
  'a -> ('b -> bool) -> ('b -> 'b) -> ('b -> 'a -> 'a) -> 'b -> 'a
val iterativeBFor :
  'a -> ('b -> bool) -> ('b -> 'b) -> ('b -> 'a -> 'a) -> 'b -> 'a
val recursiveTerminale3 :
  ('a -> 'a) ->
  ('a -> bool) -> ('a -> 'a) -> ('a -> 'b -> 'b) -> 'b -> 'a -> 'b
val terminale2 :
  'a ->
  ('b -> bool) ->
  ('b -> 'b) ->
  ('b -> 'b) -> ('a -> 'a -> 'a) -> ('a -> 'a) -> ('a -> 'a) -> 'b -> 'a
val iterative2While :
  'a ->
  ('b -> bool) ->
  ('b -> 'b) ->
  ('b -> 'b) -> ('a -> 'a -> 'a) -> ('a -> 'a) -> ('a -> 'a) -> 'b -> 'a
val iterative2For :
  'a ->
  ('b -> bool) ->
  ('b -> 'b) ->
  ('b -> 'b) -> ('a -> 'a -> 'a) -> ('a -> 'a) -> ('a -> 'a) -> 'b -> 'a
val terminaleB2 :
  ('a -> 'b) -> ('a -> bool) -> ('a -> 'a) -> ('a -> 'b -> 'b) -> 'a -> 'b
val iterativeB2While :
  ('a -> 'b) -> ('a -> bool) -> ('a -> 'a) -> ('a -> 'b -> 'b) -> 'a -> 'b
val iterativeB2For :
  ('a -> 'b) -> ('a -> bool) -> ('a -> 'a) -> ('a -> 'b -> 'b) -> 'a -> 'b
