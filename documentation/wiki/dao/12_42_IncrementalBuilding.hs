-- Incremental Rebuilds:

-- "Impedance mismatch" relates directly to Ten's challenge of efficiently determining what needs rebuilding:
diffGraph :: BuildGraph -> BuildGraph -> (a -> GraphF a) -> a -> Graph
