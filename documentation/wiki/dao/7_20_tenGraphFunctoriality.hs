-- The implementation of foldGraph in Ten.Graph showcases functoriality by applying a function recursively over the entire dependency structure:
foldGraph :: (a -> BuildNode -> a) -> a -> BuildGraph -> a
