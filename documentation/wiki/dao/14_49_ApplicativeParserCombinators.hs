-- Parsers

-- Ten would benefit from a more robust parser implementation for .ten files, which could leverage applicative parser combinators.

--The current implementation has placeholder deserializers like:
deserializeDerivation :: BS.ByteString -> Either Text Derivation
deserializeDerivation _ = Left "Deserialization not implemented yet"
