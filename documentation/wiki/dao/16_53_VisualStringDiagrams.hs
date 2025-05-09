-- String Diagrams

{-
Current Implementation: Ten doesn't explicitly use string diagrams, but its architecture implicitly follows categorical patterns that could be visualized with string diagrams.

Potential Enhancement: Ten could implement visualization tools for build graphs using string diagram notation, making the dependencies and build flow more intuitive.
This would be particularly valuable for debugging complex build dependencies.
-}

-- Example enhancement for visualization
visualizeBuildGraph :: BuildGraph -> StringDiagram
visualizeBuildGraph graph = createDiagram $
  foldGraph addNodeToStringDiagram emptyDiagram graph
