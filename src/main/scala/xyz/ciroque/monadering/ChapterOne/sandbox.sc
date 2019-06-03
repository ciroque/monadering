import xyz.ciroque.monadering.ChapterOne.{Leaf, Node, Tree}

val tree = Node(Node(Node(Node(Leaf("A"), Leaf("Z")), Leaf("B")), Leaf("Y")), Leaf("C"))

val leafCount = Tree.leafCount(tree)

val wtf = Tree.identity(tree)

Tree.relabel(tree)(0)
