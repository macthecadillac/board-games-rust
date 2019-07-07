struct Node<T> {
    children: Vec<usize>,
    data: T
}

struct Tree<T> {
    arena: Node<T>
}
