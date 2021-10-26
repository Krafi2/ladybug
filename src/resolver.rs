use crate::topic::registry::TopicId;

pub trait Interface {
    type Open;
    type Closed;
    type OpenError;
    type CloseError;

    fn open(&mut self, id: TopicId) -> Result<Self::Open, Self::OpenError>;
    fn close(&mut self, open: Self::Open) -> Result<Self::Closed, Self::CloseError>;
    fn dependencies<'a>(&'a mut self, open: &'a Self::Open) -> &'a [TopicId];
    fn satisfy(&mut self, open: &mut Self::Open, dep: &mut Self::Closed);
}

pub enum NodeErr<O, C> {
    /// A dependency couldn't be satisfied
    Unsatisfied(NodeId),
    /// This node is a part of a cycle
    Cycle(NodeId),
    /// There was an error when opening this node
    OpenError(O),
    /// There was an error when closing this node
    CloseError(C),
}

pub enum Node<O, C, OE, CE> {
    Open(O),
    Closed(C),
    Err(NodeErr<OE, CE>),
}

pub use resolver::{NodeId, Resolver};
mod resolver {
    use std::ops::{Index, IndexMut};

    use super::{Interface, Node};
    use crate::topic::registry::{Handle, Storage, TopicId};

    // type Node<T: Interface> = super::Node<T::Open, T::Closed, T::OpenError, T::CloseError>;

    #[derive(Clone, Copy, Debug, Hash, PartialEq, Eq)]
    pub struct NodeId(TopicId);

    impl Into<TopicId> for NodeId {
        fn into(self) -> TopicId {
            self.0
        }
    }

    pub struct Resolver<O, C, OE, CE>(Storage<Node<O, C, OE, CE>>);

    impl<O, C, OE, CE> Resolver<O, C, OE, CE> {
        pub fn new() -> Self {
            Self(Storage::new())
        }

        fn convert_id(&self, id: NodeId) -> Handle<Node<O, C, OE, CE>> {
            self.0.try_get_handle(id.0).expect("Invalid handle")
        }

        pub fn get_handle<F>(&mut self, id: TopicId, func: F) -> NodeId
        where
            F: FnOnce() -> Node<O, C, OE, CE>,
        {
            self.0.get_handle(id, func);
            NodeId(id)
        }

        pub fn get_raw(&self, id: NodeId) -> &Option<Node<O, C, OE, CE>> {
            let handle = self.convert_id(id);
            self.0.get_raw(handle)
        }

        pub fn get_raw_mut(&mut self, id: NodeId) -> &mut Option<Node<O, C, OE, CE>> {
            let handle = self.convert_id(id);
            self.0.get_raw_mut(handle)
        }

        pub fn replace<F>(&mut self, id: NodeId, f: F)
        where
            F: FnOnce(Node<O, C, OE, CE>) -> Node<O, C, OE, CE>,
        {
            let handle = self.convert_id(id);
            self.0.replace(handle, f)
        }

        pub(super) fn open_node<
            T: Interface<Open = O, Closed = C, OpenError = OE, CloseError = CE>,
        >(
            &mut self,
            interface: &mut T,
            id: TopicId,
        ) -> NodeId {
            self.get_handle(id, || match interface.open(id) {
                Ok(open) => Node::Open(open),
                Err(err) => Node::Err(super::NodeErr::OpenError(err)),
            })
        }
    }

    impl<O, C, OE, CE> Index<NodeId> for Resolver<O, C, OE, CE> {
        type Output = Node<O, C, OE, CE>;

        fn index(&self, index: NodeId) -> &Self::Output {
            self.get_raw(index).as_ref().expect("Handle not available")
        }
    }

    impl<O, C, OE, CE> IndexMut<NodeId> for Resolver<O, C, OE, CE> {
        fn index_mut(&mut self, index: NodeId) -> &mut Self::Output {
            self.get_raw_mut(index)
                .as_mut()
                .expect("Handle not available")
        }
    }
}

#[derive(Clone)]
struct Frame {
    node: NodeId,
    child: usize,
}

impl Frame {
    fn new(node: NodeId) -> Self {
        Self { node, child: 0 }
    }
}

/// Try to deploy a single topic and return its final enviroment.
/// Results are cached by the resolver.
pub fn resolve<T: Interface>(
    resolver: &mut Resolver<T::Open, T::Closed, T::OpenError, T::CloseError>,
    interface: &mut T,
    root: TopicId,
) -> Result<NodeId, NodeId> {
    // The root node we want to deploy
    let root = resolver.open_node(interface, root);
    // All the nodes in the stack should be open with the exception of the head node
    let mut stack = Vec::from([Frame::new(root)]);

    // We use a depth first search algorithm to make sure all dependencies are satisfied before
    // deploying a node. Any errors are propagated up towards the root node.
    loop {
        let head = stack.last().unwrap().clone();
        match &resolver[head.node] {
            // If the node is open, try to go deeper or deploy it
            Node::Open(open) => {
                let child = head.child;
                stack.last_mut().unwrap().child += 1;
                match interface.dependencies(open).get(child) {
                    // A child is available so we will try to push it onto the stack
                    Some(&child) => {
                        let child = resolver.open_node(interface, child);

                        // Check for cycles
                        // NOTE: this has quadratic complexity so we may want to replace this
                        // with a more efficient implementation in the future. What comes to
                        // mind is either a hashset, or a `Storage` keeping track of active
                        // nodes
                        let mut cycle = None;
                        for (i, Frame { node, .. }) in stack.iter().enumerate() {
                            if node == &child {
                                cycle = Some(i);
                                break;
                            }
                        }

                        // There is a cycle so we will mark the appropriate nodes and remove
                        // them from the stack
                        if let Some(pos) = cycle {
                            let first = stack[pos].clone();
                            let mut iter = stack.drain(pos..).peekable();
                            loop {
                                match iter.next() {
                                    Some(Frame { node, .. }) => {
                                        let next = iter.peek().unwrap_or(&first).node;
                                        resolver[node] = Node::Err(NodeErr::Cycle(next))
                                    }
                                    None => break,
                                }
                            }
                        } else {
                            // Everything is ok, so we push the new node
                            stack.push(Frame::new(child));
                        }
                    }
                    // All children have been deployed so we can do the same with the node
                    None => {
                        resolver.replace(head.node, |node| match node {
                            Node::Open(open) => match interface.close(open) {
                                // Close the node to be popped on the next iteration
                                Ok(closed) => Node::Closed(closed),
                                // Mark the node as an error. It will be propagated on the next
                                // iteration
                                Err(err) => Node::Err(NodeErr::CloseError(err)),
                            },
                            _ => unreachable!("Node should be open"),
                        })
                    }
                };
            }
            // The node is closed so we pop it and update its parent
            Node::Closed(_) => {
                let head = stack.pop().unwrap().node;

                // To satisfy the borrow checker, we have to take the data  out of the storage,
                // making the handle temporarily invalid
                let raw = resolver.get_raw_mut(head);
                let mut dep = std::mem::replace(raw, None).expect("Invalid handle");

                let finish = match stack.last() {
                    // Notify the interface that the node's dependency is satisfied                    Some(Frame { node, .. }) => match &mut storage[node] {
                    Some(Frame { node, .. }) => match &mut resolver[*node] {
                        Node::Open(open) => {
                            let dep = match &mut dep {
                                Node::Closed(closed) => closed,
                                _ => unreachable!("Node should be closed"),
                            };
                            interface.satisfy(open, dep);
                            false
                        }
                        _ => unreachable!("Node should be open"),
                    },
                    // We are finished
                    None => true,
                };

                // We have to put everything back together
                resolver.get_handle(head.into(), || dep);

                if finish {
                    break Ok(head);
                }
            }
            // There is an error so we need to mark the parent as having an unsatisfied
            // dependency
            Node::Err(_) => {
                // Last one already is an error so we remove it
                let head = stack.pop().unwrap().node;
                match stack.last() {
                    Some(Frame { node, .. }) => {
                        resolver[*node] = Node::Err(NodeErr::Unsatisfied(head))
                    }
                    // The error is on the root, nothing to do
                    None => break Err(head),
                };
            }
        }
    }
}
