package com.linovi.treeset

/** A simple implementation of an immutable binary tree */
case class ImmutableTreeSet(root: ImmutableNode, size: Int)
{
  // ===== ADD RELATED STUFF =====
  /** Returns a new tree with the given value added. If value already exists, same tree will be returned. */
  def add(v: Int): ImmutableTreeSet =
  {
    if(size == 0)
      ImmutableTreeSet(ImmutableNode(v), 1)
    else
    {
      findNodeToAdd(root, v) match
      {
        case Some(n) =>
        {
          val updateStack = findPathToUpdate(new MyStack(), root, v)
          updateStack.pop() match
          {
            case Some(nP) =>
            {
              if(v < nP.value)
              {
                val newRoot = updateNodesOnPath(updateStack, ImmutableNode(nP.value, ImmutableNode(v), nP.right))
                ImmutableTreeSet(newRoot, size + 1)
              }
              else
              {
                val newRoot = updateNodesOnPath(updateStack, ImmutableNode(nP.value, nP.left, ImmutableNode(v)))
                ImmutableTreeSet(newRoot, size + 1)
              }
            }
            
            // If somehow update fails, return this (unchanged) tree
            case None => this
          }
        }
        
        // If somehow a place cannot be found, return this (unchanged) tree
        case None => this
      }
    }
  }
  
  /** See -> add(Int): ImmutableTreeSet */
  def +(v: Int): ImmutableTreeSet = add(v)
  
  /** Recursively finds the correct place to add the given value */
  private def findNodeToAdd(n: ImmutableNode, v: Int): Option[ImmutableNode] =
  {
    if(n == null)
      None
    else if(v == n.value)
      None
    else if(v < n.value)
    {
      if(n.left == null)
        Option(n)
      else
        findNodeToAdd(n.left, v)
    }
    else
    {
      if(n.right == null)
        Option(n)
      else
        findNodeToAdd(n.right, v)
    }
  }
  
  // ===== REMOVE RELATED STUFF =====
  /** Returns a new tree with given value deleted. If value doesn't exist, same tree will be returned. */
  def remove(v: Int): ImmutableTreeSet =
  {
    if(size == 0)
      this
    else
      removeValue(root, root, v)
  }
  
  /** See -> remove(Int): ImmutableTreeSet */
  def -(v: Int): ImmutableTreeSet = remove(v)
  
  /** Traverses the tree starting from a node with it's parent and deletes the
   *  given value if it exists in the tree */
  private def removeValue(n: ImmutableNode, p: ImmutableNode, v: Int): ImmutableTreeSet =
  {
    if(n == null)
      this
    else if(v < n.value)
      removeValue(n.left, n, v)
    else if(v > n.value)
      removeValue(n.right, n, v)
    else
    {
      val left = n.left
      val right = n.right
      
      if(left == null && right == null)
      {
        if(v == root.value)
          ImmutableTreeSet() // This was root node and it was the only node, return a new empty tree
        else
        {
          // This was a leaf node, just update nodes starting from it's parent to root
          
          if(v < p.value)
          {
            // Deleting from left
            val newParent = ImmutableNode(p.value, null, p.right)
            ImmutableTreeSet(updateNodesOnPath(findPathToUpdate(new MyStack(), root, newParent.value), newParent), size - 1)
          }
          else
          {
            // Deleting from right
            val newParent = ImmutableNode(p.value, p.left, null)
            ImmutableTreeSet(updateNodesOnPath(findPathToUpdate(new MyStack(), root, newParent.value), newParent), size - 1)
          }
        }
      }
      else if(left != null && right == null)
      {
        if(v == root.value)
          ImmutableTreeSet(n.left, size - 1) // This was root node and it only had a left subtree, return a new tree whose root is the left subtree
        else
        {
          // This was a node only having a left subtree, put the left subtree in deleted nodes place and update nodes starting from the parent to root
          
          if(v < p.value)
          {
            // Deleting from left
            val newParent = ImmutableNode(p.value, left, p.right)
            ImmutableTreeSet(updateNodesOnPath(findPathToUpdate(new MyStack(), root, newParent.value), newParent), size - 1)
          }
          else
          {
            // Deleting from right
            val newParent = ImmutableNode(p.value, p.left, left)
            ImmutableTreeSet(updateNodesOnPath(findPathToUpdate(new MyStack(), root, newParent.value), newParent), size - 1)
          }
        }
      }
      else if(left == null && right != null)
      {
        if(v == root.value)
          ImmutableTreeSet(n.right, size - 1) // This was root node and it only had a right subtree, return a new tree whose root is the right subtree
        else
        {
          // This was a node only having a left subtree, put the left subtree in deleted nodes place and update nodes starting from the parent to root
          
          if(v < p.value)
          {
            // Deleting from left
            val newParent = ImmutableNode(p.value, right, p.right)
            ImmutableTreeSet(updateNodesOnPath(findPathToUpdate(new MyStack(), root, newParent.value), newParent), size - 1)
          }
          else
          {
            // Deleting from right
            val newParent = ImmutableNode(p.value, p.left, right)
            ImmutableTreeSet(updateNodesOnPath(findPathToUpdate(new MyStack(), root, newParent.value), newParent), size - 1)
          }
        }
      }
      else
      {
        findNextNodeToReplace(n.right) match
        {
          case Some(nTR) =>
          {
            val nodeToReplace = ImmutableNode(nTR.value, left, nTR.right)
            
            if(v == root.value)
            {
              // This was root node and it had both a left and a right subtree
              // Return a new tree whose root is the minimum node on the right subtree
              
              ImmutableTreeSet(nodeToReplace, size - 1)
            }
            else
            {
              // This was a node having both a left subtree and a right subtree
              // Return a new tree with the minimum node on the right subtree is placed in place of deleted node
              
              if(v < p.value)
              {
                // Deleting from left
                val newParent = ImmutableNode(p.value, nodeToReplace, p.right)
                ImmutableTreeSet(updateNodesOnPath(findPathToUpdate(new MyStack(), root, newParent.value), newParent), size - 1)
              }
              else
              {
                // Deleting from right
                val newParent = ImmutableNode(p.value, p.left, nodeToReplace)
                ImmutableTreeSet(updateNodesOnPath(findPathToUpdate(new MyStack(), root, newParent.value), newParent), size - 1)
              }
            }
          }
          
          case None => this
        }
      }        
    }
  }
  
  /** Finds the next node to be placed in the place of node being deleted */
  private def findNextNodeToReplace(n: ImmutableNode): Option[ImmutableNode] =
  {
    if(n == null)
      None
    else if(n.left == null)
      Option(n)
    else
      findNextNodeToReplace(n.left)
  }
  
  // ===== CONTAINS RELATED STUFF =====
  /** Returns true if given value exists in the tree */
  def contains(v: Int): Boolean = contains(root, v)
  
  /** See -> contains(Int): Boolean */
  def apply(v: Int): Boolean = contains(v)
  
  /** Traverses the tree starting from a node and checks if the given value
   *  exists in the tree */
  private def contains(n: ImmutableNode, v: Int): Boolean =
  {
    if(n == null)
      false
    else if(v == n.value)
      true
    else if(v < n.value)
      contains(n.left, v)
    else
      contains(n.right, v)
  }
  
  // ===== FILTER RELATED STUFF =====
  /** Returns a new tree with elements that satisfy the given condition */
  def filter(f: (Int) => Boolean): ImmutableTreeSet = filter(this, root, f)
  
  /** Traverses the tree starting from a node and deletes nodes that doesn't satisfy the given condition */
  private def filter(t: ImmutableTreeSet, n: ImmutableNode, f: (Int) => Boolean): ImmutableTreeSet =
  {
    if(n == null)
      t
    else
    {
      if(!f(n.value))
        filter(filter(t - n.value, n.left, f), n.right, f)
      else
        filter(filter(t, n.left, f), n.right, f)
    }
  }
  
  // ===== MAP RELATED STUFF =====
  /** Returns a new tree with each element having applied to the given function */
  def map(f: (ImmutableNode) => ImmutableNode): ImmutableTreeSet = map(ImmutableTreeSet(), root, f)
  
  /** Traverses the tree starting from a node and applies given function to all nodes during the traversal */
  private def map(t: ImmutableTreeSet, n: ImmutableNode, f: (ImmutableNode) => ImmutableNode): ImmutableTreeSet =
  {
    if(n == null)
      t
    else
      map(map(t + f(n).value, n.left, f), n.right, f)
  }
  
  // ===== OUTPUT RELATED STUFF =====
  /** Traverses the tree starting from a node and prints all it's contents recursively */
  private def print(n: ImmutableNode): String =
  {
    if(n == null)
      "_"
    else
      "{" + n + ", " + print(n.left) + ", " + print(n.right) + "}"
  }
  
  /** Gives a String representation of this tree */
  override def toString = "Tree (" + (hashCode() % 1000) + ")\n" + print(root)
  
  // ===== OTHER HELPER STUFF =====
  /** Recursively finds the path to the given value's node and returns a stack of the nodes on the way */
  private def findPathToUpdate(stack: MyStack, node: ImmutableNode, v: Int): MyStack =
  {
    if(node == null || v == node.value)
      stack
    else
    {
      if(v < node.value)
        findPathToUpdate(stack.push(node), node.left, v)
      else
        findPathToUpdate(stack.push(node), node.right, v)
    }
  }
  
  /** Walks through the path and creates new nodes with updated references on the way, returns the last updated node which will be new root */
  private def updateNodesOnPath(stack: MyStack, node: ImmutableNode): ImmutableNode =
  {
    if(stack.size == 0)
      node
    else
    {
      stack.pop() match
      {
        case Some(newParent) =>
        {
          if(node.value < newParent.value)
            updateNodesOnPath(stack, ImmutableNode(newParent.value, node, newParent.right))
          else
            updateNodesOnPath(stack, ImmutableNode(newParent.value, newParent.left, node))
        }
        
        case None => node
      }
    }
  }
}

object ImmutableTreeSet
{
  def apply() = new ImmutableTreeSet(null, 0)
}