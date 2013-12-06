package com.linovi.treeset

/** Represents a node in the ImmutableTreeSet */
case class ImmutableNode(value: Int, left: ImmutableNode, right: ImmutableNode)
{
  override def toString = value.toString
}

object ImmutableNode
{
  def apply(v: Int) = new ImmutableNode(v, null, null)
}

/** A simple implementation of an immutable binary tree */
case class ImmutableTreeSet(root: ImmutableNode, count: Int)
{
  def +(v: Int): ImmutableTreeSet = add(v)
  def -(v: Int): ImmutableTreeSet = remove(v)
  def apply(v: Int): Boolean = contains(v)
  def size = count
  
  /** Adds the given value as a new node to the tree */
  def add(v: Int): ImmutableTreeSet =
  {
    if(count == 0)
    {
      ImmutableTreeSet(ImmutableNode(v), 1)
    }
    else
    {
      findNodeToAdd(root, v) match
      {
        case Some(n) =>
        {
          getUpdatedRootAfterAdd(v) match
          {
            case Some(newRoot) => ImmutableTreeSet(newRoot, count + 1)
            
            // If somehow update fails, return this (unchanged) tree
            case None => this
          }
        }
        
        // If somehow a place cannot be found, return this (unchanged) tree
        case None => this
      }
    }
  }
  
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
  
  /** Gets a new root by creating new nodes where necessary after an addition */
  private def getUpdatedRootAfterAdd(v: Int): Option[ImmutableNode] =
  {
    var stack = findPathToUpdate(new MyStack(), root, v)
    
    stack.pop() match
    {
      case Some(nP) =>
      {
        var newParent = nP
        if(v < newParent.value)
          newParent = ImmutableNode(newParent.value, ImmutableNode(v), newParent.right)
        else
          newParent = ImmutableNode(newParent.value, newParent.left, ImmutableNode(v))
        
        Option(updateNodesOnPath(stack, newParent))
      }
      
      case None => None
    }
  }
  
  /** Returns a new tree with given value deleted if it exists */
  def remove(v: Int): ImmutableTreeSet =
  {
    if(count == 0)
      this
    else
      removeValue(root, root, v)
  }
  
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
        {
          // This was root node and it was the only node, return a new empty tree
          
          ImmutableTreeSet()
        }
        else
        {
          // This was a leaf node, just update nodes starting from it's parent to root
          
          var newParent: ImmutableNode = null
          if(v < p.value)
          {
            // Deleting from left
            newParent = ImmutableNode(p.value, null, p.right)
          }
          else
          {
            // Deleting from right
            newParent = ImmutableNode(p.value, p.left, null)
          }
          
          val stack = findPathToUpdate(new MyStack(), root, newParent.value)
          val newRoot = updateNodesOnPath(stack, newParent)
          
          ImmutableTreeSet(newRoot, count - 1)
        }
      }
      else if(left != null && right == null)
      {
        if(v == root.value)
        {
          // This was root node and it only had a left subtree, return a new tree whose root is the left subtree
          
          ImmutableTreeSet(n.left, count - 1)
        }
        else
        {
          // This was a node only having a left subtree, put the left subtree in deleted nodes place and update nodes starting from the parent to root
          
          var newParent: ImmutableNode = null
          if(v < p.value)
          {
            // Deleting from left
            newParent = ImmutableNode(p.value, left, p.right)
          }
          else
          {
            // Deleting from right
            newParent = ImmutableNode(p.value, p.left, left)
          }
          
          val stack = findPathToUpdate(new MyStack(), root, newParent.value)
          val newRoot = updateNodesOnPath(stack, newParent)
          
          ImmutableTreeSet(newRoot, count - 1)
        }
      }
      else if(left == null && right != null)
      {
        if(v == root.value)
        {
          // This was root node and it only had a right subtree, return a new tree whose root is the right subtree
          
          ImmutableTreeSet(n.right, count - 1)
        }
        else
        {
          // This was a node only having a left subtree, put the left subtree in deleted nodes place and update nodes starting from the parent to root
          
          var newParent: ImmutableNode = null
          if(v < p.value)
          {
            // Deleting from left
            newParent = ImmutableNode(p.value, right, p.right)
          }
          else
          {
            // Deleting from right
            newParent = ImmutableNode(p.value, p.left, right)
          }
          
          val stack = findPathToUpdate(new MyStack(), root, newParent.value)
          val newRoot = updateNodesOnPath(stack, newParent)
          
          ImmutableTreeSet(newRoot, count - 1)
        }
      }
      else
      {
        if(v == root.value)
        {
          // This was root node and it had both a left and a right subtree
          // Return a new tree whose root is the left subtree and root of the right subtree added to the resulting tree
          
          val newTree = ImmutableTreeSet(left, count - 1)
          val newRoot = newTree.addNode(right)
          
          ImmutableTreeSet(newRoot, newTree.size)
        }
        else
        {
          // This was a node having both a left subtree and a right subtree
          // Put the left subtree in deleted nodes place, add the right subtree to the resulting tree and update nodes to the root
          
          val newTree = ImmutableTreeSet(left, count - 1)
          val newSubRoot = newTree.addNode(right)
          
          var newParent: ImmutableNode = null
          if(v < p.value)
          {
            // Deleting from left
            newParent = ImmutableNode(p.value, newSubRoot, p.right)
          }
          else
          {
            // Deleting from right
            newParent = ImmutableNode(p.value, p.left, newSubRoot)
          }
          
          val stack = findPathToUpdate(new MyStack(), root, newParent.value)
          val newRoot = updateNodesOnPath(stack, newParent)
          
          ImmutableTreeSet(newRoot, newTree.size)
        }
      }        
    }
  }
  
  /** Adds an existing node to the tree and returns the new root */
  private def addNode(n: ImmutableNode): ImmutableNode =
  {
    // Find place to add the right subtree
    findNodeToAdd(root, n.value) match
    {
      case Some(nP) =>
      {
        var newParent: ImmutableNode = null
        if(n.value < nP.value)
        {
          // Adding to left
          newParent = ImmutableNode(nP.value, n, nP.right)
        }
        else
        {
          // Adding to right
          newParent = ImmutableNode(nP.value, nP.left, n)
        }
        
        val stack = findPathToUpdate(new MyStack(), root, newParent.value)
        
        updateNodesOnPath(stack, newParent)
      }
      
      case None => root
    }
  }
  
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
  
  /** Returns true if given value exists in the tree */
  def contains(v: Int): Boolean = contains(root, v)
  
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
}

object ImmutableTreeSet
{
  def apply() = new ImmutableTreeSet(null, 0)
}