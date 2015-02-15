// Copyright (c) 2015 Masaru Nomura
// Released under the MIT license
// http://opensource.org/licenses/mit-license.php


import scala.annotation.tailrec

object AVLTree {

	def isEmpty(tree: Tree[_]): Boolean = tree eq null

	def count(tree: Tree[_]): Int = if (tree eq null) 0 else tree.count

	def height(tree: Tree[_]): Int = if (tree eq null) 0 else tree.height

	def contains[A: Ordering](tree: Tree[A], x: A): Boolean = lookup(tree, x) ne null

	def get[A: Ordering](tree: Tree[A], x: A): Option[A] =
	lookup(tree, x) match {
		case null => None
		case tree => Some(tree.value)
	}


	@tailrec
	def lookup[A](tree: Tree[A], x: A)(implicit ordering: Ordering[A]): Tree[A] =
	tree match {
		case null => null
		case tree =>
			val cmp = ordering.compare(x, tree.value)
			if (cmp < 0)
				lookup(tree.left, x)
			else if (cmp > 0)
				lookup(tree.right, x)
			else
				tree
	}


	def update[A](tree: Tree[A], x: A)(implicit ordering: Ordering[A]): Tree[A] =
	tree match {
		case null => Tree(x, null, null)
		case tree =>
			val cmp = ordering.compare(x, tree.value)
			val newRoot =
				if (cmp <= 0)
					Tree(tree.value, update(tree.left, x), tree.right)
				else
					Tree(tree.value, tree.left, update(tree.right, x))
			AVLTree.balance(newRoot)
	}


	def delete[A](tree: Tree[A], x: A)(implicit ordering: Ordering[A]): Tree[A] =
	tree match {
		case null => null
		case tree =>
			val cmp = ordering.compare(x, tree.value)
			val root =
				if (cmp < 0)
					Tree(tree.value, delete(tree.left, x), tree.right)
				else if (cmp > 0)
					Tree(tree.value, tree.left, delete(tree.right, x))
				else { // Found x to remove
					if (tree.left eq null)
						tree.right
					else if (tree.left.right eq null)
						AVLTree.setRightChild(tree.left, tree.right)
					else {
						val value  = AVLTree.getRightMostValue(tree.left)
						val newLch = AVLTree.delete(tree.left, value)
						Tree(value, newLch, tree.right)
					}
				}
			AVLTree.balance(root)
	}

	@tailrec
	private def getRightMostValue[A](tree: Tree[A]): A = tree match {
		case Tree(v, _, null) => v
		case Tree(_, _, rch)  => getRightMostValue(rch)
	}

	private def setRightChild[A](tree: Tree[A], rch: Tree[A]): Tree[A] = tree match {
		case null => rch
		case Tree(v, lch, _) => Tree(v, lch, rch)
	}


	private def setLeftChild[A](tree: Tree[A], lch: Tree[A]): Tree[A] = tree match {
		case null => lch
		case Tree(v, _, rch) => Tree(v, lch, rch)
	}


	private def balance[A](tree: Tree[A]): Tree[A] = tree match {
		case null => null
		case _	  =>
			val rchHeight = if (tree.right eq null) 0 else tree.right.height
			val lchHeight = if (tree.left  eq null) 0 else tree.left.height
			val balanceDegree = rchHeight - lchHeight

			balanceDegree match {
				case -1 | 0 | 1 => tree // No balancing necessary
				case  2 => AVLTree.balanceWithDegree2(tree)
				case -2 => AVLTree.balanceWithDegreeN2(tree)
			}
	}


	// Return balanced tree
	// assuming that balance degree of tree is 2 
	private def balanceWithDegree2[A](tree: Tree[A]): Tree[A] = {
		if ((tree.left        eq null) ||
			(tree.right.right eq null)) {
			AVLTree.rotateRightLeft(tree)
		}
		else if (tree.left.right eq null) {
			AVLTree.rotateLeft(tree)
		}
		else { // Check balance degree of grand children
			val rch = tree.right
			val grchHeight = if (rch.right eq null) 0 else rch.right.height
			val glchHeight = if (rch.left  eq null) 0 else rch.left.height
			val balanceDegreeGch = grchHeight - glchHeight
			
			balanceDegreeGch match {
				case  1 => AVLTree.rotateLeft(tree)
				case -1 => AVLTree.rotateRightLeft(tree)
			}
		}		
	}

	// Return balanced tree
	// assuming that balance degree of tree is -2 
	private def balanceWithDegreeN2[A](tree: Tree[A]): Tree[A] = {		
		if ((tree.right     eq null) ||
			(tree.left.left eq null)) {
			AVLTree.rotateLeftRight(tree)
		}
		else if (tree.left.right eq null) {
			AVLTree.rotateRight(tree)
		}
		else {
			val lch = tree.left
			val grchHeight = if (lch.right eq null) 0 else lch.right.height
			val glchHeight = if (lch.left  eq null) 0 else lch.left.height
			val balanceDegreeGch = grchHeight - glchHeight

			balanceDegreeGch match {
				case  1 => AVLTree.rotateLeftRight(tree)
				case -1 => AVLTree.rotateRight(tree)
			}
		}
	}


	// 	   root	       newRoot 
	// 	  /    \       /   \
	// newRoot  C =>  A   root
	//  / \                / \
	// A   B              B   C
	private def rotateRight[A](tree: Tree[A]): Tree[A] =
	tree match {
		case null => null
		case root =>
			val newRoot = root.left
			if (newRoot eq null)
				root
			else {
				val newRch = AVLTree.setLeftChild(root, newRoot.right)
				AVLTree.setRightChild(newRoot, newRch)
			}
	}


	//   root	        newRoot 
	//  /    \           /   \
	// A   newRoot =>  root   C
	//       / \       / \
	//      B   C     A   B
	private def rotateLeft[A](tree: Tree[A]): Tree[A] =
	tree match {
		case null => null
		case root =>
			val newRoot = root.right
			if (newRoot eq null)
				root
			else {
				val newLch = AVLTree.setRightChild(root, newRoot.left)
				AVLTree.setLeftChild(newRoot, newLch)
			}
	}


	// <First LEFT roration>
	//      root              root 
	//     /    \            /    \
	//    A      D  =>   newRoot   D
	//  /  \               / \
	// B  newRoot         A   C
	//       \           /
	//        C         B

	// <Second RIGHT rotation>
	//       root               newRoot
	//      /    \               /   \
	//   newRoot  D      =>     A    root
	//    / \                  /     / \
	//   A   C                B     C   D
	//  /
	// B
	private def rotateLeftRight[A](tree: Tree[A]): Tree[A] =
	tree match {
		case null => null
		case tree =>
			val newRoot = if (tree.left eq null) null else AVLTree.rotateLeft(tree.left)
			val root    = AVLTree.setLeftChild(tree, newRoot)
			AVLTree.rotateRight(root)
	}


	// <First RIGHT roration>
	//    root           root
	//   /   \           /  \
	//  D     A     =>  D  newRoot
	//      /   \             \
	//  newRoot  B             A
	//      \                 / \
	//       C               C   B

	// <Second LEFT rotation>
	//   root            newRoot
	//   /  \             /   \
	//  D  newRoot  =>  root   A
	//        \         /     / \
	//         A       D     C   B
	//        / \
	//       C   B
	private def rotateRightLeft[A](tree: Tree[A]): Tree[A] =
	tree match {
		case null => null
		case tree =>
			val newRoot = if (tree.right eq null) null else AVLTree.rotateRight(tree.right)
			val root    = AVLTree.setRightChild(tree, newRoot)
			AVLTree.rotateLeft(root)
	}


	final class Tree[A](
		final val value: A,
		final val left : Tree[A],
		final val right: Tree[A])
	extends Serializable {
		final val count: Int = 1 + AVLTree.count(left) + AVLTree.count(right)
		final val height: Int = 1 + math.max(AVLTree.height(left), AVLTree.height(right))
	}

	object Tree {
		def apply[A](value: A, left: Tree[A], right: Tree[A]) = new Tree(value, left, right)
		def unapply[A](t: Tree[A]) = Some((t.value, t.left, t.right))
	}
}