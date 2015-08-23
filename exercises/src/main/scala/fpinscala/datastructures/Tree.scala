package fpinscala.datastructures

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]


object Tree {

	def size[A](t:Tree[A]):Int = t match {
		case Leaf(_) => 1
		case Branch(l:Tree[A], r:Tree[A]) => 1 + size(l) + size(r)
	}

	def maximum(t:Tree[Int]):Int = t match {
		case Leaf(a:Int) => a
		case Branch(l:Tree[Int], r:Tree[Int]) => maximum(l) max maximum(r)

	}

	def depth[A](t:Tree[A]):Int = t match {
		case Leaf(_) => 1
		case Branch(l:Tree[A], r:Tree[A]) => (1+ depth(l)) max (1 + depth(r))

	}

	def map[A,B](t:Tree[A],f:A=>B):Tree[B] = t match {
		case Leaf(a:A) => Leaf(f(a))
		case Branch(l:Tree[A], r:Tree[A]) => Branch(map(l,f), map(r,f))
	}
	def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = t match {
		case Leaf(a: A) => l(a)
		case Branch(lf:Tree[A], rt:Tree[A]) => b(fold(lf)(l)(b), fold(rt)(l)(b))
	}

	def sizeFold[A](t:Tree[A]): Int = fold(t)(_ => 1)((i: Int, j: Int) => 1 + i + j)

	def maximumFold(t:Tree[Int]): Int = fold(t)((i:Int) => i)((i: Int, j: Int) => i max j)

	def depthFold[A](t:Tree[A]): Int = fold(t)(_ => 1)((i: Int, j: Int) => (1 + i) max (1 + j))

	def mapFold[A,B](t:Tree[A], f: A=>B): Tree[B] = fold(t)((l:A) => Leaf(f(l)):Tree[B])((i: Tree[B], j: Tree[B]) => Branch(i,j))

}