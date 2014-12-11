class OInt(x:Int) extends Ordered[OInt] {
	var value = x
	def compare(other: OInt): Int = {
		if(other.value < this.value) {
			1
		}
		else if(other.value > this.value) {
			-1
		}
		else 0
	}

	override def toString():String = "<"+this.value.toString()+">"
}

abstract class OTree[T <: Ordered[T]] extends Ordered[OTree[T]] {
	def compareLists(L1:List[OTree[T]],L2:List[OTree[T]]) : Int = {
		(L1,L2) match {
			case (Nil,Nil) => 0;
			case (x,Nil) => 1;
			case (Nil,y) => -1;
			case (x::xs,y::ys) => var z = x.compare(y);
								  if(z>0) 1
								  else if(z<0) -1
								  else compareLists(xs,ys) 
		}

	}
}

case class OLeaf[T <: Ordered[T]](t:T) extends OTree[T] {
	def compare(other: OTree[T]) : Int = {
		other match {
			case OLeaf(x) => t.compare(x)
			case ONode(y) => -1
		}
	}
}

case class ONode[T <: Ordered[T]](l:List[OTree[T]]) extends OTree[T] {
	def compare(other: OTree[T]) : Int = {
		other match {
			case ONode(x) => compareLists(l,x)
			case OLeaf(y) => 1
		}
	}
}

object Part2 {
	def compareTrees[T <: Ordered[T]](tree1:OTree[T],tree2:OTree[T]) = {
		var comparison = tree1.compare(tree2);
		if(comparison<0) {
			println("Less");
		}
		else if(comparison>0) {
			println("Greater");
		}
		else {
			println("Equal");
		}
	}

	def test() {

    val tree1 = ONode(List(OLeaf(new OInt(6))))

    val tree2 = ONode(List(OLeaf(new OInt(3)),
			   OLeaf(new OInt(4)), 
			   ONode(List(OLeaf(new OInt(5)))), 
			   ONode(List(OLeaf(new OInt(6)), 
				      OLeaf(new OInt(7))))));

    val treeTree1: OTree[OTree[OInt]] = 
      ONode(List(OLeaf(OLeaf(new OInt(1)))))

    val treeTree2: OTree[OTree[OInt]] = 
      ONode(List(OLeaf(OLeaf(new OInt(1))),
		 OLeaf(ONode(List(OLeaf(new OInt(2)), 
				  OLeaf(new OInt(2)))))))


    print("tree1: ")
    println(tree1)
    print("tree2: ")
    println(tree2)
    print("treeTree1: ")
    println(treeTree1)
    print("treeTree2: ")
    println(treeTree2)
    print("Comparing tree1 and tree2: ")
    compareTrees(tree1, tree2)
    print("Comparing tree2 and tree2: ")
    compareTrees(tree2, tree2)
    print("Comparing tree2 and tree1: ")
    compareTrees(tree2, tree1)
    print("Comparing treeTree1 and treeTree2: ")
    compareTrees(treeTree1, treeTree2)
    print("Comparing treeTree2 and treeTree2: ")
    compareTrees(treeTree2, treeTree2)
    print("Comparing treeTree2 and treeTree1: ")
    compareTrees(treeTree2, treeTree1)

  }
  	def main(args:Array[String]) = {

		test();
	}

}