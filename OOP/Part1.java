import java.util.*;

class ComparableList<T extends Comparable<T>> extends ArrayList<T> implements Comparable<ComparableList<T>> {
		@Override
		public int compareTo(ComparableList<T> L2)
		{
			int k=0;
			while(k<this.size() && k<L2.size())
			{
				if(this.get(k).compareTo(L2.get(k))>0) {
					return 1;
				}
				else if(this.get(k).compareTo(L2.get(k))<0) {
					return -1;
				}
				k++;
			}
			if(this.size()<L2.size()) {
				return -1;
			}
			else if(this.size()>L2.size()){
				return 1;
			}

			return 0;
		}
	}

class A implements Comparable<A> {
	protected Integer x;

	public A(Integer x) {
		this.x = x;
	}

	@Override
	public int compareTo(A other) {
		if(other instanceof B) {
			B casted_other = (B) other;
			Integer sum = casted_other.x + casted_other.y;

			return this.x.compareTo(sum);
		}

		return this.x.compareTo(other.x);
	}

	@Override
	public String toString()
	{
		return "A<"+this.x+">";
	}
}

class B extends A {
	protected Integer y;

	public B(Integer x,Integer y) {
		super(x);
		this.y = y;
	}

	@Override
	public int compareTo(A other) {
		Integer this_sum = this.x+this.y;

		if(other instanceof B) {
			B casted_other = (B) other;
			Integer other_sum = casted_other.x + casted_other.y;

			return this_sum.compareTo(other_sum);
		}

		return this_sum.compareTo(other.x);

	}

	@Override
	public String toString()
	{
		return "B<"+this.x+","+this.y+">";
	}
}

class Part1 {

	public static <T extends Comparable<T>> void addToCList(T z,ComparableList<T> L) {
		L.add(z);
	}

	static void test() {
	ComparableList<A> c1 = new ComparableList<A>();
	ComparableList<A> c2 = new ComparableList<A>();
	for(int i = 0; i < 10; i++) {
	    addToCList(new A(i), c1);
	    addToCList(new A(i), c2);
	}
	
	addToCList(new A(12), c1);
	addToCList(new B(6,6), c2);
	
	addToCList(new B(7,11), c1);
	addToCList(new A(13), c2);

	System.out.print("c1: ");
	System.out.println(c1);
	
	System.out.print("c2: ");
	System.out.println(c2);

	switch (c1.compareTo(c2)) {
	case -1: 
	    System.out.println("c1 < c2");
	    break;
	case 0:
	    System.out.println("c1 = c2");
	    break;
	case 1:
	    System.out.println("c1 > c2");
	    break;
	default:
	    System.out.println("Uh Oh");
	    break;
	}

    }
	

	public static void main(String[] args){
		ComparableList<A> thisone = new ComparableList<A>();
		ComparableList<A> thatone = new ComparableList<A>();
		test();

	}

}