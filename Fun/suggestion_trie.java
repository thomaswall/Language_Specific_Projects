import java.util.*;
import java.io.*;

public class suggestion_trie {

	public static ArrayList<Node> suggestions;

	static class Node implements Comparable<Node> {
		char[] letters;
		int count;
		StringBuilder whole_word = new StringBuilder(" ");
		boolean end;
		ArrayList<Node> children;
		public Node (char[] letters) {
			this.count = 0;
			this.end = false;
			this.children = new ArrayList<Node>();
			this.letters = new char[letters.length];
			for(int i=1;i<letters.length;i++) {
				this.letters[i] = letters[i];
				this.whole_word.append(letters[i]);
			}
		}

		@Override
		public int compareTo(Node other) {
			if(this.count>other.count) return -1;
			else if(this.count<other.count) return 1;
			else return 0;
		}
	}

	static void build_a_node(char[] current_letters, Node node) {
		if(node.letters.length<=current_letters.length) {
			char[] partial = new char[node.letters.length+1];
			for(int i=0;i<node.letters.length;i++) {
				partial[i] = node.letters[i];
			}
			partial[node.letters.length] = current_letters[node.letters.length];
			Node new_node = new Node(partial);
			node.children.add(new_node);
			if(new_node.letters.length<current_letters.length) {build_a_node(current_letters,new_node);}
			else if(new_node.letters.length==current_letters.length) {new_node.count++; new_node.end = true;}
		}

	}

	static void trie_creator(char[] current_letters, int position, Node node) {
		ArrayList<Node> list = node.children;
		char x = current_letters[position];
        for(int i=0;i<list.size();i++) {
        	Node temp = list.get(i);
        	if(temp.letters[temp.letters.length-1]==x) {
        		if(position<current_letters.length-1) { trie_creator(current_letters,position+1,temp); }
        		else if(position==current_letters.length-1) { temp.count++; temp.end = true; }
   				return;
        	}
        }
        build_a_node(current_letters,node);
      }

    static void found_it(Node current) {
    	if(current.end == true) { suggestions.add(current);}
    	if(current.children.size()>0) {
    		for(int i=0;i<current.children.size();i++) {
      			found_it(current.children.get(i));
      		}
    	}
    }

    static void tree_finder(Node current, Node query) {
    	if(current.whole_word.toString().equals(query.whole_word.toString())) {
    		found_it(current);
    		return;
    	}
    	if(current.children.size()>0) {
      		for(int i=0;i<current.children.size();i++) {
      			int temp = current.letters.length;
      			if(current.children.get(i).letters[temp]==query.letters[temp]) {tree_finder(current.children.get(i),query);}
      		}
      	}
      	else {
      		System.out.println("No results.");
      	}
    }

      public static void main(String[] args) throws IOException{

      	File file = new File(args[0]);
      	Scanner stdin = new Scanner(file);
      	StringBuilder sb = new StringBuilder();

      	while(stdin.hasNextLine()) {
      		sb.append(stdin.nextLine());
      	}
      	stdin.close();


      	String[] words = sb.toString().split(" ");
      	char[] first = new char[] {'.'};
      	Node root = new Node(first);
      	for(int i=0;i<words.length;i++) {
      		char[] temp = new char[words[i].length() + 1];
      		temp[0] = '.';
      		char[] temp_char_array = words[i].toCharArray();
      		for(int j=0;j<words[i].length();j++) {
      			temp[j+1] = temp_char_array[j];
      		}
      		trie_creator(temp,1,root);
      	}

      	String query;
      	stdin = new Scanner(System.in);
      	while(true) {
	      	System.out.print("Query: ");
		      	if(!(query = stdin.nextLine()).equals("exit")) {
			      	char[] char_query = new char[query.length()+1];
			      	char[] temp = query.toCharArray();
			      	char_query[0] = '.';
			      	for(int i=1;i<query.length()+1;i++) {
			      		char_query[i] = temp[i-1];
			      	}
			      	Node searching = new Node(char_query);

			      	suggestions = new ArrayList<Node>();

			      	tree_finder(root,searching);

			      	Collections.sort(suggestions);
			      	for(int i=0;i<suggestions.size();i++) {
			      		System.out.println(suggestions.get(i).whole_word + ": " + suggestions.get(i).count);
			      	}
			     }
			    else break;
	     }
      	

      }
}