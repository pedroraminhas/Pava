package ist.meic.pa;

import java.util.TreeMap;
/*
 * Class TypeMap has a treeMap that with a given key (a type) 
 * returns the string corresponding the class name
 */
public class TypeMap {
	 static TreeMap typeMap = new TreeMap();
	 /*
	 public static void initializeMap(){
		 typeMap.put("String", "java.lang.String");
		 typeMap.put("double","java.lang.Double");
		 typeMap.put("int","java.lang.Integer");
		 typeMap.put("byte", "java.lang.Byte");
		 typeMap.put("short", "java.lang.Short");
		 typeMap.put("long", "java.lang.Long");
		 typeMap.put("float", "java.lang.Float");
		 typeMap.put("char", "java.lang.Character");
		 typeMap.put("boolean", "java.lang.Boolean");
	 }*/
	 
	 public static void initializeMap(){
		 typeMap.put("String", new String());
		 typeMap.put("double",new Double(3));
		 typeMap.put("int",new Integer(5));
		 typeMap.put("byte", new Byte((byte)0xe0));
		 typeMap.put("short", new Short((short) 6));
		 typeMap.put("long", new Long(6));
		 typeMap.put("float", new Float(5.8));
		 typeMap.put("char", new Character('d'));
		 typeMap.put("boolean", new Boolean(true));
	 }
}
