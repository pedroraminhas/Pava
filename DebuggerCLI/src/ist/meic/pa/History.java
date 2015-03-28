package ist.meic.pa;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Stack;

class ObjectFieldValue{
	Object object;
	String fields;
	Object[] args;
	String calledMethod;
	String calledClass;
	String calledObject;
	
	public ObjectFieldValue(Object object, String fields,Object[] args, String calledClass,String calledMethod, String calledObject) {
		this.object= object;
		this.fields = fields;
		this.args= args;
		this.calledClass = calledClass;
		this.calledMethod = calledMethod;
		this.calledObject=calledObject;
	}
}


public class History {
	static Stack<ObjectFieldValue> stack = new Stack<ObjectFieldValue>();
	
	public History() {
		// TODO Auto-generated constructor stub
	}
	
	public static void setStackValue(Object clazz,String fields,Object[] args, String calledClass,String calledMethod, String calledObject){
		//System.out.println(clazz);
		stack.push(new ObjectFieldValue(clazz,fields,args, calledClass,calledMethod,calledObject));
	}
	
	
	public static void printStack(){
		//System.out.println(stack.size());
        List<ObjectFieldValue> list = new ArrayList<ObjectFieldValue>(stack);
        ListIterator<ObjectFieldValue> it = list.listIterator(list.size());
        ObjectFieldValue currentCall = null;
        if(it.hasPrevious()){
        	currentCall = it.previous();
        	System.out.println("Called Object: "+ currentCall.calledObject);
        	System.out.println("       Fields: "+ currentCall.fields);
        	System.out.println("Call Stack: ");
        	System.out.println(currentCall.calledClass+"."+currentCall.calledMethod + "("  + parseArgs(currentCall.args)+")");
        }
        while(it.hasPrevious()) {
        	currentCall = it.previous();
        	System.out.println(currentCall.calledClass+"."+currentCall.calledMethod + "("  + parseArgs(currentCall.args)+")");
        	}
	}
	
	
	public static String parseArgs(Object[] args) {
		String result = "";
		
		if(args[0].getClass().getName().contains("String")){
			String[] castme = (String[]) args[0];
			for (String s : castme){
				if(result=="")
					result= s;
				else
					result += "," +s;
			}
		} else {	
			for(int i=0; i < args.length; i++ ) {
				if(result == "")
						result = args[i].toString();
				else
						result += ","+args[i].toString();
			}
		}
		return result;
	}
}
