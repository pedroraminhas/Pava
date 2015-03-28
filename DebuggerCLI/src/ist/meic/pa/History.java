package ist.meic.pa;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Stack;

class ObjectFieldValue{
	Object object;
	Object[] args;
	String calledMethod;
	String calledClass;
	
	public ObjectFieldValue(Object object, Object[] args, String calledClass,String calledMethod) {
		this.object= object;
		this.args= args;
		this.calledClass = calledClass;
		this.calledMethod = calledMethod;
	}
}


public class History {
	static Stack<ObjectFieldValue> stack = new Stack<ObjectFieldValue>();
	
	public History() {
		// TODO Auto-generated constructor stub
	}
	
	public static void setStackValue(Object clazz ,Object[] args, String calledClass,String calledMethod){
		//System.out.println(clazz);

			stack.push(new ObjectFieldValue(clazz,args, calledClass,calledMethod));

	}
	
	
	public static void printStack(){
		//System.out.println(stack.size());
		
        List<ObjectFieldValue> list = new ArrayList<ObjectFieldValue>(stack);
        ListIterator<ObjectFieldValue> it = list.listIterator(list.size());
        ObjectFieldValue currentCall = null;
        if(it.hasPrevious()){
        	currentCall = it.previous();
        	System.out.println("Called Object: ");
        	System.out.println("       Fields: ");
        	System.out.println("Call Stack: ");
        	System.out.println(currentCall.calledClass+"."+currentCall.calledMethod + "("  + parseArgs(currentCall.args)+")");
        }
        while(it.hasPrevious()) {
        	currentCall = it.previous();
        	System.out.println(currentCall.calledClass+"."+currentCall.calledMethod + "("  + parseArgs(currentCall.args)+")");
        	}
	}
	
	public static String parseArgs(Object[] args) {
		String result = null;
		
		if(args[0].getClass().getName().contains("String")){
			String[] castme = (String[]) args[0];
			for (String s : castme){
				if(result==null)
					result= s;
				else
					result += "," +s;
			}
		} else {	
			for(int i=0; i < args.length; i++ ) {
				if(result == null)
						result = args[i].toString();
				else
						result += ","+args[i].toString();
			}
		}
		return result;
	}
}
