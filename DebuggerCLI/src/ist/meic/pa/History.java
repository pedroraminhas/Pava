package ist.meic.pa;

import java.util.ArrayList;
import java.util.List;
import java.util.ListIterator;
import java.util.Stack;

class ObjectFieldValue{
	Object object;
	Object[] fields;
	String methodCall;
	
	public ObjectFieldValue(Object object2, Object[] fields2, String methodName) {
		this.object= object2;
		this.fields = fields2;
		this.methodCall = methodName;
	}
}


public class History {
	static Stack<ObjectFieldValue> stack = new Stack<ObjectFieldValue>();
	
	public History() {
		// TODO Auto-generated constructor stub
	}
	
	public static void setStackValue(Object[] fields2, String methodName){
		stack.push(new ObjectFieldValue(null,fields2, methodName));
	}
	
	
	public static void printStack(){
		System.out.println("Call stack:");
		System.out.println(stack.size());
		
        List<ObjectFieldValue> list = new ArrayList<ObjectFieldValue>(stack);
        ListIterator<ObjectFieldValue> it = list.listIterator(list.size());
        
        while(it.hasPrevious()) {
        	ObjectFieldValue currentCall = it.previous();
        		System.out.println(currentCall.methodCall + "::"  + currentCall.fields.toString());
        	}
	}
	
}
