package ist.meic.pa;

import java.util.Iterator;
import java.util.Stack;
/*
 * Class History holds the stack with information about Objects, methods called, arguments called 
 * and classes called
 */
public class History {
	static Stack<ObjectFieldValue> calledObjects = new Stack<ObjectFieldValue>();
	static Stack<String> calledMethods =new Stack<String>();
	static Stack<Object[]> calledArgs = new Stack<Object[]>();
	static Stack<Class<?>> calledClasses = new Stack<Class<?>>();
	
	public static void pushCalledObjectsStack (ObjectFieldValue object){
		calledObjects.push(object);
	}
	public static void pushStack(Object object2, String className2, Object[] args2, String method2){
		ObjectFieldValue obj = new ObjectFieldValue (object2,className2,args2,method2);
		pushCalledObjectsStack(obj);
		calledArgs.push(args2);
	}
	
	public static Stack<ObjectFieldValue> returnPreviousCalledObject(Stack<ObjectFieldValue> stack){
		
		Stack<ObjectFieldValue> stackWithoutLastElement = (Stack<ObjectFieldValue>) calledObjects.clone();
		stackWithoutLastElement.pop();
		return stackWithoutLastElement;
	}
}
