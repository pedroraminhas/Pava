package ist.meic.pa;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/*
 * Class MethodTranslator catches the exception thrown by the invocation of the method and calls 
 * the DebuggerShell
 */
public class MethodTranslator {
	public static Object c = null;
	public static Object genericMethod(Object obj , Class<?> objClass, String methodName,Object[] args, Class<?>[] paramTypes) throws Throwable {
		History.calledClasses.push(objClass);
		History.calledObjects.push(new ObjectFieldValue(obj,objClass.getName(),args,methodName));
		History.calledArgs.push(args);
		try{
			Method method = objClass.getMethod(methodName,paramTypes);
			method.setAccessible(true);
			c = method.invoke(obj, args);
		}catch(InvocationTargetException e){
			System.out.println(e.getCause());
			DebuggerShell.main(e);
		}
		History.calledObjects.pop();
		History.calledArgs.pop();
		History.calledClasses.pop();
		return c;
	}
	
} 
