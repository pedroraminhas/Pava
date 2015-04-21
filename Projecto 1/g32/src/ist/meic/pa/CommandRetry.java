package ist.meic.pa;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Stack;

/*
 * Command Retry repeats the methodCall that was interrupted
 */
public class CommandRetry {
	public static void main() throws Throwable{
		
		// Clone the stacks in order to have different stacks
		Stack<ObjectFieldValue> stack = (Stack<ObjectFieldValue>) History.calledObjects.clone();
		Stack<Object[]> stackArgs = (Stack<Object[]>) History.calledArgs.clone();

		//Get the last Object called and its info
		Object lastObject = stack.peek().getObject();
		Class lastObjectClass = stack.peek().getObject().getClass();
		String lastMethodName = stack.peek().getMethod();
		Class<?>[] lastParamTypes=(Class<?>[]) stack.peek().getArgs();
		Object[] lastArgs = stackArgs.peek();

		/*
		 * Searches for the method that was interrupted and invoke it in the current object
		 */
		for(Method m : lastObjectClass.getDeclaredMethods()){
			if (m.getName().equals(lastMethodName)){
				m.setAccessible(true);
				try {
					m.invoke(lastObject, lastArgs);
				} catch (IllegalAccessException e) {
					System.out.println(e.getCause());
				} catch (IllegalArgumentException e) {
					System.out.println(e.getCause());
				} catch (InvocationTargetException e) {
					System.out.println(e.getCause());
				}
			}
		}

	}
}
