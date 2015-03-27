package ist.meic.pa;


import java.lang.reflect.Constructor;
import java.lang.reflect.Method;
import java.util.LinkedList;
import java.util.List;

import javassist.*;


public class DebuggerCLI {
	public static Class<?> History;
	
	public static void main(String[] args) {
		try{
			Translator translator = new MemoizeTranslator();
			
			Loader classLoader = new Loader();
			loadAllClasses(classLoader);
			
			ClassPool pool = ClassPool.getDefault();
			
			classLoader.addTranslator(pool,translator);
			String[] restArgs= new String[args.length - 1];
			System.arraycopy(args,1, restArgs,0,restArgs.length);
			try {
				classLoader.run(args[0],restArgs);
			} catch (Throwable e) {
				Method printStack= History.getMethod("printStack");
				printStack.invoke(null);
				//e.printStackTrace();
			//	System.out.println(e.getClass().getName() + ": " + e.getMessage());
				}
			
	}catch (Exception e){
		//TODO
		}
	}
	
	public static void loadAllClasses(Loader classLoader) {
		try {
		classLoader.loadClass("javassist.CannotCompileException");
		classLoader.loadClass("javassist.ClassPool");
		classLoader.loadClass("javassist.CtClass");
		classLoader.loadClass("javassist.CtMethod");
		classLoader.loadClass("javassist.NotFoundException");
		classLoader.loadClass("javassist.Translator");
		classLoader.loadClass("javassist.expr.ExprEditor");
		classLoader.loadClass("javassist.expr.MethodCall");
		classLoader.loadClass("javassist.expr.ConstructorCall");
		classLoader.loadClass("ist.meic.pa.DebuggerCLI");
		classLoader.loadClass("java.lang.System");
		History = classLoader.loadClass("ist.meic.pa.History");
		
		
		//classLoader.loadClass("ist.meic.pa.ObjectFieldValue");
		
		} catch (Exception e){
			e.printStackTrace();
		}
		
	}
	
	public static Object exec(String className, String methodName, Object object,Object[] args){
		Object ret = null;
		try {		
			try {
				Class<?> c = Class.forName(className);
		
				Constructor<?> constructor = c.getDeclaredConstructor();
				
				constructor.setAccessible(true);
				
				Object o = constructor.newInstance();
				
				Method method=null;
				for(Method m : c.getDeclaredMethods()) {
					if(m.getName().equals(methodName)){
						m.setAccessible(true);
						method = (Method) m;
						break;
					}
				}
				ret = method.invoke(o,args[0]);		
				
			} catch (IllegalAccessException e) {
				System.out.println("ALGUUM!!");
			} catch (ClassNotFoundException e) {
				System.out.println("ALGU3");			
			}
		} catch(Throwable e) {
			System.out.println(e.getCause());
		}
		return ret;		
		
	}
	
	public static List<Method> getAllMethodsOf(Class<?> clazz) {
		Method[] classMethods = clazz.getDeclaredMethods();
		List<Method> allMethods;
		if (clazz.equals(Object.class)) {
			allMethods = new LinkedList<Method>();
		} else {
			allMethods = getAllMethodsOf(clazz.getSuperclass());
		}
		for (Method m : classMethods) {
			allMethods.add(m);
		}
		return allMethods;
	}
	
	
	public static void infoCommand(){
		
	}
}