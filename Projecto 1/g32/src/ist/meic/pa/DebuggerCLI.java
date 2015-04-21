package ist.meic.pa;

import java.lang.reflect.*;
import javassist.*;

import java.io.*;
import java.lang.reflect.*;
import java.util.Iterator;
import java.util.Scanner;
import java.util.Stack;

/*
 * DebuggerCLI class
 */
public class DebuggerCLI {
	
	public static void main(String[] args) {
		try{
			Translator translator = new DebuggerTranslator();
			ClassPool pool = ClassPool.getDefault();
			Loader classLoader = new Loader();
			Class<?> History =classLoader.loadClass("ist.meic.pa.History");
			classLoader.addTranslator(pool,translator);
			String[] restArgs= new String[args.length - 1];
			Method m = History.getMethod("pushStack", new Class[]{ Object.class,String.class,Object[].class,String.class});
			System.arraycopy(args,1, restArgs,0,restArgs.length);
			m.invoke(null, new Object[] {null, args[0], restArgs,"main"});
			try {
				classLoader.run(args[0],restArgs);
			} catch (Throwable e) {
				e.printStackTrace();
				}
			
	}catch (Exception e){
		//TODO
		}
	}
}