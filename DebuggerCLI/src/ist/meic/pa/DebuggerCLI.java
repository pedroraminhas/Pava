package ist.meic.pa;

import javassist.*;
import java.io.*;
import java.lang.reflect.*;

public class DebuggerCLI {
	public static void main(String[] args) {
		try{
		ClassPool pool = ClassPool.getDefault();
		CtClass ctClass= pool.get(args[0]);
		Class<?> rtClass = ctClass.toClass();
		Method main = rtClass.getMethod("main", args.getClass());
		String[] restArgs = new String[args.length-2];
		System.arraycopy(args,2,restArgs,0,restArgs.length);
		main.invoke(null,new Object[]{restArgs});
	}catch (Exception e){
		//TODO 
		}
	}
}