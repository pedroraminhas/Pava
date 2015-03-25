package ist.meic.pa;

import java.lang.reflect.*;

import javassist.*;

import java.io.*;
import java.lang.reflect.*;
import java.util.Iterator;
import java.util.Scanner;
import java.util.Stack;

class ObjectFieldValue{
	public ObjectFieldValue(Object object2, CtField[] fields2, String className2, String methodName) {
		this.object= object2;
		this.fields = fields2;
		this.className= className2;
		this.call = methodName;
	}
	Object object;
	CtField[] fields;
	String className;
	String call;
}

public class DebuggerCLI {
	public static Stack<ObjectFieldValue> undoTrail = new Stack<ObjectFieldValue>();
	
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
				System.out.println(e.getClass().getName() + ": " + e.getMessage());
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
		classLoader.loadClass("ist.meic.pa.ObjectFieldValue");
		
		} catch (Exception e){
			e.printStackTrace();
		}
		
	}
	public static void infoCommand(){
		
	}
}