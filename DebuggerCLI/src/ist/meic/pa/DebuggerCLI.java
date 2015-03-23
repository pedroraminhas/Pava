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
			ClassPool pool = ClassPool.getDefault();
			Loader classLoader = new Loader();
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
	
	public static void infoCommand(){
		
	}
}