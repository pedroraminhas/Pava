package ist.meic.pa;

import java.lang.reflect.*;

import javassist.*;

import java.io.*;
import java.lang.reflect.*;
import java.util.Scanner;

public class DebuggerCLI {
	public static void main(String[] args) {
		try{
			System.out.println("entrei");
			Translator translator = new MemoizeTranslator();
			ClassPool pool = ClassPool.getDefault();
			Loader classLoader = new Loader();
			classLoader.addTranslator(pool,translator);
			String[] restArgs= new String[args.length - 1];
			System.arraycopy(args,1, restArgs,0,restArgs.length);
			try {
				classLoader.run(args[0],restArgs);
			} catch (Throwable e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
	}catch (Exception e){
		//TODO 
		}
	}
}