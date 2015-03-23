package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

public class MemoizeTranslator implements Translator {

	@Override
	public void onLoad(ClassPool pool, String className) throws NotFoundException,
			CannotCompileException {
			CtClass ctClass =pool.get(className);
			try {
				memoizeMethods(ctClass);
			} catch (ClassNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
	}

	@Override
	public void start(ClassPool arg0) throws NotFoundException,
			CannotCompileException {
		// TODO Auto-generated method stub

	}
	
	void memoizeMethods(CtClass ctClass) throws NotFoundException,CannotCompileException,ClassNotFoundException{
		 for(CtMethod ctMethod : ctClass.getDeclaredMethods()){
			 ctMethod.instrument(
				        new ExprEditor() {
				            public void edit(MethodCall m)  throws CannotCompileException
				            {
				            	String s = m.getClassName() + " " + m.getMethodName();
				            	//DebuggerCLI.undoTrail.push(new ObjectFieldValue(null,ctClass.getFields(),m.getClassName(),m.getMethodName()));
				            }
				        });
				  }
		}
	
	boolean NotSystemCall(String name){
		String[] splittedCall = name.split(".");
		if (splittedCall[0].equals("java")){
			return false;}
		return true;
	}
	}

