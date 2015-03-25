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
	public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
			CtClass ctClass =pool.get(className);
			//System.out.println(ctClass.getName());
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
			 //System.out.println(ctMethod.getName());
			 //System.out.println(ctClass.getName());
			 //if (!ctMethod.getName().equals("main"))
			 //System.out.println(ctMethod.getName());
				// ctMethod.insertBefore("System.out.println(\"Inseri isto!->\");");
			 /*String tmp = "{System.out.println(\"%s\");}";
			 
			 String modif = String.format(tmp, ctMethod.toString());
			 
			 ctMethod.insertBefore(modif);*/
			 
			 ctMethod.instrument(
					    new ExprEditor() {
					        public void edit(MethodCall m)
					                      throws CannotCompileException
					        {
					        	String tmp = "{  System.out.println(\"%s\"); $_ = $proceed($$); }";
								 
								String modif = String.format(tmp, m.where().getLongName().toString());
								 
								 m.replace(modif);
					        }
					    });
			// ctMethod.addCatch("{ System.out.println($e); throw $e; }", ctClass);
		 }
		 /*CtMethod method = ctClass.getDeclaredMethod("main");
		    method.instrument(
		            new ExprEditor() {
		                public void edit(MethodCall m)
		                              throws CannotCompileException
		                {
		                    System.out.println("Classe: "+m.getClassName() + " Metodo: " + m.getMethodName() + " Assinatura: " + m.getSignature());
		                }
		            });*/
	}
		 
	
/*	boolean NotSystemCall(String name){
		String[] splittedCall = name.split(".");
		if (splittedCall[0].equals("java")){
			return false;}
		return true;
	}*/
}

