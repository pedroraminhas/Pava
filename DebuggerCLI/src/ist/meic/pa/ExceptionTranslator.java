package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;


public class ExceptionTranslator implements Translator {

	@Override
	public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
			CtClass ctClass =pool.get(className);
			//System.out.println(ctClass.getName());
			try {
				memoizeMethods(ctClass,className);
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
	
	void memoizeMethods(CtClass ctClass, String className) throws NotFoundException,CannotCompileException,ClassNotFoundException{
		for(CtMethod ctMethod : ctClass.getDeclaredMethods()){
			 String tmp = "{ist.meic.pa.History.setStackValue(null,$args, \"%s\",\"%s\");}";
			 String modif = String.format(tmp,className ,ctMethod.getName());
			 
			 ctMethod.insertBefore(modif);
			 ctMethod.instrument(
					    new ExprEditor() {
					        public void edit(MethodCall m) throws CannotCompileException {
					        	if(!(m.getClassName().contains("java")) && !(m.getClassName().contains("ist.meic.pa"))) {
					        	String template = "{$_ = ($r) ist.meic.pa.DebuggerCLI.exec(\"" + m.getClassName() + "\",\"" + m.getMethodName() + "\",$0,$args);}";        	
					        	m.replace(template);
					        	}
				        }
				    });
		 }
	}
}

