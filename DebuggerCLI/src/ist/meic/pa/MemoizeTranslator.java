package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;

public class MemoizeTranslator implements Translator {

	@Override
	public void onLoad(ClassPool pool, String className) throws NotFoundException,
			CannotCompileException {
			CtClass ctClass =pool.get(className);
	}

	@Override
	public void start(ClassPool arg0) throws NotFoundException,
			CannotCompileException {
		// TODO Auto-generated method stub

	}
	
	void memoizeMethods(CtClass ctClass) throws NotFoundException,CannotCompileException,ClassNotFoundException{
		/*for(CtMethod ctMethod : ctClass.getDeclaredMethods()){
			Object[] annotations = ctMethod.getAnnotations();
			if((annotations.length ==1 ) && (annotations[0] instanceof Memoized))
		}*/
	}

}
