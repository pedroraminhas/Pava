package ist.meic.pa;

import javassist.CannotCompileException;
import javassist.ClassPool;
import javassist.CtClass;
import javassist.CtMethod;
import javassist.NotFoundException;
import javassist.Translator;
import javassist.expr.ExprEditor;
import javassist.expr.MethodCall;

public class DebuggerTranslator implements Translator {

	@Override
	public void start(ClassPool pool) throws NotFoundException,CannotCompileException {
		// TODO Auto-generated method stub
	}
	
	@Override
	public void onLoad(ClassPool pool, String className) throws NotFoundException, CannotCompileException {
		CtClass ctClass = pool.get(className);
		if(!ctClass.getPackageName().contains("ist.meic.pa") && !ctClass.getPackageName().contains("javassist")){
			makeUndoable(ctClass);
		}
	}



	void makeUndoable(CtClass ctClass) throws NotFoundException,CannotCompileException {

		for (CtMethod ctMethod : ctClass.getDeclaredMethods()) {
			final String template = "{"
					+ "$_ =($r) ist.meic.pa.MethodTranslator.genericMethod($0, $class, \"%s\", $args, $sig); }";

			ctMethod.instrument(new ExprEditor() {
				public void edit(MethodCall m)  throws CannotCompileException {
					String modif = String.format(template, m.getMethodName());
					m.replace(modif);
				}

			});

		}

	}

}

