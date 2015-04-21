package ist.meic.pa;

/*
 * Class ObjectFieldValue
 */
public class ObjectFieldValue {
	Object object;
	String className;
	Object[] args;
	String method;
	
	/*
	 * Constructor
	 */
	public ObjectFieldValue(Object object2, String className2, Object[] args2, String method2){
		this.object=object2;
		this.className=className2;
		this.args=args;
		this.method=method2;
	}
	
	public Object getObject(){
		return this.object;
	}
	
	public String getClassName(){
		return this.className;
	}
	
	public Object[] getArgs(){
		return this.args;
	}
	public String getMethod(){
		return this.method;
	}
}
