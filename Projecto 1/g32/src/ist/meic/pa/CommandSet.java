package ist.meic.pa;

import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

/*
 * Command Set sets a value to the given attribute
 */
public class CommandSet {
	
	public static void main(String[] input) throws InvocationTargetException{
		
		Field[] allFields = History.calledObjects.peek().getObject().getClass().getDeclaredFields();
		
		
		//Searches for the field given in input
		for(Field field : allFields){
			if(field.getName().equals(input[1])){
				try {
					Object obj= null;
					field.setAccessible(true);
					
					//Gets the type of the field
					Class type = field.get(History.calledObjects.peek().getObject()).getClass();
					Object fieldValue =field.get(History.calledObjects.peek().getObject());

					//invoke valueOf in the given type with input
					for(Method m : type.getDeclaredMethods() ){
						if(m.getName().equals("valueOf") && m.getGenericParameterTypes()[0].equals(input[2].getClass()) && (m.getGenericParameterTypes().length==1)){
							obj =m.invoke(type, input[2]);
						}
					}
					//Sets the field of the object
					field.set(History.calledObjects.peek().getObject(), obj);


				} catch (IllegalArgumentException e) {
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				}
			}
		}
	}
}
