package ist.meic.pa;

import java.lang.reflect.Field;
/*
 * Command Get
 * It receives a variable name and returns the value of the variable on the current moment
 * 
 */
public class CommandGet {
	public static void main(String[] input){
		Field[] allFields = History.calledObjects.peek().getObject().getClass().getDeclaredFields();
		
		/*
		 * Searches for the field that was given at the input and prints its value
		 * */
		for(Field field : allFields){
			if(field.getName().equals(input[1])){
				try {
					field.setAccessible(true);
					System.out.println(field.get(History.calledObjects.peek().getObject()));
				} catch (IllegalArgumentException e) {
					e.printStackTrace();
				} catch (IllegalAccessException e) {
					e.printStackTrace();
				}
			}
		}
	}
}