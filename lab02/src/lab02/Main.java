package lab02;

import java.lang.reflect.Array;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.util.Arrays;
import java.util.Scanner;
import java.util.TreeMap;

public class Main {
	public static void main(String[] args) {

		TreeMap vectorObjectos= new TreeMap();
		Object objectoParaGuardar=null;
		String[] input;
		Scanner in = new Scanner(System.in);

		while(true){
			input = in.nextLine().split(" ");

			switch (input[0]){
			
			case "Class": try {
				objectoParaGuardar = Class.forName(input[1]).newInstance();
				System.out.println(objectoParaGuardar.getClass());
			} catch (InstantiationException | IllegalAccessException| ClassNotFoundException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
			break;

			case "Set": vectorObjectos.put(input[1], objectoParaGuardar);
			System.out.println((vectorObjectos.get(input[1])).getClass());
			break;

			case "Get": System.out.println((vectorObjectos.get(input[1])).getClass());
			objectoParaGuardar= vectorObjectos.get(input[1]);
			break;
			
			default :  Method[] allMethods = objectoParaGuardar.getClass().getDeclaredMethods();
			Object[] tiposParametros = null;
			 for (Method m : allMethods) {
				 
				 if(input[0].equals(m.getName()))
					try {
						tiposParametros = m.getParameterTypes();
						if(tiposParametros.length== (input.length-1))
						System.out.println(m.invoke(objectoParaGuardar, Arrays.copyOfRange(input, 1, input.length)));
						
					} catch (IllegalAccessException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (IllegalArgumentException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					} catch (InvocationTargetException e) {
						// TODO Auto-generated catch block
						e.printStackTrace();
					}
			 }
			 break;

		}
	}
}
}
