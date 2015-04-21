import java.lang.reflect.*;

import java.util.HashMap; 

public class RunTests {
   public static void main(String[] args) throws Exception {
      int passed = 0, failed = 0;
      final HashMap<String,Method> methods = new HashMap<String,Method>();
   
      for (final Method m : Class.forName(args[0]).getDeclaredMethods()) {
      	 if(m.isAnnotationPresent(Setup.class)){   //Catch Annotated Setup Methods
      	    Setup annotInstance = (Setup) m.getAnnotation(Setup.class);
      	    if(!annotInstance.value().equals("")) {
      	       methods.put(annotInstance.value(), m);
      	    }
      	 }

         if (m.isAnnotationPresent(Test.class)) {
            try {
               Test annotInstance = (Test) m.getAnnotation(Test.class);
               String[] setupMethods = annotInstance.value();
               if(setupMethods[0].equals("*")) {
                    for (String s : methods.keySet()) {
                       Method mSetup = methods.get(s);        
                          try {
                       	     mSetup.setAccessible(true);
                             mSetup.invoke(null);	     //Invoke Setup Methods
                          } catch (Throwable ex) {
                              throw ex;
                            }
                    } 
               } else {
                    for(String sx : setupMethods) { 
                       Method s = methods.get(sx);
                       try {
                          s.setAccessible(true);
                          s.invoke(null);		     //Invoke Setup Methods
                       } catch (Throwable ex) {
                           throw ex;
                         }
                    }
                }
               	m.setAccessible(true);
                m.invoke(null);					//Invoke Test Method
                System.out.printf("Test " + m +" OK!\n");
              	passed++;
            } catch (Exception ex) {
                 System.out.printf("Test %s failed %n", m);
                 failed++;
              }       
        }
      }
      System.out.printf("Passed: %d, Failed %d%n", passed, failed);
   }
}

