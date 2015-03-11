import java.util.Scanner;

//HelloWorld Class
public class HelloWorld implements Message{
	public void say(){
		System.out.println("Hello World!");
	}

	//Main method
	public static void main(String[] args) {
		String tipo;
		Scanner in = new Scanner(System.in);
		tipo = in.nextLine();
		try {
			Message m= (Message) Class.forName(tipo).newInstance();
			m.say();
		} catch (InstantiationException e) {
			System.out.println("An InstantitionException has benn raised!\n");
			e.printStackTrace();
		} catch (IllegalAccessException e) {
			System.out.println("An IllegalAccessException has benn raised!\n");
			e.printStackTrace();
		} catch (ClassNotFoundException e) {
			System.out.println("An ClassNotFoundException has benn raised!\n");
			e.printStackTrace();
		}

	}

}