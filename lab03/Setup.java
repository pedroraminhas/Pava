import java.lang.annotation.*;

/**
 * Indicates that the annotated method is a setup method.
 * This annotation should be used only on parameterless static methods.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Setup {
	public String value() default ""; 
}

