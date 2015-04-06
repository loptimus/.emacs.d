package refactorerl.ui.core.utils;

import java.lang.reflect.Constructor;
import java.util.ArrayList;
import java.util.List;

import org.eclipse.core.commands.ExecutionException;

/**
 * Convenience functions to handle and parse extension points defined for Refactorerl's extension points
 * 
 * @author zoltan verebes
 * 
 */
public final class ExtensionPointUtils {
	/**
	 * Instantiate a class of the given class name and class its constructor with the defined arguments. It checks all of the possible
	 * constructors to find the one which best fit
	 * 
	 * @param className
	 * @param args
	 * @return
	 * @throws ExecutionException
	 */
	public static Object createObject(String className, Object... args) throws ExecutionException {
		if (className == null || className.equals("")) {
			throw new IllegalArgumentException("Empty class name found.");
		}

		ClassLoader loader = Thread.currentThread().getContextClassLoader();

		Class<?> clazz = null;
		try {
			clazz = (Class<?>) loader.loadClass(className);
		} catch (ClassNotFoundException e) {
			throw new ExecutionException("Coul" + "d not found the given class. Its name is " + className
					+ ". Please double-check your plugin.xml", e);
		}

		List<Class<?>> paramTypes = new ArrayList<Class<?>>();
		for (Object obj : args) {
			paramTypes.add(obj.getClass());
		}

		Object obj = null;
		try {
			Constructor<?> con = clazz.getConstructor(paramTypes.toArray(new Class<?>[0]));
			obj = con.newInstance(args);
		} catch (Exception e) {
			throw new ExecutionException("Could not instantiate the given class. Its name is " + className
					+ ". Please double-check your plugin.xml", e);
		}

		return obj;
	}

}
