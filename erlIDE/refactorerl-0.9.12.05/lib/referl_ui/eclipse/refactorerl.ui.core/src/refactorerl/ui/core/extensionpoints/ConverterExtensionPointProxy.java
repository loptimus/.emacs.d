package refactorerl.ui.core.extensionpoints;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IConfigurationElement;

import refactorerl.ui.core.utils.ExtensionPointUtils;

/**
 * 
 * @author zoltan verebes
 */
public class ConverterExtensionPointProxy {

	public static final String EXTENSION_POINT_NAME = "converters";
	public static final String KEY = "key";
	public static final String CLASSNAME = "class";

	private String key;
	private String className;
	private IErlangObjectConverter converter;

	public ConverterExtensionPointProxy(IConfigurationElement ce) {
		this.key = ce.getAttribute(KEY);
		this.className = ce.getAttribute(CLASSNAME);
	}

	public String getKey() {
		return key;
	}

	public String getClassName() {
		return className;
	}

	public IErlangObjectConverter getConverter() {
		if (converter == null) {
			try {
				converter = (IErlangObjectConverter) ExtensionPointUtils.createObject(className);
			} catch (ExecutionException e) {
				throw new IllegalArgumentException("Could not instantiate class: " + className, e);
			}
		}

		return converter;
	}

}
