package refactorerl.ui.core.extensionpoints;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IConfigurationElement;

import refactorerl.ui.core.connection.IInboxListener;
import refactorerl.ui.core.utils.ExtensionPointUtils;

public class InboxListenerExtensionPointProxy {

	public static final String EXTENSION_POINT_NAME = "inboxlisteners";
	public static final String CLASS = "class";
	public static final String NAME = "name";
	public static final String KEY = "key";

	private String name;
	private String className;
	private String key;
	private IInboxListener inboxListener;

	public InboxListenerExtensionPointProxy(IConfigurationElement ce) {
		this.name = ce.getAttribute(NAME);
		this.className = ce.getAttribute(CLASS);
		this.key = ce.getAttribute(KEY);
		if (key.equals("")) {
			this.key = null;
		}
	}

	public String getName() {
		return name;
	}

	public String getClassName() {
		return className;
	}

	public IInboxListener getInboxListener() {
		if (inboxListener == null) {
			try {
				inboxListener = (IInboxListener) ExtensionPointUtils.createObject(className);
			} catch (ExecutionException e) {
				throw new IllegalArgumentException("Could not instantiate class: " + className, e);
			}
		}

		return inboxListener;
	}

	public String getKey() {
		return key;
	}

}
