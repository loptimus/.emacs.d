package refactorerl.ui.core.extensionpoints;

import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.runtime.IConfigurationElement;

import refactorerl.ui.core.assistants.IDragAndDropAdatper;
import refactorerl.ui.core.utils.ExtensionPointUtils;

public class ContextMenuDragAndDropsExtensionPointProxy {

	public static final String EXTENSION_POINT_NAME = "contextmenudraganddrops";
	public static final String PARAMETER_COMMAND_ID = "commandId";
	public static final String PARAMETER_CLASSNAME = "class";
	public static final String PARAMETER_TRANSFORM = "transformName";

	private String commandId;
	private String className;
	private String transformName;
	private IDragAndDropAdatper adapter;

	public ContextMenuDragAndDropsExtensionPointProxy(IConfigurationElement ce) {
		this.commandId = ce.getAttribute(PARAMETER_COMMAND_ID);
		this.className = ce.getAttribute(PARAMETER_CLASSNAME);
		this.transformName = ce.getAttribute(PARAMETER_TRANSFORM);
	}

	public String getCommandId() {
		return commandId;
	}

	public String getClassName() {
		return className;
	}

	public String getTransformName() {
		return transformName;
	}

	public IDragAndDropAdatper getAdapter() {
		if (adapter == null) {
			Object obj = null;
			try {
				obj = ExtensionPointUtils.createObject(className);
			} catch (ExecutionException e) {
				throw new IllegalArgumentException("Could not load the class: " + className, e);
			}

			if (!(obj instanceof IDragAndDropAdatper)) {
				throw new IllegalArgumentException("Loaded class is not instance of IDragAndDropAdapter");
			}
			
			adapter = (IDragAndDropAdatper) obj;
		}

		return adapter;
	}


}
