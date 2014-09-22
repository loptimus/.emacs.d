package refactorerl.ui.core.extensionpoints;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.core.runtime.IConfigurationElement;
import org.eclipse.core.runtime.IExtension;
import org.eclipse.core.runtime.IExtensionPoint;
import org.eclipse.core.runtime.IExtensionRegistry;
import org.eclipse.core.runtime.Platform;

import refactorerl.ui.Activator;

/**
 * Singleton class to parse the defined extension points
 * for the different refactor tools. It is singleton as hot 
 * plugin is not supported and
 * enough to parse the plugin.xml only once after startup
 * 
 * @author zoltan verebes
 * 
 */
public class ExtensionPointParser {

	private static ExtensionPointParser instance;

	private Map<String, RefactorExtensionPointProxy> refactorExtensions = new HashMap<String, RefactorExtensionPointProxy>();
	private List<ContextMenuDragAndDropsExtensionPointProxy> dragAndDropExtensions = null;
	private List<InboxListenerExtensionPointProxy> inboxListenerExtensions = null;
	private Map<String, ConverterExtensionPointProxy> converters = new HashMap<String, ConverterExtensionPointProxy>();
	private Logger logger = Logger.getLogger(ExtensionPointParser.class.getName());
	/**
	 * search for extension of the commandId. If there are multiple extentions the first is returned
	 * 
	 * @param commandId
	 * @return first extension of the command. null when there is no extension
	 */
	public RefactorExtensionPointProxy getRefactorExtension(String commandId) {
		if (refactorExtensions.containsKey(commandId)) {
			return refactorExtensions.get(commandId);
		}

		IExtensionRegistry er = Platform.getExtensionRegistry();
		IExtensionPoint ep = er.getExtensionPoint(Activator.PLUGIN_ID, RefactorExtensionPointProxy.EXTENSION_POINT_NAME);

		for (IExtension extension : ep.getExtensions()) {
			for (IConfigurationElement ce : extension.getConfigurationElements()) {
				String extensionCommandId = ce.getAttribute(RefactorExtensionPointProxy.PARAMETER_COMMAND_ID);
				if (extensionCommandId != null && extensionCommandId.equals(commandId)) {
					RefactorExtensionPointProxy refactor = new RefactorExtensionPointProxy(ce);
					refactorExtensions.put(commandId, refactor);
					return refactor;
				}
			}
		}

		return null;
	}

	/**
	 * search for extension of the commandId. If there are multiple extentions the first is returned
	 * 
	 * @param commandId
	 * @return first extension of the command. null when there is no extension
	 */
	public List<ContextMenuDragAndDropsExtensionPointProxy> findContextMenuDragAndDropsExtensionPointProxyExtensions() {
		if (dragAndDropExtensions != null) {
			return dragAndDropExtensions;
		}

		IExtensionRegistry er = Platform.getExtensionRegistry();
		IExtensionPoint ep = er.getExtensionPoint(Activator.PLUGIN_ID, ContextMenuDragAndDropsExtensionPointProxy.EXTENSION_POINT_NAME);

		dragAndDropExtensions = new ArrayList<ContextMenuDragAndDropsExtensionPointProxy>();

		for (IExtension extension : ep.getExtensions()) {
			for (IConfigurationElement ce : extension.getConfigurationElements()) {
				dragAndDropExtensions.add(new ContextMenuDragAndDropsExtensionPointProxy(ce));
			}
		}

		return dragAndDropExtensions;
	}

	/**
	 * search for converter extension at the converters extension point
	 * 
	 * @param key
	 *            of the converter and the message
	 * @return first converter extension with the given key. null when there is no such extension
	 */
	public ConverterExtensionPointProxy getErlangObjectConverterProxy(String key) {
		if (converters.containsKey(key)) {
			return converters.get(key);
		}
		IExtensionRegistry er = Platform.getExtensionRegistry();
		IExtensionPoint ep = er.getExtensionPoint(Activator.PLUGIN_ID, ConverterExtensionPointProxy.EXTENSION_POINT_NAME);

		for (IExtension extension : ep.getExtensions()) {
			for (IConfigurationElement ce : extension.getConfigurationElements()) {
				String keyParameter = ce.getAttribute(ConverterExtensionPointProxy.KEY);
				if (keyParameter != null && keyParameter.equals(key)) {
					ConverterExtensionPointProxy converter = new ConverterExtensionPointProxy(ce);
					converters.put(key, converter);
					return converter;
				}
			}
		}

		return null;

	}

	/**
	 * search for extension on the inbox listeners extension point
	 * 
	 * @return list of inbox listeners which are configured in the plugins
	 */
	public List<InboxListenerExtensionPointProxy> findInboxListenerExtensionPointProxies() {
		if (inboxListenerExtensions != null) {
			return inboxListenerExtensions;
		}

		IExtensionRegistry er = Platform.getExtensionRegistry();
		IExtensionPoint ep = er.getExtensionPoint(Activator.PLUGIN_ID, InboxListenerExtensionPointProxy.EXTENSION_POINT_NAME);

		inboxListenerExtensions = new ArrayList<InboxListenerExtensionPointProxy>();
		for (IExtension extension : ep.getExtensions()) {
			for (IConfigurationElement ce : extension.getConfigurationElements()) {
				inboxListenerExtensions.add(new InboxListenerExtensionPointProxy(ce));
			}
		}
		logger.info("Listeners found: " + inboxListenerExtensions.size());
		return inboxListenerExtensions;
	}

	/**
	 * 
	 * @return the singleton instance of the ExtensionPointParser
	 */
	public static ExtensionPointParser getInstance() {
		if (instance == null) {
			instance = new ExtensionPointParser();
		}

		return instance;
	}

	public List<String> getContextTransformParameterClassNames(IConfigurationElement ce) {
		return getTransformParameterClassNames(ce, RefactorExtensionPointProxy.CONTEXT_TRANSFORM_PARAMETER);
	}

	public List<String> getManualTransformParameterClassNames(IConfigurationElement ce) {
		return getTransformParameterClassNames(ce, RefactorExtensionPointProxy.MANUAL_TRANSFORM_PARAMETER);
	}

	private List<String> getTransformParameterClassNames(IConfigurationElement ce, String elementName) {
		List<String> classNames = new ArrayList<String>();
		for (IConfigurationElement element : ce.getChildren(elementName)) {
			String className = element.getAttribute(RefactorExtensionPointProxy.CLASS);
			if (className != null && !className.equals("")) {
				classNames.add(className);
			}
		}
		return classNames;
	}

	public List<String> getManualTransformParameters(IConfigurationElement ce, String className) {

		List<String> parameters = new ArrayList<String>();
		for (IConfigurationElement element : ce.getChildren(RefactorExtensionPointProxy.MANUAL_TRANSFORM_PARAMETER)) {

			if (className.equals(element.getAttribute(RefactorExtensionPointProxy.CLASS))) {
				for (IConfigurationElement paramElement : element.getChildren(RefactorExtensionPointProxy.PARAMETER)) {
					String key = paramElement.getAttribute("key");
					if (key != null && !key.equals("")) {
						parameters.add(key);
					}
				}
			}

		}

		return parameters;

	}

}
