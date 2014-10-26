package refactorerl.ui.core.extensionpoints;

import java.util.List;

import org.eclipse.core.runtime.IConfigurationElement;

public class RefactorExtensionPointProxy {

	public static final String EXTENSION_POINT_NAME = "refactors";
	public static final String PARAMETER_COMMAND_ID = "commandId";
	public static final String PARAMETER_NAME = "name";
	public static final String PARAMETER_DESCRIPTION = "description";
	public static final String PARAMETER_TRANSFORM_NAME = "transformName";
	public static final String CONTEXT_TRANSFORM_PARAMETER = "ContextTransformParameterClassProvider";
	public static final String MANUAL_TRANSFORM_PARAMETER = "ManualTransformParameterClassProvider";
	public static final String PARAMETER = "parameter";
	public static final String CLASS = "class";

	private IConfigurationElement ce; // will be used to cache class names
	private String commandId;
	private String transformName;
	private String name;
	private String description;
	private List<String> parameterProviderClassNames;
	private List<String> manualParamaterProviderClassName;

	public RefactorExtensionPointProxy(IConfigurationElement ce) {
		this.ce = ce;
		this.commandId = ce.getAttribute(PARAMETER_COMMAND_ID);
		this.transformName = ce.getAttribute(PARAMETER_TRANSFORM_NAME);
		this.name = ce.getAttribute(PARAMETER_NAME);
		this.description = ce.getAttribute(PARAMETER_DESCRIPTION);
	}

	public String getCommandId() {
		return commandId;
	}

	public String getTransformName() {
		return transformName;
	}

	public String getName() {
		return name;
	}

	public String getDescription() {
		return description;
	}
	
	public List<String> getContextParameterProviderClassNames() {
		if (parameterProviderClassNames == null) {
			parameterProviderClassNames = ExtensionPointParser.getInstance().getContextTransformParameterClassNames(ce);
		}
		return parameterProviderClassNames;
	}

	public List<String> getManualParameterProviderClassNames() {
		if (manualParamaterProviderClassName == null) {
			manualParamaterProviderClassName = ExtensionPointParser.getInstance()
					.getManualTransformParameterClassNames(ce);
		}
		return manualParamaterProviderClassName;
	}

	public List<String> getParameters(String className) {
		return ExtensionPointParser.getInstance().getManualTransformParameters(ce, className);
	}
}
