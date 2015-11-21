package refactorerl.ui.core.parameterproviders;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.ltk.ui.refactoring.UserInputWizardPage;

public abstract class ManualTransformParameterProvider extends UserInputWizardPage 
	implements ITransformParameterProvider {

	private Map<String, Object> parameters = new HashMap<String, Object>();

	protected List<String> args;
	
	protected ExecutionEvent event;

	public ManualTransformParameterProvider(ExecutionEvent event, String name, String description, ArrayList<String> args) {
		super(name);
		
		if (event == null) {
			throw new IllegalArgumentException("Event parameter cannot be null");
		}
		
		setTitle(name == null || name.equals("") ? "Refactor name not configured" : name);
		setDescription(description == null || description.equals("") ? "Refactor Description not provided"
				: description);
		this.args = args;
		this.event = event;
	}

	public void setParameterMap(Map<String, Object> parameters) {
		this.parameters = parameters;
	}

	public void addParameter(String key, Object value) {
		if (parameters == null) {
			throw new IllegalArgumentException("Parameter map is not set for this page");
		}

		parameters.put(key, value);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erlang.refactor.core.parameterproviders.ITransformParameterProvider#getParameterMap()
	 */
	@Override
	public Map<String, Object> getParameterMap() {
		return parameters;
	}
}
