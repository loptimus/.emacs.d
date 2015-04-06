package refactorerl.ui.extensions.contextparameters;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;

import refactorerl.ui.core.parameterproviders.ContextTransformParameterProvider;
/**
 * 
 * @author Gergely Horvath
 *
 */
public class NullParameterLocator extends ContextTransformParameterProvider {
	public NullParameterLocator(ExecutionEvent event) {
		super(event);
	}

	@Override
	public Map<String, Object> getParameterMap() throws ExecutionException {
		Map<String, Object> parameters = new HashMap<String, Object>();
		return parameters;
	}

}
