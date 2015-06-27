package refactorerl.ui.extensions.contextparameters;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;

import refactorerl.ui.core.parameterproviders.ContextTransformParameterProvider;

/**
 * Collects module name, function name and arity when the selection is an ITreeSelection with one element and it is an instance of IErlFunction.
 * It is used by rename function Refactoring
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */
public class ModuleFunctionArityLocator extends ContextTransformParameterProvider {
	public ModuleFunctionArityLocator(ExecutionEvent event) {
		super(event);
	}

	/*
	 * (non-Javadoc)
	 * @see refactorerl.ui.core.parameterproviders.ITransformParameterProvider#getParameterMap()
	 */
	@Override
	public Map<String, Object> getParameterMap() throws ExecutionException {
		ISelection selection = HandlerUtil.getCurrentSelectionChecked(event);

		if (!(selection instanceof ITreeSelection)) {
			throw new IllegalArgumentException("");
		}

		ITreeSelection sel = (ITreeSelection) selection;

		if (sel.size() != 1) {
			throw new IllegalArgumentException("Exactly one function selection is legal");
		}

		if (!(sel.getFirstElement() instanceof IErlFunction)) {
			throw new IllegalArgumentException("Function selection is required");
		}

		IErlFunction fun = (IErlFunction) sel.getFirstElement();

		Map<String, Object> parameters = new HashMap<String, Object>();
		IErlModule module = (IErlModule) fun.getParent();
		parameters.put(MODULE, module.getName().replace(".erl", ""));
		parameters.put(FUNCTION, fun.getFunctionName());
		parameters.put(ARITY, fun.getArity());

		return parameters;
	}
}
