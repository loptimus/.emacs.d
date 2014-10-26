package refactorerl.ui.extensions.contextparameters;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.runtime.IPath;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.ui.IWorkbench;
import org.eclipse.ui.IWorkbenchPage;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.part.FileEditorInput;

import refactorerl.ui.Activator;
import refactorerl.ui.core.parameterproviders.ContextTransformParameterProvider;
import refactorerl.ui.core.utils.WorkspaceUtils;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlMacroDef;
import org.erlide.core.model.root.IErlElement;

/**
 * Class to locate the selected resource from a TreeSelection or from a TextSelection of the Erlang editor.
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */
public class ResourceLocator extends ContextTransformParameterProvider {
	public ResourceLocator(ExecutionEvent event) {
		super(event);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @seeerlang.refactor.core.parameterproviders.ITransformParameterProvider# getParameterMap()
	 */
	@Override
	public Map<String, Object> getParameterMap() throws ExecutionException {
		ISelection selection = HandlerUtil.getCurrentSelectionChecked(event);

		if (selection.isEmpty()) {
			throw new ExecutionException("Selection is empty: no selection");
		}

		if (selection instanceof ITreeSelection) {
			ITreeSelection treeSelection = (ITreeSelection) selection;
			
			if (treeSelection.getFirstElement() instanceof IResource) {
				IResource resource = (IResource) (treeSelection).getFirstElement();
				return createParamaterMap(resource.getFullPath());
			}
			
			if (treeSelection.getFirstElement() instanceof IErlRecordDef) {
				IErlElement element = (IErlElement)(treeSelection.getFirstElement());
				IResource resource = element.getResource();
				IPath path = resource.getFullPath();
				Map<String, Object> parameters = createParamaterMap(path);
				parameters.put(RECORD,((IErlRecordDef)(treeSelection.getFirstElement())).getDefinedName() );
				return parameters;
			}
			
			if (treeSelection.getFirstElement() instanceof IErlMacroDef) {
				IErlElement element = (IErlElement)(treeSelection.getFirstElement());
				IResource resource = element.getResource();
				IPath path = resource.getFullPath();
				Map<String, Object> parameters = createParamaterMap(path);
				parameters.put(MACRO,((IErlMacroDef)(treeSelection.getFirstElement())).getDefinedName() );
				return parameters;
			}
			
			return null;
		}

		if (selection instanceof ITextSelection) {
			IWorkbench workbench = Activator.getDefault().getWorkbench();
			IWorkbenchPage page = workbench.getActiveWorkbenchWindow().getActivePage();

			if (page.getActiveEditor() == null) {
				throw new IllegalArgumentException("Active editor not found");
			}

			Map<String, Object> parameters = new HashMap<String, Object>();
			IPath path = ((FileEditorInput) page.getActiveEditor().getEditorInput()).getPath();
			path = path.setDevice(path.getDevice().toLowerCase());
			parameters.put(FILE, path.toPortableString());
			return parameters;
			//return createParamaterMap(path);
		}

		return null;
	}

	private Map<String, Object> createParamaterMap(IPath path) {
		Map<String, Object> parameters = new HashMap<String, Object>();
		parameters.put(FILE, WorkspaceUtils.pathToString(path));
		return parameters;
	}
}
