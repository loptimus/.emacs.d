package refactorerl.ui.core.parameterproviders;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.core.model.root.IErlElement;

/**
 * Clients are intended to subclass this abstract class to collect required parameters for referl refactors from the
 * execution context
 * 
 * @author zoltan verebes
 * 
 */
public abstract class ContextTransformParameterProvider implements ITransformParameterProvider {

	protected ExecutionEvent event;

	public ContextTransformParameterProvider(ExecutionEvent event) {
		
		if (event == null) {
			throw new IllegalArgumentException("Event paramater is required");
		}
		
		this.event = event;
	}

	/**
	 * it tries to find the resource to be modified in the execution event. It check the selections.
	 * Override this function if you want to define the resource by yourself
	 * 
	 * when the current selection is...
	 * <ul>
	 * <li>IErlElement selection in an ITreeSelection: the result is the selected element's module</li>
	 * <li>IResource selection in an ITreeSelection: the result is the selected resource</li>
	 * <li>ITextSelection and the EditorInput is IFileEditorInput the result is the input</li>
	 * </ul>
	 * 
	 *  
	 * @return resource to be modified 
	 */
	public IResource getModifiedResource() throws ExecutionException {
		ISelection selection = HandlerUtil.getCurrentSelectionChecked(event);
		if (selection instanceof ITreeSelection) {
			ITreeSelection treeSelection = (ITreeSelection) selection;
			
			if (treeSelection.getFirstElement() instanceof IResource) {
				return (IResource) treeSelection.getFirstElement();
			}
			
			if (treeSelection.getFirstElement() instanceof IErlElement) {
				IErlElement element = (IErlElement) treeSelection.getFirstElement();
				return element.getResource();
			}
		}
		
		if (selection instanceof ITextSelection)  {
			IEditorInput input = (IEditorInput) HandlerUtil.getActiveEditor(event).getEditorInput();
			if (input instanceof IFileEditorInput) {
				return ((IFileEditorInput) input).getFile();
			}
		}
		
		return null;
	}
}
