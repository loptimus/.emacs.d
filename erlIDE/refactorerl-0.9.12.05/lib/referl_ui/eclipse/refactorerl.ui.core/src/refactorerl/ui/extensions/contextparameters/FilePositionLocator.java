package refactorerl.ui.extensions.contextparameters;

import java.util.HashMap;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.IFileEditorInput;
import org.eclipse.ui.handlers.HandlerUtil;

import refactorerl.ui.core.parameterproviders.ContextTransformParameterProvider;
import refactorerl.ui.core.utils.WorkspaceUtils;

/**
 * Class to locate the selected file and the selection position of the cursor. It is Emacs style position, so the
 * integer is the number of bytes from the beginning of the file.
 * 
 * This locator is legal only on the editor's context, so if the selection is not an ITextSelection or there is no
 * active editor it throws exception
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */
public class FilePositionLocator extends ContextTransformParameterProvider {
	public FilePositionLocator(ExecutionEvent event) {
		super(event);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see erlang.refactor.core.parameterproviders.ITransformParameterProvider#getParameterMap()
	 */
	@Override
	public Map<String, Object> getParameterMap() throws ExecutionException {		
		ISelection selection = HandlerUtil.getCurrentSelectionChecked(event);

		if (!(selection instanceof ITextSelection)) {
			throw new ExecutionException("Selection is not a text selection of Erlang Editor");
		}

		ITextSelection textSelection = (ITextSelection) selection;

		// it throws exception in the case of no active editor
		IEditorPart editorPart = HandlerUtil.getActiveEditorChecked(event);

		// check if the selected file is the same as the file in the editor input
		if (editorPart.getEditorInput() == null || !(editorPart.getEditorInput() instanceof IFileEditorInput)) {
			throw new ExecutionException(
					"Position cannot be located as there is no active editor or its type is not IFileEditorInput");
		}
		Map<String, Object> parameters = new HashMap<String, Object>();
		
		IFileEditorInput input = (IFileEditorInput) editorPart.getEditorInput();
		parameters.put(FILE, WorkspaceUtils.pathToString(input.getFile().getFullPath()));

		/*
		 * get the position of the cursor in the editor: emacs style position calculation
		 */
		//parameters.put(POSITION, textSelection.getOffset() + 1);
		int offset = textSelection.getOffset();
		int startLine = textSelection.getStartLine();
		offset-=startLine*(System.getProperty("line.separator").length()-1);
		
		parameters.put(POSITION, offset+1);
		//System.err.println("Position: " + textSelection.getOffset());
		
		return parameters;
	}
}
