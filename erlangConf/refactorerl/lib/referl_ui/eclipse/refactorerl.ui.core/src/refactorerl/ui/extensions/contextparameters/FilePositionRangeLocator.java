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
 *  * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */

public class FilePositionRangeLocator extends ContextTransformParameterProvider {
	public FilePositionRangeLocator(ExecutionEvent event) {
		super(event);
	}

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
			throw new ExecutionException("Position cannot be located as there is no active editor or its type is not IFileEditorInput");
		}
		Map<String, Object> parameters = new HashMap<String, Object>();

		IFileEditorInput input = (IFileEditorInput) editorPart.getEditorInput();
		parameters.put(FILE, WorkspaceUtils.pathToString(input.getFile().getFullPath()));

		/*
		 * get the position of the cursor in the editor: emacs style position calculation
		 */
		int offset = textSelection.getOffset();
		int length = textSelection.getLength();
		int startLine = textSelection.getStartLine();
		int endLine = textSelection.getEndLine();
		offset-=startLine*(System.getProperty("line.separator").length()-1);
		if (endLine>0) length-=(endLine-startLine)*(System.getProperty("line.separator").length()-1);

		parameters.put(POSRANGE, new int[] { offset+1, offset + length});
		
		return parameters;
	}

}
