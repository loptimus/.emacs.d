package refactorerl.ui.extensions.draganddropadapters;

import java.util.Iterator;

import org.eclipse.core.resources.IFile;
import org.eclipse.jface.util.LocalSelectionTransfer;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.swt.dnd.TransferData;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlMacroDef;

import refactorerl.ui.core.assistants.IDragAndDropAdatper;
import refactorerl.ui.core.constants.ReferlConstants;
import refactorerl.ui.core.utils.SelectionUtils;

public class ErlElementDragAndDropAdapter implements IDragAndDropAdatper, ReferlConstants {

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.assistants.IDragAndDropAdatper#validateDrop(java.lang.Object, int, org.eclipse.swt.dnd.TransferData)
	 */
	@Override
	public boolean validateDrop(String transformName, Object target, int operation, TransferData transferData) {
		if (LocalSelectionTransfer.getTransfer().isSupportedType(transferData)) {

			// target must be a module or header
			// TODO: check if it is part of the database, but to do this a new function is required in referl_ui module
			if (!(target instanceof IFile) || !(((IFile) target).getFileExtension().equals("erl") || (((IFile) target).getFileExtension().equals("hrl"))  )) {
				return false;
			}

			ISelection selection = LocalSelectionTransfer.getTransfer().getSelection();

			if (selection.isEmpty() || !(selection instanceof ITreeSelection)) {
				return false;
			}

			Object first = ((ITreeSelection) selection).getFirstElement();
			if (MOVE_FUN.equals(transformName) && !(first instanceof IErlFunction)) {
				return false;
			} else if (MOVE_REC.equals(transformName) && !(first instanceof IErlRecordDef)) {
				return false;
			} else if (MOVE_MAC.equals(transformName) && !(first instanceof IErlMacroDef)) {
				return false;
			} 

			Iterator<?> it = ((ITreeSelection) selection).iterator();
			while (it.hasNext()) {
				Object element = it.next();
				if (!element.getClass().equals(first.getClass())) {
					return false;
				}
			}

			TreePath commonPath = SelectionUtils.getTreeSelectionTreePathPrefix((ITreeSelection) selection);
			if (commonPath.getLastSegment() instanceof IErlElement) {
				return true;
			} else if (commonPath.getLastSegment() instanceof IFile) {
				return true;
			}
		}

		return false;
	}

}
