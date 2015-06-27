package refactorerl.ui.extensions.contextparameters;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.jface.viewers.TreePath;
import org.eclipse.ui.handlers.HandlerUtil;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.erlang.IErlFunction;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.erlang.IErlRecordDef;
import org.erlide.core.model.erlang.IErlMacroDef;


import refactorerl.ui.core.assistants.DragAndDropEvent;
import refactorerl.ui.core.parameterproviders.ContextTransformParameterProvider;
import refactorerl.ui.core.utils.SelectionUtils;
import refactorerl.ui.core.utils.WorkspaceUtils;

public class DragAndDropLocator extends ContextTransformParameterProvider {
	public DragAndDropLocator(ExecutionEvent event) {
		super(event);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.parameterproviders.ITransformParameterProvider#getParameterMap()
	 */
	@Override
	public Map<String, Object> getParameterMap() throws ExecutionException {
		if (!(event.getTrigger() instanceof DragAndDropEvent)) {
			return Collections.emptyMap();
		}

		DragAndDropEvent dndEvent = (DragAndDropEvent) event.getTrigger();

		// source
		Object obj = dndEvent.getTargetEvent().data;
		if (!(obj instanceof ITreeSelection)) {
			return Collections.emptyMap();
		}

		ITreeSelection selection = (ITreeSelection) obj;
		if (selection.isEmpty()) {
			return Collections.emptyMap();
		}
		TreePath commonPath = SelectionUtils.getTreeSelectionTreePathPrefix(selection);

		Map<String, Object> params = new HashMap<String, Object>();
		// source file
		IResource source = null;
		if (commonPath.getLastSegment() instanceof IErlElement && commonPath.getParentPath() != null) {
			source = (IResource) commonPath.getParentPath().getLastSegment();
		} else if (commonPath.getLastSegment() instanceof IFile) {
			source = (IResource) commonPath.getLastSegment();
		} else {
			throw new IllegalArgumentException("Move function, macro or record: file paramater could not be located");
		}
		params.put(FILE, WorkspaceUtils.pathToString(source.getFullPath()));

		// target file
		IResource target = null;
		if (dndEvent.getTarget() instanceof IErlElement) {
			target = ((IErlElement) dndEvent.getTarget()).getResource();
		} else if (dndEvent.getTarget() instanceof IResource) {
			target = (IResource) dndEvent.getTarget();
		} else {
			throw new IllegalArgumentException("Move function, macro, record: target module could not be located");
		}

		if (dndEvent.getTransformName().equals(MOVE_FUN)) {
			params.put(NAME, target.getName().replace("." + target.getFileExtension(), ""));
		} else if (dndEvent.getTransformName().equals(MOVE_REC) || dndEvent.getTransformName().equals(MOVE_MAC)) {
			params.put(FILENAME, WorkspaceUtils.pathToString(target.getFullPath()));
		} else {
			throw new IllegalArgumentException("Move function, macro, record: file paramater could not be located");
		}

		if (dndEvent.getTransformName().equals(MOVE_FUN)) {
			// selected functions - funlist
			List<String> funs = new ArrayList<String>();
			Iterator<?> it = selection.iterator();
			while (it.hasNext()) {
				Object selected = it.next();

				if (selected instanceof IErlFunction) {
					IErlFunction fun = (IErlFunction) selected;
					funs.add(fun.getFunctionName() + "/" + fun.getArity());
				}
			}
			params.put(FUNLIST, funs);

		}

		if (dndEvent.getTransformName().equals(MOVE_REC)) {
			// selected records - reclist
			List<String> records = new ArrayList<String>();
			Iterator<?> it = selection.iterator();
			while (it.hasNext()) {
				Object selected = it.next();
				if (selected instanceof IErlRecordDef) {
					IErlRecordDef rec = (IErlRecordDef) selected;
					records.add(rec.getDefinedName());
				}
			}
			params.put(RECLIST, records);

		}

		if (dndEvent.getTransformName().equals(MOVE_MAC)) {
			// selected records - reclist
			List<String> records = new ArrayList<String>();
			Iterator<?> it = selection.iterator();
			while (it.hasNext()) {
				Object selected = it.next();
				if (selected instanceof IErlMacroDef) {
					IErlMacroDef rec = (IErlMacroDef) selected;
					records.add(rec.getDefinedName());
				}
			}
			params.put(MACLIST, records);

		}
		
		return params;
	}

	@Override
	public IResource getModifiedResource() throws ExecutionException {
		ISelection selection = HandlerUtil.getCurrentSelectionChecked(event);

		if (selection instanceof ITreeSelection) {
			ITreeSelection treeSelection = (ITreeSelection) selection;
			Object selected = treeSelection.getFirstElement();

			if (selected instanceof IErlElement) {
				TreePath[] paths = treeSelection.getPathsFor(selected);

				for (int i = paths[0].getSegmentCount() - 1; i > 0; i--) {
					if (paths[0].getSegment(i) instanceof IErlModule) {
						return ((IErlModule) paths[i].getSegment(i)).getResource();
					} else if (paths[0].getSegment(i) instanceof IFile) {
						return (IResource) paths[0].getSegment(i);
					}
				}

				return null;
			}
			return (IResource) treeSelection.getFirstElement();
		}

		return super.getModifiedResource();
	}
}
