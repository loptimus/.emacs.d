package refactorerl.ui.extensions.contextparameters;

import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;

import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.common.NotDefinedException;
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


import refactorerl.ui.core.parameterproviders.ContextTransformParameterProvider;
import refactorerl.ui.core.utils.SelectionUtils;
import refactorerl.ui.core.utils.WorkspaceUtils;
/**
* 
* @author  Gergely Horvath 
* 
*/

public class ErlElementLocator extends ContextTransformParameterProvider {
	public ErlElementLocator(ExecutionEvent event) {
		super(event);
	}

	/*
	 * (non-Javadoc)
	 * 
	 * @see refactorerl.ui.core.parameterproviders.ITransformParameterProvider#getParameterMap()
	 */
	@Override
	public Map<String, Object> getParameterMap() throws ExecutionException {
		ISelection iselection = HandlerUtil.getCurrentSelectionChecked(event);
	
		if (iselection.isEmpty() || !(iselection instanceof ITreeSelection)) {
			throw new ExecutionException("Selection is not TreeSelection");
		}
		
		ITreeSelection selection = (ITreeSelection) iselection;
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

		String s = null;
		try {
			s = event.getCommand().getName();
		} catch (NotDefinedException e){
			e.printStackTrace();
		}
		
		if (s != null && s.equals("movefun"))  {
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


		if (s != null && s.equals("moverec")) {
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

		if (s!=null && s.equals("movemac")) {
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
