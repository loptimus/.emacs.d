package refactorerl.ui.core.handlers;

import java.lang.reflect.InvocationTargetException;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IResourceProxy;
import org.eclipse.core.resources.IResourceProxyVisitor;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.jface.text.ITextSelection;
import org.eclipse.jface.viewers.ISelection;
import org.eclipse.jface.viewers.ITreeSelection;
import org.eclipse.swt.widgets.DirectoryDialog;
import org.eclipse.swt.widgets.Display;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.IEditorInput;
import org.eclipse.ui.IEditorPart;
import org.eclipse.ui.PlatformUI;
import org.eclipse.ui.handlers.HandlerUtil;
import org.eclipse.ui.part.FileEditorInput;

import refactorerl.ui.Activator;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.connection.ReferlUI;
import refactorerl.ui.core.constants.ReferlConstants;

/**
 * Handles a call of the referl_ui's functions with path parameter.
 * It is configured as a command handler in the plugin.xml handlers category.
 * 
 * @author zoltan verebes, modified by Gergely Horvath
 * 
 */
public class FilemanHandler extends AbstractHandler implements IHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String command = event.getParameter("refactorerl.ui.core.fileman.cmd");
		
		if (null == command || command.equals("")) {
			return null;
		}
		
		ISelection selection = HandlerUtil.getCurrentSelection(event);
		
		if (command.equals("add_dir")) {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			DirectoryDialog dlg = new DirectoryDialog(shell);
			dlg.setText("Load directory");
			dlg.setMessage("Select directory");
			
			String dir = dlg.open();
			if (null == dir) {
				return null;
			}
			
			try {
    			new ProgressMonitorDialog(Display.getCurrent().getActiveShell()).run(true, true,
    			          new ShowProgress(dir));
    		} catch (InvocationTargetException e) {
    			e.printStackTrace();
    		} catch (InterruptedException e) {
    			e.printStackTrace();
    		}
		} else {
    		final Set<IPath> paths = new HashSet<IPath>();
    
    		if (selection instanceof ITreeSelection) {
    			ITreeSelection tSelection = (ITreeSelection) selection;
    			Iterator<?> it = tSelection.iterator();
    
    			while (it.hasNext()) {
    				IResource resource = (IResource)it.next();
    
    				try {
    					resource.accept(
    							new IResourceProxyVisitor() {
    								@Override
    								public boolean visit(IResourceProxy proxy) throws CoreException {
    									if (proxy.getName().endsWith(".erl") || proxy.getName().endsWith(".hrl")) {
    										paths.add(proxy.requestFullPath());
    									}
    									
    									return true;
    								}
    							}, 0);
    				} catch (CoreException e) {
    					throw new IllegalArgumentException("File information collecting failed", e);
    				}
    			}
    		}
    		
    		if (selection instanceof ITextSelection) {
    			IEditorPart editorPart = HandlerUtil.getActiveEditorChecked(event);
    			IEditorInput editorInput = editorPart.getEditorInput();
    
    			if (editorInput instanceof FileEditorInput) {
    				editorPart.doSave(new NullProgressMonitor());
    				IResource resource = ((FileEditorInput) editorInput).getFile();
    				paths.add(resource.getFullPath());
    			}
    		}
    		
    		try {
    			new ProgressMonitorDialog(Display.getCurrent().getActiveShell()).run(true, true,
    			          new ShowProgress(command, paths));
    		} catch (InvocationTargetException e) {
    			e.printStackTrace();
    		} catch (InterruptedException e) {
    			e.printStackTrace();
    		}
		}
		
		return null;
	}
	
	private class ShowProgress implements IRunnableWithProgress {
		private Set<IPath> paths;
		String command;
		String dir;
		
		public ShowProgress(String command, Set<IPath> paths) {
			this.command = command;
			this.paths = paths;
		}
		
		public ShowProgress(String dir) {
			this.command = "add_dir";
			this.dir = dir;
		}
		
		@Override
		public void run(IProgressMonitor monitor)
				throws InvocationTargetException, InterruptedException {
			
			IConnection connection = ConnectionPool.getInstance().getConnection();
			if (null == connection) {
				MessageDialog.openWarning(
						Activator.getDefault().getWorkbench().getDisplay().getActiveShell(), 
						ReferlConstants.MESSAGE_NOCONFIG_HEADER,
						ReferlConstants.MESSAGE_NOCONFIG_BODY);
				return;
			}
			
			if (!connection.ping()) {
				connection.connect();
			}
			
			ReferlUI referlUI = connection.getReferlUI();
			
			if (command.equals("add_dir")) {
				referlUI.addDirectory(monitor, dir).waitForResult();
			} else {
				IWorkspaceRoot root = ResourcesPlugin.getWorkspace().getRoot();
				
    			for (IPath path : paths) {
    				referlUI.callFunWithPath(
    						monitor,
    						command,
    						root.getLocation().toString() + path.toString()).waitForResult();
    			}
			}
		}
	}
}
