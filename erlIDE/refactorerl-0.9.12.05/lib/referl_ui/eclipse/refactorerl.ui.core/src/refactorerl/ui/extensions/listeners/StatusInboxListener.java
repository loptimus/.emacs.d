package refactorerl.ui.extensions.listeners;

import java.io.File;

import com.ericsson.otp.erlang.*;

import org.eclipse.core.resources.*;
import org.eclipse.core.runtime.*;
import org.eclipse.ui.*;
//import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.filesystem.EFS;
import org.eclipse.core.filesystem.IFileStore;
import org.erlide.core.model.erlang.IErlModule;
import org.erlide.core.model.util.ModelUtils;
import org.erlide.core.CoreScope;
import org.erlide.core.model.root.ErlModelException;
import org.erlide.core.model.root.IErlElement;
import org.erlide.core.model.root.IOpenable;
import org.eclipse.ui.ide.IDE;
import org.eclipse.swt.widgets.Display;

import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.connection.IInboxListener;
import refactorerl.ui.core.utils.ProplistUtils;
import refactorerl.ui.core.utils.WorkspaceUtils;

/**
 * Inbox listeners for status notification. If it is not "Finished" it notifies the user about the error
 * 
 * @author Gergely Horvath
 * 
 */
public class StatusInboxListener implements IInboxListener {
	@Override
	public void messageReceived(Object message) throws ErlangException {
		if (message == null || !(message instanceof OtpErlangList)) {
			return;
		}
		
		OtpErlangList list = (OtpErlangList) message;
		for (OtpErlangObject obj : list) {
			if (!(obj instanceof OtpErlangTuple)) {
				continue;
			}
			
			OtpErlangTuple tuple = (OtpErlangTuple)obj;
			if (tuple.arity() != 2) {
				throw new IllegalArgumentException("Expected type is 2 arity tuple");
			}
			
			if (tuple.elementAt(0).toString().equals("change")) {
				if (!(tuple.elementAt(1) instanceof OtpErlangList)) {
					throw new IllegalArgumentException("Expected type is list");
				}
				
				OtpErlangList lista = (OtpErlangList) tuple.elementAt(1);
				for (OtpErlangObject o : lista) {
					if (!(o instanceof OtpErlangTuple)) {
						continue;
					}
					
					tuple = (OtpErlangTuple)o;
					if (!(tuple.elementAt(0) instanceof OtpErlangString)) {
						throw new IllegalArgumentException("Expected type is OtpErlangString");
					}
					
					IWorkspace workspace= ResourcesPlugin.getWorkspace();
					IPath location= Path.fromOSString(((OtpErlangString)tuple.elementAt(0)).stringValue());
					IFile file= workspace.getRoot().getFileForLocation(location);
					if (null == file) {
						return;
					}
					
					IErlModule module = null;
					try {
						module = ModelUtils.getModuleFromExternalModulePath(file.getName());
					} catch (ErlModelException e1) {
						// TODO Auto-generated catch block
						e1.printStackTrace();
					}
					
					try {
						if (module != null) {
							((IOpenable) module.getParent()).open(null);
						}
					} catch (CoreException e) {
						throw new ErlangException("Could not refresh the workspace after refactoring", e);
					}
					
					IWorkbenchWindow wbWindows[] = PlatformUI.getWorkbench().getWorkbenchWindows();
					for (IWorkbenchWindow wbWindow : wbWindows) {
						if (wbWindow != null) {
							IWorkbenchPage pages[] = wbWindow.getPages();
							for (IWorkbenchPage page : pages) {
								if (page != null) {
									IEditorPart part = page.findEditor(new org.eclipse.ui.part.FileEditorInput(file) );
									
									if (part != null) {
										if (tuple.arity()>1 && tuple.elementAt(1) instanceof OtpErlangList) {
											OtpErlangList proplist = (OtpErlangList) tuple.elementAt(1);
											OtpErlangObject renamedTo = ProplistUtils.getValue("rename", proplist);
											
											if (renamedTo != null) {
												File fileToOpen = new File(((OtpErlangString)renamedTo).stringValue());
												
												if (fileToOpen.exists() && fileToOpen.isFile()) {
													final IFileStore fileStore = EFS.getLocalFileSystem().getStore(fileToOpen.toURI());
													final IWorkbenchPage page2 = page;										 
													final IPath oldlocation = (location.getDevice() != null) ? location.setDevice(location.getDevice().toUpperCase()):location;
													
											    	Display.getDefault().asyncExec(new Runnable() {
														@Override
														public void run() {
															try {
																WorkspaceUtils.refreshWorkspace(oldlocation);
																IFile file= ResourcesPlugin.getWorkspace().getRoot().getFileForLocation(oldlocation);
																IErlElement element = CoreScope.getModel().findElement(file);
																
																if (element != null) {
																	element.resourceChanged(null);
																} else { 
																	try
																	{
																		WorkspaceUtils.refreshWorkspace();
																	}
																	catch (CoreException e) {}CoreScope.getModel().resourceChanged(null);
																}
																IDE.openEditorOnFileStore(page2, fileStore);
															} catch (PartInitException e) {
																e.printStackTrace();
															} catch (CoreException e) {
																e.printStackTrace();
															}
														}
													});
											    } else {
											    //Do something if the file does not exist
											    }
											}
										}
									}
								}
							}
						}
					}
					
					try {
						if (location.getDevice() != null) {
							location = location.setDevice(location.getDevice().toUpperCase());
						}
						WorkspaceUtils.refreshWorkspace(location);
					} catch (CoreException e) {
						e.printStackTrace();
					}
					
					IErlElement element = CoreScope.getModel().findElement(file);
					if (element != null) {
						element.resourceChanged(null);
					} else { 
						try {
							WorkspaceUtils.refreshWorkspace();
						}
						catch (CoreException e) {
							e.printStackTrace();
						}
					}
					
					CoreScope.getModel().resourceChanged(null);
				}
			}
		}
	}
}
