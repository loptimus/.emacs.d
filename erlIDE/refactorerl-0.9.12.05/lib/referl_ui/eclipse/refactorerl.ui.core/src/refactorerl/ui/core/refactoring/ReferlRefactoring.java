package refactorerl.ui.core.refactoring;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.IWorkspaceRoot;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.core.runtime.IStatus;
import org.eclipse.core.runtime.NullProgressMonitor;
import org.eclipse.core.runtime.OperationCanceledException;
import org.eclipse.core.runtime.Path;
import org.eclipse.core.runtime.Status;
import org.eclipse.ltk.core.refactoring.Change;
import org.eclipse.ltk.core.refactoring.CompositeChange;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.core.refactoring.RefactoringStatus;
import org.eclipse.ltk.core.refactoring.resource.RenameResourceChange;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;


import refactorerl.ui.Activator;
import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.assistants.RefactorErlChangedFile;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.connection.ReferlUI;
import refactorerl.ui.core.inboxlisteners.RequestListener;
import refactorerl.ui.core.utils.ProplistUtils;
/**
* 
* @author zoltan verebes, modified by Gergely Horvath 
* 
*/
public class ReferlRefactoring extends Refactoring {
	private static final Logger logger =  Logger.getLogger(Activator.class.toString());
	private static final String REFACTOR_NAME = "referlrefactor";
	protected ArrayList<RefactorErlChangedFile> changedFiles; 
	private String name;
	private Map<String, Object> parameters;
	private IResource resource;
	private Boolean undoable = false;
	
	public ReferlRefactoring(String name, IResource resource, Map<String, Object> parameters) {
		if (name == null || name.equals("")) {
			throw new IllegalArgumentException("Name paramater is required");
		}
		
		if (resource == null) {
			throw new IllegalArgumentException("Modified resource parameter is required");
		} 

		this.name = name;
		this.parameters = parameters;
		this.resource = resource;
	}

	@Override
	public RefactoringStatus checkFinalConditions(IProgressMonitor pm) 
		throws CoreException, OperationCanceledException {
		RefactoringStatus status = new RefactoringStatus();
		
		if (pm == null) {
			pm = new NullProgressMonitor();
		}
		
		try {
			pm.beginTask(getName(), 4);
			IConnection connection = ConnectionPool.getInstance().getConnection();
			pm.worked(1);
			if (!connection.ping()) {
				connection.connect();
			}
			pm.worked(1);
		
			ReferlUI referlUI = connection.getReferlUI();
			RequestListener request = referlUI.transform(name, parameters);
		
			OtpErlangObject o = request.getResult();
			
			if (o instanceof OtpErlangTuple) {
				OtpErlangTuple tuple = (OtpErlangTuple)o;
				logger.info(tuple.toString());
			
				// {error,{{reflib_transform,save,[]},"error during saving results"}}
				if (tuple.arity() > 1 && tuple.elementAt(0).toString().equals("error")) {
					OtpErlangTuple error = (OtpErlangTuple)tuple.elementAt(1);
					status.addFatalError(((OtpErlangString)error.elementAt(1)).stringValue());
				} else if (
					tuple.arity() > 1 && tuple.elementAt(0).toString().equals("ok") &&
					tuple.elementAt(1) instanceof OtpErlangTuple && ((OtpErlangTuple)tuple.elementAt(1)).arity() > 1) {
					
					tuple = (OtpErlangTuple) tuple.elementAt(1);
					
					if (tuple.elementAt(0).toString().equals("abort") || tuple.elementAt(0).toString().equals("error")) {
						OtpErlangTuple error = (OtpErlangTuple)tuple.elementAt(1);
						status.addFatalError(((OtpErlangString)error.elementAt(1)).stringValue());
					} else {
						changedFiles = new ArrayList<RefactorErlChangedFile>();
						undoable = true;
						
						if (tuple.arity() > 1 && tuple.elementAt(1) instanceof OtpErlangList) {
							OtpErlangList proplist = (OtpErlangList)tuple.elementAt(1);
							
							OtpErlangObject obj = ProplistUtils.getValue("saved", proplist);
							if (!(obj instanceof OtpErlangList)) {
								throw new ErlangException("Not a OtpErlangList!");
							}
							OtpErlangList saved = (OtpErlangList)obj;
							
							obj = ProplistUtils.getValue("renamed", proplist);
							if (!(obj instanceof OtpErlangList)) {
								throw new ErlangException("Not a OtpErlangList!");
							}
							OtpErlangList renamed = (OtpErlangList) obj;
							
							HashMap<String,String> renamedMap = new HashMap<String, String>();
							for (OtpErlangObject t : renamed) {
								OtpErlangTuple nevek = (OtpErlangTuple)t;
								renamedMap.put(
										((OtpErlangString)nevek.elementAt(1)).stringValue(),
										((OtpErlangString)nevek.elementAt(0)).stringValue());
							}
							
							for (OtpErlangObject t : saved) {
								if (!(t instanceof OtpErlangString)) {
									continue;
								}
								
								OtpErlangString name = (OtpErlangString)t;
								String oldName = renamedMap.get(name.stringValue());
								
								if (oldName == null) {
									oldName = name.stringValue();
								}
								
								IWorkspace workspace = ResourcesPlugin.getWorkspace();
								IWorkspaceRoot root = workspace.getRoot();
								Path p = new Path(oldName);
								IFile file = root.getFileForLocation(p);
								
								if (file != null) {
									changedFiles.add(new RefactorErlChangedFile(oldName, name.stringValue()));
								}
							}
						}
					}
				}
			}
		} catch (ErlangException e) {
			status.addFatalError(e.getMessage());
		}
		
		pm.done();

		return status;
	}

	@Override
	public RefactoringStatus checkInitialConditions(IProgressMonitor pm) 
		throws CoreException, OperationCanceledException {
		// TODO: check conditions
		return new RefactoringStatus();
	}

	@Override
	public Change createChange(IProgressMonitor pm) throws CoreException, OperationCanceledException {		
		pm.beginTask("Creating changes", changedFiles.size() + 1);
		CompositeChange change = new CompositeChange(getName());
		pm.internalWorked(1);
		
		try {
			Change c;
			for (RefactorErlChangedFile e : changedFiles) {
				c = e.createChanges();
				
				if (c != null) {
					change.add(c);
					pm.internalWorked(1);
				}
				
				if (e.isNameChanged()) {
					IPath p = e.getIPath();
					String s = e.getNewName();
					RenameResourceChange rch = new RenameResourceChange(p, s) {
						@Override
						public Change perform(IProgressMonitor pm) {
							return null;
						}
					};
					change.add(rch);
				}
			}
		} catch (IOException e) {
			e.printStackTrace();
			Status s = new Status(IStatus.ERROR, Activator.PLUGIN_ID, e.getMessage());
			throw new CoreException(s);
		}

		return change;
	}

	/*
	 * (non-Javadoc)
	 * @see org.eclipse.ltk.core.refactoring.Refactoring#getName()
	 */
	@Override
	public String getName() {
		return REFACTOR_NAME;
	}

	public Boolean getUndoable() {
		return undoable;
	}

	/**
	 * @param resource the resource to set
	 */
	public void setResource(IResource resource) {
		this.resource = resource;
	}

	/**
	 * @return the resource
	 */
	public IResource getResource() {
		return resource;
	}
}
