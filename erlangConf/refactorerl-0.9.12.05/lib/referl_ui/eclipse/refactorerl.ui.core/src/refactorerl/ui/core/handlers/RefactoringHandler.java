package refactorerl.ui.core.handlers;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.resources.IFile;
import org.eclipse.core.resources.IResource;
import org.eclipse.core.resources.IWorkspace;
import org.eclipse.core.resources.ResourcesPlugin;
import org.eclipse.core.runtime.CoreException;
import org.eclipse.core.runtime.IPath;
import org.eclipse.core.runtime.Path;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ltk.core.refactoring.Refactoring;
import org.eclipse.ltk.ui.refactoring.RefactoringWizard;
import org.eclipse.ltk.ui.refactoring.RefactoringWizardOpenOperation;
import org.eclipse.swt.widgets.Shell;
import org.eclipse.ui.PlatformUI;

import refactorerl.ui.Activator;
import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.connection.ReferlUI;
import refactorerl.ui.core.constants.ReferlConstants;
import refactorerl.ui.core.extensionpoints.ExtensionPointParser;
import refactorerl.ui.core.extensionpoints.RefactorExtensionPointProxy;
import refactorerl.ui.core.inboxlisteners.RequestListener;
import refactorerl.ui.core.parameterproviders.ContextTransformParameterProvider;
import refactorerl.ui.core.parameterproviders.ITransformParameterProvider;
import refactorerl.ui.core.refactoring.ReferlRefactoring;
import refactorerl.ui.core.refactoring.ReferlRefactoringWizard;
import refactorerl.ui.core.utils.ExtensionPointUtils;
import refactorerl.ui.core.utils.FileUtils;
import refactorerl.ui.core.utils.WorkspaceUtils;

import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
/**
 * This is the default handler for Commands which are bind to RefactorErl 
 * refactorings through extension points. It
 * instantiates and executes a RefactoringWizard and its Refactoring.
 * 
 * When the wizard is finished it sends a parameterized message to an 
 * Erlang process to perform the refactoring. The
 * message has all of the required arguments to make the Erlang process able 
 * to perform its task. It does its tasks
 * async way, so when the message is sent the lifecycle of the Refactoring ends.
 * 
 * Parameters are defined when the RefactorErl core is extended at 
 * specified extension points.
 * 
 * TODO: add extension point information
 * 
 * @author zoltan verebes, modified by Gergely Horvath, J�zsa B�la - Tam�s
 * 
 */
public class RefactoringHandler extends AbstractHandler implements IHandler {
	private static Logger logger = Logger.getLogger(Activator.class.getName());
	private static final int MANUAL = 1;
	private static final int CONTEXT = 2;

	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.core.commands.AbstractHandler#execute(org.eclipse.core.commands .ExecutionEvent)
	 */
	/**
	 * This method is called every time when an execution event appears on the
	 * interface of RefactorErl refactorings. It collects necessary information
	 * and parameters to send to the RefactorErl
	 */
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		IConnection connection = ConnectionPool.getInstance().getConnection();
		
		if (null == connection) {
			MessageDialog.openWarning(
					Activator.getDefault().getWorkbench().getDisplay().getActiveShell(), 
					ReferlConstants.MESSAGE_NOCONFIG_HEADER,
					ReferlConstants.MESSAGE_NOCONFIG_BODY);
			return null;
		}
		
		if (!connection.ping()){
			if(!connection.connect()) {
				throw new ErlangException("ReferlCommandHandler" +
						" was unable to connect to RefactorErl");
			}
		}
		
		//Getting commands specified in plugin.xml
		//Command example: refactorerl.ui.extensions.transform.renamefun
		String commandId = event.getCommand().getId();
		logger.info(commandId);
		//XML parser. 
		ExtensionPointParser parser = ExtensionPointParser.getInstance();
		//Gets the corresponding refactor for a specific command
		RefactorExtensionPointProxy extensionPoint = parser.getRefactorExtension(commandId);
		
		if (extensionPoint == null) {
			throw new IllegalArgumentException("Extension not found for command: " + commandId);
		}
		
		if (extensionPoint.getTransformName() == null || extensionPoint.getTransformName().equals("")) {
			throw new IllegalArgumentException("Transform name is not specified in the extension point. Command id: "
					+ commandId);
		}
		
		//Gets the parameter providers specified in plugin file
		List<ITransformParameterProvider> parameterProviders = getTransformParameterProvider(extensionPoint, event, CONTEXT);
		logger.info(parameterProviders.toString());
		IResource resource = null;
		final Map<String, Object>parameters = new HashMap<String, Object>();
		//Collecting parameters specified by parameter providers
		for (ITransformParameterProvider transformParameterProvider : parameterProviders) {
			if (resource == null) {
				resource = ((ContextTransformParameterProvider) transformParameterProvider).getModifiedResource();
			}
			parameters.putAll(transformParameterProvider.getParameterMap());
		}
		logger.info(parameters.toString());
		final List<ITransformParameterProvider> parameterProvidersPages = 
			getTransformParameterProvider(extensionPoint, event, MANUAL);
		
		// RefactoringProcessor processor = new RefactoringProcessor
		List<String> temporaryFiles = saveOldFiles(connection);
		
		Refactoring refactoring = new ReferlRefactoring(
				extensionPoint.getTransformName(), 
				resource, 
				parameters);
		
		RefactoringWizard wizard = new ReferlRefactoringWizard(
				refactoring, 
				RefactoringWizard.NO_BACK_BUTTON_ON_STATUS_DIALOG,
				parameterProvidersPages,
				parameters);
		
		RefactoringWizardOpenOperation op = new RefactoringWizardOpenOperation(wizard);
		
		try {
			Shell shell = PlatformUI.getWorkbench().getActiveWorkbenchWindow().getShell();
			op.run(shell, extensionPoint.getTransformName());
		}
		catch (InterruptedException e) {
			throw new ExecutionException("Operation could not be performed", e);
		}
		
		deleteTemporaryFiles(temporaryFiles);
		return null;
	}
	
	private List<String> saveOldFiles(IConnection connection) { 
		List<String> files = new ArrayList<String>();
		List<String> temporaryFiles = new ArrayList<String>();
		
		ReferlUI referlUI = connection.getReferlUI();
		RequestListener req = referlUI.sendRequest("filelist");
		
		OtpErlangObject message = req.getResult();
		
		if (message instanceof OtpErlangTuple && ((OtpErlangTuple)message).arity()>1 && ((OtpErlangTuple)message).elementAt(1) instanceof OtpErlangList ) {
			OtpErlangList filelist = (OtpErlangList)((OtpErlangTuple)message).elementAt(1);
			for (OtpErlangObject o : filelist) {
				if ( !(o instanceof OtpErlangTuple) || 
					 ((OtpErlangTuple)o).arity() > 2)  {
						 continue;
				}
				OtpErlangTuple toWork = (OtpErlangTuple)o;
				
				String filename = ((OtpErlangString)toWork.elementAt(0)).stringValue();
				if (filename.endsWith(".erl") || filename.endsWith(".hrl")) {
					files.add(filename);
				}
			}
		}
		
		IWorkspace workspace= ResourcesPlugin.getWorkspace();
		for (String filename : files) {
			IPath location = new Path(filename);
			
			try {
				IFile file= workspace.getRoot().getFileForLocation(location);
				
				if (file != null) {
					location = location.addFileExtension("_referltmp");
					
					String newName = location.toPortableString();
					FileUtils.copy(filename, newName);
					temporaryFiles.add(newName);
					
					try {
						file.getParent().refreshLocal(IResource.DEPTH_ONE, null);
					} catch (CoreException e) {
						e.printStackTrace();
					}
				}
			} catch (IOException e) {
				e.printStackTrace();
			}
		}
		
		return temporaryFiles;
	}

	private void deleteTemporaryFiles(List<String> fileList) {
		for (String fileName : fileList) {
			FileUtils.delete(fileName);
		}
		
		try {
			WorkspaceUtils.refreshWorkspace();
		} catch (CoreException e) {
			e.printStackTrace();
		}		
	}
	
	private List<ITransformParameterProvider> getTransformParameterProvider(RefactorExtensionPointProxy proxy,
			ExecutionEvent event, int type) throws ExecutionException {
		List<ITransformParameterProvider> parameterProviders = new ArrayList<ITransformParameterProvider>();

		List<String> classNames = (type == MANUAL ? proxy.getManualParameterProviderClassNames() : proxy
				.getContextParameterProviderClassNames());
		
		for (String className : classNames) {
			Object[] args;
			
			if (type == MANUAL) {
				args = new Object[] {event, proxy.getName() == null ? "" : proxy.getName(),
						proxy.getDescription() == null ? "" : proxy.getDescription(), proxy.getParameters(className) };
			} else {
				args = new Object[] { event };
			}

			ITransformParameterProvider parameterProvider = (ITransformParameterProvider) ExtensionPointUtils
					.createObject(className, args);
			parameterProviders.add(parameterProvider);
		}

		return parameterProviders;
	}
}
