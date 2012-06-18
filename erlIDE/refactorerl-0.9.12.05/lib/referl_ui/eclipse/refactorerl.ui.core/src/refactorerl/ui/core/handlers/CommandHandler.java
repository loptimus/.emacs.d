package refactorerl.ui.core.handlers;

import java.lang.reflect.InvocationTargetException;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.core.runtime.IProgressMonitor;
import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.jface.dialogs.ProgressMonitorDialog;
import org.eclipse.jface.operation.IRunnableWithProgress;
import org.eclipse.swt.widgets.Display;

import refactorerl.ui.Activator;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.connection.ReferlUI;
import refactorerl.ui.core.constants.ReferlConstants;

public class CommandHandler extends AbstractHandler implements IHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String command = event.getParameter("refactorerl.ui.core.server.cmd");
		
		if (null == command || "".equals(command)) {
			return null;
		}
		
		try {
			new ProgressMonitorDialog(Display.getCurrent().getActiveShell()).run(true, true,
			          new ShowProgress(command));
		} catch (InvocationTargetException e) {
			e.printStackTrace();
		} catch (InterruptedException e) {
			e.printStackTrace();
		}
		
		return null;
	}
	
	private class ShowProgress implements IRunnableWithProgress {
		String command;
		
		public ShowProgress(String command) {
			this.command = command;
		}
		
		@Override
		public void run(IProgressMonitor monitor)
				throws InvocationTargetException, InterruptedException {
			monitor.beginTask(getDescription(command), IProgressMonitor.UNKNOWN);
			
			IConnection connection = ConnectionPool.getInstance().getConnection();
			if (null == connection) {
				monitor.done();
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
			referlUI.sendRequest(monitor, command).waitForResult();
		}
		
		private String getDescription(String key) {
			if (key.equals("reset")) {
				return "Resetting RefactorErl database";
			}
			
			return key;
		}
	}
}
