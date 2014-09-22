package refactorerl.ui.core.handlers;

import org.eclipse.core.commands.AbstractHandler;
import org.eclipse.core.commands.ExecutionEvent;
import org.eclipse.core.commands.ExecutionException;
import org.eclipse.core.commands.IHandler;
import org.eclipse.jface.dialogs.MessageDialog;

import refactorerl.ui.Activator;
import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.constants.ReferlConstants;

public class ClusteringHandler extends AbstractHandler implements IHandler {
	@Override
	public Object execute(ExecutionEvent event) throws ExecutionException {
		String algorithm  = event.getParameter("refactorerl.ui.extensions.clustering.algorithm");
		if (null == algorithm || "".equals(algorithm)) {
			return null;
		}
		
		String entityType = event.getParameter("refactorerl.ui.extensions.clustering.entityType");
		if (null == entityType || "".equals(algorithm)) {
			return null;
		}
		
		IConnection connection = ConnectionPool.getInstance().getConnection();
		if (null == connection) {
			MessageDialog.openWarning(
					Activator.getDefault().getWorkbench().getDisplay().getActiveShell(),
					ReferlConstants.MESSAGE_NOCONFIG_HEADER,
					ReferlConstants.MESSAGE_NOCONFIG_BODY);
			return null;
		}

		if (!connection.ping()) {
			connection.connect();
		}
		
		connection.getReferlUI().clustering(entityType, algorithm);
		
		return null;
	}
}
