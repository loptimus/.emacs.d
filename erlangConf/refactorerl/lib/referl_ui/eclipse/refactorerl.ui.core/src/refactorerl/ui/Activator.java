package refactorerl.ui;

import java.util.logging.Logger;

import org.eclipse.jface.dialogs.MessageDialog;
import org.eclipse.ui.plugin.AbstractUIPlugin;
import org.osgi.framework.BundleContext;

import refactorerl.ui.core.connection.ConnectionPool;
import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.constants.ReferlConstants;
import refactorerl.ui.core.utils.ConnectionUtils;

/**
 * The activator class controls the plug-in life cycle
 */
public class Activator extends AbstractUIPlugin {
	// The plug-in ID
	public static final String PLUGIN_ID = "refactorerl.ui.core";

	// The shared instance
	private static Activator plugin;
	private static Logger logger = Logger.getLogger(Activator.class.getName());
	
	// User Variables
	/**
	 * The connection used by this plug-in
	 */
	private IConnection connection;
	
	/**
	 * The constructor
	 */
	public Activator() {
	}
	
	/*
	 * (non-Javadoc)
	 *
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#start
	 * (org.osgi.framework.BundleContext)
	 */
	public void start(BundleContext context) throws Exception {
		super.start(context);
		plugin = this;
		
		logger.info("Plugin Activation started");
		connection = ConnectionPool.getInstance().getConnection();
		
		if (connection != null) {
			logger.info("Connection Estabilished");
					
			ConnectionUtils.configureConnection(
				connection, 
				getPreferenceStore().getBoolean(ReferlConstants.KEY_REFACTORERL_DBSYNC)
			);
			
			connection.startInbox();
			logger.info("INBOX STARTED");
		} else {
			MessageDialog.openWarning(
					Activator.getDefault().getWorkbench().getDisplay().getActiveShell(), 
					ReferlConstants.MESSAGE_NOCONFIG_HEADER,
					ReferlConstants.MESSAGE_NOCONFIG_BODY);
		}
	}
	
	/*
	 * (non-Javadoc)
	 * 
	 * @see org.eclipse.ui.plugin.AbstractUIPlugin#stop(org.osgi.framework.BundleContext)
	 */
	public void stop(BundleContext context) throws Exception {
		if (connection != null) {
			connection.getReferlUI().sendRequest("stop");
			connection.resetListeners();
			connection.killProcessIfRunning();
		}
		
		plugin = null;
		super.stop(context);
	}

	/**
	 * Returns the shared instance
	 * 
	 * @return the shared instance
	 */
	public static Activator getDefault() {
		return plugin;
	}
}
