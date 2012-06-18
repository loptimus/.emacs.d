package refactorerl.ui.core.utils;

import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;

import org.eclipse.jface.preference.IPreferenceStore;

import refactorerl.ui.core.connection.IConnection;
import refactorerl.ui.core.constants.ReferlConstants;
import refactorerl.ui.core.exceptions.ErlangException;
import refactorerl.ui.core.extensionpoints.ExtensionPointParser;
import refactorerl.ui.core.extensionpoints.InboxListenerExtensionPointProxy;

/**
 * Contains convenience functions to handle Refactorerl connection related things
 * 
 * @author zoltan verebes
 * 
 */
public final class ConnectionUtils implements ReferlConstants {
	private static Logger logger = Logger.getLogger(ConnectionUtils.class.getName());
	
	/**
	 * initialize the connect:
	 * <ul>
	 * <li>removes original listeners</li>
	 * <li>starts the RefactorErl process by calling connection's connect</li>
	 * <li>add defined listeners to the new connection. they are specified through an extension point</li>
	 * <li>synchronizes the workspace to RefactorErl database</li>
	 * </ul>
	 * @throws ErlangException
	 */
	public static void configureConnection(IConnection connection, boolean dbsync) throws ErlangException {
		if (connection == null) {
			throw new IllegalArgumentException("Connection parameter is required");
		}

		connection.resetListeners();
		if (!connection.ping()) {
			connection.connectChecked();
		}
		
		for (InboxListenerExtensionPointProxy proxy : ExtensionPointParser.getInstance().findInboxListenerExtensionPointProxies()) {
			connection.addListener(proxy.getKey(), proxy.getInboxListener());
		}
		
		if (dbsync) {
			logger.info("Database Sync START");
			DatabaseUpdateService.getInstance().fullSynchronize(connection.getReferlUI());
			logger.info("Database Sync FINISH");
		}
	}

	/**
	 * 
	 * @param preferenceStore
	 * @return
	 */
	public static boolean validatePreferences(IPreferenceStore store) {
		if (store == null) {
			throw new IllegalArgumentException("Store parameter cannot be null");
		}

		String startScript = store.getString(KEY_REFACTORERL_REFERL_DIR);
		if (!store.contains(KEY_REFACTORERL_REFERL_DIR) || startScript == null || startScript.equals("")) {
			return false;
		}

		String workingDir = store.getString(KEY_REFACTORERL_WORKING_DIR);
		if (!store.contains(KEY_REFACTORERL_WORKING_DIR) || workingDir == null || workingDir.equals("")) {
			return false;
		}

		int startWaitingTime = store.getInt(KEY_REFACTORERL_START_WAITINGTIME);
		if (!store.contains(KEY_REFACTORERL_START_WAITINGTIME) || startWaitingTime <= 0) {
			return false;
		}

		Boolean startProcess = store.getBoolean(KEY_REFACTORERL_START_PROCESS);
		if (!store.contains(KEY_REFACTORERL_START_PROCESS) || startProcess == null || startProcess.equals("")) {
			return false;
		}

		String dbType = store.getString(KEY_REFACTORERL_DB_TYPE);
		if (!store.contains(KEY_REFACTORERL_DB_TYPE) || dbType == null || dbType.equals("")) {
			return false;
		}

		// TODO: check other parameters, too

		return true;
	}

	/**
	 * Converts the preference store to a map
	 * 
	 * @param preferenceStore
	 * @return
	 */
	public static Map<String, Object> preferenceStoreToMap(IPreferenceStore store) {

		if (store == null) {
			throw new IllegalArgumentException("Store parameter cannot be null");
		}

		Map<String, Object> preferences = new HashMap<String, Object>();
		preferences.put(KEY_REFACTORERL_REFERL_DIR, store.getString(KEY_REFACTORERL_REFERL_DIR));
		preferences.put(KEY_REFACTORERL_WORKING_DIR, store.getString(KEY_REFACTORERL_WORKING_DIR));
		preferences.put(KEY_REFACTORERL_START_PROCESS, store.getBoolean(KEY_REFACTORERL_START_PROCESS));
		preferences.put(KEY_REFACTORERL_START_WAITINGTIME, store.getInt(KEY_REFACTORERL_START_WAITINGTIME));
		preferences.put(KEY_REFACTORERL_DB_TYPE, store.getString(KEY_REFACTORERL_DB_TYPE));
		return preferences;
	}
}
