package refactorerl.ui.core.connection;

import java.util.ArrayList;
import java.util.List;

import org.eclipse.jface.preference.IPreferenceStore;

import refactorerl.ui.Activator;
import refactorerl.ui.core.utils.ConnectionUtils;
import refactorerl.ui.core.utils.DatabaseUpdateService;

/**
 * Pool of connections. Connections are used to ping an active or inactive Erlang node. When the Erlang node is available it provides a subsystem
 * for the user to Erlang Refactoring tools through an implementation of IReferlFaced interface.
 * 
 * It is a singleton class.
 * 
 * @author zoltan verebes, modified by J�zsa B�la - Tam�s
 * 
 */
public class ConnectionPool {
	// single instance of the connection pool
	protected static ConnectionPool instance;
	
	// cache of the currently available connections
	protected List<IConnection>availableConnections = new ArrayList<IConnection>();
	
	protected ConnectionPool() {
	}
	
	/**
	 * Standard singleton instance getter. It is available from anywhere in the application
	 * 
	 * @return single instance of the pool
	 */
	public static ConnectionPool getInstance() {
		if (instance == null) {
			instance = new ConnectionPool();
		}
		
		return instance;
	}

	/**
	 * @return any connection to the RefactorErl Erlang module
	 */
	public IConnection getConnection() {
		IConnection toReturn = null;
		
		if (availableConnections.size() == 0) {
			IPreferenceStore store = Activator.getDefault().getPreferenceStore();
			
			if (!ConnectionUtils.validatePreferences(store)) {
				// TODO: exception
				return null;
			}
			
			ReferlConnection connection =
				new ReferlConnection(ConnectionUtils.preferenceStoreToMap(store));
			
			if (connection.connect()) {
				availableConnections.add(connection);
				toReturn = connection;
			}
		} else {
			IConnection temp = availableConnections.get(0);
			
			if(!temp.isConnected()) {
				temp.connect();
			}
			toReturn = temp;
		}
		
		return toReturn;
	}

	/**
	 * Resets all of the connections. Before losing the reference it resets the connections' listeners
	 */
	public void reset() {
		for (IConnection connection : availableConnections) {
			connection.resetListeners();
			connection.killProcessIfRunning();
			DatabaseUpdateService.getInstance().reset();
		}

		availableConnections = new ArrayList<IConnection>();
	}
	
	public void stop() {
		for (IConnection connection : availableConnections) {
			connection.getReferlUI().sendRequest("stop");
			connection.resetListeners();
			connection.killProcessIfRunning();
		}
	}
}
