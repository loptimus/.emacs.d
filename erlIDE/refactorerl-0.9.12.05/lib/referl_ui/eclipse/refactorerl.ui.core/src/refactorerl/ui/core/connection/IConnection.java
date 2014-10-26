package refactorerl.ui.core.connection;

import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;

/**
 * Connection interface to RefactorErl process. there are some typical functions
 * like connect, ping or send but it is also good to get  a trunk to the referl_ui
 * module and it is also possible to add listeners to the connection to handle
 * messages arrived from RefactorErl
 * 
 * @author zoltan verebes
 * 
 */
public interface IConnection {
	/**
	 * 
	 * @return true when reply arrived for the ping, false otherwise
	 */
	boolean ping();

	/**
	 * Getter for referl_ui trunk in Java to call its functions
	 * 
	 * @return Java trunk to referl_ui module
	 */
	ReferlUI getReferlUI();

	/**
	 * Connects to RefactorErl process and creates a referlUI
	 * 
	 * @return true when connection was successful, false otherwise 
	 */
	boolean connect();

	/**
	 * function to send an Erlang message to RefactorErl
	 * 
	 * @param module
	 *            is the target module
	 * @param function
	 *            is the target function on the target module
	 * @param args
	 *            wrapped Erlang objects as parameters
	 */
	void send(String module, String function, OtpErlangObject... args);

	/**
	 * Convenience method to add an inbox listener to the connection. Listener will be notified about any type of message from Refactorerl
	 * 
	 * @param listener
	 *            to be added
	 */
	void addListener(IInboxListener listener);

	/**
	 * Convenience method to remove an inbox listener to the connection
	 * 
	 * @param listener
	 *            to be removed
	 */
	void removeListener(IInboxListener listener);

	/**
	 * Convenience method to add an inbox listener to the connection. Listener will be notified about messages which key is the specified key
	 * 
	 * @param listener
	 *            to be added
	 */
	void addListener(String key, IInboxListener listener);

	/**
	 * Convenience method to remove an inbox listener to the connection
	 * 
	 * @param listener
	 *            to be removed
	 */
	void removeListener(String key, IInboxListener listener);

	/**
	 * Removes all of the listeners of the connection
	 */
	void resetListeners();

	/**
	 * starts the RefactorErl process as a system process in the background 
	 */
	void startRefactorerlProcess();

	/**
	 * Same as connect, but a ping is called after connection to check whether the connection was successful
	 * 
	 * @return true when connection was successful, false otherwise. Success is checked by pinging the new connection
	 */
	boolean connectChecked();

	/**
	 * Stops the running system process if it is running
	 */
	void killProcessIfRunning();
	
	/**
	 * Starts the executing of inbox Thread.
	 * The thread has the role of accepting messages from RefactorErl
	 * @return true if the thread is running or it has been successful started, false otherwise
	 */
	boolean startInbox();
	
	/**
	 * Checks if the connection is in connected status.
	 * @return True if the connection is established
	 */
	boolean isConnected();
	
	IdHandler getIdHandler();
	
	OtpErlangPid getPid();
}
