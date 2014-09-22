package refactorerl.ui.core.connection;

/**
 * Listener interface of classes which wants to be notified when a message arrives to the inbox of the connection. Inbox listeners can be
 * registered via inboxlisteners extension point or by calling addListener function of IConnection
 * 
 * @author zoltan verebes
 * 
 */
public interface IInboxListener {
	/**
	 * @param message sent by RefactorErl
	 */
	public void messageReceived(Object message);
}
