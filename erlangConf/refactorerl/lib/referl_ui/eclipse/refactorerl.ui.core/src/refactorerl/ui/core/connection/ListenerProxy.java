package refactorerl.ui.core.connection;
/**
 * @author Gergely Horvath
 *
 */

import java.util.concurrent.Executor;

public class ListenerProxy implements IInboxListener{
	private IInboxListener receiver;
	private static Executor executor = new Executor() {
		@Override
		public void execute(Runnable r) {
			new Thread(r).start();
		}
	};
	
	public ListenerProxy(IInboxListener receiver){
		this.receiver = receiver;
	}

	public void messageReceived(Object message) {
		final Object finalMessage = message;
		Runnable task = new Runnable() {
			@Override
			public void run() {
				receiver.messageReceived(finalMessage);
			}
		};
		synchronized (executor) {
			executor.execute(task);			
		}
	}
}
